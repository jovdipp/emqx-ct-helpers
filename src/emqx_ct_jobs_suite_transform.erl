%%--------------------------------------------------------------------
%% Copyright (c) 2021 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(emqx_ct_jobs_suite_transform).

-export([parse_transform/2, intl_function_name/1]).

-include("./emqx_ct_jobs_transform.hrl").

-record(audit, { module, found=[], eof_line }).

-define(EOF, eof).

parse_transform( AST, _Options ) ->
    { AST_Transformed, Audit } = transform( AST, [], #audit{} ),
    FinalAST = case has_function(Audit, ?JOB_MATRIX) of
                    true  -> jobs_transform( AST_Transformed, Audit);
                    false -> ensure_surefire( AST_Transformed, Audit )
                end,
    lists:reverse(FinalAST).

%% -------------------------------------------------------------------------------------------------
%%  AST TRANSFORMING
%% -------------------------------------------------------------------------------------------------

transform( [], AST, Audit ) ->
    { AST, Audit };
transform( [{ ?EOF, Line }], AST, Audit ) ->
    { AST, Audit#audit{ eof_line = Line } };
transform( [ { attribute, _Line, module, ModuleName } = A | Rest ], AST, Audit ) ->
    transform( Rest, [ A | AST ], Audit#audit{ module = ModuleName } );
transform( [ { function, _Line, ?JOB_MATRIX, _ArgCnt, _Clauses } = F | Rest ], AST, Audit ) ->
    rename_intl_function( F, Audit, Rest, AST );
transform( [ { function, _Line, ?JOB_OPTIONS, _ArgCnt, _Clauses } = F | Rest ], AST, Audit ) ->
    rename_intl_function( F, Audit, Rest, AST );
transform([ { function, _Line, ?INIT_PER_JOB, _ArgCnt, _Clauses } = F | Rest ], AST, Audit ) ->
    rename_intl_function( F, Audit, Rest, AST );
transform([ { function, _Line, ?END_PER_JOB, _ArgCnt, _Clauses } = F | Rest ], AST, Audit ) ->
    rename_intl_function( F, Audit, Rest, AST );
transform( [ { function, _Line, ?CT_ALL, _ArgCnt, _Clauses } = F | Rest ], AST, Audit ) ->
    rename_intl_function( F, Audit, Rest, AST );
transform( [ { function, _Line, ?CT_SUITE, _ArgCnt, _Clauses } = F | Rest ], AST, Audit ) ->
    rename_intl_function( F, Audit, Rest, AST );
transform( [ { function, _Line, ?CT_INIT_PER_SUITE, _ArgCnt, _Clauses } = F | Rest ], AST, Audit ) ->
    rename_intl_function( F, Audit, Rest, AST );
transform( [ { function, _Line, ?CT_END_PER_SUITE, _ArgCnt, _Clauses } = F | Rest ], AST, Audit ) ->
    rename_intl_function( F, Audit, Rest, AST );
transform( [ { function, _Line, ?CT_INIT_PER_GROUP, _ArgCnt, _Clauses } = F | Rest ], AST, Audit ) ->
    rename_intl_function( F, Audit, Rest, AST );
transform( [ { function, _Line, ?CT_END_PER_GROUP, _ArgCnt, _Clauses } = F | Rest ], AST, Audit ) ->
    rename_intl_function( F, Audit, Rest, AST );
transform( [ Other | Rest ], AST, Audit ) ->
    transform( Rest, [ Other | AST ], Audit ).

update_found( Function, Audit ) ->
    Found = Audit#audit.found,
    Audit#audit{ found = [ Function | Found ]}.

rename_intl_function( { function, Line, Name, ArgCnt, Clauses }, Audit, Rest, AST ) ->
    IntlName = intl_function_name(Name),
    AuditUpdated = update_found( Name, Audit ),
    FunctionRenamed = { function, Line, IntlName, ArgCnt, Clauses },
    transform( Rest, [ FunctionRenamed | AST ], AuditUpdated ).

%% -------------------------------------------------------------------------------------------------
%%  JOB FUNCTION INSERTIONS
%% -------------------------------------------------------------------------------------------------

jobs_transform( ReversedAST, Audit ) ->
    EOF_L = Audit#audit.eof_line,

    { ok, Fwds } = generate_fwds(Audit),
    FwdsLen = length(Fwds),

    EOF_L_Shifted = EOF_L + FwdsLen,
    EOF = [{?EOF, EOF_L_Shifted }],
    EOF ++ Fwds ++ ReversedAST.

generate_fwds(Audit) ->
    Module = Audit#audit.module,
    EOF_L = Audit#audit.eof_line,
    generate_fwds( ?FWD_FUNCTIONS, Module, Audit, EOF_L ).

generate_fwds( FwdFunctions, Module, Audit, EOF_L ) ->
    generate_fwds( FwdFunctions, Module, Audit, EOF_L, [] ).

generate_fwds( [], _Module, _Audit, _EOF_L, FunctionsAcc ) ->
    { ok, FunctionsAcc };
generate_fwds( [ { Name, ArgCnt } | Fwds ], Module, Audit, EOF_L, FunctionsAcc ) ->
    FunctionsAccUpdated = [ forward_function( EOF_L, Name, Audit, Module, ArgCnt ) | FunctionsAcc ],
    generate_fwds( Fwds, Module, Audit, EOF_L+1, FunctionsAccUpdated ).

ensure_surefire( ReversedAST, Audit ) ->
    Module = Audit#audit.module,
    EOF_L = Audit#audit.eof_line,
    SuiteFwd = forward_function( EOF_L+1, ?CT_SUITE, Audit, Module, 0 ),
    EOF = { ?EOF, EOF_L+2 },
    [ EOF, SuiteFwd | ReversedAST ].

%% -------------------------------------------------------------------------------------------------
%%  AST FUNCTION
%% -------------------------------------------------------------------------------------------------

forward_function( Line, Name, Audit, Module, ArgsCnt ) ->
    OriginalExisted = has_function( Audit, Name ),
    GenVars = [ {var,Line,var_arg(Num)} || Num <- lists:seq(0,ArgsCnt), Num =/= 0 ],
    OriginalExistedAtom = {atom,Line,OriginalExisted},
    ModuleAtom = {atom,Line,Module},
    Vars = [ OriginalExistedAtom, ModuleAtom | GenVars ],
    
    CtHelpersJobsModAtom = {atom,Line,emqx_ct_helpers_jobs},
    CtHelpersJobsFucAtom = {atom,Line,Name},
    RemoteMod = {remote,Line,CtHelpersJobsModAtom,CtHelpersJobsFucAtom},
    
    Forward = {call,Line,RemoteMod,Vars},
    Clauses = [{clause,Line,GenVars,[],[Forward]}],
    {function,Line,Name,ArgsCnt,Clauses}.

%% -------------------------------------------------------------------------------------------------
%%  Helpers
%% -------------------------------------------------------------------------------------------------

has_function(Audit, Function) ->
    lists:member( Function, Audit#audit.found ).

var_arg(Num) ->
    Arg = lists:concat([ 'Arg', Num ]),
    list_to_atom(Arg).

intl_function_name(Name) ->
    IntlName = lists:concat([?INTERNAL_PREFIX, Name]),
    list_to_atom(IntlName).
