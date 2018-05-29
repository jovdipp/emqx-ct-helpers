%%%===================================================================
%%% Copyright (c) 2013-2018 EMQ Inc. All rights reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%===================================================================

-module(emqx_ct_transform).

-export([parse_transform/2]).

%% @private
parse_transform(AST, _Options) ->
    walk_ast(AST, []).

walk_ast([], Acc) ->
    lists:reverse(Acc);
walk_ast([H = {attribute, _, module, Module}|T], Acc) ->
    put(module, Module),
    walk_ast(T, [H|Acc]);
walk_ast([{function, Line, Name, Arity, Clauses}|T], Acc) ->
    put(function, Name),
    walk_ast(T, [{function, Line, Name, Arity,
                  walk_clauses(Clauses, [])}|Acc]);
walk_ast([H|T], Acc) ->
    walk_ast(T, [H|Acc]).

walk_clauses([], Acc) ->
    lists:reverse(Acc);
walk_clauses([{clause, Line, Arguments, Guards, Body}|T], Acc) ->
    walk_clauses(T, [{clause, Line, Arguments, Guards,
                      walk_body(Body, [])}|Acc]).

walk_body([], Acc) ->
    lists:reverse(Acc);
walk_body([H|T], Acc) ->
    walk_body(T, [do_transform(H)|Acc]).

do_transform({call, Line, {remote, Line1, {atom, Line2, emqx_broker},
                           {atom, Line3, Function}}, Arguments0}) ->
    io:format("Transform emqx_broker:~s !!!!~n", [Function]),
    {call, Line, {remote, Line1, {atom, Line2, emqx_ct_broker},
                  {atom, Line3, Function}}, Arguments0};
do_transform(Stmt) when is_tuple(Stmt) ->
    list_to_tuple(do_transform(tuple_to_list(Stmt)));
do_transform(Stmt) when is_list(Stmt) ->
    [do_transform(S) || S <- Stmt];
do_transform(Stmt) ->
    Stmt.


