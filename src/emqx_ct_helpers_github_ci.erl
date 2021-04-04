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

-module(emqx_ct_helpers_github_ci).

-include_lib("../include/emqx_ct_job.hrl").

%% API
-export([main/1]).

%% Caution : this function will result in halting the VM
main([ Cmd | Args ]) ->
    ensure_code_paths(),
    JsonStr = cmd(Cmd, Args),
    io:format(JsonStr),
    erlang:halt().

cmd("get-matrix", [ SuiteStr | _Args ]) ->
    get_matrix(SuiteStr);
cmd(Cmd, _Args) ->
    io:format("error : unknown command [~p]",[Cmd]),
    erlang:halt(1).

ensure_code_paths() ->
    PlatformRoot = emqx_ct_util:get_platform_root_dir(),
    Paths = filelib:wildcard(PlatformRoot++"/**/{ebin,test}"),
    code:add_pathsa(Paths).

get_matrix(SuiteStr) ->
    Suite = list_to_atom(SuiteStr),
    Matrix = Suite:job_matrix(),
    Jobs = emqx_ct_helpers_jobs:matrix_jobs(Matrix),
    JsonJobs = jobs_to_json_structs(Jobs),
    jiffy:encode(JsonJobs).

jobs_to_json_structs(Jobs) ->
    [ job_to_struct(Job) || Job <- Jobs ].

%% explicitly extracting fields to ensure future ct_job field changes
%% do not affect export formatting
job_to_struct(#ct_job{name=Name, vectors=VectorsAtoms, index=Index}) ->
    Vectors = [to_binary(Term)||Term <-VectorsAtoms],
    {[{name,Name},{vectors,Vectors},{index,Index}]}.

to_binary(Atom) -> atom_to_binary(Atom,utf8);
to_binary(Int) -> integer_to_binary(Int).

%% -------------------------------------------------------------------------------------------------
%%  Eunit
%% =================================================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

matrix_as_json_str_test() ->
    JsonStr = <<"[{\"name\":\"mysqlV8_tls_ipv4\",\"vectors\":[\"mysqlV8\",\"tls\",\"ipv4\"],",
                "\"index\":0},{\"name\":\"mysqlV8_tls_ipv6\",\"vectors\":[\"mysqlV8\",\"tls\",",
                "\"ipv6\"],\"index\":1},{\"name\":\"mysqlV8_tcp_ipv4\",\"vectors\":[\"mysqlV8\",",
                "\"tcp\",\"ipv4\"],\"index\":2},{\"name\":\"mysqlV8_tcp_ipv6\",\"vectors\":",
                "[\"mysqlV8\",\"tcp\",\"ipv6\"],\"index\":3},{\"name\":\"mysqlV5_7_tls_ipv4\",",
                "\"vectors\":[\"mysqlV5_7\",\"tls\",\"ipv4\"],\"index\":4},{\"name\":",
                "\"mysqlV5_7_tls_ipv6\",\"vectors\":[\"mysqlV5_7\",\"tls\",\"ipv6\"],",
                "\"index\":5},{\"name\":\"mysqlV5_7_tcp_ipv4\",\"vectors\":[\"mysqlV5_7\",\"tcp\",",
                "\"ipv4\"],\"index\":6},{\"name\":\"mysqlV5_7_tcp_ipv6\",\"vectors\":",
                "[\"mysqlV5_7\",\"tcp\",\"ipv6\"],\"index\":7}]">>,
    ?assertEqual(JsonStr, get_matrix("emqx_jobs_SUITE")).

-endif.
