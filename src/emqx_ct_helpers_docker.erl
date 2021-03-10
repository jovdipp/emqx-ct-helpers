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

-module(emqx_ct_helpers_docker).

-export([docker_ip/2, compose/5, stop/1]).

docker_ip( ContainerName, ipv4 ) ->
    inspect_network( ContainerName, ".IPAddress" );
docker_ip( ContainerName, ipv6 ) ->
    inspect_network( ContainerName, ".GlobalIPv6Address").

inspect_network( ContainerName, Item ) ->
    Response = inspect( ContainerName, "{{range.NetworkSettings.Networks}}{{"++Item++"}}{{end}}" ),
    { ok, Response }.

inspect( ContainerName, Inspection ) ->
    docker("inspect -f "++Inspection++" "++ContainerName).

docker(Cmd) ->
    sh("docker "++Cmd).

compose( File, Project_name, ServiceName, Args, Env ) ->
    { ok, EnvPath } = set_env_file(Env),
    Cmd = "docker-compose --project-name "++Project_name++" --env-file "++EnvPath++" "++Args
          ++" -f "++File++" up --detach "++ServiceName,
    sh(Cmd).

stop(ContainerName) ->
    Cmd = "docker stop " ++ ContainerName,
    sh(Cmd).

set_env_file(Env) ->
    { ok, CWD } = file:get_cwd(),
    Unique = erlang:unique_integer()*-1,
    Filename = lists:concat([ ".docker-env-", Unique ]),
    Path = filename:join([ CWD, Filename]),
    ok = file:write_file(Path, Env),
    { ok, Path }.

sh(Cmd0) ->
    Cmd = "sh -c \"" ++ esc(Cmd0) ++ "\"; echo $?",
    oscmd(Cmd).

esc([]) -> [];
esc([$" | Cmd]) -> [$\\, $" | esc(Cmd)];
esc("$(" ++ Cmd) -> [$\\, $$, $( | esc(Cmd)];
esc([C | Cmd]) -> [C | esc(Cmd)].

oscmd(Cmd) ->
    case parse(os:cmd(Cmd)) of
        {0, Output} -> Output;
        {N, Output} -> error({N, Output})
    end.

parse(Lines0) ->
    %% last line is alwyas a \n
    [$\n | Lines] = lists:reverse(Lines0),
    %% second last line is $?
    {LastLine, OutputLines} = lists:splitwith(fun (C) -> C =/= $\n end, Lines),
    {list_to_integer(lists:reverse(LastLine)), lists:reverse(OutputLines)}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

sh_test_() ->
    [ {"echo ab",
       fun() -> ?assertEqual("ab\n", sh("echo 'ab'")) end}
    , {"echo $",
       fun() -> ?assertEqual("$\n", sh("echo '$'")) end}
    , {"echo var",
       fun() ->
               os:putenv("AA", "abc"),
               ?assertEqual("abc\n", sh("echo ${AA}")) end}
    , {"echo sub cmd",
       fun() ->
               os:putenv("AAA", "abcd"),
               ?assertEqual("xabcd\n", sh("echo \"x$(echo $AAA)\"")) end}
    , {"non-zero exit",
       fun() -> ?assertError({42, "Ab\n"}, sh("echo Ab; exit 42")) end}
    ].
-endif.
