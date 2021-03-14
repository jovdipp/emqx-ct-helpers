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

-export([docker_ip/2, compose/5, stop/1, remove/1]).

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

remove(ContainerName) ->
    Cmd = "docker rm " ++ ContainerName,
    sh(Cmd).

set_env_file(Env) ->
    { ok, CWD } = file:get_cwd(),
    Unique = erlang:unique_integer()*-1,
    Filename = lists:concat([ ".docker-env-", Unique ]),
    Path = filename:join([ CWD, Filename]),
    ok = file:write_file(Path, Env),
    { ok, Path }.

sh(Cmd) ->
    Port = open_port({spawn, Cmd}, [ use_stdio, stderr_to_stdout, eof, hide, exit_status, { parallelism, true } ]),
    get_data(Port, []).

get_data(Port, Sofar) ->
    receive
        {Port, {data, Bytes}} ->
            get_data(Port, [Sofar|Bytes]);
        {Port, eof} ->
            Port ! {self(), close},
            receive
                { Port, closed } ->
                    true
            end,
            receive
                {'EXIT',  Port,  _} ->
                    ok
            after 1 ->
                ok
            end,
            ExitCode = receive
                           { Port, { exit_status, Code } } ->
                               Code
                       end,
            { ExitCode, lists:flatten( Sofar) }
    end.