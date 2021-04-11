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

-export([ docker_ip/2
         ,compose/5
         ,stop/1
         ,remove/1
         ,remove/2
         ,remove/3
         ,force_remove/1
         ,force_remove/2
        ]).

docker_ip( ContainerName, ipv4 ) ->
    inspect_network( ContainerName, ".IPAddress" );
docker_ip( ContainerName, ipv6 ) ->
    inspect_network( ContainerName, ".GlobalIPv6Address").

inspect_network( ContainerName, Item ) ->
    inspect( ContainerName, "{{range.NetworkSettings.Networks}}{{"++Item++"}}{{end}}" ).

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
    sh("docker stop " ++ ContainerName).

remove(ContainerName) -> remove(ContainerName, false).

remove(ContainerName, RemoveVolume) -> remove(ContainerName, RemoveVolume, false).

remove(ContainerName, RemoveVolume, true ) -> remove(ContainerName, RemoveVolume, " -f ");
remove(ContainerName, RemoveVolume, false) -> remove(ContainerName, RemoveVolume, "");
remove(ContainerName, true,         Force) -> remove(ContainerName, " -v ", Force);
remove(ContainerName, false,        Force) -> remove(ContainerName, "", Force);
remove(ContainerName, RemoveVolume, Force) when is_list(RemoveVolume), is_list(Force) ->
    sh("docker rm " ++ RemoveVolume ++ Force ++ ContainerName).


force_remove(ContainerName) -> remove(ContainerName, false, true).

force_remove(ContainerName, RemoveVolume) -> remove(ContainerName, RemoveVolume, true).

set_env_file(Env) ->
    { ok, CWD } = file:get_cwd(),
    Unique = erlang:unique_integer()*-1,
    Filename = lists:concat([ ".docker-env-", Unique ]),
    Path = filename:join([ CWD, Filename]),
    ok = file:write_file(Path, Env),
    { ok, Path }.

sh(Cmd) ->
    Options = [ use_stdio, stderr_to_stdout, eof, hide, exit_status, { parallelism, true } ],
    Port = open_port({spawn, Cmd}, Options),
    case get_data(Port, []) of
        {0, Output} ->
            OutputTrimmed = string:trim(Output),
            {ok, OutputTrimmed};
        {Code, Output} ->
            {error, {Code, Output}}
    end.

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
