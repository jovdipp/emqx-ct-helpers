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

-module(emqx_jobs_SUITE).

%% API
-compile(export_all).
-compile(nowarn_export_all).
-compile([{parse_transform, emqx_ct_jobs_suite_transform}]).

-include("emqx_ct_job.hrl").
-include_lib("common_test/include/ct.hrl").

-define(JOB_MYSQLV8, mysqlV8).
-define(JOB_MYSQLV5_7, mysqlV5_7).
-define(JOB_TCP, tcp).
-define(JOB_TLS, tls).
-define(JOB_IPV4, ipv4).
-define(JOB_IPV6, ipv6).

all() ->
    [t_test_running_in_each_job].

%% ( Multiple configurations concurrently in unique nodes )
job_matrix() ->
    [ [ ?JOB_MYSQLV8, ?JOB_MYSQLV5_7 ],
      [ ?JOB_TLS, ?JOB_TCP ],
      [ ?JOB_IPV4, ?JOB_IPV6 ] ].

init_per_job( #ct_job{ name=Name, tree=[ VSN, _Connection, _IPv ], index = Index }) ->
    Env = docker_env(Name, VSN, Index),
    File = docker_compose_file(),
    JobName = binary_to_list(Name),
    emqx_ct_helpers_docker:compose( File, JobName, "mysql_server", "", Env ),
    ok.

end_per_job( #ct_job{ name=NameBin }, _Config ) ->
    JobName = binary_to_list(NameBin),
    emqx_ct_helpers_docker:stop(JobName),
    ok.

job_options( #ct_job{name =NameBin, tree =[ _VSN, tls, IPv ], index = Index } ) ->
    AllJobsEnv = all_jobs_env(Index),
    JobName = binary_to_list(NameBin),
    TlsEnvs = [ server_env( JobName, IPv, Index ),
                { "EMQX_AUTH__MYSQL__SSL", "on" },
                { "EMQX_AUTH__MYSQL__SSL__CACERTFILE","/emqx/apps/emqx_auth_mysql/test/emqx_auth_mysql_SUITE_data/ca.pem" },
                { "EMQX_AUTH__MYSQL__SSL__CERTFILE","/emqx/apps/emqx_auth_mysql/test/emqx_auth_mysql_SUITE_data/client-cert.pem" },
                { "EMQX_AUTH__MYSQL__SSL__KEYFILE","/emqx/apps/emqx_auth_mysql/test/emqx_auth_mysql_SUITE_data/client-key.pem" }
              ],
    Envs = AllJobsEnv ++ TlsEnvs,
    [ { env, Envs } ];
job_options( #ct_job{name =NameBin, tree =[ _VSN, tcp, IPv ], index = Index } ) ->
    AllJobsEnv = all_jobs_env(Index),
    JobName = binary_to_list(NameBin),
    TcpEnvs = [ server_env( JobName, IPv, Index ),
                { "EMQX_AUTH__MYSQL__SSL", "off" } ],
    Envs = AllJobsEnv ++ TcpEnvs,
    [ { env, Envs } ];
job_options(#ct_job{ index = Index }) ->
    [ { env, all_jobs_env(Index) }].

init_per_suite(Config) ->
    io:format("~n Config : ~p ", [ Config ] ),
    [{ additional, job_only }].

end_per_suite(Config) ->
    ok.

t_test_running_in_each_job(_Config) ->
    EnvValues = os:getenv(),
    [ #ct_job{ name=JobName } | _ ] = emqx_ct_helpers_jobs:matrix_jobs(job_matrix()),
    JobNode = binary_to_atom(emqx_ct_helpers_jobs:node_name(JobName ), utf8 ),
    case node() of
        JobNode -> exit(failing_test_me_for_first_job_name);
        _Other -> ok
    end.

%% -------------------------------------------------------------------------------------------------
%% INTERNAL HELPERS
%% =================================================================================================

server_env( ContainerName, IPv, Index ) ->
    { ok, SERVER_IP } = emqx_ct_helpers_jobs:container_ip(ContainerName, IPv),
    Port = ensure_port(Index, 3306),
    PortStr = integer_to_list(Port),
    { "EMQX_AUTH__MYSQL__SERVER", SERVER_IP++":"++PortStr }.

all_jobs_env( Index ) ->
    Port = ensure_port(Index, 8084),
    PortStr = integer_to_list(Port),
    [{ "EMQX_LISTENER__WSS__EXTERNAL", PortStr },
     { "EMQX_AUTH__MYSQL__USERNAME","root" },
     { "EMQX_AUTH__MYSQL__PASSWORD","public" },
     { "EMQX_AUTH__MYSQL__DATABASE","mqtt" },
     { "CUTTLEFISH_ENV_OVERRIDE_PREFIX","EMQX_" }].

mysql_vsn(?JOB_MYSQLV8) -> "8";
mysql_vsn(?JOB_MYSQLV5_7) -> "5.7".

docker_env( JobName, VSN, Index ) ->
    Port = ensure_port(Index, 3306),
    PortStr = integer_to_list(Port),
    MYSQL_TAG = mysql_vsn(VSN),
    JobNameStr = binary_to_list(JobName),
    Env = "MYSQL_CONTAINER_NAME=" ++ JobNameStr ++
          "\nMYSQL_TAG=" ++ MYSQL_TAG ++
          "\nMAP_PORT=" ++ PortStr,
    Env.

docker_compose_file() ->
    {ok, RepoRoot} = emqx_ct_helpers_jobs:repo_root(),
    File = filename:join([RepoRoot, ".ci", "compatibility_tests", "docker-compose-mysql.yaml"]),
    File.

ensure_port( Index, Port ) ->
    { ok, Platform } = emqx_ct_helpers_jobs:test_platform_host(),
    case Platform of
        local  -> Port * 10 + Index;
        _Other -> Port
    end.
