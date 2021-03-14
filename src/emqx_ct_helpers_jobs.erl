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

-module(emqx_ct_helpers_jobs).

-export([matrix_jobs/1, surefire_out_path/2, generate_specfile/2, get_name/0, parse_results/2,
         is_job_node/0, node_name/1, ct_master_orchestration/3, repo_root/0, container_ip/1,
         container_ip/2, test_platform_host/0]).

-include("emqx_ct_job.hrl").
-include("emqx_ct_jobs_transform.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%% Forwarded Functions
-export([?CT_ALL/2, ?CT_SUITE/2, ?CT_INIT_PER_SUITE/3, ?CT_END_PER_SUITE/3,
         ?CT_INIT_PER_GROUP/4, ?CT_END_PER_GROUP/4,
         ?JOB_MATRIX/2, ?INIT_PER_JOB/3, ?END_PER_JOB/4, ?JOB_OPTIONS/3]).

-define(SUCCESSFUL, successful).
-define(ERROR_TYPE_FAILED_TEST, failed_tests).
-define(JOBS_NODE_PREFIX, <<"job-matrix-test-node-">>).


%% -------------------------------------------------------------------------------------------------
%%  Forwarded Functions ( Macro used to keep in sync with emqx_ct_jobs_suite_transform )
%% =================================================================================================

?JOB_MATRIX(FuncExists, Suite) ->
	override_function(FuncExists, Suite, ?JOB_MATRIX, []).

?INIT_PER_JOB(FuncExists, Suite, Job) ->
	override_function(FuncExists, Suite, ?INIT_PER_JOB, [Job], ok).

?END_PER_JOB(FuncExists, Suite, Job, Config) ->
	override_function(FuncExists, Suite, ?END_PER_JOB, [Job, Config], ok).

?JOB_OPTIONS(FuncExists, Suite, Job) ->
	override_function(FuncExists, Suite, ?JOB_OPTIONS, [Job], []).

?CT_ALL(FuncExists, Suite) ->
	case is_job_node() of
		true -> override_function(FuncExists, Suite, ?CT_ALL, []);
		false -> [ct_master_orchestration]
	end.

?CT_SUITE(FuncExists, Suite) ->
	try override_function(FuncExists, Suite, ?CT_SUITE, []) of
		Config ->
			Name = get_name(),
			SureFirePath = surefire_out_path(Suite, Name),
			SureFireHook = {cth_surefire, [{path, SureFirePath}]},
			case lists:keyfind(ct_hooks, 1, Config) of
				{ct_hooks, Hooks} ->
					case lists:keymember(cth_surefire, 1, Hooks) of
						true ->
							Config;
						false ->
							CTHooks = {ct_hooks, [SureFireHook | Hooks]},
							lists:keyreplace(ct_hooks, 1, Config, CTHooks)
					end;
				false ->
					[{ct_hooks, [SureFireHook]} | Config]
			end
	catch
		Type:Exception:Stack ->
			intl_end_per_job(Suite, { Type, Exception } ),
			exit(Exception, Stack)
	end.

?CT_INIT_PER_SUITE(FuncExists, Suite, Config) ->
	Jobs = matrix_jobs(Suite:?JOB_MATRIX()),
	JobName = list_to_binary(get_name()),
	CurrentJob = lists:keyfind(JobName, 2, Jobs),
	JobsConfig = [ { job, CurrentJob }, { all_jobs, Jobs } ],
	ConfigUpdated = [ { ?JOBS_MATRIX_CONFIG, JobsConfig } | Config ],
	case is_job_node() of
		true -> override_function(FuncExists, Suite, ?CT_INIT_PER_SUITE, [ConfigUpdated], Config);
		false -> ConfigUpdated
	end.

?CT_END_PER_SUITE(FuncExists, Suite, Config) ->
	case is_job_node() of
		true ->
			override_function(FuncExists, Suite, ?CT_END_PER_SUITE, [Config], ok),
			intl_end_per_job(Suite, Config);
		false ->
			ok
	end.

?CT_INIT_PER_GROUP(FuncExists, Suite, GroupName, Config) ->
	case is_job_node() of
		true -> override_function(FuncExists, Suite, ?CT_INIT_PER_GROUP, [GroupName, Config], Config);
		false -> Config
	end.

?CT_END_PER_GROUP(FuncExists, Suite, GroupName, Config) ->
	case is_job_node() of
		true -> override_function(FuncExists, Suite, ?CT_END_PER_GROUP, [GroupName, Config], ok);
		false -> ok
	end.


%% -------------------------------------------------------------------------------------------------
%%  Matrix
%% =================================================================================================

matrix_jobs(Matrix) ->
	matrix_into_jobs(Matrix).

matrix_into_jobs(Matrix) ->
	Vectors = enum_dimentions(Matrix),
	make_jobs(Vectors).

make_jobs(Vectors) ->
	make_jobs(Vectors, 0, []).

make_jobs([], _Index, Acc) -> lists:reverse(Acc);
make_jobs([Vector | Vectors], Index, Acc) ->
    Job = #ct_job{ name = make_name(Vector)
                 , tree = Vector
                 , index = Index
                 },
	make_jobs(Vectors, Index + 1, [Job | Acc]).

make_name(Atoms) ->
    iolist_to_binary(infix([atom_to_binary(A, utf8) || A <- Atoms], "_")).

infix([I], _) -> [I];
infix([H | T], In) -> [H, In | infix(T, In)].

%% enumerate all dimentions of a matrix
%% sample  input: [[a,b], [1], [x,y,z]]
%% sample output: [[a,1,x],[a,1,y],[a,1,z],[b,1,x],[b,1,y],[b,1,z]]
enum_dimentions(Matrix) ->
    enum_dimentions(Matrix, [[]]).

enum_dimentions([], Vectors) ->
    lists:map(fun lists:reverse/1, Vectors);
enum_dimentions([Row | Rows], Vectors) ->
    NewVec = [[Col | Vector] || Vector <- Vectors, Col <- Row],
    enum_dimentions(Rows, NewVec).

ct_master_orchestration(_FuncExists, Suite, Config) ->
	JobsConfig = proplists:get_value( ?JOBS_MATRIX_CONFIG, Config ),
	Jobs = proplists:get_value( all_jobs, JobsConfig ),
	{ok, SpecFilename} = generate_specfile(Suite, Jobs),
	ct_master:run(SpecFilename),
	case parse_results(Suite, Jobs) of
		{error, Errors} ->
			exit({failed_tests, Errors});
		{ok, successful} ->
			{ok, successful}
	end.

%% -------------------------------------------------------------------------------------------------
%%  Job Generation
%% =================================================================================================

generate_specfile(SuiteModule, Jobs) ->
	BaseDir = jobs_dir(),
	LogDir = dir(BaseDir, "logs"),
	SuiteDir = suite_dir(SuiteModule),
	ChildNodes = generate_child_nodes(Jobs, SuiteModule),
	SpecFilename = spec_filename(SuiteModule, BaseDir),
	Suite = atom_to_binary(SuiteModule, utf8),

	Spec = [ChildNodes,
	        <<"{logdir, all_nodes, \"">>, LogDir, <<"\"}.\n">>,
	        <<"{logdir, master, \"">>, LogDir, <<"\"}.\n">>,
	        <<"{suites, all_nodes, \"">>, SuiteDir, <<"\", ">>, Suite, <<"}.\n">>],
	ok = file:write_file(SpecFilename, Spec),
	{ok, SpecFilename}.

spec_filename(SuiteModule, BaseDir) ->
	Filename = lists:concat([SuiteModule, ".generated.tmp.spec"]),
	filename:join([BaseDir, Filename]).

generate_child_nodes(Jobs, SuiteModule) ->
	[child_node_spec(Job, SuiteModule) || Job <- Jobs].

child_node_spec(#ct_job{name = Name} = Job, SuiteModule) ->
	Node = node_name(Name),
	%% Starting Job Deps to allow Options to include Dep runtime info ( i.e. Docker Networking )
	SuiteModule:?INIT_PER_JOB(Job),
	Options = options(Job, SuiteModule),
	[<<"{node,">>, Name, <<",'">>, Node, <<"'}.\n">>,
	 <<"{init,">>, Name, <<",[{node_start,">>, Options, <<"}]}.\n">>].

options(Job, SuiteModule) ->
	Opts = SuiteModule:job_options(Job),
	CodePaths = {startup_functions, [{code, add_paths, [code:get_path()]}]},
	OptsUpdated = [{monitor_master, true}, CodePaths | Opts],
	OptsStr = io_lib:format("~1p", [OptsUpdated]),
	list_to_binary(lists:flatten(OptsStr)).

node_name(Name) ->
	Localhost = net_adm:localhost(),
	Hostname = list_to_binary(Localhost),
	<<?JOBS_NODE_PREFIX/binary, Name/binary, "@", Hostname/binary>>.

jobs_dir() ->
	{ok, CWD} = file:get_cwd(),
	case is_job_node() of
		false -> dir(CWD, "jobs");
		true -> filename:join([dir(CWD, "jobs"), "..", "..", ".."])
	end.

dir(Path, Dirname) ->
	Dir = filename:join([Path, Dirname, "."]),
	ok = filelib:ensure_dir(Dir),
	Dir.

suite_dir(SuiteModule) ->
	Filename = atom_to_list(SuiteModule) ++ ".erl",
	FilenamePath = code:where_is_file(Filename),
	filename:dirname(FilenamePath).

is_job_node() ->
	Node = node(),
	NodeStr = atom_to_list(Node),
	lists:prefix(binary_to_list(?JOBS_NODE_PREFIX), NodeStr).

repo_root() ->
	{ok, CWD} = file:get_cwd(),
	[BaseDir | _] = string:split(CWD, "_build"),
	{ok, BaseDir}.

get_name() ->
	Node = atom_to_list(node()),
	[Name, _Host] = string:tokens(Node, "@"),
	case string:prefix(Name, binary_to_list(?JOBS_NODE_PREFIX)) of
		nomatch -> Name;
		NameCleaned -> NameCleaned
	end.

%% -------------------------------------------------------------------------------------------------
%%  Result Parsing
%% =================================================================================================

parse_results(SuiteModule, Jobs) ->
	parse_results(SuiteModule, Jobs, []).

parse_results(_SuiteModule, [], []) ->
	{ok, ?SUCCESSFUL};
parse_results(_SuiteModule, [], Errors) ->
	{error, Errors};
parse_results(SuiteModule, [#ct_job{name = JobName} | Jobs], Errors) ->
	Name = binary_to_list(JobName),
	SureFirePath = surefire_out_path(SuiteModule, Name),
	case xmerl_scan:file(SureFirePath) of
		{error, enoent} ->
			ErrorsUpdated = update_error(Errors, no_results, JobName, SureFirePath),
			parse_results(SuiteModule, Jobs, ErrorsUpdated);
		{Xml, _Rest} ->
			Select = "testsuite/@failures|testsuite/@errors|testsuite/@skipped",
			SuiteRes = xmerl_xpath:string(Select, Xml),
			FailureTotals = [{Attr#xmlAttribute.name, Attr#xmlAttribute.value} || Attr <- SuiteRes],
			case FailureTotals of
				[{_1, "0"}, {_2, "0"}, {_3, "0"}] ->
					parse_results(SuiteModule, Jobs, Errors);
				FailureTotals ->
					ErrorsUpdated = update_error(Errors, ?ERROR_TYPE_FAILED_TEST, JobName,
					                             FailureTotals),
					parse_results(SuiteModule, Jobs, ErrorsUpdated)
			end
	end.

update_error(Errors, Type, JobName, Info) -> [{Type, JobName, Info} | Errors].

override_function(FuncExists, Suite, Name, Default) ->
	override_function(FuncExists, Suite, Name, [], Default).

override_function(true, Suite, Name, Args, _Default) ->
	OverrideFunction = emqx_ct_jobs_suite_transform:intl_function_name(Name),
	try apply(Suite, OverrideFunction, Args) of
		Success -> Success
	catch
		_Type:Exception:Stack ->
			io:format("~n Exception : ~p ", [Exception]),
			exit(Exception, Stack)
	end;
override_function(_DoesNotExist, _Suite, _Name, _Args, Default) ->
	Default.

%% -------------------------------------------------------------------------------------------------
%%  Surefire Injection
%% =================================================================================================

surefire_out_path(SuiteModule, Name) ->
	JobsDir = jobs_dir(),
	SureFireName = atom_to_list(SuiteModule) ++ "-" ++ Name ++ "-surefire.xml",
	Path = filename:join([JobsDir, "results", SureFireName]),
	filelib:ensure_dir(Path),
	Path.

%% -------------------------------------------------------------------------------------------------
%%  Networking
%% =================================================================================================

container_ip(ContainerName) ->
	container_ip(ContainerName, host).

container_ip(ContainerName, Type) ->
	{ok, Platform} = test_platform_host(),
	case Platform of
		local ->
			{ok, localhost(Type)}; %% Using local port mapping to access service on container
		"github_action_docker" ->
			dockerhost(ContainerName, Type)
	end.

localhost(ipv6)   -> "::1";
localhost(ipv4)   -> "127.0.0.1";
localhost(_Other) -> "localhost".

dockerhost(ContainerName, host) ->
	{ok, ContainerName};
dockerhost(ContainerName, IP_VSN) ->
	emqx_ct_helpers_docker:docker_ip(ContainerName, IP_VSN).

test_platform_host() ->
	TEST_HOST_PLATFORM = os:getenv("TEST_HOST_PLATFORM"),
	case TEST_HOST_PLATFORM of
		false -> {ok, local};
		Value -> {ok, Value}
	end.

%% -------------------------------------------------------------------------------------------------
%%  Helpers
%% =================================================================================================

intl_end_per_job(Suite, Config) ->
	JobsConfig = proplists:get_value( ?JOBS_MATRIX_CONFIG, Config ),
	Job = proplists:get_value( job, JobsConfig ),
	Suite:?END_PER_JOB(Job, Config).

%% -------------------------------------------------------------------------------------------------
%%  Eunit
%% =================================================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

enum_dimentions_1_test() ->
    ?assertEqual([[a], [b]], enum_dimentions([[a, b]])).

enum_dimentions_test() ->
    Matrix = [[a, b], [1], [x, y, z]],
    ?assertEqual([[a, 1, x],
                  [a, 1, y],
                  [a, 1, z],
                  [b, 1, x],
                  [b, 1, y],
                  [b,1,z]], enum_dimentions(Matrix)).

make_name_test_() ->
    [ {"one atom", ?_assertEqual(<<"a">>, make_name([a]))}
    , {"tow atoms", ?_assertEqual(<<"a_b">>, make_name([a,b]))}
    , {"three atoms", ?_assertEqual(<<"a_b_c">>, make_name([a,b,c]))}
    ].

-endif.
