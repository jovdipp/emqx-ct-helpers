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
	Config = override_function(FuncExists, Suite, ?CT_SUITE, []),
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
	end.

?CT_INIT_PER_SUITE(FuncExists, Suite, Config) ->
	case is_job_node() of
		true -> override_function(FuncExists, Suite, ?CT_INIT_PER_SUITE, [Config], Config);
		false -> Config
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
	GroupsAcc = [],
	MatrixReversed = lists:reverse(Matrix),
	Jobs = matrix_jobs(MatrixReversed, GroupsAcc),
	index_jobs(Jobs).

index_jobs(Jobs) ->
	index_jobs(Jobs, 0, []).

index_jobs([], _Index, Acc) ->
	Acc;
index_jobs([Job | Jobs], Index, Acc) ->
	AccUpdated = [Job#ct_job{index = Index} | Acc],
	index_jobs(Jobs, Index + 1, AccUpdated).

matrix_jobs([], Jobs) ->
	Jobs;
matrix_jobs([Row | Rows], AllJobs) ->
	AllJobsUpdated = combine(Row, AllJobs, []),
	matrix_jobs(Rows, AllJobsUpdated).

combine([], _Jobs, Acc) ->
	Acc;
combine(Row, [], _Acc) ->
	[ct_job(Item, <<"">>, []) || Item <- Row];
combine([Item | Rest], Jobs, Acc) ->
	ItemJobs = [ct_job(Item, Job#ct_job.name, Job#ct_job.tree) || Job <- Jobs],
	AccUpdated = ItemJobs ++ Acc,
	combine(Rest, Jobs, AccUpdated).

ct_job(Item, NamePrev, TreePrev) ->
	Name = job_name(Item, NamePrev),
	Tree = [Item | TreePrev],
	#ct_job{name = Name, tree = Tree}.

job_name(ItemName, Job) ->
	ItemNameBin = atom_to_binary(ItemName, utf8),
	Join = case Job of
		       <<"">> -> <<"">>;
		       Job -> <<"_">>
	       end,
	<<ItemNameBin/binary, Join/binary, Job/binary>>.

ct_master_orchestration(_FuncExists, Suite, _Config) ->
	Jobs = matrix_jobs(Suite:job_matrix()),
	{ok, SpecFilename} = emqx_ct_helpers_jobs:generate_specfile(Suite, Jobs),
	ct_master:run(SpecFilename),
	case emqx_ct_helpers_jobs:parse_results(Suite, Jobs) of
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

%% This should be improved in future, due to the Job Matrix not in the SUITE config
%% the matrix structure has to be re-generated
intl_end_per_job(Suite, Config) ->
	JobName = list_to_binary(get_name()),
	Jobs = matrix_jobs(Suite:?JOB_MATRIX()),
	Job = lists:keyfind(JobName, 2, Jobs),
	Suite:?END_PER_JOB(Job, Config).
