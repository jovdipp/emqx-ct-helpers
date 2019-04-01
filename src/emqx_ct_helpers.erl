%%%===================================================================
%%% Copyright (c) 2013-2019 EMQ Inc. All rights reserved.
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

-module(emqx_ct_helpers).

-include_lib("common_test/include/ct.hrl").

-export([ set_config/1
        , run_setup_steps/2
        , reload/2
        , start_apps/1
        , start_apps/2
        , stop_apps/1
        ]).

set_config(Config) when is_list(Config) ->
    set_config(Config, []).

set_config([], Acc) ->
    Acc;
set_config([{App, SchemaPath, ConfPath} | ConfigInfo], Acc) ->
    set_config(ConfigInfo, [{App, path(SchemaPath), path(ConfPath)} | Acc]).

path(RelativePath) ->
    path(undefined, RelativePath).

path(undefined, RelativePath) ->
    PluginDepsPath = plugin_dep_dir(),
    PluginPath = filename:dirname(PluginDepsPath),
    filename:join([PluginPath, RelativePath]);
path(App, RelativePath) ->
    PluginDepsPath = plugin_dep_dir(),
    PluginPath = filename:dirname(PluginDepsPath),
    CurrentPluginPathNmae = filename:basename(PluginPath),
    case l2b(CurrentPluginPathNmae) =:= App of
        true -> filename:join([PluginPath, RelativePath]);
        false -> filename:join([PluginDepsPath, App, RelativePath])
    end.

plugin_dep_dir() ->
    filename:dirname(get_base_dir(?MODULE)).

get_base_dir(App) ->
    {file, Here} = code:is_loaded(App),
    filename:dirname(filename:dirname(Here)).

run_setup_steps(Config, Opts) when is_list(Config) ->
    [start_app(App, {SchemaFile, ConfigFile}, Opts) || {App, SchemaFile, ConfigFile} <- Config].

start_apps(Apps) ->
    start_apps(Apps, []).

start_apps([], _Opts) ->
    ok;
start_apps([App | LeftApps], Opts) ->
    SchemaFile = path(App, filename:join(["priv", atom_to_list(App) ++ ".schema"])),
    ConfigFile = path(App, filename:join(["etc", atom_to_list(App) ++ ".conf"])),
    start_app(App, {SchemaFile, ConfigFile}, Opts),
    start_apps(LeftApps, Opts).

stop_apps(Apps) ->
    [application:stop(App) || App <- Apps].

start_app(App, {SchemaFile, ConfigFile}, Opts) ->
    read_schema_configs(App, {SchemaFile, ConfigFile}),
    set_special_configs(App, Opts),
    application:ensure_all_started(App).

read_schema_configs(App, {SchemaFile, ConfigFile}) ->
    ct:pal("Read configs - SchemaFile: ~p, ConfigFile: ~p", [SchemaFile, ConfigFile]),
    Schema = cuttlefish_schema:files([SchemaFile]),
    Conf = conf_parse:file(ConfigFile),
    NewConfig = cuttlefish_generator:map(Schema, Conf),
    Vals = proplists:get_value(App, NewConfig, []),
    [application:set_env(App, Par, Value) || {Par, Value} <- Vals].

set_special_configs(emqx, Opts) when is_list(Opts) ->
    case Opts of
        [] -> ok;
        Opts -> [application:set_env(emqx, Par, path(App, Dir)) || {Par, App, Dir} <- Opts]
    end;
set_special_configs(_App, _Opts) ->
    ok.

reload(APP, {Par, Vals}) when is_atom(APP), is_list(Vals) ->
    application:stop(APP),
    {ok, TupleVals} = application:get_env(APP, Par),
    NewVals = lists:filtermap(fun({K, V}) ->
                                  case lists:keymember(K, 1, Vals) of
                                      false -> {true, {K, V}};
                                      _ -> false
                                  end
                              end, TupleVals),
    application:set_env(APP, Par, lists:append(NewVals, Vals)),
    application:start(APP).

l2b(L) when is_list(L) ->
    list_to_atom(L);
l2b(L) ->
    L.
