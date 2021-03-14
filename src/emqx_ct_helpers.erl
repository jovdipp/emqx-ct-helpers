%%--------------------------------------------------------------------
%% Copyright (c) 2019 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(emqx_ct_helpers).

-define(THIS_APP, ?MODULE).
-include_lib("common_test/include/ct.hrl").
-include("emqx_ct_helpers.hrl").

-type(special_config_handler() :: fun()).

-type(apps() :: list(atom())).

-export([ boot_modules/1
        , start_apps/1
        , start_apps/2
        , start_app/4
        , stop_apps/1
        , reload/2
        , app_path/2
        , deps_path/2
        , flush/0
        , flush/1
        ]).

-export([ ensure_mnesia_stopped/0
        , wait_for/4
        , change_emqx_opts/1
        , change_emqx_opts/2
        , client_ssl_twoway/0
        , client_ssl_twoway/1
        , client_ssl/0
        , client_ssl/1
        , wait_mqtt_payload/1
        , not_wait_mqtt_payload/1
        , render_config_file/2
        ]).


%%------------------------------------------------------------------------------
%% APIs
%%------------------------------------------------------------------------------

-spec(boot_modules(all|list(atom())) -> ok).
boot_modules(Mods) ->
    application:set_env(emqx, boot_modules, Mods).

-spec(start_apps(Apps :: apps()) -> ok).
start_apps(Apps) ->
    start_apps(Apps, fun(_) -> ok end).

-spec(start_apps(Apps :: apps(), Handler :: special_config_handler()) -> ok).
start_apps(Apps, Handler) when is_function(Handler) ->
    %% Load all application code to beam vm first
    %% Because, minirest, ekka etc.. application will scan these modules
    [application:load(App) || App <- Apps],
    [start_app(App, Handler) || App <- [emqx | Apps]],
    ok.

start_app(App, Handler) ->
    start_app(App,
              app_path(App, filename:join(["priv", atom_to_list(App) ++ ".schema"])),
              app_path(App, filename:join(["etc", atom_to_list(App) ++ ".conf"])),
              Handler).

mustache_vars(App) ->
    [{platform_data_dir, app_path(App, "data")},
     {platform_etc_dir,  app_path(App, "etc")},
     {platform_log_dir,  app_path(App, "log")},
     {platform_plugins_dir,  app_path(App, "plugins")}
    ].

start_app(App, SchemaFile, ConfigFile, SpecAppConfig) ->
    Vars = mustache_vars(App),
    RenderedConfigFile = render_config_file(ConfigFile, Vars),
    read_schema_configs(SchemaFile, RenderedConfigFile),
    SpecAppConfig(App),
    {ok, _} = application:ensure_all_started(App).

render_config_file(ConfigFile, Vars0) ->
    {ok, Temp} = file:read_file(ConfigFile),
    Vars = [{atom_to_list(N), iolist_to_binary(V)} || {N, V} <- Vars0],
    Targ = bbmustache:render(Temp, Vars),
    NewName = ConfigFile ++ ".rendered",
    ok = file:write_file(NewName, Targ),
    NewName.

read_schema_configs(SchemaFile, ConfigFile) ->
    %% ct:pal("Read configs - SchemaFile: ~p, ConfigFile: ~p", [SchemaFile, ConfigFile]),
    Schema = cuttlefish_schema:files([SchemaFile]),
    Conf = conf_parse:file(ConfigFile),
    NewConfig = cuttlefish_generator:map(Schema, Conf),
    lists:foreach(
        fun({App, Configs}) ->
            [application:set_env(App, Par, Value) || {Par, Value} <- Configs]
        end, NewConfig).

-spec(stop_apps(list()) -> ok).
stop_apps(Apps) ->
    [application:stop(App) || App <- Apps ++ [emqx, mnesia]].

%% backward compatible
deps_path(App, RelativePath) -> app_path(App, RelativePath).

app_path(App, RelativePath) ->
    ok = ensure_app_loaded(App),
    Lib = code:lib_dir(App),
    Priv0 = code:priv_dir(App),
    Priv = case file:read_link(Priv0) of
               {ok, Resolved} -> filename:join([Lib, Resolved]);
               {error, _} -> Priv0
           end,
    safe_relative_path(filename:join([Priv, "..", RelativePath])).

assert_app_loaded(App) ->
    case code:lib_dir(App) of
        {error, bad_name} -> error({not_loaded, ?THIS_APP});
        _ -> ok
    end.

ensure_app_loaded(?THIS_APP) ->
    ok = assert_app_loaded(?THIS_APP);
ensure_app_loaded(App) ->
    case code:lib_dir(App) of
        {error, bad_name} ->
            ok = assert_app_loaded(?THIS_APP),
            Dir0 = code:lib_dir(?THIS_APP),
            LibRoot = upper_level(Dir0),
            Dir = filename:join([LibRoot, atom_to_list(App), "ebin"]),
            case code:add_pathz(Dir) of
                true -> ok;
                {error, bad_directory} -> error({bad_directory, Dir})
            end,
            case application:load(App) of
                ok -> ok;
                {error, Reason} -> error({failed_to_load, App, Reason})
            end,
            ok = assert_app_loaded(App);
        _ ->
            ok
    end.

upper_level(Dir) ->
    Split = filename:split(Dir),
    UpperReverse = tl(lists:reverse(Split)),
    filename:join(lists:reverse(UpperReverse)).

safe_relative_path(Path) ->
    case filename:split(Path) of
        ["/" | T] ->
            T1 = do_safe_relative_path(filename:join(T)),
            filename:join(["/", T1]);
        _ ->
            do_safe_relative_path(Path)
    end.

do_safe_relative_path(Path) ->
    case safe_relative_path_2(Path) of
        unsafe -> Path;
        OK -> OK
    end.

-if(?OTP_RELEASE < 23).
safe_relative_path_2(Path) ->
    filename:safe_relative_path(Path).
-else.
safe_relative_path_2(Path) ->
    {ok, Cwd} = file:get_cwd(),
    filelib:safe_relative_path(Path, Cwd).
-endif.

-spec(reload(App :: atom(), SpecAppConfig :: special_config_handler()) -> ok).
reload(App, SpecAppConfigHandler) ->
    application:stop(App),
    start_app(App, SpecAppConfigHandler),
    application:start(App).

ensure_mnesia_stopped() ->
    ekka_mnesia:ensure_stopped(),
    ekka_mnesia:delete_schema().

%% Help function to wait for Fun to yield 'true'.
wait_for(Fn, Ln, F, Timeout) ->
    {Pid, Mref} = erlang:spawn_monitor(fun() -> wait_loop(F, catch_call(F)) end),
    wait_for_down(Fn, Ln, Timeout, Pid, Mref, false).

change_emqx_opts(SslType) ->
    change_emqx_opts(SslType, []).

change_emqx_opts(SslType, MoreOpts) ->
    {ok, Listeners} = application:get_env(emqx, listeners),
    NewListeners =
        lists:map(fun(Listener) ->
                          maybe_inject_listener_ssl_options(SslType, MoreOpts, Listener)
                  end, Listeners),
    application:set_env(emqx, listeners, NewListeners).

maybe_inject_listener_ssl_options(SslType, MoreOpts, {sll, Port, Opts}) ->
    %% this clause is kept to be backward compatible
    %% new config for listener is a map, old is a three-element tuple
    {ssl, Port, inject_listener_ssl_options(SslType, Opts, MoreOpts)};
maybe_inject_listener_ssl_options(SslType, MoreOpts, #{proto := ssl, opts := Opts} = Listener) ->
    Listener#{opts := inject_listener_ssl_options(SslType, Opts, MoreOpts)};
maybe_inject_listener_ssl_options(_SslType, _MoreOpts, Listener) ->
    Listener.

inject_listener_ssl_options(SslType, Opts, MoreOpts) ->
    SslOpts = proplists:get_value(ssl_options, Opts),
    Keyfile = app_path(emqx, filename:join(["etc", "certs", "key.pem"])),
    Certfile = app_path(emqx, filename:join(["etc", "certs", "cert.pem"])),
    TupleList1 = lists:keyreplace(keyfile, 1, SslOpts, {keyfile, Keyfile}),
    TupleList2 = lists:keyreplace(certfile, 1, TupleList1, {certfile, Certfile}),
    TupleList3 =
        case SslType of
            ssl_twoway ->
                CAfile = app_path(emqx, proplists:get_value(cacertfile, ?MQTT_SSL_TWOWAY)),
                MutSslList = lists:keyreplace(cacertfile, 1, ?MQTT_SSL_TWOWAY, {cacertfile, CAfile}),
                lists:merge(TupleList2, MutSslList);
            _ ->
                lists:filter(fun ({cacertfile, _}) -> false;
                                 ({verify, _}) -> false;
                                 ({fail_if_no_peer_cert, _}) -> false;
                                 (_) -> true
                             end, TupleList2)
        end,
    TupleList4 = emqx_misc:merge_opts(TupleList3, proplists:get_value(ssl_options, MoreOpts, [])),
    NMoreOpts = emqx_misc:merge_opts(MoreOpts, [{ssl_options, TupleList4}]),
    emqx_misc:merge_opts(Opts, NMoreOpts).

flush() ->
    flush([]).

flush(Msgs) ->
    receive
        M -> flush([M|Msgs])
    after
        0 -> lists:reverse(Msgs)
    end.

client_ssl_twoway() ->
    client_ssl_twoway(default).

client_ssl_twoway(TLSVsn) ->
    client_certs() ++ ciphers(TLSVsn).

%% Paths prepended to cert filenames
client_certs() ->
    [ { Key, app_path(emqx, FilePath) } || { Key, FilePath } <- ?MQTT_SSL_CLIENT_CERTS ].

client_ssl() ->
    client_ssl(default).

client_ssl(TLSVsn) ->
    ciphers(TLSVsn) ++ [{reuse_sessions, true}].

ciphers(default) -> []; %% determined via config file defaults
ciphers('tlsv1.3') -> ?TLS_1_3_CIPHERS;
ciphers(_OlderTLSVsn) -> ?TLS_OLD_CIPHERS.

wait_mqtt_payload(Payload) ->
    receive
        {publish, #{payload := Payload}} ->
            ct:pal("OK - received msg: ~p~n", [Payload])
    after 1000 ->
        ct:fail({timeout, Payload, {msg_box, flush()}})
    end.

not_wait_mqtt_payload(Payload) ->
    receive
        {publish, #{payload := Payload}} ->
            ct:fail({received, Payload})
    after 1000 ->
        ct:pal("OK - msg ~p is not received", [Payload])
    end.

wait_for_down(Fn, Ln, Timeout, Pid, Mref, Kill) ->
    receive
        {'DOWN', Mref, process, Pid, normal} ->
            ok;
        {'DOWN', Mref, process, Pid, {unexpected, Result}} ->
            erlang:error({unexpected, Fn, Ln, Result});
        {'DOWN', Mref, process, Pid, {crashed, {C, E, S}}} ->
            erlang:raise(C, {Fn, Ln, E}, S)
    after
        Timeout ->
            case Kill of
                true ->
                    erlang:demonitor(Mref, [flush]),
                    erlang:exit(Pid, kill),
                    erlang:error({Fn, Ln, timeout});
                false ->
                    Pid ! stop,
                    wait_for_down(Fn, Ln, Timeout, Pid, Mref, true)
            end
    end.

wait_loop(_F, ok) -> exit(normal);
wait_loop(F, LastRes) ->
    receive
        stop -> erlang:exit(LastRes)
    after
        100 ->
            Res = catch_call(F),
            wait_loop(F, Res)
    end.

catch_call(F) ->
    try
        case F() of
            true -> ok;
            Other -> {unexpected, Other}
        end
    catch
        C : E : S ->
            {crashed, {C, E, S}}
    end.
