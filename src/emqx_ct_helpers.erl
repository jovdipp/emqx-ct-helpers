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

-type(special_config_handler() :: fun()).

-type(apps() :: list(atom())).

-export([ start_apps/1
        , start_apps/2
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
        , client_ssl_twoway/0
        , client_ssl/0
        , wait_mqtt_payload/1
        , not_wait_mqtt_payload/1
        , render_config_file/2
        ]).

-define(CIPHERS, [{ciphers,
                   ["ECDHE-ECDSA-AES256-GCM-SHA384",
                    "ECDHE-RSA-AES256-GCM-SHA384",
                    "ECDHE-ECDSA-AES256-SHA384",
                    "ECDHE-RSA-AES256-SHA384","ECDHE-ECDSA-DES-CBC3-SHA",
                    "ECDH-ECDSA-AES256-GCM-SHA384",
                    "ECDH-RSA-AES256-GCM-SHA384",
                    "ECDH-ECDSA-AES256-SHA384","ECDH-RSA-AES256-SHA384",
                    "DHE-DSS-AES256-GCM-SHA384","DHE-DSS-AES256-SHA256",
                    "AES256-GCM-SHA384","AES256-SHA256",
                    "ECDHE-ECDSA-AES128-GCM-SHA256",
                    "ECDHE-RSA-AES128-GCM-SHA256",
                    "ECDHE-ECDSA-AES128-SHA256",
                    "ECDHE-RSA-AES128-SHA256",
                    "ECDH-ECDSA-AES128-GCM-SHA256",
                    "ECDH-RSA-AES128-GCM-SHA256",
                    "ECDH-ECDSA-AES128-SHA256","ECDH-RSA-AES128-SHA256",
                    "DHE-DSS-AES128-GCM-SHA256","DHE-DSS-AES128-SHA256",
                    "AES128-GCM-SHA256","AES128-SHA256",
                    "ECDHE-ECDSA-AES256-SHA","ECDHE-RSA-AES256-SHA",
                    "DHE-DSS-AES256-SHA","ECDH-ECDSA-AES256-SHA",
                    "ECDH-RSA-AES256-SHA","AES256-SHA",
                    "ECDHE-ECDSA-AES128-SHA","ECDHE-RSA-AES128-SHA",
                    "DHE-DSS-AES128-SHA","ECDH-ECDSA-AES128-SHA",
                    "ECDH-RSA-AES128-SHA","AES128-SHA"]}]).

-define(MQTT_SSL_TWOWAY, [{cacertfile, filename:join(["etc", "certs", "cacert.pem"])},
                          {verify, verify_peer},
                          {fail_if_no_peer_cert, true}]).

-define(MQTT_SSL_CLIENT, [{keyfile, filename:join(["etc", "certs", "client-key.pem"])},
                          {cacertfile, filename:join(["etc", "certs", "cacert.pem"])},
                          {certfile, filename:join(["etc", "certs", "client-cert.pem"])}]).

%%------------------------------------------------------------------------------
%% APIs
%%------------------------------------------------------------------------------

-spec(start_apps(Apps :: apps()) -> ok).
start_apps(Apps) ->
    start_apps(Apps, fun(_) -> ok end).

-spec(start_apps(Apps :: apps(), Handler :: special_config_handler()) -> ok).
start_apps(Apps, Handler) when is_function(Handler) ->
    %% Load all application code to beam vm first
    %% Becasue, minirest, ekka etc.. application will scan these modules
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
    read_schema_configs(App, SchemaFile, RenderedConfigFile),
    SpecAppConfig(App),
    {ok, _} = application:ensure_all_started(App).

render_config_file(ConfigFile, Vars0) ->
    {ok, Temp} = file:read_file(ConfigFile),
    Vars = [{atom_to_list(N), iolist_to_binary(V)} || {N, V} <- Vars0],
    Targ = bbmustache:render(Temp, Vars),
    NewName = ConfigFile ++ ".rendered",
    ok = file:write_file(NewName, Targ),
    NewName.

read_schema_configs(App, SchemaFile, ConfigFile) ->
    ct:pal("Read configs - SchemaFile: ~p, ConfigFile: ~p", [SchemaFile, ConfigFile]),
    Schema = cuttlefish_schema:files([SchemaFile]),
    Conf = conf_parse:file(ConfigFile),
    NewConfig = cuttlefish_generator:map(Schema, Conf),
    Vals = proplists:get_value(App, NewConfig, []),
    [application:set_env(App, Par, Value) || {Par, Value} <- Vals].

-spec(stop_apps(list()) -> ok).
stop_apps(Apps) ->
    [application:stop(App) || App <- Apps ++ [emqx]].

%% backward compatible
deps_path(App, RelativePath) -> app_path(App, RelativePath).

app_path(App, RelativePath) ->
    Lib = code:lib_dir(App),
    Priv0 = code:priv_dir(App),
    Priv = case file:read_link(Priv0) of
               {ok, Resolved} -> filename:join([Lib, Resolved]);
               {error, _} -> Priv0
           end,
    safe_relative_path(filename:join([Priv, "..", RelativePath])).

safe_relative_path(Path) ->
    case filename:split(Path) of
        ["/" | T] ->
            T1 = do_safe_relative_path(filename:join(T)),
            filename:join(["/", T1]);
        _ ->
            do_safe_relative_path(Path)
    end.

do_safe_relative_path(Path) ->
    case filename:safe_relative_path(Path) of
        unsafe -> Path;
        OK -> OK
    end.

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
    {ok, Listeners} = application:get_env(emqx, listeners),
    GenNewListener = fun({Protocol, Port, Opts} = Listener, Acc) ->
                         case Protocol of
                             ssl ->
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
                                 [{Protocol, Port, lists:keyreplace(ssl_options, 1, Opts, {ssl_options, TupleList3})} | Acc];
                             _ ->
                                 [Listener | Acc]
                         end
                     end,
    NewListeners = lists:foldl(GenNewListener, [], Listeners),
    application:set_env(emqx, listeners, NewListeners).

flush() ->
    flush([]).

flush(Msgs) ->
    receive
        M -> flush([M|Msgs])
    after
        0 -> lists:reverse(Msgs)
    end.

client_ssl_twoway() ->
    [{Key, app_path(emqx, FilePath)} || {Key, FilePath} <- ?MQTT_SSL_CLIENT] ++ ?CIPHERS.

client_ssl() ->
    ?CIPHERS ++ [{reuse_sessions, true}].

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
