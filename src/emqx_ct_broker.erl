%%%===================================================================
%%% Copyright (c) 2013-2018 EMQ Inc. All rights reserved.
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

-module(emqx_ct_broker).

-behaviour(gen_server).

-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/emqx_mqtt.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([start_link/0, stop/0]).
-export([messages/0, messages/1]).
-export([subscribe/1, subscribe/2, subscribe/3, subscribe/4]).
-export([publish/2]).
-export([unsubscribe/1, unsubscribe/2]).

%% gen_server Function Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(TAB, ?MODULE).
-define(TIMEOUT, 60000).
-define(LOG(Format, Args), ct:print("CT_BROKER: " ++ Format, Args)).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE, normal, infinity).

%%--------------------------------------------------------------------
%% PubSub APIs
%%--------------------------------------------------------------------

subscribe(Topic) when is_binary(Topic) ->
    subscribe(Topic, self()).

subscribe(Topic, Subscriber) when is_binary(Topic) ->
    subscribe(Topic, Subscriber, []).

subscribe(Topic, Subscriber, Options) when is_binary(Topic) ->
    subscribe(Topic, Subscriber, Options, ?TIMEOUT).

subscribe(Topic, Subscriber, Options, Timeout) ->
    {Topic1, Options1} = emqx_topic:parse(Topic, Options),
    SubReq = {subscribe, Topic1, with_subpid(Subscriber), Options1},
    gen_server:call(?MODULE, SubReq, Timeout).

publish(Topic, Payload) ->
    lists:foreach(
      fun({To, {_SubId, SubPid}}) ->
              SubPid ! {dispatch, To, #message{payload = Payload}};
         ({To, SubPid}) when is_pid(SubPid) ->
              SubPid ! {dispatch, To, #message{payload = Payload}}
      end, subscribers(Topic)).

unsubscribe(Topic) when is_binary(Topic) ->
    unsubscribe(Topic, self()).

unsubscribe(Topic, Subscriber) when is_binary(Topic) ->
    unsubscribe(Topic, Subscriber, ?TIMEOUT).

unsubscribe(Topic, Subscriber, Timeout) ->
    {Topic1, _} = emqx_topic:parse(Topic),
    UnsubReq = {unsubscribe, Topic1, with_subpid(Subscriber)},
    gen_server:call(?MODULE, UnsubReq, Timeout).

messages() ->
    messages(self()).

messages(Pid) ->
    {messages, Messages} = erlang:process_info(Pid, messages),
    [{To, Msg}|| {dispatch, To, Msg} <- Messages].

topics() ->
    lists:usort(lists:flatten(ets:match(?TAB, {'$1', '_'}))).

subscribers(TopicName) ->
    lists:flatten([ets:lookup(?TAB, TopicFilter)
                   || TopicFilter <- topics(),
                      emqx_topic:match(TopicName, TopicFilter)]).

with_subpid(SubPid) when is_pid(SubPid) ->
    SubPid;
with_subpid(SubId) when is_binary(SubId) ->
    {SubId, self()};
with_subpid({SubId, SubPid}) when is_binary(SubId), is_pid(SubPid) ->
    {SubId, SubPid}.

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([]) ->
    _ = ets:new(?TAB, [bag, named_table, public]),
    {ok, #state{}}.

handle_call({subscribe, Topic, Subscriber, _Options}, _From, State) ->
    _ = ets:insert(?TAB, {Topic, Subscriber}),
    {reply, ok, State};

handle_call({unsubscribe, Topic, Subscriber}, _From, State) ->
    _ = ets:delete_object(?TAB, {Topic, Subscriber}),
    {reply, ok, State};

handle_call(Req, _From, State) ->
    ?LOG("Unexpected request: ~p~n", [Req]),
    {reply, ignore, State}.

handle_cast(Msg, State) ->
    ?LOG("Unexpected msg: ~p~n", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    ?LOG("Unexpected info: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

