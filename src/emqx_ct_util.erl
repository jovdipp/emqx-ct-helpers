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

-module(emqx_ct_util).

%% API
-export([get_platform_root_dir/0, where_ami/1]).

get_platform_root_dir() ->
    ModPath = where_ami(?MODULE),
    get_platform_root_dir(ModPath).

get_platform_root_dir(ModPath) ->
    case string:rstr( ModPath, "_build" ) of
        1 ->
            ".";
        Index ->
            End_Index = Index - 1,
            string:sub_string( ModPath, 1, End_Index )
    end.

where_ami(Module) ->
    filename:dirname(code:which(Module)).
