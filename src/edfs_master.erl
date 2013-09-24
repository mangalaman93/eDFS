%% -*- erlang-indent-level: 4;
%%% Copyright 2012 Erlware, LLC. All Rights Reserved.
%%%
%%% This file is provided to you under the Apache License,
%%% Version 2.0 (the "License"); you may not use this file
%%% except in compliance with the License.  You may obtain
%%% a copy of the License at
%%%
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing,
%%% software distributed under the License is distributed on an
%%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%%% KIND, either express or implied.  See the License for the
%%% specific language governing permissions and limitations
%%% under the License.
%%% --------------------------------------------------------------------------
%%% @author Aman Mangal <mangalaman93@gmail.com>
%%% @doc edfs master node API
%%%

-module(edfs_master).
-behaviour(application).
-export([start/2, stop/1]).
-include("edfs.hrl").


% ====================================================================
%% API functions
%% ====================================================================
-export([create/1]).

%% create/1
%% ====================================================================
%% @doc creates a file with the given file name
-spec create(Name) -> ok when
    Name :: string().
%% ====================================================================
create(Name) ->
    gen_server:call(global:whereis_name(?EDFSM_METADATA_SERVER), {createFile, Name}).


%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% @private
start(_Type, _Args) ->
    edfsm_sup:start().

%% @private
stop(_State) ->
    ok.
