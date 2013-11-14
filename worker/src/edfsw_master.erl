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
%%% @doc edfs master node API for worker
%%%

-module(edfsw_master).
-include("edfsw.hrl").


% ====================================================================
%% API functions
%% ====================================================================
-export([handshake/3,
	     changeState/1]).

%% handshake/2
%% ====================================================================
%% @doc handshakes with master, tells about worker's presence
-spec handshake(State, IP, Port) -> ok when
	State :: atom(),
	IP    :: tuple(),
	Port  :: integer().
%% ====================================================================
handshake(State, IP, Port) ->
	global:sync(),
    gen_server:call(global:whereis_name(?EDFSM_METADATA_SERVER), {handshake, State, IP, Port}).

%% changeState/1
%% ====================================================================
%% @doc informs the master about the change of the state of the worker node
-spec changeState(NewState :: atom()) -> ok.
%% ====================================================================
changeState(NewState) ->
    gen_server:cast(global:whereis_name(?EDFSM_METADATA_SERVER), {changeState, node(), NewState}).
