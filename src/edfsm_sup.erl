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
%%% @doc edfs master top supervisor
%%%

-module(edfsm_sup).
-behaviour(supervisor).
-export([init/1]).
-include("edfs.hrl").


%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0]).

%% start/0
%% ====================================================================
%% @doc starts the edfs master supervisor
-spec start() -> Result when
    Result :: {ok, pid()}
            | ignore
            | {error, Reason},
    Reason :: {already_started, pid()}
            | shutdown
            | term().
%% ====================================================================
start() ->
    supervisor:start_link(?MODULE, []).


%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% @private
init([]) ->
    Edfsm_metadata_server = ?CHILD(?EDFSM_METADATA_SERVER, worker, []),
    {ok,{{one_for_one, ?MAXR, ?MAXT},
         [Edfsm_metadata_server]}}.
