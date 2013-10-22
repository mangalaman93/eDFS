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
%%% @doc edfs chunk server on worker node
%%%

-module(edfsw_chunk_server).
-behaviour(gen_server).
-include("edfs_worker.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1]).

%% start_link/1
%% ====================================================================
%% @doc starts the chunk server on worker node
-spec start_link([]) -> Result when
    Result :: {error, Reason :: term()}
            | {ok, Pid :: pid()}.
%% ====================================================================
start_link([]) ->
    gen_server:start_link({local, ?EDFSW_CHUNK_SERVER}, ?MODULE, [], []).


%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% @private
init([]) ->
    case net_adm:ping(?MASTER_NODE) of
        pong ->
            ok = edfsw_os:mkdir(edfsw_os:get_abs_path("")),
            global:sync(),
            ok = edfsw_master:handshake(),
            lager:info("connected to master sitting at ~p", [?MASTER_NODE]),
            {ok, {}};
        pang ->
            lager:error("unable to connect to master node sitting at ~p", [?MASTER_NODE]),
            error
    end.

handle_call(Request, From, State) ->
    lager:info("unknown request in line from ~p: ~p", [?LINE, From, Request]),
    {reply, error, State}.

%% @private
handle_cast(Request, State) ->
    lager:info("unknown request in line ~p: ~p", [?LINE, Request]),
    {noreply, State}.

%% @private
handle_info(Info, State) ->
    lager:info("unknown info in line ~p: ~p", [?LINE, Info]),
    {noreply, State}.

%% @private
terminate(Reason, State) ->
    lager:info("terminating server ~p, reason: ~p, state:~p", [?MODULE, Reason, State]),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================
