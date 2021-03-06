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
%%% @doc edfs client server
%%%

-module(edfsc_server).
-behaviour(gen_server).
-include("edfsc.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1]).

%% start_link/1
%% ====================================================================
%% @doc starts the client server
-spec start_link([]) -> Result when
    Result :: {error, Reason :: term()}
            | {ok, Pid :: pid()}.
%% ====================================================================
start_link([]) ->
    gen_server:start_link({global, ?EDFSC_SERVER}, ?MODULE, [], []).


%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% @private
init([]) ->
    case net_adm:ping(?MASTER_NODE) of
        pong ->
            {ok, dict:new()};
        pang ->
            lager:error("unable to connect to master node sitting at ~p", [?MASTER_NODE]),
            error
    end.

handle_call({openFile, FileName, a}, _From, State) ->
    case dict:is_key(FileName, State) of
        true ->
            dict:fetch(FileName, State);
        false ->
            case edfsc_master:open_file(FileName, a) of
                {ok, OpenFile} ->
                    create_file_handler(FileName, OpenFile, State);
                {error, Reason} ->
                    lager:error("unable to open file ~p because ~p", [FileName, Reason]),
                    {reply, error, State}
            end
    end;
handle_call(Request, From, State) ->
    lager:info("unknown request in line ~p from ~p: ~p", [?LINE, From, Request]),
    {reply, error, State}.

%% @private
handle_cast({createFile, FileName}, State) ->
    edfsc_master:create_file(FileName),
    {noreply, State};
handle_cast({closedSocket, FileName}, State) ->
    lager:info("socket closed for File ~p", [FileName]),
    {noreply, dict:erase(FileName, State)};
handle_cast(Request, State) ->
    lager:info("unknown request in line ~p: ~p", [?LINE, Request]),
    {noreply, State}.

%% @private
handle_info(Info, State) ->
    lager:info("unknown info in line ~p: ~p", [?LINE, Info]),
    {noreply, State}.

%% @private
terminate(Reason, State) ->
    lager:info("terminating server ~p, reason: ~p, state:~p", [?EDFSC_SERVER, Reason, State]).

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

create_file_handler(FileName, OpenFile, Dict) ->
    case supervisor:start_child(?EDFSC_FILE_SUP, [erlang:append_element(OpenFile, FileName)]) of
        {ok, undefined} ->
            lager:error("unable to start file handler in line ~p", [?LINE]),
            {reply, error, Dict};
        {ok, undefined, _} ->
            lager:error("unable to start file handler in line ~p", [?LINE]),
            {reply, error, Dict};
        {ok, Pid} ->
            lager:info("file handler started with pid ~p", [Pid]),
            {reply, Pid, dict:store(FileName, Pid, Dict)};
        {ok, Pid, _} ->
            lager:info("file handler started with pid ~p", [Pid]),
            {reply, Pid, dict:store(FileName, Pid, Dict)};
        {error, Reason} ->
            lager:error("unable to start file handler because: ~p", [Reason]),
            {reply, error, Dict}
    end.
