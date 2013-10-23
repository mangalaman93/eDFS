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
%%% @doc Tcp listen server, listens on a given port and creates more sockets
%%% to communicate with more requests
%%%

-module(edfsw_listen_server).
-export([init/1]).
-include("edfs_worker.hrl").


%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).

%% start_link/1
%% ====================================================================
%% @doc Creates a listen socket to accept connections from clients.
%% The function should be called, directly or indirectly, by the
%% supervisor. It will, among other things, ensure that the server
%% is linked to the supervisor. It also registers the process with the
%% local name <b>edfsw_listen_server</b>. It returns {ok, Pid}
-spec start_link() -> {ok, pid()} | error.
%% ====================================================================
start_link()->
    Pid = spawn_link(?MODULE, init, [[]]),
    lager:info("edfsw_listen_server started with process id ~p", [Pid]),
    erlang:register(?EDFSW_LISTEN_SERVER, Pid),
    {ok, Pid}.


%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% @private
init([]) ->
    Port = list_to_integer(os:getenv("PORT")),
    case gen_tcp:listen(Port, [{active, false}, {reuseaddr, true}, {keepalive, true}]) of
        {ok, ListenSocket} ->
            lager:info("listening socket created on port ~p", [Port]),
            accept_connections(ListenSocket);
        {error, Reason} ->
            lager:error("unable to listen on port ~p because ~p", [Port, Reason])
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

 accept_connections(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
         {ok, AcceptSocket} ->
             case supervisor:start_child(?EDFSW_SOCKET_SUP, [[AcceptSocket]]) of
                {ok, undefined} ->
                    lager:error("unable to start connection socket process in line ~p", [?LINE]),
                    gen_tcp:close(AcceptSocket);
                {ok, undefined, _} ->
                    lager:error("unable to start connection socket process in line ~p", [?LINE]),
                    gen_tcp:close(AcceptSocket);
                {ok, Pid} ->
                    lager:info("socket connection process started with pid ~p", [Pid]),
                    gen_tcp:controlling_process(AcceptSocket, Pid);
                {ok, Pid, _} ->
                    lager:info("socket connection process started with pid ~p", [Pid]),
                    gen_tcp:controlling_process(AcceptSocket, Pid);
                {error, Reason} ->
                    lager:error("unable to start connection socket process because: ~p", [Reason]),
                    gen_tcp:close(AcceptSocket)
            end;
        {error, Reason} ->
            lager:error("error occurred in line ~p of edfsw_listen_server because ~p", [?LINE, Reason])
     end,
     accept_connections(ListenSocket).
