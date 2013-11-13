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
-include("edfsw.hrl").
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
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% @private
init([]) ->
    case net_adm:ping(?MASTER_NODE) of
        pong ->
            ok = edfsw_os:mkdir(edfsw_os:get_abs_path(atom_to_list(node()))),
            mnesia:create_table(chunk, [{attributes, record_info(fields, chunk)}, {type, set}]),
            ok = edfsw_master:handshake(readWrite, get_eth_ip(), list_to_integer(os:getenv("PORT"))),
            lager:info("connected to master sitting at ~p", [?MASTER_NODE]),
            {ok, readWrite, 0};
        pang ->
            lager:error("unable to connect to master node sitting at ~p", [?MASTER_NODE]),
            error
    end.

handle_call(Request, From, State) ->
    lager:info("unknown request in line from ~p: ~p", [?LINE, From, Request]),
    {reply, error, State, ?TIMEPERIOD}.

%% @private
handle_cast({changeState, NewState}, State) ->
    ok = edfsw_master:changeState(NewState),
    lager:info("changing the state of worker ~p from ~p to ~p", [node(), State, NewState]),
    {noreply, NewState, 0};
handle_cast(Request, State) ->
    lager:info("unknown request in line ~p: ~p", [?LINE, Request]),
    {noreply, State, ?TIMEPERIOD}.

%% @private @todo
handle_info(timeout, State=readWrite) ->
    {noreply, State, ?TIMEPERIOD};
handle_info(timeout, State=readOnly) ->
    {noreply, State};
handle_info(timeout, State=distress) ->
    {noreply, State, ?TIMEPERIOD};
handle_info(timeout, State=unavailable) ->
    {noreply, State, ?TIMEPERIOD};
handle_info(Info, State) ->
    lager:info("unknown info in line ~p: ~p", [?LINE, Info]),
    {noreply, State, ?TIMEPERIOD}.

%% @private
terminate(Reason, State) ->
    lager:info("terminating server ~p, reason: ~p, state:~p", [?MODULE, Reason, State]).

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

get_eth_ip() ->
    {ok, AddressList} = inet:getif(),
    Filter = fun({{127,0,0,1}, _, _}, Acc) ->
            Acc;
        (Elem, Acc) ->
            [Elem|Acc]
    end,
    case lists:foldl(Filter, [], AddressList) of
        [{IP, _, _}] ->
            IP;
        L when is_list(L) ->
            {ok, Interfaces} = inet:getifaddrs(),
            make_sure(find_eth_ip(Interfaces), L);
        _ ->
            throw(ip_not_found)
    end.

find_eth_ip([]) ->
    throw(ip_not_found);
find_eth_ip([{Name, Info}|L]) ->
    case string:str(Name, "eth") of
        0 ->
            find_eth_ip(L);
        _ ->
            lists:keyfind(addr, 1, Info)
    end.

make_sure(IP, L) ->
    case lists:any(fun({NewIP, _, _}) -> NewIP == IP end, L) of
        true ->
            IP;
        false ->
            throw(ip_not_found)
    end.
