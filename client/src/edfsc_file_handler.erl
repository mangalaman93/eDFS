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
%%% @doc edfs client server for each file handling
%%%

-module(edfsc_file_handler).
-behaviour(gen_server).
-include("edfsc.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1]).

%% start_link/1
%% ====================================================================
%% @doc starts the file handler
-spec start_link({Replicas, Chunk, MaxSize, FileName}) -> Result when
    Replicas :: [{NodeId, Ip, Port}],
    NodeId   :: atom(),
    Ip       :: tuple(),
    Port     :: integer(),
    Chunk    :: string(),
    MaxSize  :: integer(),
    FileName :: string(),
    Result   :: {error, Reason :: term()}
              | {ok, Pid :: pid()}.
%% ====================================================================
start_link({Replicas, Chunk, MaxSize, FileName}) ->
    gen_server:start_link(?MODULE, {FileName, Replicas, Chunk, MaxSize}, []).


%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% @private
init({FileName, Replicas, Chunk, MaxSize}) ->
    case connect_to_worker(FileName, Replicas, Chunk, MaxSize, ?MAX_RETRIES) of
        {ok, State} ->
            {ok, State, ?TIMEOUT};
        {error, Reason} ->
            lager:error("unable to connect to worker node for appendFile because ~p", [Reason]),
            error
    end.

%% @private
handle_call({closeFile}, _From, {FileName, Replicas, Chunk, MaxSize, Socket, {BinaryData, Size}}) ->
    Ret = send_data(Socket, BinaryData, Size, Size),
    gen_tcp:close(Socket),
    gen_server:cast(?EDFSC_SERVER, {closedSocket, FileName}),
    {stop, normal, ok, {FileName, Replicas, Chunk, MaxSize, Socket, Ret}};
handle_call(Request, From, State) ->
    lager:info("unknown request in line ~p from ~p: ~p", [?LINE, From, Request]),
    {reply, error, State, ?TIMEOUT}.

%% @private
handle_cast({appendFile, Data}, {FileName, Replicas, Chunk, MaxSize, Socket, {BinaryData, Size}}) when is_list(Data) ->
    BData = list_to_binary(Data),
    NewBinaryData = << BinaryData/binary, BData/binary >>,
    NewSize = Size + length(Data),
    if
        % Size >= MaxSize ->
        %     ;
        Size > ?MTU ->
            {noreply, {FileName, Replicas, Chunk, MaxSize-?MTU, Socket, send_data(Socket, NewBinaryData, NewSize, ?MTU)}};
        true ->
            {noreply, {FileName, Replicas, Chunk, MaxSize, Socket, {NewBinaryData, NewSize}}}
    end;
handle_cast(Request, State) ->
    lager:info("unknown request in line ~p: ~p", [?LINE, Request]),
    {noreply, State, ?TIMEOUT}.

%% @private
handle_info(timeout, {FileName, Replicas, Chunk, MaxSize, Socket, {BinaryData, Size}}) ->
    Ret = send_data(Socket, BinaryData, Size, Size),
    gen_tcp:close(Socket),
    gen_server:cast(?EDFSC_SERVER, {closedSocket, FileName}),
    {stop, normal, {FileName, Replicas, Chunk, MaxSize, Socket, Ret}};
handle_info(Info, State) ->
    lager:info("unknown info in line ~p: ~p", [?LINE, Info]),
    {noreply, State, ?TIMEOUT}.

%% @private
terminate(Reason, State) ->
    lager:info("terminating server ~p, reason: ~p, state:~p", [?MODULE, Reason, State]).

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

connect_to_worker(FileName, Replicas=[{_NodeId, Ip, Port}|Rest], Chunk, MaxSize, Retry) ->
    case gen_tcp:connect(Ip, Port, [{active, true}, binary]) of
        {ok, Socket} ->
            Tuple = {openChunk, Chunk, Rest},
            gen_tcp:send(Socket, append_delimiter(Tuple)),
            {ok, {FileName, Replicas, Chunk, MaxSize, Socket, {<<>>, 0}}};
        {error, Reason} ->
            case edfsc_master:open_file(FileName, a) of
                {ok, {FileName, Replicas2, Chunk2, MaxSize2}} ->
                    connect_to_worker(FileName, Replicas2, Chunk2, MaxSize2, Retry-1);
                {error, Reason}=E ->
                    lager:error("unable to open file ~p because ~p", [FileName, Reason]),
                    E
            end
    end.

send_data(Socket, Data, TotalSize, SizeToSend) ->
    <<ToSend:SizeToSend/binary, Rest/binary >> = Data,
    Tuple = {writeData, ToSend, erlang:crc32(Data)},
    gen_tcp:send(Socket, append_delimiter(Tuple)),
    {Rest, TotalSize-SizeToSend}.

append_delimiter(Data) ->
    A = bert:encode(Data),
    B = bert:encode(?DELIMITER),
     << A/binary, B/binary >>.
