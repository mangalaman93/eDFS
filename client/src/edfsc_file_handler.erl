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
    case connect_to_worker(FileName, Replicas, Chunk, MaxSize, ?MAX_TRIES) of
        {ok, State} ->
            {ok, State, ?TIMEOUT};
        {error, Reason} ->
            lager:error("unable to connect to worker node for appendFile because ~p", [Reason]),
            error
    end.

%% @private
handle_call({closeFile}, _From, State) ->
    NewState = end_connection(State),
    {stop, normal, ok, NewState};
handle_call(Request, From, State) ->
    lager:info("unknown request in line ~p from ~p: ~p", [?LINE, From, Request]),
    {reply, error, State, ?TIMEOUT}.

%% @private
handle_cast({appendFile, Data}, {FileName, Replicas, Chunk, Socket, {MaxSize, Sent, BinaryData, Size}}) when is_list(Data) ->
    BData = list_to_binary(Data),
    NewBinaryData = << BinaryData/binary, BData/binary >>,
    NewSize = Size + length(Data),
    if
        NewSize >= MaxSize ->
            {0, Sent2, BinaryData2, Size2} = send_data(Socket, NewBinaryData, NewSize, MaxSize, Sent),
            gen_tcp:close(Socket),
            {ok, {Replicas2, Chunk2, MaxSize2}} = edfsc_master:written_data(FileName, a, Chunk, Sent2, true),
            {ok, {FileName, Replicas3, Chunk3, Socket3, {MaxSize3, 0, <<>>, 0}}} = connect_to_worker(FileName, Replicas2, Chunk2, MaxSize2, ?MAX_TRIES),
            {noreply, {FileName, Replicas3, Chunk3, Socket3, send_data(Socket3, BinaryData2, Size2, MaxSize3, 0)}, ?TIMEOUT};
        NewSize > ?MTU ->
            {noreply, {FileName, Replicas, Chunk, Socket, send_data(Socket, NewBinaryData, NewSize, MaxSize, Sent)}, ?TIMEOUT};
        true ->
            {noreply, {FileName, Replicas, Chunk, Socket, {MaxSize, Sent, NewBinaryData, NewSize}}, ?TIMEOUT}
    end;
handle_cast(Request, State) ->
    lager:info("unknown request in line ~p: ~p", [?LINE, Request]),
    {noreply, State, ?TIMEOUT}.

%% @private
handle_info(timeout, State) ->
    NewState = end_connection(State),
    {stop, normal, NewState};
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

connect_to_worker(_FileName, _Replicas, _Chunk, _MaxSize, 0) ->
    {error, "unable to connect to worker"};
connect_to_worker(FileName, Replicas=[{_NodeId, Ip, Port}|Rest], Chunk, MaxSize, Retry) ->
    case gen_tcp:connect(Ip, Port, [{active, true}, binary]) of
        {ok, Socket} ->
            Tuple = {openChunk, Chunk, Rest},
            gen_tcp:send(Socket, append_delimiter(Tuple)),
            {ok, {FileName, Replicas, Chunk, Socket, {MaxSize, 0, <<>>, 0}}};
        {error, Reason} ->
            case edfsc_master:open_file(FileName, a) of
                {ok, {Replicas2, Chunk2, MaxSize2}} ->
                    connect_to_worker(FileName, Replicas2, Chunk2, MaxSize2, Retry-1);
                {error, Reason}=E ->
                    lager:error("unable to open file ~p because ~p", [FileName, Reason]),
                    E
            end
    end.

% actually sends the data
send_data(_Socket, Data, 0.0) ->
    Data;
send_data(Socket, Data, SizeToSend) ->
    S = round(SizeToSend),
    <<ToSend:S/binary, Rest/binary >> = Data,
    Tuple = {writeData, ToSend, erlang:crc32(Data)},
    gen_tcp:send(Socket, append_delimiter(Tuple)),
    Rest.
% send as much data as possible
send_data(Socket, Data, Size, MaxSize, Sent) when MaxSize =< ?MTU ->
    case MaxSize > Size of
        true ->
            {MaxSize, Sent, Data, Size};
        false ->
            RestData = send_data(Socket, Data, MaxSize),
            {0, Sent+MaxSize, RestData, Size-MaxSize}
    end;
send_data(Socket, Data, Size, MaxSize, Sent) ->
    if
        Size >= ?MTU ->
            RestData = send_data(Socket, Data, ?MTU),
            send_data(Socket, RestData, Size-?MTU, MaxSize-?MTU, Sent+?MTU);
        ?MTU > Size ->
            {MaxSize, Sent, Data, Size}
    end.

append_delimiter(Data) ->
    A = bert:encode(Data),
    B = bert:encode(?DELIMITER),
     << A/binary, B/binary >>.

end_connection({FileName, Replicas, Chunk, Socket, {MaxSize, Sent, BinaryData, Size}}) ->
    <<>> = send_data(Socket, BinaryData, Size),
    edfsc_master:written_data(FileName, a, Chunk, Sent+Size, false),
    gen_tcp:close(Socket),
    gen_server:cast(?EDFSC_SERVER, {closedSocket, FileName}),
    {FileName, Replicas, Chunk, Socket, {MaxSize-Size, Sent+Size, <<>>, 0}}.
