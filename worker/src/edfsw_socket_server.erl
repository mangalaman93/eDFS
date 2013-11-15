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
%%% @doc edfs worker socket handling server
%%%

-module(edfsw_socket_server).
-include("edfsw.hrl").
-export([init/1]).


%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1]).

%% start_link/1
%% ====================================================================
%% @doc Creates a listen socket to accept requests from application
%% servers. The function should be called, directly or indirectly, by
%% the supervisor. It will, among other things, ensure that the server
%% is linked to the supervisor.  It returns {ok, Pid}
-spec start_link([AcceptSocket]) -> Result when
	AcceptSocket :: port(),
	Result       :: {ok, pid()}
				  | error.
%% ====================================================================
start_link([AcceptSocket]) ->
    Pid = spawn_link(?MODULE, init, [[AcceptSocket]]),
    lager:info("socket server started with pid ~p", [Pid]),
    {ok, Pid}.


%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% @private
init([AcceptSocket]) ->
    receive_it(AcceptSocket, {<<>>, 0}, false).


%% ====================================================================
%% Internal functions
%% ====================================================================

receive_it(AcceptSocket, {BinAcc, Size}, State) ->
	inet:setopts(AcceptSocket, [{active, once}]),
	receive
		{tcp, AcceptSocket, Bin} ->
			BBin = list_to_binary(Bin),
			{Rest, NewState} = parse(<<BinAcc/binary, BBin/binary>>, Size, byte_size(BBin), State),
			receive_it(AcceptSocket, Rest, NewState);
		{tcp_closed, AcceptSocket}->
			handle_message({closedSocket}, State),
	        lager:info("Socket ~p closed!", [AcceptSocket]);
	    {tcp_error, AcceptSocket, Reason} ->
	        lager:error("Error on socket ~p reason: ~p", [AcceptSocket, Reason]),
	        handle_message({closedSocket}, State),
	        gen_tcp:close(AcceptSocket)
    end.

parse(BinAcc, OldSize, NewSize, State) ->
	DelmSize = byte_size(bert:encode(?DELIMITER)),
	{First, Last} = if
		OldSize =< DelmSize ->
			{<<>>, BinAcc};
		OldSize > DelmSize ->
			S1 = (OldSize-DelmSize),
			S2 = (DelmSize+NewSize),
			<< A:S1/binary, B:S2/binary >> = BinAcc,
			{A, B}
	end,
	case binary:match(Last, bert:encode(?DELIMITER)) of
        {Pos, Len} ->
        	<< MessageLast:Pos/binary, _Delimiter:Len/binary, Rest/binary>> = Last,
        	Message = << First/binary, MessageLast/binary >>,
            NewState = handle_message(bert:decode(Message), State),
            parse(Rest, 0, OldSize+NewSize-byte_size(Message)-DelmSize, NewState);
        nomatch ->
            {{BinAcc, OldSize + NewSize}, State}
    end.

handle_message({openChunk, Chunk, []}, false) ->
	edfsw_os:touch(edfsw_os:get_abs_path(filename:join(atom_to_list(node()), Chunk))),
	{Chunk, false};
handle_message({openChunk, Chunk, [{_NodeId, Ip, Port}|Rest]}, false) ->
	edfsw_os:touch(edfsw_os:get_abs_path(filename:join(atom_to_list(node()), Chunk))),
	{ok, Socket} = gen_tcp:connect(Ip, Port, [{active, true}, binary]),
	gen_tcp:send(Socket, append_delimiter({openChunk, Chunk, Rest})),
	{Chunk, Socket};
handle_message(Message={writeData, Data, _CheckSum}, {Chunk, Socket}) ->
	% CheckSum = erlang:crc32(Data),
	file:write_file(edfsw_os:get_abs_path(filename:join(atom_to_list(node()), Chunk)), Data, [append]),
	case Socket of
		false ->
			ok;
		_ ->
			gen_tcp:send(Socket, append_delimiter(Message))
	end,
	{Chunk, Socket};
handle_message({closedSocket}, {_Chunk, false}) ->
	ok;
handle_message({closedSocket}, {_Chunk, Socket}) ->
	gen_tcp:close(Socket).

append_delimiter(Data) ->
    A = bert:encode(Data),
    B = bert:encode(?DELIMITER),
     << A/binary, B/binary >>.
