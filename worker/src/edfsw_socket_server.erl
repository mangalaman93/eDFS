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
    receive_it(AcceptSocket, {<<>>, 0}).


%% ====================================================================
%% Internal functions
%% ====================================================================

receive_it(AcceptSocket, {BinAcc, Size}) ->
	inet:setopts(AcceptSocket, [{active, once}]),
	receive
		{tcp, AcceptSocket, Bin} ->
			BBin = list_to_binary(Bin),
			Rest = parse(<<BinAcc/binary, BBin/binary>>, Size, byte_size(Bin)),
			receive_it(AcceptSocket, Rest);
		{tcp_closed, AcceptSocket}->
	        lager:info("Socket ~p closed!", [AcceptSocket]);
	    {tcp_error, AcceptSocket, Reason} ->
	        lager:error("Error on socket ~p reason: ~p", [AcceptSocket, Reason]),
	        gen_tcp:close(AcceptSocket)
    end.

parse(BinAcc, OldSize, NewSize) ->
	DelmSize = byte_size(bert:encode(?DELIMITER)),
	{First, Last} = if
		OldSize =< DelmSize ->
			{<<>>, BinAcc};
		OldSize > DelmSize ->
			S = (OldSize-DelmSize),
			<< A:S/binary, B:DelmSize/binary >> = BinAcc,
			{A, B}
	end,
	case binary:match(Last, bert:encode(?DELIMITER)) of
        {Pos, Len} ->
        	<< MessageLast:Pos/binary, _Delimiter:Len/binary, Rest/binary>> = Last,
        	Message = << First/binary, MessageLast/binary >>,
            io:format("~p~n", [bert:decode(Message)]),
            parse(Rest, 0, OldSize+NewSize-byte_size(Message)-DelSize);
        nomatch ->
            {BinAcc, OldSize + NewSize}
    end.
