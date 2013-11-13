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
%%% @doc edfs metadata server on master node
%%%

-module(edfsm_metadata_server).
-behaviour(gen_server).
-include("edfsm.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1,
         gen_chunk_id/0,
         gen_sec_chunk_id/0]).

%% start_link/1
%% ====================================================================
%% @doc starts the metadata server on master node
-spec start_link([]) -> Result when
    Result :: {error, Reason :: term()}
            | {ok, Pid :: pid()}.
%% ====================================================================
start_link([]) ->
    % @todo will not be successful on restart by supervisor
    create_table(file, [{attributes, record_info(fields, file)}, {type, set}]),
    create_table(node, [{attributes, record_info(fields, node)}, {type, set}]),
    create_table(chunk, [{attributes, record_info(fields, chunk)}, {type, set}]),
    gen_server:start_link({global, ?EDFSM_METADATA_SERVER}, ?MODULE, [], []).


%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% @private
init([]) ->
    {ok, []}.

%% @private
handle_call({handshake, NodeState, IP, Port}, {_Pid, NodeRef}, State) ->
    NodeId = node(NodeRef),
    case mnesia:transaction(fun() -> mnesia:write(#node{id=NodeId, state=NodeState, ip=IP, port=Port}) end) of
        {atomic, ok} ->
            true = erlang:monitor_node(NodeId, true),
            lager:info("node added with id ~p", [NodeId]),
            {reply, ok, [{NodeId, 0, IP, Port}|State]};
        {aborted, Reason} ->
            later:error("node cannot be added because ~p", [NodeId, Reason]),
            {reply, {error, Reason}, State}
    end;
handle_call({createFile, Name}, _From, State) ->
    F = fun() ->
        case mnesia:read({file, Name}) of
            [] ->
                mnesia:write(#file{name=Name}),
                lager:info("file created with name ~p", [Name]);
            _ ->
                lager:error("file already exists!")
        end
    end,
    case mnesia:transaction(F) of
        {atomic, ok} ->
            {reply, ok, State};
        {aborted, Reason} ->
            later:error("file with name ~p cannot be created because ~p", [Name, Reason]),
            {reply, error, State}
    end;
handle_call({openFile, FileName, w}, _From, State) ->
    {atomic, [File]} = mnesia:transaction(fun() -> mnesia:read({file, FileName}) end),
    case File#file.chunks of
        [H|_] when element(2, H) < ?CHUNK_SIZE ->
            [Chunk] = mnesia:read({chunk, element(1, H)}),
            %% @todo modify the state
            {reply, {ok, {Chunk#chunk.replicas, Chunk#chunk.id, ?CHUNK_SIZE - element(2, H)}}, State};
        _ ->
            NewChunkId = gen_chunk_id(),
            {Replicas, NewState} = choose_replicas(State, File#file.repfactor),
            {atomic, _} = mnesia:transaction(fun() -> mnesia:write(File#file{chunks=[{NewChunkId, 0}|File#file.chunks]}),
                mnesia:write(#chunk{id=NewChunkId, filename=FileName, size=0, replicas=Replicas}) end),
            {reply, {ok, {Replicas, NewChunkId, ?CHUNK_SIZE}}, NewState}
    end;
handle_call(Request, From, State) ->
    lager:info("unknown request in line ~p from ~p: ~p", [?LINE, From, Request]),
    {reply, error, State}.

%% @private
handle_cast({changeState, NodeId, NodeState}, State) ->
    case mnesia:transaction(fun() -> mnesia:write(#node{id=NodeId, state=NodeState}) end) of
        {atomic, ok} ->
            lager:info("state of node ~p changed to ~p", [NodeId, NodeState]);
        {aborted, Reason} ->
            later:error("state of node ~p cannot be changed to ~p because ~p", [NodeId, NodeState, Reason])
    end,
    case NodeState of
        readWrite ->
            {noreply, State};
        _ ->
            {noreply, lists:keydelete(NodeId, 1, State)}
    end;
handle_cast(Request, State) ->
    lager:info("unknown request in line ~p: ~p", [?LINE, Request]),
    {noreply, State}.

%% @private @todo
handle_info({nodedown, Node}, State) ->
    case mnesia:transaction(fun() -> mnesia:delete({node, Node}) end) of
        {atomic, _Result} ->
            lager:info("node ~p is down", [Node]);
        {aborted, Reason} ->
            lager:error("node ~p is down but cannot be removed from node table, reason: ~p", [Node, Reason])
    end,
    {noreply, lists:keydelete(Node, 1, State)};
handle_info(Info, State) ->
    lager:info("unknown info in line ~p: ~p", [?LINE, Info]),
    {noreply, State}.

%% @private
terminate(Reason, State) ->
    lager:info("terminating server ~p, reason: ~p, state:~p", [?MODULE, Reason, State]).

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

% creates table with given properties, the arguments are passed directly
% mnesia create_table function without any checks or modification
create_table(Table, Props) ->
    case mnesia:create_table(Table, Props) of
         {atomic, ok} ->
            lager:info("~p table created", [Table]);
        {aborted, Reason} ->
            lager:error("~p table cannot be created, reason:~p", [Table, Reason]),
            {error, mnesia_error}
    end.

% generate random strings of given length
gen_rand_str(Len) ->
    gen_rand_str(Len, []).
gen_rand_str(0, Acc) ->
    Acc;
gen_rand_str(Len, Acc) ->
    Char = lists:nth(random:uniform(?LEN_AC), ?ALLOWED_CHARS),
    gen_rand_str(Len-1, [Char|Acc]).

curr_time_millis() ->
    {MegaSec, Sec, MicroSec} = erlang:now(),
    1000000000000*MegaSec + Sec*1000000 + MicroSec.

% generate unique random number (not secure) of length 64 bit
% can got until year 2170 (200 years from the time when cpu counter began counting, currently 1970)
gen_chunk_id() ->
    gen_chunk_id(48, curr_time_millis(), []).
gen_chunk_id(0, 0, Acc) ->
    lists:reverse(Acc);
gen_chunk_id(Len, Num, Acc) ->
    RestLength = Len - 6,
    << Pos:6, RestNum:RestLength >> = << Num:Len >>,
    gen_chunk_id(RestLength, RestNum, [lists:nth(Pos+1, ?ALLOWED_CHARS)|Acc]).

% generate unique *secure* random number of length 128 bit
% can got until year 2170 (200 years from the time when cpu counter began counting, currently 1970)
gen_sec_chunk_id() ->
    gen_chunk_id(48, curr_time_millis(), gen_rand_str(8)).

% chooses replicas to place a given chunk
choose_replicas(ListOfNodes, RepFactor) ->
    choose_replicas(ListOfNodes, RepFactor, [], []).
choose_replicas([], _, [], _) ->
    erlang:thorw(no_node_found);
choose_replicas(ListOfNodes, 0, Replicas, Acc) ->
    {Replicas, lists:keymerge(2, ListOfNodes, lists:keysort(2, Acc))};
choose_replicas([], _, Replicas, Acc) ->
    {Replicas, lists:keysort(2, Acc)};
choose_replicas([{Id, Util, Ip, Port}|T], RepFactor, Replicas, Acc) ->
    choose_replicas(T, RepFactor-1, [{Id, Ip, Port}|Replicas], [{Id, Util+?CHUNK_SIZE, Ip, Port}|Acc]).
