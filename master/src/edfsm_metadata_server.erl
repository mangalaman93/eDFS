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
-export([start_link/1]).

%% start_link/1
%% ====================================================================
%% @doc starts the metadata server on master node
-spec start_link([]) -> Result when
    Result :: {error, Reason :: term()}
            | {ok, Pid :: pid()}.
%% ====================================================================
start_link([]) ->
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
            later:error("node ~p cannot be added because ~p", [NodeId, Reason]),
            {reply, error, State}
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
handle_call({openFile, FileName, a}, _From, State) ->
    case mnesia:transaction(fun() -> mnesia:read({file, FileName}) end) of
        {error, Reason} ->
            lager:error("file ~p doesn't exists! reason: ~p", [FileName, Reason]),
            {reply, error, State};
        {atomic, [File]} ->
            case File#file.chunks of
                [H|_] when element(2, H) < ?CHUNK_SIZE ->
                    [Chunk] = mnesia:read({chunk, element(1, H)}),
                    {reply, {ok, {Chunk#chunk.replicas, Chunk#chunk.id, ?CHUNK_SIZE-element(2, H)}}, State};
                _ ->
                    NewChunkId = edfsm_util:gen_chunk_id(),
                    {Replicas, NewState} = choose_replicas(State, File#file.repfactor),
                    {atomic, _} = mnesia:transaction(fun() ->
                        mnesia:write(File#file{chunks=[{NewChunkId, 0}|File#file.chunks]}),
                        mnesia:write(#chunk{id=NewChunkId, filename=FileName, size=0, replicas=Replicas})
                    end),
                    {reply, {ok, {Replicas, NewChunkId, ?CHUNK_SIZE}}, NewState}
            end
    end;
handle_call(Request, From, State) ->
    lager:info("unknown request in line ~p from ~p: ~p", [?LINE, From, Request]),
    {reply, error, State}.

%% @private
handle_cast({changeState, NodeId, NodeState}, State) ->
    F = fun() ->
        [Node] = mnesia:read({node, NodeId}),
        mnesia:write(Node#node{state=NodeState}),
        Node
    end,
    case mnesia:transaction(F) of
        {atomic, Node} ->
            lager:info("state of node ~p changed to ~p", [NodeId, NodeState]),
            case NodeState of
                readWrite ->
                    {noreply, lists:keymerge(1, {NodeId, Node#node.space_util, Node#node.ip, Node#node.port}, State)};
                _ ->
                    {noreply, lists:keydelete(NodeId, 1, State)}
            end;
        {aborted, Reason} ->
            later:error("state of node ~p cannot be changed to ~p because ~p", [NodeId, NodeState, Reason]),
            {noreply, State}
    end;
handle_cast(Request, State) ->
    lager:info("unknown request in line ~p: ~p", [?LINE, Request]),
    {noreply, State}.

%% @private
%% @todo delete all the chunks of the node
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
% to mnesia create_table function without any checks or modification
create_table(Table, Props) ->
    case mnesia:create_table(Table, Props) of
         {atomic, ok} ->
            lager:info("~p table created", [Table]);
        {aborted, Reason} ->
            lager:error("~p table cannot be created, reason:~p", [Table, Reason]),
            {error, mnesia_error}
    end.

% chooses replicas to place a given chunk
choose_replicas(ListOfNodes, RepFactor) ->
    choose_replicas(ListOfNodes, RepFactor, [], []).
choose_replicas([], _, [], _) ->
    throw(no_node_found);
choose_replicas(ListOfNodes, 0, Replicas, Acc) ->
    {Replicas, lists:keymerge(2, ListOfNodes, lists:keysort(2, Acc))};
choose_replicas([], _, Replicas, Acc) ->
    {Replicas, lists:keysort(2, Acc)};
choose_replicas([{Id, Util, Ip, Port}|T], RepFactor, Replicas, Acc) ->
    choose_replicas(T, RepFactor-1, [{Id, Ip, Port}|Replicas], [{Id, Util+?CHUNK_SIZE, Ip, Port}|Acc]).
