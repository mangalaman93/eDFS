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
-include("edfs_master.hrl").
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
    case mnesia:create_table(file, [{attributes, record_info(fields, file)}, {type, set}]) of
         {atomic, ok} ->
            lager:info("file table created"),
            gen_server:start_link({global, ?MODULE}, ?MODULE, [], []);
        {aborted, Reason} ->
            lager:error("file table cannot be created, reason:~p", [Reason]),
            {error, mnesia_error}
    end.
    

%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% @private
init([]) ->
    {ok, []}.

%% @private
handle_call({createFile, Name}, _From, State) ->
    Now = os:timestamp(),
    File = #file{name=Name, created_at=Now, last_read=never, size=0, replication_factor=0, chunks=[]},
    case mnesia:transaction(fun() -> mnesia:write(File) end) of
        {atomic, ok} ->
            lager:info("file create with name ~p", [Name]),
            {reply, ok, State};
        {aborted, Reason} ->
            later:error("file with name ~p cannot be created because ~p", [Name, Reason]),
            {reply, error, State}
    end;
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

% generate random strings of given length
gen_rand_str(Len) ->
    gen_rand_str(Len, []).
gen_rand_str(0, Acc) ->
    Acc;
gen_rand_str(Len, Acc) ->
    Char = lists:nth(random:uniform(?LEN_AC), ?ALLOWED_CHARS),
    gen_rand_str(Len-1, [Char|Acc]).

curr_time_millis() ->
    {MegaSec, Sec, MicroSec} = os:timestamp(),
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
