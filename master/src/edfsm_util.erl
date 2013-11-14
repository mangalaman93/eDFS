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
%%% @doc edfs master utility file
%%%

-module(edfsm_util).
-include("edfsm.hrl").


%% ====================================================================
%% API functions
%% ====================================================================
-export([gen_chunk_id/0,
         gen_sec_chunk_id/0]).

%% gen_chunk_id/0
%% ====================================================================
%% @doc generate unique random number (not secure) of length 64 bit.
%% It can generate unique numbers until year 2170 (for 200 years from
%% the time when cpu counter began counting, currently 1970)
-spec gen_chunk_id() -> string().
%% ====================================================================
gen_chunk_id() ->
    gen_chunk_id(48, curr_time_millis(), []).
gen_chunk_id(0, 0, Acc) ->
    lists:reverse(Acc);
gen_chunk_id(Len, Num, Acc) ->
    RestLength = Len - 6,
    << Pos:6, RestNum:RestLength >> = << Num:Len >>,
    gen_chunk_id(RestLength, RestNum, [lists:nth(Pos+1, ?ALLOWED_CHARS)|Acc]).

%% gen_sec_chunk_id/0
%% ====================================================================
%% @doc generate unique <b>secure</b> random number of length 128 bit.
%% It can generate unique numbers until year 2170 (for 200 years from
%% the time when cpu counter began counting, currently 1970)
-spec gen_sec_chunk_id() -> string().
%% ====================================================================
gen_sec_chunk_id() ->
    gen_chunk_id(48, curr_time_millis(), gen_rand_str(8)).


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

% returns time in millisec
curr_time_millis() ->
    {MegaSec, Sec, MicroSec} = erlang:now(),
    1000000000000*MegaSec + Sec*1000000 + MicroSec.
