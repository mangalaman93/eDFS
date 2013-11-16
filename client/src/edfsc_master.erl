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
%%% @doc edfs master API for client
%%%

-module(edfsc_master).
-include("edfsc.hrl").


% ====================================================================
%% API functions
%% ====================================================================
-export([create_file/1,
		 open_file/2,
		 written_data/5]).

%% createFile/1
%% ====================================================================
%% @doc creates a file with the given file name
-spec create_file(FileName) -> ok | error when
    FileName :: string().
%% ====================================================================
create_file(FileName) ->
    gen_server:call(global:whereis_name(?EDFSM_METADATA_SERVER), {createFile, FileName}).

%% open_file/2
%% ====================================================================
%% @doc gets information about the given file
-spec open_file(FileName, Mode) -> Info | error when
    FileName :: string(),
    Mode     :: atom(),
    Info     :: tuple().
%% ====================================================================
open_file(FileName, Mode) ->
	gen_server:call(global:whereis_name(?EDFSM_METADATA_SERVER), {openFile, FileName, Mode}).

%% written_data/5 @todo
%% ====================================================================
%% @doc informs to master node that data is successully written on
%% chunks and returns new chunk if demanded
-spec written_data(FileName, Mode, Chunk, SentSize, WantNew) -> ok | error | tuple() when
    FileName :: string(),
    Mode     :: atom(),
    Chunk    :: string(),
    SentSize :: integer(),
    WantNew :: boolean().
%% ====================================================================
written_data(FileName, Mode, Chunk, SentSize, WantNew) ->
	gen_server:call(global:whereis_name(?EDFSM_METADATA_SERVER), {writtenData, FileName, Mode, Chunk, SentSize, WantNew}).
