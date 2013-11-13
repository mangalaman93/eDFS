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
%%% @doc edfs API
%%%

-module(edfs).
-include("edfs.hrl").


%% ====================================================================
%% API functions
%% ====================================================================
-export([createFile/1, openFile/2, write/2, close/1]).

%% createFile/1
%% ====================================================================
%% @doc creates a file
-spec createFile(FileName :: string()) -> ok.
%% ====================================================================
createFile(FileName) ->
	global:sync(),
	gen_server:cast(global:whereis_name(?EDFSC_SERVER), {createFile, FileName}).

%% openFile/1
%% ====================================================================
%% @doc opens a file in given mode
-spec openFile(FileName, Mode) -> pid() when
	FileName :: string(),
	Mode     :: atom().
%% ====================================================================
openFile(FileName, Mode) ->
	gen_server:call(global:whereis_name(?EDFSC_SERVER), {openFile, FileName, Mode}).

%% write/2
%% ====================================================================
%% @doc writes data to file
-spec write(File, Data) -> ok when
	File :: pid(),
	Data :: term().
%% ====================================================================
write(File, Data) ->
	gen_server:cast(File, {writeFile, Data}).

%% close/1
%% ====================================================================
%% @doc closes the already opened file
-spec close(File) -> ok when
	File :: pid().
%% ====================================================================
close(File) ->
	gen_server:call(File, {closeFile}).


%% ====================================================================
%% Internal functions
%% ====================================================================
