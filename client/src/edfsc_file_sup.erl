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
%%% @doc edfs client supervisor for file handlers

-module(edfsc_file_sup).
-behaviour(supervisor).
-export([init/1]).
-include("edfsc.hrl").


%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).

%% start_link/0
%% ====================================================================
%% @doc starts the file handler supervisor
-spec start_link() -> Result when
	 Result :: {ok, pid()}
			 | ignore
             | {error, Reason},
	 Reason :: {already_started, pid()}
		  	 | shutdown
			 | term().
%% ====================================================================
start_link() ->
	supervisor:start_link({local, ?EDFSC_FILE_SUP}, ?MODULE, []).


%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% @private
init([]) ->
    EdfscFileHandler = {?EDFSC_FILE_HANDLER, {?EDFSC_FILE_HANDLER, start_link, []}, temporary, ?SHUTDOWNTIME, worker, [?EDFSC_FILE_HANDLER]},
    {ok, {{simple_one_for_one, ?MAXR, ?MAXT},
          [EdfscFileHandler]}}.
