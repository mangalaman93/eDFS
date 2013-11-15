%%% -*- erlang-indent-level: 4;
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
%%% ---------------------------------------------------------------------------
%%% @author Aman Mangal <mangalaman93@gmail.com>
%%%

% various parameters
-define(SHUTDOWNTIME, infinity).
-define(MAXR, 10).
-define(MAXT, 60).
-define(TIMEOUT, 60*1000).
-define(MAX_RETRIES, 3).
% maximum transmission unit
-define(MTU, 1024).
-define(DELIMITER, "\r\n").

% other macros
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, ?SHUTDOWNTIME, Type, [I]}).
-define(CHILD(I, Type, Args), {I, {I, start_link, [Args]}, permanent, ?SHUTDOWNTIME, Type, [I]}).

% processes and gen_server
-define(EDFSC_SERVER, edfsc_server).
-define(EDFSC_FILE_HANDLER, edfsc_file_handler).
-define(EDFSC_FILE_SUP, edfsc_file_sup).

% distributed parameter
-define(MASTER_NODE, 'master@127.0.0.1').
-define(EDFSM_METADATA_SERVER, edfsm_metadata_server).
