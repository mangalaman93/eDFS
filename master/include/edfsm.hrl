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

% metadata structure of master node
% chunks list = [{chunk_id, size}] in the reverse order of their actual order in the file
-record(file, {name, created_at=os:timestamp(), size=0, repfactor=3, chunks=[]}).
% replicas = [{id, ip, port}] with first one being primary
-record(chunk, {id, filename, size, replicas=[]}).
-record(node, {id, state, ip, port, space_util=0}).

% name of chunks, allowed Characters: 0..9, A..Z, a..z, ._ (64)
-define(ALLOWED_CHARS, [46|lists:seq(48, 57)] ++ lists:seq(65, 90) ++ [95|lists:seq(97, 122)]).
-define(LEN_AC, erlang:length(?ALLOWED_CHARS)).

% various parameters
-define(SHUTDOWNTIME, infinity).
-define(MAXR, 10).
-define(MAXT, 60).

% other macros
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, ?SHUTDOWNTIME, Type, [I]}).
-define(CHILD(I, Type, Args), {I, {I, start_link, [Args]}, permanent, ?SHUTDOWNTIME, Type, [I]}).

% processes and gen_server
-define(EDFSM_METADATA_SERVER, edfsm_metadata_server).

% chunk settings
-define(CHUNK_SIZE, 64*1024).
