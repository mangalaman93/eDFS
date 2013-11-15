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
%%% @doc edfs test run
%%%

-module(test).
-include("test.hrl").


%% ====================================================================
%% API functions
%% ====================================================================
-export([run/0]).

%% run/0
%% ====================================================================
%% @doc test run
-spec run() -> ok.
%% ====================================================================
run() ->
	case net_adm:ping(?CLIENT_NODE) of
        pong ->
            edfs:createFile("numbers2.txt"),
            File = edfs:openFile("numbers2.txt", a),
            edfs:write(File, erlang:integer_to_list(random:uniform(1000))),
            do_ntimes(fun() ->
                    Num = erlang:integer_to_list(random:uniform(1000)),
                    edfs:write(File, string:concat("\n", Num))
                end,
                9999),
            edfs:close(File);
        pang ->
            lager:error("unable to connect to client node sitting at ~p", [?CLIENT_NODE]),
            error
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

do_ntimes(_F, 0) ->
	ok;
do_ntimes(F, N) ->
	F(),
	do_ntimes(F, N-1).
