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
-export([run/3,
         run/4]).

%% run/3
%% ====================================================================
%% @doc test run
-spec run(FileName :: string(), N :: integer(), Sep :: string()) -> ok.
%% ====================================================================
run(FileName, N, Sep) ->
	case net_adm:ping(?CLIENT_NODE) of
        pong ->
            edfs:createFile(FileName),
            File = edfs:openFile(FileName, a),
            edfs:write(File, erlang:integer_to_list(random:uniform(1000))),
            do_ntimes(fun() ->
                    Num = erlang:integer_to_list(random:uniform(1000)),
                    edfs:write(File, string:concat(Sep, Num))
                end,
                N),
            edfs:close(File);
        pang ->
            lager:error("unable to connect to client node sitting at ~p", [?CLIENT_NODE]),
            error
    end.

%% run/4
%% ====================================================================
%% @doc test run of large strings
-spec run(FileName, N, Len, Sep) -> ok when
    FileName :: string(),
    N :: integer(),
    Len :: integer(),
    Sep :: string().
%% ====================================================================
run(FileName, N, Len, Sep) ->
    case net_adm:ping(?CLIENT_NODE) of
        pong ->
            edfs:createFile(FileName),
            File = edfs:openFile(FileName, a),
            edfs:write(File, random_str(Len+random:uniform(Len), lists:seq(100,200))),
            do_ntimes(fun() ->
                    Data = random_str(Len+random:uniform(Len), lists:seq(100,200)),
                    edfs:write(File, string:concat(Sep, Data))
                end,
                N),
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

random_str(Len, Chars) ->
    random_str(Len, Chars, []).
random_str(0, _Chars, Acc) ->
    Acc;
random_str(Len, Chars, Acc) ->
    random_str(Len-1, Chars, [random_char(Chars)|Acc]).

random_char(Chars) ->
    lists:nth(random:uniform(length(Chars)), Chars).
