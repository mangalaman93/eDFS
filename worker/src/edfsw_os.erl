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
%%% @doc edfs os dependent module
%%%

-module(edfsw_os).

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_abs_path/1, mkdir/1, rmdir/1]).

%% get_abs_path/1
%% ====================================================================
%% @doc returns absolute path to the given location which is relative
%% to ~/.edfs folder
-spec get_abs_path(Location) -> Result | {error, Reason} when
    Location :: string(),
    Result   :: string(),
    Reason   :: term().
%% ====================================================================
get_abs_path(Location) ->
    case init:get_argument(home) of
        {ok, [[Home]]} ->
            case filename:pathtype(Location) of
                absolute ->
                    Location;
                _ ->
                    filename:join([Home, ".edfs", Location])
            end;
        error ->
            {error, "init not returned properly"}
    end.

%% mkdir/1
%% ====================================================================
%% @doc create the directory if it does not exist already. Also makes
%% parent directories as needed.
-spec mkdir(Path) -> ok | {error, Reason} when
    Path   :: string(),
    Reason :: term().
%% ====================================================================
mkdir(Path) ->
    filelib:ensure_dir(Path),
    case filelib:is_dir(Path) of
        false ->
            case file:make_dir(Path) of
                ok ->
                    ok;
                {error, eexist} ->
                    ok;
                {error, _Reason}=E ->
                    E
            end;
        true ->
            ok
    end.

%% rmdir/1
%% ====================================================================
%% @doc deletes the given directory
-spec rmdir(Path) -> ok | {error, Reason} when
    Path   :: string(),
    Reason :: term().
%% ====================================================================
rmdir(Path) ->
    case os:type() of
        {unix, _} ->
            Cmd = epax_com:format(["rm -rf ", Path]);
        {win32, _} ->
            Cmd = epax_com:format(["rmdir /s/q ", Path])
    end,
    case cmd(Cmd) of
        {ok, _} ->
            ok;
        {error, _Reason}=E ->
            E
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

cmd(Cmd) ->
    Port = erlang:open_port({spawn, Cmd}, [exit_status]),
    loop(Port, []).
    
loop(Port, Data) ->
    receive
        {Port, {data, NewData}} ->
            loop(Port, Data ++ NewData);
        {Port, {exit_status, 0}} ->
            {ok, Data};
        {Port, {exit_status, Reason}} ->
            {error, Reason}
    end.
