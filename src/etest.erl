%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Jacob Perkins.
%% Portions created by Jacob Perkins are Copyright 2009, Mr Buzz, Inc.
%% All Rights Reserved.''
%%
%% @author Jacob Perkins
%% @copyright 2009 Mr Buzz, Inc.

%%% @doc Common test utility functions.

-module(etest).

-export([init_env/1, load_apps/1, start_apps/1]).

%% @doc Set application env based on optional common test config values.
%% @spec init_env(Application::atom()) -> ok
%%
%% @see application:set_env/3
%% @see ct:get_config/2
init_env(App) ->
	S = fun({Key, Val}) -> application:set_env(App, Key, Val) end,
	lists:foreach(S, ct:get_config(App, [])).

%% @doc Load applications with optional common test config values.
%% @spec load_apps([Application::atom()]) -> ok
%%
%% @see init_env/1
%% @see application:load/1
load_apps(Apps) ->
	lists:foreach(fun application:load/1, Apps),
	lists:foreach(fun init_env/1, Apps).

start_app(App) when is_atom(App) ->
	application:load(App),
	init_env(App),
	{ok, Deps} = application:get_key(App, applications),
	lists:foreach(fun start_app/1, Deps),
	application:start(App).

start_apps(App) when is_atom(App) -> start_app(App);
%% @doc Start applications with optional common test config values.
%% @spec start_apps([Application::atom()]) -> ok
%%
%% @see load_apps/1
%% @see application:start/1
start_apps(Apps) -> lists:foreach(fun start_app/1, Apps).
