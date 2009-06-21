%%% @doc Common test utility functions.
%%%
%%% @see application
%%% @see common_test
%%%
%%% @author Jacob Perkins <jacob@weotta.com>
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

start_apps(App) when is_atom(App) ->
	application:load(App),
	{ok, Deps} = application:get_key(App, applications),
	start_apps(Deps ++ [App]);
%% @doc Start applications with optional common test config values.
%% @spec start_apps([Application::atom()]) -> ok
%%
%% @see load_apps/1
%% @see application:start/1
start_apps(Apps) ->
	load_apps(Apps),
	lists:foreach(fun application:start/1, Apps).
