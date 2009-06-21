-module(elib_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
	{ok, {{one_for_one, 1, 60}, [
		{timeout_server, {timeout_server, start_link, []},
		 permanent, brutal_kill, worker, [timeout_server]}
	]}}.
