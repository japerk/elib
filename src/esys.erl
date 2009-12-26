-module(esys).

-export([trace_opts/0, trace_debug/2, handle_debug/3, write_debug/3]).

trace_opts() -> sys:debug_options([log, trace]).

trace_debug(Module, Event) ->
	handle_debug(trace_opts(), Module, Event).

handle_debug(Deb, Module, Event) ->
	sys:handle_debug(Deb, {?MODULE, write_debug}, Module, Event).

write_debug(Dev, Event, Module) ->
	%{_, Time} = calendar:local_time(),
	Time = erlang:now(),
	io:format(Dev, "~p: ~p @ ~p~n", [Event, Module, Time]).