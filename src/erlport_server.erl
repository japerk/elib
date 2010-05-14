-module(erlport_server).

-behaviour(gen_server).

-export([start_link/3, start_link_opts/3, request/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%%%%%%%%%%%%%%
%% public api %%
%%%%%%%%%%%%%%%%

start_link(ServerName, PySpawn, PyPath) ->
	PortOpts = [{packet, 1}, binary, {env, [{"PYTHONPATH", PyPath}]}],
	start_link_opts(ServerName, PySpawn, PortOpts).

start_link_opts(ServerName, PySpawn, PortOpts) ->
	gen_server:start_link({local, ServerName}, ?MODULE, {PySpawn, PortOpts}, []).

request(ServerRef, Request) -> gen_server:call(ServerRef, Request).

%%%%%%%%%%%%%%%%
%% gen_server %%
%%%%%%%%%%%%%%%%

init({PySpawn, PortOpts}) ->
	Port = open_port({spawn, PySpawn}, PortOpts),
	{ok, {Port, queue:new()}}.

handle_call(Request, From, {Port, Queue}) ->
	port_command(Port, term_to_binary(Request)),
	{noreply, {Port, queue:in(From, Queue)}}.

handle_info({Port, {data, Data}}, {Port, Queue}) ->
	{{value, To}, Queue2} = queue:out(Queue),
	gen_server:reply(To, binary_to_term(Data)),
	{noreply, {Port, Queue2}}.

handle_cast(_, State) -> {noreply, State}.

terminate(_, {Port, Queue}) ->
	port_close(Port),
	R = fun(To) -> gen_server:reply(To, closed) end,
	lists:foreach(R, queue:to_list(Queue)).

code_change(_Old, State, _Extra) -> {ok, State}.