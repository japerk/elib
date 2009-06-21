-module(delegation_server_SUITE).

-include("ct.hrl").

-compile(export_all).

-record(delegation_state, {len=0, queue=queue:new(), procs=dict:new(), delegates=[]}).

-define(NOOP, {erlang, self, []}).

%%%%%%%%%%%%%%%%%
%% common_test %%
%%%%%%%%%%%%%%%%%

all() -> [free_nodes, set_delegates, delegate_process, queue_process,
		  singleton_process, singleton_queue].

init_per_suite(Config) -> Config.

end_per_suite(Config) -> Config.

%%%%%%%%%%%%%%%%
%% test cases %%
%%%%%%%%%%%%%%%%

free_nodes(_) ->
	State = #delegation_state{delegates=[{node(), 1}]},
	Free = [{node(), 0}],
	Free = delegation_server:free_nodes(State).

set_delegates(_) ->
	State0 = #delegation_state{delegates=[{node(), 1}]},
	{next_state, processing, State0} = delegation_server:handle_info({done, node()}, processing, State0),
	{next_state, processing, State0} = delegation_server:handle_info({done, foo@bar}, processing, State0),
	
	Procs1 = dict:update_counter(node(), 1, State0#delegation_state.procs),
	State1 = State0#delegation_state{procs=Procs1},
	Procs2 = dict:update_counter(node(), -1, State1#delegation_state.procs),
	State2 = State1#delegation_state{procs=Procs2},
	{next_state, processing, State2} = delegation_server:handle_info({done, node()}, processing, State1),
	{next_state, processing, State1} = delegation_server:handle_info({done, foo@bar}, processing, State1),
	
	Delegates3 = [{foo@bar, 1}],
	State3 = State0#delegation_state{delegates=Delegates3},
	{next_state, processing, State3} = delegation_server:handle_event({delegates, Delegates3}, processing, State0),
	{next_state, processing, State3} = delegation_server:handle_event({delegates, Delegates3}, processing, State1),
	{next_state, processing, State3} = delegation_server:handle_event({delegates, Delegates3}, processing, State2).

delegate_process(_) ->
	InitState = #delegation_state{delegates=[{node(), 1}]},
	{reply, 0, processing, InitState} = delegation_server:handle_sync_event(queue_len, [], processing, InitState),
	
	Procs1 = dict:update_counter(node(), 1, InitState#delegation_state.procs),
	State1 = InitState#delegation_state{procs=Procs1},
	{next_state, processing, State1} = delegation_server:processing({normal, ?NOOP}, InitState),
	{reply, 0, processing, State1} = delegation_server:handle_sync_event(queue_len, [], processing, State1),
	
	receive
		{done, Node} -> true = (Node == node())
	after
		2000 -> ct:fail("done timeout")
	end,
	
	Procs2 = dict:update_counter(node(), -1, State1#delegation_state.procs),
	State2 = State1#delegation_state{procs=Procs2},
	{next_state, processing, State2} = delegation_server:handle_info({done, node()}, processing, State1),
	{reply, 0, processing, State2} = delegation_server:handle_sync_event(queue_len, [], processing, State2).

queue_process(_) ->
	State0 = #delegation_state{delegates=[{node(), 1}]},
	{reply, 0, processing, State0} = delegation_server:handle_sync_event(queue_len, [], processing, State0),
	
	Procs1 = dict:update_counter(node(), 1, dict:new()),
	State1 = State0#delegation_state{procs=Procs1},
	{reply, 0, processing, State1} = delegation_server:handle_sync_event(queue_len, [], processing, State1),
	
	Procs2 = dict:update_counter(node(), -1, State1#delegation_state.procs),
	State2 = State1#delegation_state{procs=Procs2},
	{reply, 0, processing, State2} = delegation_server:handle_sync_event(queue_len, [], processing, State2),
	
	Queue3 = queue:in({normal, ?NOOP}, State1#delegation_state.queue),
	State3 = State1#delegation_state{len=1, queue=Queue3},
	{reply, 1, processing, State3} = delegation_server:handle_sync_event(queue_len, [], processing, State3),
	
	{next_state, processing, State1} = delegation_server:processing({normal, ?NOOP}, State0),
	{next_state, processing, State3} = delegation_server:processing({normal, ?NOOP}, State1),
	
	receive
		{done, _} -> ok
	after
		2000 -> ct:fail("done 2 timeout")
	end,
	
	{next_state, processing, State1} = delegation_server:handle_info({done, node()}, processing, State3),
	
	receive
		{done, _} -> ok
	after
		2000 -> ct:fail("done 3 timeout")
	end,
	
	{next_state, processing, State2} = delegation_server:handle_info({done, node()}, processing, State1).
	
singleton_process(_) ->
	State0 = #delegation_state{delegates=[{node(), 1}]},
	{next_state, singleton, State0} = delegation_server:processing({singleton, ?NOOP}, State0),
	
	receive
		{done, _} -> ok
	after
		2000 -> ct:fail("done timeout")
	end,
	
	{next_state, processing, State0} = delegation_server:handle_info({done, node()}, singleton, State0).

singleton_queue(_) ->
	State0 = #delegation_state{delegates=[{node(), 1}]},
	{reply, 0, processing, State0} = delegation_server:handle_sync_event(queue_len, [], processing, State0),
	
	Queue1 = queue:in({normal, ?NOOP}, State0#delegation_state.queue),
	State1 = State0#delegation_state{len=1, queue=Queue1},
	{next_state, singleton, State1} = delegation_server:singleton({normal, ?NOOP}, State0),
	{reply, 1, singleton, State1} = delegation_server:handle_sync_event(queue_len, [], singleton, State1),
	
	Queue2 = queue:in({singleton, ?NOOP}, State1#delegation_state.queue),
	State2 = State1#delegation_state{len=2, queue=Queue2},
	{next_state, singleton, State2} = delegation_server:singleton({singleton, ?NOOP}, State1),
	{reply, 2, singleton, State2} = delegation_server:handle_sync_event(queue_len, [], singleton, State2),
	
	Queue3 = queue:tail(Queue2),
	Procs3 = dict:update_counter(node(), 1, State2#delegation_state.procs),
	State3 = State2#delegation_state{len=1, queue=Queue3, procs=Procs3},
	{next_state, processing, State3} = delegation_server:handle_info({done, node()}, singleton, State2),
	{reply, 1, processing, State3} = delegation_server:handle_sync_event(queue_len, [], processing, State3),
	
	receive
		{done, _} -> ok
	after
		2000 -> ct:fail("done timeout")
	end,
	
	Procs4 = dict:update_counter(node(), -1, State3#delegation_state.procs),
	Queue4 = queue:tail(Queue3),
	State4 = State3#delegation_state{len=0, queue=Queue4, procs=Procs4},
	{next_state, singleton, State4} = delegation_server:handle_info({done, node()}, processing, State3),
	{reply, 0, singleton, State4} = delegation_server:handle_sync_event(queue_len, [], singleton, State4),
	
	{next_state, processing, State4} = delegation_server:handle_info({done, node()}, singleton, State4),
	{reply, 0, processing, State4} = delegation_server:handle_sync_event(queue_len, [], processing, State4).
