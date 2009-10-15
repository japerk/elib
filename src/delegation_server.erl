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

-module(delegation_server).

-behaviour(gen_fsm).

-export([start_link/2, set_delegates/2, queue/2, queue_singleton/2, queue_len/1]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
		 terminate/3, code_change/4]).
-export([processing/2, processing/3, singleton/2, singleton/3]).
% for testing
-export([free_nodes/1]).

-record(delegation_state, {len=0, queue=queue:new(), procs=dict:new(), delegates=[]}).

%%%%%%%%%%%%%%%%
%% public API %%
%%%%%%%%%%%%%%%%

start_link(Ref, Delegates) -> gen_fsm:start_link(Ref, ?MODULE, Delegates, []).

%% @doc Set new delegate specs. Delegates is a list of {Node, Max} where Max
%% is the maximum number of process that can execute in parallel on the
%% delegate node.
%% @spec set_delegates(Ref, [{node(), integer()}]) -> ok
set_delegates(Ref, Delegates) ->
	gen_fsm:send_all_state_event(Ref, {delegates, Delegates}).

%% @doc Queue a process for execution on one of the delegate nodes.
queue(Ref, MFA) -> gen_fsm:send_event(Ref, {normal, MFA}).

%% @doc Queue a singleton process for execution. When all other processes
%% queue before the singleton are complete, then the singleton process will
%% execute on a delegate node, while the rest of the delegates wait. No other
%% processes will execute until the singleton process completes.
queue_singleton(Ref, MFA) -> gen_fsm:send_event(Ref, {singleton, MFA}).

queue_len(Ref) -> gen_fsm:sync_send_all_state_event(Ref, queue_len).

%%%%%%%%%%%%%
%% gen_fsm %%
%%%%%%%%%%%%%

init(Delegates) ->
	process_flag(trap_exit, true),
	Procs = dict:from_list([{Node, 0} || {Node, _} <- Delegates]),
	{ok, processing, #delegation_state{procs=Procs, delegates=Delegates}}.

handle_event({delegates, Delegates}, Current, State) ->
	% remove nodes from procs that aren't in delegates
	F = fun(Node, _) -> proplists:is_defined(Node, Delegates) end,
	Procs = dict:filter(F, State#delegation_state.procs),
	{next_state, Current, State#delegation_state{procs=Procs, delegates=Delegates}};
handle_event(_, Current, State) ->
	{next_state, Current, State}.

handle_sync_event(queue_len, _, Current, State) ->
	{reply, State#delegation_state.len, Current, State};
handle_sync_event(_, _, Current, State) ->
	{reply, undefined, Current, State}.

handle_info({done, Node}, processing, State) ->
	% it's possible that a node has been removed by set_delegates, in which
	% case we don't want to update the process counter to negative numbers
	case dict:is_key(Node, State#delegation_state.procs) of
		true ->
			Procs = dict:update_counter(Node, -1, State#delegation_state.procs),
			{Next, State2} = process_state(State#delegation_state{procs=Procs});
		false ->
			{Next, State2} = process_state(State)
	end,
	
	{next_state, Next, State2};
handle_info({done, _}, singleton, State) ->
	{Next, State2} = process_state(State),
	{next_state, Next, State2};
handle_info({'EXIT', Pid, _}, Current, State) ->
	handle_info({done, node(Pid)}, Current, State);
handle_info(_, Current, State) ->
	{next_state, Current, State}.

terminate(_, _, _) -> ok.

code_change(_, Current, State, _) ->
	process_flag(trap_exit, true),
	{ok, Current, State}.

%%%%%%%%%%%%%%%%
%% processing %%
%%%%%%%%%%%%%%%%

processing(Event, State) ->
	{Next, NewState} = handle_processing_event(Event, State),
	{next_state, Next, NewState}.

processing(Event, _, State) ->
	{Next, NewState} = handle_processing_event(Event, State),
	{reply, Next, Next, NewState}.

handle_processing_event({normal, MFA}, State) ->
	process_state(insert({normal, MFA}, State));
handle_processing_event({singleton, MFA}, State) ->
	process_state(insert({singleton, MFA}, State));
handle_processing_event(_, State) ->
	{processing, State}.

%%%%%%%%%%%%%%%
%% singleton %%
%%%%%%%%%%%%%%%

singleton(Event, State) ->
	{Next, NewState} = handle_singleton_event(Event, State),
	{next_state, Next, NewState}.

singleton(Event, _, State) ->
	{Next, NewState} = handle_singleton_event(Event, State),
	{reply, Next, Next, NewState}.

handle_singleton_event({normal, MFA}, State) ->
	{singleton, insert({normal, MFA}, State)};
handle_singleton_event({singleton, MFA}, State) ->
	{singleton, insert({singleton, MFA}, State)};
handle_singleton_event(_, State) ->
	{singleton, State}.

%%%%%%%%%%%%%%
%% queueing %%
%%%%%%%%%%%%%%

insert(Item, State) ->
	Queue = queue:in(Item, State#delegation_state.queue),
	Len = State#delegation_state.len + 1,
	State#delegation_state{len=Len, queue=Queue}.

pop(State) ->
	{{value, Item}, Queue} = queue:out(State#delegation_state.queue),
	{Item, Queue, State#delegation_state.len - 1}.

process_state(State) ->
	case queue:is_empty(State#delegation_state.queue) of
		true -> {processing, State};
		false -> process_state2(State)
	end.

process_state2(State) ->
	case queue:head(State#delegation_state.queue) of
		{normal, _} -> delegate_normal(State);
		{singleton, _} -> delegate_singleton(State)
	end.

delegate_normal(State) ->
	case free_nodes(State) of
		[] ->
			{processing, State};
		[{Node, _} | _] ->
			% process front item from queue on first available node
			{{normal, MFA}, Queue, Len} = pop(State),
			Procs = dict:update_counter(Node, 1, State#delegation_state.procs),
			spawn_delegate(MFA, Node),
			% continue processing from queue until no more free nodes
			process_state(State#delegation_state{len=Len, queue=Queue, procs=Procs})
	end.

delegate_singleton(State) ->
	{Nodes, Counts} = lists:unzip(free_nodes(State)),
	NDelegates = length(State#delegation_state.delegates),
	
	case lists:sum(Counts) of
		0 when length(Nodes) == NDelegates ->
			% all nodes are free, so use first node to do singleton process
			{{singleton, MFA}, Queue, Len} = pop(State),
			spawn_delegate(MFA, hd(Nodes)),
			{singleton, State#delegation_state{len=Len, queue=Queue}};
		_ ->
			% wait until all nodes are free
			{processing, State}
	end.

%% @doc Return available nodes with counts, sorted by lowest count.
%% @private
free_nodes(State) ->
	Procs = State#delegation_state.procs,
	
	F = fun({Node, Max}) ->
			case dict:find(Node, Procs) of
				{ok, Count} when Count < Max -> {Node, Count};
				{ok, _} -> false;
				error -> {Node, 0}
			end
		end,
	
	lists:keysort(2, elists:mapfilter(F, State#delegation_state.delegates)).

spawn_delegate({Module, Function, Args}, Node) ->
	Pid = self(),
	
	F = fun() ->
			% use catch so that errors are ignored
			catch apply(Module, Function, Args),
			% send a process message so that we can test using a normal
			% receive block, instead of mocking a gen_fsm
			Pid ! {done, node()}
		end,
	% we are trapping exits (can't monitor pids if spawning on different nodes)
	proc_lib:spawn_link(Node, F).
