%% @doc Round robin server starts child processes and creates a queue of Pids.
%% On each call to next(), the Pid at the front of the queue is returned and
%% moved to the back of the queue. If a child process exits, it is removed from
%% the queue and a new child process is started.
-module(round_robin_server).

-behaviour(gen_server).

-export([start_link/2, start_link/3]).
-export([next/0, next/1, next/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%%%%%%%%%%%%%%
%% public api %%
%%%%%%%%%%%%%%%%

start_link(MFA, N) -> gen_server:start_link(?MODULE, {MFA, N}, []).

%% @doc Start round robin server named Name with N children.
%% MFA must return {ok, Pid}.
start_link(Name, MFA, N) -> gen_server:start_link(Name, ?MODULE, {MFA, N}, []).

next() -> next(?MODULE).

next(Ref) -> next(Ref, infinity).

%% @doc Get the next Pid from the server
next(Ref, Timeout) -> gen_server:call(Ref, next, Timeout).

%%%%%%%%%%%%%%%%
%% gen_server %%
%%%%%%%%%%%%%%%%

init({{Mod, Fun, Args}=MFA, N}) ->
	F = fun(_) ->
			{ok, Pid} = apply(Mod, Fun, Args),
			link(Pid),
			Pid
		end,
	
	process_flag(trap_exit, true),
	Pids = lists:map(F, lists:seq(1, N)),
	{ok, {MFA, queue:from_list(Pids)}}.

handle_call(next, _From, {MFA, Queue}=State) ->
	case queue:out(Queue) of
		{empty, Queue} -> {reply, empty, State};
		% empty at front of queue is returned and put at the end of queue
		{{value, Pid}, Q2} -> {reply, Pid, {MFA, queue:in(Pid, Q2)}}
	end;
handle_call(_, _, State) ->
	{reply, undefined, State}.

handle_cast(_, State) -> {noreply, State}.

handle_info({'EXIT', Pid, Reason}, {{Mod, Fun, Args}=MFA, Queue}) ->
	error_logger:warning_report([?MODULE, {child_exit, Pid}, {reason, Reason}]),
	F = fun(P) -> P == Pid end,
	Q2 = queue:filter(F, Queue),
	{ok, Pid2} = apply(Mod, Fun, Args),
	link(Pid2),
	{noreply, {MFA, queue:in(Pid2, Q2)}};
handle_info(_, State) ->
	{noreply, State}.

terminate(_, _) -> ok.

code_change(_, State, _) -> {ok, State}.