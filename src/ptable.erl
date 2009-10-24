%% @doc Like `etable', but uses plists for table chunks.
-module(ptable).

-export([foreach/4, foreach/5, foreach/6, foreach/7]).
-export([ets_foreach/4, ets_foreach/5]).
-export([fold/5, fold/6, fold/7, fold/8, fold/9]).
-export([nobjects/1]).

%%%%%%%%%%%%%
%% foreach %%
%%%%%%%%%%%%%

foreach(F, Table, Spec, Malt) ->
	foreach(async_dirty, F, Table, Spec, read, Malt).

foreach(F, Table, Spec, write, Malt) ->
	foreach(sync_transaction, F, Table, Spec, write, Malt);
foreach(F, Table, Spec, Lock, Malt) ->
	foreach(async_dirty, F, Table, Spec, Lock, Malt).

foreach(Context, F, Table, Spec, Lock, Malt) ->
	foreach(Context, F, Table, Spec, nobjects(Malt), Lock, Malt).

foreach(Context, F, Table, Spec, N, Lock, Malt) ->
	T = fun() ->
			case mnesia:select(Table, Spec, N, Lock) of
				'$end_of_table' -> ok;
				{Objs, Cont} -> foreach2(F, Objs, Malt, Cont)
			end
		end,
	
	mnesia:activity(Context, T).

foreach2(F, Objs, Malt, Cont) ->
	plists:foreach(F, Objs, Malt),
	
	case mnesia:select(Cont) of
		'$end_of_table' -> ok;
		{More, Cont2} -> foreach2(F, More, Malt, Cont2)
	end.

%%%%%%%%%%%%%%%%%
%% ets_foreach %%
%%%%%%%%%%%%%%%%%

ets_foreach(F, Table, Spec, Malt) ->
	ets_foreach(F, Table, Spec, nobjects(Malt), Malt).

ets_foreach(F, Table, Spec, N, Malt) ->
	case ets:select(Table, Spec, N) of
		'$end_of_table' -> ok;
		{Objs, Cont} -> ets_foreach2(F, Objs, Cont, Malt)
	end.

ets_foreach2(F, Objs, Cont, Malt) ->
	plists:foreach(F, Objs, Malt),
	
	case ets:select(Cont) of
		'$end_of_table' -> ok;
		{More, Cont2} -> ets_foreach2(F, More, Cont2, Malt)
	end.

%%%%%%%%%%
%% fold %%
%%%%%%%%%%

fold(F, Acc0, Table, Spec, Malt) ->
	fold(async_dirty, F, Acc0, Table, Spec, read, F, Malt).

fold(F, Acc0, Table, Spec, Fuse, Malt) ->
	fold(async_dirty, F, Acc0, Table, Spec, read, Fuse, Malt).

fold(F, Acc0, Table, Spec, write, Fuse, Malt) ->
	fold(sync_transaction, F, Acc0, Table, Spec, write, Fuse, Malt);
fold(F, Acc0, Table, Spec, Lock, Fuse, Malt) ->
	fold(async_dirty, F, Acc0, Table, Spec, Lock, Fuse, Malt).

fold(Context, F, Acc0, Table, Spec, Lock, Fuse, Malt) ->
	fold(Context, F, Acc0, Table, Spec, nobjects(Malt), Lock, Fuse, Malt).

fold(Context, F, Acc0, Table, Spec, N, Lock, Fuse, Malt) ->
	T = fun() ->
			case mnesia:select(Table, Spec, N, Lock) of
				'$end_of_table' -> Acc0;
				{Objs, Cont} -> fold2(F, Acc0, Objs, Cont, Fuse, Malt)
			end
		end,
	
	mnesia:activity(Context, T).

fold2(F, Acc, Objs, Cont, Fuse, Malt) ->
	Acc2 = plists:fold(F, Fuse, Acc, Objs, Malt),
	
	case mnesia:select(Cont) of
		'$end_of_table' -> Acc2;
		{More, Cont2} -> fold2(F, Acc2, More, Cont2, Fuse, Malt)
	end.

%%%%%%%%%%%
%% utils %%
%%%%%%%%%%%

nobjects(Malt) ->
	S = fun({_, Procs}) -> Procs; (_) -> 1 end,
	% get total number of available processes
	case proplists:get_value(nodes, Malt) of
		undefined ->
			NProcs = proplists:get_value(processes, Malt, 1);
		Nodes ->
			NProcs = lists:sum(lists:map(S, Nodes))
	end,
	% get length of sublists for each plist process
	case elists:first(fun is_integer/1, Malt) of
		none -> SubLen = 1;
		SubLen -> ok
	end,
	% the number of objects to select is enough so there's 1 sublist for each
	% available process. if only have 1 processor and want to process 1 item
	% at a time, the result is 1 element from table at a time. if have
	% 4 processors and want to process 100 elements at a time, result is 400.
	NProcs * SubLen.