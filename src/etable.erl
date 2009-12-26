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

%% @doc Mnesia utility functions.

-module(etable).

-export([opts/2, force_load/1, force_load/2, force_delete_table/1,
		 add_copies/3, add_copies/4]).
-export([fragment/2, activate_frag/1, unfragment/1,
		 add_frag/1, del_frag/1, add_frags/2, del_frags/2,
		 add_frag_node/2, del_frag_node/2, clear_frags/1, delete_frags/1,
		 frag_info/2, frag_read/2, frag_write/2, frag_activity/3]).
-export([collect/2, collect/4, foldl/3, foldl/4, foldl/6, foldl/7]).
-export([foreach/2, foreach/3, foreach/4, foreach/6]).
-export([ets_foreach/2, ets_foreach/3, ets_foreach/4]).
-export([ets_update_counter/2, ets_update_counter/3]).
-export([dets_foreach/3, dets_foreach/4]).
-export([dets_update_counter/2, dets_update_counter/3]).

%%%%%%%%%%%%%%%%%%%%%%
%% table management %%
%%%%%%%%%%%%%%%%%%%%%%

opts(Table, Keys) -> [{Key, mnesia:table_info(Table, Key)} || Key <- Keys].

%% @spec force_load([atom()]) -> ok
%% @equiv force_load(Tables, 20000)
force_load(Tables) -> force_load(Tables, 20000).

%% @doc Force load any Tables that do not load within Timeout.
%% @spec force_load([atom()], integer()) -> ok
%% @see mnesia:wait_for_tables/2
%% @see mnesia:force_load_table/1
force_load(Tables, Timeout) ->
	case mnesia:wait_for_tables(Tables, Timeout) of
		ok -> ok;
		{timeout, Tabs} -> lists:foreach(fun mnesia:force_load_table/1, Tabs);
		{error, Reason} -> error_logger:error_report([{mnesia_load, Reason}])
	end.

%% @doc Use this function if mnesia:table_info(Tab, cstruct) returns a value
%% but mnesia:delete_table(Tab) returns {aborted, {no_exists, Tab}}.
%% Only do this as a last resort.
force_delete_table(Tab) ->
	% TODO: could do our own sanity checks to make sure table is actually in
	% an unstable condition as documented above.
	Cs = mnesia:table_info(Tab, cstruct),
	% this is copied from mnesia_schema:make_delete_table2/1,
	% but without any sanity checks
	T = fun() ->
			TidTs = mnesia_schema:get_tid_ts_and_lock(schema, write),
			Op = {op, delete_table, mnesia_schema:cs2list(Cs)},
			mnesia_schema:insert_schema_ops(TidTs, [Op])
		end,
	
	{atomic, ok} = mnesia_schema:schema_transaction(T).

%% @spec add_copies([atom()], atom(), atom()) -> ok
%% @equiv add_copies(Tabs, Node, Type, 20000)
add_copies(Tabs, Node, Type) -> add_copies(Tabs, Node, Type, 20000).

%% @doc Add table copies of Type to Node. Waits for Tabs to load first.
%% @spec add_copies([atom()], atom(), atom(), integer()) -> ok
%% @see mnesia:add_table_copy/3
add_copies(Tabs, Node, Type, Timeout) ->
	F = fun(Tab) -> mnesia:add_table_copy(Tab, Node, Type) end,
	ok = mnesia:wait_for_tables(Tabs, Timeout),
	lists:foreach(F, Tabs).

%%%%%%%%%%%%%%%%%%%
%% fragmentation %%
%%%%%%%%%%%%%%%%%%%

% TODO: apparently fragmented tables have to be accessed thru mnesia:activity/4
% with mnesia_frag AccessMod. That could cause sweeping change waves, forcing
% apps to use etable:frag_activity and helper functions.
% OR, can set mnesia access_module to mnesia_frag in sys.config. Must be set
% at boot, doing application:set_env doesn't change mnesia:system_info(access_module).
% BUT mnesia_monitor:set_env(access_module, mnesia_frag) does change the
% result of mnesia:system_info(access_module).
%
% NOTE: even after setting mnesia_monitor:set_env(access_module, mnesia_frag),
% index table fragments aren't getting any records, which means erldex writes
% aren't using mnesia_frag. That means all erldex functions need to use
% mnesia:activity.

fragment(Table, NFrags) ->
	% if table is not fragmented, then activate
	case mnesia:table_info(Table, frag_properties) of
		[] -> {atomic, ok} = activate_frag(Table);
		_ -> ok
	end,
	
	N = frag_info(Table, n_fragments),
	% make n_fragments equal NFrags (see note in activate)
	if
		N < NFrags -> add_frags(Table, NFrags - N);
		N > NFrags -> del_frags(Table, N - NFrags);
		true -> ok
	end.

activate_frag(Table) ->
	RamNodes = frag_info(Table, ram_copies),
	DiscNodes = frag_info(Table, disc_copies),
	DiscOnlyNodes = frag_info(Table, disc_only_copies),
	
	% NOTE: n_fragments cannot be set on activate, only at table creation
	FragProps = [
		{node_pool, RamNodes ++ DiscNodes ++ DiscOnlyNodes},
		{n_ram_copies, length(RamNodes)},
		{n_disc_copies, length(DiscNodes)},
		{n_disc_only_copies, length(DiscOnlyNodes)}
	],
	
	mnesia:change_table_frag(Table, {activate, FragProps}).

unfragment(Table) -> unfragment(Table, frag_info(Table, n_fragments)).

unfragment(_, 0) ->
	ok;
unfragment(Table, 1) ->
	% number of fragments must be 1 to deactivate a fragmented table
	mnesia:change_table_frag(Table, deactivate);
unfragment(Table, N) ->
	del_frag(Table),
	unfragment(Table, N-1).

add_frag(Table) ->
	FragDist = frag_info(Table, frag_dist),
	% NOTE: this crashes on large existing tables when trying to add first frag
	% that means tables need to be fragmented well before they get too big,
	% otherwise can't fragment. Tried each available eheap_alloc strategy
	% with bin/start +MHas, but they all crash the same. That means the only
	% way to fragment an existing large table is to backup, then restore with
	% a modified schema, using frag_properties at table creation time. Some way
	% where you can either recreate the table, or clear it & add frags, before
	% recreating it.
	mnesia:change_table_frag(Table, {add_frag, FragDist}).

del_frag(Table) -> mnesia:change_table_frag(Table, del_frag).

add_frags(_, 0) ->
	ok;
add_frags(Table, N) ->
	add_frag(Table),
	add_frags(Table, N-1).

del_frags(_, 0) ->
	ok;
del_frags(Table, N) ->
	del_frag(Table),
	del_frags(Table, N-1).

add_frag_node(Table, Node) -> mnesia:change_table_frag(Table, {add_node, Node}).

del_frag_node(Table, Node) -> mnesia:change_table_frag(Table, {del_node, Node}).

clear_frags(Table) ->
	lists:foreach(fun mnesia:clear_table/1, frag_info(Table, frag_names)).

delete_frags(Table) ->
	lists:foreach(fun mnesia:delete_table/1, frag_info(Table, frag_names)).

frag_info(Table, Key) ->
	frag_activity(sync_dirty, fun mnesia:table_info/2, [Table, Key]).

frag_read(Table, Key) ->
	frag_activity(sync_dirty, fun mnesia:read/1, [{Table, Key}]).

frag_write(Table, Record) ->
	frag_activity(sync_dirty, fun mnesia:write/3, [Table, Record, write]).

frag_activity(Context, F, Args) ->
	mnesia:activity(Context, F, Args, mnesia_frag).

%%%%%%%%%%%%%%%%
%% collection %%
%%%%%%%%%%%%%%%%

%% @spec collect(atom(), Spec) -> Result
%% @equiv collect(Table, Spec, 100, read)
collect(Table, Spec) -> collect(Table, Spec, 100, read).

%% @doc Collect all results from Table, NObjects at a time. Spec is a match
%% specification, Lock is a mnesia lock atom. Collection is async dirty.
%% @spec collect(atom(), Spec, integer(), atom()) -> Result
%% @see mnesia:select/4
collect(Table, Spec, NObjects, Lock) ->
	T = fun() ->
			case mnesia:select(Table, Spec, NObjects, Lock) of
				'$end_of_table' -> [];
				{Objs, Cont} -> collect2(Cont, Objs)
			end
		end,
	
	mnesia:async_dirty(T).

collect2(Cont, Objs) ->
	case mnesia:select(Cont) of
		'$end_of_table' -> Objs;
		{More, Cont2} -> collect2(Cont2, More ++ Objs)
	end.

%%%%%%%%%%%%%
%% folding %%
%%%%%%%%%%%%%

foldl(F, Acc0, Table) -> dirty_foldl(F, Acc0, Table, mnesia:dirty_first(Table)).

dirty_foldl(_, Acc, _, '$end_of_table') ->
	Acc;
dirty_foldl(F, Acc, Table, Key) ->
	Acc2 = lists:foldl(F, Acc, mnesia:dirty_read(Table, Key)),
	dirty_foldl(F, Acc2, Table, mnesia:dirty_next(Table, Key)).

foldl(F, Acc0, Table, Spec) -> foldl(F, Acc0, Table, Spec, 100, read).

foldl(F, Acc0, Table, Spec, NObject, Lock) ->
	foldl(async_dirty, F, Acc0, Table, Spec, NObject, Lock).

foldl(Context, F, Acc0, Table, Spec, NObject, Lock) ->
	T = fun() ->
			case mnesia:select(Table, Spec, NObject, Lock) of
				'$end_of_table' -> Acc0;
				{Objs, Cont} -> foldl2(F, Acc0, Objs, Cont)
			end
		end,
	
	mnesia:activity(Context, T).

foldl2(F, Acc, Objs, Cont) ->
	Acc2 = lists:foldl(F, Acc, Objs),
	
	case mnesia:select(Cont) of
		'$end_of_table' -> Acc2;
		{More, Cont2} -> foldl2(F, Acc2, More, Cont2)
	end.

%%%%%%%%%%%%%
%% foreach %%
%%%%%%%%%%%%%

%% @doc Iterate over each record in table, calling F on each record.
%% Uses mnesia:dirty_first/1 and mnesia:dirty_next/2.
%% @spec foreach(function(), atom()) -> ok
foreach(F, Table) -> dirty_foreach(F, Table, mnesia:dirty_first(Table)).

dirty_foreach(_, _, '$end_of_table') ->
	ok;
dirty_foreach(F, Table, Key) ->
	lists:foreach(F, mnesia:dirty_read(Table, Key)),
	dirty_foreach(F, Table, mnesia:dirty_next(Table, Key)).

%% @spec foreach(function(), atom(), Spec) -> ok
%% @equiv foreach(F, Table, Spec, read)
foreach(F, Table, Spec) -> foreach(F, Table, Spec, read).

%% @doc If Lock is write, then Context = sync_transaction. Otherwise
%% Context = async_dirty.
%% @spec foreach(function(), atom(), Spec, atom()) -> ok
%% @equiv foreach(Context, F, Table, Spec, 100, Lock)
foreach(F, Table, Spec, write) ->
	foreach(sync_transaction, F, Table, Spec, 100, write);
foreach(F, Table, Spec, Lock) ->
	foreach(async_dirty, F, Table, Spec, 100, Lock).

%% @doc Calls F on each result from Table, NObjects at a time. Context is the
%% mnesia transaction context, Spec is a match specification, Lock is a
%% mnesia lock atom.
%% @spec foreach(atom(), function(), atom(), Spec, integer(), atom()) -> Result
foreach(Context, F, Table, Spec, NObjects, Lock) ->
	T = fun() ->
			case mnesia:select(Table, Spec, NObjects, Lock) of
				'$end_of_table' -> ok;
				{Objs, Cont} -> foreach2(F, Objs, Cont)
			end
		end,
	
	mnesia:activity(Context, T).

foreach2(F, Objs, Cont) ->
	lists:foreach(F, Objs),
	
	case mnesia:select(Cont) of
		'$end_of_table' -> ok;
		{More, Cont2} -> foreach2(F, More, Cont2)
	end.

%%%%%%%%%%%%%%%
%% ets utils %%
%%%%%%%%%%%%%%%

ets_foreach(F, Table) -> ets_foreach_iter(F, Table, ets:first(Table)).

ets_foreach_iter(_, _, '$end_of_table') ->
	ok;
ets_foreach_iter(F, Table, Key) ->
	lists:foreach(F, ets:lookup(Table, Key)),
	ets_foreach_iter(F, Table, ets:next(Table, Key)).

ets_foreach(F, Table, Spec) -> ets_foreach(F, Table, Spec, 100).

ets_foreach(F, Table, Spec, Limit) ->
	case ets:select(Table, Spec, Limit) of
		'$end_of_table' -> ok;
		{Objs, Cont} -> ets_foreach2(F, Objs, Cont)
	end.

ets_foreach2(F, Objs, Cont) ->
	lists:foreach(F, Objs),
	
	case ets:select(Cont) of
		'$end_of_table' -> ok;
		{More, Cont2} -> ets_foreach2(F, More, Cont2)
	end.

ets_update_counter(Table, Key) -> ets_update_counter(Table, Key, 1).

ets_update_counter(Table, Key, Count) ->
	case ets:member(Table, Key) of
		false ->
			% ets:insert is much faster than ets:insert_new
			true = ets:insert(Table, {Key, Count}),
			Count;
		true ->
			% ets:update_counter throws badarg if Key doesn't exist
			ets:update_counter(Table, Key, Count)
	end.

%%%%%%%%%%%%%%%%
%% dets utils %%
%%%%%%%%%%%%%%%%

dets_foreach(F, Table, Spec) -> dets_foreach(F, Table, Spec, 100).

dets_foreach(F, Table, Spec, Limit) ->
	dets:safe_fixtable(Table, true),
	
	case dets:select(Table, Spec, Limit) of
		'$end_of_table' ->
			dets:safe_fixtable(Table, false);
		{error, Reason} ->
			error_logger:error_report([dets_foreach, {table, Table},
				{spec, Spec}, {error, Reason}]);
		{Objs, Cont} ->
			dets_foreach2(F, Table, Objs, Cont)
	end.

dets_foreach2(F, Table, Objs, Cont) ->
	lists:foreach(F, Objs),
	
	case dets:select(Cont) of
		'$end_of_table' ->
			dets:safe_fixtable(Table, false);
		{error, Reason} ->
			error_logger:error_report([dets_foreach2, {table, Table}, {error, Reason}]);
		{More, Cont2} ->
			dets_foreach2(F, Table, More, Cont2)
	end.

dets_update_counter(Table, Key) -> dets_update_counter(Table, Key, 1).

dets_update_counter(Table, Key, Count) ->
	case dets:member(Table, Key) of
		false ->
			dets:insert(Table, {Key, Count}),
			Count;
		true ->
			dets:update_counter(Table, Key, Count)
	end.