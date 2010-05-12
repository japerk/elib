%% @doc Osmos Term Storage aka Ordered Term Storage.
%% Helper module for using the osmos on-disk ordered set key-value store.
-module(ots).

-export([open/3, open/4]).
-export([clear/1, delete/2, read/2, read/3, write/3]).
-export([foldl/3, foldl/4, foldl/5, foldl/6, foldl/7]).
-export([foreach/2, foreach/3, foreach/4, foreach/5, foreach/6]).
-export([select_range/4, select_range/5]).

-define(NOBJECTS, 100).
-define(CMP0, fun(_Key) -> 0 end).

%%%%%%%%%%%%%%%%%%%
%% table formats %%
%%%%%%%%%%%%%%%%%%%

-ifdef(OSMOS).

% if osmos is installed, then can compile with
% -pa PATH/TO/OSMOS/ebin -I PATH/TO/OSMOS/src -DOSMOS
% to get these table format functions

-include("osmos.hrl").
-export([float_format/1, uint_format/1, max_uint_format/1, merge_return_greater/3]).

float_format(BlockSize) ->
	Fmt = osmos_table_format:new(term, term_replace, BlockSize),
	SC = fun osmos_table_format:value_is_delete/2,
	D = fun osmos_table_format:value_is_delete/2,
	Fmt#osmos_table_format{short_circuit=SC, delete=D}.

uint_format(BlockSize) ->
	Fmt = osmos_table_format:new(term, uint64_sum_delete, BlockSize),
	Fmt#osmos_table_format{merge=fun osmos_table_format:merge_return_later/3}.

max_uint_format(BlockSize) ->
	Fmt = osmos_table_format:new(term, uint64_sum_delete, BlockSize),
	Fmt#osmos_table_format{merge=fun merge_return_greater/3}.

% Earlier may be 'delete'
merge_return_greater(_Key, Earlier, Later) when is_atom(Earlier) -> Later;
merge_return_greater(_Key, Earlier, Later) when Later > Earlier -> Later;
merge_return_greater(_Key, Earlier, _Later) -> Earlier.

-endif.

%%%%%%%%%%%%%%%%
%% open table %%
%%%%%%%%%%%%%%%%

open(Table, Dir, Format) -> open(Table, Dir, Format, []).

open(Table, Dir, Format, Opts) ->
	ok = filelib:ensure_dir(Dir),
	osmos:open(Table, [{directory, Dir}, {format, Format} | Opts]).

%%%%%%%%%
%% ops %%
%%%%%%%%%

clear(Table) -> foreach(fun({Key, _}) -> delete(Table, Key) end, Table).

delete(Table, Key) -> write(Table, Key, delete).

read(Table, Key) -> read(Table, Key, 0).

read(Table, Key, Default) ->
	case osmos:read(Table, Key) of
		{ok, delete} -> Default;
		{ok, Val} -> Val;
		not_found -> Default
	end.

write(Table, Key, Val) -> timeout_write(Table, Key, Val, 1).

% try writing up to 50 times, then exit
timeout_write(_, _, _, N) when N > 50 ->
	exit(timeout);
timeout_write(Table, Key, Val, N) ->
	try
		osmos:write(Table, Key, Val)
	catch
		% can get gen_server timeout during high volume writes
		exit:{timeout, Stack} ->
			esys:trace_debug(?MODULE, {timeout, Stack}),
			erlang:yield(),
			esys:trace_debug(?MODULE, {sleep, N*10}),
			% sleeps for N*10 milliseconds, sleeping longer on each increment
			timer:sleep(N*10),
			timeout_write(Table, Key, Val, N+1)
	end.

%%%%%%%%%%%
%% foldl %%
%%%%%%%%%%%

foldl(F, Acc0, Table) -> foldl(F, Acc0, Table, ?CMP0).

foldl(F, Acc0, Table, Cmp) -> foldl(F, Acc0, Table, Cmp, fun select_all/2).

foldl(F, Acc0, Table, Cmp, S) ->
	foldl(F, Acc0, Table, less_lo(Cmp), less_hi(Cmp), S).

foldl(F, Acc0, Table, L, H, S) -> foldl(F, Acc0, Table, L, H, S, ?NOBJECTS).

foldl(F, Acc0, Table, L, H, S, NObjects) ->
	foldl_select(F, Acc0, Table, NObjects, select_range(Table, L, H, S, NObjects)).

foldl_select(_, Acc, _, _, {[], _}) ->
	Acc;
foldl_select(_, _, _, _, {error, Reason}) ->
	{error, Reason};
foldl_select(F, Acc0, Table, NObjects, {Results, Cont}) ->
	Acc = lists:foldl(F, Acc0, Results),
	foldl_select(F, Acc, Table, NObjects, select_continue(Table, Cont, NObjects)).

%%%%%%%%%%%%%
%% foreach %%
%%%%%%%%%%%%%

foreach(F, Table) -> foreach(F, Table, ?CMP0).

foreach(F, Table, Cmp) -> foreach(F, Table, Cmp, fun select_all/2).

foreach(F, Table, Cmp, S) -> foreach(F, Table, less_lo(Cmp), less_hi(Cmp), S).

foreach(F, Table, L, H, S) -> foreach(F, Table, L, H, S, ?NOBJECTS).

foreach(F, Table, L, H, S, NObjects) ->
	foreach_select(F, Table, NObjects, select_range(Table, L, H, S, NObjects)).

foreach_select(_, _, _, {[], _}) ->
	ok;
foreach_select(_, _, _, {error, Reason}) ->
	{error, Reason};
foreach_select(F, Table, NObjects, {Results, Cont}) ->
	lists:foreach(F, Results),
	foreach_select(F, Table, NObjects, select_continue(Table, Cont, NObjects)).

%%%%%%%%%%%%
%% select %%
%%%%%%%%%%%%

select_range(Table, L, H, S) -> select_range(Table, L, H, S, ?NOBJECTS).

select_range(Table, L, H, S, NObjects) ->
	case osmos:select_range(Table, L, H, S, NObjects) of
		{ok, Results, Cont} -> {Results, Cont};
		{error, Reason} -> {error, Reason}
	end.

select_continue(Table, Cont, NObjects) ->
	case osmos:select_continue(Table, Cont, NObjects) of
		{ok, Results, Cont2} -> {Results, Cont2};
		{error, Reason} -> {error, Reason}
	end.

%%%%%%%%%%%%%%%%%%
%% cmp function %%
%%%%%%%%%%%%%%%%%%

less_lo(Cmp) -> fun(Key) -> Cmp(Key) < 0 end.

less_hi(Cmp) -> fun(Key) -> Cmp(Key) < 1 end.

select_all(_, _) -> true.