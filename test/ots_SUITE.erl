-module(ots_SUITE).

-compile(export_all).

-include("ct.hrl").

%%%%%%%%%%%%%%%%%
%% common_test %%
%%%%%%%%%%%%%%%%%

all() -> [float_table, uint_table, max_uint_table, foldl_sum, foreach_ets].

init_per_suite(Config) ->
	etest:start_apps([elib, osmos]),
	Dir = filename:join(?config(priv_dir, Config), "ots_test/"),
	[{block_size, 256}, {dir, Dir}, {table, ots_test} | Config].

end_per_suite(Config) -> Config.

init_per_testcase(_, Config) -> Config.

end_per_testcase(_, Config) ->
	ots:close(?config(table, Config)),
	efile:recursive_delete(?config(dir, Config)),
	Config.

%%%%%%%%%%%%%%%%
%% test cases %%
%%%%%%%%%%%%%%%%

float_table(Config) ->
	Fmt = ots:float_format(?config(block_size, Config)),
	{ok, Tab} = ots:open(?config(table, Config), ?config(dir, Config), Fmt),
	0 = ots:read(Tab, foo),
	ots:write(Tab, foo, 1.5),
	1.5 = ots:read(Tab, foo),
	ots:delete(Tab, foo),
	0 = ots:read(Tab, foo),
	ots:write(Tab, foo, 5.2),
	5.2 = ots:read(Tab, foo),
	ots:write(Tab, bar, 10.8),
	10.8 = ots:read(Tab, bar),
	ots:clear(Tab),
	0 = ots:read(Tab, foo),
	0 = ots:read(Tab, bar).

uint_table(Config) ->
	Fmt = ots:uint_format(?config(block_size, Config)),
	{ok, Tab} = ots:open(?config(table, Config), ?config(dir, Config), Fmt),
	0 = ots:read(Tab, foo),
	ots:write(Tab, foo, 1),
	1 = ots:read(Tab, foo),
	ots:delete(Tab, foo),
	0 = ots:read(Tab, foo),
	ots:write(Tab, foo, 5),
	5 = ots:read(Tab, foo),
	ots:write(Tab, bar, 10),
	10 = ots:read(Tab, bar),
	ots:clear(Tab),
	0 = ots:read(Tab, foo),
	0 = ots:read(Tab, bar).

% NOTE: disabled due to noisy logging, but does pass
uint_bad_write(Config) ->
	Fmt = ots:uint_format(?config(block_size, Config)),
	{ok, Tab} = ots:open(?config(table, Config), ?config(dir, Config), Fmt),
	
	case catch ots:write(Tab, foo, 1.5) of
		1.5 -> ct:fail("uint shouldn't accept floats");
		{'EXIT', {{function_clause, _Stack}, _Caller}} -> ok
	end.

max_uint_table(Config) ->
	Fmt = ots:max_uint_format(?config(block_size, Config)),
	{ok, Tab} = ots:open(?config(table, Config), ?config(dir, Config), Fmt),
	0 = ots:read(Tab, foo),
	ots:write(Tab, foo, 1),
	1 = ots:read(Tab, foo),
	ots:delete(Tab, foo),
	0 = ots:read(Tab, foo),
	ots:write(Tab, foo, 5),
	5 = ots:read(Tab, foo),
	ots:write(Tab, foo, 10),
	10 = ots:read(Tab, foo),
	ots:write(Tab, foo, 8),
	10 = ots:read(Tab, foo).

foldl_sum(Config) ->
	Fmt = ots:uint_format(?config(block_size, Config)),
	{ok, Tab} = ots:open(?config(table, Config), ?config(dir, Config), Fmt),
	
	Nums = lists:seq(1, 9),
	Sum = lists:sum(Nums),
	lists:foreach(fun(N) -> ots:write(Tab, integer_to_list(N), N) end, Nums),
	S = fun({_Key, N}, Acc) -> Acc + N end,
	Sum = ots:foldl(S, 0, Tab),
	
	L = fun(Key) -> Key < "1" end,
	H = fun(Key) -> Key < "6" end,
	Select = fun(_, _) -> true end,
	Sum5 = lists:sum(lists:seq(1, 5)),
	Sum5 = ots:foldl(S, 0, Tab, L, H, Select),
	
	Cmp51 = fun(Key) ->
		if
			Key > "5" -> 1;
			Key < "1" -> -1;
			true -> 0
		end
	end,
	
	Sum5 = ots:foldl(S, 0, Tab, Cmp51),
	Cmp52 = fun(Key) when Key < "1" -> -1; (Key) when Key < "6" -> 0; (_) -> 1 end,
	Sum5 = ots:foldl(S, 0, Tab, Cmp52),
	
	Cmp38 = fun(Key) when Key < "3" -> -1; (Key) when Key < "9" -> 0; (_) -> 1 end,
	Sum38 = lists:sum(lists:seq(3, 8)),
	Sum38 = ots:foldl(S, 0, Tab, Cmp38),
	
	Cmp69 = fun(Key) when Key < "6" -> -1; (Key) when Key =< "9" -> 0; (_) -> 1 end,
	Sum69 = lists:sum(lists:seq(6, 9)),
	Sum69 = ots:foldl(S, 0, Tab, Cmp69).

foreach_ets(Config) ->
	Fmt = ots:uint_format(?config(block_size, Config)),
	{ok, Tab} = ots:open(?config(table, Config), ?config(dir, Config), Fmt),
	
	Nums = lists:seq(1, 9),
	Sum = lists:sum(Nums),
	lists:foreach(fun(N) -> ots:write(Tab, integer_to_list(N), N) end, Nums),
	
	I = fun(KV) -> ets:insert(test, KV) end,
	ets:new(test, [private, named_table]),
	ots:foreach(I, Tab),
	S = fun({_Key, N}, Acc) -> Acc + N end,
	Sum = ets:foldl(S, 0, test),
	ets:delete(test),
	
	ets:new(test, [private, named_table]),
	Cmp38 = fun(Key) when Key < "3" -> -1; (Key) when Key < "9" -> 0; (_) -> 1 end,
	Sum38 = lists:sum(lists:seq(3, 8)),
	ots:foreach(I, Tab, Cmp38),
	Sum38 = ets:foldl(S, 0, test),
	ets:delete(test).