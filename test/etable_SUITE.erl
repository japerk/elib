-module(etable_SUITE).

-compile(export_all).

-include("ct.hrl").

%%%%%%%%%%%%%%%%%
%% common_test %%
%%%%%%%%%%%%%%%%%

all() -> [fragmentation].

init_per_suite(Config) ->
	mnesia:start(),
	Config.

end_per_suite(Config) ->
	mnesia:stop(),
	Config.

%%%%%%%%%%%%%%%%
%% test cases %%
%%%%%%%%%%%%%%%%

fragmentation(_Config) ->
	% make sure not using mnesia_frag access_module
	mnesia = mnesia:system_info(access_module),
	{atomic, ok} = mnesia:create_table(frag_test, []),
	% create initial test objects before fragmenting table
	Obj1 = {frag_test, "key1", "val1"},
	Obj2 = {frag_test, "key2", "val2"},
	ok = mnesia:dirty_write(Obj1),
	ok = mnesia:dirty_write(Obj2),
	[Obj1] = mnesia:dirty_read(frag_test, "key1"),
	[Obj2] = mnesia:dirty_read(frag_test, "key2"),
	% frag_read works before fragmenting
	[Obj1] = etable:frag_read(frag_test, "key1"),
	[Obj2] = etable:frag_read(frag_test, "key2"),
	% activate fragment and test fragmentation behavior
	ok = etable:fragment(frag_test, 2),
	Node = node(),
	[Node] = etable:frag_info(frag_test, node_pool),
	1 = etable:frag_info(frag_test, n_ram_copies),
	2 = etable:frag_info(frag_test, n_fragments),
	% info not available thru mnesia:table_info, even after fragmenting
	NoInfo = {no_exists, frag_test, node_pool},
	{'EXIT', {aborted, NoInfo}} = (catch mnesia:table_info(frag_test, node_pool)),
	% can read fragmented records
	[Obj1] = etable:frag_read(frag_test, "key1"),
	[Obj2] = etable:frag_read(frag_test, "key2"),
	% frag info works as expected
	[frag_test, frag_test_frag2] = etable:frag_info(frag_test, frag_names),
	2 = etable:frag_info(frag_test, size),
	1 = mnesia:table_info(frag_test, size),
	1 = mnesia:table_info(frag_test_frag2, size),
	% but mnesia:dirty_read can only read from individual fragments
	case mnesia:dirty_read(frag_test, "key1") of
		[] ->
			% obj1 is on first fragment, but obj2 is still on original table
			[Obj1] = mnesia:dirty_read(frag_test_frag2, "key1"),
			[] = mnesia:dirty_read(frag_test_frag2, "key2"),
			[Obj2] = mnesia:dirty_read(frag_test, "key2");
		[Obj1] ->
			% obj2 is on first fragment since obj1 is on original table
			[] = mnesia:dirty_read(frag_test, "key2"),
			[Obj2] = mnesia:dirty_read(frag_test_frag2, "key2"),
			[] = mnesia:dirty_read(frag_test_frag2, "key1")
	end,
	% TODO: test frag_write
	Obj3 = {frag_test, "key3", "val3"},
	ok = etable:frag_write(frag_test, Obj3),
	3 = etable:frag_info(frag_test, size),
	% unfragment table
	etable:unfragment(frag_test),
	3 = mnesia:table_info(frag_test, size),
	[Obj1] = mnesia:dirty_read(frag_test, "key1"),
	[Obj2] = mnesia:dirty_read(frag_test, "key2"),
	[Obj3] = mnesia:dirty_read(frag_test, "key3").