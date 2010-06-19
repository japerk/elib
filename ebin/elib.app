{application, elib, [
	{description, "Extended Erlang library modules"},
	{vsn, "1.7.2"},
	{mod, {elib, []}},
	{registered, [elib_sup, timeout_server]},
	{applications, [kernel, stdlib]},
	{modules, [
		baktools, datetime, delegation_server, edict, efile, elib,
		elib_sup, elists, emath, emod, emp2, erlport_server, estring, esys,
		etable, etest, gen_cache, geohash, ots, plists, ptable, uri_server,
		reltools, round_robin_server, rrule, timeout_server
	]},
	{env, []}
]}.
