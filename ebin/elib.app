{application, elib, [
	{description, "Extended Erlang library modules"},
	{vsn, "1.6.3"},
	{mod, {elib, []}},
	{registered, [elib_sup, timeout_server]},
	{applications, [kernel, stdlib]},
	{modules, [
		datetime, edict, efile, elib, elib_sup, elists, emath, emod, emp2,
		erlport_server, estring, esys, etable, etest, gen_cache, ots,
		plists, ptable, uri_server, reltools, timeout_server,
		delegation_server, baktools, rrule
	]},
	{env, []}
]}.
