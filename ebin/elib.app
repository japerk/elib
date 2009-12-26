{application, elib, [
	{description, "Extended Erlang library modules"},
	{vsn, "1.5.20"},
	{mod, {elib, []}},
	{registered, [elib_sup, timeout_server]},
	{applications, [kernel, stdlib]},
	{modules, [
		datetime, efile, elib, elib_sup, elists, emath, emod, estring, esys,
		etable, etest, gen_cache, plists, ptable, uri_server, reltools,
		timeout_server, emp2, delegation_server, baktools, rrule
	]},
	{env, []}
]}.
