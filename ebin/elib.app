{application, elib, [
	{description, "Extended Erlang library modules"},
	{vsn, "1.5.11"},
	{mod, {elib, []}},
	{registered, [elib_sup, timeout_server]},
	{applications, [kernel, stdlib]},
	{modules, [
		datetime, elib, elib_sup, elists, emath, emod, estring, etable,
		etest, gen_cache, plists, uri_server, reltools, timeout_server, emp2,
		delegation_server, baktools
	]},
	{env, []}
]}.
