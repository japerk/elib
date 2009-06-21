-module(baktools).

-export([backup/0, backup/1, backup/3]).

backup() -> backup(mnesia:system_info(tables)).

backup(Tables) -> backup(Tables, "backup", "/tmp/mnesia.bak").

backup(Tables, Name, Bakfile) ->
	{ok, Name, _Nodes} = mnesia:activate_checkpoint([{name, Name}, {min, Tables}]),
	ok = mnesia:backup_checkpoint(Name, Bakfile),
	ok = mnesia:deactivate_checkpoint(Name).