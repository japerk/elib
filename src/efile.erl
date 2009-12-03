-module(efile).

-export([recursive_delete/1]).

recursive_delete(Dir) -> recursive_delete(Dir, filelib:wildcard("*", Dir)).

recursive_delete(Dir, []) ->
	file:del_dir(Dir);
recursive_delete(Dir, [Name | Rest]) ->
	Path = filename:join(Dir, Name),
	
	case filelib:is_file(Path) of
		true -> file:delete(Path);
		false -> ok
	end,
	
	case filelib:is_dir(Path) of
		true -> recursive_delete(Path);
		false -> ok
	end,
	
	recursive_delete(Dir, Rest).