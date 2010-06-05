-module(edict).

-export([find_default/2, find_default/3, from_proplist/1]).

find_default(Key, Dict) -> find_default(Key, Dict, undefined).

%% @doc Return Default value if Key is not found in Dict.
find_default(Key, Dict, Default) ->
	case dict:find(Key, Dict) of
		{ok, Val} -> Val;
		error -> Default
	end.

%% @doc Creates a dict of {Key, [Val]} from a proplist that may have multiple
%% values for the same key, as in [{Key, Val1}, {Key, Val2}].
from_proplist(Props) ->
	F = fun({Key, Val}, Dict) -> dict:append(Key, Val, Dict) end,
	lists:foldl(F, dict:new(), Props).