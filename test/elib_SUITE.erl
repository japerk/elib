-module(elib_SUITE).

-compile(export_all).

-include("ct.hrl").

all() -> [validate].

%%%%%%%%%%%%%%%%
%% test cases %%
%%%%%%%%%%%%%%%%

validate(_Config) ->
	"foo" = elib:validate(foo, [{foo, "foo"}]),
	notfound = (catch elib:validate(foo, [{bar, "foo"}])),
	"foo" = elib:validate(foo, [{foo, "foo"}], [{is_list, '$1'}]),
	badarg = (catch elib:validate(foo, [{foo, "foo"}], [{is_integer, '$1'}])).
