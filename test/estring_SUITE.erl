-module(estring_SUITE).

-compile(export_all).

-include("ct.hrl").

%%%%%%%%%%%%%%%%%
%% common_test %%
%%%%%%%%%%%%%%%%%

all() -> [split, replace, title, utf8].

init_per_suite(Config) -> Config.

end_per_suite(Config) -> Config.

%%%%%%%%%%%%%%%%
%% test cases %%
%%%%%%%%%%%%%%%%

split(_Config) ->
	["foo", "bar"] = estring:split("foo:bar", ":"),
	["foobar"] = estring:split("foobar", ":"),
	{"foo", "bar"} = estring:splitc("foo:bar", $:),
	{"foobar", []} = estring:splitc("foobar", $:).

replace(_Config) ->
	{ok, "foo:baz"} = estring:replace("foo:bar", "bar", "baz"),
	{error, notfound} = estring:replace("foo:bar", "baz", "bzz"),
	{ok, "foo-bar-baz"} = estring:replace_all("foo:bar:baz", ":", "-").

title(_Config) ->
	"Foo Bar" = estring:to_title("foo bar"),
	"Foo" = estring:title_word("FOO"),
	true = estring:is_upper("FOO"),
	false = estring:is_upper("Foo").

utf8(_Config) ->
	false = estring:is_utf8([128]),
	true = estring:is_utf8([127]),
	[127] = estring:encode_utf8([127]),
	[194, 128] = estring:encode_utf8([128]).
