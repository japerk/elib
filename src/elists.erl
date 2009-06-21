%% @doc Extra lists functions.
%%
%% @author Jacob Perkins
-module(elists).

-export([mapfilter/2, splitmany/2]).

%% @doc Combines map and filter into 1 operation.
%% Like map, calls F on each item in List. But if F returns false, then the
%% result is ignored.
%% @spec mapfilter(F::function(), List::list()) -> list()
mapfilter(F, List) -> lists:reverse(mapfilter(F, List, [])).

mapfilter(_, [], Results) ->
	Results;
mapfilter(F, [Item | Rest], Results) ->
	case F(Item) of
		false -> mapfilter(F, Rest, Results);
		Term -> mapfilter(F, Rest, [Term | Results])
	end.

splitmany(N, List) -> lists:reverse(splitmany(N, List, [])).

splitmany(_, [], Split) ->
	Split;
splitmany(N, List, Split) when length(List) < N ->
	[List | Split];
splitmany(N, List, Split) ->
	{Part, Rest} = lists:split(N, List),
	splitmany(N, Rest, [Part | Split]).
