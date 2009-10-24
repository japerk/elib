%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Jacob Perkins.
%% Portions created by Jacob Perkins are Copyright 2009, Mr Buzz, Inc.
%% All Rights Reserved.''
%%
%% @author Jacob Perkins
%% @copyright 2009 Mr Buzz, Inc.

%% @doc Extra lists functions.

-module(elists).

-export([first/2, prepend/2, mapfilter/2, mapfilter_chain/2,
		 sort_chain_generator/1, splitmany/2]).

%% @doc Return the first element where F returns true, or none.
first(F, List) ->
	case lists:dropwhile(fun(Elem) -> not F(Elem) end, List) of
		[First | _] -> First;
		[] -> none
	end.

%% @equiv [Elem | List]
prepend(Elem, List) -> [Elem | List].

%% @doc Combines map and filter into 1 operation. Like map, calls F on each
%% item in List. But if F returns false, then the result is dropped.
%% @spec mapfilter(F::function(), List::list()) -> list()
mapfilter(F, List) -> lists:reverse(mapfilter(F, List, [])).

mapfilter(_, [], Results) ->
	Results;
mapfilter(F, [Item | Rest], Results) ->
	case F(Item) of
		false -> mapfilter(F, Rest, Results);
		Term -> mapfilter(F, Rest, [Term | Results])
	end.

mapfilter_chain([], Item) ->
	Item;
mapfilter_chain([F | Rest], Item) ->
	case F(Item) of
		false -> false;
		Item2 -> mapfilter_chain(Rest, Item2)
	end.

sort_chain_generator(SortFuns) -> fun(A, B) -> sort_chain(SortFuns, A, B) end.

sort_chain([], _, _) ->
	true;
sort_chain([F | Rest], A, B) ->
	case F(A, B) of
		true -> true;
		false -> false;
		undefined -> sort_chain(Rest, A, B)
	end.

%% @doc Split `List' into many lists, each with `N' elements. The last list
%% may contain less than `N' elements.
%% @spec splitmany(N::integer(), List::list()) -> [list()]
splitmany(N, List) -> lists:reverse(splitmany(N, List, [])).

splitmany(_, [], Split) ->
	Split;
splitmany(N, List, Split) when length(List) < N ->
	[List | Split];
splitmany(N, List, Split) ->
	{Part, Rest} = lists:split(N, List),
	splitmany(N, Rest, [Part | Split]).