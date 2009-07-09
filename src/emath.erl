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

%% @doc Mathematical functions.

-module(emath).

-export([elfhash/1, mean/1, moving_avg/3, random/1, scale/1, scale/3]).

%% @doc Calculates ELFHash of character list.
%% See <a href="http://www.partow.net/programming/hashfunctions/index.html">Hash Functions</a>.
%% @spec elfhash(List::list()|binary()) -> integer()
elfhash(Binary) when is_binary(Binary) ->
	elfhash(binary_to_list(Binary));
elfhash(L) when is_list(L) ->
	F = fun(C, H) ->
			H1 = (H bsl 4) + C, % (H << 4) + C
			X = H1 band 16#f0000000, % H & 0xF0000000
			
			case X of
				0 -> H1 band (bnot X); % H & ~X
				_ -> (H1 bxor (X bsr 24)) band (bnot X) % (H ^ (X >> 24)) & ~X
			end
		end,
	
	lists:foldl(F, 0, L);
elfhash(Term) ->
	elfhash(term_to_binary(Term)).

%% @doc Calculates mean of list of numbers. The mean of empty list is 0.
%% @spec mean([N::number()]) -> number()
%% @see lists:sum/1
mean([]) -> 0;
mean(Nums) -> lists:sum(Nums) / length(Nums).

%% @doc Calculates moving average given a Ratio. The new average is
%% (Old * Ratio) + (New * (1 - Ratio)).
%% @spec moving_avg(Old::number(), New::number(), Ratio::float()) -> number()
moving_avg(Old, New, Ratio) -> (Old * Ratio) + (New * (1 - Ratio)).

%% @doc Drop in replacement for random:uniform.
%% Provides 'extra randomness' by seeding random with a timestamp before
%% calling random:uniform.
%% @spec random(N::integer()) -> integer()
%% @see random
random(N) ->
% drop in replacement for random:uniform that actually works
% random:uniform by default produces the same sequence on each node
	{_, _, X} = now(),
	{H, M, S} = time(),
	H1 = H * X rem 32767,
	M1 = M * X rem 32767,
	S1 = S * X rem 32767,
	random:seed(H1, M1, S1),
	random:uniform(N).

%% @doc Ensures N is between 0 and 1.
%% @spec scale(N::float()) -> float()
%% @equiv scale(N, 0.0, 1.0)
scale(N) -> scale(N, 0.0, 1.0).

%% @doc Ensures N is between Min and Max.
%% @spec scale(N::number(), Min::number(), Max::number()) -> number()
%% @see lists:max/1
%% @see lists:min/1
scale(N, Min, Max) -> lists:max([Min, lists:min([Max, N])]).
