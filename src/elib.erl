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

-module(elib).

-behaviour(application).

-export([start/2, stop/1]).
-export([validate/2, validate/3, validate_match/2]).

%%%%%%%%%%%%%%%%%
%% application %%
%%%%%%%%%%%%%%%%%

start(_Type, _Args) -> elib_sup:start_link().

stop(_State) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% tuple / proplist validation %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

validate(Key, Props) -> validate(Key, Props, []).

validate(Key, Props, Guards) when is_atom(Key), is_list(Props), is_list(Guards) ->
	case proplists:lookup(Key, Props) of
		% TODO: throwing notfound on bad validate doesn't make sense if it
		% translates to 404 in updater API.
		none -> throw(notfound);
		{Key, Val} -> validate_match({Key, Val}, [{{Key, '$1'}, Guards, ['$1']}])
	end.

validate_match(Tuple, Match) ->
	case ets:test_ms(Tuple, Match) of
		{error, Errs} -> throw(Errs);
		{ok, false} -> throw(badarg);
		{ok, Val} -> Val
	end.