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

%% @doc Module helper functions.

-module(emod).

-export([compile_templates/1, reload/1, proc_chain/1]).

%% @doc Compile application's template modules.
%% @spec compile_templates(atom()) -> ok
compile_templates(App) when is_atom(App) ->
	application:load(App),
	Dir = code:priv_dir(App),
	Out = filename:join([code:lib_dir(App), "ebin"]),
	
	F = fun(Template) ->
			File = filename:join(Dir, atom_to_list(Template)),
			io:format("Compile Template: ~s~n", [File]),
			ok = erlydtl_compiler:compile(File, Template, [{out_dir, Out}])
		end,
	
	{ok, Templates} = application:get_env(App, templates),
	lists:foreach(F, Templates);
compile_templates(Apps) ->
	lists:foreach(fun compile_templates/1, Apps).

%% @doc Forced reload Module or Modules.
%% @spec reload(Module::atom()|[Module::atom()]) -> term()
reload(Module) when is_atom(Module) ->
	case code:purge(Module) of
		false -> code:soft_purge(Module);
		true -> ok
	end,
	
	code:load_file(Module);
reload(Mods) -> lists:foreach(fun reload/1, Mods).

proc_chain(F) ->
	Parent = self(),
	% return a single argument function that spawns a process to call F with arg
	fun(Arg) ->
		C = fun() -> Parent ! {ok, self(), F(Arg)} end,
		Child = spawn(C),
		receive {ok, Child, Result} -> Result end
	end.
