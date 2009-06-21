%% @doc Module helper functions.
%%
%% @author Jacob Perkins
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
