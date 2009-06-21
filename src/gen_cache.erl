%% @doc Generic caching server.
%%
%% @author Jacob Perkins
-module(gen_cache).

-behaviour(gen_server).

-export([start_link/1, cache/3, cache/4, forget/2,
		 recall/2, recall_apply/4, recall_apply/5]).
-export([init/1, handle_call/3, handle_cast/2,
		 handle_info/2, terminate/2, code_change/3]).

% default timeout = 5 minutes
-define(GENCACHE_TIMEOUT, 300000).

-record(memcache, {key, bin, timeout=?GENCACHE_TIMEOUT, tref}).

%%%%%%%%%%%%%%%%%%%%%%
%% public functions %%
%%%%%%%%%%%%%%%%%%%%%%

%% @spec start_link(atom()) -> {ok, pid()}
%% @equiv gen_server:start_link({local, Name}, {@module}, Name, [])
start_link(Name) -> gen_server:start_link({local, Name}, ?MODULE, Name, []).

%% @spec cache(atom(), term(), Value::term()) -> Value::term()
%% @equiv cache(Name, Term, Value, 300000)
cache(Name, Term, Value) -> cache(Name, Term, Value, ?GENCACHE_TIMEOUT).

%% @doc Store Value in cache server, referenced by Term. Value is stored as a
%% compressed binary. Timeout is how long Value should remain in cache.
%% @spec cache(atom(), term(), Value::term(), integer()) -> Value::term()
cache(Name, Term, Value, Timeout) ->
	Bin = term_to_binary(Value, [compressed]),
	Cache = #memcache{key=erlang:phash2(Term), bin=Bin, timeout=Timeout},
	gen_server:cast(Name, {cache, Cache}),
	Value.

%% @doc Remove the Value referenced by Term from the cache server.
%% @spec forget(atom(), term()) -> ok
forget(Name, Term) -> gen_server:cast(Name, {forget, erlang:phash2(Term)}).

%% @doc Lookup the Value referenced by Term from the cache server.
%% @spec recall(atom(), term()) -> term()
recall(Name, Term) ->
	Key = erlang:phash2(Term),
	
	case ets:lookup(Name, Key) of
		[] ->
			undefined;
		[Cache] ->
			gen_server:cast(Name, {reset, Cache}),
			binary_to_term(Cache#memcache.bin)
	end.

%% @doc Looks up result in cache server. If no result is found, calls apply.
%% Use this function to simplify the recall-cache pattern.
%% @spec recall_apply(atom(), atom(), atom(), list()) -> term()
recall_apply(Name, Module, Function, Args) ->
	recall_apply(Name, Module, Function, Args, ?GENCACHE_TIMEOUT).

recall_apply(Name, Module, Function, Args, Timeout) ->
	Term = {Module, Function, Args},
	
	case recall(Name, Term) of
		undefined ->
			Value = apply(Module, Function, Args),
			cache(Name, Term, Value, Timeout);
		Value ->
			Value
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks %%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
init(Name) ->
	ets:new(Name, [named_table, {keypos, #memcache.key}]),
	{ok, Name}.

%% @private
handle_call(From, Request, State) ->
	Terms = [{from, From}, {request, Request}, {state, State}],
	error_logger:warning_report(Terms),
	{reply, badarg, State}.

%% @private
handle_cast({cache, Cache}, Name) ->
	insert(Name, Cache),
	{noreply, Name};
handle_cast({forget, Key}, Name) ->
	delete(Name, Key),
	{noreply, Name};
handle_cast({reset, Cache}, Name) ->
	timer:cancel(Cache#memcache.tref),
	insert(Name, Cache),
	{noreply, Name};
handle_cast(Request, State) ->
	error_logger:warning_report([{request, Request}, {state, State}]),
	{noreply, State}.

%% @private
handle_info(Info, State) ->
	error_logger:warning_report([{info, Info}, {state, State}]),
	{noreply, State}.

%% @private
terminate(_Reason, Name) -> ets:delete(Name).

%% @private
code_change(_Old, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%
%% caching %%
%%%%%%%%%%%%%

insert(Name, Cache) ->
	Timeout = Cache#memcache.timeout,
	Args = [Name, {forget, Cache#memcache.key}],
	{ok, Tref} = timer:apply_after(Timeout, gen_server, cast, Args),
	ets:insert(Name, Cache#memcache{tref=Tref}).

delete(Name, Key) ->
	case ets:lookup(Name, Key) of
		[] ->
			ok;
		[Cache] ->
			timer:cancel(Cache#memcache.tref),
			ets:delete(Name, Key)
	end.
