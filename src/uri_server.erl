%% @doc Uri Server generates uris suitable for thing ids. It basically exists
%% because random:uniform is process oriented and does not work well if numbers
%% are generated in many different processes. In the current model, each yaws
%% POST in worl_updater_api is a separate process, and so there is a high
%% likelihood of processes being seeded the same, which causes duplicate uri
%% generation. That won't happen with uri_server because it is a locally
%% unique process. It definitely slows things down, but updates don't need to
%% happen fast.
%%
%% @author Jacob Perkins
-module(uri_server).

-behaviour(gen_server).

-export([
	start_link/2,
	start_link/3,
	start_link/4,
	uri/1
]).

-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

%%%%%%%%%%%%%%%%%%%%%%
%% public functions %%
%%%%%%%%%%%%%%%%%%%%%%

%% @spec start_link(atom(), atom()) -> {ok, pid()}
%% @equiv start_link(Name, Table, [], 30)
start_link(Name, Table) -> start_link(Name, Table, [], 30).

%% @spec start_link(atom(), atom(), string()) -> {ok, pid()}
%% @equiv start_link(Name, Table, Prefix ++ ":", 30)
start_link(Name, Table, Prefix) -> start_link(Name, Table, Prefix ++ ":", 30).

%% @spec start_link(atom(), atom(), string(), integer()) -> {ok, pid()}
%% @equiv gen_server:start_link({local, Name}, {@module}, {Table, Prefix, Len}, [])
start_link(Name, Table, Prefix, Len) ->
	gen_server:start_link({local, Name}, ?MODULE, {Table, Prefix, Len}, []).

%% @doc Generate a new uri.
%% @spec uri(atom()) -> string()
uri(Name) -> gen_server:call(Name, uri).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks %%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
init(State) ->
	{A, B, C} = now(),
	random:seed(A, B, C),
	{ok, State}.

%% @private
handle_call(uri, From, {Table, Prefix, Len}=State) ->
	Uri = Prefix ++ estring:random(Len),
	
	case mnesia:dirty_read(Table, Uri) of
		[] ->
			{reply, Uri, State};
		_ ->
			% if Uri already exists in table, try again
			error_logger:warning_report([{found, Uri}]),
			handle_call(uri, From, State)
	end.

%% @private
handle_cast(_Request, State) -> {noreply, State}.

%% @private
handle_info(_Info, State) -> {noreply, State}.

%% @private
terminate(_Reason, _State) -> ok.

%% @private
code_change(_Old, State, _Extra) -> {ok, State}.
