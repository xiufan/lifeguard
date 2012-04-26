-module(lifeguard_ds_manager).
-behavior(gen_server).
-export([start_link/0, get/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% @doc Start the data source manager.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Get data from a data source.
-spec get(string(), [term()]) -> {ok, term()}.
get(SourceName, Args) ->
    gen_server:call(?MODULE, {get, SourceName, Args}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_Args) -> {ok, no_state}.

handle_call({get, SourceName, Args}, _From, State) ->
    SourceRef = list_to_atom("ds_" ++ SourceName),
    {ok, Response} = gen_server:call(SourceRef, {get, Args}),
    {reply, {ok, Response}, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Request, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
