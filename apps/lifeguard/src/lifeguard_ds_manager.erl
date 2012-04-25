-module(lifeguard_ds_manager).
-behavior(gen_server).
-export([start_link/0, get/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Represents the state of our server.
-record(manager_state, { sources }).

%% @doc Start the data source manager.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get(SourceName, Args) ->
    gen_server:call(?MODULE, {get, SourceName, Args}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_Args) ->
    State = #manager_state{sources=dict:new()},
    {ok, State}.

handle_call({get, SourceName, Args}, _From, State) ->
    SourceRef = list_to_atom("ds_" ++ SourceName),
    Response = gen_server:call(SourceRef, {get, Args}),
    {reply, Response, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Request, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
