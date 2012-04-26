-module(lifeguard_js_vm).
-behavior(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(vm_state, {
        id, % ID of the VM
        vm  % Actual V8 VM
    }).

%% @doc Start the JS VM under a supervision tree.
start_link(Number) ->
    ServerRef = list_to_atom("js_vm_" ++ integer_to_list(Number)),
    gen_server:start_link({local, ServerRef}, ?MODULE, [Number], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([Number]) ->
    lager:info("JS VM started: ~d", [Number]),

    % Create a new V8 VM
    {ok, VM} = erlv8_vm:start(),

    State = #vm_state{
        id = Number,
        vm = VM
    },

    {ok, State}.

handle_call(_Request, _From, State) -> {noreply, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Request, State) -> {noreply, State}.

terminate(_Reason, State) ->
    lager:info("JS VM stopped; ~d", [State#vm_state.id]),

    % Stop the V8 VM
    erlv8_vm:stop(State#vm_state.vm).

code_change(_OldVsn, State, _Extra) -> {ok, State}.
