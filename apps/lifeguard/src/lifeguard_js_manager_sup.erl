-module(lifeguard_js_manager_sup).
-behavior(supervisor).
-export([start_link/1, init/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

start_link(VMCount) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, VMCount).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Supervisor Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(VMCount) ->
    % Start a JavaScript VM up to the max given...
    VMSpecs = vm_specs(VMCount),

    % Return the supervisor spec
    {ok, {{one_for_one, 10, 60}, VMSpecs}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

vm_specs(Count) ->
    vm_specs(Count, []).

vm_specs(0, Accum) ->
    Accum;
vm_specs(Count, Accum) ->
    vm_specs(Count - 1, [vm_spec(Count) | Accum]).

vm_spec(Number) ->
    {{js_vm, Number},
        {lifeguard_js_vm, start_link, [Number]},
        permanent, 60000, worker, [lifeguard_js_vm]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).

vm_specs_empty_test() ->
    [] = vm_specs(0).

vm_specs_count_test() ->
    Result = vm_specs(5),
    ?assert(length(Result) =:= 5).

vm_spec_test() ->
    Number = 5,
    Expected = {{js_vm, Number},
        {lifeguard_js_vm, start_link, [Number]},
        permanent, 60000, worker, [lifeguard_js_vm]},
    Expected = vm_spec(Number).

-endif.
