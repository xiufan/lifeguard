-module(lifeguard_watch_manager_test).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-define(TEST_MODULE, lifeguard_watch_manager).

start_stop_test() ->
    % Test starting is okay.
    {ok, Pid} = ?TEST_MODULE:start_link(?cmd("mktemp -t lifeguard")),

    % Stop it.
    unlink(Pid),
    exit(Pid, normal).
