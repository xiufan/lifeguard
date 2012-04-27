-module(lifeguard_watch_manager).
-behavior(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(watch, {
        name,    % Name for the watch
        code,    % Code for the watch (JavaScript)
        interval % Interval that it runs on in milliseconds
    }).

-define(TABLE_NAME, table).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @doc Start the watch manager.
start_link(StoragePath) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, StoragePath, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(StoragePath) ->
    % Open the dets table
    {ok, ?TABLE_NAME} = dets:open_file(?TABLE_NAME, [
            {file, StoragePath},
            {auto_save, 60000}
        ]),

    % Log it out and start
    lager:info("Watch manager started. Storage path: ~p", [StoragePath]),
    {ok, no_state}.

handle_call(_Request, _From, State) -> {noreply, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Request, State) -> {noreply, State}.

terminate(_Reason, _State) ->
    lager:info("Watch manager terminated."),
    ok = dets:close(?TABLE_NAME).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

delete_watch(Name) ->
    dets:delete(?TABLE_NAME, Name).

get_watch(Name) ->
    case dets:lookup(?TABLE_NAME, Name) of
        [] -> {error, no_watch};
        [{Name, Watch}] -> {ok, Watch}
    end.

set_watch(#watch{name=Name} = Watch) ->
    dets:insert(?TABLE_NAME, {Name, Watch});
set_watch(_) ->
    {error, invalid_watch}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests for internal methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).

main_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            fun test_delete_watch/1,
            fun test_delete_watch_nonexistent/1,
            fun test_get_watch_nonexistent/1,
            fun test_set_watch_invalid/1,
            fun test_set_and_get_watch/1
        ]}.

setup() ->
    % Open up a dets table
    StoragePath = ?cmd("mktemp -t lifeguard"),
    {ok, ?TABLE_NAME} = dets:open_file(?TABLE_NAME, [{file, StoragePath}]),

    % State is just the path to the table
    StoragePath.

teardown(StoragePath) ->
    ok = dets:close(?TABLE_NAME),
    ?cmd("rm " ++ StoragePath).

test_delete_watch(_) ->
    fun() ->
            Name = "key",
            Watch = #watch{name=Name},
            ok = set_watch(Watch),
            ok = delete_watch(Name),
            {error, no_watch} = get_watch(Name)
    end.

test_delete_watch_nonexistent(_) ->
    fun() ->
            ok = delete_watch("nope")
    end.

test_get_watch_nonexistent(_) ->
    fun() ->
            {error, no_watch} = get_watch("no good")
    end.

test_set_watch_invalid(_) ->
    fun() ->
            {error, invalid_watch} = set_watch("NOT A WATCH!")
    end.

test_set_and_get_watch(_) ->
    fun() ->
            Name  = "key",
            Watch = #watch{name=Name},
            ok = set_watch(Watch),
            {ok, Watch} = get_watch(Name)
    end.

-endif.
