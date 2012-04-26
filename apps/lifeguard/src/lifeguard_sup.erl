-module(lifeguard_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    % Get the data sources from the application configuration
    {ok, DataSources} = application:get_env(data_sources),

    % Run the data store manager supervisor
    DSManager = {data_store_manager_sup,
        {lifeguard_ds_manager_sup, start_link, [DataSources]},
        permanent, 30000, supervisor, [lifeguard_ds_manager_sup]},

    % Return the full spec
    Children = [DSManager],
    {ok, { {one_for_one, 5, 10}, Children} }.
