-module(lifeguard_ds_manager_sup).
-behavior(supervisor).
-export([start_link/1, init/1]).

start_link(DataSources) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, DataSources).

init(DataSources) ->
    % Define the manager...
    Manager = {data_source_manager,
        {lifeguard_ds_manager, start_link, []},
        permanent, 5000, worker, dynamic},

    % Add all the individual sources
    SourceSpecs = [data_source_spec(Source) || Source <- DataSources],

    {ok, {{one_for_one, 10, 10}, [Manager | SourceSpecs]} }.

data_source_spec({Name, Module, Args}) ->
    {{data_source, Name},
        {Module, start_link, [Name, Args]},
        permanent, 30000, worker, [Module]}.
