%% @doc This module serves the static resources for the web UI. This
%% works by simply finding the files in the "static" folder in the priv
%% directory, and serves it.

-module(lifeguard_web_static).
-export([init/1,
         content_types_provided/2,
         resource_exists/2,
         to_response/2]).

-include_lib("webmachine/include/webmachine.hrl").

init(_) ->
    {ok, no_context}.

content_types_provided(ReqData, Context) ->
    StaticPath = wrq:disp_path(ReqData),
    ContentType = webmachine_util:guess_mime(StaticPath),
    {[{ContentType, to_response}], ReqData, Context}.

resource_exists(ReqData, Context) ->
    % Get the static path...
    StaticPath = wrq:disp_path(ReqData),

    % Expand the file path and check that it exists
    FullPath = full_resource_path(StaticPath),
    Exists = filelib:is_regular(FullPath),
    {Exists, ReqData, Context}.

to_response(ReqData, Context) ->
    % Read the static file
    StaticPath  = wrq:disp_path(ReqData),
    {ok, Value} = file:read_file(full_resource_path(StaticPath)),

    % Push it out!
    {Value, ReqData, Context}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec full_resource_path(string()) -> string().
full_resource_path(ShortPath) ->
    filename:join([code:priv_dir(lifeguard), "static", ShortPath]).
