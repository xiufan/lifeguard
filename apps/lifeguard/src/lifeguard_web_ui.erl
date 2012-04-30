-module(lifeguard_web_ui).
-export([init/1,
         to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init(_) ->
    {ok, no_context}.

to_html(ReqData, Context) ->
    % Redirect to "/pages/index.html" so that we can just use the static
    % folders for our stuff.
    Response  = wrq:set_resp_header("location", "/pages/index.html", ReqData),
    Response1 = wrq:do_redirect(true, Response),
    {{halt, 302}, Response1, Context}.
