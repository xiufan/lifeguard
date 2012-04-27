-module(lifeguard_web_ui).
-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init(_) ->
    {ok, no_context}.

to_html(ReqData, Context) ->
    {ok, Content} = index_dtl:render([]),
    {Content, ReqData, Context}.
