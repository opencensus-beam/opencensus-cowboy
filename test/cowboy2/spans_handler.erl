-module(spans_handler).

%% -behaviour(cowboy_handler).

-export([
         init/2,
         terminate/3
        ]).

%% ===================================================================
%% cowboy_handler callbacks
%% ===================================================================

init(Req, _Opts) ->
  Body = case ocp:current_span_ctx() of
           undefined -> <<"qwe">>;
           SpanCtx -> oc_span_ctx_header:encode(SpanCtx)
         end,
  {ok, cowboy_req:reply(200, #{}, Body, Req), undefined}.

terminate(_Reason, _Req, _State) ->
  ok.
