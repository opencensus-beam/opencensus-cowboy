-module(opencensus_cowboy2_context).

-export([execute/2]).

execute(Req, Env) ->
  
  case cowboy_req:header(string:lowercase(oc_span_ctx_header:field_name()), Req) of
    undefined -> ok;
    TraceParent ->
      case oc_span_ctx_header:decode(TraceParent) of
        undefined ->          
          error_logger:error_msg("Unable to decode ~p header ~p~n",
                                 [oc_span_ctx_header:field_name(), TraceParent]);
        SpanCtx ->
          ocp:with_span_ctx(SpanCtx)
      end
  end,

  {ok, Req, Env}.
