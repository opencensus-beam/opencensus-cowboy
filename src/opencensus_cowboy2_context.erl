-module(opencensus_cowboy2_context).

-export([execute/2]).

execute(Req, Env) ->
  Module = application:get_env(opencensus, http_propagation, oc_propagation_http_tracecontext),

  Headers = cowboy_req:headers(Req),
  case Module:from_headers(Headers) of
    undefined -> ok;
    SpanCtx -> ocp:with_span_ctx(SpanCtx)
  end,

  case try_decode_header(Req, oc_tag_ctx_header) of
    undefined -> ok;
    Tags -> ocp:with_tags(Tags)
  end,

  {ok, Req, Env}.


try_decode_header(Req, Module) ->
  case cowboy_req:header(string:lowercase(Module:field_name()), Req) of
    undefined -> undefined;
    RawThing ->
      case Module:decode(RawThing) of
        {ok, Thing} -> Thing;
        {error, Error} ->
          error_logger:error_msg("Unable to decode ~p header ~p~n",
                                 [Module:field_name(), Error]),
          undefined
      end
  end.
