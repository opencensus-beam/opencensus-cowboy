-module(opencensus_cowboy2_context_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% MACROS
%% ===================================================================

-define(URL(Path),
        "http://localhost:" ++ integer_to_list(?config(port, Config)) ++ "/" ++ Path).

%% @doc All tests of this suite.
all() ->
  [
   test_span_context_decoding,
   test_no_span_context_decoding,
   test_tag_context_decoding,
   test_no_tag_context_decoding
  ].

%% @doc Groups of tests
groups() ->
  [
  ].

%% @doc Start the application.
init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(cowboy),
  {ok, _} = application:ensure_all_started(opencensus),
  {Port, Listener} = opencensus_cowboy2_app:start(),
  %% debugger:start(),
  %% timer:sleep(80000),
  [{port, Port}, {listener, Listener} | Config].

%% @doc Stop the application.
end_per_suite(Config) ->
  ok = application:stop(cowboy),
  ok = application:stop(opencensus),
  Config.

end_per_testcase(_, Config) ->
  Config.

%% ===================================================================
%% TESTS
%% ===================================================================

test_span_context_decoding(Config) ->

  SpanCtx = oc_trace:start_span("My Span", undefined),

  {ok, Response} =
    httpc:request(get, {?URL("spans"),
                        [{binary_to_list(oc_span_ctx_header:field_name()),
                          oc_span_ctx_header:encode(SpanCtx)}]}, [], []),

  ?assertEqual(SpanCtx, oc_span_ctx_header:decode(body(Response))).

test_no_span_context_decoding(Config) ->

  _SpanCtx = oc_trace:start_span("My Span", undefined),

  {ok, Response} =
    httpc:request(get, {?URL("spans"),
                        []}, [], []),

  ?assertEqual("qwe", body(Response)).

test_tag_context_decoding(Config) ->

  Tags = oc_tags:new(#{"key1" => "value1",
                       "key2" => "key2"}),

  {ok, Response} =
    httpc:request(get, {?URL("tags"),
                        [{binary_to_list(oc_tag_ctx_header:field_name()),
                          oc_tag_ctx_header:encode(Tags)}]}, [], []),

  ?assertEqual(Tags, oc_tag_ctx_header:decode(body(Response))).

test_no_tag_context_decoding(Config) ->

  Tags = #{},

  {ok, Response} =
    httpc:request(get, {?URL("tags"),
                        []}, [], []),

  ?assertEqual(Tags, oc_tag_ctx_header:decode(body(Response))).

%% ===================================================================
%% Private parts
%% ===================================================================

%%% Helpers

status({{_, Status, _}, _, _}) ->
  Status.
body({_, _, Body}) ->
  Body.

headers({_, Headers, _}) ->
  lists:sort(Headers).
