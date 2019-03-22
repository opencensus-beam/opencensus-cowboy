%% @doc
%% Collects Cowboy metrics using
%% <a href="https://github.com/ninenines/cowboy/blob/master/src/cowboy_metrics_h.erl">
%%   metrics stream handler
%% </a>.
%%
%% ==Exported metrics==
%% <ul>
%%   <li>
%%     `cowboy_early_errors_total'<br/>
%%     Type: counter.<br/>
%%     Tags: default - `[]', configured via `early_errors_tags'.<br/>
%%     Total number of Cowboy early errors, i.e. errors that occur before a request is received.
%%   </li>
%%   <li>
%%     `cowboy_protocol_upgrades_total'<br/>
%%     Type: counter.<br/>
%%     Tags: default - `[]', configured via `protocol_upgrades_tags'.<br/>
%%     Total number of protocol upgrades, i.e. when http connection
%%     upgraded to websocket connection.
%%   </li>
%%   <li>
%%     `cowboy_requests_total'<br/>
%%     Type: counter.<br/>
%%     Tags: default - `[method, reason, status_class]', configured via `request_tags'.<br/>
%%     Total number of Cowboy requests.
%%   </li>
%%   <li>
%%     `cowboy_spawned_processes_total'<br/>
%%     Type: counter.<br/>
%%     Tags: default - `[method, reason, status_class]', configured via `request_tags'.<br/>
%%     Total number of spawned processes.
%%   </li>
%%   <li>
%%     `cowboy_errors_total'<br/>
%%     Type: counter.<br/>
%%     Tags: default - `[method, reason, error]', configured via `error_tags'.<br/>
%%     Total number of Cowboy request errors.
%%   </li>
%%   <li>
%%     `cowboy_request_duration_seconds'<br/>
%%     Type: histogram.<br/>
%%     Tags: default - `[method, reason, status_class]', configured via `request_tags'.<br/>
%%     Buckets: default - `[0.01, 0.1, 0.25, 0.5, 0.75, 1, 1.5, 2, 4]',
%%     configured via `duration_buckets'.<br/>
%%     Cowboy request duration.
%%   </li>
%%   <li>
%%     `cowboy_receive_body_duration_seconds'<br/>
%%     Type: histogram.<br/>
%%     Tags: default - `[method, reason, status_class]', configured via `request_tags'.<br/>
%%     Buckets: default - `[0.01, 0.1, 0.25, 0.5, 0.75, 1, 1.5, 2, 4]',
%%     configured via `duration_buckets'.<br/>
%%     Request body receiving duration.
%%   </li>
%% </ul>
%%
%% ==Configuration==
%%
%% Prometheus Cowboy2 instrumenter configured via `cowboy_instrumenter' key of `prometheus'
%% app environment.
%%
%% Default configuration:
%%
%% <pre lang="erlang">
%% {prometheus, [
%%   ...
%%   {cowboy_instrumenter, [{duration_buckets, [0.01, 0.1, 0.25, 0.5, 0.75, 1, 1.5, 2, 4]},
%%                          {early_error_tags,  []},
%%                          {request_tags, [method, reason, status_class]},
%%                          {error_tags, [method, reason, error]},
%%                          {registry, default}]}
%%   ...
%% ]}
%% </pre>
%%
%% ==Tags==
%%
%% Builtin:
%%  - host,
%%  - port,
%%  - method,
%%  - status,
%%  - status_class,
%%  - reason,
%%  - error.
%%
%% ===Custom tags===
%% can be implemented via module exporting tag_value/2 function.
%% First argument will be tag name, second is Metrics data from
%% <a href="https://github.com/ninenines/cowboy/blob/master/src/cowboy_metrics_h.erl">
%% metrics stream handler
%% </a>.
%% Set this module to `tags_module' configuration option.
%%
%% @end
-module(opencensus_cowboy2_instrumenter).

-export([setup/0]).
-export([observe/1]).

%% ===================================================================
%% API
%% ===================================================================

-spec observe(map()) -> ok.
%% @doc
%% <a href="https://github.com/ninenines/cowboy/blob/master/src/cowboy_metrics_h.erl">
%% Metrics stream handler
%% </a> callback.
%% @end
observe(Metrics=#{ref:=ListenerRef}) ->
  {Host, Port} = ranch:get_addr(ListenerRef),
  dispatch_metrics(Metrics#{listener_host=>Host,
                            listener_port=>Port}),
  ok.

%% @doc
%% Sets all metrics up. Call this when the app starts.
%% @end
setup() ->
  oc_stat_measure:new(cowboy_early_errors, "Counts Cowboy early errors.", 1),
  oc_stat_measure:new(cowboy_protocol_upgrades, "Counts protocol upgrades.", 1),
  oc_stat_measure:new(cowboy_request_duration, "Cowboy request duration.", native_time_unit),
  oc_stat_measure:new(cowboy_spawned_processes, "Counts spawned processes.", 1),
  oc_stat_measure:new(cowboy_errors, "Counts request errors.", 1),
  oc_stat_measure:new(cowboy_receive_body_duration, "Time needed to receive full body.",
                      native_time_unit),

  ok.

%% ===================================================================
%% Private functions
%% ===================================================================

dispatch_metrics(#{early_time_error := _}=Metrics) ->
  oc_stat:record(tags(Metrics), cowboy_early_errors, 1);
dispatch_metrics(#{reason := switch_protocol}=Metrics) ->
  oc_stat:record(tags(Metrics), cowboy_protocol_upgrades, 1);
dispatch_metrics(#{req_start := ReqStart,
                   req_end := ReqEnd,
                   req_body_start := ReqBodyStart,
                   req_body_end := ReqBodyEnd,
                   reason := Reason,
                   procs := Procs}=Metrics) ->
  RequestTags = tags(Metrics),
  oc_stat:record(RequestTags, cowboy_spawned_processes_total, maps:size(Procs)),
  oc_stat:record(RequestTags, cowboy_request_duration, ReqEnd - ReqStart),
  case ReqBodyEnd of
    undefined -> ok;
    _ -> oc_stat:record(RequestTags, cowboy_receive_body_duration,
                        ReqBodyEnd - ReqBodyStart)
  end,

  case Reason of
    normal ->
      ok;
    switch_protocol ->
      ok;
    stop ->
      ok;
    _ ->
      oc_stat:record(tags(Metrics), cowboy_errors_total, 1)
  end.

%% tags

tags(Metrics) ->
  Tags = #{
           host => tag_host(Metrics),
           port => tag_port(Metrics),
           method => tag_method(Metrics),
           status => tag_status(Metrics),
           status_class => tag_status_class(Metrics),
           reason => tag_reason(Metrics),
           error => tag_error(Metrics)
          },

  maps:merge(oc_tags:to_map(ocp:current_tags()), Tags).

tag_host(#{listener_host:=Host}) ->
  Host.
tag_port(#{listener_port:=Port}) ->
  Port.
tag_method(#{req:=Req}) ->
  cowboy_req:method(Req).

tag_status(#{resp_status:=Status}) ->
  Status.

tag_status_class(#{resp_status:=undefined}) ->
  undefined;
tag_status_class(#{resp_status:=Status}) ->
  status_class(Status).

tag_reason(#{reason:=Reason}) ->
  case Reason of
    _ when is_atom(Reason) -> Reason;
    {ReasonAtom, _} -> ReasonAtom;
    {ReasonAtom, _, _} -> ReasonAtom
  end.

tag_error(#{reason:=Reason}) ->
  case Reason of
    _ when is_atom(Reason) -> undefined;
    {_, {Error, _}, _} -> Error;
    {_, Error, _} when is_atom(Error) -> Error;
    _ -> undefined
  end.

%% @doc
%% Returns status class for the http status code `SCode'.
%%
%% Raises `{invalid_value_error, SCode, Message}' error if `SCode'
%% isn't a positive integer.
%% @end
status_class(SCode) when is_integer(SCode)
                         andalso SCode > 0
                         andalso SCode < 100 ->
  "unknown";
status_class(SCode) when is_integer(SCode)
                         andalso SCode > 0
                         andalso SCode < 200 ->
  "informational";
status_class(SCode) when is_integer(SCode)
                         andalso SCode > 0
                         andalso SCode < 300 ->
  "success";
status_class(SCode) when is_integer(SCode)
                         andalso SCode > 0
                         andalso SCode < 400 ->
  "redirection";
status_class(SCode) when is_integer(SCode)
                         andalso SCode > 0
                         andalso SCode < 500 ->
  "client-error";
status_class(SCode) when is_integer(SCode)
                         andalso SCode > 0
                         andalso SCode < 600 ->
  "server-error";
status_class(SCode) when is_integer(SCode)
                         andalso SCode > 0
                         andalso SCode >= 600 ->
  "unknown";
status_class(C) ->
  erlang:error({invalid_value, C, "status code must be a positive integer"}).
