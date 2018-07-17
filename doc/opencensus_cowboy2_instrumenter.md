

# Module opencensus_cowboy2_instrumenter #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Collects Cowboy metrics using
[
metrics stream handler
](https://github.com/ninenines/cowboy/blob/master/src/cowboy_metrics_h.erl).

<a name="description"></a>

## Description ##


### <a name="Exported_metrics">Exported metrics</a> ###


* `cowboy_early_errors_total`<br />
Type: counter.<br />
Tags: default - `[]`, configured via `early_errors_tags`.<br />
Total number of Cowboy early errors, i.e. errors that occur before a request is received.

* `cowboy_protocol_upgrades_total`<br />
Type: counter.<br />
Tags: default - `[]`, configured via `protocol_upgrades_tags`.<br />
Total number of protocol upgrades, i.e. when http connection upgraded to websocket connection.

* `cowboy_requests_total`<br />
Type: counter.<br />
Tags: default - `[method, reason, status_class]`, configured via `request_tags`.<br />
Total number of Cowboy requests.

* `cowboy_spawned_processes_total`<br />
Type: counter.<br />
Tags: default - `[method, reason, status_class]`, configured via `request_tags`.<br />
Total number of spawned processes.

* `cowboy_errors_total`<br />
Type: counter.<br />
Tags: default - `[method, reason, error]`, configured via `error_tags`.<br />
Total number of Cowboy request errors.

* `cowboy_request_duration_seconds`<br />
Type: histogram.<br />
Tags: default - `[method, reason, status_class]`, configured via `request_tags`.<br />
Buckets: default - `[0.01, 0.1, 0.25, 0.5, 0.75, 1, 1.5, 2, 4]`, configured via `duration_buckets`.<br />
Cowboy request duration.

* `cowboy_receive_body_duration_seconds`<br />
Type: histogram.<br />
Tags: default - `[method, reason, status_class]`, configured via `request_tags`.<br />
Buckets: default - `[0.01, 0.1, 0.25, 0.5, 0.75, 1, 1.5, 2, 4]`, configured via `duration_buckets`.<br />
Request body receiving duration.



### <a name="Configuration">Configuration</a> ###

Prometheus Cowboy2 instrumenter configured via `cowboy_instrumenter` key of `prometheus`
app environment.

Default configuration:

```erlang

  {prometheus, [
    ...
    {cowboy_instrumenter, [{duration_buckets, [0.01, 0.1, 0.25, 0.5, 0.75, 1, 1.5, 2, 4]},
                           {early_error_tags,  []},
                           {request_tags, [method, reason, status_class]},
                           {error_tags, [method, reason, error]},
                           {registry, default}]}
    ...
  ]}
```


### <a name="Tags">Tags</a> ###

Builtin:
- host,
- port,
- method,
- status,
- status_class,
- reason,
- error.


#### <a name="Custom_tags">Custom tags</a> ####

can be implemented via module exporting tag_value/2 function.
First argument will be tag name, second is Metrics data from
[
metrics stream handler
](https://github.com/ninenines/cowboy/blob/master/src/cowboy_metrics_h.erl).
Set this module to `tags_module` configuration option.
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#observe-1">observe/1</a></td><td>
<a href="https://github.com/ninenines/cowboy/blob/master/src/cowboy_metrics_h.erl">
Metrics stream handler
</a> callback.</td></tr><tr><td valign="top"><a href="#setup-0">setup/0</a></td><td>
Sets all metrics up.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="observe-1"></a>

### observe/1 ###

<pre><code>
observe(Metrics::map()) -&gt; ok
</code></pre>
<br />

[
Metrics stream handler
](https://github.com/ninenines/cowboy/blob/master/src/cowboy_metrics_h.erl) callback.

<a name="setup-0"></a>

### setup/0 ###

`setup() -> any()`

Sets all metrics up. Call this when the app starts.

