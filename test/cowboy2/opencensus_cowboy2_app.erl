%% @private
-module(opencensus_cowboy2_app).

%% API.
-export([start/0]).
%% API.

start() ->
  Routes = [
            {'_', [
                   {"/spans", spans_handler, []},
                   {"/tags", tags_handler, []}
                  ]}
           ],
  Dispatch = cowboy_router:compile(Routes),
  {ok, _} = cowboy:start_clear(http, [{port, 0}],
                               #{env => #{dispatch => Dispatch},
                                 middlewares => [opencensus_cowboy2_context, cowboy_router, cowboy_handler]}),
  {ranch:get_port(http), http}.
