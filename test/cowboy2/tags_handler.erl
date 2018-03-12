-module(tags_handler).

%% -behaviour(cowboy_handler).

-export([
         init/2,
         terminate/3
        ]).

%% ===================================================================
%% cowboy_handler callbacks
%% ===================================================================

init(Req, _Opts) ->
  Body = oc_tag_ctx_header:encode(ocp:current_tags()),
  {ok, cowboy_req:reply(200, #{}, Body, Req), undefined}.

terminate(_Reason, _Req, _State) ->
  ok.
