%%%-------------------------------------------------------------------
%% @doc banks_fetch public API
%% @end
%%%-------------------------------------------------------------------

-module(banks_fetch_app).

-behaviour(application).

-export([start/2, stop/1]).

-spec start(normal | {takeover, node()} | {failover, node()}, any()) -> {ok, pid()} | {ok, pid(), any()} | {error, any()}.
start(_StartType, _StartArgs) ->
  {ok, _} = prometheus_httpd:start(),
  banks_fetch_http:setup(),
  banks_fetch_bank:setup(),

  banks_fetch_sup:start_link().

-spec stop(any()) -> ok.
stop(_State) ->
    ok.

%% internal functions
