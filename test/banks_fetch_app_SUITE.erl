-module(banks_fetch_app_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([should_launch_app_supervisor/1]).

all() -> [should_launch_app_supervisor].

init_per_testcase(should_launch_app_supervisor, Config) ->
  % Prometheus may be running on default port (8081), choose next to prevent eaddrinuse
  ok = application:set_env(prometheus, prometheus_http, [{path, "/metrics"}, {format, auto}, {port, 8082}]),
  application:start(prometheus),
  meck:new(banks_fetch_sup),
  Config.

end_per_testcase(should_launch_app_supervisor, _Config) ->
  application:stop(prometheus),
  meck:unload(banks_fetch_sup).

should_launch_app_supervisor(_Config) ->
  meck:expect(banks_fetch_sup, start_link, fun() -> {ok, fake_pid} end),

  ct:comment("Start app"),
  {ok, fake_pid} = banks_fetch_app:start(start, []),

  ct:comment("Verify that app supervisor is started"),
  true = meck:validate(banks_fetch_sup),

  ct:comment("Stop app"),
  ok = banks_fetch_app:stop(stop),

  true = meck:validate(banks_fetch_sup),

  ok.
