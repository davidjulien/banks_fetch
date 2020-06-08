-module(banks_fetch_sup_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([white_box_tests/1, black_box_tests/1]).

all() -> [white_box_tests, black_box_tests].

init_per_testcase(_, Config) ->
  Config.

end_per_testcase(_, _Config) ->
  ok.

white_box_tests(_Config) ->
  ct:comment("Start supervisor"),
  {ok, SuperPid} = banks_fetch_sup:start_link(),

  receive after 100 -> nil end, % wait

  ct:comment("Stop supervisor"),
  exit(SuperPid, normal),

  receive after 200 -> nil end, % wait

  ct:comment("Call supervisor init directly"),
  ExpectedSupervisorSpec = #{strategy => one_for_all, intensity => 0, period => 1},
  ExpectedChildSpec = [],
  {ok, {ExpectedSupervisorSpec, ExpectedChildSpec}} = banks_fetch_sup:init([]),

  ct:comment("Check check expected child specs"),
  ok = supervisor:check_childspecs(ExpectedChildSpec),

  ok.

black_box_tests(_Config) ->
  process_flag(trap_exit, true),

  ct:comment("Start supervisor"),
  {ok, SupervisorPid} = banks_fetch_sup:start_link(),
  receive after 100 -> nil end, % wait

  ct:comment("Verify that there is no children running"),
  [] = supervisor:which_children(banks_fetch_sup),

  ct:comment("Stop the supervisor"),
  exit(SupervisorPid, normal),

  receive after 200 -> nil end, % let stop

  ct:comment("Check supervisor is not running"),
  false = erlang:is_process_alive(SupervisorPid),
  ok.
