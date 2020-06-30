-module(banks_fetch_sup_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([white_box_tests/1, black_box_tests/1]).

all() -> [white_box_tests, black_box_tests].

init_per_testcase(black_box_tests, Config) ->
  meck:new(banks_fetch_client_sup),
  Config;
init_per_testcase(_, Config) ->
  Config.

end_per_testcase(black_box_tests, Config) ->
  meck:unload(banks_fetch_client_sup),
  Config;
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
  ExpectedSupervisorSpec = #{strategy => one_for_one, intensity => 1, period => 60},
  ExpectedChildSpec = [#{
      id => banks_fetch_client_sup,
      start => {banks_fetch_client_sup, start_link, []},
      restart => permanent,
      shutdown => 1,
      type => supervisor,
      modules => [banks_fetch_client_sup]
     }],
  {ok, {ExpectedSupervisorSpec, ExpectedChildSpec}} = banks_fetch_sup:init([]),

  ct:comment("Check check expected child specs"),
  ok = supervisor:check_childspecs(ExpectedChildSpec),

  ok.

black_box_tests(_Config) ->
  process_flag(trap_exit, true),

  meck:expect(banks_fetch_client_sup, start_link, fun() ->
                                                      Pid = spawn_link(fun() -> timer:sleep(60*1000) end),
                                                      {ok, Pid}
                                                  end),

  ct:comment("Start supervisor"),
  {ok, SupervisorPid} = banks_fetch_sup:start_link(),
  receive after 100 -> nil end, % wait

  ct:comment("Verify that there is a client supervisor"),
  [{banks_fetch_client_sup, ClientSupPid1, supervisor, [banks_fetch_client_sup]}] = supervisor:which_children(banks_fetch_sup),

  % ------------------------------------------------
  ct:comment("Kill client manager ~p", [ClientSupPid1]),
  exit(ClientSupPid1, kill),
  receive after 100 -> nil end, % wait a little bit

  ct:comment("Verify that client sup is restarted"),
  [{banks_fetch_client_sup, ClientSupPid2, supervisor, [banks_fetch_client_sup]}] = supervisor:which_children(banks_fetch_sup),

  false = erlang:is_process_alive(ClientSupPid1),
  true = erlang:is_process_alive(ClientSupPid2),

  ct:comment("Stop the supervisor"),
  exit(SupervisorPid, normal),

  receive after 200 -> nil end, % let stop

  ct:comment("Check supervisor and client supervisor are not running"),
  false = erlang:is_process_alive(SupervisorPid),
  false = erlang:is_process_alive(ClientSupPid2),

  ok.
