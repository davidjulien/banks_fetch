-module(banks_fetch_client_sup_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([should_returns_valid_specs/1, white_box_tests/1, black_box_tests/1]).

all() -> [should_returns_valid_specs, black_box_tests].

init_per_testcase(black_box_tests, Config) ->
  meck:new(banks_fetch_client_server_sup),
  meck:new(banks_fetch_client_manager),
  Config;
init_per_testcase(_, Config) ->
  Config.

end_per_testcase(black_box_tests, _Config) ->
  meck:unload(banks_fetch_client_manager),
  meck:unload(banks_fetch_client_server_sup),
  ok;
end_per_testcase(_, _Config) ->
  ok.

should_returns_valid_specs(_Config) ->
  ExpectedSupervisorSpec = #{strategy => one_for_all, intensity => 1, period => 60},

  ExpectedChildSpec = [
                       #{ id => banks_fetch_client_manager,
                         start => {banks_fetch_client_manager, start_link, []},
                         restart => permanent,
                         shutdown => 1,
                         type => worker,
                         modules => [banks_fetch_client_manager]
                        },
                       #{ id => banks_fetch_client_server_sup,
                         start => {banks_fetch_client_server_sup, start_link, []},
                         restart => permanent,
                         shutdown => 1,
                         type => supervisor,
                         modules => [banks_fetch_client_server_sup]
                        }
                      ],

  ct:comment("Call supervisor init directly"),
  {ok, {ExpectedSupervisorSpec, ExpectedChildSpec}} = banks_fetch_client_sup:init([]),

  ct:log("Check child specs"),
  ok = supervisor:check_childspecs(ExpectedChildSpec),

  ok.


white_box_tests(_Config) ->
  % check start_link/0
  ct:log("Start supervisor"),
  {ok, SuperPid} = banks_fetch_client_sup:start_link(),

  receive after 100 -> nil end, % let start

  % clean up the instance we just started (as a 'side effect')
  ct:log("Stop supervisor~n"),
  exit(SuperPid, normal),

  receive after 200 -> nil end, % let stop

  ok.


black_box_tests(_Config) ->
  process_flag(trap_exit, true),

  meck:expect(banks_fetch_client_manager, start_link, fun() ->
                                                          Pid = spawn_link(fun() -> timer:sleep(60*1000) end),
                                                          {ok, Pid}
                                                      end),
  meck:expect(banks_fetch_client_server_sup, start_link, fun() ->
                                                             Pid = spawn_link(fun() -> timer:sleep(60*1000) end),
                                                             {ok, Pid}
                                                         end),

  ct:comment("Start supervisor"),
  {ok, SupervisorPid} = banks_fetch_client_sup:start_link(),
  receive after 100 -> nil end, % wait

  ct:comment("Verify that there are two children running"),
  ChildrenList1 = supervisor:which_children(banks_fetch_client_sup),
  2 = length(ChildrenList1),
  {_,ClientServerSupPid1, supervisor, [banks_fetch_client_server_sup]} = lists:keyfind(banks_fetch_client_server_sup, 1, ChildrenList1),
  {_,ClientManagerPid1, worker, [banks_fetch_client_manager]} = lists:keyfind(banks_fetch_client_manager, 1, ChildrenList1),

  true = meck:validate(banks_fetch_client_manager),
  true = meck:validate(banks_fetch_client_server_sup),

  % ------------------------------------------------
  ct:comment("Kill client manager ~p", [ClientManagerPid1]),
  exit(ClientManagerPid1, kill),
  receive after 100 -> nil end, % wait a little bit

  ct:comment("Verify that client manager is restarted and client server sup as well"),
  ChildrenList2 = supervisor:which_children(banks_fetch_client_sup),
  2 = length(ChildrenList2),
  {_,ClientServerSupPid2, supervisor, [banks_fetch_client_server_sup]} = lists:keyfind(banks_fetch_client_server_sup, 1, ChildrenList2),
  {_,ClientManagerPid2, worker, [banks_fetch_client_manager]} = lists:keyfind(banks_fetch_client_manager, 1, ChildrenList2),

  false = erlang:is_process_alive(ClientManagerPid1),
  false = erlang:is_process_alive(ClientServerSupPid1),
  true = erlang:is_process_alive(ClientManagerPid2),
  true = erlang:is_process_alive(ClientServerSupPid2),

  ct:comment("Kill client server sup ~p", [ClientServerSupPid2]),
  exit(ClientServerSupPid2, kill),

  ct:comment("Verify that supervisor is shutdown because of the strategy set in supervisor"),
  receive
    {'EXIT',SupervisorPid,shutdown} ->
      undefined = whereis(banks_fetch_client_sup),
      undefined = whereis(banks_fetch_client_manager);
    Message ->
      ct:fail("Unexpected message received : ~p", [Message])
  after 1000 ->
          ct:fail("No exit message received, supervisor still alived ?")
  end,

  ct:comment("Verify that all processes are not alived"),
  false = erlang:is_process_alive(SupervisorPid),
  false = erlang:is_process_alive(ClientManagerPid2),
  false = erlang:is_process_alive(ClientServerSupPid2),

  ct:comment("Verify calls to start processes"),
  2 = meck:num_calls(banks_fetch_client_manager, start_link, '_'),
  2 = meck:num_calls(banks_fetch_client_server_sup, start_link, '_'),

  true = meck:validate(banks_fetch_client_manager),
  true = meck:validate(banks_fetch_client_server_sup),

  ok.
