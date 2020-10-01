-module(banks_fetch_sup_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([should_returns_valid_specs/1, white_box_tests/1, black_box_tests/1]).

all() -> [should_returns_valid_specs, black_box_tests].

-define(DATABASE_PARAMS, {"banks_fetch_test","banks_fetch_user",""}).

init_per_testcase(black_box_tests, Config) ->
  meck:new(banks_fetch_client_sup),
  meck:new(banks_fetch_storage),
  meck:new(elli),
  Config;
init_per_testcase(_, Config) ->
  application:set_env(banks_fetch, storage, ?DATABASE_PARAMS),
  Config.

end_per_testcase(black_box_tests, _Config) ->
  meck:unload(banks_fetch_client_sup),
  meck:unload(banks_fetch_storage),
  meck:unload(elli),
  ok;
end_per_testcase(_, _Config) ->
  ok.

should_returns_valid_specs(_Config) ->
  ct:comment("Call supervisor init directly"),
  ExpectedSupervisorSpec = #{strategy => one_for_one, intensity => 1, period => 60},
  {ok, {ExpectedSupervisorSpec, ChildSpecs}} = banks_fetch_sup:init([]),

  ct:comment("Check child specs"),
  ok = supervisor:check_childspecs(ChildSpecs),

  ok.

white_box_tests(_Config) ->
  meck:expect(banks_fetch_storage, get_clients, fun() -> {value, []} end),

  ct:comment("Start supervisor"),
  {ok, SuperPid} = banks_fetch_sup:start_link(),

  receive after 100 -> nil end, % wait

  true = meck:validate(banks_fetch_storage),

  ct:comment("Stop supervisor"),
  exit(SuperPid, normal),

  receive after 200 -> nil end, % wait

  true = meck:validate(banks_fetch_storage),

  ok.

black_box_tests(_Config) ->
  process_flag(trap_exit, true),

  meck:expect(banks_fetch_client_sup, start_link, fun() ->
                                                      Pid = spawn_link(fun() -> timer:sleep(60*1000) end),
                                                      {ok, Pid}
                                                  end),
  meck:expect(banks_fetch_storage, start_link, fun(MockParams) ->
                                                   ?DATABASE_PARAMS = MockParams,
                                                   Pid = spawn_link(fun() -> timer:sleep(60*1000) end),
                                                   {ok, Pid}
                                               end),
  meck:expect(elli, start_link, fun(MockParams) ->
                                    [{callback, banks_fetch_api}, {port, _}] = MockParams,
                                    Pid = spawn_link(fun() -> timer:sleep(60*1000) end),
                                    {ok, Pid}
                                end),

  ct:comment("Start supervisor"),
  {ok, SupervisorPid} = banks_fetch_sup:start_link(),
  receive after 100 -> nil end, % wait

  ct:comment("Verify that there is a client supervisor and a storage worker"),
  ChildrenList1 = supervisor:which_children(banks_fetch_sup),
  3 = length(ChildrenList1),
  {banks_fetch_client_sup, ClientSupPid1, supervisor, [banks_fetch_client_sup]} = lists:keyfind(banks_fetch_client_sup, 1, ChildrenList1),
  {banks_fetch_storage, StoragePid1, worker, [banks_fetch_storage]} = lists:keyfind(banks_fetch_storage, 1, ChildrenList1),
  {banks_fetch_api, ApiPid1, worker, [elli, banks_fetch_api]} = lists:keyfind(banks_fetch_api, 1, ChildrenList1),

  % ------------------------------------------------
  ct:comment("Kill client supervisor ~p", [ClientSupPid1]),
  exit(ClientSupPid1, kill),
  receive after 100 -> nil end, % wait a little bit

  ct:comment("Verify that client supervisor is restarted"),
  ChildrenList2 = supervisor:which_children(banks_fetch_sup),
  3 = length(ChildrenList2),
  {banks_fetch_client_sup, ClientSupPid2, supervisor, [banks_fetch_client_sup]} = lists:keyfind(banks_fetch_client_sup, 1, ChildrenList2),
  {banks_fetch_storage, StoragePid1, worker, [banks_fetch_storage]} = lists:keyfind(banks_fetch_storage, 1, ChildrenList2),
  {banks_fetch_api, ApiPid1, worker, [elli, banks_fetch_api]} = lists:keyfind(banks_fetch_api, 1, ChildrenList1),

  false = erlang:is_process_alive(ClientSupPid1),
  true = erlang:is_process_alive(ClientSupPid2),
  true = erlang:is_process_alive(StoragePid1),
  true = erlang:is_process_alive(ApiPid1),

  ct:comment("Stop the supervisor"),
  exit(SupervisorPid, normal),

  receive after 200 -> nil end, % let stop

  ct:comment("Check supervisor and client supervisor and storage are not running"),
  false = erlang:is_process_alive(SupervisorPid),
  false = erlang:is_process_alive(ClientSupPid2),
  false = erlang:is_process_alive(StoragePid1),
  false = erlang:is_process_alive(ApiPid1),

  ok.
