-module(banks_fetch_client_server_sup_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([white_box_tests/1, black_box_tests/1]).

all() -> [white_box_tests, black_box_tests].

%%
%% Overall setup/teardwon
%%
init_per_suite(Config) ->
  ok = lager:start(),
  Config.

end_per_suite(_Config) ->
  application:stop(lager).

%%
%% Test cases
%%

init_per_testcase(black_box_tests, Config) ->
  meck:new(banks_fetch_bank_fakebank, [non_strict]), % this module does not exist
  meck:new(banks_fetch_storage),
  Config;
init_per_testcase(_, Config) ->
  Config.

end_per_testcase(black_box_tests, _Config) ->
  meck:unload(banks_fetch_storage),
  meck:unload(banks_fetch_bank_fakebank),
  ok;
end_per_testcase(_, _Config) ->
  ok.

white_box_tests(_Config) ->
  ct:log(sys_state, "white_box_tests: start~n"),

  % check start_link/0
  ct:log(sys_state, "white_box_tests: start supervisor~n"),
  {ok, SuperPid} = banks_fetch_client_server_sup:start_link(),

  receive after 100 -> nil end, % let start

  % clean up the instance we just started (as a 'side effect')
  ct:log(sys_state, "white_box_tests: stopping the supervisor~n"),
  exit(SuperPid, normal),

  receive after 200 -> nil end, % let stop

  % check init/1
  ExpectedSupervisorSpec = #{strategy => simple_one_for_one, intensity => 1, period => 60},

  ExpectedChildSpec = [
                       #{ id => banks_fetch_client_server,
                         start => {banks_fetch_client_server, start_link, []},
                         restart => temporary,
                         shutdown => 1,
                         type => worker,
                         modules => [banks_fetch_client_server]
                        }],

  ct:log(sys_state, "white_box_tests: call supervisor init directly (may often make no sense)~n"),
  {ok, {ExpectedSupervisorSpec, ExpectedChildSpec}} = banks_fetch_client_server_sup:init([]),

  % check child spec syntax
  ct:log(sys_state, "white_box_tests: check expected child specs~n"),
  ok = supervisor:check_childspecs(ExpectedChildSpec),

  ok.

-define(BANK_ID, {bank_id, <<"fakebank">>}).
-define(BANK_CLIENT_ID_A, {client_id, <<"12345">>}).
-define(BANK_CREDENTIAL_A, {client_credential, credential_a}).
-define(BANK_CLIENT_ID_B, {client_id, <<"67890">>}).
-define(BANK_CREDENTIAL_B, {client_credential, credential_b}).
-define(BANK_CLIENT_A_ACCOUNTS, [client_a_account_1]).
-define(BANK_CLIENT_B_ACCOUNTS, [client_b_account_1]).

black_box_tests(_Config) ->
  process_flag(trap_exit, true),

  ct:comment("Supervisor is not running yet"),
  undefined = whereis('ing_123456789'),

  ct:comment("Start supervisor"),
  {ok, SupervisorPid} = banks_fetch_client_server_sup:start_link(),
  receive after 100 -> nil end, % wait

  ct:comment("Verify that there is no children running"),
  [] = supervisor:which_children(banks_fetch_client_server_sup),

  % ------------------------------------------------
  ct:comment("Start 2 client servers from supervisor"),
  meck:expect(banks_fetch_bank_fakebank, connect, fun(MockClientId, MockCredential) ->
                                                      case MockClientId of
                                                        ?BANK_CLIENT_ID_A -> 
                                                          ?BANK_CREDENTIAL_A = MockCredential,
                                                          {ok, fake_auth_a};
                                                        ?BANK_CLIENT_ID_B ->
                                                          ?BANK_CREDENTIAL_B = MockCredential,
                                                          {ok, fake_auth_b}
                                                      end
                                                  end),
  meck:expect(banks_fetch_bank_fakebank, fetch_accounts, fun(MockAuth) ->
                                                      case MockAuth of
                                                        fake_auth_a -> {ok, ?BANK_CLIENT_A_ACCOUNTS};
                                                        fake_auth_b -> {ok, ?BANK_CLIENT_B_ACCOUNTS}
                                                      end
                                                  end),
  meck:expect(banks_fetch_storage, store_accounts, fun(MockBankName, MockClientId, _MockFetchingAt, MockAccounts) ->
                                                       case MockClientId of
                                                         ?BANK_CLIENT_ID_A ->
                                                           ?BANK_ID = MockBankName,
                                                           ?BANK_CLIENT_A_ACCOUNTS = MockAccounts;
                                                         ?BANK_CLIENT_ID_B ->
                                                           ?BANK_ID = MockBankName,
                                                           ?BANK_CLIENT_B_ACCOUNTS = MockAccounts
                                                       end
                                                   end),
  {ok, ClientPidA1} = banks_fetch_client_server_sup:start_child(?BANK_ID, ?BANK_CLIENT_ID_A, ?BANK_CREDENTIAL_A),
  {ok, ClientPidB1} = banks_fetch_client_server_sup:start_child(?BANK_ID, ?BANK_CLIENT_ID_B, ?BANK_CREDENTIAL_B),

  ct:comment("Verify that there are two running children : ~p and ~p", [ClientPidA1, ClientPidB1]),
  [{undefined, ClientPidA1, worker, _},{undefined,ClientPidB1,worker,_}] = supervisor:which_children(banks_fetch_client_server_sup),

  ct:comment("Verify that client_server A is working"),
  ?BANK_CLIENT_A_ACCOUNTS = banks_fetch_client_server:accounts(ClientPidA1),

  ct:comment("Verify that client_server B is working"),
  ?BANK_CLIENT_B_ACCOUNTS = banks_fetch_client_server:accounts(ClientPidB1),

  % ------------------------------------------------
  ct:comment("Kill client server A ~p", [ClientPidA1]),
  exit(ClientPidA1, internal_error),
  receive after 100 -> nil end, % wait a little bit to ensure that client is not restarted

  ct:comment("Verify that client server A is not restarted and client B still alived"),
  [{undefined, ClientPidB1, worker, _}] = supervisor:which_children(banks_fetch_client_server_sup),
  false = erlang:is_process_alive(ClientPidA1),
  true = erlang:is_process_alive(ClientPidB1),

  ct:comment("Kill client server B ~p", [ClientPidB1]),
  exit(ClientPidB1, internal_error),

  ct:comment("Verify that supervisor is still running"),
  receive
    {'EXIT',SupervisorPid,shutdown} ->
      undefined = whereis(banks_fetch_client_server_sup),
      ct:fail("Exit message received, supervisor no more alived");
    Message ->
      ct:fail("Unexpected message received : ~p", [Message])
  after 1000 ->
      ct:comment("Supervisor still alived"),
      SupervisorPid = whereis(banks_fetch_client_server_sup)
  end,

  ct:comment("Verify that all clients servers are not alived"),
  false = erlang:is_process_alive(ClientPidA1),
  false = erlang:is_process_alive(ClientPidB1),

  ok.
