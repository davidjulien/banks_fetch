-module(banks_fetch_client_manager_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2 ]).
-export([
         should_handle_cast_do_nothing/1,
         should_launch_all_client_servers_on_init/1,
         should_launch_new_client_server_when_a_new_client_is_added/1,
         should_not_launch_new_client_server_when_an_existing_client_is_added/1
        ]).

-define(BANK_ID_1, {bank_id, <<"bank_id_1">>}).
-define(CLIENT_ID_1, {client_id, <<"client_id_1">>}).
-define(CLIENT_CREDENTIAL_1, {client_credential, client_credential_1}).
-define(BANK_CLIENTS,
        [
         {?BANK_ID_1, ?CLIENT_ID_1, ?CLIENT_CREDENTIAL_1},
         {{bank_id, <<"bank_id_1">>}, {client_id, <<"client_id_2">>}, {client_credential, client_credential_2}},
         {{bank_id, <<"bank_id_2">>}, {client_id, <<"client_id_3">>}, {client_credential, client_credential_3}}
        ]).

all() -> [
          should_handle_cast_do_nothing,
          should_launch_all_client_servers_on_init,
          should_launch_new_client_server_when_a_new_client_is_added,
          should_not_launch_new_client_server_when_an_existing_client_is_added
         ].

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

init_per_testcase(should_handle_cast_do_nothing, Config) ->
  Config;
init_per_testcase(should_launch_all_client_servers_on_init, Config) ->
  meck:new(banks_fetch_storage),
  meck:new(banks_fetch_client_server_sup),
  Config;
init_per_testcase(should_launch_new_client_server_when_a_new_client_is_added, Config) ->
  meck:new(banks_fetch_storage),
  meck:new(banks_fetch_client_server_sup),
  Config;
init_per_testcase(should_not_launch_new_client_server_when_an_existing_client_is_added, Config) ->
  meck:new(banks_fetch_storage),
  meck:new(banks_fetch_client_server_sup),
  Config.

end_per_testcase(should_handle_cast_do_nothing, _Config) ->
  ok;
end_per_testcase(should_launch_all_client_servers_on_init, _Config) ->
  meck:unload(banks_fetch_client_server_sup),
  meck:unload(banks_fetch_storage),
  ok;
end_per_testcase(should_launch_new_client_server_when_a_new_client_is_added, _Config) ->
  meck:unload(banks_fetch_client_server_sup),
  meck:unload(banks_fetch_storage),
  ok;
end_per_testcase(should_not_launch_new_client_server_when_an_existing_client_is_added, _Config) ->
  meck:unload(banks_fetch_client_server_sup),
  meck:unload(banks_fetch_storage),
  ok.

should_handle_cast_do_nothing(_Config) ->
  {noreply, dummystate} = banks_fetch_client_manager:handle_cast(dummycall, dummystate),
  ok.


%% this function spawns a fake client server
spawn_fake_client() ->
  spawn(fun() ->
            receive
            after 1000 ->
                    ok
            end
        end).


should_launch_all_client_servers_on_init(_Config) ->
  ct:comment("Expect calls to storage to get all clients and start_child calls for every client"),
  meck:expect(banks_fetch_storage, get_clients, fun() -> {value, ?BANK_CLIENTS} end),

  ClientsPids = lists:zip([ [BankId, ClientId, ClientCredential] || {BankId, ClientId, ClientCredential} <- ?BANK_CLIENTS], [ {ok, spawn_fake_client()} || _N <- lists:seq(1,length(?BANK_CLIENTS)) ]),
  meck:expect(banks_fetch_client_server_sup, start_child, ClientsPids),

  ct:comment("Start clients manager"),
  {ok, ClientsManagerPID} = banks_fetch_client_manager:start_link(),

  ct:comment("Wait get_banks_clients"),
  meck:wait(banks_fetch_storage, get_clients, '_', 1000),

  ct:comment("Verify that we have fetched clients from storage"),
  true = meck:validate(banks_fetch_storage),

  ct:comment("Verify clients pid"),
  ClientsPidsManager = lists:sort(banks_fetch_client_manager:get_clients_pids()),
  ExpectedClientsPidsManager = [ {BankId,ClientId,Pid} || {[BankId,ClientId,_ClientCredential],{ok, Pid}} <- ClientsPids ],
  ExpectedClientsPidsManager = ClientsPidsManager,

  ct:comment("Verify that we have started all required clients"),
  NbrBankClients = length(?BANK_CLIENTS),
  NbrBankClients = meck:num_calls(banks_fetch_client_server_sup, start_child, '_'),
  true = meck:validate(banks_fetch_client_server_sup),

  ct:comment("Kill first client"),
  [{_,{ok,PidA}}|_] = ClientsPids,
  exit(PidA, killed),

  ct:comment("Verify that this client has been removed from manager"),
  ClientsPidsManager2 = lists:sort(banks_fetch_client_manager:get_clients_pids()),
  ExpectedClientsPidsManager2 = tl(ExpectedClientsPidsManager),
  ExpectedClientsPidsManager2 = ClientsPidsManager2,

  ct:comment("Exit manager"),
  exit(ClientsManagerPID, normal),

  ok.


should_launch_new_client_server_when_a_new_client_is_added(_Config) ->
  ct:comment("Expect calls to storage to get all clients and start_child calls for every client"),
  meck:expect(banks_fetch_storage, get_clients, fun() -> {value, []} end),

  ct:comment("Start clients manager"),
  {ok, ClientsManagerPID} = banks_fetch_client_manager:start_link(),

  ct:comment("Wait get_banks_clients"),
  meck:wait(banks_fetch_storage, get_clients, '_', 1000),

  ct:comment("Verify that we have fetched clients from storage"),
  true = meck:validate(banks_fetch_storage),

  ct:comment("Verify that we have not started client_servers"),
  true = meck:validate(banks_fetch_client_server_sup),

  ct:comment("Verify clients pid"),
  [] = lists:sort(banks_fetch_client_manager:get_clients_pids()),


  ClientPid = spawn_fake_client(),
  meck:expect(banks_fetch_storage, insert_client, fun(MockBankId, MockClientId, MockClientCredential) ->
                                                      ?BANK_ID_1 = MockBankId,
                                                      ?CLIENT_ID_1 = MockClientId,
                                                      ?CLIENT_CREDENTIAL_1 = MockClientCredential,
                                                      ok
                                                  end),

  meck:expect(banks_fetch_client_server_sup, start_child, fun(MockBankId, MockClientId, MockClientCredential) ->
                                                              ?BANK_ID_1 = MockBankId,
                                                              ?CLIENT_ID_1 = MockClientId,
                                                              ?CLIENT_CREDENTIAL_1 = MockClientCredential,
                                                              {ok, ClientPid}
                                                          end),

  ct:comment("Add new client"),
  ok = banks_fetch_client_manager:add_client(?BANK_ID_1, ?CLIENT_ID_1, ?CLIENT_CREDENTIAL_1),

  ct:comment("Verify that we have started all required clients"),
  1 = meck:num_calls(banks_fetch_client_server_sup, start_child, '_'),
  true = meck:validate(banks_fetch_client_server_sup),

  true = meck:validate(banks_fetch_storage),

  ct:comment("Verify clients pid"),
  [{?BANK_ID_1, ?CLIENT_ID_1, ClientPid}] = lists:sort(banks_fetch_client_manager:get_clients_pids()),

  ct:comment("Exit clients manager"),
  exit(ClientsManagerPID, normal),

  ok.


should_not_launch_new_client_server_when_an_existing_client_is_added(_Config) ->
  ct:comment("Expect calls to storage to get all clients and start_child calls for every client"),
  meck:expect(banks_fetch_storage, get_clients, fun() -> {value, [{?BANK_ID_1, ?CLIENT_ID_1, ?CLIENT_CREDENTIAL_1}]} end),

  ClientPid = spawn_fake_client(),
  meck:expect(banks_fetch_client_server_sup, start_child, fun(MockBankId, MockClientId, MockClientCredential) ->
                                                              ?BANK_ID_1 = MockBankId,
                                                              ?CLIENT_ID_1 = MockClientId,
                                                              ?CLIENT_CREDENTIAL_1 = MockClientCredential,
                                                              {ok, ClientPid}
                                                          end),

  ct:comment("Start clients manager"),
  {ok, ClientsManagerPID} = banks_fetch_client_manager:start_link(),

  ct:comment("Wait get_banks_clients"),
  meck:wait(banks_fetch_storage, get_clients, '_', 1000),

  ct:comment("Verify that we have fetched clients from storage"),
  true = meck:validate(banks_fetch_storage),

  ct:comment("Verify that we have not started client_servers"),
  true = meck:validate(banks_fetch_client_server_sup),

  ct:comment("Verify clients pid"),
  [{?BANK_ID_1, ?CLIENT_ID_1, ClientPid}] = lists:sort(banks_fetch_client_manager:get_clients_pids()),

  meck:reset(banks_fetch_client_server_sup),
  meck:expect(banks_fetch_storage, insert_client, fun(MockBankId, MockClientId, MockClientCredential) ->
                                                      ?BANK_ID_1 = MockBankId,
                                                      ?CLIENT_ID_1 = MockClientId,
                                                      ?CLIENT_CREDENTIAL_1 = MockClientCredential,
                                                      {error, already_inserted}
                                                  end),

  ct:comment("Add new client"),
  {error, already_defined} = banks_fetch_client_manager:add_client(?BANK_ID_1, ?CLIENT_ID_1, ?CLIENT_CREDENTIAL_1),

  ct:comment("Verify that we have started all required clients"),
  0 = meck:num_calls(banks_fetch_client_server_sup, start_child, '_'),
  true = meck:validate(banks_fetch_client_server_sup),

  true = meck:validate(banks_fetch_storage),

  ct:comment("Verify clients pid"),
  [{?BANK_ID_1, ?CLIENT_ID_1, ClientPid}] = lists:sort(banks_fetch_client_manager:get_clients_pids()),

  ct:comment("Exit clients manager"),
  exit(ClientsManagerPID, normal),

  ok.
