-module(banks_fetch_client_manager_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, init_per_testcase/2, end_per_testcase/2 ]).
-export([
         should_handle_cast_do_nothing/1,
         should_launch_all_client_servers_on_init/1
        ]).

-define(BANK_CLIENTS, 
        [
         {bank_id_1, client_id_1, client_credential_1}, 
         {bank_id_1, client_id_2, client_credential_2},
         {bank_id_2, client_id_3, client_credential_3}
        ]).

all() -> [
          should_handle_cast_do_nothing,
          should_launch_all_client_servers_on_init
         ].

init_per_testcase(should_handle_cast_do_nothing, Config) ->
  Config;
init_per_testcase(should_launch_all_client_servers_on_init, Config) ->
  meck:new(banks_fetch_storage),
  meck:new(banks_fetch_client_server_sup),
  Config.

end_per_testcase(should_handle_cast_do_nothing, _Config) ->
  ok;
end_per_testcase(should_launch_all_client_servers_on_init, _Config) ->
  meck:unload(banks_fetch_client_server_sup),
  meck:unload(banks_fetch_storage).

should_handle_cast_do_nothing(_Config) ->
  {noreply, dummystate} = banks_fetch_client_manager:handle_cast(dummycall, dummystate),
  ok.


should_launch_all_client_servers_on_init(_Config) ->
  ct:comment("Expect calls to storage to get all clients and start_child calls for every client"),
  meck:expect(banks_fetch_storage, get_clients, fun() -> {value, ?BANK_CLIENTS} end),

  ClientsPids = lists:zip([ [BankId, ClientId, ClientCredential] || {BankId, ClientId, ClientCredential} <- ?BANK_CLIENTS], [ {ok, list_to_atom("pid_" ++ integer_to_list(N))} || N <- lists:seq(1,length(?BANK_CLIENTS)) ]),
  meck:expect(banks_fetch_client_server_sup, start_child, ClientsPids),

  ct:comment("Start clients manager"),
  {ok, ClientsManagerPID} = banks_fetch_client_manager:start_link(),

  ct:comment("Wait get_banks_clients"),
  meck:wait(banks_fetch_storage, get_clients, '_', 1000),

  ct:comment("Verify that we have fetched clients from storage"),
  true = meck:validate(banks_fetch_storage),

  ct:comment("Verify that we have triggered client_servers"),
  true = meck:validate(banks_fetch_client_server_sup),

  ct:comment("Verify that we have started all required clients"),
  NbrBankClients = length(?BANK_CLIENTS),
  NbrBankClients = meck:num_calls(banks_fetch_client_server_sup, start_child, '_'),

  ct:comment("Verify clients pid"),
  [{bank_id_1, client_id_1, pid_1}, {bank_id_1, client_id_2, pid_2}, {bank_id_2, client_id_3, pid_3}] = lists:sort(banks_fetch_client_manager:get_clients_pids()),

  exit(ClientsManagerPID, normal),

  ok.
