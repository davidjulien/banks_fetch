-module(banks_fetch_storage_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, init_per_testcase/2, end_per_testcase/2 ]).
-export([
         should_get_clients/1,
         should_store_accounts/1
        ]).

all() -> [
          should_get_clients,
          should_store_accounts
         ].

init_per_testcase(_, Config) ->
  Config.

end_per_testcase(_, _Config) ->
  ok.

should_get_clients(_Config) ->
  % FAKE TEST
  {value, Clients} = banks_fetch_storage:get_clients(),
  [] = Clients,

  ok.

should_store_accounts(_Config) ->
  % FAKE TEST
  ok = banks_fetch_storage:store_accounts(bank_id, client_id, []).
