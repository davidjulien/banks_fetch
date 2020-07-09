-module(banks_fetch_client_server_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, init_per_testcase/2, end_per_testcase/2 ]).
-export([should_fetch_data_without_bank_storage/1, should_handle_cast_do_nothing/1]).

-define(BANK_ID, {bank_id, <<"ing">>}).
-define(CLIENT_ID, {client_id, <<"123456789">>}).
-define(CLIENT_CREDENTIAL, {client_credential, {<<"987654">>,<<"130279">>}}).

all() -> [
          should_handle_cast_do_nothing,
          should_fetch_data_without_bank_storage
         ].

init_per_testcase(should_handle_cast_do_nothing, Config) ->
  Config;
init_per_testcase(should_fetch_data_without_bank_storage, Config) ->
  meck:new(banks_fetch_bank_ing),
  meck:new(banks_fetch_storage),
  Config.

end_per_testcase(should_handle_cast_do_nothing, _Config) ->
  ok;
end_per_testcase(should_fetch_data_without_bank_storage, _Config) ->
  meck:unload(banks_fetch_storage),
  meck:unload(banks_fetch_bank_ing).

should_handle_cast_do_nothing(_Config) ->
  {noreply, dummystate} = banks_fetch_client_server:handle_cast(dummycall, dummystate),
  ok.

should_fetch_data_without_bank_storage(_Config) ->
  FakeAccounts = [account1, account2],
  meck:expect(banks_fetch_bank_ing, connect, fun(MockClientId, MockCredential) ->
                                                 ?CLIENT_ID = MockClientId,
                                                 ?CLIENT_CREDENTIAL = MockCredential,
                                                 {ok, fake_auth} end),
  meck:expect(banks_fetch_bank_ing, fetch_accounts, fun(MockAuth) ->
                                                        fake_auth = MockAuth,
                                                        {ok, FakeAccounts}
                                                    end),
  BeforeFetchingDateTime = calendar:universal_time(),
  meck:expect(banks_fetch_storage, store_accounts, fun(MockBankId, MockClientId, MockFetchingAt, MockAccounts) ->
                                                       ?BANK_ID = MockBankId,
                                                       ?CLIENT_ID = MockClientId,
                                                       AfterFetchingDatetime = calendar:universal_time(),
                                                       true = BeforeFetchingDateTime =< MockFetchingAt,
                                                       true = MockFetchingAt =< AfterFetchingDatetime,
                                                       FakeAccounts = MockAccounts,
                                                       ok
                                                   end),

  {ok, BanksPID} = banks_fetch_client_server:start_link(?BANK_ID, ?CLIENT_ID, ?CLIENT_CREDENTIAL),

  ct:comment("Wait storage is done"),
  meck:wait(banks_fetch_storage, store_accounts, '_', 1000),

  ct:comment("Verify that expected functions has been called"),
  true = meck:validate(banks_fetch_bank_ing),
  true = meck:validate(banks_fetch_storage),

  ct:comment("Verify that client server contain fetched accounts"),
  NewAccounts = banks_fetch_client_server:accounts(BanksPID),
  FakeAccounts = NewAccounts,

  ok.
