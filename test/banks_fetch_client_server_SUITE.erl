-module(banks_fetch_client_server_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, init_per_testcase/2, end_per_testcase/2 ]).
-export([should_fetch_data/1]).

-define(BANK_ID, {bank_id, <<"ing">>}).
-define(CLIENT_ID, {client_id, <<"123456789">>}).
-define(CLIENT_CREDENTIAL, {client_credential, {<<"987654">>,<<"130279">>}}).

all() -> [
          should_fetch_data
         ].

init_per_testcase(should_fetch_data, Config) ->
  meck:new(banks_fetch_bank_ing),
  meck:new(banks_fetch_storage),
  Config.

end_per_testcase(should_fetch_data, _Config) ->
  meck:unload(banks_fetch_storage),
  meck:unload(banks_fetch_bank_ing).

should_fetch_data(_Config) ->
  FakeAccounts = [account1, account2],
  meck:expect(banks_fetch_bank_ing, connect, fun(MockClientId, MockCredential) -> 
                                                 ?CLIENT_ID = {client_id, MockClientId},
                                                 ?CLIENT_CREDENTIAL = MockCredential, 
                                                 {ok, fake_auth} end),
  meck:expect(banks_fetch_bank_ing, fetch_accounts, fun(MockAuth) -> 
                                                        fake_auth = MockAuth,
                                                        {ok, FakeAccounts} 
                                                    end),
  meck:expect(banks_fetch_storage, store_accounts, fun(MockBankId, MockClientId, MockAccounts) ->
                                                       ?BANK_ID = MockBankId,
                                                       ?CLIENT_ID = MockClientId,
                                                       FakeAccounts = MockAccounts,
                                                       ok
                                                   end),

  {ok, BanksPID} = banks_fetch_client_server:start_link(?BANK_ID, ?CLIENT_ID, ?CLIENT_CREDENTIAL),

  ct:comment("Wait storage is done"),
  meck:wait(banks_fetch_storage, store_accounts, '_', 1000),

  ct:comment("Verify that expected functions has been called"),
  true = meck:validate(banks_fetch_bank_ing),
  true = meck:validate(banks_fetch_storage),

  ct:comment("Verify that accounts contain fetched accounts"),
  NewAccounts = banks_fetch_client_server:accounts(BanksPID),
  FakeAccounts = NewAccounts,

  ok.
