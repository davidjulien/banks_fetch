-module(banks_fetch_client_server_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2 ]).
-export([
         should_handle_cast_do_nothing/1,
         should_fetch_data_without_bank_storage/1,
         should_not_fetch_data_if_connection_failed_without_bank_storage/1,
         should_not_fetch_data_if_network_error_without_bank_storage/1
        ]).

-define(BANK_ID, {bank_id, <<"ing">>}).
-define(CLIENT_ID, {client_id, <<"123456789">>}).
-define(CLIENT_CREDENTIAL, {client_credential, {<<"987654">>,<<"130279">>}}).

all() -> [
          should_handle_cast_do_nothing,
          should_fetch_data_without_bank_storage,
          should_not_fetch_data_if_connection_failed_without_bank_storage,
          should_not_fetch_data_if_network_error_without_bank_storage
         ].

init_per_suite(Config) ->
  ok = lager:start(),
  Config.

end_per_suite(_Config) ->
  application:stop(lager).

init_per_testcase(should_handle_cast_do_nothing, Config) ->
  Config;
init_per_testcase(should_fetch_data_without_bank_storage, Config) ->
  meck:new(banks_fetch_bank_ing),
  meck:new(banks_fetch_storage),
  meck:new(timer, [unstick,passthrough]), % unstick because timer resides in sticky dir, passthrough because lager needs now_diff
  Config;
init_per_testcase(should_not_fetch_data_if_connection_failed_without_bank_storage, Config) ->
  meck:new(banks_fetch_bank_ing),
  meck:new(banks_fetch_storage),
  meck:new(timer, [unstick,passthrough]), % unstick because timer resides in sticky dir, passthrough because lager needs now_diff
  Config;
init_per_testcase(should_not_fetch_data_if_network_error_without_bank_storage, Config) ->
  meck:new(banks_fetch_bank_ing),
  meck:new(banks_fetch_storage),
  meck:new(timer, [unstick,passthrough]), % unstick because timer resides in sticky dir, passthrough because lager needs now_diff
  Config.

end_per_testcase(should_handle_cast_do_nothing, _Config) ->
  ok;
end_per_testcase(should_fetch_data_without_bank_storage, _Config) ->
  meck:unload(timer),
  meck:unload(banks_fetch_storage),
  meck:unload(banks_fetch_bank_ing);
end_per_testcase(should_not_fetch_data_if_connection_failed_without_bank_storage, _Config) ->
  meck:unload(timer),
  meck:unload(banks_fetch_storage),
  meck:unload(banks_fetch_bank_ing);
end_per_testcase(should_not_fetch_data_if_network_error_without_bank_storage, _Config) ->
  meck:unload(timer),
  meck:unload(banks_fetch_storage),
  meck:unload(banks_fetch_bank_ing).


should_handle_cast_do_nothing(_Config) ->
  {noreply, dummystate} = banks_fetch_client_server:handle_cast(dummycall, dummystate),
  ok.

should_fetch_data_without_bank_storage(_Config) ->
  FakeAccounts = [#{ id => <<"ACCOUNT_1">> }, #{ id => <<"ACCOUNT_2">> }],
  FakeAuth = fake_auth,
  meck:expect(banks_fetch_bank_ing, connect, fun(MockClientId, MockCredential) ->
                                                 ?CLIENT_ID = MockClientId,
                                                 ?CLIENT_CREDENTIAL = MockCredential,
                                                 {ok, fake_auth} end),
  meck:expect(banks_fetch_bank_ing, fetch_accounts, fun(MockAuth) ->
                                                        FakeAuth = MockAuth,
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

  meck:expect(banks_fetch_storage, get_last_transactions_id,
              fun(MockBankId, MockClientId) ->
                  ?BANK_ID = MockBankId,
                  ?CLIENT_ID = MockClientId,
                  {value, [{{account_id, <<"ACCOUNT_1">>},{transaction_id, <<"TRANSACTION_ID_1">>}}]}
              end),

  TransactionsAccount1 = [transaction_11, transaction_12],
  TransactionsAccount2 = [transaction_21, transaction_22, transaction_23],
  ExpectedFetchTransactionsCalls = [
                                    {[FakeAuth,{account_id, <<"ACCOUNT_1">>},{transaction_id, <<"TRANSACTION_ID_1">>}], {ok, TransactionsAccount1}},
                                    {[FakeAuth,{account_id, <<"ACCOUNT_2">>},first_call], {ok, TransactionsAccount2}}
                                   ],
  meck:expect(banks_fetch_bank_ing, fetch_transactions, ExpectedFetchTransactionsCalls),

  ExpectedStoreTransactionsCalls = [
                                    {[?BANK_ID, ?CLIENT_ID, {account_id, <<"ACCOUNT_1">>}, BeforeFetchingDateTime, TransactionsAccount1], ok},
                                    {[?BANK_ID, ?CLIENT_ID, {account_id, <<"ACCOUNT_2">>}, BeforeFetchingDateTime, TransactionsAccount2], ok}
                                   ],
  meck:expect(banks_fetch_storage, store_transactions, ExpectedStoreTransactionsCalls),

  meck:expect(timer, send_after, fun(MockTime, MockCall) ->
                                     4 * 60 * 60 * 1000 = MockTime,
                                     fetch_data = MockCall,
                                     {ok, tref}
                                 end),
  {ok, BanksPID} = banks_fetch_client_server:start_link(?BANK_ID, ?CLIENT_ID, ?CLIENT_CREDENTIAL),

  ct:comment("Wait storage is done"),
  meck:wait(banks_fetch_storage, store_transactions, '_', 5000),

  ct:comment("Verify that client server contain fetched accounts"),
  % This call ensures that fetch_data call in banks_fetch_client_server is terminated
  NewAccounts = banks_fetch_client_server:accounts(BanksPID),
  FakeAccounts = NewAccounts,

  ct:comment("Verify that expected functions has been called"),
  true = meck:validate(banks_fetch_bank_ing),
  true = meck:validate(banks_fetch_storage),
  true = meck:validate(timer),
  NbrAccounts = length(FakeAccounts),
  NbrAccounts = meck:num_calls(banks_fetch_bank_ing, fetch_transactions, '_'),
  NbrAccounts = meck:num_calls(banks_fetch_storage, store_transactions, '_'),

  ok.

should_not_fetch_data_if_connection_failed_without_bank_storage(_Config) ->
  meck:expect(banks_fetch_bank_ing, connect, fun(MockClientId, MockCredential) ->
                                                 ?CLIENT_ID = MockClientId,
                                                 ?CLIENT_CREDENTIAL = MockCredential,
                                                 {error, invalid_credential} end),

  {ok, BanksPID} = banks_fetch_client_server:start_link(?BANK_ID, ?CLIENT_ID, ?CLIENT_CREDENTIAL),

  ct:comment("Verify that we don't try to fetch accounts"),
  try
    meck:wait(banks_fetch_bank_ing, fetch_acccounts, '_', 1000)
  catch error:timeout ->
          ok
  end,

  ct:comment("Verify that expected functions has been called"),
  true = meck:validate(banks_fetch_bank_ing),
  true = meck:validate(banks_fetch_storage),
  true = meck:validate(timer),

  ct:comment("Verify that client server does not contain accounts"),
  NewAccounts = banks_fetch_client_server:accounts(BanksPID),
  [] = NewAccounts,

  ok.

should_not_fetch_data_if_network_error_without_bank_storage(_Config) ->
  meck:expect(banks_fetch_bank_ing, connect, fun(MockClientId, MockCredential) ->
                                                 ?CLIENT_ID = MockClientId,
                                                 ?CLIENT_CREDENTIAL = MockCredential,
                                                 {error, network_error}
                                             end),
  meck:expect(timer, send_after, fun(MockTime, MockCall) ->
                                     1 * 60 * 60 * 1000 = MockTime,
                                     fetch_data = MockCall,
                                     {ok, tref}
                                 end),

  {ok, BanksPID} = banks_fetch_client_server:start_link(?BANK_ID, ?CLIENT_ID, ?CLIENT_CREDENTIAL),

  ct:comment("Verify that we don't try to fetch accounts"),
  try
    meck:wait(banks_fetch_bank_ing, fetch_acccounts, '_', 1000)
  catch error:timeout ->
          ok
  end,

  ct:comment("Verify that expected functions has been called"),
  true = meck:validate(banks_fetch_bank_ing),
  true = meck:validate(banks_fetch_storage),
  true = meck:validate(timer),

  ct:comment("Verify that client server does not contain accounts"),
  NewAccounts = banks_fetch_client_server:accounts(BanksPID),
  [] = NewAccounts,

  ok.
