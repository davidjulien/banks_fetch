-module(banks_fetch_bank_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([
         all/0,
         init_per_testcase/2,
         end_per_testcase/2,

         should_call_connect/1,
         should_call_fetch_accounts/1,
         should_call_fetch_transactions/1
        ]).

all() ->
  [
   should_call_connect,
   should_call_fetch_accounts,
   should_call_fetch_transactions
  ].


init_per_testcase(_, Config) ->
  meck:new(banks_fetch_bank_test, [non_strict]), % this module does not exist
  Config.

end_per_testcase(_, _Config) ->
  meck:unload(banks_fetch_bank_test),
  ok.


-define(CLIENT_ID, {client_id, <<"client1">>}).
-define(CLIENT_CREDENTIAL, {client_credential, <<"credential1">>}).

should_call_connect(_Config) ->
  BankAuth = {bank_auth, banks_fetch_bank_test, "AUTH_TOKEN"},
  meck:expect(banks_fetch_bank_test, connect, fun(MockClientId, MockClientCredential) ->
                                                  ?CLIENT_ID = MockClientId,
                                                  ?CLIENT_CREDENTIAL = MockClientCredential,
                                                  {ok, BankAuth}
                                              end),
  ct:comment("Verify connect"),
  {ok, BankAuth} = banks_fetch_bank:connect(banks_fetch_bank_test, ?CLIENT_ID, ?CLIENT_CREDENTIAL),

  true = meck:validate(banks_fetch_bank_test),

  ok.

should_call_fetch_accounts(_Config) ->
  BankAuth = {bank_auth, banks_fetch_bank_test, "AUTH_TOKEN"},
  Accounts = [account1],
  meck:expect(banks_fetch_bank_test, fetch_accounts, fun(MockAuth) ->
                                                         BankAuth = MockAuth,
                                                         {ok, Accounts}
                                              end),
  ct:comment("Verify fetch accounts"),
  {ok, Accounts} = banks_fetch_bank:fetch_accounts(banks_fetch_bank_test, BankAuth),

  true = meck:validate(banks_fetch_bank_test),

  ok.

should_call_fetch_transactions(_Config) ->
  BankAuth = {bank_auth, banks_fetch_bank_test, "AUTH_TOKEN"},
  AccountId = <<"account1">>,
  LastTransactionId = <<"last_transaction">>,
  Transactions = [transaction1, transaction2],
  meck:expect(banks_fetch_bank_test, fetch_transactions, fun(MockAuth, MockAccountId, MockLastTransactionId) ->
                                                         BankAuth = MockAuth,
                                                         AccountId = MockAccountId,
                                                         LastTransactionId = MockLastTransactionId,
                                                         {ok, Transactions}
                                              end),
  ct:comment("Verify fetch transactions"),
  {ok, Transactions} = banks_fetch_bank:fetch_transactions(banks_fetch_bank_test, BankAuth, AccountId, LastTransactionId),

  true = meck:validate(banks_fetch_bank_test),

  ok.
