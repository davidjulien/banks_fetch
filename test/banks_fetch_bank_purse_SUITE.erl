-module(banks_fetch_bank_purse_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([
         all/0,
         init_per_suite/1,
         end_per_suite/1,

         init_per_testcase/2,
         end_per_testcase/2,

         should_setup/1,
         should_connect/1,
         should_fetch_accounts/1,
         should_fetch_transactions/1
        ]).

all() ->
  [
   should_setup,
   should_connect,
   should_fetch_accounts,
   should_fetch_transactions
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

init_per_testcase(_, Config) ->
  meck:new(calendar, [passthrough, unstick]),
  meck:new(banks_fetch_storage),
  Config.

end_per_testcase(_, _Config) ->
  meck:unload(calendar),
  meck:unload(banks_fetch_storage),
  ok.

-define(CLIENT_ID_VAL, <<"123456789">>).
-define(CLIENT_ID, {client_id, ?CLIENT_ID_VAL}).
-define(START_DATE, {2021,1,5}).
-define(CURRENT_DATE, {{2021,1,5}, {12,14,15}}).
-define(SOURCES, [source1, source2]).

should_setup(_Config) ->
  ok = banks_fetch_bank_purse:setup(),

  ok.

should_connect(_Config) ->
  FakeCurrentDate = ?CURRENT_DATE,

  meck:expect(calendar, universal_time, fun() -> FakeCurrentDate end),

  {ok, {bank_auth, banks_fetch_bank_purse, {purse, FakeCurrentDate, ?CLIENT_ID, ?START_DATE, ?SOURCES}}} = banks_fetch_bank_purse:connect(?CLIENT_ID, {client_credential, {?START_DATE, ?SOURCES}}),

  true = meck:validate(banks_fetch_storage),
  true = meck:validate(calendar),

  ok.

%%
%% Fetch accounts
%%
should_fetch_accounts(_Config) ->
  FakeBalance = -200.00,
  meck:expect(banks_fetch_storage, aggregate_amounts_for_purse, fun(MockStartDate, MockCurrentDate, MockPurseId, MockSourcesList) ->
                                                                    ?START_DATE = MockStartDate,
                                                                    ?CURRENT_DATE = MockCurrentDate,
                                                                    ?CLIENT_ID = MockPurseId,
                                                                    ?SOURCES = MockSourcesList,
                                                                    {value, FakeBalance}
                                                                end),

  {ok, AccountInfoList} = banks_fetch_bank_purse:fetch_accounts({bank_auth, banks_fetch_bank_purse, {purse, ?CURRENT_DATE, ?CLIENT_ID, ?START_DATE, ?SOURCES}}),

  1 = length(AccountInfoList),
  #{balance := -200.0, id := ?CLIENT_ID_VAL, name := <<"Purse">>, number := <<>>, owner := ?CLIENT_ID_VAL, ownership := single, type := purse} = lists:nth(1, AccountInfoList),

  true = meck:validate(banks_fetch_storage),
  true = meck:validate(calendar),

  ok.

%%
%% Fetch transactions
%%
should_fetch_transactions(_Config) ->
  FakeTransactions = [transaction1, transaction2],
  meck:expect(banks_fetch_storage, get_new_transactions_for_purse, fun(MockStartDate, MockCurrentDate, MockPurseId, MockSourcesList) ->
                                                                    ?START_DATE = MockStartDate,
                                                                    ?CURRENT_DATE = MockCurrentDate,
                                                                    ?CLIENT_ID = MockPurseId,
                                                                    ?SOURCES = MockSourcesList,
                                                                    {value, FakeTransactions}
                                                                end),

  {ok, FakeTransactions} = banks_fetch_bank_purse:fetch_transactions({bank_auth, banks_fetch_bank_purse, {purse, ?CURRENT_DATE, ?CLIENT_ID, ?START_DATE, ?SOURCES}}, <<"purse">>, first_call),

  true = meck:validate(banks_fetch_storage),
  true = meck:validate(calendar),

  ok.
