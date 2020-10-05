-module(banks_fetch_api_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("elli/include/elli.hrl").
-export([
         all/0,
         init_per_suite/1,
         end_per_suite/1,

         init_per_testcase/2,
         end_per_testcase/2,

         should_handle_event_do_nothing/1,
         should_handle_unknown_request/1,

         should_return_last_transactions/1,
         should_return_last_transactions_with_client/1,

         should_return_banks/1,
         should_return_banks_with_client/1,

         should_return_all_accounts/1,
         should_return_all_accounts_with_client/1
        ]).

all() ->
  [
   should_handle_event_do_nothing,
   should_handle_unknown_request,

   should_return_last_transactions,
   should_return_last_transactions_with_client,

   should_return_banks,
   should_return_banks_with_client,

   should_return_all_accounts,
   should_return_all_accounts_with_client
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

init_elli(Config) ->
  application:start(crypto),
  application:start(public_key),
  application:start(ssl),
  {ok, _} = application:ensure_all_started(hackney),

  ElliConfig = [
                {mods, [
                        {banks_fetch_api, []}
                       ]}
               ],

  {ok, P} = elli:start_link([{callback, elli_middleware},
                             {callback_args, ElliConfig},
                             {port, 3003}]),
  unlink(P),

  [{elli_pid, P}|Config].

teardown_elli(Config) ->
  {elli_pid, P} = lists:keyfind(elli_pid, 1, Config),
  elli:stop(P),
  ok.


init_per_testcase(should_handle_event_do_nothing, Config) ->
  Config;

init_per_testcase(should_handle_unknown_request, Config) ->
  Config;

init_per_testcase(should_return_last_transactions, Config) ->
  meck:new(banks_fetch_storage),
  Config;

init_per_testcase(should_return_last_transactions_with_client, Config) ->
  meck:new(banks_fetch_storage),
  init_elli(Config);

init_per_testcase(should_return_banks, Config) ->
  meck:new(banks_fetch_storage),
  Config;

init_per_testcase(should_return_banks_with_client, Config) ->
  meck:new(banks_fetch_storage),
  init_elli(Config);

init_per_testcase(should_return_all_accounts, Config) ->
  meck:new(banks_fetch_storage),
  Config;

init_per_testcase(should_return_all_accounts_with_client, Config) ->
  meck:new(banks_fetch_storage),
  init_elli(Config).



end_per_testcase(should_handle_event_do_nothing, _Config) ->
  ok;

end_per_testcase(should_handle_unknown_request, _Config) ->
  ok;

end_per_testcase(should_return_last_transactions, _Config) ->
  meck:unload(banks_fetch_storage),
  ok;

end_per_testcase(should_return_last_transactions_with_client, Config) ->
  meck:unload(banks_fetch_storage),
  teardown_elli(Config),
  ok;

end_per_testcase(should_return_banks, _Config) ->
  meck:unload(banks_fetch_storage),
  ok;

end_per_testcase(should_return_banks_with_client, Config) ->
  meck:unload(banks_fetch_storage),
  teardown_elli(Config),
  ok;

end_per_testcase(should_return_all_accounts, _Config) ->
  meck:unload(banks_fetch_storage),
  ok;

end_per_testcase(should_return_all_accounts_with_client, Config) ->
  meck:unload(banks_fetch_storage),
  teardown_elli(Config),
  ok.



should_handle_event_do_nothing(_Config) ->
  ok = banks_fetch_api:handle_event('any', 'any', 'any'),
  ok.


should_handle_unknown_request(_Config) ->
  Req = #req{ method = 'GET', path = [<<"invalid">>] },
  {Status, Headers, Body} = banks_fetch_api:handle(Req, no_args),
  404 = Status,
  [] = Headers,
  <<"Not Found">> = Body,

  ok.


%%
%% Should return last transactions
%%

-define(TRANSACTIONS,
        [
         #{ id => <<"TRANSACTION_2">>, bank_id => {bank_id, <<"ing">>}, client_id => {client_id, <<"client1">>}, account_id => {account_id, <<"account1">>},
            accounting_date => {2020,7,22}, effective_date => {2020,7,22}, amount => -14.32, description => <<"PRLV SEPA XXX">>, type => sepa_debit },
         #{ id => <<"TRANSACTION_1">>, bank_id => {bank_id, <<"ing">>}, client_id => {client_id, <<"client2">>}, account_id => {account_id, <<"account2">>},
            accounting_date => {2020,7,21}, effective_date => {2020,7,21}, amount => -34.32, description => <<"PAIEMENT PAR CARTE 20/07/2020 XXX">>, type => card_debit }
        ]).
-define(TRANSACTIONS_JSON, <<"{\"transactions\":[{\"type\":\"sepa_debit\",\"id\":\"TRANSACTION_2\",\"effective_date\":\"2020-07-22\",\"description\":\"PRLV SEPA XXX\",\"client_id\":\"client1\",\"bank_id\":\"ing\",\"amount\":-14.32,\"accounting_date\":\"2020-07-22\",\"account_id\":\"account1\"},{\"type\":\"card_debit\",\"id\":\"TRANSACTION_1\",\"effective_date\":\"2020-07-21\",\"description\":\"PAIEMENT PAR CARTE 20/07/2020 XXX\",\"client_id\":\"client2\",\"bank_id\":\"ing\",\"amount\":-34.32,\"accounting_date\":\"2020-07-21\",\"account_id\":\"account2\"}]}">>).

should_return_last_transactions(_Config) ->
  Req = #req{ method = 'GET', path = [<<"api">>,<<"1.0">>,<<"transactions">>] },
  meck:expect(banks_fetch_storage, get_last_transactions, fun(MockN) ->
                                                              10 = MockN,
                                                              {value, ?TRANSACTIONS}
                                                          end),
  {Status, Headers, Body} = banks_fetch_api:handle(Req, no_args),
  200 = Status,
  [{<<"Content-Type">>, <<"application/json">>}] = Headers,
  ?TRANSACTIONS_JSON = Body,

  true = meck:validate(banks_fetch_storage),

  ok.

should_return_last_transactions_with_client(_Config) ->
  meck:expect(banks_fetch_storage, get_last_transactions, fun(MockN) ->
                                                              10 = MockN,
                                                              {value, ?TRANSACTIONS}
                                                          end),

  Response = hackney:get("http://localhost:3003/api/1.0/transactions"),
  200 = status(Response),
  ?TRANSACTIONS_JSON = body(Response),

  true = meck:validate(banks_fetch_storage),

  ok.


-define(BANKS, [#{ id => <<"ing">>, name => <<"ING">>} ]).
-define(BANKS_JSON, <<"[{\"name\":\"ING\",\"id\":\"ing\"}]">>).

should_return_banks(_Config) ->
  Req = #req{ method = 'GET', path = [<<"api">>,<<"1.0">>,<<"banks">>] },
  meck:expect(banks_fetch_storage, get_banks, fun() -> {value, ?BANKS} end),
  {Status, Headers, Body} = banks_fetch_api:handle(Req, no_args),
  200 = Status,
  [{<<"Content-Type">>, <<"application/json">>}] = Headers,
  ?BANKS_JSON = Body,

  true = meck:validate(banks_fetch_storage),

  ok.


should_return_banks_with_client(_Config) ->
  meck:expect(banks_fetch_storage, get_banks, fun() -> {value, ?BANKS} end),

  Response = hackney:get("http://localhost:3003/api/1.0/banks"),
  200 = status(Response),
  ?BANKS_JSON = body(Response),

  true = meck:validate(banks_fetch_storage),

  ok.



-define(ACCOUNTS, [
                   #{ id => <<"account1">>, balance => 234.12, number => <<"number1">>, owner => <<"owner1">>, ownership => single, type => current, name => <<"CURRENT">> },
                   #{ id => <<"account2">>, balance => 4321.78, number => <<"number2">>, owner => <<"owner2">>, ownership => single, type => savings, name => <<"LDD">> }
                  ]).
-define(ACCOUNTS_JSON, <<"[{\"type\":\"current\",\"ownership\":\"single\",\"owner\":\"owner1\",\"number\":\"number1\",\"name\":\"CURRENT\",\"id\":\"account1\",\"balance\":234.12},{\"type\":\"savings\",\"ownership\":\"single\",\"owner\":\"owner2\",\"number\":\"number2\",\"name\":\"LDD\",\"id\":\"account2\",\"balance\":4321.78}]">>).

should_return_all_accounts(_Config) ->
  Req = #req{ method = 'GET', path = [<<"api">>,<<"1.0">>,<<"accounts">>] },
  meck:expect(banks_fetch_storage, get_all_accounts, fun() -> {value, ?ACCOUNTS} end),
  {Status, Headers, Body} = banks_fetch_api:handle(Req, no_args),
  200 = Status,
  [{<<"Content-Type">>, <<"application/json">>}] = Headers,
  ?ACCOUNTS_JSON = Body,

  true = meck:validate(banks_fetch_storage),

  ok.


should_return_all_accounts_with_client(_Config) ->
  meck:expect(banks_fetch_storage, get_all_accounts, fun() -> {value, ?ACCOUNTS} end),

  Response = hackney:get("http://localhost:3003/api/1.0/accounts"),
  200 = status(Response),
  ?ACCOUNTS_JSON = body(Response),

  true = meck:validate(banks_fetch_storage),

  ok.


%%
%% Functions to extract data from hackney response (from elli_test.hrl)
%%

status({ok, Status, _Headers, _ClientRef}) ->
  Status;
status({ok, Status, _Headers}) ->
  Status.

body({ok, _Status, _Headers, ClientRef}) ->
  {ok, Body} = hackney:body(ClientRef),
  Body.

%headers({ok, _Status, Headers, _ClientRef}) ->
%    lists:sort(Headers);
%headers({ok, _Status, Headers}) ->
%    lists:sort(Headers).
