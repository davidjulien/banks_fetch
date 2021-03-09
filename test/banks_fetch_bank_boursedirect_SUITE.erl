-module(banks_fetch_bank_boursedirect_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([
         all/0,

         init_per_suite/1,
         end_per_suite/1,

         init_per_testcase/2,
         end_per_testcase/2,

         should_setup/1,

         should_connect_without_net/1,
         should_not_authenticate_if_password_is_invalid/1,
         should_not_authenticate_if_connection_failed/1,

         should_fetch_accounts_without_net/1,

         should_fetch_transactions_without_net/1,
         should_fetch_transactions_empty_without_net/1,
         should_fetch_transactions_until_without_net/1,

         test_with_real_credential/1
        ]).

-define(CLIENT_USERNAME, <<"david">>).
-define(CLIENT_PASSWORD, <<"mypassword">>).

all() ->
  [
   should_setup,

   should_connect_without_net,
   should_not_authenticate_if_password_is_invalid,
   should_not_authenticate_if_connection_failed,

   should_fetch_accounts_without_net,

   should_fetch_transactions_without_net,
   should_fetch_transactions_empty_without_net,
   should_fetch_transactions_until_without_net,

   test_with_real_credential
  ].

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

init_per_testcase(should_setup, Config) ->
  ok = application:start(prometheus),
  Config;
init_per_testcase(should_connect_without_net, Config) ->
  meck:new(prometheus_counter),
  meck:new(banks_fetch_http),
  Config;
init_per_testcase(should_not_authenticate_if_password_is_invalid, Config) ->
  meck:new(prometheus_counter),
  meck:new(banks_fetch_http),
  Config;
init_per_testcase(should_not_authenticate_if_connection_failed, Config) ->
  meck:new(prometheus_counter),
  meck:new(banks_fetch_http),
  Config;
init_per_testcase(should_fetch_accounts_without_net, Config) ->
  meck:new(prometheus_counter),
  meck:new(banks_fetch_http),
  Config;
init_per_testcase(should_fetch_transactions_without_net, Config) ->
  meck:new(prometheus_counter),
  meck:new(banks_fetch_http),
  Config;
init_per_testcase(should_fetch_transactions_empty_without_net, Config) ->
  meck:new(prometheus_counter),
  meck:new(banks_fetch_http),
  Config;
init_per_testcase(should_fetch_transactions_until_without_net, Config) ->
  meck:new(prometheus_counter),
  meck:new(banks_fetch_http),
  Config;
init_per_testcase(test_with_real_credential, Config) ->
  ct:comment("Load credential"),
  case file:consult(filename:join([?config(data_dir, Config), "boursedirect_real_credential.hrl"])) of
    {error, enoent} ->
      {skip, "You may add a banks_fetch_bank_boursedirect_SUITE_data/boursedirect_real_credential.hrl file with your Bourse Direct credential to launch a test with a real connection"};
    {ok, [{ClientId, ClientCredential}]} ->
      ok = application:start(prometheus),
      [{client_info, {ClientId, ClientCredential}} | Config]
  end.

end_per_testcase(should_setup, _Config) ->
  ok = application:stop(prometheus),
  ok;
end_per_testcase(should_connect_without_net, _Config) ->
  meck:unload(banks_fetch_http),
  meck:unload(prometheus_counter),
  ok;
end_per_testcase(should_not_authenticate_if_password_is_invalid, _Config) ->
  meck:unload(banks_fetch_http),
  meck:unload(prometheus_counter),
  ok;
end_per_testcase(should_not_authenticate_if_connection_failed, _Config) ->
  meck:unload(banks_fetch_http),
  meck:unload(prometheus_counter),
  ok;
end_per_testcase(should_fetch_accounts_without_net, _Config) ->
  meck:unload(banks_fetch_http),
  meck:unload(prometheus_counter),
  ok;
end_per_testcase(should_fetch_transactions_without_net, _Config) ->
  meck:unload(banks_fetch_http),
  meck:unload(prometheus_counter),
  ok;
end_per_testcase(should_fetch_transactions_empty_without_net, _Config) ->
  meck:unload(banks_fetch_http),
  meck:unload(prometheus_counter),
  ok;
end_per_testcase(should_fetch_transactions_until_without_net, _Config) ->
  meck:unload(banks_fetch_http),
  meck:unload(prometheus_counter),
  ok;
end_per_testcase(test_with_real_credential, _Config) ->
  application:stop(prometheus),
  ok.


%%
%% Setup
%%

should_setup(_Config) ->
  ok = banks_fetch_bank_boursedirect:setup(),

  ok.

%%
%% Connect
%%
should_connect_without_net(_Config) ->
  ct:comment("Connect to boursedirect account"),

  meck:expect(prometheus_counter, inc,
              [
               {['bank_boursedirect_connect_total_count'],ok},
               {['bank_boursedirect_connect_ok_count'],ok}
              ]),

  HttpExpectations = [
                       % Login
                       {
                        [post, {"https://api-mobile-v1.boursedirect.fr/api/authenticate", '_', "application/x-www-form-urlencoded", "site=BDI&password="++binary_to_list(?CLIENT_PASSWORD)++"&username="++binary_to_list(?CLIENT_USERNAME)}, '_', []],
                        {ok, {{'fakeversion', 200, 'fakereason'}, 'fakeheaders', "{\"token\":\"958bfc97dca0d620c6fa322d01524917\",\"code\":0,\"libelle\":\"Bienvenue sur l'application mobile Bourse Direct\",\"serviceClient\":\"01 72 78 58 16\",\"nbMessagesNonLus\":0}"}}
                       }
                      ],
  NbrHttpExpectations = length(HttpExpectations),

  meck:expect(banks_fetch_http, set_options, fun(MockOptions) -> [{cookies,enabled}] = MockOptions, ok end),
  meck:expect(banks_fetch_http, request, HttpExpectations),

  {ok, {bank_auth, banks_fetch_bank_boursedirect, none}} = banks_fetch_bank_boursedirect:connect({client_id, ?CLIENT_USERNAME}, {client_credential, ?CLIENT_PASSWORD}),

  ct:comment("Verify banks_fetch_http and monitoring calls"),
  true = meck:validate(banks_fetch_http),
  true = meck:validate(prometheus_counter),
  NbrHttpExpectations = meck:num_calls(banks_fetch_http, request, '_'),
  2 = meck:num_calls(prometheus_counter, inc, '_'),

  ok.


should_not_authenticate_if_password_is_invalid(_Config) ->
  ct:comment("Connect to boursedirect account"),

  meck:expect(prometheus_counter, inc,
              [
               {['bank_boursedirect_connect_total_count'],ok},
               {['bank_boursedirect_connect_wrong_authentication_count'],ok}
              ]),

  HttpExpectations = [
                       % Login
                       {
                        [post, {"https://api-mobile-v1.boursedirect.fr/api/authenticate", '_', "application/x-www-form-urlencoded", "site=BDI&password="++binary_to_list(?CLIENT_PASSWORD)++"&username="++binary_to_list(?CLIENT_USERNAME)}, '_', []],
                        {ok, {{'fakeversion', 200, 'fakereason'}, 'fakeheaders', "{\"token\":\"958bfc97dca0d620c6fa322d01524917\",\"code\":1006,\"libelle\":\"Votre identifiant ou votre mot de passe est incorrect....\"}"}}
                       }
                      ],
  NbrHttpExpectations = length(HttpExpectations),

  meck:expect(banks_fetch_http, set_options, fun(MockOptions) -> [{cookies,enabled}] = MockOptions, ok end),
  meck:expect(banks_fetch_http, request, HttpExpectations),

  {error, invalid_credential} = banks_fetch_bank_boursedirect:connect({client_id, ?CLIENT_USERNAME}, {client_credential, ?CLIENT_PASSWORD}),

  ct:comment("Verify banks_fetch_http and monitoring calls"),
  true = meck:validate(banks_fetch_http),
  true = meck:validate(prometheus_counter),
  NbrHttpExpectations = meck:num_calls(banks_fetch_http, request, '_'),
  2 = meck:num_calls(prometheus_counter, inc, '_'),

  ok.

should_not_authenticate_if_connection_failed(_Config) ->
  ct:comment("Connect to boursedirect account"),

  meck:expect(prometheus_counter, inc,
              [
               {['bank_boursedirect_connect_total_count'],ok},
               {['bank_boursedirect_connect_network_error_count'],ok}
              ]),

  HttpExpectations = [
                       % Login
                       {
                        [post, {"https://api-mobile-v1.boursedirect.fr/api/authenticate", '_', "application/x-www-form-urlencoded", "site=BDI&password="++binary_to_list(?CLIENT_PASSWORD)++"&username="++binary_to_list(?CLIENT_USERNAME)}, '_', []],
                        {error, failed_connect}
                       }
                      ],
  NbrHttpExpectations = length(HttpExpectations),

  meck:expect(banks_fetch_http, set_options, fun(MockOptions) -> [{cookies,enabled}] = MockOptions, ok end),
  meck:expect(banks_fetch_http, request, HttpExpectations),

  {error, network_error} = banks_fetch_bank_boursedirect:connect({client_id, ?CLIENT_USERNAME}, {client_credential, ?CLIENT_PASSWORD}),

  ct:comment("Verify banks_fetch_http and monitoring calls"),
  true = meck:validate(banks_fetch_http),
  true = meck:validate(prometheus_counter),
  NbrHttpExpectations = meck:num_calls(banks_fetch_http, request, '_'),
  2 = meck:num_calls(prometheus_counter, inc, '_'),

  ok.

%%
%% Fetch accounts
%%
should_fetch_accounts_without_net(Config) ->
  ct:comment("Load accounts examples"),
  {ok, AccountsJSON} = file:read_file(filename:join([?config(data_dir, Config), "accounts.json"])),

  meck:expect(prometheus_counter, inc,
              [
               {['bank_boursedirect_accounts_total_count'],ok},
               {['bank_boursedirect_accounts_ok_count'],ok}
              ]),

  meck:expect(banks_fetch_http, request, fun(MockMethod, {MockURL, MockHeaders}, MockHTTPOptions, MockOptions) ->
                                  get = MockMethod,
                                  "https://api-mobile-v1.boursedirect.fr/api/dashboard/accounts" = MockURL,
                                  [_|_] = MockHeaders,
                                  [] = MockHTTPOptions,
                                  [] = MockOptions,
                                  {ok, {{'fakeversion', 200, 'fakereason'}, 'fakeheaders', binary_to_list(AccountsJSON)}}
                              end),

  {ok, Accounts} = banks_fetch_bank_boursedirect:fetch_accounts({bank_auth, banks_fetch_bank_boursedirect, none}),

  ExpectedAccounts = [
                      #{balance => 14467.69, id => <<"1">>,
                        link => <<"/accounts/508TI00083333445EUR">>,name => <<"Stock market">>,
                        number => <<"508TI00083333445EUR">>,owner => <<"MR JULIEN DAVID">>,
                        ownership => single,type => markets},
                      #{balance => 10,id => <<"2">>,
                        link => <<"/accounts/508TI00084009089EUR">>,name => <<"Stock market">>,
                        number => <<"508TI00084009089EUR">>,owner => <<"MR JULIEN DAVID">>,
                        ownership => single,type => markets},
                      #{balance => 10,id => <<"3">>,
                        link => <<"/accounts/508TI00085534885EUR">>,name => <<"Stock market">>,
                        number => <<"508TI00085534885EUR">>,owner => <<"MR JULIEN DAVID">>,
                        ownership => single,type => markets}],

  NbrExpectedAccounts = length(ExpectedAccounts),
  NbrExpectedAccounts = length(Accounts),
  ExpectedAccounts = Accounts,

  ct:comment("Verify banks_fetch_http and monitoring calls"),
  true = meck:validate(banks_fetch_http),
  true = meck:validate(prometheus_counter),
  2 = meck:num_calls(prometheus_counter, inc, '_'),

  ok.

%%
%% Fetch transactions
%%
should_fetch_transactions_without_net(Config) ->
  ct:comment("Load transactions examples"),
  {ok, TransactionsJSON} = file:read_file(filename:join([?config(data_dir, Config), "transactions.json"])),

  FakeAccountId = <<"1">>,
  URL = "https://api-mobile-v1.boursedirect.fr/api/accounts/" ++ binary_to_list(FakeAccountId) ++ "/history",

  meck:expect(prometheus_counter, inc,
              [
               {['bank_boursedirect_transactions_total_count'],ok},
               {['bank_boursedirect_transactions_ok_count'],ok}
              ]),
  HttpExpectations = [
                      {[get, {URL, ['_']}, [], []],
                       {ok, {{'fakeversion', 200, 'fakereason'}, 'fakeheaders', binary_to_list(TransactionsJSON)}}
                      }
                     ],
  NbrHttpExpectations = length(HttpExpectations),

  meck:expect(banks_fetch_http, request, HttpExpectations),

  ct:comment("Fetch transactions"),
  {ok, Transactions} = banks_fetch_bank_boursedirect:fetch_transactions({bank_auth, banks_fetch_bank_ing, none}, {account_id, FakeAccountId}, first_call),

  ExpectedTransactions =
  [
   #{accounting_date => {{2021,2,24},{23,0,0}},
     amount => -266.89,
     description => <<"ACHAT COMPTANT : AM.RUSSELL 2000 UCIT.ETF EUR C A.RUS.2000 E.EUR C">>,
     effective_date => {{2021,2,24},{23,0,0}},
     id => <<"2021-02-25T00:00:00+0100_AM.RUSSELL 2000 UCIT.ETF EUR C A.RUS.2000 E.EUR C">>,
     type => stock},
   #{accounting_date => {{2021,2,24},{23,0,0}},
     amount => 300.0,
     description => <<"VIRT M DAVID JULIE">>,
     effective_date => {{2021,2,24},{23,0,0}},
     id => <<"2021-02-25T00:00:00+0100_VIRT M DAVID JULIE">>,
     type => transfer}
  ],

  ct:comment("Verify transactions count"),
  NbrExpectedTransactions = length(ExpectedTransactions),
  NbrExpectedTransactions = length(Transactions),
  lists:foreach(fun({E1,R1}) -> E1 = R1 end, lists:zip(ExpectedTransactions, Transactions)),

  ct:comment("Verify banks_fetch_http and monitoring alls"),
  true = meck:validate(banks_fetch_http),
  true = meck:validate(prometheus_counter),

  NbrHttpExpectations = meck:num_calls(banks_fetch_http, request, '_'),
  2 = meck:num_calls(prometheus_counter, inc, '_'),

  ok.

should_fetch_transactions_empty_without_net(Config) ->
  ct:comment("Load transactions examples"),
  {ok, TransactionsJSON} = file:read_file(filename:join([?config(data_dir, Config), "transactions_empty.json"])),

  FakeAccountId = <<"1">>,
  URL = "https://api-mobile-v1.boursedirect.fr/api/accounts/" ++ binary_to_list(FakeAccountId) ++ "/history",

  meck:expect(prometheus_counter, inc,
              [
               {['bank_boursedirect_transactions_total_count'],ok},
               {['bank_boursedirect_transactions_ok_count'],ok}
              ]),
  HttpExpectations = [
                      {[get, {URL, ['_']}, [], []],
                       {ok, {{'fakeversion', 200, 'fakereason'}, 'fakeheaders', binary_to_list(TransactionsJSON)}}
                      }
                     ],
  NbrHttpExpectations = length(HttpExpectations),

  meck:expect(banks_fetch_http, request, HttpExpectations),

  ct:comment("Fetch transactions"),
  {ok, Transactions} = banks_fetch_bank_boursedirect:fetch_transactions({bank_auth, banks_fetch_bank_ing, none}, {account_id, FakeAccountId}, first_call),

  ExpectedTransactions = [],

  ct:comment("Verify transactions count"),
  NbrExpectedTransactions = length(ExpectedTransactions),
  NbrExpectedTransactions = length(Transactions),

  ct:comment("Verify banks_fetch_http and monitoring alls"),
  true = meck:validate(banks_fetch_http),
  true = meck:validate(prometheus_counter),

  NbrHttpExpectations = meck:num_calls(banks_fetch_http, request, '_'),
  2 = meck:num_calls(prometheus_counter, inc, '_'),

  ok.


should_fetch_transactions_until_without_net(Config) ->
  ct:comment("Load transactions examples"),
  {ok, TransactionsJSON} = file:read_file(filename:join([?config(data_dir, Config), "transactions.json"])),

  FakeAccountId = <<"1">>,
  URL = "https://api-mobile-v1.boursedirect.fr/api/accounts/" ++ binary_to_list(FakeAccountId) ++ "/history",

  meck:expect(prometheus_counter, inc,
              [
               {['bank_boursedirect_transactions_total_count'],ok},
               {['bank_boursedirect_transactions_ok_count'],ok}
              ]),
  HttpExpectations = [
                      {[get, {URL, ['_']}, [], []],
                       {ok, {{'fakeversion', 200, 'fakereason'}, 'fakeheaders', binary_to_list(TransactionsJSON)}}
                      }
                     ],
  NbrHttpExpectations = length(HttpExpectations),

  meck:expect(banks_fetch_http, set_options, fun(MockOptions) -> [{cookies,enabled}] = MockOptions, ok end),
  meck:expect(banks_fetch_http, request, HttpExpectations),

  ct:comment("Fetch transactions"),
  {ok, Transactions} = banks_fetch_bank_boursedirect:fetch_transactions({bank_auth, banks_fetch_bank_ing, none}, {account_id, FakeAccountId}, {transaction_id, <<"2021-02-25T00:00:00+0100_VIRT M DAVID JULIE">>}),

  ExpectedTransactions =
  [
   #{accounting_date => {{2021,2,24},{23,0,0}},
     amount => -266.89,
     description => <<"ACHAT COMPTANT : AM.RUSSELL 2000 UCIT.ETF EUR C A.RUS.2000 E.EUR C">>,
     effective_date => {{2021,2,24},{23,0,0}},
     id => <<"2021-02-25T00:00:00+0100_AM.RUSSELL 2000 UCIT.ETF EUR C A.RUS.2000 E.EUR C">>,
     type => stock}
  ],

  ct:comment("Verify transactions count"),
  NbrExpectedTransactions = length(ExpectedTransactions),
  NbrExpectedTransactions = length(Transactions),
  lists:foreach(fun({E1,R1}) -> E1 = R1 end, lists:zip(ExpectedTransactions, Transactions)),

  ct:comment("Verify banks_fetch_http and monitoring alls"),
  true = meck:validate(banks_fetch_http),
  true = meck:validate(prometheus_counter),

  NbrHttpExpectations = meck:num_calls(banks_fetch_http, request, '_'),
  2 = meck:num_calls(prometheus_counter, inc, '_'),

  ok.



%%
%% Test with real credential
%%
test_with_real_credential(Config) ->
  {client_info, {ClientId, ClientCredential}} = lists:keyfind(client_info,1, Config),

  ct:comment("Setup"),
  ok = banks_fetch_http:setup(),
  ok = banks_fetch_bank:setup(banks_fetch_bank_boursedirect),

  ct:comment("Connect to Bourse Direct account"),
  {ok, Auth} = banks_fetch_bank_boursedirect:connect(ClientId, ClientCredential),

  {bank_auth, banks_fetch_bank_boursedirect, none} = Auth,

  ct:comment("Fetch accounts"),
  {ok, Accounts} = banks_fetch_bank_boursedirect:fetch_accounts(Auth),
  true = length(Accounts) > 0,

  lists:foreach(fun(#{ id := AccountId } = Account) ->
                    ct:comment("Fetch transactions for account ~1000p", [Account]),
                    {ok, Transactions} = banks_fetch_bank_boursedirect:fetch_transactions(Auth, {account_id, AccountId}, first_call),
                    NbrTransactions = length(Transactions),
                    ct:pal("~p : ~B transactions fetched", [Account, NbrTransactions])
                end, Accounts),
  ok.
