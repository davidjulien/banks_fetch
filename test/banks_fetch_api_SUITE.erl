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
         should_return_last_transactions_with_client/1
        ]).

all() ->
  [
   should_handle_event_do_nothing,
   should_handle_unknown_request,
   should_return_last_transactions,
   should_return_last_transactions_with_client
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

init_per_testcase(should_handle_event_do_nothing, Config) ->
  Config;

init_per_testcase(should_handle_unknown_request, Config) ->
  Config;

init_per_testcase(should_return_last_transactions, Config) ->
  meck:new(banks_fetch_storage),
  Config;

init_per_testcase(should_return_last_transactions_with_client, Config) ->
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

  meck:new(banks_fetch_storage),

  [{elli_pid, P}|Config].


end_per_testcase(should_handle_event_do_nothing, _Config) ->
  ok;

end_per_testcase(should_handle_unknown_request, _Config) ->
  ok;

end_per_testcase(should_return_last_transactions, _Config) ->
  meck:unload(banks_fetch_storage),
  ok;

end_per_testcase(should_return_last_transactions_with_client, Config) ->
  meck:unload(banks_fetch_storage),

  {elli_pid, P} = lists:keyfind(elli_pid, 1, Config),
  elli:stop(P),
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
         #{ id => <<"TRANSACTION_2">>, accounting_date => {2020,7,22}, effective_date => {2020,7,22}, amount => -14.32, description => <<"PRLV SEPA XXX">>, type => sepa_debit },
         #{ id => <<"TRANSACTION_1">>, accounting_date => {2020,7,21}, effective_date => {2020,7,21}, amount => -34.32, description => <<"PAIEMENT PAR CARTE 20/07/2020 XXX">>, type => card_debit }
        ]).
-define(TRANSACTIONS_JSON, <<"{\"transactions\":[{\"type\":\"sepa_debit\",\"id\":\"TRANSACTION_2\",\"effective_date\":\"2020-07-22\",\"description\":\"PRLV SEPA XXX\",\"amount\":-14.32,\"accounting_date\":\"2020-07-22\"},{\"type\":\"card_debit\",\"id\":\"TRANSACTION_1\",\"effective_date\":\"2020-07-21\",\"description\":\"PAIEMENT PAR CARTE 20/07/2020 XXX\",\"amount\":-34.32,\"accounting_date\":\"2020-07-21\"}]}">>).

should_return_last_transactions(_Config) ->
  Req = #req{ method = 'GET', path = [<<"transactions">>] },
  meck:expect(banks_fetch_storage, get_last_transactions, fun(MockN) ->
                                                              10 = MockN,
                                                              {value, ?TRANSACTIONS}
                                                          end),
  {Status, Headers, Body} = banks_fetch_api:handle(Req, no_args),
  200 = Status,
  [] = Headers,
  ?TRANSACTIONS_JSON = Body,

  true = meck:validate(banks_fetch_storage),

  ok.

should_return_last_transactions_with_client(_Config) ->
  meck:expect(banks_fetch_storage, get_last_transactions, fun(MockN) ->
                                                              10 = MockN,
                                                              {value, ?TRANSACTIONS}
                                                          end),
 
  Response = hackney:get("http://localhost:3003/transactions"),
  200 = status(Response),
  ?TRANSACTIONS_JSON = body(Response),

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
