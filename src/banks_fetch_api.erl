-module(banks_fetch_api).
-export([
         handle/2,
         handle_event/3,
        
         handle_transactions/1
        ]).

-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

-define(MAX_TRANSACTIONS_RETURNED, 10).

-spec handle(elli:req(), elli_handler:callback_args()) -> elli_handler:result().
handle(Req, _Args) ->
  handle(elli_request:method(Req), elli_request:path(Req), Req).

handle('GET',[<<"api">>, <<"1.0">>, <<"transactions">>], _Req) ->
  handle_transactions(?MAX_TRANSACTIONS_RETURNED);

handle('GET',[<<"api">>, <<"1.0">>, <<"banks">>], _Req) ->
  handle_banks();

handle(_, _, _Req) ->
  {404, [], <<"Not Found">>}.


-spec handle_event(elli_handler:event(), elli_handler:callback_args(), [tuple()]) -> ok.
handle_event(_Event, _Data, _Args) ->
  ok.

%%
%% Functions to handle queries
%%

-spec handle_transactions(non_neg_integer()) -> elli_handler:result().
handle_transactions(N) ->
  ok = lager:info("[API] Fetch ~B transactions", [N]),
  {value, Transactions} = banks_fetch_storage:get_last_transactions(N),

  Transactions1 = [ to_json_transaction(Transaction) || Transaction <- Transactions ],
  JSON = jiffy:encode(#{ transactions => Transactions1 }),
  {200, [{<<"Content-Type">>, <<"application/json">>}], JSON}.

-spec handle_banks() -> elli_handler:result().
handle_banks() ->
  {value, Banks} = banks_fetch_storage:get_banks(),
  JSON = jiffy:encode(Banks),
  {200, [{<<"Content-Type">>, <<"application/json">>}], JSON}.


%% @doc Transform an internal transaction data to a json compatible transaction data (transform dates and protected ids)
-spec to_json_transaction(banks_fetch_bank:transaction()) -> map().
to_json_transaction(#{ bank_id := {bank_id, BankIdVal}, client_id := {client_id, ClientIdVal}, account_id := {account_id, AccountIdVal}, accounting_date := AccountingDate, effective_date := EffectiveDate } = Transaction) ->
  Transaction#{ bank_id := BankIdVal, client_id := ClientIdVal, account_id := AccountIdVal, accounting_date := fix_date(AccountingDate), effective_date := fix_date(EffectiveDate) }.

%% @doc Transform date to ISO8601 format
-spec fix_date(calendar:date()) -> unicode:unicode_binary().
fix_date({Year,Month,Day}) ->
  iolist_to_binary(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B", [Year, Month, Day])).
