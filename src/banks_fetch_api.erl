-module(banks_fetch_api).
-export([
         handle/2,
         handle_event/3,

         handle_transactions/2
        ]).

-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

-define(MAX_TRANSACTIONS_RETURNED, 10).

-spec handle(elli:req(), elli_handler:callback_args()) -> elli_handler:result().
handle(Req, _Args) ->
  try
    handle(elli_request:method(Req), elli_request:path(Req), Req)
  catch
    C:V:StackTrace ->
      ok = lager:error("API Error for ~1000p\nException=~p\nStack=~p", [Req, {C,V}, StackTrace]),
      {500, [], <<"Internal error">>}
  end.

handle('GET',[<<"api">>, <<"1.0">>, <<"transactions">>], _Req) ->
  handle_transactions(none, ?MAX_TRANSACTIONS_RETURNED);
handle('GET',[<<"api">>, <<"1.0">>, <<"transactions">>, Cursor], _Req) ->
  handle_transactions(Cursor, ?MAX_TRANSACTIONS_RETURNED);

handle('GET',[<<"api">>, <<"1.0">>, <<"banks">>], _Req) ->
  handle_banks();

handle('GET',[<<"api">>, <<"1.0">>, <<"budgets">>], _Req) ->
  handle_budgets();

handle('GET',[<<"api">>, <<"1.0">>, <<"categories">>], _Req) ->
  handle_categories();

handle('GET',[<<"api">>, <<"1.0">>, <<"stores">>], _Req) ->
  handle_stores();

handle('GET',[<<"api">>, <<"1.0">>, <<"accounts">>], _Req) ->
  handle_accounts();

handle(_, _, _Req) ->
  {404, [], <<"Not Found">>}.


-spec handle_event(elli_handler:event(), elli_handler:callback_args(), [tuple()]) -> ok.
handle_event(_Event, _Data, _Args) ->
  ok.

%%
%% Functions to handle queries
%%

-spec handle_transactions(none | unicode:unicode_binary(), non_neg_integer()) -> elli_handler:result().
handle_transactions(CursorOpt, N) ->
  case banks_fetch_storage:get_last_transactions(CursorOpt, N) of
    {value, {NextCursor, Total, Transactions}} ->
      Transactions1 = [ to_json_transaction(Transaction) || Transaction <- Transactions ],
      JSON = jiffy:encode(#{ next_cursor => NextCursor, total => Total, transactions => Transactions1 }),
      {200, [{<<"Content-Type">>, <<"application/json">>}], JSON};
    {error, invalid_cursor} ->
      {400, [{<<"Content-Type">>, <<"text/plain">>}], <<"Invalid cursor">>}
  end.

-spec handle_banks() -> elli_handler:result().
handle_banks() ->
  {value, Banks} = banks_fetch_storage:get_banks(),
  JSON = jiffy:encode(Banks),
  {200, [{<<"Content-Type">>, <<"application/json">>}], JSON}.

-spec handle_accounts() -> elli_handler:result().
handle_accounts() ->
  {value, Accounts} = banks_fetch_storage:get_all_accounts(),
  JSON = jiffy:encode(Accounts),
  {200, [{<<"Content-Type">>, <<"application/json">>}], JSON}.

-spec handle_budgets() -> elli_handler:result().
handle_budgets() ->
  {value, Budgets} = banks_fetch_storage:get_budgets(),
  JSON = jiffy:encode(Budgets),
  {200, [{<<"Content-Type">>, <<"application/json">>}], JSON}.

-spec handle_categories() -> elli_handler:result().
handle_categories() ->
  {value, Categories0} = banks_fetch_storage:get_categories(),
  Categories1 = [ to_json_category(C) || C <- Categories0 ],
  JSON = jiffy:encode(Categories1),
  {200, [{<<"Content-Type">>, <<"application/json">>}], JSON}.

-spec handle_stores() -> elli_handler:result().
handle_stores() ->
  {value, Stores} = banks_fetch_storage:get_stores(),
  JSON = jiffy:encode(Stores),
  {200, [{<<"Content-Type">>, <<"application/json">>}], JSON}.


%% @doc Transform an internal transaction data to a json compatible transaction data (transform dates and protected ids)
-spec to_json_transaction(banks_fetch_bank:transaction()) -> map().
to_json_transaction(#{ bank_id := {bank_id, BankIdVal}, client_id := {client_id, ClientIdVal}, account_id := {account_id, AccountIdVal}, accounting_date := AccountingDate, effective_date := EffectiveDate } = Transaction) ->
  ExtDate = maps:get('ext_date', Transaction, undefined),
  ExtPeriod = maps:get('ext_period', Transaction, undefined),
  ExtBudgetId = maps:get('ext_budget_id', Transaction, undefined),
  ExtCategoriesId = maps:get('ext_categories_id', Transaction, undefined),
  ExtStoreId = maps:get('ext_store_id', Transaction, undefined),
  Transaction#{ bank_id := BankIdVal, client_id := ClientIdVal, account_id := AccountIdVal, accounting_date := fix_date(AccountingDate), effective_date := fix_date(EffectiveDate),
                ext_date => fix_date(ExtDate), ext_period => undefined_to_null(ExtPeriod), ext_budget_id => undefined_to_null(ExtBudgetId), ext_categories_id => undefined_to_null(ExtCategoriesId),
                ext_store_id => undefined_to_null(ExtStoreId) }.

undefined_to_null(undefined) -> null;
undefined_to_null(V) -> V.

%% @doc Transform an internal category data to a json compatible category data
-spec to_json_category(banks_fetch_bank:category()) -> map().
to_json_category(#{ up_category_id := none } = Category) ->
  Category#{ up_category_id := null };
to_json_category(Category) ->
  Category.


%% @doc Transform date to ISO8601 format
-spec fix_date(undefined) -> null; (calendar:date()) -> unicode:unicode_binary().
fix_date({Year,Month,Day}) ->
  iolist_to_binary(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B", [Year, Month, Day]));
fix_date(undefined) ->
  null.
