%%%-------------------------------------------------------------------
%% @doc banks_fetch_api provides an API to fetch data stored in banks_fetch database.
%% WARNING: access is not yet protected. Check that opened port (2020 by default) is not accessible from outside.
%%
%% API endpoints:
%% - /api/1.0/transactions : return last 10 transactions stored, provides a cursor to fetch next transactions
%% - /api/1.0/transactions/cursor={CURSOR} : return last 10 transactions stored after {CURSOR}
%% - /api/1.0/transactions/{BANKID}/{CLIENTID}/{ACCOUNTID}/{TRANSACTIONID} (PATCH) : patch a transaction
%% - /api/1.0/transactions/{BANKID}/{CLIENTID}/{ACCOUNTID}/{TRANSACTIONID}/split (POST) : split a transaction
%% - /api/1.0/transactions/{BANKID}/{CLIENTID}/{ACCOUNTID}/{TRANSACTIONID}/copy_to_purse (POST) : copy a transaction to purse
%% - /api/1.0/banks : return all banks
%% - /api/1.0/budgets : return all budgets
%% - /api/1.0/categories : return all categories
%% - /api/1.0/stores : return all stores
%% - /api/1.0/stores/new (POST) : add a new store, returns a store object
%% - /api/1.0/accounts : return all user accounts
%% - /api/1.0/mappings/new (POST) : add a new mapping, returns a mapping object
%%
%% API returns 400 in case of invalid parameters.
%% API returns 404 for unknown endpoints.
%% API returns 500 in case of internal error (exception triggered).
%%
%% Data are returned in JSON format.
%% @end
%%%-------------------------------------------------------------------
-module(banks_fetch_api).
-export([
         handle/2,
         handle_event/3,

         handle_transactions/2
        ]).

-ifdef(TEST).
-export([
         join_strings/2,
         verify_type/2
        ]).
-endif.


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

handle('GET',[<<"api">>, <<"1.0">>, <<"transactions">>], Req) ->
  Cursor = elli_request:get_arg(<<"cursor">>, Req, none),
  handle_transactions(Cursor, ?MAX_TRANSACTIONS_RETURNED);

handle(<<"PATCH">>, [<<"api">>, <<"1.0">>, <<"transactions">>, BankIdStr, ClientIdStr, AccountIdStr, TransactionIdStr], Req) ->
  Body = elli_request:body(Req),
  handle_transactions_update(BankIdStr, ClientIdStr, AccountIdStr, TransactionIdStr, Body);

handle('POST', [<<"api">>, <<"1.0">>, <<"transactions">>, BankIdStr, ClientIdStr, AccountIdStr, TransactionIdStr, <<"split">>], _Req) ->
  handle_transactions_split(BankIdStr, ClientIdStr, AccountIdStr, TransactionIdStr);

handle('POST', [<<"api">>, <<"1.0">>, <<"transactions">>, BankIdStr, ClientIdStr, AccountIdStr, TransactionIdStr, <<"copy_to_purse">>], _Req) ->
  handle_transactions_copy_to_purse(BankIdStr, ClientIdStr, AccountIdStr, TransactionIdStr);

handle('GET',[<<"api">>, <<"1.0">>, <<"banks">>], _Req) ->
  handle_banks();

handle('GET',[<<"api">>, <<"1.0">>, <<"budgets">>], _Req) ->
  handle_budgets();

handle('GET',[<<"api">>, <<"1.0">>, <<"categories">>], _Req) ->
  handle_categories();

handle('GET',[<<"api">>, <<"1.0">>, <<"stores">>], _Req) ->
  handle_stores();

handle('POST',[<<"api">>, <<"1.0">>, <<"stores">>, <<"new">>], Req) ->
  StoreName = elli_request:body(Req),
  handle_stores_new(StoreName);

handle('GET',[<<"api">>, <<"1.0">>, <<"accounts">>], _Req) ->
  handle_accounts();

handle('POST',[<<"api">>, <<"1.0">>, <<"mappings">>, <<"new">>], Req) ->
  Body = elli_request:body(Req),
  handle_mappings_new(Body);

handle(Method, URL, _Req) ->
  error_logger:info_msg("Method=~p, URL=~p, _Req=~p", [Method, URL, _Req]),
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

-spec handle_transactions_update(unicode:unicode_binary(), unicode:unicode_binary(), unicode:unicode_binary(), unicode:unicode_binary(), unicode:unicode_binary()) -> elli_handler:result().
handle_transactions_update(BankIdVal, ClientIdVal, AccountIdVal, TransactionIdVal, Body) ->
  JSON = jsx:decode(Body),
  {_, DateStr} = lists:keyfind(<<"ext_date">>, 1, JSON),
  {_, StoreId} = lists:keyfind(<<"ext_store_id">>, 1, JSON),
  {_, BudgetId} = lists:keyfind(<<"ext_budget_id">>, 1, JSON),
  {_, CategoriesIds} = lists:keyfind(<<"ext_categories_ids">>, 1, JSON),
  {_, PeriodStr} = lists:keyfind(<<"ext_period">>, 1, JSON),
  Amount1 = case lists:keyfind(<<"amount">>, 1, JSON) of
             false -> null;
             {_, Amount0} -> Amount0
           end,
  Date = date_to_iso8601(DateStr),
  case verify_types([{<<"ext_date">>, Date, {optional, date}}, {<<"ext_store_id">>, StoreId, {optional, integer}}, {<<"ext_budget_id">>, BudgetId, {optional, integer}},
                     {<<"ext_categories_ids">>, CategoriesIds, {optional, {array, integer}}}, {<<"ext_period">>, PeriodStr, {optional, period}}, {<<"amount">>, Amount1, {optional, float}}]) of
    [] ->
      Period = case PeriodStr of
                 null -> undefined;
                 _ -> binary_to_atom(PeriodStr, 'utf8')
               end,
      Amount = case Amount1 of
                 null -> undefined;
                 _ -> Amount1 * 1.0 %% Ensure float
               end,
      case banks_fetch_storage:update_transaction({bank_id, BankIdVal}, {client_id, ClientIdVal}, {account_id, AccountIdVal}, {transaction_id, TransactionIdVal},
                                                  null_to_undefined(Date), null_to_undefined(Period), null_to_undefined(StoreId), null_to_undefined(BudgetId), null_to_undefined(CategoriesIds),
                                                  null_to_undefined(Amount)) of
        {ok, Transaction} ->
          ResultJSON = jsx:encode(to_json_transaction(Transaction)),
          {200, [{<<"Content-Type">>, <<"application/json">>}], ResultJSON};
        {error, R} ->
          ok = lager:warning("Unable to update transaction ~s/~s/~s/~s: ~p", [BankIdVal, ClientIdVal, AccountIdVal, TransactionIdVal, R]),
          {400, [{<<"Content-Type">>, <<"text/plain">>}], <<"Unable to update">>}
      end;
    InvalidFields ->
      ok = lager:warning("Invalid parameters: ~p", [InvalidFields]),
      {400, [{<<"Content-Type">>, <<"text/plain">>}], list_to_binary([<<"Invalid parameters: ">>, join_strings(InvalidFields, <<", ">>)]) }
  end.

-spec handle_transactions_split(unicode:unicode_binary(), unicode:unicode_binary(), unicode:unicode_binary(), unicode:unicode_binary()) -> elli_handler:result().
handle_transactions_split(BankIdVal, ClientIdVal, AccountIdVal, TransactionIdVal) ->
  case banks_fetch_storage:split_transaction({bank_id, BankIdVal}, {client_id, ClientIdVal}, {account_id, AccountIdVal}, {transaction_id, TransactionIdVal}) of
    {ok, Transactions} ->
      ResultJSON = jsx:encode([ to_json_transaction(Transaction) || Transaction <- Transactions ]),
      {200, [{<<"Content-Type">>, <<"application/json">>}], ResultJSON};
    {error, _R} ->
      ok = lager:warning("Unable to split transaction ~s/~s/~s/~s", [BankIdVal, ClientIdVal, AccountIdVal, TransactionIdVal]),
      {400, [{<<"Content-Type">>, <<"text/plain">>}], <<"Unable to split">>}
  end.

-spec handle_transactions_copy_to_purse(unicode:unicode_binary(), unicode:unicode_binary(), unicode:unicode_binary(), unicode:unicode_binary()) -> elli_handler:result().
handle_transactions_copy_to_purse(BankIdVal, ClientIdVal, AccountIdVal, TransactionIdVal) ->
  case banks_fetch_storage:copy_withdrawal_transaction_to_purse({bank_id, BankIdVal}, {client_id, ClientIdVal}, {account_id, AccountIdVal}, {transaction_id, TransactionIdVal}) of
    {ok, Transactions} ->
      ResultJSON = jsx:encode([ to_json_transaction(Transaction) || Transaction <- Transactions ]),
      {200, [{<<"Content-Type">>, <<"application/json">>}], ResultJSON};
    {error, R} ->
      ok = lager:warning("Unable to copy transaction ~s/~s/~s/~s: ~1000p", [BankIdVal, ClientIdVal, AccountIdVal, TransactionIdVal, R]),
      {400, [{<<"Content-Type">>, <<"text/plain">>}], <<"Unable to copy transaction to purse">>}
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

-spec handle_stores_new(unicode:unicode_binary()) -> elli_handler:result().
handle_stores_new(StoreName) ->
  case banks_fetch_storage:insert_store(StoreName) of
    {ok, Store} ->
      JSON = jiffy:encode(Store),
      {200, [{<<"Content-Type">>, <<"application/json">>}], JSON};
    {error, already_inserted} ->
      {400, [{<<"Content-Type">>, <<"application/json">>}], <<"Store already inserted">>}
  end.

-spec handle_mappings_new(unicode:unicode_binary()) -> elli_handler:result().
handle_mappings_new(Body) ->
  JSON = jsx:decode(Body),
  {_, Pattern} = lists:keyfind(<<"pattern">>, 1, JSON),
  {_, StoreId} = lists:keyfind(<<"store_id">>, 1, JSON),
  {_, BudgetId} = lists:keyfind(<<"budget_id">>, 1, JSON),
  {_, CategoriesIds} = lists:keyfind(<<"categories_ids">>, 1, JSON),
  {_, FixDateStr} = lists:keyfind(<<"fix_date">>, 1, JSON),
  {_, PeriodStr} = lists:keyfind(<<"period">>, 1, JSON),
  case verify_types([{<<"pattern">>, Pattern, string}, {<<"store_id">>, StoreId, {optional, integer}}, {<<"budget_id">>, BudgetId, {optional, integer}},
                     {<<"categories_ids">>, CategoriesIds, {optional, {array, integer}}}, {<<"period">>, PeriodStr, period}, {<<"fix_date">>, FixDateStr, fix_date}]) of
    [] ->
      Period = binary_to_atom(PeriodStr, 'utf8'),
      FixDate = binary_to_atom(FixDateStr,  'utf8'),
      case banks_fetch_storage:insert_mapping(Pattern, null_to_none(BudgetId), null_to_none(CategoriesIds), null_to_none(StoreId), FixDate, Period) of
        {ok, Mapping} ->
          ok = banks_fetch_storage:apply_mappings(),
          Result = jiffy:encode(to_json_mapping(Mapping)),
          {200, [{<<"Content-Type">>, <<"application/json">>}], Result};
        {error, already_inserted} ->
          {400, [{<<"Content-Type">>, <<"application/json">>}], <<"Mapping already inserted">>}
      end;
    InvalidFields ->
      ok = lager:warning("Invalid parameters: ~p", [InvalidFields]),
      {400, [{<<"Content-Type">>, <<"text/plain">>}], list_to_binary([<<"Invalid parameters: ">>, join_strings(InvalidFields, <<", ">>)]) }
  end.


%% @doc Transform an internal transaction data to a json compatible transaction data (transform dates and protected ids)
-spec to_json_transaction(banks_fetch_bank:transaction()) -> map().
to_json_transaction(#{ bank_id := {bank_id, BankIdVal}, client_id := {client_id, ClientIdVal}, account_id := {account_id, AccountIdVal}, accounting_date := AccountingDate, effective_date := EffectiveDate } = Transaction) ->
  ExtMappingId = maps:get('ext_mapping_id', Transaction, undefined),
  ExtDate = maps:get('ext_date', Transaction, undefined),
  ExtPeriod = maps:get('ext_period', Transaction, undefined),
  ExtBudgetId = maps:get('ext_budget_id', Transaction, undefined),
  ExtCategoriesIds = maps:get('ext_categories_ids', Transaction, undefined),
  ExtStoreId = maps:get('ext_store_id', Transaction, undefined),
  ExtSplitOfId = case maps:get('ext_split_of_id', Transaction) of
                   {transaction_id, MainTransactionId} -> MainTransactionId;
                   _ -> null
                 end,
  ExtToPurse = maps:get('ext_to_purse', Transaction, undefined),
  Transaction#{ bank_id := BankIdVal, client_id := ClientIdVal, account_id := AccountIdVal, accounting_date := fix_date(AccountingDate), effective_date := fix_date(EffectiveDate),
                ext_date => fix_date(ExtDate), ext_period => undefined_to_null(ExtPeriod), ext_budget_id => undefined_to_null(ExtBudgetId), ext_categories_ids => undefined_to_null(ExtCategoriesIds),
                ext_mapping_id => undefined_to_null(ExtMappingId), ext_split_of_id => ExtSplitOfId, ext_store_id => undefined_to_null(ExtStoreId), ext_to_purse => undefined_to_null(ExtToPurse) }.

%% @doc Transform an internal mapping data to a json compatible mapping data (none to null)
to_json_mapping(Mapping) ->
  BudgetId  = maps:get(budget_id, Mapping),
  StoreId  = maps:get(store_id, Mapping),
  CategoriesIds  = maps:get(categories_ids, Mapping),
  Mapping#{ budget_id => none_to_null(BudgetId), store_id => none_to_null(StoreId), categories_ids => none_to_null(CategoriesIds) }.


undefined_to_null(undefined) -> null;
undefined_to_null(V) -> V.

null_to_undefined(null) -> undefined;
null_to_undefined(V) -> V.

null_to_none(null) -> none;
null_to_none(V) -> V.

none_to_null(none) -> null;
none_to_null(V) -> V.

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

date_to_iso8601(null) ->
  undefined;
date_to_iso8601(DateStr) ->
  case re:run(DateStr, <<"([0-9]{4})-([0-9]{2})-([0-9]{2})">>, [{capture,all_but_first,list}]) of
    nomatch -> invalid_format;
    {match, [YearStr, MonthStr, DayStr]} ->
      {list_to_integer(YearStr), list_to_integer(MonthStr), list_to_integer(DayStr)}
  end.

%% @doc Verify types of a list of parameters and returned list of invalid ones
verify_types(List) ->
  Filtered = lists:filter(fun({_Name, Value, Type}) -> not verify_type(Value, Type) end, List),
  [ Name || {Name, _, _} <- Filtered ].

verify_type(V, string) -> is_binary(V);
verify_type(V, float) -> is_float(V) orelse is_integer(V);
verify_type(V, integer) -> is_integer(V);
verify_type(undefined, {optional, _T}) -> true;
verify_type(null, {optional, _T}) -> true;
verify_type(V, {optional, T}) -> verify_type(V, T);
verify_type(V, {array, T}) ->
  case is_list(V) of
    true -> lists:all(fun(SV) -> verify_type(SV, T) end, V);
    false -> false
  end;
verify_type({_,_,_} = D, date) -> calendar:valid_date(D);
verify_type(_, date) -> false;

verify_type(<<"month">>, period) -> true;
verify_type(<<"bimester">>, period) -> true;
verify_type(<<"quarter">>, period) -> true;
verify_type(<<"semester">>, period) -> true;
verify_type(<<"annual">>, period) -> true;
verify_type(_, period) -> false;

verify_type(<<"previous2">>, fix_date) -> true;
verify_type(<<"previous">>, fix_date) -> true;
verify_type(<<"previous_if_begin">>, fix_date) -> true;
verify_type(<<"none">>, fix_date) -> true;
verify_type(<<"next">>, fix_date) -> true;
verify_type(<<"next_if_end">>, fix_date) -> true;
verify_type(_, fix_date) -> false.

-spec join_strings([unicode:unicode_binary()], unicode:unicode_binary()) -> unicode:unicode_binary().
join_strings([E], _Sep) -> E;
join_strings([E|L], Sep) ->
  L1 = [ [Sep, Item] || Item <- L ],
  list_to_binary([ E | L1 ]).
