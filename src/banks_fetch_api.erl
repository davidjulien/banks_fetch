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

handle('GET',[<<"transactions">>], _Req) ->
  handle_transactions(?MAX_TRANSACTIONS_RETURNED);

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
  error_logger:info_msg("handle_transactions"),
  {value, Transactions} = banks_fetch_storage:get_last_transactions(N),
  Transactions1 = [ to_json_transaction(Transaction) || Transaction <- Transactions ],
  error_logger:info_msg("handle_transactions : ~p", [Transactions1]),
  JSON = jiffy:encode(#{ transactions => Transactions1 }),
  {200, [], JSON}.



%% @doc Transform an internal transaction data to a json compatible transaction data (transform dates)
-spec to_json_transaction(banks_fetch_bank:transaction()) -> map().
to_json_transaction(#{ accounting_date := AccountingDate, effective_date := EffectiveDate } = Transaction) ->
  Transaction#{ accounting_date := fix_date(AccountingDate), effective_date := fix_date(EffectiveDate) }.

%% @doc Transform date to ISO8601 format
-spec fix_date(calendar:date()) -> unicode:unicode_binary().
fix_date({Year,Month,Day}) ->
  iolist_to_binary(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B", [Year, Month, Day])).
