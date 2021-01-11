%%%-------------------------------------------------------------------
%% @doc banks_fetch_bank_purse: fake bank to simulate purses containing cash (withdraws from accounts, cash received or given).
%% Implement banks_fetch_bank behavior.
%% Credential contains a list of bank/client/account where purse will collect all cash withdrawals.
%% @end
%%%-------------------------------------------------------------------

-module(banks_fetch_bank_purse).
-behaviour(banks_fetch_bank).

-export([
         setup/0,
         connect/2,
         fetch_accounts/1,
         fetch_transactions/3
        ]).

-type purse_sources() :: [{banks_fetch_bank:bank_id(), banks_fetch_bank:client_id(), banks_fetch_bank:account_id()}].
-type purse_bank_auth() :: {bank_auth, ?MODULE, {purse, calendar:datetime(), banks_fetch_bank:client_id(), calendar:date(), purse_sources()}}.
-type purse_client_credential() :: banks_fetch_bank:client_credential({calendar:date(), purse_sources()}).

%%
%% @doc Setup. Nothing to do.
%%
-spec setup() -> ok.
setup() ->
  ok.

%%
%% @doc Connect. Return an auth containing current date and withdrawals sources.
%%
-spec connect(banks_fetch_bank:client_id(), purse_client_credential()) -> {ok, purse_bank_auth()}.
connect(ClientId, {client_credential, {StartDate, SourcesList}}) ->
  CurrentDate = calendar:universal_time(),
  {ok, {bank_auth, ?MODULE, {purse, CurrentDate, ClientId, StartDate, SourcesList}}}.

%%
%% @doc Fetch accounts. Build acccount info from aggregation of amounts.
%%
-spec fetch_accounts(purse_bank_auth()) -> {ok, [banks_fetch_bank:account()]}.
fetch_accounts({bank_auth, ?MODULE, {purse, CurrentDate, {client_id, ClientIdVal} = PurseId, StartDate, SourcesList}}) ->
  {value, Balance} = banks_fetch_storage:aggregate_amounts_for_purse(StartDate, CurrentDate, PurseId, SourcesList),
  AccountInfo = #{ id => ClientIdVal, balance => Balance, number => <<>>, owner => ClientIdVal, ownership => single, type => purse, name => <<"Purse">> },
  {ok, [ AccountInfo ]}.

%%
%% @doc Fetch transactions. These transactions will be added into purse account.
%%
-spec fetch_transactions(purse_bank_auth(), banks_fetch_bank:account_id(), first_call | banks_fetch_bank:transaction_id()) -> {ok, [banks_fetch_bank:transaction()]}.
fetch_transactions({bank_auth, ?MODULE, {purse, CurrentDate, PurseId, StartDate, SourcesList}}, _PurseAccountId, _LastTransactionId) ->
  {value, Transactions} = banks_fetch_storage:get_new_transactions_for_purse(StartDate, CurrentDate, PurseId, SourcesList),
  {ok, Transactions}.
