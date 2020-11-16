%%%-------------------------------------------------------------------
%% @doc banks_fetch_bank defines expected behavior for modules fetching data from banks
%% and defines common datatypes.
%% @end
%%%-------------------------------------------------------------------

-module(banks_fetch_bank).

-export([
         setup/0
        ]).

%% Callback functions
-export([
         setup/1,
         connect/3,
         fetch_accounts/2,
         fetch_transactions/4
        ]).

-export_type([
              connection_error/0,
              bank_id/0,
              client_id/0,
              client_credential/1,
              account_id/0,
              transaction_id/0,
              bank/0,
              budget/0,
              category/0,
              store/0,
              mapping/0,
              mapping_fix_date/0,
              mapping_period/0,
              account/0,
              transaction/0
             ]).

-type bank_auth() :: {bank_auth, BankModuleName :: atom(), Auth :: any()}.


%% Banks data datatypes
-type bank_id() :: {bank_id, unicode:unicode_binary()}.
-type client_id() :: {client_id, unicode:unicode_binary()}.
-type client_credential(A) :: {client_credential, A}.
-type account_id() :: {account_id, unicode:unicode_binary()}.
-type transaction_id() :: {transaction_id, unicode:unicode_binary()}.

-type bank() :: #{
        id := unicode:unicode_binary(),
        name := unicode:unicode_binary()
       }.

-type budget() :: #{
        id := unicode:unicode_binary(),
        name := unicode:unicode_binary()
       }.

-type category() :: #{
        id := non_neg_integer(),
        name := unicode:unicode_binary(),
        up_category_id := non_neg_integer() | none
       }.

-type store() :: #{
        id := non_neg_integer(),
        name := unicode:unicode_binary()
       }.

-type mapping_fix_date() :: previous2 | previous | previous_if_begin | none | next | next_if_end.
-type mapping_period() :: month | bimester | quarter | semester | annual.

-type mapping() :: #{
        id := non_neg_integer(),
        pattern := unicode:unicode_binary(),
        fix_date := mapping_fix_date(),
        period := mapping_period(),
        budget_id := none | non_neg_integer(),
        categories_id := none | [non_neg_integer()],
        store_id := none | non_neg_integer()
       }.

-type account_type() :: current | savings | home_loan.
-type account_ownership() :: single | joint.
-type account() :: #{ id => unicode:unicode_binary(),
                      balance => float(),
                      number => unicode:unicode_binary(),
                      owner => unicode:unicode_binary(),
                      ownership => account_ownership(),
                      type => account_type(),
                      name => unicode:unicode_binary(),

                      bank_id => bank_id(),
                      client_id => client_id()
                    }.

-type transaction_type() :: 'card_debit' | 'card_withdrawal' | 'check' | 'sepa_debit' | 'transfer' | 'interests' | 'bank_fees' | 'other'.
-type transaction_period() :: 'month' | 'bimester' | 'quarter' | 'semester' | 'annual'.
-type transaction() :: #{
        id := unicode:unicode_binary(),
        accounting_date := calendar:date(),
        effective_date := calendar:date(),
        amount := float(),
        description := unicode:unicode_binary(),
        type := transaction_type(),

        bank_id => bank_id(),
        client_id => client_id(),
        account_id => account_id(),

        ext_date => undefined | calendar:date(),
        ext_period => undefined | transaction_period(),
        ext_budget_id => undefined | non_neg_integer(),
        ext_categories_id => undefined | [non_neg_integer()],
        ext_store_id => undefined | non_neg_integer()
       }.

-type connection_error() :: invalid_credential | account_locked | internal_error | network_error.

%% Callbacks required by all modules implementing banks_fetch_bank behavior

-callback setup() -> ok.
-callback connect(ClientId :: client_id(), ClientCredential :: client_credential(_)) -> {ok, BankAuth :: bank_auth()} | {error, connection_error()}.
-callback fetch_accounts(BankAuth :: bank_auth()) -> {ok, Accounts :: [account()]}.
-callback fetch_transactions(BankAuth :: bank_auth(), AcountId :: account_id(), FirstCallOrLastFetchedTransactionId :: first_call | transaction_id()) -> {ok, Transactions :: [banks_fetch_bank:transaction()]}.

-spec setup(BankModule :: module()) -> ok.
setup(BankModule) ->
  BankModule:setup().

-spec connect(BankModule :: module(), ClientId :: client_id(), ClientCredential :: client_credential(_)) -> {ok, BankAuth :: bank_auth()} | {error, connection_error()}.
connect(BankModule, ClientId, ClientCredential) ->
  BankModule:connect(ClientId, ClientCredential).

-spec fetch_accounts(BankModule :: module(), BankAuth :: bank_auth()) -> {ok, Accounts :: [account()]}.
fetch_accounts(BankModule, BankAuth) ->
  BankModule:fetch_accounts(BankAuth).

-spec fetch_transactions(BankModule :: module(), BankAuth :: bank_auth(), AccountId :: account_id(), FirstCallOrLastFetchedTransactionId :: first_call | transaction_id()) ->
  {ok, Transactions :: [banks_fetch_bank:transaction()]}.
fetch_transactions(Module, BankAuth, AccountId, FirstCallOrLastFetchedTransactionId) ->
  Module:fetch_transactions(BankAuth, AccountId, FirstCallOrLastFetchedTransactionId).

%%
%% Common functions for all banks modules
%%

% Setup all banks modules
-spec setup() -> ok.
setup() ->
  banks_fetch_bank_ing:setup().
