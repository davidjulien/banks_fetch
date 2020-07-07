%%%-------------------------------------------------------------------
%% @doc banks_fetch_bank defines expected behavior for modules fetching data from banks
%% @end
%%%-------------------------------------------------------------------

-module(banks_fetch_bank).

-export_type([
              bank_id/0,
              client_id/0,
              client_credential/1,
              account/0,
              transaction/0
             ]).

-type bank_auth() :: {bank_auth, BankModuleName :: atom(), Auth :: any()}.


%% Banks data datatypes
-type bank_id() :: {bank_id, unicode:unicode_binary()}.
-type client_id() :: {client_id, unicode:unicode_binary()}.
-type client_credential(A) :: {client_credential, A}.

-type account_type() :: current | savings | home_loan.
-type account_ownership() :: single | joint.
-type account() :: #{ id => unicode:unicode_binary(),
                      balance => float(),
                      number => unicode:unicode_binary(),
                      owner => unicode:unicode_binary(),
                      ownership => account_ownership(),
                      type => account_type(),
                      name => unicode:unicode_binary() }.

-type transaction_type() :: 'card_debit' | 'card_withdrawal' | 'check' | 'sepa_debit' | 'transfer'.
-type transaction() :: #{ id => unicode:unicode_binary(),
                          accounting_date => calendar:date(),
                          effective_date => calendar:date(),
                          amount => float(),
                          description => unicode:unicode_binary(),
                          type => transaction_type()
                        }.

%% Callbacks required

-callback connect(client_id(), client_credential(_)) -> {ok, bank_auth()}.
-callback fetch_accounts(bank_auth()) -> {unicode:unicode_binary(), [account()]}.
-callback fetch_transactions(bank_auth(), unicode:unicode_binary(), first_call | unicode:unicode_binary()) -> [banks_fetch_bank:transaction()].
