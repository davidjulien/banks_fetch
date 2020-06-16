%%%-------------------------------------------------------------------
%% @doc banks_fetch_bank defines expected behavior for modules fetching data from banks
%% @end
%%%-------------------------------------------------------------------

-module(banks_fetch_bank).

-export_type([
              account/0
             ]).

-type bank_auth() :: {bank_auth, BankModuleName :: atom(), Auth :: any()}.


%% Banks data datatypes
-type account_type() :: current | savings | home_loan.
-type account_ownership() :: single | joint.
-type account() :: #{ id => unicode:unicode_binary(),
                      balance => float(),
                      number => unicode:unicode_binary(),
                      owner => unicode:unicode_binary(),
                      ownership => account_ownership(),
                      type => account_type(),
                      name => unicode:unicode_binary() }.

%% Callbacks required

-callback connect(string(), any()) -> {ok, bank_auth()}.
-callback fetch_accounts(bank_auth()) -> {unicode:unicode_binary(), [account()]}.
