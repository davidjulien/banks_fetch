%%%-------------------------------------------------------------------
%% @doc banks_fetch_bank defines expected behavior for modules fetching data from banks
%% @end
%%%-------------------------------------------------------------------

-module(banks_fetch_bank).

-type bank_auth() :: {bank_auth, BankModuleName :: atom(), Auth :: any()}.

%% Callbacks required

-callback connect(unicode:unicode_binary(), any()) -> {ok, bank_auth()}.
