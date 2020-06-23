-module(banks_fetch_storage).

-export([
         get_clients/0,
         store_accounts/3
        ]).

-spec get_clients() -> {value, []}.
get_clients() ->
  {value, []}.

store_accounts(_, _, _) ->
  ok.
