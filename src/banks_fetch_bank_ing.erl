%%%-------------------------------------------------------------------
%% @doc banks_fetch_bank_ing : fetch data from ING bank. Implement banks_fetch_bank behavior.
%% @end
%%%-------------------------------------------------------------------

-module(banks_fetch_bank_ing).
-behaviour(banks_fetch_bank).

-export([
         connect/2
        ]).

-define(USER_AGENT, "Mozilla/5.0 (Linux; Android 7.0; SM-A520F Build/NRD90M; wv) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/65.0.3325.109 Mobile Safari/537.36").
-define(HEADERS, [{"Origin", "https://m.ing.fr"},
                  {"Host", "m.ing.fr"},
                  {"Accept", "Accept: application/json,text/plain, */*"},
                  {"User-Agent", "Mozilla/5.0 (Linux; Android 7.0; SM-A520F Build/NRD90M; wv) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/65.0.3325.109 Mobile Safari/537.36"},
                  {"Ingdf-Originating-Device", "Android"},
                  {"Content-Type", "application/json;charset=UTF-8"}]).

-spec connect(unicode:unicode_binary(), {unicode:unicode_binary(), unicode:unicode_binary()}) -> {ok, {bank_auth, ?MODULE, unicode:unicode_binary()}}.
connect(ClientId, {ClientPassword, ClientBirthDate}) ->
  {ok, {{_Version0, 200, _ReasonPhrase0}, Headers0, _Body0}} = httpc:request(get, {"https://m.ing.fr/", ?HEADERS}, [{timeout,60000}], []),
  case lists:keyfind("ingdf-auth-token", 1, Headers0) of
    {_, AuthToken} -> {ok, AuthToken};
    false ->
      {ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, _Body1}} = httpc:request(post, {"https://m.ing.fr/secure/api-v1/login/cif", ?HEADERS, "application/json;charset=UTF-8", "{\"cif\":\""++ClientId++"\",\"birthDate\":\""++ClientBirthDate++"\"}"}, [{timeout,60000}], []),
      {ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(post, {"https://m.ing.fr/secure/api-v1/login/keypad", ?HEADERS, "application/json;charset=UTF-8", "{\"keyPadSize\":{\"width\":2840,\"height\":1136}}"}, [{timeout,60000}], []),

      JSON = jsx:decode(list_to_binary(Body2)),
      {_, KeypadURL} = lists:keyfind(<<"keyPadUrl">>, 1, JSON),
      {_, PinPositions} = lists:keyfind(<<"pinPositions">>, 1, JSON),

      FullKeypadURL = "https://m.ing.fr/secure/api-v1"++binary_to_list(KeypadURL),
      {ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(get, {FullKeypadURL, ?HEADERS, "image/png,image/svg+xml,image/*;q=0.8,*/*;q=0.5", ""}, [{timeout,60000}], []),

      ClickPositions = banks_fetch_bank_ing_keypad:resolve_keypad(list_to_binary(Body3), PinPositions, ClientPassword),
      ClickPositionsStr = binary_to_list(jsx:encode([{<<"clickPositions">>,ClickPositions}])),

      {ok, {{_Version4, 200, _ReasonPhrase4}, Headers4, _Body4}} = httpc:request(post, {"https://m.ing.fr/secure/api-v1/login/sca/pin", ?HEADERS, "application/json;charset=UTF-8", ClickPositionsStr}, [], []),
      {_, AuthToken} = lists:keyfind("ingdf-auth-token", 1, Headers4),

      {ok, AuthToken}
  end.
