%%%-------------------------------------------------------------------
%% @doc banks_fetch_bank_boursedirect : fetch data from Bourse Direct bank. Implement banks_fetch_bank behavior.
%% @end
%%%-------------------------------------------------------------------

-module(banks_fetch_bank_boursedirect).
-behaviour(banks_fetch_bank).

-export([
         setup/0,
         connect/2,
         fetch_accounts/1,
         fetch_transactions/3
        ]).

-define(USER_AGENT, "BourseDirect/1.1.0/Android 7.0/Moto G (4)").
-define(HEADERS, [{"User-Agent",?USER_AGENT}]).

-type boursedirect_bank_auth() :: {bank_auth, ?MODULE, none}.
-type boursedirect_client_credential() :: banks_fetch_bank:client_credential(unicode:unicode_binary()).

%%
%% @doc Setup. Registers prometheus counter to monitor connections to Bourse Direct server.
%%
-spec setup() -> ok.
setup() ->
  banks_fetch_http:setup_monitoring("api-mobile-v1.boursedirect.fr"),
  prometheus_counter:declare([{name, 'bank_boursedirect_connect_total_count'}, {help, "Bank Bourse Direct connect total count"}]),
  prometheus_counter:declare([{name, 'bank_boursedirect_connect_ok_count'}, {help, "Bank Bourse Direct connect ok count"}]),
  prometheus_counter:declare([{name, 'bank_boursedirect_connect_wrong_authentication_count'}, {help, "Bank Bourse Direct connect error 'wrong authentication' count"}]),
  prometheus_counter:declare([{name, 'bank_boursedirect_connect_network_error_count'}, {help, "Bank Bourse Direct connect error 'network error' count"}]),
  prometheus_counter:declare([{name, 'bank_boursedirect_accounts_total_count'}, {help, "Bank Bourse Direct accounts total count"}]),
  prometheus_counter:declare([{name, 'bank_boursedirect_accounts_ok_count'}, {help, "Bank Bourse Direct accounts ok count"}]),
  prometheus_counter:declare([{name, 'bank_boursedirect_transactions_total_count'}, {help, "Bank Bourse Direct transactions total count"}]),
  prometheus_counter:declare([{name, 'bank_boursedirect_transactions_ok_count'}, {help, "Bank Bourse Direct transactions ok count"}]),
  ok.

%%
%% @doc Connect to Bourse Direct API
%%
-spec connect(banks_fetch_bank:client_id(), boursedirect_client_credential()) -> {ok, boursedirect_bank_auth()} | {error, banks_fetch_bank:connection_error()}.
connect({client_id, ClientIdVal}, {client_credential, ClientPassword}) ->
  ok = banks_fetch_http:set_options([{cookies,enabled}]),

  prometheus_counter:inc('bank_boursedirect_connect_total_count'),

  case banks_fetch_http:request(post, {"https://api-mobile-v1.boursedirect.fr/api/authenticate", ?HEADERS, "application/x-www-form-urlencoded", "site=BDI&password="++binary_to_list(ClientPassword)++"&username="++binary_to_list(ClientIdVal)}, [{timeout,60000}], []) of
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
      Decoded = jsx:decode(list_to_binary(Body)),
      case lists:keyfind(<<"code">>, 1, Decoded) of
        {<<"code">>, 0} ->
          prometheus_counter:inc('bank_boursedirect_connect_ok_count'),
          {ok, {bank_auth, ?MODULE, none}};
        {<<"code">>, 1006} ->
          prometheus_counter:inc('bank_boursedirect_connect_wrong_authentication_count'),
          {error, invalid_credential}
      end;
    {error, failed_connect} = Err ->
      ok = lager:warning("~p/~s : network error : ~p", [?MODULE, ClientIdVal, Err]),
      prometheus_counter:inc('bank_boursedirect_connect_network_error_count'),
      {error, network_error}
  end.

%%
%% @doc Fetch accounts
%%
-spec fetch_accounts(boursedirect_bank_auth()) -> {ok, [banks_fetch_bank:account()]}.
fetch_accounts({bank_auth, ?MODULE, none}) ->
  prometheus_counter:inc('bank_boursedirect_accounts_total_count'),
  {_JSON, R} = request_json("https://api-mobile-v1.boursedirect.fr/api/dashboard/accounts"),
  prometheus_counter:inc('bank_boursedirect_accounts_ok_count'),
  {ok, [ account_transform(AccountInfo) || AccountInfo <- R ]}.

account_transform(#{ <<"numCompte">> := NumCompte, <<"libCompte">> := LibCompte, <<"index">> := Index, <<"total">> := #{ <<"valo">> := #{ <<"valeur">> := Val } } }) ->
  #{ id => integer_to_binary(Index), link => list_to_binary(["/accounts/",NumCompte]), balance => Val, number => NumCompte, owner => LibCompte, ownership => single, type => markets, name => <<"Stock market">> }.


%%
%% @doc Fetch transactions
%%
-spec fetch_transactions(boursedirect_bank_auth(), banks_fetch_bank:account_id(), first_call | banks_fetch_bank:transaction_id()) -> {ok, [banks_fetch_bank:transaction()]}.
fetch_transactions(AuthToken, AccountId, first_call) ->
  fetch_transactions(AuthToken, AccountId, {transaction_id, <<"">>});
fetch_transactions(_AuthToken, {account_id, AccountIdValue}, {transaction_id, LastKnownTransactionIdValue}) ->
  prometheus_counter:inc('bank_boursedirect_transactions_total_count'),
  {_JSON, R} = request_json("https://api-mobile-v1.boursedirect.fr/api/accounts/" ++ binary_to_list(AccountIdValue) ++ "/history"),
  prometheus_counter:inc('bank_boursedirect_transactions_ok_count'),

  case maps:get(<<"operation">>, R, undefined) of
    undefined -> {ok, []};
    Operations ->
      Transactions0 = [ transaction_transform(TransactionInfo) || TransactionInfo <- lists:reverse(Operations) ],
      {Transactions1, _Remaining} = lists:splitwith(fun(T) -> #{ id := TID } = T, TID =/= LastKnownTransactionIdValue end, Transactions0),
      {ok, Transactions1}
  end.

transaction_transform(#{ <<"libOperation">> := LibOperation, <<"operation">> := Operation, <<"dateAffectation">> := DateAffectation, <<"dateOperation">> := DateOperation, <<"montantNet">> := #{ <<"valeur">> := Valeur }, <<"categorie">> := Category }) ->
  ID = list_to_binary([DateOperation,"_",LibOperation]),
  Amount = if is_integer(Valeur) -> Valeur * 1.0; 
              true -> Valeur
           end,
  Description = if LibOperation =:= Operation -> LibOperation;
                   true -> list_to_binary([Operation, <<" : ">>, LibOperation])
                end,
  Type = case Category of
           99 -> transfer;
           1 -> stock
         end,
  EffectiveDate = iso8601_to_datetime(DateAffectation),
  AccountingDate = iso8601_to_datetime(DateOperation),
  #{ id => ID, effective_date => EffectiveDate, accounting_date => AccountingDate, amount => Amount, description => Description, type => Type }.

%%--------------------------------------------------------------------
%% @doc Convert a time in iso8601 format to erlang datetime().
%% All possible formats are not implemented.
-define(IS_DIGIT(X), (X =< $9 andalso X >= $0)).

-spec iso8601_to_datetime(unicode:unicode_binary() | string()) -> any().
iso8601_to_datetime(Value) when is_binary(Value) ->
    iso8601_to_datetime(binary_to_list(Value));
iso8601_to_datetime([Y1,Y2,Y3,Y4,$-,M1,M2,$-,D1,D2, $T | TimeStr])
        when ?IS_DIGIT(Y1), ?IS_DIGIT(Y2), ?IS_DIGIT(Y3), ?IS_DIGIT(Y4),
            ?IS_DIGIT(M1), ?IS_DIGIT(M2), ?IS_DIGIT(D1), ?IS_DIGIT(D2) ->
    Year = list_to_integer([Y1, Y2, Y3, Y4]),
    Month = list_to_integer([M1, M2]),
    Day = list_to_integer([D1, D2]),
    iso8601_to_datetime0({Year, Month, Day}, TimeStr).

iso8601_to_datetime0(Date, [H1,H2,$:,M1,M2,$:,S1,S2 | TZStr])
        when ?IS_DIGIT(H1), ?IS_DIGIT(H2), ?IS_DIGIT(M1), ?IS_DIGIT(M2),
            ?IS_DIGIT(S1), ?IS_DIGIT(S2) ->
    Hour = list_to_integer([H1, H2]),
    Minute = list_to_integer([M1, M2]),
    Second = list_to_integer([S1, S2]),
    iso8601_to_datetime1(Date, {Hour, Minute, Second}, TZStr).

iso8601_to_datetime1(Date, Time, [SignS | DeltaStr]) when SignS =:= $+ orelse SignS =:= $- ->
    {DeltaHS, DeltaMS} = case DeltaStr of
%        [HS0, HS1, $:, MS0, MS1] -> {[HS0, HS1], [MS0, MS1]};
        [HS0, HS1, MS0, MS1] -> {[HS0, HS1], [MS0, MS1]}
    end,
    {ok, [DeltaH, DeltaM], []} = io_lib:fread("~u:~u", DeltaHS ++ ":" ++ DeltaMS),
    DeltaVal = DeltaH * 60 + DeltaM,
    DeltaUTC = case SignS of
        $+ -> - DeltaVal
%        $- -> DeltaVal
    end,
    DateLocalGS = calendar:datetime_to_gregorian_seconds({Date, Time}),
    DateUTCGS = DateLocalGS + DeltaUTC * 60,
    calendar:gregorian_seconds_to_datetime(DateUTCGS).

%%
%% @doc Internal function to fetch data
%%
request_json(URL) ->
  {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = banks_fetch_http:request(get, {URL, ?HEADERS}, [], []),
  Bin = list_to_binary(Body),
  {Bin, jsx:decode(Bin,[return_maps])}.
