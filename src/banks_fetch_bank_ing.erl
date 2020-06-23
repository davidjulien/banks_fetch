%%%-------------------------------------------------------------------
%% @doc banks_fetch_bank_ing : fetch data from ING bank. Implement banks_fetch_bank behavior.
%% @end
%%%-------------------------------------------------------------------

-module(banks_fetch_bank_ing).
-behaviour(banks_fetch_bank).

-export([
         connect/2,
         fetch_accounts/1,
         fetch_transactions/3
        ]).

-define(USER_AGENT, "Mozilla/5.0 (Linux; Android 7.0; SM-A520F Build/NRD90M; wv) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/65.0.3325.109 Mobile Safari/537.36").
-define(HEADERS, [{"Origin", "https://m.ing.fr"},
                  {"Host", "m.ing.fr"},
                  {"Accept", "Accept: application/json,text/plain, */*"},
                  {"User-Agent", "Mozilla/5.0 (Linux; Android 7.0; SM-A520F Build/NRD90M; wv) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/65.0.3325.109 Mobile Safari/537.36"},
                  {"Ingdf-Originating-Device", "Android"},
                  {"Content-Type", "application/json;charset=UTF-8"}]).

-type ing_bank_auth() :: {bank_auth, ?MODULE, string()}.

-spec connect(banks_fetch_bank:client_id(), {client_credential, {string(), string()}}) -> {ok, ing_bank_auth()}.
connect({client_id, ClientId}, {client_credential, {ClientPassword, ClientBirthDate}}) ->
  {ok, {{_Version0, 200, _ReasonPhrase0}, Headers0, _Body0}} = httpc:request(get, {"https://m.ing.fr/", ?HEADERS}, [{timeout,60000}], []),
  case lists:keyfind("ingdf-auth-token", 1, Headers0) of
    {_, AuthToken} -> {ok, {bank_auth, ?MODULE, AuthToken}};
    false ->
      case httpc:request(post, {"https://m.ing.fr/secure/api-v1/login/cif", ?HEADERS, "application/json;charset=UTF-8", "{\"cif\":\""++ClientId++"\",\"birthDate\":\""++ClientBirthDate++"\"}"}, [{timeout,60000}], []) of
        {ok,{{"HTTP/1.1",412,"Precondition Failed"}, _Headers1, BodyError}} ->
          decode_body_error(BodyError);
        {ok, {{_Version1, 200, _ReasonPhrase1}, _Headers1, _Body1}} ->
          {ok, {{_Version2, 200, _ReasonPhrase2}, _Headers2, Body2}} = httpc:request(post, {"https://m.ing.fr/secure/api-v1/login/keypad", ?HEADERS, "application/json;charset=UTF-8", "{\"keyPadSize\":{\"width\":2840,\"height\":1136}}"}, [{timeout,60000}], []),

          JSON = jsx:decode(list_to_binary(Body2)),
          {_, KeypadURL} = lists:keyfind(<<"keyPadUrl">>, 1, JSON),
          {_, PinPositions} = lists:keyfind(<<"pinPositions">>, 1, JSON),

          FullKeypadURL = "https://m.ing.fr/secure/api-v1"++binary_to_list(KeypadURL),
          {ok, {{_Version3, 200, _ReasonPhrase3}, _Headers3, Body3}} = httpc:request(get, {FullKeypadURL, lists:keyreplace("Accept", 1, ?HEADERS, {"Accept", "image/png,image/svg+xml,image/*;q=0.8,*/*;q=0.5"})}, [{timeout,60000}], []),

          ClickPositions = banks_fetch_bank_ing_keypad:resolve_keypad(list_to_binary(Body3), PinPositions, ClientPassword),
          ClickPositionsStr = binary_to_list(jsx:encode([{<<"clickPositions">>,ClickPositions}])),

          case httpc:request(post, {"https://m.ing.fr/secure/api-v1/login/sca/pin", ?HEADERS, "application/json;charset=UTF-8", ClickPositionsStr}, [], []) of
            {ok,{{"HTTP/1.1",412,"Precondition Failed"}, _Headers4, BodyError}} ->
              decode_body_error(BodyError);
            {ok, {{_Version4, 200, _ReasonPhrase4}, Headers4, _Body4}} ->
              {_, AuthToken} = lists:keyfind("ingdf-auth-token", 1, Headers4),
              {ok, {bank_auth, ?MODULE, AuthToken}}
          end
      end
  end.

%% @doc internal function to decode error message in body
decode_body_error(BodyError) ->
  JSON = jsx:decode(list_to_binary(BodyError)),
  {_, Error} = lists:keyfind(<<"error">>, 1, JSON),
  {_, Code} = lists:keyfind(<<"code">>, 1, Error),
  case Code of
    <<"AUTHENTICATION.INVALID_CIF_AND_BIRTHDATE_COMBINATION">> -> {error, invalid_birthdate};
    <<"SCA.WRONG_AUTHENTICATION">> -> {error, invalid_password}
  end.

%%
%% @doc Fetch accounts
%%
-spec fetch_accounts(ing_bank_auth()) -> {unicode:unicode_binary(), [banks_fetch_bank:account()]}.
fetch_accounts({bank_auth, ?MODULE, AuthToken}) ->
  {JSON, R} = request_json(AuthToken, "https://m.ing.fr/secure/api-v1/accounts"),
  #{ <<"accounts">> := AccountInfoList} = R,
  {JSON, [ account_transform(AccountInfo) || AccountInfo <- AccountInfoList ]}.

account_transform(#{ <<"uid">> := UID, <<"ledgerBalance">> := Balance, <<"label">> := Label, <<"owner">> := Owner, <<"type">> := #{ <<"code">> := Code, <<"label">> := TypeLabel },
                  <<"ownership">> := #{<<"code">> := OwnershipCode } }) ->
  AccountType = case Code of
                  <<"CA">> -> current;
                  <<"LDD">> -> savings;
                  <<"HOME_LOAN_PURCHASE">> -> home_loan
                end,
  Ownership = case OwnershipCode of
                <<"JOINT">> -> joint;
                <<"JOINT_WITH_SPOUSE">> -> joint;
                <<"SINGLE">> -> single
              end,
  #{ id => UID, link => list_to_binary(["/accounts/",UID]), balance => Balance, number => Label, owner => Owner, ownership => Ownership, type => AccountType, name => TypeLabel }.


%%
%% @doc Fetch transactions
%%
-spec fetch_transactions(ing_bank_auth(), unicode:unicode_binary(), first_call | unicode:unicode_binary()) -> [banks_fetch_bank:transaction()].
fetch_transactions(AuthToken, AccountId, first_call) ->
  fetch_transactions(AuthToken, AccountId, <<"0">>);
fetch_transactions({bank_auth, ?MODULE, AuthToken}, AccountId, LastKnownTransactionId) ->
  Transactions0 = transactions(AuthToken, AccountId, LastKnownTransactionId, <<"0">>, []),
  Transactions1 = [ transaction_transform(T) || T <- Transactions0 ],
  Transactions1.

transactions(AuthToken, AccountId, LastKnownTransactionId, NextCallId, AccTransactions) ->
    URL = "https://m.ing.fr/secure/api-v1/accounts/"++binary_to_list(AccountId)++"/transactions/after/"++binary_to_list(NextCallId)++"/limit/50",
    case request_json(AuthToken, URL) of
      {_JSON, []} -> AccTransactions;
      {_JSON, Transactions0} ->
        {Transactions1, Remaining} = lists:splitwith(fun(T) -> #{ <<"id">> := TID } = T, TID =/= LastKnownTransactionId end, Transactions0),
        if Remaining =/= [] ->
             AccTransactions++Transactions1;
           true ->
             NbrTransactions = length(Transactions0),

             % Reverse engineering of ING API shows that API uses second to last transaction to fetch next transactions.
             % We don't test if we have less than 50 transactions to stop because APIs are not always reliable
             NextCallTransaction = if NbrTransactions < 2 -> lists:last(Transactions0);                 % last transaction
                                      true -> lists:nth(length(Transactions0)-1,Transactions0)          % second to last transaction
                                   end,
             #{ <<"id">> := NewNextCallId } = NextCallTransaction,
             transactions(AuthToken, AccountId, LastKnownTransactionId, NewNextCallId, AccTransactions++Transactions0)
        end
    end.

transaction_transform(#{ <<"id">> := ID, <<"effectiveDate">> := EffectiveDateStr, <<"accountingDate">> := AccountingDateStr, <<"detail">> := Detail, <<"amount">> := Amount, <<"type">> := TypeStr } = _Transaction) ->
  EffectiveDate = string_to_date(EffectiveDateStr),
  AccountingDate = string_to_date(AccountingDateStr),
  Type = case TypeStr of
           <<"SEPA_DEBIT">> -> sepa_debit;
           <<"PURCHASE_CARD">> -> card_debit;
           <<"TRANSFER">> -> transfer;
           <<"CHECK">> -> check;
           <<"CARD_WITHDRAWAL">> -> card_withdrawal;
           <<"OTHER">> when Detail =:= <<"INTÉRÊTS PAYÉS"/utf8>> -> interests
           %_ -> io:format("T=~p", [_Transaction])
         end,
  #{ id => ID, effective_date => EffectiveDate, accounting_date => AccountingDate, amount => Amount, description => Detail, type => Type }.


%%
%% @doc Internal function to convert date from string
%%
-spec string_to_date(unicode:unicode_binary()) -> calendar:date().
string_to_date(DateStr) ->
  case re:run(DateStr, "([0-9]{4})-([0-9]{2})-([0-9]{2})", [{capture, all_but_first, list}]) of
    {match, [YearStr, MonthStr, DayStr]} ->
      {list_to_integer(YearStr), list_to_integer(MonthStr), list_to_integer(DayStr)}
  end.

%%
%% @doc Internal function to fetch data
%%
request_json(AuthToken, URL) ->
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(get, {URL, [{"ingdf-auth-token", AuthToken} | ?HEADERS]}, [], []),
    Bin = list_to_binary(Body),
    {Bin, jsx:decode(Bin,[return_maps])}.
