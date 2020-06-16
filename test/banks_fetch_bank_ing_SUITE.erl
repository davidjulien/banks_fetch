-module(banks_fetch_bank_ing_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([
         all/0,
         init_per_testcase/2,
         end_per_testcase/2,

         should_not_authenticate_again_if_token_is_available/1,
         should_not_authenticate_if_birthdate_and_client_id_mismatched/1,
         should_not_authenticate_if_password_is_invalid/1,

         should_connect_without_net_keypad/1,
         should_connect_without_net/1,

         should_fetch_accounts_without_net/1,

         test_with_real_credential/1
        ]).

all() ->
  [
   should_not_authenticate_again_if_token_is_available,
   should_not_authenticate_if_birthdate_and_client_id_mismatched,
   should_not_authenticate_if_password_is_invalid,


   should_connect_without_net_keypad,
   should_connect_without_net,

   should_fetch_accounts_without_net,

   test_with_real_credential
  ].

init_per_testcase(test_with_real_credential, Config) ->
  Config;

init_per_testcase(should_connect_without_net_keypad, Config) ->
  meck:new(httpc),
  meck:new(banks_fetch_bank_ing_keypad),
  Config;
init_per_testcase(should_connect_without_net, Config) ->
  meck:new(httpc),
  Config;

init_per_testcase(should_not_authenticate_again_if_token_is_available, Config) ->
  meck:new(httpc),
  meck:new(banks_fetch_bank_ing_keypad),
  Config;
init_per_testcase(should_not_authenticate_if_birthdate_and_client_id_mismatched, Config) ->
  meck:new(httpc),
  meck:new(banks_fetch_bank_ing_keypad),
  Config;
init_per_testcase(should_not_authenticate_if_password_is_invalid, Config) ->
  meck:new(httpc),
  meck:new(banks_fetch_bank_ing_keypad),
  Config;

init_per_testcase(should_fetch_accounts_without_net, Config) ->
  meck:new(httpc),
  Config.


end_per_testcase(test_with_real_credential, _Config) ->
  ok;

end_per_testcase(should_connect_without_net_keypad, _Config) ->
  meck:unload(banks_fetch_bank_ing_keypad),
  meck:unload(httpc),
  ok;
end_per_testcase(should_connect_without_net, _Config) ->
  meck:unload(httpc),
  ok;

end_per_testcase(should_not_authenticate_again_if_token_is_available, _Config) ->
  meck:unload(banks_fetch_bank_ing_keypad),
  meck:unload(httpc),
  ok;
end_per_testcase(should_not_authenticate_if_birthdate_and_client_id_mismatched, _Config) ->
  meck:unload(banks_fetch_bank_ing_keypad),
  meck:unload(httpc),
  ok;
end_per_testcase(should_not_authenticate_if_password_is_invalid, _Config) ->
  meck:unload(banks_fetch_bank_ing_keypad),
  meck:unload(httpc),
  ok;

end_per_testcase(should_fetch_accounts_without_net, _Config) ->
  meck:unload(httpc),
  ok.

-define(CLIENT_ID, "123456789").
-define(CLIENT_PWD, "234567").
-define(CLIENT_BIRTHDATE, "230378").

%%
%% Should not authenticate cases
%%

should_not_authenticate_again_if_token_is_available(_Config) ->
  ct:comment("Connect to ing account"),
  meck:expect(httpc, request,
              [
               {
                [get, {"https://m.ing.fr/", '_'}, '_', []],
                {ok, {{'fakeversion', 200, 'fakereason'}, [{"ingdf-auth-token", "AUTH_TOKEN"}], 'fakebody'}}
               }
              ]),
  {ok, {bank_auth, banks_fetch_bank_ing, "AUTH_TOKEN"}} = banks_fetch_bank_ing:connect(?CLIENT_ID, {?CLIENT_PWD, ?CLIENT_BIRTHDATE}),
  true = meck:validate(httpc),
  true = meck:validate(banks_fetch_bank_ing_keypad),
  1 = meck:num_calls(httpc, request, '_'),

  ok.

should_not_authenticate_if_birthdate_and_client_id_mismatched(_Config) ->
  ct:comment("Connect to ing account"),

  HttpcExpectations = [
                       % Main page
                       { [get, {"https://m.ing.fr/", '_'}, '_', []],
                         {ok, {{'fakeversion', 200, 'fakereason'}, [], 'fakebody'}}
                       },
                       % Login
                       {
                        [post, {"https://m.ing.fr/secure/api-v1/login/cif", '_', "application/json;charset=UTF-8", "{\"cif\":\""++?CLIENT_ID++"\",\"birthDate\":\""++?CLIENT_BIRTHDATE++"\"}"}, '_', []],
                        {ok,{{"HTTP/1.1",412,"Precondition Failed"},
                             fake_headers,
                             "{\"error\":{\"code\":\"AUTHENTICATION.INVALID_CIF_AND_BIRTHDATE_COMBINATION\",\"message\":\"Votre numéro de client et votre date de naissance ne correspondent pas. Veuillez réessayer.\",\"values\":{}}}"}}
                       }
                      ],
  NbrHttpcExpectations = length(HttpcExpectations),
  meck:expect(httpc, request, HttpcExpectations),
  {error, invalid_birthdate} = banks_fetch_bank_ing:connect(?CLIENT_ID, {?CLIENT_PWD, ?CLIENT_BIRTHDATE}),
  true = meck:validate(httpc),
  true = meck:validate(banks_fetch_bank_ing_keypad),
  NbrHttpcExpectations = meck:num_calls(httpc, request, '_'),

  ok.

should_not_authenticate_if_password_is_invalid(_Config) ->
  KeypadImage = "fake_keypadimage",
  PinPositions = [1,2,3,4,5,6],
  PinPositionsStr = "[1,2,3,4,5,6]",
  ClickPositions = [[1,2],[3,4]],
  ClickPositionsStr = "[[1,2],[3,4]]",

  ct:comment("Connect to ing account"),

  meck:expect(banks_fetch_bank_ing_keypad, resolve_keypad, fun(MockKeypadImage, MockPinPositions, MockClientPassword) ->
                                                               KeypadImage = binary_to_list(MockKeypadImage),
                                                               PinPositions = MockPinPositions,
                                                               ?CLIENT_PWD = MockClientPassword,
                                                               ClickPositions
                                                           end),
  HttpcExpectations = [
                       % Main page
                       { [get, {"https://m.ing.fr/", '_'}, '_', []],
                         {ok, {{'fakeversion', 200, 'fakereason'}, [], 'fakebody'}}
                       },
                       % Login
                       {
                        [post, {"https://m.ing.fr/secure/api-v1/login/cif", '_', "application/json;charset=UTF-8", "{\"cif\":\""++?CLIENT_ID++"\",\"birthDate\":\""++?CLIENT_BIRTHDATE++"\"}"}, '_', []],
                        {ok, {{'fakeversion', 200, 'fakereason'}, 'fakeheaders', 'fakebody'}}
                       },
                       % Get keypad info
                       {
                        [post, {"https://m.ing.fr/secure/api-v1/login/keypad", '_', "application/json;charset=UTF-8", "{\"keyPadSize\":{\"width\":2840,\"height\":1136}}"}, '_', []],
                        {ok, {{'fakeversion', 200, 'fakereason'}, 'fakeheaders', "{\"keyPadUrl\":\"/keypad.png\",\"pinPositions\":"++PinPositionsStr++"}"}}
                       },
                       % Get keypad image
                       {
                        [get, {"https://m.ing.fr/secure/api-v1/keypad.png", '_'}, '_', []],
                        {ok, {{'fakeversion', 200, 'fakereason'}, 'fakeheaders', KeypadImage}}
                       },
                       % Send keypad resolution -> raise wrong authentification
                       {
                        [post, {"https://m.ing.fr/secure/api-v1/login/sca/pin", '_', "application/json;charset=UTF-8", "{\"clickPositions\":"++ClickPositionsStr++"}"}, [], []],
                        {ok,{{"HTTP/1.1",412,"Precondition Failed"}, 'fakeheaders',
                             "{\"error\":{\"code\":\"SCA.WRONG_AUTHENTICATION\",\"message\":\"Votre authentification est incorrecte. Il vous reste 2 tentatives. Veuillez rÃ©essayer.\",\"values\":{\"numberAttemptsRemaining\":\"2 tentatives\"}}}"}}
                       }
                      ],
  NbrHttpcExpectations = length(HttpcExpectations),
  meck:expect(httpc, request, HttpcExpectations),
  {error, invalid_password} = banks_fetch_bank_ing:connect(?CLIENT_ID, {?CLIENT_PWD, ?CLIENT_BIRTHDATE}),
  true = meck:validate(httpc),
  true = meck:validate(banks_fetch_bank_ing_keypad),
  NbrHttpcExpectations = meck:num_calls(httpc, request, '_'),

  ok.

%%
%% Should connect cases
%%

should_connect_without_net_keypad(_Config) ->
  KeypadImage = "fake_keypadimage",
  PinPositions = [1,2,3,4,5,6],
  PinPositionsStr = "[1,2,3,4,5,6]",
  ClickPositions = [[1,2],[3,4]],
  ClickPositionsStr = "[[1,2],[3,4]]",
  AuthToken = "fake_authtoken",

  ct:comment("Connect to ing account"),

  meck:expect(banks_fetch_bank_ing_keypad, resolve_keypad, fun(MockKeypadImage, MockPinPositions, MockClientPassword) ->
                                                               KeypadImage = binary_to_list(MockKeypadImage),
                                                               PinPositions = MockPinPositions,
                                                               ?CLIENT_PWD = MockClientPassword,
                                                               ClickPositions
                                                           end),
  HttpcExpectations = [
                       % Main page
                       { [get, {"https://m.ing.fr/", '_'}, '_', []],
                         {ok, {{'fakeversion', 200, 'fakereason'}, [], 'fakebody'}}
                       },
                       % Login
                       {
                        [post, {"https://m.ing.fr/secure/api-v1/login/cif", '_', "application/json;charset=UTF-8", "{\"cif\":\""++?CLIENT_ID++"\",\"birthDate\":\""++?CLIENT_BIRTHDATE++"\"}"}, '_', []],
                        {ok, {{'fakeversion', 200, 'fakereason'}, 'fakeheaders', 'fakebody'}}
                       },
                       % Get keypad info
                       {
                        [post, {"https://m.ing.fr/secure/api-v1/login/keypad", '_', "application/json;charset=UTF-8", "{\"keyPadSize\":{\"width\":2840,\"height\":1136}}"}, '_', []],
                        {ok, {{'fakeversion', 200, 'fakereason'}, 'fakeheaders', "{\"keyPadUrl\":\"/keypad.png\",\"pinPositions\":"++PinPositionsStr++"}"}}
                       },
                       % Get keypad image
                       {
                        [get, {"https://m.ing.fr/secure/api-v1/keypad.png", '_'}, '_', []],
                        {ok, {{'fakeversion', 200, 'fakereason'}, 'fakeheaders', KeypadImage}}
                       },
                       % Send keypad resolution
                       {
                        [post, {"https://m.ing.fr/secure/api-v1/login/sca/pin", '_', "application/json;charset=UTF-8", "{\"clickPositions\":"++ClickPositionsStr++"}"}, [], []],
                        {ok, {{'fakeversion', 200, 'fakereason'}, [{"ingdf-auth-token", AuthToken}], "{\"name\":\"David\"}"}}
                       }
                      ],
  NbrHttpcExpectations = length(HttpcExpectations),
  meck:expect(httpc, request, HttpcExpectations),
  {ok, {bank_auth, banks_fetch_bank_ing, AuthToken}} = banks_fetch_bank_ing:connect(?CLIENT_ID, {?CLIENT_PWD, ?CLIENT_BIRTHDATE}),
  true = meck:validate(httpc),
  true = meck:validate(banks_fetch_bank_ing_keypad),
  NbrHttpcExpectations = meck:num_calls(httpc, request, '_'),

  ok.

should_connect_without_net(Config) ->
  ct:comment("Load keypad image"),
  {ok, KeypadImage} = file:read_file(filename:join([?config(data_dir, Config), "keypad_ing_1.png"])),

  PinPositionsStr = "[1,2,3]",
  ExpectedClickPositionsStr = "[[1209.5,237.0],[2177.5,712.0],[1693.5,237.0]]",
  AuthToken = "fake_authtoken",

  ct:comment("Connect to ing account"),

  HttpcExpectations = [
                       % Main page
                       { [get, {"https://m.ing.fr/", '_'}, '_', []],
                         {ok, {{'fakeversion', 200, 'fakereason'}, [], 'fakebody'}}
                       },
                       % Login
                       {
                        [post, {"https://m.ing.fr/secure/api-v1/login/cif", '_', "application/json;charset=UTF-8", "{\"cif\":\""++?CLIENT_ID++"\",\"birthDate\":\""++?CLIENT_BIRTHDATE++"\"}"}, '_', []],
                        {ok, {{'fakeversion', 200, 'fakereason'}, 'fakeheaders', 'fakebody'}}
                       },
                       % Get keypad info
                       {
                        [post, {"https://m.ing.fr/secure/api-v1/login/keypad", '_', "application/json;charset=UTF-8", "{\"keyPadSize\":{\"width\":2840,\"height\":1136}}"}, '_', []],
                        {ok, {{'fakeversion', 200, 'fakereason'}, 'fakeheaders', "{\"keyPadUrl\":\"/keypad.png\",\"pinPositions\":"++PinPositionsStr++"}"}}
                       },
                       % Get keypad image
                       {
                        [get, {"https://m.ing.fr/secure/api-v1/keypad.png", '_'}, '_', []],
                        {ok, {{'fakeversion', 200, 'fakereason'}, 'fakeheaders', binary_to_list(KeypadImage)}}
                       },
                       % Send keypad resolution
                       {
                        [post, {"https://m.ing.fr/secure/api-v1/login/sca/pin", '_', "application/json;charset=UTF-8", "{\"clickPositions\":"++ExpectedClickPositionsStr++"}"}, [], []],
                        {ok, {{'fakeversion', 200, 'fakereason'}, [{"ingdf-auth-token", AuthToken}], "{\"name\":\"David\"}"}}
                       }
                      ],
  NbrHttpcExpectations = length(HttpcExpectations),
  meck:expect(httpc, request, HttpcExpectations),
  {ok, {bank_auth, banks_fetch_bank_ing, AuthToken}} = banks_fetch_bank_ing:connect(?CLIENT_ID, {?CLIENT_PWD, ?CLIENT_BIRTHDATE}),
  true = meck:validate(httpc),
  NbrHttpcExpectations = meck:num_calls(httpc, request, '_'),

  ok.


%%
%% Fetch accounts
%%
should_fetch_accounts_without_net(Config) ->
  ct:comment("Load accounts examples"),
  {ok, AccountsJSON} = file:read_file(filename:join([?config(data_dir, Config), "accounts.json"])),

  FakeToken = fake_authtoken,

  meck:expect(httpc, request, fun(MockMethod, {MockURL, MockHeaders}, MockHTTPOptions, MockOptions) ->
                                  get = MockMethod,
                                  "https://m.ing.fr/secure/api-v1/accounts" = MockURL,
                                  [{"ingdf-auth-token", FakeToken}|_] = MockHeaders,
                                  [] = MockHTTPOptions,
                                  [] = MockOptions,
                                  {ok, {{'fakeversion', 200, 'fakereason'}, 'fakeheaders', binary_to_list(AccountsJSON)}}
                              end),

  {JSON, Accounts} = banks_fetch_bank_ing:fetch_accounts({bank_auth, banks_fetch_bank_ing, FakeToken}),

  ExpectedAccounts = [
                      #{balance => 3445.19,id => <<"MyAccount1">>, link => <<"/accounts/MyAccount1">>, name => <<"Compte Courant">>, number => <<"XXXX ACC1">>,owner => <<"M ING CLIENT">>, ownership => joint, type => current},
                      #{balance => 6044.09,id => <<"MyAccount2">>, link => <<"/accounts/MyAccount2">>, name => <<"Livret Développement Durable"/utf8>>, number => <<"XXXX ACC2">>,owner => <<"M. ING CLIENT">>, ownership => single, type => savings},
                      #{balance => 493795.02,id => <<"MyAccount3">>, link => <<"/accounts/MyAccount3">>, name => <<"Crédit Immobilier"/utf8>>,number => <<"XXXX ACC3">>, owner => <<"ING CLIENT 1 et 2">>, ownership => joint, type => home_loan}
                     ],
  NbrExpectedAccounts = length(ExpectedAccounts),
  NbrExpectedAccounts = length(Accounts),
  ExpectedAccounts = Accounts,

  AccountsJSON = JSON,

  true = meck:validate(httpc),

  ok.


%%
%% Test with real credential
%%
test_with_real_credential(Config) ->
  ct:comment("Load credential"),
  case file:consult(filename:join([?config(data_dir, Config), "ing_real_credential.hrl"])) of
    {error, enoent} ->
      ct:comment("No credential, no real test"),
      ok;
    {ok, [{ClientId, {ClientPwd, ClientBirthDate}}]} ->
      ct:comment("Connect to ing account"),
      ok = httpc:set_options([{cookies,enabled}]),
      {ok, Auth} = banks_fetch_bank_ing:connect(ClientId, {ClientPwd, ClientBirthDate}),

      {bank_auth, banks_fetch_bank_ing, _AuthToken} = Auth,

      {_, Accounts} = banks_fetch_bank_ing:fetch_accounts(Auth),
      true = length(Accounts) > 0,

      ok
  end.
