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
         should_connect/1
        ]).

all() ->
  [
   should_not_authenticate_again_if_token_is_available,
   should_not_authenticate_if_birthdate_and_client_id_mismatched,
   should_not_authenticate_if_password_is_invalid,
   should_connect_without_net_keypad,
   should_connect_without_net,
   should_connect
  ].

init_per_testcase(should_connect_without_net_keypad, Config) ->
  meck:new(httpc),
  meck:new(banks_fetch_bank_ing_keypad),
  Config;
init_per_testcase(should_connect_without_net, Config) ->
  meck:new(httpc),
  Config;
init_per_testcase(should_connect, Config) ->
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
  Config.

end_per_testcase(should_connect_without_net_keypad, _Config) ->
  meck:unload(banks_fetch_bank_ing_keypad),
  meck:unload(httpc),
  ok;
end_per_testcase(should_connect_without_net, _Config) ->
  meck:unload(httpc),
  ok;
end_per_testcase(should_connect, _Config) ->
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

should_connect(Config) ->
  ct:comment("Load credential"),
  case file:consult(filename:join([?config(data_dir, Config), "ing_real_credential.hrl"])) of
    {error, enoent} ->
      ct:comment("No credential, no real test"),
      ok;
    {ok, [{ClientId, {ClientPwd, ClientBirthDate}}]} ->
      ct:comment("Connect to ing account"),
      ok = httpc:set_options([{cookies,enabled}]),
      {ok, {bank_auth, banks_fetch_bank_ing, _AuthToken}} = banks_fetch_bank_ing:connect(ClientId, {ClientPwd, ClientBirthDate}),
      ok
  end.
