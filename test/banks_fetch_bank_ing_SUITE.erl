-module(banks_fetch_bank_ing_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([
         all/0, 
         init_per_testcase/2,
         end_per_testcase/2,
         should_not_authenticate_again_if_token_is_available/1,
         should_connect/1
        ]).

all() ->
  [
   should_not_authenticate_again_if_token_is_available,
   should_connect
  ].

init_per_testcase(_, Config) ->
  meck:new(httpc),
  meck:new(banks_fetch_bank_ing_keypad),
  Config.

end_per_testcase(_, _Config) ->
  meck:unload(banks_fetch_bank_ing_keypad),
  meck:unload(httpc),
  ok.

-define(CLIENT_ID, "123456789").
-define(CLIENT_PWD, "212121").
-define(CLIENT_BIRTHDATE, "230378").

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


should_connect(_Config) ->
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
                        [get, {"https://m.ing.fr/secure/api-v1/keypad.png", '_', "image/png,image/svg+xml,image/*;q=0.8,*/*;q=0.5", ""}, '_', []],
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
