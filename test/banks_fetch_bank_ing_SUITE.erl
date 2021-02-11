-module(banks_fetch_bank_ing_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([
         all/0,
         init_per_suite/1,
         end_per_suite/1,

         init_per_testcase/2,
         end_per_testcase/2,

         should_not_authenticate_again_if_token_is_available/1,
         should_not_authenticate_if_connection_failed/1,
         should_not_authenticate_if_birthdate_and_client_id_mismatched/1,
         should_not_authenticate_if_password_is_invalid/1,
         should_not_authenticate_if_account_is_locked/1,
         should_not_authenticate_if_invalid_birthdate/1,
         should_not_authenticate_if_sms_verification/1,

         should_connect_without_net_keypad/1,
         should_connect_without_net/1,

         should_fetch_accounts_without_net/1,

         should_fetch_transactions_without_net/1,
         should_fetch_transactions_until_without_net/1,
         should_fetch_transactions_single_case_without_net/1,

         test_with_real_credential/1
        ]).

all() ->
  [
   should_not_authenticate_again_if_token_is_available,
   should_not_authenticate_if_connection_failed,
   should_not_authenticate_if_birthdate_and_client_id_mismatched,
   should_not_authenticate_if_password_is_invalid,
   should_not_authenticate_if_account_is_locked,
   should_not_authenticate_if_invalid_birthdate,
   should_not_authenticate_if_sms_verification,


   should_connect_without_net_keypad,
   should_connect_without_net,

   should_fetch_accounts_without_net,
   should_fetch_transactions_without_net,
   should_fetch_transactions_until_without_net,
   should_fetch_transactions_single_case_without_net,

   test_with_real_credential
  ].


%%
%% Overall setup/teardwon
%%
init_per_suite(Config) ->
  ok = lager:start(),
  Config.

end_per_suite(_Config) ->
  application:stop(lager).

%%
%% Test cases
%%

init_per_testcase(test_with_real_credential, Config) ->
  ct:comment("Load credential"),
  case file:consult(filename:join([?config(data_dir, Config), "ing_real_credential.hrl"])) of
    {error, enoent} ->
      {skip, "You may add a banks_fetch_bank_ing_SUITE_data/ing_real_credential.hrl file with your ing credential to launch a test with a real connection"};
    {ok, [{ClientId, ClientCredential}]} ->
      ok = application:start(prometheus),
      [{client_info, {ClientId, ClientCredential}} | Config]
  end;

init_per_testcase(should_connect_without_net_keypad, Config) ->
  meck:new(banks_fetch_http),
  meck:new(banks_fetch_bank_ing_keypad),
  Config;
init_per_testcase(should_connect_without_net, Config) ->
  meck:new(banks_fetch_http),
  Config;

init_per_testcase(should_not_authenticate_again_if_token_is_available, Config) ->
  meck:new(banks_fetch_http),
  meck:new(banks_fetch_bank_ing_keypad),
  meck:new(prometheus_counter),
  Config;
init_per_testcase(should_not_authenticate_if_connection_failed, Config) ->
  meck:new(banks_fetch_http),
  meck:new(banks_fetch_bank_ing_keypad),
  meck:new(prometheus_counter),
  Config;
init_per_testcase(should_not_authenticate_if_birthdate_and_client_id_mismatched, Config) ->
  meck:new(banks_fetch_http),
  meck:new(banks_fetch_bank_ing_keypad),
  meck:new(prometheus_counter),
  Config;
init_per_testcase(should_not_authenticate_if_password_is_invalid, Config) ->
  meck:new(banks_fetch_http),
  meck:new(banks_fetch_bank_ing_keypad),
  meck:new(prometheus_counter),
  Config;
init_per_testcase(should_not_authenticate_if_account_is_locked, Config) ->
  meck:new(banks_fetch_http),
  meck:new(banks_fetch_bank_ing_keypad),
  meck:new(prometheus_counter),
  Config;
init_per_testcase(should_not_authenticate_if_invalid_birthdate, Config) ->
  meck:new(banks_fetch_http),
  meck:new(banks_fetch_bank_ing_keypad),
  meck:new(prometheus_counter),
  Config;
init_per_testcase(should_not_authenticate_if_sms_verification, Config) ->
  meck:new(banks_fetch_http),
  meck:new(banks_fetch_bank_ing_keypad),
  meck:new(prometheus_counter),
  Config;



init_per_testcase(should_fetch_accounts_without_net, Config) ->
  meck:new(banks_fetch_http),
  meck:new(prometheus_counter),
  Config;

init_per_testcase(should_fetch_transactions_without_net, Config) ->
  meck:new(banks_fetch_http),
  meck:new(prometheus_counter),
  Config;
init_per_testcase(should_fetch_transactions_until_without_net, Config) ->
  meck:new(banks_fetch_http),
  meck:new(prometheus_counter),
  Config;
init_per_testcase(should_fetch_transactions_single_case_without_net, Config) ->
  meck:new(banks_fetch_http),
  meck:new(prometheus_counter),
  Config.

end_per_testcase(test_with_real_credential, _Config) ->
  application:stop(prometheus),
  ok;

end_per_testcase(should_connect_without_net_keypad, _Config) ->
  meck:unload(banks_fetch_bank_ing_keypad),
  meck:unload(banks_fetch_http),
  meck:unload(prometheus_counter),
  ok;
end_per_testcase(should_connect_without_net, _Config) ->
  meck:unload(banks_fetch_http),
  meck:unload(prometheus_counter),
  ok;

end_per_testcase(should_not_authenticate_again_if_token_is_available, _Config) ->
  meck:unload(banks_fetch_bank_ing_keypad),
  meck:unload(banks_fetch_http),
  meck:unload(prometheus_counter),
  ok;
end_per_testcase(should_not_authenticate_if_connection_failed, _Config) ->
  meck:unload(banks_fetch_bank_ing_keypad),
  meck:unload(banks_fetch_http),
  meck:unload(prometheus_counter),
  ok;
end_per_testcase(should_not_authenticate_if_birthdate_and_client_id_mismatched, _Config) ->
  meck:unload(banks_fetch_bank_ing_keypad),
  meck:unload(banks_fetch_http),
  meck:unload(prometheus_counter),
  ok;
end_per_testcase(should_not_authenticate_if_password_is_invalid, _Config) ->
  meck:unload(banks_fetch_bank_ing_keypad),
  meck:unload(banks_fetch_http),
  meck:unload(prometheus_counter),
  ok;
end_per_testcase(should_not_authenticate_if_account_is_locked, _Config) ->
  meck:unload(banks_fetch_bank_ing_keypad),
  meck:unload(banks_fetch_http),
  meck:unload(prometheus_counter),
  ok;
end_per_testcase(should_not_authenticate_if_invalid_birthdate, _Config) ->
  meck:unload(banks_fetch_bank_ing_keypad),
  meck:unload(banks_fetch_http),
  meck:unload(prometheus_counter),
  ok;
end_per_testcase(should_not_authenticate_if_sms_verification, _Config) ->
  meck:unload(banks_fetch_bank_ing_keypad),
  meck:unload(banks_fetch_http),
  meck:unload(prometheus_counter),
  ok;


end_per_testcase(should_fetch_accounts_without_net, _Config) ->
  meck:unload(banks_fetch_http),
  meck:unload(prometheus_counter),
  ok;

end_per_testcase(should_fetch_transactions_without_net, _Config) ->
  meck:unload(banks_fetch_http),
  meck:unload(prometheus_counter),
  ok;
end_per_testcase(should_fetch_transactions_until_without_net, _Config) ->
  meck:unload(banks_fetch_http),
  meck:unload(prometheus_counter),
  ok;
end_per_testcase(should_fetch_transactions_single_case_without_net, _Config) ->
  meck:unload(banks_fetch_http),
  meck:unload(prometheus_counter),
  ok.

-define(CLIENT_ID_VAL, <<"123456789">>).
-define(CLIENT_ID, {client_id, ?CLIENT_ID_VAL}).
-define(CLIENT_PWD, "234567").
-define(CLIENT_BIRTHDATE, "23031978").
-define(CLIENT_CREDENTIAL, {client_credential, {?CLIENT_PWD, ?CLIENT_BIRTHDATE}}).

-define(CLIENT_INVALID_BIRTHDATE, "INVALID_BIRTHDATE").

%%
%% Should not authenticate cases
%%

should_not_authenticate_again_if_token_is_available(_Config) ->
  ct:comment("Connect to ing account"),
  meck:expect(prometheus_counter, inc,
              [
               {['bank_ing_connect_total_count'],ok},
               {['bank_ing_connect_already_count'],ok}
              ]),
  meck:expect(banks_fetch_http, set_options, fun(MockOptions) -> [{cookies,enabled}] = MockOptions, ok end),
  meck:expect(banks_fetch_http, request,
              [
               {
                [get, {"https://m.ing.fr/", '_'}, '_', []],
                {ok, {{'fakeversion', 200, 'fakereason'}, [{"ingdf-auth-token", "AUTH_TOKEN"}], 'fakebody'}}
               }
              ]),
  {ok, {bank_auth, banks_fetch_bank_ing, "AUTH_TOKEN"}} = banks_fetch_bank_ing:connect(?CLIENT_ID, {client_credential, {?CLIENT_PWD, ?CLIENT_BIRTHDATE}}),

  ct:comment("Verify banks_fetch_http, ing_keypad and monitoring calls"),
  true = meck:validate(banks_fetch_http),
  true = meck:validate(banks_fetch_bank_ing_keypad),
  true = meck:validate(prometheus_counter),
  1 = meck:num_calls(banks_fetch_http, request, '_'),
  2 = meck:num_calls(prometheus_counter, inc, '_'),

  ok.

should_not_authenticate_if_connection_failed(_Config) ->
  ct:comment("Connect to ing account"),
  meck:expect(prometheus_counter, inc,
              [
               {['bank_ing_connect_total_count'],ok},
               {['bank_ing_connect_network_error_count'],ok}
              ]),
  meck:expect(banks_fetch_http, set_options, fun(MockOptions) -> [{cookies,enabled}] = MockOptions, ok end),
  meck:expect(banks_fetch_http, request,
              [
               {
                [get, {"https://m.ing.fr/", '_'}, '_', []],
                {error, failed_connect}
               }
              ]),
  {error, network_error} = banks_fetch_bank_ing:connect(?CLIENT_ID, {client_credential, {?CLIENT_PWD, ?CLIENT_BIRTHDATE}}),

  ct:comment("Verify banks_fetch_http, ing_keypad and monitoring calls"),
  true = meck:validate(banks_fetch_http),
  true = meck:validate(banks_fetch_bank_ing_keypad),
  true = meck:validate(prometheus_counter),
  1 = meck:num_calls(banks_fetch_http, request, '_'),
  2 = meck:num_calls(prometheus_counter, inc, '_'),

  ok.



should_not_authenticate_if_birthdate_and_client_id_mismatched(_Config) ->
  ct:comment("Connect to ing account"),

  meck:expect(prometheus_counter, inc,
              [
               {['bank_ing_connect_total_count'],ok},
               {['bank_ing_connect_invalid_cif_birthdate_count'],ok}
              ]),
  HttpExpectations = [
                       % Main page
                       { [get, {"https://m.ing.fr/", '_'}, '_', []],
                         {ok, {{'fakeversion', 200, 'fakereason'}, [], 'fakebody'}}
                       },
                       % Login
                       {
                        [post, {"https://m.ing.fr/secure/api-v1/login/cif", '_', "application/json;charset=UTF-8", "{\"cif\":\""++binary_to_list(?CLIENT_ID_VAL)++"\",\"birthDate\":\""++?CLIENT_BIRTHDATE++"\"}"}, '_', []],
                        {ok,{{"HTTP/1.1",412,"Precondition Failed"},
                             fake_headers,
                             "{\"error\":{\"code\":\"AUTHENTICATION.INVALID_CIF_AND_BIRTHDATE_COMBINATION\",\"message\":\"Votre numéro de client et votre date de naissance ne correspondent pas. Veuillez réessayer.\",\"values\":{}}}"}}
                       }
                      ],
  NbrHttpExpectations = length(HttpExpectations),

  meck:expect(banks_fetch_http, set_options, fun(MockOptions) -> [{cookies,enabled}] = MockOptions, ok end),
  meck:expect(banks_fetch_http, request, HttpExpectations),

  {error, invalid_credential} = banks_fetch_bank_ing:connect(?CLIENT_ID, {client_credential, {?CLIENT_PWD, ?CLIENT_BIRTHDATE}}),

  ct:comment("Verify banks_fetch_http, ing_keypad and monitoring calls"),
  true = meck:validate(banks_fetch_http),
  true = meck:validate(banks_fetch_bank_ing_keypad),
  true = meck:validate(prometheus_counter),
  NbrHttpExpectations = meck:num_calls(banks_fetch_http, request, '_'),
  2 = meck:num_calls(prometheus_counter, inc, '_'),

  ok.

should_not_authenticate_if_password_is_invalid(_Config) ->
  KeypadImage = "fake_keypadimage",
  PinPositions = [1,2,3,4,5,6],
  PinPositionsStr = "[1,2,3,4,5,6]",
  ClickPositions = [[1,2],[3,4]],
  ClickPositionsStr = "[[1,2],[3,4]]",

  ct:comment("Connect to ing account"),

  meck:expect(prometheus_counter, inc,
              [
               {['bank_ing_connect_total_count'],ok},
               {['bank_ing_connect_wrong_authentication_count'],ok}
              ]),
  meck:expect(banks_fetch_bank_ing_keypad, resolve_keypad, fun(MockKeypadImage, MockPinPositions, MockClientPassword) ->
                                                               KeypadImage = binary_to_list(MockKeypadImage),
                                                               PinPositions = MockPinPositions,
                                                               ?CLIENT_PWD = MockClientPassword,
                                                               ClickPositions
                                                           end),

  HttpExpectations = [
                       % Main page
                       { [get, {"https://m.ing.fr/", '_'}, '_', []],
                         {ok, {{'fakeversion', 200, 'fakereason'}, [], 'fakebody'}}
                       },
                       % Login
                       {
                        [post, {"https://m.ing.fr/secure/api-v1/login/cif", '_', "application/json;charset=UTF-8", "{\"cif\":\""++binary_to_list(?CLIENT_ID_VAL)++"\",\"birthDate\":\""++?CLIENT_BIRTHDATE++"\"}"}, '_', []],
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
  NbrHttpExpectations = length(HttpExpectations),

  meck:expect(banks_fetch_http, set_options, fun(MockOptions) -> [{cookies,enabled}] = MockOptions, ok end),
  meck:expect(banks_fetch_http, request, HttpExpectations),

  {error, invalid_credential} = banks_fetch_bank_ing:connect(?CLIENT_ID, {client_credential, {?CLIENT_PWD, ?CLIENT_BIRTHDATE}}),

  ct:comment("Verify banks_fetch_http, ing_keypad and monitoring calls"),
  true = meck:validate(banks_fetch_http),
  true = meck:validate(banks_fetch_bank_ing_keypad),
  true = meck:validate(prometheus_counter),
  NbrHttpExpectations = meck:num_calls(banks_fetch_http, request, '_'),
  2 = meck:num_calls(prometheus_counter, inc, '_'),

  ok.

should_not_authenticate_if_account_is_locked(_Config) ->
  ct:comment("Connect to ing account"),

  meck:expect(prometheus_counter, inc,
              [
               {['bank_ing_connect_total_count'],ok},
               {['bank_ing_connect_account_locked_count'],ok}
              ]),
  HttpExpectations = [
                       % Main page
                       { [get, {"https://m.ing.fr/", '_'}, '_', []],
                         {ok, {{'fakeversion', 200, 'fakereason'}, [], 'fakebody'}}
                       },
                       % Login
                       {
                        [post, {"https://m.ing.fr/secure/api-v1/login/cif", '_', "application/json;charset=UTF-8", "{\"cif\":\""++binary_to_list(?CLIENT_ID_VAL)++"\",\"birthDate\":\""++?CLIENT_BIRTHDATE++"\"}"}, '_', []],
                        {ok,{{"HTTP/1.1",412,"Precondition Failed"},
                             fake_headers,
                             "{\"error\":{\"code\":\"SCA.ACCOUNT_LOCKED\",\"message\":\"Votre compte est bloqué.\",\"values\":{}}}"}}
                       }
                      ],
  NbrHttpExpectations = length(HttpExpectations),

  meck:expect(banks_fetch_http, set_options, fun(MockOptions) -> [{cookies,enabled}] = MockOptions, ok end),
  meck:expect(banks_fetch_http, request, HttpExpectations),

  {error, account_locked} = banks_fetch_bank_ing:connect(?CLIENT_ID, {client_credential, {?CLIENT_PWD, ?CLIENT_BIRTHDATE}}),

  ct:comment("Verify banks_fetch_http, ing_keypad and monitoring calls"),
  true = meck:validate(banks_fetch_http),
  true = meck:validate(banks_fetch_bank_ing_keypad),
  true = meck:validate(prometheus_counter),
  NbrHttpExpectations = meck:num_calls(banks_fetch_http, request, '_'),
  2 = meck:num_calls(prometheus_counter, inc, '_'),

  ok.

should_not_authenticate_if_invalid_birthdate(_Config) ->
  ct:comment("Connect to ing account"),

  meck:expect(prometheus_counter, inc,
              [
               {['bank_ing_connect_total_count'],ok},
               {['bank_ing_connect_internal_error_count'],ok}
              ]),
  HttpExpectations = [
                       % Main page
                       { [get, {"https://m.ing.fr/", '_'}, '_', []],
                         {ok, {{'fakeversion', 200, 'fakereason'}, [], 'fakebody'}}
                       },
                       % Login
                       {
                        [post, {"https://m.ing.fr/secure/api-v1/login/cif", '_', "application/json;charset=UTF-8", "{\"cif\":\""++binary_to_list(?CLIENT_ID_VAL)++"\",\"birthDate\":\""++?CLIENT_INVALID_BIRTHDATE++"\"}"}, '_', []],
                        {ok,{{"HTTP/1.1",500,"Internal Server Error"},
                             fake_headers,
                             "{\"error\":{\"code\":\"INTERNAL_ERROR\",\"message\":\"Ce service est indisponible pour le moment. Toutes nos excuses pour la gêne occasionnée. Pour effectuer vos opérations habituelles, réessayez plus tard ou contactez notre Centre de Relation Client.\"}}"}}
                       }
                      ],
  NbrHttpExpectations = length(HttpExpectations),

  meck:expect(banks_fetch_http, set_options, fun(MockOptions) -> [{cookies,enabled}] = MockOptions, ok end),
  meck:expect(banks_fetch_http, request, HttpExpectations),

  {error, internal_error} = banks_fetch_bank_ing:connect(?CLIENT_ID, {client_credential, {?CLIENT_PWD, ?CLIENT_INVALID_BIRTHDATE}}),

  ct:comment("Verify banks_fetch_http, ing_keypad and monitoring calls"),
  true = meck:validate(banks_fetch_http),
  true = meck:validate(banks_fetch_bank_ing_keypad),
  true = meck:validate(prometheus_counter),
  NbrHttpExpectations = meck:num_calls(banks_fetch_http, request, '_'),
  2 = meck:num_calls(prometheus_counter, inc, '_'),

  ok.

should_not_authenticate_if_sms_verification(_Config) ->
  KeypadImage = "fake_keypadimage",
  PinPositions = [1,2,3,4,5,6],
  PinPositionsStr = "[1,2,3,4,5,6]",
  ClickPositions = [[1,2],[3,4]],
  ClickPositionsStr = "[[1,2],[3,4]]",
  AuthToken = "fake_authtoken",

  ct:comment("Connect to ing account"),

  meck:expect(prometheus_counter, inc,
              [
               {['bank_ing_connect_total_count'],ok},
               {['bank_ing_connect_sms_verification_count'],ok}
              ]),

  meck:expect(banks_fetch_bank_ing_keypad, resolve_keypad, fun(MockKeypadImage, MockPinPositions, MockClientPassword) ->
                                                               KeypadImage = binary_to_list(MockKeypadImage),
                                                               PinPositions = MockPinPositions,
                                                               ?CLIENT_PWD = MockClientPassword,
                                                               ClickPositions
                                                           end),

  HttpExpectations = [
                       % Main page
                       { [get, {"https://m.ing.fr/", '_'}, '_', []],
                         {ok, {{'fakeversion', 200, 'fakereason'}, [], 'fakebody'}}
                       },
                       % Login
                       {
                        [post, {"https://m.ing.fr/secure/api-v1/login/cif", '_', "application/json;charset=UTF-8", "{\"cif\":\""++binary_to_list(?CLIENT_ID_VAL)++"\",\"birthDate\":\""++?CLIENT_BIRTHDATE++"\"}"}, '_', []],
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
                        {ok, {{'fakeversion', 200, 'fakereason'}, [], "{\"secretCode\":\"877bab7a-cb8e-4e49-a635-7faca8a90a7e\",\"strongAuthenticationLoginExempted\":false}"}}
                       }
                      ],
  NbrHttpExpectations = length(HttpExpectations),

  meck:expect(banks_fetch_http, set_options, fun(MockOptions) -> [{cookies,enabled}] = MockOptions, ok end),
  meck:expect(banks_fetch_http, request, HttpExpectations),

  {error, {sms_verification, <<"877bab7a-cb8e-4e49-a635-7faca8a90a7e">>}} = banks_fetch_bank_ing:connect(?CLIENT_ID, {client_credential, {?CLIENT_PWD, ?CLIENT_BIRTHDATE}}),

  ct:comment("Verify banks_fetch_http, ing_keypad and monitoring calls"),
  true = meck:validate(banks_fetch_http),
  true = meck:validate(banks_fetch_bank_ing_keypad),
  true = meck:validate(prometheus_counter),
  NbrHttpExpectations = meck:num_calls(banks_fetch_http, request, '_'),
  2 = meck:num_calls(prometheus_counter, inc, '_'),

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

  meck:expect(prometheus_counter, inc,
              [
               {['bank_ing_connect_total_count'],ok},
               {['bank_ing_connect_ok_count'],ok}
              ]),

  meck:expect(banks_fetch_bank_ing_keypad, resolve_keypad, fun(MockKeypadImage, MockPinPositions, MockClientPassword) ->
                                                               KeypadImage = binary_to_list(MockKeypadImage),
                                                               PinPositions = MockPinPositions,
                                                               ?CLIENT_PWD = MockClientPassword,
                                                               ClickPositions
                                                           end),

  HttpExpectations = [
                       % Main page
                       { [get, {"https://m.ing.fr/", '_'}, '_', []],
                         {ok, {{'fakeversion', 200, 'fakereason'}, [], 'fakebody'}}
                       },
                       % Login
                       {
                        [post, {"https://m.ing.fr/secure/api-v1/login/cif", '_', "application/json;charset=UTF-8", "{\"cif\":\""++binary_to_list(?CLIENT_ID_VAL)++"\",\"birthDate\":\""++?CLIENT_BIRTHDATE++"\"}"}, '_', []],
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
  NbrHttpExpectations = length(HttpExpectations),

  meck:expect(banks_fetch_http, set_options, fun(MockOptions) -> [{cookies,enabled}] = MockOptions, ok end),
  meck:expect(banks_fetch_http, request, HttpExpectations),

  {ok, {bank_auth, banks_fetch_bank_ing, AuthToken}} = banks_fetch_bank_ing:connect(?CLIENT_ID, {client_credential, {?CLIENT_PWD, ?CLIENT_BIRTHDATE}}),

  ct:comment("Verify banks_fetch_http, ing_keypad and monitoring calls"),
  true = meck:validate(banks_fetch_http),
  true = meck:validate(banks_fetch_bank_ing_keypad),
  true = meck:validate(prometheus_counter),
  NbrHttpExpectations = meck:num_calls(banks_fetch_http, request, '_'),
  2 = meck:num_calls(prometheus_counter, inc, '_'),

  ok.

should_connect_without_net(Config) ->
  ct:comment("Load keypad image"),
  {ok, KeypadImage} = file:read_file(filename:join([?config(data_dir, Config), "keypad_ing_1.png"])),

  PinPositionsStr = "[1,2,3]",
  ExpectedClickPositionsStr = "[[1209.5,237.0],[2177.5,712.0],[1693.5,237.0]]",
  AuthToken = "fake_authtoken",

  ct:comment("Connect to ing account"),

  meck:expect(prometheus_counter, inc,
              [
               {['bank_ing_connect_total_count'],ok},
               {['bank_ing_connect_ok_count'],ok}
              ]),

  HttpExpectations = [
                       % Main page
                       { [get, {"https://m.ing.fr/", '_'}, '_', []],
                         {ok, {{'fakeversion', 200, 'fakereason'}, [], 'fakebody'}}
                       },
                       % Login
                       {
                        [post, {"https://m.ing.fr/secure/api-v1/login/cif", '_', "application/json;charset=UTF-8", "{\"cif\":\""++binary_to_list(?CLIENT_ID_VAL)++"\",\"birthDate\":\""++?CLIENT_BIRTHDATE++"\"}"}, '_', []],
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
  NbrHttpExpectations = length(HttpExpectations),

  meck:expect(banks_fetch_http, set_options, fun(MockOptions) -> [{cookies,enabled}] = MockOptions, ok end),
  meck:expect(banks_fetch_http, request, HttpExpectations),

  {ok, {bank_auth, banks_fetch_bank_ing, AuthToken}} = banks_fetch_bank_ing:connect(?CLIENT_ID, {client_credential, {?CLIENT_PWD, ?CLIENT_BIRTHDATE}}),

  ct:comment("Verify banks_fetch_http and monitoring calls"),
  true = meck:validate(banks_fetch_http),
  true = meck:validate(prometheus_counter),
  NbrHttpExpectations = meck:num_calls(banks_fetch_http, request, '_'),
  2 = meck:num_calls(prometheus_counter, inc, '_'),

  ok.


%%
%% Fetch accounts
%%
should_fetch_accounts_without_net(Config) ->
  ct:comment("Load accounts examples"),
  {ok, AccountsJSON} = file:read_file(filename:join([?config(data_dir, Config), "accounts.json"])),

  FakeToken = fake_authtoken,

  meck:expect(prometheus_counter, inc,
              [
               {['bank_ing_accounts_total_count'],ok},
               {['bank_ing_accounts_ok_count'],ok}
              ]),
  meck:expect(banks_fetch_http, set_options, fun(MockOptions) -> [{cookies,enabled}] = MockOptions, ok end),
  meck:expect(banks_fetch_http, request, fun(MockMethod, {MockURL, MockHeaders}, MockHTTPOptions, MockOptions) ->
                                  get = MockMethod,
                                  "https://m.ing.fr/secure/api-v1/accounts" = MockURL,
                                  [{"ingdf-auth-token", FakeToken}|_] = MockHeaders,
                                  [] = MockHTTPOptions,
                                  [] = MockOptions,
                                  {ok, {{'fakeversion', 200, 'fakereason'}, 'fakeheaders', binary_to_list(AccountsJSON)}}
                              end),

  {ok, Accounts} = banks_fetch_bank_ing:fetch_accounts({bank_auth, banks_fetch_bank_ing, FakeToken}),

  ExpectedAccounts = [
                      #{balance => 3445.19,id => <<"MyAccount1">>, link => <<"/accounts/MyAccount1">>, name => <<"Compte Courant">>, number => <<"XXXX ACC1">>,owner => <<"M ING CLIENT">>, ownership => joint, type => current},
                      #{balance => 6044.09,id => <<"MyAccount2">>, link => <<"/accounts/MyAccount2">>, name => <<"Livret Développement Durable"/utf8>>, number => <<"XXXX ACC2">>,owner => <<"M. ING CLIENT">>, ownership => single, type => savings},
                      #{balance => 493795.02,id => <<"MyAccount3">>, link => <<"/accounts/MyAccount3">>, name => <<"Crédit Immobilier"/utf8>>,number => <<"XXXX ACC3">>, owner => <<"ING CLIENT 1 et 2">>, ownership => joint, type => home_loan}
                     ],
  NbrExpectedAccounts = length(ExpectedAccounts),
  NbrExpectedAccounts = length(Accounts),
  ExpectedAccounts = Accounts,

  ct:comment("Verify banks_fetch_http and monitoring calls"),
  true = meck:validate(banks_fetch_http),
  true = meck:validate(prometheus_counter),
  2 = meck:num_calls(prometheus_counter, inc, '_'),

  ok.

%%
%% Fetch transactions
%%
should_fetch_transactions_without_net(Config) ->
  ct:comment("Load transactions examples"),
  {ok, TransactionsJSON_1} = file:read_file(filename:join([?config(data_dir, Config), "transactions_1.json"])),
  {ok, TransactionsJSON_2} = file:read_file(filename:join([?config(data_dir, Config), "transactions_2.json"])),
  TransactionsJSON_3 = <<"[]">>,

  FakeToken = fake_authtoken,
  FakeAccountId = <<"FAKEACCOUNT">>,

  URL_1 = "https://m.ing.fr/secure/api-v1/accounts/" ++ binary_to_list(FakeAccountId) ++ "/transactions/after/0/limit/50",
  URL_2 = "https://m.ing.fr/secure/api-v1/accounts/" ++ binary_to_list(FakeAccountId) ++ "/transactions/after/12714/limit/50",
  URL_3 = "https://m.ing.fr/secure/api-v1/accounts/" ++ binary_to_list(FakeAccountId) ++ "/transactions/after/12535/limit/50",

  meck:expect(prometheus_counter, inc,
              [
               {['bank_ing_transactions_total_count'],ok},
               {['bank_ing_transactions_ok_count'],ok}
              ]),
  HttpExpectations = [
                      {[get, {URL_1, [{"ingdf-auth-token", FakeToken}|'_']}, [], []],
                       {ok, {{'fakeversion', 200, 'fakereason'}, 'fakeheaders', binary_to_list(TransactionsJSON_1)}}
                      },
                      {[get, {URL_2, [{"ingdf-auth-token", FakeToken}|'_']}, [], []],
                       {ok, {{'fakeversion', 200, 'fakereason'}, 'fakeheaders', binary_to_list(TransactionsJSON_2)}}
                      },
                      {[get, {URL_3, [{"ingdf-auth-token", FakeToken}|'_']}, [], []],
                       {ok, {{'fakeversion', 200, 'fakereason'}, 'fakeheaders', binary_to_list(TransactionsJSON_3)}}
                      }
                     ],
  NbrHttpExpectations = length(HttpExpectations),

  meck:expect(banks_fetch_http, set_options, fun(MockOptions) -> [{cookies,enabled}] = MockOptions, ok end),
  meck:expect(banks_fetch_http, request, HttpExpectations),

  ct:comment("Fetch transactions"),
  {ok, Transactions} = banks_fetch_bank_ing:fetch_transactions({bank_auth, banks_fetch_bank_ing, FakeToken}, {account_id, FakeAccountId}, first_call),

  ExpectedTransactions =
  [
   #{accounting_date => {2020,6,16}, amount => -34.0,description => <<"PRLV SEPA XXX">>, effective_date => {2020,6,16}, id => <<"12875">>,type => sepa_debit},
   #{accounting_date => {2020,6,16}, amount => -51.94, description => <<"PAIEMENT PAR CARTE 14/06/2020 XXX">>, effective_date => {2020,6,16}, id => <<"12854">>,type => card_debit},
   #{accounting_date => {2020,6,16}, amount => -116.32, description => <<"VIREMENT SEPA EMIS VERS  123456789 XXX">>, effective_date => {2020,6,16}, id => <<"12850">>,type => transfer},
   #{accounting_date => {2020,6,15}, amount => -50.0, description => <<"PAIEMENT D'UN CHÈQUE 9999999"/utf8>>, effective_date => {2020,6,15}, id => <<"12848">>,type => check},
   #{accounting_date => {2020,6,15}, amount => -118.0,description => <<"PRLV SEPA XXX">>, effective_date => {2020,6,15}, id => <<"12843">>,type => sepa_debit},
   #{accounting_date => {2020,6,15}, amount => -74.0, description => <<"PAIEMENT PAR CARTE 13/06/2020 XXX">>, effective_date => {2020,6,15}, id => <<"12819">>,type => card_debit},
   #{accounting_date => {2020,6,9}, amount => -41.58, description => <<"PAIEMENT D'UN CHÈQUE 9999999"/utf8>>, effective_date => {2020,6,9}, id => <<"12740">>,type => check},
   #{accounting_date => {2020,6,9}, amount => -3.8, description => <<"PAIEMENT PAR CARTE 05/06/2020 XXX">>, effective_date => {2020,6,9}, id => <<"12732">>,type => card_debit},
   #{accounting_date => {2020,6,9}, amount => -23.99, description => <<"PAIEMENT PAR CARTE 05/06/2020 XXX">>, effective_date => {2020,6,9}, id => <<"12729">>,type => card_debit},
   #{accounting_date => {2020,6,8}, amount => -117.25,description => <<"PRLV SEPA XXX">>, effective_date => {2020,6,8}, id => <<"12725">>,type => sepa_debit},
   #{accounting_date => {2020,6,8}, amount => -60.00,description => <<"RETRAIT DAB EN EURO ZONE EURO 07/06/2020 GAB XXX">>, effective_date => {2020,6,8}, id => <<"12722">>,type => card_withdrawal},
   #{accounting_date => {2020,6,8}, amount => -22.3,description => <<"PRLV SEPA XXX">>, effective_date => {2020,6,8}, id => <<"12720">>,type => sepa_debit},
   #{accounting_date => {2020,6,6}, amount => -32.0, description => <<"PAIEMENT PAR CARTE 05/06/2020 XXX">>, effective_date => {2020,6,6}, id => <<"12714">>,type => card_debit},
   #{accounting_date => {2020,6,5}, amount => 641.8,description => <<"VIREMENT SEPA RECU XXX">>, effective_date => {2020,6,5}, id => <<"12710">>,type => transfer},
   #{accounting_date => {2020,6,5}, amount => 2567.42,description => <<"VIREMENT SEPA RECU XXX">>, effective_date => {2020,6,5}, id => <<"12704">>,type => transfer},
   #{accounting_date => {2020,6,5}, amount => -357.0, description => <<"VIREMENT SEPA EMIS VERS  999999 XXX">>, effective_date => {2020,6,5}, id => <<"12683">>,type => transfer},
   #{accounting_date => {2020,6,1}, amount => 3420.0,description => <<"VIREMENT SEPA RECU XXX">>, effective_date => {2020,6,1}, id => <<"12624">>,type => transfer},
   #{accounting_date => {2020,6,1}, amount => -135.0, description => <<"VIREMENT SEPA EMIS VERS XXX">>, effective_date => {2020,6,1}, id => <<"12619">>,type => transfer},
   #{accounting_date => {2020,6,1}, amount => -500.0, description => <<"VIREMENT SEPA EMIS VERS XXX">>, effective_date => {2020,6,1}, id => <<"12603">>,type => transfer},
   #{accounting_date => {2020,5,29}, amount => -23.66,description => <<"PRLV SEPA XXX">>, effective_date => {2020,5,29}, id => <<"12589">>,type => sepa_debit},
   #{accounting_date => {2020,5,28}, amount => -2.93, description => <<"PAIEMENT PAR CARTE 27/05/2020 XXX">>, effective_date => {2020,5,28}, id => <<"12582">>,type => card_debit},
   #{accounting_date => {2020,5,27}, amount => 2978.11,description => <<"VIREMENT SEPA RECU XXX">>, effective_date => {2020,5,27}, id => <<"12579">>,type => transfer},
   #{accounting_date => {2020,5,27}, amount => -150.58, description => <<"PAIEMENT PAR CARTE 26/05/2020 XXX">>, effective_date => {2020,5,27}, id => <<"12541">>,type => card_debit},
   #{accounting_date => {2020,5,26}, amount => -14.9, description => <<"PAIEMENT PAR CARTE 24/05/2020 XXX">>, effective_date => {2020,5,26}, id => <<"12537">>,type => card_debit},
   #{accounting_date => {2020,5,25}, amount => 18.0, description => <<"AVOIR CARTE">>, effective_date => {2020,5,25}, id => <<"12535">>,type => other},
   #{accounting_date => {2020,5,24}, amount => -15.0, description => <<"RENOUVELLEMENT ANTICIPE CARTE BANCAIRE">>, effective_date => {2020,5,24}, id => <<"12531">>,type => bank_fees}
  ],

  ct:comment("Verify transactions count"),
  NbrExpectedTransactions = length(ExpectedTransactions),
  NbrExpectedTransactions = length(Transactions),
  lists:foreach(fun({E1,R1}) -> E1 = R1 end, lists:zip(ExpectedTransactions, Transactions)),

  ct:comment("Verify banks_fetch_http and monitoring alls"),
  true = meck:validate(banks_fetch_http),
  true = meck:validate(prometheus_counter),

  NbrHttpExpectations = meck:num_calls(banks_fetch_http, request, '_'),
  2 = meck:num_calls(prometheus_counter, inc, '_'),

  ok.


%%
%% Fetch transactions until last transaction fetch
%%
should_fetch_transactions_until_without_net(Config) ->
  ct:comment("Load transactions examples"),
  {ok, TransactionsJSON_1} = file:read_file(filename:join([?config(data_dir, Config), "transactions_1.json"])),

  FakeToken = fake_authtoken,
  FakeAccountId = <<"FAKEACCOUNT">>,

  meck:expect(prometheus_counter, inc,
              [
               {['bank_ing_transactions_total_count'],ok},
               {['bank_ing_transactions_ok_count'],ok}
              ]),

  URL_1 = "https://m.ing.fr/secure/api-v1/accounts/" ++ binary_to_list(FakeAccountId) ++ "/transactions/after/0/limit/50",

  HttpExpectations = [
                      {[get, {URL_1, [{"ingdf-auth-token", FakeToken}|'_']}, [], []],
                       {ok, {{'fakeversion', 200, 'fakereason'}, 'fakeheaders', binary_to_list(TransactionsJSON_1)}}
                      }
                     ],
  NbrHttpExpectations = length(HttpExpectations),

  meck:expect(banks_fetch_http, set_options, fun(MockOptions) -> [{cookies,enabled}] = MockOptions, ok end),
  meck:expect(banks_fetch_http, request, HttpExpectations),

  ct:comment("Fetch transactions"),
  {ok, Transactions} = banks_fetch_bank_ing:fetch_transactions({bank_auth, banks_fetch_bank_ing, FakeToken}, {account_id, FakeAccountId}, {transaction_id, <<"12819">>}),

  ExpectedTransactions =
  [
   #{accounting_date => {2020,6,16}, amount => -34.0,description => <<"PRLV SEPA XXX">>, effective_date => {2020,6,16}, id => <<"12875">>,type => sepa_debit},
   #{accounting_date => {2020,6,16}, amount => -51.94, description => <<"PAIEMENT PAR CARTE 14/06/2020 XXX">>, effective_date => {2020,6,16}, id => <<"12854">>,type => card_debit},
   #{accounting_date => {2020,6,16}, amount => -116.32, description => <<"VIREMENT SEPA EMIS VERS  123456789 XXX">>, effective_date => {2020,6,16}, id => <<"12850">>,type => transfer},
   #{accounting_date => {2020,6,15}, amount => -50.0, description => <<"PAIEMENT D'UN CHÈQUE 9999999"/utf8>>, effective_date => {2020,6,15}, id => <<"12848">>,type => check},
   #{accounting_date => {2020,6,15}, amount => -118.0,description => <<"PRLV SEPA XXX">>, effective_date => {2020,6,15}, id => <<"12843">>,type => sepa_debit}
  ],

  ct:comment("Verify transactions count"),
  NbrExpectedTransactions = length(ExpectedTransactions),
  NbrExpectedTransactions = length(Transactions),
  lists:foreach(fun({E1,R1}) -> E1 = R1 end, lists:zip(ExpectedTransactions, Transactions)),

  ct:comment("Verify banks_fetch_http and prometheus calls"),
  true = meck:validate(banks_fetch_http),
  true = meck:validate(prometheus_counter),
  NbrHttpExpectations = meck:num_calls(banks_fetch_http, request, '_'),
  2 = meck:num_calls(prometheus_counter, inc, '_'),

  ok.

%%
%% Fetch transactions (single result case)
%%
should_fetch_transactions_single_case_without_net(_Config) ->
  TransactionsJSON_1 = <<"[{ \"id\": \"12875\", \"effectiveDate\": \"2020-06-16\", \"accountingDate\": \"2020-06-16\", \"detail\": \"INTÉRÊTS PAYÉS\", \"amount\": 34.25, \"transcodeNeedCustomerAction\": false, \"type\": \"OTHER\", \"isOldBankCode\": false, \"sameMonthAsPrevious\": false, \"sameDateAsPrevious\": false, \"sameDateAsNext\": true }]"/utf8>>,
  TransactionsJSON_2 = <<"[]">>,

  FakeToken = fake_authtoken,
  FakeAccountId = <<"FAKEACCOUNT">>,

  meck:expect(prometheus_counter, inc,
              [
               {['bank_ing_transactions_total_count'],ok},
               {['bank_ing_transactions_ok_count'],ok}
              ]),
  meck:expect(banks_fetch_http, set_options, fun(MockOptions) -> [{cookies,enabled}] = MockOptions, ok end),

  URL_1 = "https://m.ing.fr/secure/api-v1/accounts/" ++ binary_to_list(FakeAccountId) ++ "/transactions/after/0/limit/50",
  URL_2 = "https://m.ing.fr/secure/api-v1/accounts/" ++ binary_to_list(FakeAccountId) ++ "/transactions/after/12875/limit/50",

  HttpExpectations = [
                      {[get, {URL_1, [{"ingdf-auth-token", FakeToken}|'_']}, [], []],
                       {ok, {{'fakeversion', 200, 'fakereason'}, 'fakeheaders', binary_to_list(TransactionsJSON_1)}}
                      },
                      {[get, {URL_2, [{"ingdf-auth-token", FakeToken}|'_']}, [], []],
                       {ok, {{'fakeversion', 200, 'fakereason'}, 'fakeheaders', binary_to_list(TransactionsJSON_2)}}
                      }
                     ],
  NbrHttpExpectations = length(HttpExpectations),

  meck:expect(banks_fetch_http, request, HttpExpectations),

  ct:comment("Fetch transactions"),
  {ok, Transactions} = banks_fetch_bank_ing:fetch_transactions({bank_auth, banks_fetch_bank_ing, FakeToken}, {account_id, FakeAccountId}, first_call),

  ExpectedTransactions = [
                          #{accounting_date => {2020,6,16}, amount => 34.25,description => <<"INTÉRÊTS PAYÉS"/utf8>>, effective_date => {2020,6,16}, id => <<"12875">>,type => interests}
                         ],

  ct:comment("Verify transactions count"),
  NbrExpectedTransactions = length(ExpectedTransactions),
  NbrExpectedTransactions = length(Transactions),
  lists:foreach(fun({E1,R1}) -> E1 = R1 end, lists:zip(ExpectedTransactions, Transactions)),

  ct:comment("Verify banks_fetch_http and prometheus calls"),
  true = meck:validate(banks_fetch_http),
  true = meck:validate(prometheus_counter),
  NbrHttpExpectations = meck:num_calls(banks_fetch_http, request, '_'),
  2 = meck:num_calls(prometheus_counter, inc, '_'),

  ok.




%%
%% Test with real credential
%%
test_with_real_credential(Config) ->
  {client_info, {ClientId, ClientCredential}} = lists:keyfind(client_info,1, Config),

  ct:comment("Setup"),
  ok = banks_fetch_http:setup(),
  ok = banks_fetch_bank:setup(banks_fetch_bank_ing),

  ct:comment("Connect to ing account"),
  {ok, Auth} = banks_fetch_bank_ing:connect(ClientId, ClientCredential),

  {bank_auth, banks_fetch_bank_ing, _AuthToken} = Auth,

  ct:comment("Fetch accounts"),
  {ok, Accounts} = banks_fetch_bank_ing:fetch_accounts(Auth),
  true = length(Accounts) > 0,

  lists:foreach(fun(#{ id := AccountId, type := Type } = Account) ->
                    ct:comment("Fetch transactions for account ~1000p", [Account]),
                    {ok, Transactions} = banks_fetch_bank_ing:fetch_transactions(Auth, {account_id, AccountId}, first_call),
                    NbrTransactions = length(Transactions),
                    ct:pal("~p : ~B transactions fetched", [Account, NbrTransactions]),
                    if Type =/= home_loan -> true = NbrTransactions > 0;
                       true -> ok
                    end
                end, Accounts),
  ok.
