-module(banks_fetch_http_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([
         all/0,
         init_per_testcase/2,
         end_per_testcase/2,

         should_call_set_options/1,
         should_call_request/1
        ]).

all() ->
  [
   should_call_set_options,
   should_call_request
  ].

init_per_testcase(should_call_set_options, Config) ->
  meck:new(httpc),
  Config;
init_per_testcase(should_call_request, Config) ->
  meck:new(httpc),
  Config.

end_per_testcase(should_call_set_options, _Config) ->
  meck:unload(httpc),
  ok;
end_per_testcase(should_call_request, _Config) ->
  meck:unload(httpc),
  ok.

should_call_set_options(_Config) ->
  Options = [{cookies, enabled}],
  meck:expect(httpc, set_options, fun(MockOptions) -> Options = MockOptions, ok end),

  ct:comment("Call set_options"),
  ok = banks_fetch_http:set_options(Options),

  ct:comment("Verify httpc"),
  true = meck:validate(httpc),

  ok.

should_call_request(_Config) ->
  Method = get,
  Request = {"https://m.ing.fr/", none},
  HttpOptions = [],
  Options = [],
  Result = {ok, fake_result},

  meck:expect(httpc, request, fun(MockMethod, MockRequest, MockHttpOptions, MockOptions) -> 
                                  Method = MockMethod,
                                  Request = MockRequest,
                                  HttpOptions = MockHttpOptions,
                                  Options = MockOptions,
                                  Result
                              end),

  ct:comment("Call request"),
  Result = banks_fetch_http:request(Method, Request, HttpOptions, Options),

  ct:comment("Verify httpc"),
  true = meck:validate(httpc),

  ok.
