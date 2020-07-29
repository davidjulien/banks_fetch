-module(banks_fetch_http_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([
         all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,

         should_call_set_options/1,
         should_call_request_status_200/1,
         should_call_request_status_300/1,
         should_call_request_error/1
        ]).

all() ->
  [
   should_call_set_options,
   {group, call_request}
  ].

groups() ->
  [{call_request, [],
    [
     should_call_request_status_200,
     should_call_request_status_300,
     should_call_request_error
    ]}].

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

init_per_testcase(should_call_set_options, Config) ->
  meck:new(httpc),
  Config;
init_per_testcase(Test, Config) when Test =:= should_call_request_status_200 orelse Test =:= should_call_request_status_300 orelse Test =:= should_call_request_error ->
  meck:new(httpc),
  meck:new(prometheus_counter),
  Config.

end_per_testcase(should_call_set_options, _Config) ->
  meck:unload(httpc),
  ok;
end_per_testcase(Test, _Config) when Test =:= should_call_request_status_200 orelse Test =:= should_call_request_status_300 orelse Test =:= should_call_request_error ->
  meck:unload(prometheus_counter),
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

should_call_request_status_200(_Config) ->
  Method = get,
  Request = {"https://m.ing.fr/", none},
  HttpOptions = [],
  Options = [],
  Result = {ok, {{"HTTP/1.1", 200, ""}, [], ""}},

  meck:expect(httpc, request, fun(MockMethod, MockRequest, MockHttpOptions, MockOptions) ->
                                  Method = MockMethod,
                                  Request = MockRequest,
                                  HttpOptions = MockHttpOptions,
                                  Options = MockOptions,
                                  Result
                              end),
  ExpectedCounterInc = [
                        {['http_all_requests_call_count'], ok},
                        {['http_m_ing_fr_requests_call_count'], ok},

                        {['http_all_requests_ok_count'], ok},
                        {['http_m_ing_fr_requests_ok_count'], ok},

                        {['http_all_results_status_200_count'], ok},
                        {['http_m_ing_fr_results_status_200_count'], ok}
                       ],
  NbrExpectedCounterInc = length(ExpectedCounterInc),
  meck:expect(prometheus_counter, inc, ExpectedCounterInc),

  ct:comment("Call request"),
  Result = banks_fetch_http:request(Method, Request, HttpOptions, Options),

  ct:comment("Verify httpc"),
  true = meck:validate(httpc),
  true = meck:validate(prometheus_counter),
  NbrExpectedCounterInc = meck:num_calls(prometheus_counter, inc, '_'),

  ok.

should_call_request_status_300(_Config) ->
  Method = get,
  Request = {"https://m.ing.fr/", none},
  HttpOptions = [],
  Options = [],
  Result = {ok, {{"HTTP/1.1", 300, ""}, [], ""}},

  meck:expect(httpc, request, fun(MockMethod, MockRequest, MockHttpOptions, MockOptions) ->
                                  Method = MockMethod,
                                  Request = MockRequest,
                                  HttpOptions = MockHttpOptions,
                                  Options = MockOptions,
                                  Result
                              end),
  ExpectedCounterInc = [
                        {['http_all_requests_call_count'], ok},
                        {['http_m_ing_fr_requests_call_count'], ok},

                        {['http_all_requests_ok_count'], ok},
                        {['http_m_ing_fr_requests_ok_count'], ok},

                        {['http_all_results_status_other_count'], ok},
                        {['http_m_ing_fr_results_status_other_count'], ok}
                       ],
  NbrExpectedCounterInc = length(ExpectedCounterInc),
  meck:expect(prometheus_counter, inc, ExpectedCounterInc),

  ct:comment("Call request"),
  Result = banks_fetch_http:request(Method, Request, HttpOptions, Options),

  ct:comment("Verify httpc"),
  true = meck:validate(httpc),
  true = meck:validate(prometheus_counter),
  NbrExpectedCounterInc = meck:num_calls(prometheus_counter, inc, '_'),

  ok.


should_call_request_error(_Config) ->
  Method = get,
  Request = {"https://m.ing.fr/", none},
  HttpOptions = [],
  Options = [],
  Result = {error, invalid_request},

  meck:expect(httpc, request, fun(MockMethod, MockRequest, MockHttpOptions, MockOptions) ->
                                  Method = MockMethod,
                                  Request = MockRequest,
                                  HttpOptions = MockHttpOptions,
                                  Options = MockOptions,
                                  Result
                              end),
  ExpectedCounterInc = [
                        {['http_all_requests_call_count'], ok},
                        {['http_m_ing_fr_requests_call_count'], ok},

                        {['http_all_requests_error_count'], ok},
                        {['http_m_ing_fr_requests_error_count'], ok}
                       ],
  NbrExpectedCounterInc = length(ExpectedCounterInc),
  meck:expect(prometheus_counter, inc, ExpectedCounterInc),

  ct:comment("Call request"),
  Result = banks_fetch_http:request(Method, Request, HttpOptions, Options),

  ct:comment("Verify httpc"),
  true = meck:validate(httpc),
  true = meck:validate(prometheus_counter),
  NbrExpectedCounterInc = meck:num_calls(prometheus_counter, inc, '_'),

  ok.
