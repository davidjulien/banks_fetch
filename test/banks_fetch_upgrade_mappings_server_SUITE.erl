-module(banks_fetch_upgrade_mappings_server_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2 ]).
-export([
         should_handle_cast_do_nothing/1,
         should_fetch_mappings_without_http_storage/1,
         should_fetch_mappings_without_storage/1
        ]).

-define(FAKE_DATA_JSON, binary_to_list(<<"{\"budgets\": [{\"id\": 1, \"name\": \"Courant\"}], \"categories\": [{\"id\": 1, \"name\": \"Alimentation\", \"up_category_id\": null},{\"id\": 2, \"name\": \"Supermarché\", \"up_category_id\": 1}], \"stores\": [{\"id\": 1, \"name\": \"LIDL\"}], \"mappings\": [{\"id\": 1, \"pattern\": \"(LIDL ST GERMAIN|LIDL CHAMBOURCY|2635 ST GERMAIN)($| )\", \"fix_date\": \"none\", \"period\": \"month\", \"budget_id\": 1, \"categories_id\": [1,2], \"store_id\": 1}]}"/utf8>>)).
-define(FAKE_DATA_BUDGETS, [#{ id => 1, name => <<"Courant">>} ]).
-define(FAKE_DATA_CATEGORIES, [#{ id => 1, name => <<"Alimentation">>, up_category_id => null }, #{ id => 2, name => <<"Supermarché"/utf8>>, up_category_id => 1 }]).
-define(FAKE_DATA_STORES, [#{ id => 1, name => <<"LIDL">> }]).
-define(FAKE_DATA_MAPPINGS, [#{ id => 1, pattern => <<"(LIDL ST GERMAIN|LIDL CHAMBOURCY|2635 ST GERMAIN)($| )">>, fix_date => none, period => month, 
                                budget_id => 1, categories_id => [1,2], store_id => 1}]).

all() -> [
          should_handle_cast_do_nothing,
          should_fetch_mappings_without_http_storage,
          should_fetch_mappings_without_storage
         ].

init_per_suite(Config) ->
  ok = lager:start(),
  Config.

end_per_suite(_Config) ->
  application:stop(lager).

init_per_testcase(should_handle_cast_do_nothing, Config) ->
  Config;
init_per_testcase(should_fetch_mappings_without_http_storage, Config) ->
  meck:new(banks_fetch_http),
  meck:new(banks_fetch_storage),
  meck:new(timer, [unstick,passthrough]), % unstick because timer resides in sticky dir, passthrough because lager needs now_diff
  Config;
init_per_testcase(should_fetch_mappings_without_storage, Config) ->
  meck:new(banks_fetch_storage),
  meck:new(timer, [unstick,passthrough]), % unstick because timer resides in sticky dir, passthrough because lager needs now_diff
  ok = application:start(prometheus),
  Config.

end_per_testcase(should_handle_cast_do_nothing, _Config) ->
  ok;
end_per_testcase(should_fetch_mappings_without_http_storage, _Config) ->
  meck:unload(timer),
  meck:unload(banks_fetch_storage),
  meck:unload(banks_fetch_http),
  ok;
end_per_testcase(should_fetch_mappings_without_storage, _Config) ->
  ok = application:stop(prometheus),
  meck:unload(timer),
  meck:unload(banks_fetch_storage),
  ok.



should_handle_cast_do_nothing(_Config) ->
  {noreply, dummystate} = banks_fetch_upgrade_mappings_server:handle_cast(dummycall, dummystate),
  ok.

should_fetch_mappings_without_http_storage(_Config) ->
  BeforeFetchingDateTime = calendar:universal_time(),

  meck:expect(banks_fetch_http, setup_monitoring, fun(MockServername) ->
                                                      "raw.githubusercontent.com" = MockServername,
                                                      ok
                                                  end),
  meck:expect(banks_fetch_http, request, fun(MockMethod, MockURL, MockHTTPOptions, MockOptions) ->
                                             get = MockMethod,
                                             {_, []} = MockURL,
                                             [] = MockHTTPOptions,
                                             [] = MockOptions,
                                             {ok, {{"HTTP/1.1", 200, <<"OK">>}, [], ?FAKE_DATA_JSON}}
                                         end),

  meck:expect(timer, send_after, fun(MockTime, MockCall) ->
                                     24 * 60 * 60 * 1000 = MockTime,
                                     fetch_mappings = MockCall,
                                     {ok, tref}
                                 end),

  ExpectedBudgets = ?FAKE_DATA_BUDGETS,
  ExpectedCategories = ?FAKE_DATA_CATEGORIES,
  ExpectedStores = ?FAKE_DATA_STORES,
  ExpectedMappings = ?FAKE_DATA_MAPPINGS,
  meck:expect(banks_fetch_storage, upgrade_mappings, fun(MockBudgets, MockCategories, MockStores, MockMappings) -> 
                                                         error_logger:info_msg("E=~p", [ExpectedCategories]),
                                                         error_logger:info_msg("M=~p", [MockCategories]),
                                                         ExpectedBudgets = MockBudgets,
                                                         ExpectedCategories = MockCategories,
                                                         ExpectedStores = MockStores,
                                                         ExpectedMappings = MockMappings,
                                                         ok 
                                                     end),

  {ok, UpgradeServerPid} = banks_fetch_upgrade_mappings_server:start_link(),

  ct:comment("Verify that last_fetch datetime has been updated"),
  LastFetch = banks_fetch_upgrade_mappings_server:last_fetch(UpgradeServerPid),
  true = LastFetch =/= none andalso LastFetch >= BeforeFetchingDateTime,

  ct:comment("Verify that expected functions has been called"),
  true = meck:validate(banks_fetch_http),
  true = meck:validate(banks_fetch_storage),
  true = meck:validate(timer),

  ok.

should_fetch_mappings_without_storage(_Config) ->
  BeforeFetchingDateTime = calendar:universal_time(),

  ct:comment("Setup"),
  ok = banks_fetch_http:setup(),

  meck:expect(timer, send_after, fun(MockTime, MockCall) ->
                                     24 * 60 * 60 * 1000 = MockTime,
                                     fetch_mappings = MockCall,
                                     {ok, tref}
                                 end),

  meck:expect(banks_fetch_storage, upgrade_mappings, fun(Budgets, Categories, Stores, Mappings) -> 
                                                         5 = length(Budgets),
                                                         2 = length(Categories),
                                                         15 = length(Stores),
                                                         15 = length(Mappings),
                                                         ok 
                                                     end),

  {ok, UpgradeServerPid} = banks_fetch_upgrade_mappings_server:start_link(),

  %ct:comment("Wait send_after is set"),
  %meck:wait(timer, send_after, '_', 5000),

  ct:comment("Verify that last_fetch datetime has been updated"),
  LastFetch = banks_fetch_upgrade_mappings_server:last_fetch(UpgradeServerPid),
  true = LastFetch =/= none andalso LastFetch >= BeforeFetchingDateTime,

  ct:comment("Verify that expected functions has been called"),
  true = meck:validate(banks_fetch_storage),
  true = meck:validate(timer),

  ok.