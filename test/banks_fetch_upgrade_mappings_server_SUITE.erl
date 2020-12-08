-module(banks_fetch_upgrade_mappings_server_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2 ]).
-export([
         should_handle_cast_do_nothing/1,
         should_fetch_mappings_without_http_storage/1,
         should_fetch_mappings_without_storage/1,
         should_test_local_mappings/1,
         should_fetch_mappings_network_error_without_http_storage/1
        ]).

-define(FAKE_DATA_JSON, binary_to_list(<<"{\"budgets\": [{\"id\": 1, \"name\": \"Courant\"}], \"categories\": [{\"id\": 1, \"name\": \"Alimentation\", \"up_category_id\": null},{\"id\": 2, \"name\": \"Supermarché\", \"up_category_id\": 1}], \"stores\": [{\"id\": 1, \"name\": \"LIDL\"}], \"mappings\": [{\"id\": 1, \"pattern\": \"(LIDL ST GERMAIN|LIDL CHAMBOURCY|2635 ST GERMAIN)($| )\", \"fix_date\": \"none\", \"period\": \"month\", \"budget_id\": 1, \"categories_id\": [1,2], \"store_id\": 1}]}"/utf8>>)).
-define(FAKE_DATA_BUDGETS, [#{ id => 1, name => <<"Courant">>} ]).
-define(FAKE_DATA_CATEGORIES, [#{ id => 1, name => <<"Alimentation">>, up_category_id => none }, #{ id => 2, name => <<"Supermarché"/utf8>>, up_category_id => 1 }]).
-define(FAKE_DATA_STORES, [#{ id => 1, name => <<"LIDL">> }]).
-define(FAKE_DATA_MAPPINGS, [#{ id => 1, pattern => <<"(LIDL ST GERMAIN|LIDL CHAMBOURCY|2635 ST GERMAIN)($| )">>, fix_date => none, period => month,
                                budget_id => 1, categories_id => [1,2], store_id => 1}]).

all() -> [
          should_handle_cast_do_nothing,
          should_fetch_mappings_without_http_storage,
          should_fetch_mappings_without_storage,
          should_fetch_mappings_network_error_without_http_storage,
          should_test_local_mappings
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
init_per_testcase(should_test_local_mappings, Config) ->
  case file:read_file(filename:join([?config(data_dir, Config), "../../../../../../resources/mappings.json"])) of
    {error, enoent} ->
      {skip, "You may add a link to mappings.json to test it"};
    {ok, Data} ->
      meck:new(banks_fetch_http),
      meck:new(banks_fetch_storage),
      meck:new(timer, [unstick,passthrough]), % unstick because timer resides in sticky dir, passthrough because lager needs now_diff
      [{mappings_json_data, binary_to_list(Data)}|Config]
  end;
init_per_testcase(should_fetch_mappings_without_storage, Config) ->
  meck:new(banks_fetch_storage),
  meck:new(timer, [unstick,passthrough]), % unstick because timer resides in sticky dir, passthrough because lager needs now_diff
  ok = application:start(prometheus),
  Config;
init_per_testcase(should_fetch_mappings_network_error_without_http_storage, Config) ->
  meck:new(banks_fetch_http),
  meck:new(banks_fetch_storage),
  meck:new(timer, [unstick,passthrough]), % unstick because timer resides in sticky dir, passthrough because lager needs now_diff
  Config.

end_per_testcase(should_handle_cast_do_nothing, _Config) ->
  ok;
end_per_testcase(should_fetch_mappings_without_http_storage, _Config) ->
  meck:unload(timer),
  meck:unload(banks_fetch_storage),
  meck:unload(banks_fetch_http),
  ok;
end_per_testcase(should_test_local_mappings, _Config) ->
  meck:unload(timer),
  meck:unload(banks_fetch_storage),
  meck:unload(banks_fetch_http),
  ok;
end_per_testcase(should_fetch_mappings_without_storage, _Config) ->
  ok = application:stop(prometheus),
  meck:unload(timer),
  meck:unload(banks_fetch_storage),
  ok;
end_per_testcase(should_fetch_mappings_network_error_without_http_storage, _Config) ->
  meck:unload(timer),
  meck:unload(banks_fetch_storage),
  meck:unload(banks_fetch_http),
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

  ct:comment("Verify that expected functions have been called"),
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
                                                         check_data(Budgets, none),
                                                         check_data(Categories, none),
                                                         check_data(Stores, none),
                                                         check_data(Mappings, none),
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


should_test_local_mappings(Config) ->
  BeforeFetchingDateTime = calendar:universal_time(),

  {mappings_json_data, MappingsData} = lists:keyfind(mappings_json_data, 1, Config),

  meck:expect(banks_fetch_http, setup_monitoring, fun(MockServername) ->
                                                      "raw.githubusercontent.com" = MockServername,
                                                      ok
                                                  end),
  meck:expect(banks_fetch_http, request, fun(MockMethod, MockURL, MockHTTPOptions, MockOptions) ->
                                             get = MockMethod,
                                             {_, []} = MockURL,
                                             [] = MockHTTPOptions,
                                             [] = MockOptions,
                                             {ok, {{"HTTP/1.1", 200, <<"OK">>}, [], MappingsData}}
                                         end),

  meck:expect(timer, send_after, fun(MockTime, MockCall) ->
                                     24 * 60 * 60 * 1000 = MockTime,
                                     fetch_mappings = MockCall,
                                     {ok, tref}
                                 end),

  meck:expect(banks_fetch_storage, upgrade_mappings, fun(MockBudgets, MockCategories, MockStores, MockMappings) ->
                                                         check_data(MockBudgets, 5),
                                                         check_data(MockCategories, 171),
                                                         check_data(MockStores, 182),
                                                         check_data(MockMappings, 188),
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



check_data(List, ExpectedCount) ->
  Nbr = length(List),
  if ExpectedCount =/= none -> ExpectedCount = Nbr;
     true -> ct:log("Nbr elements: ~B", [Nbr]), ok
  end,
  Ids = extract_ids(List),
  UniqueIds = lists:usort(Ids), % check id unicity
  Nbr = length(UniqueIds).

extract_ids(L) ->
  [ ID || #{ id := ID } <- L ].


should_fetch_mappings_network_error_without_http_storage(_Config) ->
  meck:expect(banks_fetch_http, setup_monitoring, fun(MockServername) ->
                                                      "raw.githubusercontent.com" = MockServername,
                                                      ok
                                                  end),
  meck:expect(banks_fetch_http, request, fun(MockMethod, MockURL, MockHTTPOptions, MockOptions) ->
                                             get = MockMethod,
                                             {_, []} = MockURL,
                                             [] = MockHTTPOptions,
                                             [] = MockOptions,
                                             {error, network_error}
                                         end),

  meck:expect(timer, send_after, fun(MockTime, MockCall) ->
                                     2 * 60 * 60 * 1000 = MockTime,
                                     fetch_mappings = MockCall,
                                     {ok, tref}
                                 end),

  {ok, UpgradeServerPid} = banks_fetch_upgrade_mappings_server:start_link(),

  ct:comment("Verify that last_fetch datetime has been updated"),
  LastFetch = banks_fetch_upgrade_mappings_server:last_fetch(UpgradeServerPid),
  none = LastFetch,

  ct:comment("Verify that expected functions have been called"),
  true = meck:validate(banks_fetch_http),
  true = meck:validate(banks_fetch_storage),
  true = meck:validate(timer),

  ok.


