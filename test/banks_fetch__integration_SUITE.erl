-module(banks_fetch__integration_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([
         all/0,
         init_per_suite/1,
         end_per_suite/1,

         init_per_testcase/2,
         end_per_testcase/2,

         should_fetch_data_from_a_bank_and_store_them/1
        ]).

all() ->
  [
   should_fetch_data_from_a_bank_and_store_them
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

-define(DB_NAME, "banks_fetch_test").
-define(DB_USER, "banks_fetch_test").
-define(DB_PASSWORD, "").

init_per_testcase(should_fetch_data_from_a_bank_and_store_them, Config) ->
  ct:comment("Load credential"),
  case file:consult(filename:join([?config(data_dir, Config), "real_credential.hrl"])) of
    {error, enoent} ->
      {skip, "Credential is required to test full integration"};
    {ok, []} ->
      {skip, "Credential is required to test full integration"};
    {ok, [{BankId, ClientId, ClientCredential}]} ->
      ct:comment("Start pgsql"),
      ok = application:start(pgsql),

      ct:comment("Start prometheus"),
      ok = application:start(prometheus),

      ct:comment("Reset test database"),
      Connection = pgsql_connection:open("127.0.0.1", "postgres", ?DB_USER, ?DB_PASSWORD),

      DropTestDbQuery = "DROP DATABASE IF EXISTS " ?DB_NAME,
      {{drop, _}, []} = pgsql_connection:simple_query(DropTestDbQuery, Connection),
      CreateTestDbQuery = "CREATE DATABASE " ?DB_NAME " WITH OWNER " ?DB_USER,
      {{create, _}, []} = pgsql_connection:simple_query(CreateTestDbQuery, [], Connection),
      pgsql_connection:close(Connection),

      ok = application:set_env(banks_fetch, storage, {?DB_NAME, ?DB_USER, ?DB_PASSWORD}),

      ct:comment("Start banks_fetch application"),
      ok = application:start(banks_fetch),

      banks_fetch_http:setup(),
      banks_fetch_bank_ing:setup(),

      % Mecking banks_fetch_storage allows to wait for store_accounts call
      ok = meck:new(banks_fetch_storage, [passthrough]),

      [{client_info, {BankId, ClientId, ClientCredential}}|Config]
  end.

end_per_testcase(should_fetch_data_from_a_bank_and_store_them, _Config) ->
  meck:unload(banks_fetch_storage),
  application:stop(banks_fetch),
  application:stop(prometheus),
  application:stop(pgsql),
  ok.


should_fetch_data_from_a_bank_and_store_them(Config) ->
  {client_info, {BankId, ClientId, ClientCredential}} = lists:keyfind(client_info, 1, Config),

  ct:comment("Add new client"),
  ok = banks_fetch_client_manager:add_client(BankId, ClientId, ClientCredential),

  ct:comment("Wait for accounts storing"),
  ok = meck:wait(banks_fetch_storage, store_accounts, '_', 20000),

  ct:comment("Verify accounts data"),
  {ok, Accounts} = banks_fetch_storage:get_accounts(BankId, ClientId),
  [_|_] = Accounts,

  ok.
