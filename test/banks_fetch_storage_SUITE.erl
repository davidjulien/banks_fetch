% CREATE DATABASE banks_fetch_test;
% CREATE ROLE banks_fetch_test WITH LOGIN;
% ALTER USER banks_fetch_test WITH SUPERUSER;
% GRANT ALL PRIVILEGES ON DATABASE banks_fetch_test TO banks_fetch_test;
% ALTER DATABASE banks_fetch_test OWNER TO banks_fetch_test;

-module(banks_fetch_storage_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([
         all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         init_per_group/2,
         end_per_group/2,

         should_handle_cast_do_nothing/1,

         should_nodb_start_without_db_upgrade/1,
         should_nodb_start_with_db_upgrade/1,
         should_nodb_start_with_db_upgrade_error/1,
         should_nodb_get_clients/1,
         should_nodb_insert_client/1,
         should_nodb_store_accounts/1,

         should_db_get_banks/1,
         should_db_get_budgets/1,
         should_db_get_categories/1,

         should_db_get_stores/1,
         should_db_insert_store/1,
         should_db_not_insert_store_already_existing/1,

         should_db_get_clients/1,
         should_db_insert_client/1,
         should_db_not_insert_client_already_existing/1,

         should_db_store_accounts/1,
         should_db_get_accounts/1,
         should_db_get_all_accounts/1,
         should_db_store_transactions/1,
         should_db_get_transactions/1,
         should_db_get_last_transactions/1,
         should_db_get_last_transactions_empty/1,
         should_db_get_last_transactions_invalid_cursor/1,
         should_db_get_last_transactions_id/1,
         should_db_update_transaction/1,
         should_db_update_transaction_with_amount/1,
         should_db_update_transaction_with_amount_fails_because_not_subtransaction/1,
         should_db_update_transaction_with_amount_fails_because_remaining/1,
         should_db_split_transaction/1,
         should_db_split_transaction_fails_because_not_found/1,
         should_db_upgrade_mappings_empty/1,
         should_db_upgrade_mappings_do_not_update_manual_updates/1,
         should_db_upgrade_mappings_identical/1,
         should_db_upgrade_mappings_updates/1,
         should_db_upgrade_mappings_invalid_updates/1
        ]).

-define(DB_NAME, "banks_fetch_test").
-define(DB_USER, "banks_fetch_test").
-define(DB_PASSWORD, "banks_fetch_test").

-define(BANK_ID_1, <<"ing">>).
-define(CLIENT_ID_1, <<"client1">>).
-define(CLIENT_CREDENTIAL_1, {<<"credential1">>}).
-define(FETCHING_AT_1, {{2020,7,7},{12,0,0}}).
-define(ACCOUNT_ID_1, <<"account1">>).
-define(ACCOUNT_ID_2, <<"account2">>).
-define(ACCOUNT_ID_3, <<"account3">>).
% data used to test store_accounts
-define(ACCOUNTS_1, [
                     #{ id => ?ACCOUNT_ID_1, balance => 234.12, number => <<"number1">>, owner => <<"owner1">>, ownership => single, type => current, name => <<"CURRENT">>, bank_id => <<"ing">>, client_id => <<"client1">> },
                     #{ id => ?ACCOUNT_ID_2, balance => 4321.78, number => <<"number2">>, owner => <<"owner2">>, ownership => single, type => savings, name => <<"LDD">>, bank_id => <<"ing">>, client_id => <<"client1">> }
                    ]).
-define(ACCOUNTS_2, [
                     #{ id => ?ACCOUNT_ID_3, balance => 431.80, number => <<"number3">>, owner => <<"owner3">>, ownership => single, type => savings, name => <<"LDD">>, bank_id => <<"ing">>, client_id => <<"client2">> }
                    ]).

% data used to test store_transactions
-define(TRANSACTIONS_1,
        [
         #{ id => <<"TRANSACTION_5">>, accounting_date => {2020,7,23}, effective_date => {2020,7,23}, amount => -900.09, description => <<"PAIEMENT PAR CARTE 22/07/2020 CHARGES">>, type => transfer },
         #{ id => <<"TRANSACTION_4">>, accounting_date => {2020,7,23}, effective_date => {2020,7,23}, amount => -4.26, description => <<"URSSAF 120120">>, type => transfer },
         #{ id => <<"TRANSACTION_3">>, accounting_date => {2020,7,23}, effective_date => {2020,7,23}, amount => -64.26, description => <<"PAIEMENT PAR CARTE 20/07/2020 PETITEBOUTIQUE">>, type => card_debit },
         #{ id => <<"TRANSACTION_2">>, accounting_date => {2020,7,22}, effective_date => {2020,7,22}, amount => -14.32, description => <<"PRLV SEPA XXX">>, type => sepa_debit },
         #{ id => <<"TRANSACTION_1">>, accounting_date => {2020,7,20}, effective_date => {2020,7,20}, amount => -34.32, description => <<"PAIEMENT PAR CARTE 20/07/2020 XXX">>, type => card_debit }
        ]).

-define(TRANSACTION_1_STORED,
        [
         {?BANK_ID_1, ?CLIENT_ID_1, ?ACCOUNT_ID_1, ?FETCHING_AT_1, <<"TRANSACTION_5">>, {2020,7,23}, {2020,7,23}, -900.09, <<"PAIEMENT PAR CARTE 22/07/2020 CHARGES">>, {e_transaction_type, <<"transfer">>},
          {2020,8,1}, 3, {e_period, <<"quarter">>}, 1, {array, [1,42]}, 4}, % next month, for one quarter
         {?BANK_ID_1, ?CLIENT_ID_1, ?ACCOUNT_ID_1, ?FETCHING_AT_1, <<"TRANSACTION_4">>, {2020,7,23}, {2020,7,23}, -4.26, <<"URSSAF 120120">>, {e_transaction_type, <<"transfer">>},
          {2020,6,30}, 2, {e_period, <<"month">>}, 1, {array, [1,42]}, 3}, % previous month
         {?BANK_ID_1, ?CLIENT_ID_1, ?ACCOUNT_ID_1, ?FETCHING_AT_1, <<"TRANSACTION_3">>, {2020,7,23}, {2020,7,23}, -64.26, <<"PAIEMENT PAR CARTE 20/07/2020 PETITEBOUTIQUE">>, {e_transaction_type, <<"card_debit">>},
          {2020,7,20}, 1, {e_period, <<"month">>}, 1, {array, [1,52]}, 2},
         {?BANK_ID_1, ?CLIENT_ID_1, ?ACCOUNT_ID_1, ?FETCHING_AT_1, <<"TRANSACTION_2">>, {2020,7,22}, {2020,7,22}, -14.32, <<"PRLV SEPA XXX">>, {e_transaction_type, <<"sepa_debit">>},
          {2020,7,22}, null, null, null, null, null},
         {?BANK_ID_1, ?CLIENT_ID_1, ?ACCOUNT_ID_1, ?FETCHING_AT_1, <<"TRANSACTION_1">>, {2020,7,20}, {2020,7,20}, -34.32, <<"PAIEMENT PAR CARTE 20/07/2020 XXX">>, {e_transaction_type, <<"card_debit">>},
          {2020,7,20}, null, null, null, null, null}
        ]).

-define(BANK_ID_2, <<"ing">>).
-define(CLIENT_ID_2, <<"client2">>).
-define(CLIENT_CREDENTIAL_2, {<<"credential2">>}).

all() ->
  [
   should_handle_cast_do_nothing,
   {group, tests_without_db},
   {group, tests_with_db}
  ].

groups() ->
  [
   {tests_without_db, [], [ should_nodb_start_without_db_upgrade, should_nodb_start_with_db_upgrade, should_nodb_start_with_db_upgrade_error, should_nodb_get_clients, should_nodb_insert_client, should_nodb_store_accounts ]},
   {tests_with_db, [], [ should_db_get_banks, should_db_get_budgets, should_db_get_categories,
                         should_db_get_stores, should_db_insert_store, should_db_not_insert_store_already_existing,
                         should_db_get_clients, should_db_insert_client, should_db_not_insert_client_already_existing,
                         should_db_store_accounts, should_db_get_accounts, should_db_get_all_accounts,
                         should_db_store_transactions, should_db_get_transactions, should_db_get_last_transactions, should_db_get_last_transactions_empty, should_db_get_last_transactions_invalid_cursor,
                         should_db_get_last_transactions_id, should_db_update_transaction, should_db_split_transaction, should_db_split_transaction_fails_because_not_found,
                         should_db_update_transaction_with_amount, should_db_update_transaction_with_amount_fails_because_not_subtransaction, should_db_update_transaction_with_amount_fails_because_remaining,
                         should_db_upgrade_mappings_empty, should_db_upgrade_mappings_do_not_update_manual_updates, should_db_upgrade_mappings_identical, should_db_upgrade_mappings_updates, should_db_upgrade_mappings_invalid_updates ]}
  ].

%%
%% Overall setup/teardwon
%%
init_per_suite(Config) ->
  ok = lager:start(),
  Config.

end_per_suite(_Config) ->
  application:stop(lager).


setup_database(Config) ->
  setup_database(Config, none).

setup_database(Config, FilenameOpt) ->
  ok = application:start(pgsql),
  Connection = pgsql_connection:open("127.0.0.1", "postgres", ?DB_USER, ?DB_PASSWORD),

  DropTestDbQuery = "DROP DATABASE IF EXISTS " ?DB_NAME,
  {{drop, _}, []} = pgsql_connection:simple_query(DropTestDbQuery, Connection),
  CreateTestDbQuery = "CREATE DATABASE " ?DB_NAME " WITH OWNER " ?DB_USER,
  {{create, _}, []} = pgsql_connection:simple_query(CreateTestDbQuery, [], Connection),

  pgsql_connection:close(Connection),

  DBConnection = pgsql_connection:open("127.0.0.1", ?DB_NAME, ?DB_USER, ?DB_PASSWORD),

  {ok, PID} = banks_fetch_storage:start_link({?DB_NAME,?DB_USER,?DB_PASSWORD}),
  receive after 500 -> nil end, % wait for db init

  case FilenameOpt of
    none -> ok;
    _ ->
      % Load queries to init database content
      ct:comment("Load file ~s", [FilenameOpt]),
      {ok, FileData} = file:read_file(filename:join([?config(data_dir, Config), FilenameOpt])),
      Queries = binary:split(FileData, <<"\n">>, [global]),
      ok = lists:foldl(fun(_Query, stop) -> stop;
                     (Query, ok) ->
                      case pgsql_connection:simple_query(Query, DBConnection) of
                        {error, _} = Err ->
                          error_logger:info_msg("Unable to setup_database for ~s:\n~p", [Query, Err]),
                          stop;
                        _ -> ok
                      end
                  end, ok, Queries)
  end,

  [{db_connection, DBConnection}, {storage_pid, PID} | Config].

teardown_database() ->
  ok = application:stop(pgsql),
  ok.

% Init per group
init_per_group(tests_without_db, Config) ->
  meck:new(pgsql_connection),
  Config;
init_per_group(tests_with_db, Config) ->
  Config.

% End per group
end_per_group(tests_without_db, _Config) ->
  meck:unload(pgsql_connection),
  ok;
end_per_group(tests_with_db, _Config) ->
  ok.


% Init per testcase
init_per_testcase(should_db_get_banks, Config) ->
  setup_database(Config);

init_per_testcase(should_db_get_budgets, Config) ->
  setup_database(Config, <<"setup_db_for_get_budgets.sql">>);

init_per_testcase(should_db_get_categories, Config) ->
  setup_database(Config, <<"setup_db_for_get_categories.sql">>);

init_per_testcase(should_db_get_stores, Config) ->
  setup_database(Config, <<"setup_db_for_get_stores.sql">>);

init_per_testcase(should_db_insert_store, Config) ->
  setup_database(Config, <<"setup_db_for_get_stores.sql">>);

init_per_testcase(should_db_not_insert_store_already_existing, Config) ->
  setup_database(Config, <<"setup_db_for_get_stores.sql">>);

init_per_testcase(should_db_get_clients, Config) ->
  setup_database(Config, <<"setup_db_for_get_clients.sql">>);

init_per_testcase(should_db_insert_client, Config) ->
  setup_database(Config);

init_per_testcase(should_db_not_insert_client_already_existing, Config) ->
  setup_database(Config, <<"setup_db_for_get_clients.sql">>);

init_per_testcase(should_db_store_accounts, Config) ->
  setup_database(Config, <<"setup_db_for_store_accounts.sql">>);

init_per_testcase(should_db_get_accounts, Config) ->
  setup_database(Config, <<"setup_db_for_get_accounts.sql">>);

init_per_testcase(should_db_get_all_accounts, Config) ->
  setup_database(Config, <<"setup_db_for_get_accounts.sql">>);

init_per_testcase(should_db_store_transactions, Config) ->
  setup_database(Config, <<"setup_db_for_store_transactions.sql">>);

init_per_testcase(should_db_get_transactions, Config) ->
  setup_database(Config, <<"setup_db_for_get_transactions.sql">>);

init_per_testcase(should_db_get_last_transactions, Config) ->
  setup_database(Config,"setup_db_for_get_last_transactions.sql");

init_per_testcase(should_db_get_last_transactions_empty, Config) ->
  setup_database(Config);

init_per_testcase(should_db_get_last_transactions_invalid_cursor, Config) ->
  setup_database(Config,"setup_db_for_get_last_transactions.sql");

init_per_testcase(should_db_update_transaction, Config) ->
  setup_database(Config,"setup_db_for_update_transaction.sql");

init_per_testcase(should_db_update_transaction_with_amount, Config) ->
  setup_database(Config,"setup_db_for_update_transaction_with_amount.sql");

init_per_testcase(should_db_update_transaction_with_amount_fails_because_not_subtransaction, Config) ->
  setup_database(Config,"setup_db_for_update_transaction_with_amount.sql");

init_per_testcase(should_db_update_transaction_with_amount_fails_because_remaining, Config) ->
  setup_database(Config,"setup_db_for_update_transaction_with_amount.sql");

init_per_testcase(should_db_split_transaction, Config) ->
  setup_database(Config,"setup_db_for_split_transaction.sql");

init_per_testcase(should_db_split_transaction_fails_because_not_found, Config) ->
  setup_database(Config,"setup_db_for_split_transaction.sql");

init_per_testcase(should_db_get_last_transactions_id, Config) ->
  setup_database(Config, <<"setup_db_for_get_last_transactions_id.sql">>);

init_per_testcase(should_db_upgrade_mappings_empty, Config) ->
  setup_database(Config, <<"setup_db_for_upgrade_mappings.sql">>);
init_per_testcase(should_db_upgrade_mappings_do_not_update_manual_updates, Config) ->
  setup_database(Config, <<"setup_db_for_upgrade_mappings.sql">>);
init_per_testcase(should_db_upgrade_mappings_identical, Config) ->
  setup_database(Config, <<"setup_db_for_upgrade_mappings.sql">>);
init_per_testcase(should_db_upgrade_mappings_updates, Config) ->
  setup_database(Config);
init_per_testcase(should_db_upgrade_mappings_invalid_updates, Config) ->
  setup_database(Config);

% Other cases are without db
init_per_testcase(_, Config) ->
  Config.

% End per testcase
end_per_testcase(should_db_get_banks, _Config) ->
  teardown_database();
end_per_testcase(should_db_get_budgets, _Config) ->
  teardown_database();
end_per_testcase(should_db_get_categories, _Config) ->
  teardown_database();
end_per_testcase(should_db_get_stores, _Config) ->
  teardown_database();
end_per_testcase(should_db_insert_store, _Config) ->
  teardown_database();
end_per_testcase(should_db_not_insert_store_already_existing, _Config) ->
  teardown_database();
end_per_testcase(should_db_get_clients, _Config) ->
  teardown_database();
end_per_testcase(should_db_insert_client, _Config) ->
  teardown_database();
end_per_testcase(should_db_not_insert_client_already_existing, _Config) ->
  teardown_database();
end_per_testcase(should_db_store_accounts, _Config) ->
  teardown_database();
end_per_testcase(should_db_get_accounts, _Config) ->
  teardown_database();
end_per_testcase(should_db_get_all_accounts, _Config) ->
  teardown_database();
end_per_testcase(should_db_store_transactions, _Config) ->
  teardown_database();
end_per_testcase(should_db_get_transactions, _Config) ->
  teardown_database();
end_per_testcase(should_db_get_last_transactions, _Config) ->
  teardown_database();
end_per_testcase(should_db_get_last_transactions_empty, _Config) ->
  teardown_database();
end_per_testcase(should_db_get_last_transactions_invalid_cursor, _Config) ->
  teardown_database();
end_per_testcase(should_db_get_last_transactions_id, _Config) ->
  teardown_database();
end_per_testcase(should_db_update_transaction, _Config) ->
  teardown_database();
end_per_testcase(should_db_update_transaction_with_amount, _Config) ->
  teardown_database();
end_per_testcase(should_db_update_transaction_with_amount_fails_because_not_subtransaction, _Config) ->
  teardown_database();
end_per_testcase(should_db_update_transaction_with_amount_fails_because_remaining, _Config) ->
  teardown_database();
end_per_testcase(should_db_split_transaction, _Config) ->
  teardown_database();
end_per_testcase(should_db_split_transaction_fails_because_not_found, _Config) ->
  teardown_database();
end_per_testcase(should_db_upgrade_mappings_empty, _Config) ->
  teardown_database();
end_per_testcase(should_db_upgrade_mappings_do_not_update_manual_updates, _Config) ->
  teardown_database();
end_per_testcase(should_db_upgrade_mappings_identical, _Config) ->
  teardown_database();
end_per_testcase(should_db_upgrade_mappings_updates, _Config) ->
  teardown_database();
end_per_testcase(should_db_upgrade_mappings_invalid_updates, _Config) ->
  teardown_database();

% Other cases are without db
end_per_testcase(_, _Config) ->
  ok.


%
% Dummy test for handle_cast (function not used but required by behavior gen_server)
%
should_handle_cast_do_nothing(_Config) ->
  {noreply, dummystate} = banks_fetch_storage:handle_cast(dummycall, dummystate),
  ok.

%
% Tests without real database
%
should_nodb_start_without_db_upgrade(_Config) ->
  ct:comment("Launch storage server (current db version is last version)"),
  meck:expect(pgsql_connection, open, fun(MockDatabase, MockUser, MockPassword) ->
                                          ?DB_NAME = MockDatabase,
                                          ?DB_USER = MockUser,
                                          ?DB_PASSWORD = MockPassword,
                                          fake_connection
                                      end),
  meck:expect(pgsql_connection, extended_query,
              [
               {[<<"SELECT description FROM pg_shdescription JOIN pg_database on objoid = pg_database.oid WHERE datname = $1">>, ["banks_fetch_test"], fake_connection],
                  {{select, 1}, [{<<"0.0.1">>}]}
               }
              ]),

  {ok, _PID} = banks_fetch_storage:start_link({?DB_NAME,?DB_USER,?DB_PASSWORD}),
  meck:wait(pgsql_connection, extended_query, '_', 2000),
  true = meck:validate(pgsql_connection),

  banks_fetch_storage:stop(),

  ok.

should_nodb_start_with_db_upgrade(_Config) ->
  ct:comment("Launch storage server (current db version is not the last one)"),
  meck:reset(pgsql_connection),
  meck:expect(pgsql_connection, open, fun(MockDatabase, MockUser, MockPassword) ->
                                          ?DB_NAME = MockDatabase,
                                          ?DB_USER = MockUser,
                                          ?DB_PASSWORD = MockPassword,
                                          fake_connection
                                      end),
  meck:expect(pgsql_connection, extended_query,
              [
               {[<<"SELECT description FROM pg_shdescription JOIN pg_database on objoid = pg_database.oid WHERE datname = $1">>, ["banks_fetch_test"], fake_connection],
                  {{select, 0}, []}
               },
               {[<<"BEGIN TRANSACTION">>, [], fake_connection],
                {'begin', []}
               },
               {[<<"COMMENT ON DATABASE banks_fetch_test IS '0.1.0';">>, [], fake_connection],
                {'comment', []}
               },
               {[<<"COMMENT ON DATABASE banks_fetch_test IS '0.2.0';">>, [], fake_connection],
                {'comment', []}
               },
               {[<<"COMMENT ON DATABASE banks_fetch_test IS '0.2.1';">>, [], fake_connection],
                {'comment', []}
               },
               {[<<"COMMENT ON DATABASE banks_fetch_test IS '0.2.2';">>, [], fake_connection],
                {'comment', []}
               },
               {[<<"COMMENT ON DATABASE banks_fetch_test IS '0.2.3';">>, [], fake_connection],
                {'comment', []}
               },
               {[<<"COMMENT ON DATABASE banks_fetch_test IS '0.2.4';">>, [], fake_connection],
                {'comment', []}
               },
               {[<<"COMMENT ON DATABASE banks_fetch_test IS '0.2.5';">>, [], fake_connection],
                {'comment', []}
               },
               {[<<"COMMENT ON DATABASE banks_fetch_test IS '0.2.6';">>, [], fake_connection],
                {'comment', []}
               },
               {[<<"COMMENT ON DATABASE banks_fetch_test IS '0.2.7';">>, [], fake_connection],
                {'comment', []}
               },
               {[<<"COMMENT ON DATABASE banks_fetch_test IS '0.2.8';">>, [], fake_connection],
                {'comment', []}
               },
               {[meck_matcher:new(fun(<<"COMMENT ON DATABASE banks_fetch_test IS ", _/binary>>) -> true; (_) -> false end), [], fake_connection],
                {error, unexpected_comment}},
               {[<<"COMMIT">>, [], fake_connection],
                {'commit', []}
               },
               {['_',[], fake_connection], fake_ok}
              ]),

  {ok, _PID} = banks_fetch_storage:start_link({?DB_NAME,?DB_USER,?DB_PASSWORD}),
  % One COMMIT for each upgrade
  meck:wait(10, pgsql_connection, extended_query, [<<"COMMIT">>, [], fake_connection], 3000),
  true = meck:validate(pgsql_connection),
  % 3 queries + number of queries to upgrade
  83 = meck:num_calls(pgsql_connection, extended_query, '_'),

  banks_fetch_storage:stop(),

  ok.

should_nodb_start_with_db_upgrade_error(_Config) ->
  process_flag(trap_exit, true),

  ct:comment("Launch storage server (current db version is not the last one)"),
  meck:reset(pgsql_connection),
  meck:expect(pgsql_connection, open, fun(MockDatabase, MockUser, MockPassword) ->
                                          ?DB_NAME = MockDatabase,
                                          ?DB_USER = MockUser,
                                          ?DB_PASSWORD = MockPassword,
                                          fake_connection
                                      end),
  meck:expect(pgsql_connection, extended_query,
              [
               {[<<"SELECT description FROM pg_shdescription JOIN pg_database on objoid = pg_database.oid WHERE datname = $1">>, ["banks_fetch_test"], fake_connection],
                  {{select, 0}, []}
               },
               {[<<"BEGIN TRANSACTION">>, [], fake_connection],
                {'begin', []}
               },
               {[<<"ROLLBACK">>, [], fake_connection],
                {'rollback', []}
               },
               {['_',[], fake_connection], {error, invalid_query}}
              ]),

  ct:comment("Start banks_fetch_storage (crash is expected)"),
  {ok, PID} = banks_fetch_storage:start_link({?DB_NAME,?DB_USER,?DB_PASSWORD}),

  meck:wait(pgsql_connection, extended_query, [<<"ROLLBACK">>, [], fake_connection], 3000),
  true = meck:validate(pgsql_connection),
  4 = meck:num_calls(pgsql_connection, extended_query, '_'),

  ct:comment("Verify that banks_fetch_storage exited"),
  receive {'EXIT', PID, _Reason} -> ok after 5000 -> throw({error, timeout}) end,

  ok.


should_nodb_get_clients(_Config) ->
  ct:comment("Launch storage server"),
  meck:expect(pgsql_connection, open, fun(MockDatabase, MockUser, MockPassword) ->
                                          ?DB_NAME = MockDatabase,
                                          ?DB_USER = MockUser,
                                          ?DB_PASSWORD = MockPassword,
                                          fake_connection
                                      end),
  meck:expect(pgsql_connection, extended_query,
              fun(MockQuery, MockParams, MockConnection) ->
                  <<"SELECT description FROM pg_shdescription JOIN pg_database on objoid = pg_database.oid WHERE datname = $1">> = MockQuery,
                  ["banks_fetch_test"] = MockParams,
                  fake_connection = MockConnection,
                  {{select, 1}, [{<<"0.0.1">>}]}
              end),

  {ok, _PID} = banks_fetch_storage:start_link({?DB_NAME,?DB_USER,?DB_PASSWORD}),
  meck:wait(pgsql_connection, extended_query, '_', 1000),
  true = meck:validate(pgsql_connection),

  ct:comment("Get clients"),
  DBClients = [{?BANK_ID_1, ?CLIENT_ID_1, term_to_binary(?CLIENT_CREDENTIAL_1)}, {?BANK_ID_2, ?CLIENT_ID_2, term_to_binary(?CLIENT_CREDENTIAL_2)}],
  meck:expect(pgsql_connection, simple_query, fun(MockQuery, MockConnection) ->
                                                  fake_connection = MockConnection,
                                                  <<"SELECT bank_id, client_id, client_credential FROM clients;">> = MockQuery,
                                                  {{select, length(DBClients)}, DBClients}
                                              end),
  {value, Clients} = banks_fetch_storage:get_clients(),
  ct:comment("Verify clients returned"),
  ExpectedClients = [ {{bank_id, ?BANK_ID_1}, {client_id, ?CLIENT_ID_1}, {client_credential, ?CLIENT_CREDENTIAL_1}}, {{bank_id, ?BANK_ID_2}, {client_id, ?CLIENT_ID_2}, {client_credential, ?CLIENT_CREDENTIAL_2}}],
  ExpectedClients = Clients,
  true = meck:validate(pgsql_connection),

  ct:comment("Stop storage"),
  banks_fetch_storage:stop(),

  ok.


should_nodb_insert_client(_Config) ->
  ct:comment("Launch storage server"),
  meck:expect(pgsql_connection, open, fun(MockDatabase, MockUser, MockPassword) ->
                                          ?DB_NAME = MockDatabase,
                                          ?DB_USER = MockUser,
                                          ?DB_PASSWORD = MockPassword,
                                          fake_connection
                                      end),
  meck:expect(pgsql_connection, extended_query,
              fun(MockQuery, MockParams, MockConnection) ->
                  <<"SELECT description FROM pg_shdescription JOIN pg_database on objoid = pg_database.oid WHERE datname = $1">> = MockQuery,
                  ["banks_fetch_test"] = MockParams,
                  fake_connection = MockConnection,
                  {{select, 1}, [{<<"0.0.1">>}]}
              end),

  {ok, _PID} = banks_fetch_storage:start_link({?DB_NAME,?DB_USER,?DB_PASSWORD}),

  ct:comment("Wait initialisation"),
  meck:wait(pgsql_connection, extended_query, '_', 1000),
  true = meck:validate(pgsql_connection),

  ct:comment("Insert client"),
  meck:expect(pgsql_connection, extended_query, fun(MockQuery, MockParameters, MockConnection) ->
                                                  fake_connection = MockConnection,
                                                  <<"INSERT INTO clients(bank_id, client_id, client_credential) VALUES($1,$2,$3);">> = MockQuery,
                                                  ExpectedParameters = [?BANK_ID_1, ?CLIENT_ID_1, term_to_binary(?CLIENT_CREDENTIAL_1)],
                                                  ExpectedParameters = MockParameters,
                                                  {{insert, 0, 1}, []}
                                              end),
  ok = banks_fetch_storage:insert_client({bank_id, ?BANK_ID_1}, {client_id, ?CLIENT_ID_1}, {client_credential, ?CLIENT_CREDENTIAL_1}),

  ct:comment("Verify pgsql_connection calls"),
  true = meck:validate(pgsql_connection),

  ct:comment("Stop storage"),
  banks_fetch_storage:stop(),

  ok.


should_nodb_store_accounts(_Config) ->
  ct:comment("Launch storage server"),
  meck:reset(pgsql_connection),
  meck:expect(pgsql_connection, open, fun(MockDatabase, MockUser, MockPassword) ->
                                          ?DB_NAME = MockDatabase,
                                          ?DB_USER = MockUser,
                                          ?DB_PASSWORD = MockPassword,
                                          fake_connection
                                      end),

  meck:expect(pgsql_connection, extended_query,
              fun(MockQuery, MockParams, MockConnection) ->
                  <<"SELECT description FROM pg_shdescription JOIN pg_database on objoid = pg_database.oid WHERE datname = $1">> = MockQuery,
                  ["banks_fetch_test"] = MockParams,
                  fake_connection = MockConnection,
                  {{select, 1}, [{<<"0.0.1">>}]}
              end),

  {ok, _PID} = banks_fetch_storage:start_link({?DB_NAME,?DB_USER,?DB_PASSWORD}),
  meck:wait(pgsql_connection, extended_query, '_', 1000),
  true = meck:validate(pgsql_connection),

  ct:comment("Store accounts"),
  ExpectedQueriesForAccounts = [
                                {[<<"INSERT INTO accounts(bank_id, client_id, fetching_at, account_id, balance, number, owner, ownership, type, name) VALUES($1,$2,$3,$4,$5,$6,$7,$8,$9,$10);">>,
                                  [?BANK_ID_1, ?CLIENT_ID_1, ?FETCHING_AT_1, AccountId, Balance, Number, Owner, atom_to_binary(Ownership,'utf8'), atom_to_binary(Type,'utf8'), Name], fake_connection],
                                 {{insert, 0, 1}, []}
                                }
                                || #{ id := AccountId, balance := Balance, number := Number, owner := Owner, ownership := Ownership, type := Type, name := Name } <- ?ACCOUNTS_1
                               ],
  meck:reset(pgsql_connection),
  meck:expect(pgsql_connection, simple_query, [
                                               {[<<"BEGIN TRANSACTION">>, fake_connection],
                                                {'begin', []}
                                               },
                                               {[<<"COMMIT">>, fake_connection],
                                                {'commit', []}
                                               }
                                              ]),
  meck:expect(pgsql_connection, extended_query, ExpectedQueriesForAccounts),
  ok = banks_fetch_storage:store_accounts({bank_id, ?BANK_ID_1}, {client_id, ?CLIENT_ID_1}, ?FETCHING_AT_1, ?ACCOUNTS_1),

  ct:comment("Verify pgsql_connection calls"),
  true = meck:validate(pgsql_connection),
  NbrAccounts = length(?ACCOUNTS_1),
  NbrAccounts = meck:num_calls(pgsql_connection, extended_query, '_'),
  2 = meck:num_calls(pgsql_connection, simple_query, '_'),

  ct:comment("Stop storage"),
  banks_fetch_storage:stop(),

  ok.


%
% Tests with real database
%

should_db_get_banks(_Config) ->
  ct:comment("Get banks"),
  Banks = banks_fetch_storage:get_banks(),

  ct:comment("Verify returned banks"),
  {value, [#{ id := <<"ing">>, name := <<"ING">> }]} = Banks,
  ok.

should_db_get_budgets(_Config) ->
  ct:comment("Get budgets"),
  Budgets = banks_fetch_storage:get_budgets(),

  ct:comment("Verify returned budgets"),
  {value, [#{ id := 1, name := <<"Aucun">> }, #{ id := 2, name := <<"Courant">> }, #{ id := 3, name := <<"Plaisir">> }, #{ id := 4, name := <<"Exceptionnel">> }, #{ id := 5, name := <<"Épargne"/utf8>> }]} = Budgets,
  ok.

should_db_get_categories(_Config) ->
  ct:comment("Get categories"),
  Categories = banks_fetch_storage:get_categories(),

  ct:comment("Verify returned categories"),
  {value, [#{ id := 1, name := <<"Alimentation">>, up_category_id := none }, #{ id := 2, name := <<"Supermarché"/utf8>>, up_category_id := 1} ]} = Categories,
  ok.

should_db_get_stores(_Config) ->
  ct:comment("Get stores"),
  Stores = banks_fetch_storage:get_stores(),

  ct:comment("Verify returned stores"),
  {value, [#{ id := 1, name := <<"Carrefour">> }, #{ id := 2, name := <<"LIDL">> }, #{ id := 3, name := <<"Monoprix">> }, #{ id := 4, name := <<"Casino">> }]} = Stores,
  ok.

should_db_insert_store(_Config) ->
  ct:comment("Get stores"),
  Stores = banks_fetch_storage:get_stores(),

  ct:comment("Verify returned stores"),
  {value, [#{ id := 1, name := <<"Carrefour">> }, #{ id := 2, name := <<"LIDL">> }, #{ id := 3, name := <<"Monoprix">> }, #{ id := 4, name := <<"Casino">> }]} = Stores,

  ct:comment("Insert store by name"),
  {ok, NewStore} = banks_fetch_storage:insert_store(<<"MyNewStore">>),
  #{ id := 1000000, name := <<"MyNewStore">> } = NewStore,

  ct:comment("Verify returned stores contain new inserted store"),
  Stores2 = banks_fetch_storage:get_stores(),
  {value, [#{ id := 1, name := <<"Carrefour">> }, #{ id := 2, name := <<"LIDL">> }, #{ id := 3, name := <<"Monoprix">> }, #{ id := 4, name := <<"Casino">> }, NewStore]} = Stores2,

  ok.

should_db_not_insert_store_already_existing(_Config) ->
  ct:comment("Get stores"),
  Stores = banks_fetch_storage:get_stores(),

  ct:comment("Verify returned stores"),
  {value, [#{ id := 1, name := <<"Carrefour">> }, #{ id := 2, name := <<"LIDL">> }, #{ id := 3, name := <<"Monoprix">> }, #{ id := 4, name := <<"Casino">> }]} = Stores,

  ct:comment("Insert store by name"),
  {error, already_inserted} = banks_fetch_storage:insert_store(<<"Carrefour">>),

  ct:comment("Verify stores have not been modified"),
  Stores = banks_fetch_storage:get_stores(),

  ok.


should_db_get_clients(_Config) ->
  ct:comment("Get clients"),
  Clients = banks_fetch_storage:get_clients(),

  ct:comment("Verify returned clients"),
  {value, [{{bank_id, ?BANK_ID_1},{client_id, ?CLIENT_ID_1},{client_credential, ?CLIENT_CREDENTIAL_1}}]} = Clients,
  ok.


should_db_insert_client(Config) ->
  ct:comment("Insert client"),
  ok = banks_fetch_storage:insert_client({bank_id, ?BANK_ID_1}, {client_id, ?CLIENT_ID_1}, {client_credential, ?CLIENT_CREDENTIAL_1}),
  ok = banks_fetch_storage:insert_client({bank_id, ?BANK_ID_2}, {client_id, ?CLIENT_ID_2}, {client_credential, ?CLIENT_CREDENTIAL_2}),

  ct:comment("Load clients from database"),
  {db_connection, Connection} = lists:keyfind(db_connection, 1, Config),
  {{select, Nbr}, ClientsList} = pgsql_connection:simple_query(<<"SELECT bank_id, client_id, client_credential FROM clients">>, Connection),

  ct:comment("Verify number of clients"),
  2 = Nbr,

  ct:comment("Verify client data"),
  ExpectedClientsList = [ {?BANK_ID_1, ?CLIENT_ID_1, term_to_binary(?CLIENT_CREDENTIAL_1)}, {?BANK_ID_2, ?CLIENT_ID_2, term_to_binary(?CLIENT_CREDENTIAL_2)} ],
  ExpectedClientsList = ClientsList,

  ok.

should_db_not_insert_client_already_existing(Config) ->
  ct:comment("Load clients from database"),
  {db_connection, Connection} = lists:keyfind(db_connection, 1, Config),
  {{select, Nbr}, ClientsList} = pgsql_connection:simple_query(<<"SELECT bank_id, client_id, client_credential FROM clients">>, Connection),

  ct:comment("Insert new client"),
  {error, already_inserted} = banks_fetch_storage:insert_client({bank_id, ?BANK_ID_1}, {client_id, ?CLIENT_ID_1}, {client_credential, ?CLIENT_CREDENTIAL_1}),

  ct:comment("Verify that clients have not been modified in database"),
  {db_connection, Connection} = lists:keyfind(db_connection, 1, Config),
  {{select, Nbr}, ClientsList} = pgsql_connection:simple_query(<<"SELECT bank_id, client_id, client_credential FROM clients">>, Connection),

  ok.


should_db_store_accounts(Config) ->
  ct:comment("Store accounts"),
  ok = banks_fetch_storage:store_accounts({bank_id, ?BANK_ID_1}, {client_id, ?CLIENT_ID_1}, ?FETCHING_AT_1, ?ACCOUNTS_1),

  ct:comment("Load accounts from database"),
  {db_connection, Connection} = lists:keyfind(db_connection, 1, Config),
  {{select, Nbr}, AccountsList} = pgsql_connection:simple_query(<<"SELECT bank_id, client_id, fetching_at, account_id, balance, number, owner, ownership, type, name FROM accounts">>, Connection),

  ct:comment("Verify number of accounts"),
  NbrExpectedAccounts = length(?ACCOUNTS_1),
  NbrExpectedAccounts = Nbr,

  ct:comment("Verify accounts data"),
  ExpectedDataList = [ {?BANK_ID_1, ?CLIENT_ID_1, ?FETCHING_AT_1, ExpectedAccountId, ExpectedBalance, ExpectedNumber, ExpectedOwner, {e_account_ownership, atom_to_binary(ExpectedOwnerShip,'utf8')}, {e_account_type, atom_to_binary(ExpectedType,'utf8')}, ExpectedName} || #{ id := ExpectedAccountId, balance := ExpectedBalance, number := ExpectedNumber, owner := ExpectedOwner, ownership := ExpectedOwnerShip, type := ExpectedType, name := ExpectedName } <- ?ACCOUNTS_1 ],

  lists:foreach(fun({Expected, Account}) -> Expected = Account end, lists:zip(ExpectedDataList, AccountsList)),

  ok.

should_db_get_accounts(_Config) ->
  ct:comment("Get accounts"),
  ExpectedAccounts = ?ACCOUNTS_1,
  NbrExpectedAccounts = length(ExpectedAccounts),
  {value, Accounts} = banks_fetch_storage:get_accounts(?BANK_ID_1, ?CLIENT_ID_1),

  ct:comment("Verify returned accounts"),
  NbrExpectedAccounts = length(Accounts),
  ExpectedAccounts = Accounts,

  ok.


should_db_get_all_accounts(_Config) ->
  ct:comment("Get all accounts"),
  ExpectedAccounts = ?ACCOUNTS_1 ++ ?ACCOUNTS_2,
  NbrExpectedAccounts = length(ExpectedAccounts),
  {value, Accounts} = banks_fetch_storage:get_all_accounts(),

  ct:comment("Verify returned accounts"),
  NbrExpectedAccounts = length(Accounts),
  ExpectedAccounts = Accounts,

  ok.


should_db_store_transactions(Config) ->
  ct:comment("Store transactions"),
  ok = banks_fetch_storage:store_transactions({bank_id, ?BANK_ID_1}, {client_id, ?CLIENT_ID_1}, {account_id, ?ACCOUNT_ID_1}, ?FETCHING_AT_1, ?TRANSACTIONS_1),

  ct:comment("Load transactions from database"),
  {db_connection, Connection} = lists:keyfind(db_connection, 1, Config),
  {{select, Nbr}, TransactionsList} = pgsql_connection:simple_query(<<"SELECT bank_id, client_id, account_id, fetching_at, transaction_id, accounting_date, effective_date, amount, description, type, ext_date, ext_mapping_id, ext_period, ext_budget_id, ext_categories_id, ext_store_id FROM transactions ORDER BY transaction_id desc;">>, Connection),

  ct:comment("Verify number of transactions"),
  NbrExpectedTransactions = length(?TRANSACTIONS_1),
  NbrExpectedTransactions = Nbr,

  ct:comment("Verify transactions data"),
  ExpectedDataList = ?TRANSACTION_1_STORED,

  lists:foreach(fun({Expected, Transaction}) ->
                    error_logger:info_msg("EX=~1000p", [Expected]),
                    error_logger:info_msg("TR=~1000p", [Transaction]),

                    Expected = Transaction end, lists:zip(ExpectedDataList, TransactionsList)),

  ok.

should_db_get_transactions(_Config) ->
  ct:comment("Get transactions"),
  {value, Transactions} = banks_fetch_storage:get_transactions({bank_id, ?BANK_ID_1}, {client_id, ?CLIENT_ID_1}, {account_id, ?ACCOUNT_ID_1}),

  ct:comment("Verify transactions"),
  2 = length(Transactions),
  [#{ id := <<"transaction2">> }, #{id := <<"transaction1">> }] = Transactions,

  ok.

should_db_get_last_transactions(Config) ->
  ct:comment("Get last 1 transactions"),
  {value, {Cursor1, Total1, Transactions1}} = banks_fetch_storage:get_last_transactions(none, 1),
  <<"NToxOjU=">> = Cursor1,
  5 = Total1,
  1 = length(Transactions1),
  [#{ id := <<"transaction1">>, ext_period := 'bimester', ext_date := {2020,7,1}, ext_store_id := 8, ext_budget_id := 1, ext_categories_id := [4,5] }] = Transactions1,

  ct:comment("Get last 5 transactions"),
  {value, {Cursor2, Total2, Transactions2}} = banks_fetch_storage:get_last_transactions(none, 5),
  none = Cursor2,
  Total1 = Total2,
  5 = length(Transactions2),
  [#{ id := <<"transaction1">> }, #{ id := <<"transaction3">> }, #{ id := <<"transaction4">> }, #{ id := <<"transaction5">> }, #{ id := <<"transaction2">> }] = Transactions2,

  ct:comment("Get last 2 transactions after first one"),
  {value, {Cursor3, Total3, Transactions3}} = banks_fetch_storage:get_last_transactions(Cursor1, 2),
  <<"NTozOjU=">> = Cursor3,
  Total1 = Total3,
  2 = length(Transactions3),
  [#{ id := <<"transaction3">> }, #{ id := <<"transaction4">> }] = Transactions3,

  ct:comment("Insert a new transaction"),
  {db_connection, Connection} = lists:keyfind(db_connection, 1, Config),
  Query = <<"INSERT INTO transactions(bank_id, client_id, account_id, fetching_at, fetching_position, transaction_id, accounting_date, effective_date, amount, description, type) VALUES ('ing', 'client2', 'account4', '2020-07-05T00:00:00Z', 0, 'transaction6', '2020-07-05', '2020-07-05', -13.00, 'VIREMENT SEPA', 'sepa_debit');">>,
  {{insert, 0, 1}, []} = pgsql_connection:simple_query(Query, Connection),

  ct:comment("Verify that last get_last_transactions returns same result even if we add a new transaction"),
  {value, {Cursor3, Total3, Transactions3}} = banks_fetch_storage:get_last_transactions(Cursor1, 2),

  ok.

should_db_get_last_transactions_empty(_Config) ->
  ct:comment("Get last 1 transactions"),
  {value, {Cursor1, Total1, Transactions1}} = banks_fetch_storage:get_last_transactions(none, 1),
  none = Cursor1,
  0 = Total1,
  [] = Transactions1,

  ok.

should_db_get_last_transactions_invalid_cursor(_Config) ->
  ct:comment("Get last 1 transactions with an invalid cursor"),
  {error, invalid_cursor} = banks_fetch_storage:get_last_transactions(<<"invalidcursor">>, 1),
  ok.


should_db_get_last_transactions_id(_Config) ->
  ct:comment("Get last transactions id"),
  ExpectedData = [{{account_id, <<"account1">>}, {transaction_id,<<"transaction1">>}}, {{account_id, <<"account2">>}, {transaction_id,<<"transaction3">>}}],
  NbrExpectedData = length(ExpectedData),
  {value, LastTransactionsIdList} = banks_fetch_storage:get_last_transactions_id({bank_id, <<"ing">>}, {client_id, <<"client1">>}),

  ct:comment("Verify returned transactions id"),
  NbrExpectedData = length(LastTransactionsIdList),
  ExpectedData = LastTransactionsIdList,

  ok.

should_db_update_transaction(_Config) ->
  ct:comment("Update transaction"),
  {ok, Transaction} = banks_fetch_storage:update_transaction({bank_id, <<"ing">>}, {client_id, <<"client2">>}, {account_id, <<"account3">>}, {transaction_id, <<"transaction5">>},
                                                             {2020,11,16}, 'bimester', undefined, 1, [3,4], undefined),
  ExpectedTransaction = #{id => <<"transaction5">>, bank_id => {bank_id,<<"ing">>}, client_id => {client_id,<<"client2">>}, account_id => {account_id,<<"account3">>},
             accounting_date => {2020,7,7}, amount => -55.55, description => <<"VIREMENT SEPA">>, effective_date => {2020,7,7}, type => sepa_debit, ext_mapping_id => -1,
             ext_categories_id => [3,4], ext_date => {2020,11,16}, ext_budget_id => 1, ext_period => bimester, ext_store_id => undefined, ext_split_of_id => none, ext_splitted => false},
  ExpectedTransaction = Transaction,

  ct:comment("Update again transaction"),
  {ok, Transaction2} = banks_fetch_storage:update_transaction({bank_id, <<"ing">>}, {client_id, <<"client2">>}, {account_id, <<"account3">>}, {transaction_id, <<"transaction5">>},
                                                             undefined, undefined, 3, undefined, undefined, undefined),
  ExpectedTransaction2 = #{id => <<"transaction5">>, bank_id => {bank_id,<<"ing">>}, client_id => {client_id,<<"client2">>}, account_id => {account_id,<<"account3">>},
             accounting_date => {2020,7,7}, amount => -55.55, description => <<"VIREMENT SEPA">>, effective_date => {2020,7,7}, type => sepa_debit, ext_mapping_id => -1,
             ext_categories_id => undefined, ext_date => undefined, ext_budget_id => undefined, ext_period => undefined, ext_store_id => 3, ext_split_of_id => none, ext_splitted => false},
  ExpectedTransaction2 = Transaction2,

  ok.

should_db_update_transaction_with_amount(_Config) ->
  {value, {_, NbrTransactions0, Transactions0}} = banks_fetch_storage:get_last_transactions(none, 10),
  8 = NbrTransactions0,
  [#{ id := <<"transaction1">>, amount := -44.44 } = Transaction1,
   #{ id := <<"transaction1-001">>, amount := -30.00 } = Transaction11,
   #{ id := <<"transaction1-002">>, amount := -5.00 } = Transaction12,
   #{ id := <<"transaction1-REM">>, amount := -9.44 } = Transaction1Rem,
   #{ id := <<"transaction3">>, amount := -77.77 } = Transaction3,
   #{ id := <<"transaction4">>, amount := -66.66 } = Transaction4,
   #{ id := <<"transaction5">>, amount := -55.55 } = Transaction5,
   #{ id := <<"transaction2">>, amount := -88.88 } = Transaction2 ] = Transactions0,

  ct:comment("Update transaction"),
  {ok, UpdatedTransaction} = banks_fetch_storage:update_transaction({bank_id, <<"ing">>}, {client_id, <<"client1">>}, {account_id, <<"account1">>}, {transaction_id, <<"transaction1-001">>},
                                                             {2020,11,16}, 'bimester', undefined, 1, [3,4], -25.0),
  ExpectedTransaction = maps:merge(Transaction11, #{ amount => -25.0, ext_mapping_id => -1, ext_date => {2020,11,16}, ext_period => 'bimester', ext_budget_id => 1, ext_store_id => undefined, ext_categories_id => [3,4] }),
  ExpectedTransaction = UpdatedTransaction,

  {value, {_, NbrTransactions1, Transactions1}} = banks_fetch_storage:get_last_transactions(none, 10),
  NbrTransactions0 = NbrTransactions1,
  Transaction1RemExpected = maps:put(amount, -14.44, Transaction1Rem),
  [Transaction1, ExpectedTransaction, Transaction12, Transaction1RemExpected, Transaction3, Transaction4, Transaction5, Transaction2] = Transactions1,

  ok.


should_db_update_transaction_with_amount_fails_because_not_subtransaction(_Config) ->
  {value, {_, NbrTransactions0, Transactions0}} = banks_fetch_storage:get_last_transactions(none, 10),
  8 = NbrTransactions0,
  [#{ id := <<"transaction1">>, amount := -44.44 },
   #{ id := <<"transaction1-001">>, amount := -30.00 },
   #{ id := <<"transaction1-002">>, amount := -5.00 },
   #{ id := <<"transaction1-REM">>, amount := -9.44 },
   #{ id := <<"transaction3">>, amount := -77.77 },
   #{ id := <<"transaction4">>, amount := -66.66 },
   #{ id := <<"transaction5">>, amount := -55.55 },
   #{ id := <<"transaction2">>, amount := -88.88 } ] = Transactions0,

  ct:comment("Update transaction failed"),
  {error, not_subtransaction} = banks_fetch_storage:update_transaction({bank_id, <<"ing">>}, {client_id, <<"client1">>}, {account_id, <<"account1">>}, {transaction_id, <<"transaction1">>},
                                                                       {2020,11,16}, 'bimester', undefined, 1, [3,4], -25.0),

  ct:comment("Update transaction faileed"),
  {error, not_subtransaction} = banks_fetch_storage:update_transaction({bank_id, <<"ing">>}, {client_id, <<"client1">>}, {account_id, <<"account2">>}, {transaction_id, <<"transaction3">>},
                                                                       {2020,11,16}, 'bimester', undefined, 1, [3,4], -25.0),

  {value, {_, NbrTransactions1, Transactions1}} = banks_fetch_storage:get_last_transactions(none, 10),
  NbrTransactions0 = NbrTransactions1,
  Transactions0 = Transactions1,

  ok.


should_db_update_transaction_with_amount_fails_because_remaining(_Config) ->
  {value, {_, NbrTransactions0, Transactions0}} = banks_fetch_storage:get_last_transactions(none, 10),
  8 = NbrTransactions0,
  [#{ id := <<"transaction1">>, amount := -44.44 },
   #{ id := <<"transaction1-001">>, amount := -30.00 },
   #{ id := <<"transaction1-002">>, amount := -5.00 },
   #{ id := <<"transaction1-REM">>, amount := -9.44 },
   #{ id := <<"transaction3">>, amount := -77.77 },
   #{ id := <<"transaction4">>, amount := -66.66 },
   #{ id := <<"transaction5">>, amount := -55.55 },
   #{ id := <<"transaction2">>, amount := -88.88 } ] = Transactions0,

  ct:comment("Update transaction failed"),
  {error, remaining_subtransaction} = banks_fetch_storage:update_transaction({bank_id, <<"ing">>}, {client_id, <<"client1">>}, {account_id, <<"account1">>}, {transaction_id, <<"transaction1-REM">>},
                                                                       {2020,11,16}, 'bimester', undefined, 1, [3,4], -25.0),

  {value, {_, NbrTransactions1, Transactions1}} = banks_fetch_storage:get_last_transactions(none, 10),
  NbrTransactions0 = NbrTransactions1,
  Transactions0 = Transactions1,

  ok.



should_db_split_transaction(_Config) ->
  ct:comment("Get last 5 transactions"),
  {value, {Cursor1, Total1, Transactions1}} = banks_fetch_storage:get_last_transactions(none, 5),
  none = Cursor1,
  5 = Total1,
  5 = length(Transactions1),
  [#{ id := <<"transaction1">> }, #{ id := <<"transaction3">> }, #{ id := <<"transaction4">> }, #{ id := <<"transaction5">> }, #{ id := <<"transaction2">> }] = Transactions1,

  ct:comment("Split transaction1"),
  {ok, [SubTransaction1, SubTransaction2]} = banks_fetch_storage:split_transaction({bank_id, <<"ing">>}, {client_id, <<"client1">>}, {account_id, <<"account1">>}, {transaction_id, <<"transaction1">>}),
  ExpectedSubTransaction1 = #{id => <<"transaction1-001">>,
                               accounting_date => {2020,7,8}, amount => 0.0, description => <<"no description">>, effective_date => {2020,7,8}, type => card_debit, ext_mapping_id => undefined,
                               ext_categories_id => [4,5], ext_date => {2020,7,1}, ext_budget_id => 1, ext_period => bimester, ext_store_id => 8, ext_split_of_id => {transaction_id, <<"transaction1">>}, ext_splitted => false},
  ExpectedSubTransaction2 = #{id => <<"transaction1-REM">>, % bank_id => {bank_id,<<"ing">>}, client_id => {client_id,<<"client1">>}, account_id => {account_id,<<"account1">>},
                               accounting_date => {2020,7,8}, amount => -44.44, description => <<"no description">>, effective_date => {2020,7,8}, type => card_debit, ext_mapping_id => undefined,
                               ext_categories_id => [4,5], ext_date => {2020,7,1}, ext_budget_id => 1, ext_period => bimester, ext_store_id => 8, ext_split_of_id => {transaction_id, <<"transaction1">>}, ext_splitted => false},
  ExpectedSubTransaction1 = SubTransaction1,
  ExpectedSubTransaction2 = SubTransaction2,

  ct:comment("Get last 5 transactions"),
  {value, {Cursor2, Total2, Transactions2}} = banks_fetch_storage:get_last_transactions(none, 5),
  <<"Nzo1Ojc=">> = Cursor2,
  ct:comment("Verify we have 2 more transactions"),
  7 = Total2,
  5 = length(Transactions2),
  ct:comment("Verify we have 2 sub-transactions after splitted one"),
  [#{ id := <<"transaction1">>, ext_splitted := true, ext_split_of_id := none },
   #{ id := <<"transaction1-001">>, ext_splitted := false, ext_split_of_id := {transaction_id, <<"transaction1">>} },
   #{ id := <<"transaction1-REM">>, ext_splitted := false, ext_split_of_id := {transaction_id, <<"transaction1">>}  },
   #{ id := <<"transaction3">> },
   #{ id := <<"transaction4">> }] = Transactions2,

  ct:comment("Split again transaction1"),
  {ok, [SubTransaction2_2]} = banks_fetch_storage:split_transaction({bank_id, <<"ing">>}, {client_id, <<"client1">>}, {account_id, <<"account1">>}, {transaction_id, <<"transaction1">>}),
  ExpectedSubTransaction2_2 = #{id => <<"transaction1-002">>, % bank_id => {bank_id,<<"ing">>}, client_id => {client_id,<<"client1">>}, account_id => {account_id,<<"account1">>},
                               accounting_date => {2020,7,8}, amount => 0.0, description => <<"no description">>, effective_date => {2020,7,8}, type => card_debit, ext_mapping_id => undefined,
                               ext_categories_id => [4,5], ext_date => {2020,7,1}, ext_budget_id => 1, ext_period => bimester, ext_store_id => 8, ext_split_of_id => {transaction_id, <<"transaction1">>}, ext_splitted => false},
  ExpectedSubTransaction2_2 = SubTransaction2_2,

  ct:comment("Get last 5 transactions"),
  {value, {Cursor3, Total3, Transactions3}} = banks_fetch_storage:get_last_transactions(none, 5),
  <<"ODo1Ojg=">> = Cursor3,
  ct:comment("Verify we have 1 more transactions"),
  8 = Total3,
  5 = length(Transactions3),
  ct:comment("Verify we have 3 sub-transactions after splitted one"),
  [#{ id := <<"transaction1">>, ext_splitted := true, ext_split_of_id := none },
   #{ id := <<"transaction1-001">>, ext_splitted := false, ext_split_of_id := {transaction_id, <<"transaction1">>} },
   #{ id := <<"transaction1-002">>, ext_splitted := false, ext_split_of_id := {transaction_id, <<"transaction1">>} },
   #{ id := <<"transaction1-REM">>, ext_splitted := false, ext_split_of_id := {transaction_id, <<"transaction1">>} },
   #{ id := <<"transaction3">> }] = Transactions3,

  ok.

should_db_split_transaction_fails_because_not_found(_Config) ->
  ct:comment("Get last 5 transactions"),
  {value, {Cursor1, Total1, Transactions1}} = banks_fetch_storage:get_last_transactions(none, 5),
  none = Cursor1,
  5 = Total1,
  5 = length(Transactions1),
  [#{ id := <<"transaction1">> }, #{ id := <<"transaction3">> }, #{ id := <<"transaction4">> }, #{ id := <<"transaction5">> }, #{ id := <<"transaction2">> }] = Transactions1,

  ct:comment("Split transaction1"),
  {error, not_found} = banks_fetch_storage:split_transaction({bank_id, <<"ing">>}, {client_id, <<"client1">>}, {account_id, <<"account1">>}, {transaction_id, <<"transactionUNK">>}),

  ct:comment("Verify that transactions have not changed"),
  {value, {Cursor2, Total2, Transactions2}} = banks_fetch_storage:get_last_transactions(none, 5),
  Cursor1 = Cursor2,
  Total1 = Total2,
  Transactions1 = Transactions2,

  ok.



%% Functions related to mappings

should_db_upgrade_mappings_empty(_Config) ->
  ct:comment("Verify current mappings is empty"),
  {value, []} = banks_fetch_storage:get_budgets(),
  {value, []} = banks_fetch_storage:get_categories(),
  {value, []} = banks_fetch_storage:get_stores(),
  {value, []} = banks_fetch_storage:get_mappings(),

  ct:comment("Get transactions"),
  {value, {none, 5, Transactions1}} = banks_fetch_storage:get_last_transactions(none, 10),
  5 = length(Transactions1),
  [
   #{ id := <<"transaction1">>, ext_mapping_id := undefined, ext_date := {2020,10,10}, ext_store_id := undefined, ext_budget_id := undefined, ext_categories_id := undefined, ext_period := undefined },
   #{ id := <<"transaction3">>, ext_mapping_id := undefined, ext_date := {2020,7,8}, ext_store_id := undefined, ext_budget_id := undefined, ext_categories_id := undefined, ext_period := undefined },
   #{ id := <<"transaction4">>, ext_mapping_id := undefined, ext_date := {2020,7,8}, ext_store_id := undefined, ext_budget_id := undefined, ext_categories_id := undefined, ext_period := undefined },
   #{ id := <<"transaction5">>, ext_mapping_id := undefined, ext_date := {2020,7,7}, ext_store_id := undefined, ext_budget_id := undefined, ext_categories_id := undefined, ext_period := undefined },
   #{ id := <<"transaction2">>, ext_mapping_id := undefined, ext_date := undefined, ext_store_id := undefined, ext_budget_id := undefined, ext_categories_id := undefined, ext_period := undefined }
  ] = Transactions1,

  ct:comment("Upgrade mappings"),
  Budgets = [#{ id => 0, name => <<"Aucun">> }, #{ id => 1, name => <<"Courant">>}, #{ id => 2, name => <<"Extra">> }],
  Categories = [ #{ id => 1, name => <<"Alimentation">>, up_category_id => none }, #{ id => 2, name => <<"Supermarché"/utf8>>, up_category_id => 1 }, #{ id => 3, name => <<"Logement">>, up_category_id => none}],
  Stores = [#{ id => 1, name => <<"Auchan">> }, #{ id => 2, name => <<"Carrefour">> }],
  Mappings = [#{ id => 1, pattern => <<"AUCHAN">>, fix_date => none, period => month, budget_id => 1, categories_id => [1, 2], store_id => 1 },
              #{ id => 2, pattern => <<"URSSAF">>, fix_date => previous2, period => month, budget_id => 1, categories_id => none, store_id => none },
              #{ id => 3, pattern => <<"CHARGES">>, fix_date => none, period => quarter, budget_id => 1, categories_id => none, store_id => none } ],
  ok = banks_fetch_storage:upgrade_mappings(Budgets, Categories, Stores, Mappings),

  ct:comment("Verify mappings in database"),
  verify_mappings(Budgets, Categories, Stores, Mappings),

  ct:comment("Get transactions"),
  {value, {none, 5, Transactions2}} = banks_fetch_storage:get_last_transactions(none, 10),
  5 = length(Transactions2),
  [
   #{ id := <<"transaction1">>, ext_mapping_id := 1, ext_date := {2020,10,10}, ext_store_id := 1, ext_budget_id := 1, ext_categories_id := [1,2], ext_period := 'month' },
   #{ id := <<"transaction3">>, ext_mapping_id := 2, ext_date := {2020,5,31}, ext_store_id := undefined, ext_budget_id := 1, ext_categories_id := undefined, ext_period := 'month' },
   #{ id := <<"transaction4">>, ext_mapping_id := 3, ext_date := {2020,7,8}, ext_store_id := undefined, ext_budget_id := 1, ext_categories_id := undefined, ext_period := 'quarter' },
   #{ id := <<"transaction5">>, ext_mapping_id := 3, ext_date := {2020,7,7}, ext_store_id := undefined, ext_budget_id := 1, ext_categories_id := undefined, ext_period := 'quarter' },
   #{ id := <<"transaction2">>, ext_mapping_id := undefined, ext_store_id := undefined, ext_budget_id := undefined, ext_categories_id := undefined, ext_period := undefined }
  ] = Transactions2,

  ok.

% Same test as above but one transaction has been updated manually and should not be updated automatically by mappings
should_db_upgrade_mappings_do_not_update_manual_updates(_Config) ->
  ct:comment("Add a store"),
  {ok, MyStore} = banks_fetch_storage:insert_store(<<"MyStore">>),

  ct:comment("Verify current mappings is empty except MyStore"),
  {value, []} = banks_fetch_storage:get_budgets(),
  {value, []} = banks_fetch_storage:get_categories(),
  {value, [MyStore]} = banks_fetch_storage:get_stores(),
  {value, []} = banks_fetch_storage:get_mappings(),

  ct:comment("Get transactions"),
  {value, {none, 5, Transactions1}} = banks_fetch_storage:get_last_transactions(none, 10),
  5 = length(Transactions1),
  [
   #{ id := <<"transaction1">>, ext_mapping_id := undefined, ext_date := {2020,10,10}, ext_store_id := undefined, ext_budget_id := undefined, ext_categories_id := undefined, ext_period := undefined },
   #{ id := <<"transaction3">>, ext_mapping_id := undefined, ext_date := {2020,7,8}, ext_store_id := undefined, ext_budget_id := undefined, ext_categories_id := undefined, ext_period := undefined },
   #{ id := <<"transaction4">>, ext_mapping_id := undefined, ext_date := {2020,7,8}, ext_store_id := undefined, ext_budget_id := undefined, ext_categories_id := undefined, ext_period := undefined },
   #{ id := <<"transaction5">>, ext_mapping_id := undefined, ext_date := {2020,7,7}, ext_store_id := undefined, ext_budget_id := undefined, ext_categories_id := undefined, ext_period := undefined },
   #{ id := <<"transaction2">>, ext_mapping_id := undefined, ext_date := undefined, ext_store_id := undefined, ext_budget_id := undefined, ext_categories_id := undefined, ext_period := undefined }
  ] = Transactions1,

  ct:comment("Update transaction5 manually"),
  {ok, UpdatedTransaction} = banks_fetch_storage:update_transaction({bank_id, <<"ing">>}, {client_id, <<"client2">>}, {account_id, <<"account3">>}, {transaction_id, <<"transaction5">>}, {2020,12,6}, 'semester', 4, undefined, undefined, undefined),
  #{ ext_mapping_id := -1 } = UpdatedTransaction,

  ct:comment("Upgrade mappings"),
  Budgets = [#{ id => 0, name => <<"Aucun">> }, #{ id => 1, name => <<"Courant">>}, #{ id => 2, name => <<"Extra">> }],
  Categories = [ #{ id => 1, name => <<"Alimentation">>, up_category_id => none }, #{ id => 2, name => <<"Supermarché"/utf8>>, up_category_id => 1 }, #{ id => 3, name => <<"Logement">>, up_category_id => none}],
  Stores = [#{ id => 1, name => <<"Auchan">> }, #{ id => 2, name => <<"Carrefour">> }],
  Mappings = [#{ id => 1, pattern => <<"AUCHAN">>, fix_date => none, period => month, budget_id => 1, categories_id => [1, 2], store_id => 1 },
              #{ id => 2, pattern => <<"URSSAF">>, fix_date => previous2, period => month, budget_id => 1, categories_id => none, store_id => none },
              #{ id => 3, pattern => <<"CHARGES">>, fix_date => none, period => quarter, budget_id => 1, categories_id => none, store_id => none } ],
  ok = banks_fetch_storage:upgrade_mappings(Budgets, Categories, Stores, Mappings),

  ct:comment("Verify mappings in database"),
  verify_mappings(Budgets, Categories, Stores++[MyStore], Mappings),

  ct:comment("Get transactions"),
  {value, {none, 5, Transactions2}} = banks_fetch_storage:get_last_transactions(none, 10),
  5 = length(Transactions2),
  [
   #{ id := <<"transaction1">>, ext_mapping_id := 1, ext_date := {2020,10,10}, ext_store_id := 1, ext_budget_id := 1, ext_categories_id := [1,2], ext_period := 'month' },
   #{ id := <<"transaction3">>, ext_mapping_id := 2, ext_date := {2020,5,31}, ext_store_id := undefined, ext_budget_id := 1, ext_categories_id := undefined, ext_period := 'month' },
   #{ id := <<"transaction4">>, ext_mapping_id := 3, ext_date := {2020,7,8}, ext_store_id := undefined, ext_budget_id := 1, ext_categories_id := undefined, ext_period := 'quarter' },
   UpdatedTransaction,
   #{ id := <<"transaction2">>, ext_mapping_id := undefined, ext_store_id := undefined, ext_budget_id := undefined, ext_categories_id := undefined, ext_period := undefined }
  ] = Transactions2,

  ok.


should_db_upgrade_mappings_identical(_Config) ->
  ct:comment("Verify current mappings is empty"),
  {value, []} = banks_fetch_storage:get_budgets(),
  {value, []} = banks_fetch_storage:get_categories(),
  {value, []} = banks_fetch_storage:get_stores(),
  {value, []} = banks_fetch_storage:get_mappings(),

  ct:comment("Upgrade mappings"),
  Budgets = [#{ id => 0, name => <<"Aucun">> }, #{ id => 1, name => <<"Courant">>}, #{ id => 2, name => <<"Extra">> }],
  Categories = [ #{ id => 1, name => <<"Alimentation">>, up_category_id => none }, #{ id => 2, name => <<"Supermarché"/utf8>>, up_category_id => 1 }, #{ id => 3, name => <<"Logement">>, up_category_id => none }],
  Stores = [#{ id => 1, name => <<"Auchan">> }, #{ id => 2, name => <<"Carrefour">> }],
  Mappings = [#{ id => 1, pattern => <<"AUCHAN">>, fix_date => none, period => month, budget_id => 1, categories_id => [1, 2], store_id => 1 },
              #{ id => 2, pattern => <<"URSSAF">>, fix_date => previous2, period => month, budget_id => 1, categories_id => none, store_id => none },
              #{ id => 3, pattern => <<"CHARGES">>, fix_date => none, period => quarter, budget_id => 1, categories_id => none, store_id => none } ],
  ok = banks_fetch_storage:upgrade_mappings(Budgets, Categories, Stores, Mappings),
  verify_mappings(Budgets, Categories, Stores, Mappings),

  ct:comment("Upgrade again mappings"),
  ok = banks_fetch_storage:upgrade_mappings(Budgets, Categories, Stores, Mappings),

  ct:comment("Verify mappings in database"),
  verify_mappings(Budgets, Categories, Stores, Mappings),

  ok.


should_db_upgrade_mappings_updates(_Config) ->
  ct:comment("Verify current mappings is empty"),
  {value, []} = banks_fetch_storage:get_budgets(),
  {value, []} = banks_fetch_storage:get_categories(),
  {value, []} = banks_fetch_storage:get_stores(),
  {value, []} = banks_fetch_storage:get_mappings(),

  ct:comment("Upgrade mappings"),
  Budgets = [#{ id => 0, name => <<"Aucun">> }, #{ id => 1, name => <<"Courant">>}, #{ id => 2, name => <<"Extra">> }],
  Categories = [ #{ id => 1, name => <<"Alimentation">>, up_category_id => none }, #{ id => 2, name => <<"Supermarché"/utf8>>, up_category_id => 1 }, #{ id => 3, name => <<"Logement">>, up_category_id => none }],
  Stores = [#{ id => 1, name => <<"Auchan">> }, #{ id => 5, name => <<"Carrefour">> }],
  Mappings = [#{ id => 1, pattern => <<"AUCHAN">>, fix_date => none, period => month, budget_id => 1, categories_id => [1, 2], store_id => 1 },
              #{ id => 2, pattern => <<"URSSAF">>, fix_date => previous2, period => month, budget_id => 1, categories_id => none, store_id => none },
              #{ id => 3, pattern => <<"CHARGES">>, fix_date => none, period => quarter, budget_id => 1, categories_id => none, store_id => none } ],
  ok = banks_fetch_storage:upgrade_mappings(Budgets, Categories, Stores, Mappings),
  verify_mappings(Budgets, Categories, Stores, Mappings),

  ct:comment("Upgrade again mappings"),
  Budgets2 = [#{ id => 0, name => <<"Aucun">> }, #{ id => 2, name => <<"Extra (update)">> }, #{ id => 3, name => <<"NewBudget">> }],
  Categories2 = [ #{ id => 1, name => <<"Alimentation">>, up_category_id => none }, #{ id => 2, name => <<"Supermarché (update)"/utf8>>, up_category_id => 1 }, #{ id => 4, name => <<"NewCat">>, up_category_id => none }],
  Stores2 = [#{ id => 4, name => <<"MONOPRIX">> }, #{ id => 5, name => <<"Carrefour Update">> }],
  Mappings2 = [#{ id => 1, pattern => <<"AUCHAN">>, fix_date => none, period => month, budget_id => 1, categories_id => [1, 2], store_id => 1 },
               #{ id => 2, pattern => <<"URSSAF">>, fix_date => previous, period => month, budget_id => 1, categories_id => none, store_id => none } ],
  ok = banks_fetch_storage:upgrade_mappings(Budgets2, Categories2, Stores2, Mappings2),

  ct:comment("Verify mappings in database"),
  verify_mappings(Budgets2, Categories2, Stores2, Mappings2),

  ok.


should_db_upgrade_mappings_invalid_updates(_Config) ->
  ct:comment("Verify current mappings is empty"),
  {value, []} = banks_fetch_storage:get_budgets(),
  {value, []} = banks_fetch_storage:get_categories(),
  {value, []} = banks_fetch_storage:get_stores(),
  {value, []} = banks_fetch_storage:get_mappings(),

  ct:comment("Upgrade mappings"),
  Budgets = [#{ id => 0, name => <<"Aucun">> }, #{ id => 1, name => <<"Courant">>}, #{ id => 2, name => <<"Extra">> }],
  Categories = [ #{ id => 1, name => <<"Alimentation">>, up_category_id => none }, #{ id => 2, name => <<"Supermarché"/utf8>>, up_category_id => 1 }, #{ id => 3, name => <<"Logement">>, up_category_id => none }],
  Stores = [#{ id => 1, name => <<"Auchan">> }, #{ id => 5, name => <<"Carrefour">> }],
  Mappings = [#{ id => 1, pattern => <<"AUCHAN">>, fix_date => none, period => month, budget_id => 1, categories_id => [1, 2], store_id => 1 },
              #{ id => 2, pattern => <<"URSSAF">>, fix_date => previous2, period => month, budget_id => 1, categories_id => none, store_id => none },
              #{ id => 3, pattern => <<"CHARGES">>, fix_date => none, period => quarter, budget_id => 1, categories_id => none, store_id => none } ],
  ok = banks_fetch_storage:upgrade_mappings(Budgets, Categories, Stores, Mappings),
  verify_mappings(Budgets, Categories, Stores, Mappings),

  ct:comment("Upgrade again mappings"),
  Budgets2 = [#{ id => 0, name => <<"Aucun">> }, #{ id => 2, name => <<"Extra (update)">> }, #{ id => 3, name => <<"NewBudget">> }],
  Categories2 = [ #{ id => 1, name => <<"Alimentation">>, up_category_id => none }, #{ id => 2, name => <<"Supermarché (update)"/utf8>>, up_category_id => 1 }, #{ id => 4, name => <<"NewCat">>, up_category_id => none }],
  Stores2 = [#{ id => 4, name => <<"MONOPRIX">> }, #{ id => 5, name => <<"Carrefour Update">> }],
  % Invalid updates : two entries with same id
  Mappings2 = [#{ id => 1, pattern => <<"AUCHAN">>, fix_date => none, period => month, budget_id => 1, categories_id => [1, 2], store_id => 1 },
               #{ id => 1, pattern => <<"URSSAF">>, fix_date => previous, period => month, budget_id => 1, categories_id => none, store_id => none } ],
  {error, unable_to_upgrade_mappings} = banks_fetch_storage:upgrade_mappings(Budgets2, Categories2, Stores2, Mappings2),

  ct:comment("Verify mappings in database (no changes)"),
  verify_mappings(Budgets, Categories, Stores, Mappings),

  ok.

% Functions to verify mappings upgrade
verify_mappings(ExpectedBudgets, ExpectedCategories, ExpectedStores, ExpectedMappings) ->
  NbrExpectedBudgets = length(ExpectedBudgets),
  NbrExpectedCategories = length(ExpectedCategories),
  NbrExpectedStores = length(ExpectedStores),
  NbrExpectedMappings = length(ExpectedMappings),
  {value, NewBudgets} = banks_fetch_storage:get_budgets(),
  NbrExpectedBudgets = length(NewBudgets),
  ExpectedBudgets = sort_by_id(NewBudgets),
  {value, NewCategories} = banks_fetch_storage:get_categories(),
  NbrExpectedCategories = length(NewCategories),
  ExpectedCategories = sort_by_id(NewCategories),
  {value, NewStores} = banks_fetch_storage:get_stores(),
  NbrExpectedStores = length(NewStores),
  ExpectedStores = sort_by_id(NewStores),
  {value, NewMappings} = banks_fetch_storage:get_mappings(),
  NbrExpectedMappings = length(NewMappings),
  ExpectedMappings = sort_by_id(NewMappings),
  ok.

sort_by_id(MapsList) ->
  lists:sort(fun(#{ id := Id1 }, #{ id := Id2 }) -> Id1 < Id2 end, MapsList).
