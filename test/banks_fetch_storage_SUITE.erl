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

         should_db_get_clients/1,
         should_db_insert_client/1,
         should_db_not_insert_client_already_existing/1,
         should_db_store_accounts/1
        ]).

-define(DB_NAME, "banks_fetch_test").
-define(DB_USER, "banks_fetch_test").
-define(DB_PASSWORD, "banks_fetch_test").

-define(BANK_ID_1, <<"bank1">>).
-define(CLIENT_ID_1, <<"client1">>).
-define(CLIENT_CREDENTIAL_1, {<<"credential1">>}).
-define(FETCHING_AT_1, {{2020,7,7},{12,0,0}}).
-define(ACCOUNTS_1, [
                     #{ id => <<"ACCOUNT1">>, balance => 234.12, number => <<"NUMBER1">>, owner => <<"OWNER">>, ownership => single, type => current, name => <<"CURRENT">> },
                     #{ id => <<"ACCOUNT2">>, balance => 4321.78, number => <<"NUMBER2">>, owner => <<"OWNER">>, ownership => single, type => savings, name => <<"LDD">> }
                    ]).

-define(BANK_ID_2, <<"bank2">>).
-define(CLIENT_ID_2, <<"client2">>).
-define(CLIENT_CREDENTIAL_2, {<<"credential2">>}).

-define(BANK_ID_3, <<"bank3">>).
-define(CLIENT_ID_3, <<"client3">>).
-define(CLIENT_CREDENTIAL_3, {<<"credential3">>}).

all() ->
  [
   should_handle_cast_do_nothing,
   {group, tests_without_db},
   {group, tests_with_db}
  ].

groups() ->
  [
   {tests_without_db, [], [ should_nodb_start_without_db_upgrade, should_nodb_start_with_db_upgrade, should_nodb_start_with_db_upgrade_error, should_nodb_get_clients, should_nodb_insert_client, should_nodb_store_accounts ]},
   {tests_with_db, [], [ should_db_get_clients, should_db_insert_client, should_db_not_insert_client_already_existing, should_db_store_accounts ]}
  ].

% Init per group
init_per_group(tests_without_db, Config) ->
  meck:new(pgsql_connection),
  Config;
init_per_group(tests_with_db, Config) ->
  ok = application:start(pgsql),
  Connection = pgsql_connection:open("127.0.0.1", "postgres", ?DB_USER, ?DB_PASSWORD),

  DropTestDbQuery = "DROP DATABASE IF EXISTS " ?DB_NAME,
  {{drop, _}, []} = pgsql_connection:simple_query(DropTestDbQuery, Connection),
  CreateTestDbQuery = "CREATE DATABASE " ?DB_NAME " WITH OWNER " ?DB_USER,
  {{create, _}, []} = pgsql_connection:simple_query(CreateTestDbQuery, [], Connection),

  pgsql_connection:close(Connection),

  DBConnection = pgsql_connection:open("127.0.0.1", ?DB_NAME, ?DB_USER, ?DB_PASSWORD),
  [{db_connection, DBConnection} | Config].

% End per group
end_per_group(tests_without_db, _Config) ->
  meck:unload(pgsql_connection),
  ok;
end_per_group(tests_with_db, _Config) ->
  ok = application:stop(pgsql),
  ok.


% Init per testcase
init_per_testcase(should_db_get_clients, Config) ->
  {ok, PID} = banks_fetch_storage:start_link({?DB_NAME,?DB_USER,?DB_PASSWORD}),

  receive after 1000 -> nil end, % wait for db init

  {db_connection, Connection} = lists:keyfind(db_connection, 1, Config),
  {{insert,_,1},[]} = pgsql_connection:extended_query(<<"INSERT INTO clients(bank_id, client_id, client_credential) VALUES($1,$2,$3);">>, [?BANK_ID_1, ?CLIENT_ID_1, term_to_binary(?CLIENT_CREDENTIAL_1)], Connection),

  [{storage_pid, PID}|Config];

init_per_testcase(should_db_insert_client, Config) ->
  {ok, PID} = banks_fetch_storage:start_link({?DB_NAME,?DB_USER,?DB_PASSWORD}),
  [{storage_pid, PID}|Config];

init_per_testcase(should_db_not_insert_client_already_existing, Config) ->
  {ok, PID} = banks_fetch_storage:start_link({?DB_NAME,?DB_USER,?DB_PASSWORD}),
  [{storage_pid, PID}|Config];

init_per_testcase(should_db_store_accounts, Config) ->
  {ok, PID} = banks_fetch_storage:start_link({?DB_NAME,?DB_USER,?DB_PASSWORD}),
  [{storage_pid, PID}|Config];

init_per_testcase(_, Config) ->
  Config.

% End per testcase
end_per_testcase(should_db_get_clients, _Config) ->
  banks_fetch_storage:stop(),
  ok;
end_per_testcase(should_db_insert_client, _Config) ->
  banks_fetch_storage:stop(),
  ok;
end_per_testcase(should_db_not_insert_client_already_existing, _Config) ->
  banks_fetch_storage:stop(),
  ok;
end_per_testcase(should_db_store_accounts, _Config) ->
  banks_fetch_storage:stop(),
  ok;
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
               {[<<"COMMENT ON DATABASE banks_fetch_test IS '0.0.1';">>, [], fake_connection],
                {'comment', []}
               },
               {[<<"COMMIT">>, [], fake_connection],
                {'commit', []}
               },
               {['_',[], fake_connection], fake_ok}
              ]),

  {ok, _PID} = banks_fetch_storage:start_link({?DB_NAME,?DB_USER,?DB_PASSWORD}),
  meck:wait(pgsql_connection, extended_query, [<<"COMMIT">>, [], fake_connection], 3000),
  true = meck:validate(pgsql_connection),
  9 = meck:num_calls(pgsql_connection, extended_query, '_'),

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
  ok = banks_fetch_storage:store_accounts(?BANK_ID_1, ?CLIENT_ID_1, ?FETCHING_AT_1, ?ACCOUNTS_1),

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

should_db_get_clients(_Config) ->
  ct:comment("Get clients"),
  Clients = banks_fetch_storage:get_clients(),

  ct:comment("Verify returned clients"),
  {value, [{{bank_id, ?BANK_ID_1},{client_id, ?CLIENT_ID_1},{client_credential, ?CLIENT_CREDENTIAL_1}}]} = Clients,
  ok.


should_db_insert_client(Config) ->
  ct:comment("Insert client"),
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
  ok = banks_fetch_storage:store_accounts(?BANK_ID_1, ?CLIENT_ID_1, ?FETCHING_AT_1, ?ACCOUNTS_1),

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
