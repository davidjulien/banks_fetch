%%%-----------------------------------------------------------------------
%% @doc banks_fetch_storage manages data storage in a PostgesSQL database.
%% When the gen_server is started, it calls itself to init/upgrade database schema if necessary.
%% @end
%%%-----------------------------------------------------------------------

-module(banks_fetch_storage).
-behaviour(gen_server).

-include("banks_fetch_storage.hrl").

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-export([
         get_clients/0,
         insert_client/3,

         store_accounts/4,
         stop/0
        ]).


-record(state, {
          credential :: {string(), string(), string()},
          connection :: undefined | pgsql_connection:pgsql_connection()
         }).


-spec get_clients() -> {value, [{banks_fetch_bank:bank_id(), banks_fetch_bank:client_id(), banks_fetch_bank:client_credential(any())}]}.
get_clients() ->
  gen_server:call(?MODULE, get_clients).

-spec insert_client(banks_fetch_bank:bank_id(), banks_fetch_bank:client_id(), banks_fetch_bank:client_credential(any())) -> ok | {error, already_inserted}.
insert_client(BankId, ClientId, ClientCredential) ->
  gen_server:call(?MODULE, {insert_client, BankId, ClientId, ClientCredential}).

-spec store_accounts(banks_fetch_bank:bank_id(), banks_fetch_bank:client_id(), calendar:datetime(), [banks_fetch_bank:account()]) -> ok.
store_accounts(BankId, ClientId, FetchingAt, AccountsList) ->
  gen_server:call(?MODULE, {store_accounts, BankId, ClientId, FetchingAt, AccountsList}).

-spec stop() -> ok.
stop() ->
  gen_server:call(?MODULE, stop).


start_link({DatabaseName,Username,Password}) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, {DatabaseName,Username,Password}, []).

init(Credential) ->
  self() ! init_db,
  {ok, #state{ credential = Credential, connection = undefined }}.


handle_call(get_clients, _From, #state{ } = State0) ->
  R = do_get_clients(State0),
  {reply, R, State0};
handle_call({insert_client, BankId, ClientId, ClientCredential},  _From, #state{ } = State0) ->
  R = do_insert_client(BankId, ClientId, ClientCredential, State0),
  {reply, R, State0};
handle_call({store_accounts, BankId, ClientId, FetchingAt, AccountsList}, _From, #state{ } = State0) ->
  R = do_store_accounts(BankId, ClientId, FetchingAt, AccountsList, State0),
  {reply, R, State0};
handle_call(stop, _From, State0) ->
  {stop, normal, stopped, State0}.

handle_cast(_, State0) ->
  {noreply, State0}.

handle_info(init_db, State0) ->
  State1 = do_init_db(State0),
  {noreply, State1}.


%%
%% Database initialisation
%%
do_init_db(#state{ credential = {DatabaseName, Username, Password}, connection = undefined } = State0) ->
  State1 = State0#state{ connection = pgsql_connection:open(DatabaseName,Username,Password) },
  do_init_db(State1);
do_init_db(#state{ credential = {DatabaseName, _Username, _Password}, connection = Connection } = State) ->
  ok = upgrade_schema(DatabaseName, Connection),
  State.



%%
%% @doc Get clients from database
%%
do_get_clients(#state{ connection = Connection }) ->
  case pgsql_connection:simple_query(<<"SELECT bank_id, client_id, client_credential FROM clients;">>, Connection) of
    {{select, _N}, List0} ->
      List1 = [ {{bank_id, BankIdVal}, {client_id, ClientIdVal}, {client_credential, binary_to_term(ClientCredentialBin)}} || {BankIdVal, ClientIdVal, ClientCredentialBin} <- List0 ],
      {value, List1}
  end.

%%
%% @doc Insert new client in database
%%
-spec do_insert_client(banks_fetch_bank:bank_id(), banks_fetch_bank:client_id(), banks_fetch_bank:client_credential(any()), #state{}) -> ok | {error, already_inserted}.
do_insert_client({bank_id, BankIdValue}, {client_id, ClientIdValue}, {client_credential, ClientCredentialValue}, #state{ connection = Connection }) ->
  case pgsql_connection:extended_query(<<"INSERT INTO clients(bank_id, client_id, client_credential) VALUES($1,$2,$3);">>, [BankIdValue, ClientIdValue, term_to_binary(ClientCredentialValue)], Connection) of
    {{insert, _, 1}, []} ->
      ok;
    {error, Error} ->
      true = pgsql_error:is_integrity_constraint_violation(Error),
      {error, already_inserted}
  end.


%%
%% @doc Store accounts
%%
do_store_accounts(BankId, ClientId, FetchingAt, AccountsList, #state{ connection = Connection }) ->
  {'begin', []} = pgsql_connection:simple_query(<<"BEGIN TRANSACTION">>, Connection),
  ok = do_store_accounts_aux(BankId, ClientId, FetchingAt, AccountsList, Connection),
  {'commit', []} = pgsql_connection:simple_query(<<"COMMIT">>, Connection),
  ok.

do_store_accounts_aux(_BankId, _ClientId, _FetchingAt, [], _Connection) ->
  ok;
do_store_accounts_aux(BankId, ClientId, FetchingAt, [#{ id := AccountId, balance := Balance, number := Number, owner := Owner, ownership := Ownership, type := Type, name := Name } | NextAccounts], Connection) ->
  case pgsql_connection:extended_query(<<"INSERT INTO accounts(bank_id, client_id, fetching_at, account_id, balance, number, owner, ownership, type, name) VALUES($1,$2,$3,$4,$5,$6,$7,$8,$9,$10);">>, 
                                       [BankId, ClientId, FetchingAt, AccountId, Balance, Number, Owner, atom_to_binary(Ownership,'utf8'), atom_to_binary(Type,'utf8'), Name], Connection) of
    {{insert,_,1},[]} ->
      do_store_accounts_aux(BankId, ClientId, FetchingAt, NextAccounts, Connection)
  end.


%%
%% Upgrade schema
%%
upgrade_schema(DatabaseName, Connection) ->
  % Current schema version is stored in database description
  VersionStr = case pgsql_connection:extended_query(<<"SELECT description FROM pg_shdescription JOIN pg_database on objoid = pg_database.oid WHERE datname = $1">>, [DatabaseName], Connection) of
                 {{'select', 1}, [{VersionStr0}]} -> VersionStr0;
                 {{'select', 0}, []} -> <<"0.0.0">>
               end,
  upgrade_schema_aux(VersionStr, DatabaseName, Connection).

upgrade_schema_aux(VersionStr, DatabaseName, Connection) ->
  % Try to find an upgrade from current database version to next version. If any, applies queries in a transaction
  case lists:keyfind(VersionStr, 1, ?SCHEMA) of
    false ->
      ok;
    {_, NextVersion, Queries} ->
      {'begin', []} = pgsql_connection:extended_query(<<"BEGIN TRANSACTION">>, [], Connection),
      case upgrade_schema_aux_loop_queries(Queries, Connection) of
        {stop, QueryStr, Err} ->
          {'rollback', []} = pgsql_connection:extended_query(<<"ROLLBACK">>, [], Connection),
          {error, {unable_to_upgrade_db, QueryStr, Err}};
        ok ->
          {'comment', []} = pgsql_connection:extended_query(list_to_binary([<<"COMMENT ON DATABASE ">>, DatabaseName, <<" IS '">>, NextVersion, <<"';">>]), [], Connection),
          {'commit', []} = pgsql_connection:extended_query(<<"COMMIT">>, [], Connection),
          upgrade_schema_aux(NextVersion, DatabaseName, Connection)
      end
    end.

upgrade_schema_aux_loop_queries([], _Connection) ->
  ok;
upgrade_schema_aux_loop_queries([Query | NextQueries], Connection) ->
  case pgsql_connection:extended_query(Query, [], Connection) of
    {error, _} = Err ->
      {stop, Query, Err};
    _ ->
      upgrade_schema_aux_loop_queries(NextQueries, Connection)
  end.
