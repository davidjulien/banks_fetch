%%%-----------------------------------------------------------------------
%% @doc banks_fetch_storage manages data storage in a PostgesSQL database.
%% When the gen_server is started, it calls itself to init/upgrade database schema if necessary.
%%
%% Instructions in pgsql shell to initialize user and database:
%% CREATE ROLE banks_fetch_user WITH LOGIN;
%% CREATE DATABASE banks_fetch;
%% GRANT ALL PRIVILEGES ON DATABASE banks_fetch TO banks_fetch_user;
%% ALTER DATABASE banks_fetch OWNER TO banks_fetch_user;
%%
%% @end
%%%-----------------------------------------------------------------------

-module(banks_fetch_storage).
-behaviour(gen_server).

-include("banks_fetch_storage.hrl").

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-export([
         get_banks/0,

         get_clients/0,
         insert_client/3,

         store_accounts/4,
         get_accounts/2,

         store_transactions/5,
         get_last_transactions_id/2,
         get_transactions/3,
         get_last_transactions/1,

         stop/0
        ]).


-record(state, {
          credential :: {string(), string(), string()},
          connection :: undefined | pgsql_connection:pgsql_connection()
         }).


-spec get_banks() -> {value, [banks_fetch_bank:bank()]}.
get_banks() ->
  gen_server:call(?MODULE, get_banks).


-spec get_clients() -> {value, [{banks_fetch_bank:bank_id(), banks_fetch_bank:client_id(), banks_fetch_bank:client_credential(any())}]}.
get_clients() ->
  gen_server:call(?MODULE, get_clients).

-spec insert_client(banks_fetch_bank:bank_id(), banks_fetch_bank:client_id(), banks_fetch_bank:client_credential(any())) -> ok | {error, already_inserted}.
insert_client(BankId, ClientId, ClientCredential) ->
  gen_server:call(?MODULE, {insert_client, BankId, ClientId, ClientCredential}).


-spec store_accounts(banks_fetch_bank:bank_id(), banks_fetch_bank:client_id(), calendar:datetime(), [banks_fetch_bank:account()]) -> ok.
store_accounts(BankId, ClientId, FetchingAt, AccountsList) ->
  gen_server:call(?MODULE, {store_accounts, BankId, ClientId, FetchingAt, AccountsList}).

-spec get_accounts(banks_fetch_bank:bank_id(), banks_fetch_bank:client_id()) -> {ok, [banks_fetch_bank:account()]}.
get_accounts(BankId, ClientId) ->
  gen_server:call(?MODULE, {get_accounts, BankId, ClientId}).


%% @doc Store transactions
-spec store_transactions(banks_fetch_bank:bank_id(), banks_fetch_bank:client_id(), banks_fetch_bank:account_id(), calendar:datetime(), [banks_fetch_bank:transaction()]) -> ok.
store_transactions(BankId, ClientId, AccountId, FetchingAt, TransactionsList) ->
  gen_server:call(?MODULE, {store_transactions, BankId, ClientId, AccountId, FetchingAt, TransactionsList}).

%% @doc Returns last transaction id for each account for a given bank/client
-spec get_last_transactions_id(banks_fetch_bank:bank_id(), banks_fetch_bank:client_id()) -> {value, [{banks_fetch_bank:account_id(), banks_fetch_bank:transaction_id()}]}.
get_last_transactions_id(BankId, ClientId) ->
  gen_server:call(?MODULE, {get_last_transactions_id, BankId, ClientId}).

%% @doc Returns all transactions for a given account
-spec get_transactions(banks_fetch_bank:bank_id(), banks_fetch_bank:client_id(), banks_fetch_bank:account_id()) -> {value, [banks_fetch_bank:transaction()]}.
get_transactions(BankId, ClientId, AccountId) ->
  gen_server:call(?MODULE, {get_transactions, BankId, ClientId, AccountId}).

%% @doc Returns last N transactions for any account
-spec get_last_transactions(non_neg_integer()) -> {value, [banks_fetch_bank:transaction()]}.
get_last_transactions(N) ->
  gen_server:call(?MODULE, {get_last_transactions, N}).



-spec stop() -> ok.
stop() ->
  gen_server:call(?MODULE, stop).


start_link({DatabaseName,Username,Password}) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, {DatabaseName,Username,Password}, []).

init(Credential) ->
  self() ! init_db,
  {ok, #state{ credential = Credential, connection = undefined }}.


handle_call(get_banks, _From, #state{ } = State0) ->
  R = do_get_banks(State0),
  {reply, R, State0};
handle_call(get_clients, _From, #state{ } = State0) ->
  R = do_get_clients(State0),
  {reply, R, State0};
handle_call({insert_client, BankId, ClientId, ClientCredential},  _From, #state{ } = State0) ->
  R = do_insert_client(BankId, ClientId, ClientCredential, State0),
  {reply, R, State0};
handle_call({store_accounts, BankId, ClientId, FetchingAt, AccountsList}, _From, #state{ } = State0) ->
  R = do_store_accounts(BankId, ClientId, FetchingAt, AccountsList, State0),
  {reply, R, State0};
handle_call({get_accounts, BankId, ClientId}, _From, #state{ } = State0) ->
  R = do_get_accounts(BankId, ClientId, State0),
  {reply, R, State0};
handle_call({store_transactions, BankId, ClientId, AccountId, FetchingAt, TransactionsList}, _From, #state{ } = State0) ->
  R = do_store_transactions(BankId, ClientId, AccountId, FetchingAt, TransactionsList, State0),
  {reply, R, State0};
handle_call({get_last_transactions_id, BankId, ClientId}, _From, #state{ } = State0) ->
  R = do_get_last_transactions_id(BankId, ClientId, State0),
  {reply, R, State0};
handle_call({get_transactions, BankId, ClientId, AccountId}, _From, #state{ } = State0) ->
  R = do_get_transactions(BankId, ClientId, AccountId, State0),
  {reply, R, State0};
handle_call({get_last_transactions, N}, _From, #state{ } = State0) ->
  R = do_get_last_transactions(N, State0),
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
-spec do_init_db(#state{}) -> #state{}.
do_init_db(#state{ credential = {DatabaseName, Username, Password}, connection = undefined } = State0) ->
  State1 = State0#state{ connection = pgsql_connection:open(DatabaseName,Username,Password) },
  do_init_db(State1);
do_init_db(#state{ credential = {DatabaseName, _Username, _Password}, connection = Connection } = State) ->
  ok = upgrade_schema(DatabaseName, Connection),
  State.


%%
%% @doc Get banks from database
%%
-spec do_get_banks(#state{}) -> {value, [banks_fetch_bank:bank()]}.
do_get_banks(#state{ connection = Connection }) ->
  case pgsql_connection:simple_query(<<"SELECT id, name FROM banks;">>, Connection) of
    {{select, _N}, List0} ->
      error_logger:info_msg("List0=~p", [List0]),
      List1 = [ #{ id => Id, name => Name } || {Id, Name} <- List0 ],
      {value, List1}
  end.


%%
%% @doc Get clients from database
%%
-spec do_get_clients(#state{}) -> {value, [{banks_fetch_bank:bank_id(), banks_fetch_bank:client_id(), banks_fetch_bank:client_credential(any())}]}.
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
-spec do_store_accounts(banks_fetch_bank:bank_id(), banks_fetch_bank:client_id(), calendar:datetime(), [banks_fetch_bank:account()], #state{}) -> ok.
do_store_accounts(BankId, ClientId, FetchingAt, AccountsList, #state{ connection = Connection }) ->
  {'begin', []} = pgsql_connection:simple_query(<<"BEGIN TRANSACTION">>, Connection),
  ok = do_store_accounts_aux(BankId, ClientId, FetchingAt, AccountsList, Connection),
  {'commit', []} = pgsql_connection:simple_query(<<"COMMIT">>, Connection),
  ok.

do_store_accounts_aux(_BankId, _ClientId, _FetchingAt, [], _Connection) ->
  ok;
do_store_accounts_aux({bank_id, BankIdValue} = BankId, {client_id, ClientIdValue} = ClientId, FetchingAt, [#{ id := AccountId, balance := Balance, number := Number, owner := Owner, ownership := Ownership, type := Type, name := Name } | NextAccounts], Connection) ->
  case pgsql_connection:extended_query(<<"INSERT INTO accounts(bank_id, client_id, fetching_at, account_id, balance, number, owner, ownership, type, name) VALUES($1,$2,$3,$4,$5,$6,$7,$8,$9,$10);">>, 
                                       [BankIdValue, ClientIdValue, FetchingAt, AccountId, Balance, Number, Owner, atom_to_binary(Ownership,'utf8'), atom_to_binary(Type,'utf8'), Name], Connection) of
    {{insert,_,1},[]} ->
      do_store_accounts_aux(BankId, ClientId, FetchingAt, NextAccounts, Connection)
  end.

%%
%% @doc Get accounts
%%
-spec do_get_accounts(banks_fetch_bank:bank_id(), banks_fetch_bank:client_id(), #state{}) -> {ok, [banks_fetch_bank:account()]}.
do_get_accounts(BankId, ClientId, #state{ connection = Connection }) ->
  {{select, _Nbr}, Accounts} = pgsql_connection:simple_query(<<"SELECT distinct on (bank_id, client_id, account_id) bank_id, client_id, account_id, balance, number, owner, ownership, type, name FROM accounts ORDER BY bank_id, client_id, account_id, fetching_at DESC">>, [BankId, ClientId], Connection),
  {ok, [#{ id => AccountId, balance => Balance, number => Number, owner => Owner, ownership => binary_to_atom(Ownership), type => binary_to_atom(Type), name => Name } 
        || {_BankId, _ClientId, AccountId, Balance, Number, Owner, {e_account_ownership, Ownership}, {e_account_type, Type}, Name} <- Accounts ]}.


%%
%% @doc Store transactions
%%
-spec do_store_transactions(banks_fetch_bank:bank_id(), banks_fetch_bank:client_id(), banks_fetch_bank:account_id(), calendar:datetime(), [banks_fetch_bank:transaction()], #state{}) -> ok.
do_store_transactions({bank_id, BankIdValue}, {client_id, ClientIdValue}, {account_id, AccountIdValue}, FetchingAt, TransactionsList, #state{ connection = Connection }) ->
  {'begin', []} = pgsql_connection:simple_query(<<"BEGIN TRANSACTION">>, Connection),
  lists:foldl(fun(#{ id := TransactionId, accounting_date := AccountingDate, effective_date := EffectiveDate, amount := Amount, description := Description, type := Type }, Position) ->
                    {{insert, _, 1}, []} = pgsql_connection:extended_query(<<"INSERT INTO transactions(bank_id, client_id, account_id, fetching_at, fetching_position, transaction_id, accounting_date, effective_date, amount, description, type) VALUES($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11);">>, [BankIdValue, ClientIdValue, AccountIdValue, FetchingAt, Position, TransactionId, AccountingDate, EffectiveDate, Amount, Description, atom_to_binary(Type,'utf8')], Connection),
                    Position+1
                end, 0, TransactionsList),
  {'commit', []} = pgsql_connection:simple_query(<<"COMMIT;">>, Connection),
  ok.


%%
%% @doc Get last transactions id for each account of a given bank/client
%%
-spec do_get_last_transactions_id(banks_fetch_bank:bank_id(), banks_fetch_bank:client_id(), #state{}) -> {value, [{banks_fetch_bank:account_id(), banks_fetch_bank:transaction_id()}]}.
do_get_last_transactions_id({bank_id, BankIdValue}, {client_id, ClientIdValue}, #state{ connection = Connection }) ->
  case pgsql_connection:extended_query(<<"SELECT distinct on (account_id) account_id, transaction_id FROM transactions WHERE fetching_position = 0 and bank_id = $1 and client_id = $2 order by account_id, fetching_at desc;">>, [BankIdValue, ClientIdValue], Connection) of
    {{select, _N}, List0} ->
      List1 = [ {{account_id, AccountIdVal}, {transaction_id, TransactionIdVal}} || {AccountIdVal, TransactionIdVal} <- List0 ],
      {value, List1}
  end.


%%
%% @doc Get all transactions for a given account
%%
-spec do_get_transactions(banks_fetch_bank:bank_id(), banks_fetch_bank:client_id(), banks_fetch_bank:account_id(), #state{}) -> {value, [banks_fetch_bank:transaction()]}.
do_get_transactions({bank_id, BankIdValue}, {client_id, ClientIdValue}, {account_id, AccountIdValue}, #state{ connection = Connection }) ->
  case pgsql_connection:extended_query(<<"SELECT transaction_id, accounting_date, effective_date, amount, description, type FROM transactions WHERE bank_id = $1 and client_id = $2 and account_id = $3 ORDER BY transaction_id DESC;">>, [BankIdValue, ClientIdValue, AccountIdValue], Connection) of
    {{select, _N}, List0} ->
      List1 = [ #{ id => TransactionId, accounting_date => AccountingDate, effective_date => EffectiveDate, amount => Amount, description => Description, type => binary_to_atom(Type,'utf8') } ||
                {TransactionId, AccountingDate, EffectiveDate, Amount, Description, {e_transaction_type, Type}} <- List0 ],
      {value, List1}
  end.

%%
%% @doc Get last N transactions for all accounts. Order by effective_date desc. If effective_dates are identical, grouped transactions by bank/client/account
%%
-spec do_get_last_transactions(non_neg_integer(), #state{}) -> {value, [banks_fetch_bank:transaction()]}.
do_get_last_transactions(N, #state{ connection = Connection }) ->
  case pgsql_connection:extended_query(<<"SELECT transaction_id, bank_id, client_id, account_id, accounting_date, effective_date, amount, description, type FROM transactions ORDER BY effective_date DESC, bank_id, client_id, account_id, fetching_at DESC, fetching_position ASC LIMIT $1;">>, [N], Connection) of
    {{select, _N}, List0} ->
      List1 = [ #{ id => TransactionId, accounting_date => AccountingDate, effective_date => EffectiveDate, amount => Amount, description => Description, type => binary_to_atom(Type,'utf8'),
                bank_id => {bank_id, BankIdVal}, client_id => {client_id, ClientIdVal}, account_id => {account_id, AccountIdVal} } ||
                {TransactionId, BankIdVal, ClientIdVal, AccountIdVal, AccountingDate, EffectiveDate, Amount, Description, {e_transaction_type, Type}} <- List0 ],
      {value, List1}
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
      ok = lager:info("storage : no upgrade from ~s", [VersionStr]),
      ok;
    {_, NextVersion, Queries} ->
      ok = lager:info("storage : upgrade from ~s to ~s", [VersionStr, NextVersion]),
      {'begin', []} = pgsql_connection:extended_query(<<"BEGIN TRANSACTION">>, [], Connection),
      case upgrade_schema_aux_loop_queries(Queries, Connection) of
        {stop, QueryStr, Err} ->
          ok = lager:error("~p : unable to execute query : ~s : ~p", [QueryStr, Err]),
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
