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

         get_all_accounts/0,
         store_accounts/4,
         get_accounts/2,
         get_account_balance_history/4,

         get_budgets/0,
         get_categories/0,

         get_stores/0,
         insert_store/1,

         get_mappings/0,
         apply_mappings/0,
         upgrade_mappings/4,
         insert_mapping/6,

         store_transactions/5,
         get_last_transactions_id/2,
         get_transactions/3,
         get_last_transactions/2,
         update_transaction/10,
         split_transaction/4,

         aggregate_amounts_for_purse/4,
         get_new_transactions_for_purse/4,
         copy_withdrawal_transaction_to_purse/5,

         stop/0
        ]).


-record(state, {
          credential :: {string(), string(), string()},
          connection :: undefined | pgsql_connection:pgsql_connection()
         }).

-define(LONG_TIMEOUT, 60000).
-define(VERY_LONG_TIMEOUT, 5*60000).

% Functions related to banks

-spec get_banks() -> {value, [banks_fetch_bank:bank()]}.
get_banks() ->
  gen_server:call(?MODULE, get_banks, ?LONG_TIMEOUT).


% Functions related to clients

-spec get_clients() -> {value, [{banks_fetch_bank:bank_id(), banks_fetch_bank:client_id(), banks_fetch_bank:client_credential(any())}]}.
get_clients() ->
  gen_server:call(?MODULE, get_clients, ?LONG_TIMEOUT).

-spec insert_client(banks_fetch_bank:bank_id(), banks_fetch_bank:client_id(), banks_fetch_bank:client_credential(any())) -> ok | {error, already_inserted}.
insert_client(BankId, ClientId, ClientCredential) ->
  gen_server:call(?MODULE, {insert_client, BankId, ClientId, ClientCredential}, ?LONG_TIMEOUT).


% Functions related to budgets

%% @doc Return all stored budgets
-spec get_budgets() -> {value, [banks_fetch_bank:budget()]}.
get_budgets() ->
  gen_server:call(?MODULE, get_budgets, ?LONG_TIMEOUT).


% Functions related to categories

%% @doc Return all stored categories
-spec get_categories() -> {value, [banks_fetch_bank:category()]}.
get_categories() ->
  gen_server:call(?MODULE, get_categories, ?LONG_TIMEOUT).


% Functions related to stores

%% @doc Return all stored stores
-spec get_stores() -> {value, [banks_fetch_bank:store()]}.
get_stores() ->
  gen_server:call(?MODULE, get_stores, ?LONG_TIMEOUT).

%% @doc Insert store
-spec insert_store(unicode:unicode_binary()) -> {ok, banks_fetch_bank:store()} | {error, already_inserted}.
insert_store(StoreName) ->
  gen_server:call(?MODULE, {insert_store, StoreName}, ?LONG_TIMEOUT).


% Functions related to accounts

%% @doc Return all stored accounts
-spec get_all_accounts() -> {value, [banks_fetch_bank:account()]}.
get_all_accounts() ->
  gen_server:call(?MODULE, get_all_accounts, ?LONG_TIMEOUT).

-spec store_accounts(banks_fetch_bank:bank_id(), banks_fetch_bank:client_id(), calendar:datetime(), [banks_fetch_bank:account()]) -> ok.
store_accounts(BankId, ClientId, FetchingAt, AccountsList) ->
  gen_server:call(?MODULE, {store_accounts, BankId, ClientId, FetchingAt, AccountsList}, ?LONG_TIMEOUT).

-spec get_accounts(banks_fetch_bank:bank_id(), banks_fetch_bank:client_id()) -> {value, [banks_fetch_bank:account()]}.
get_accounts(BankId, ClientId) ->
  gen_server:call(?MODULE, {get_accounts, BankId, ClientId}, ?LONG_TIMEOUT).

% Get last N balances history in reverse order
-spec get_account_balance_history(banks_fetch_bank:bank_id(), banks_fetch_bank:client_id(), banks_fetch_bank:account_id(), non_neg_integer()) -> {value, [{calendar:datetime(), float()}]}.
get_account_balance_history(BankId, ClientId, AccountId, Nbr) ->
  gen_server:call(?MODULE, {get_account_balance_history, BankId, ClientId, AccountId, Nbr}, ?LONG_TIMEOUT).

% Functions related to mappings

-spec get_mappings() -> {value, [any()]}.
get_mappings() ->
  gen_server:call(?MODULE, get_mappings, ?LONG_TIMEOUT).

-spec apply_mappings() -> ok.
apply_mappings() ->
  gen_server:call(?MODULE, apply_mappings, ?VERY_LONG_TIMEOUT).

-spec upgrade_mappings([banks_fetch_bank:budget()], [banks_fetch_bank:category()],  [banks_fetch_bank:store()], [banks_fetch_bank:mapping()]) -> ok | {error, unable_to_upgrade_mappings}.
upgrade_mappings(Budgets, Categories, Stores, Mappings) ->
  gen_server:call(?MODULE, {upgrade_mappings, Budgets, Categories, Stores, Mappings}, ?VERY_LONG_TIMEOUT).

-spec insert_mapping(unicode:unicode_binary(), none | non_neg_integer(), none | [non_neg_integer()], none | non_neg_integer(),
                     banks_fetch_bank:mapping_fix_date(), banks_fetch_bank:mapping_period()) -> {ok, banks_fetch_bank:mapping()} | {error, already_inserted}.
insert_mapping(Pattern, BudgetId, CategoriesIds, StoreId, FixDate, Period) ->
  gen_server:call(?MODULE, {insert_mapping, Pattern, BudgetId, CategoriesIds, StoreId, FixDate, Period}, ?LONG_TIMEOUT).


% Functions related to transactions


%% @doc Store transactions
-spec store_transactions(banks_fetch_bank:bank_id(), banks_fetch_bank:client_id(), banks_fetch_bank:account_id(), calendar:datetime(), [banks_fetch_bank:transaction()]) -> ok.
store_transactions(BankId, ClientId, AccountId, FetchingAt, TransactionsList) ->
  gen_server:call(?MODULE, {store_transactions, BankId, ClientId, AccountId, FetchingAt, TransactionsList}, ?LONG_TIMEOUT).

%% @doc Returns last transaction id for each account for a given bank/client
-spec get_last_transactions_id(banks_fetch_bank:bank_id(), banks_fetch_bank:client_id()) -> {value, [{banks_fetch_bank:account_id(), banks_fetch_bank:transaction_id()}]}.
get_last_transactions_id(BankId, ClientId) ->
  gen_server:call(?MODULE, {get_last_transactions_id, BankId, ClientId}, ?LONG_TIMEOUT).

%% @doc Returns all transactions for a given account
-spec get_transactions(banks_fetch_bank:bank_id(), banks_fetch_bank:client_id(), banks_fetch_bank:account_id()) -> {value, [banks_fetch_bank:transaction()]}.
get_transactions(BankId, ClientId, AccountId) ->
  gen_server:call(?MODULE, {get_transactions, BankId, ClientId, AccountId}, ?LONG_TIMEOUT).

%% @doc Returns last N transactions for any account
-spec get_last_transactions(none | unicode:unicode_binary(), non_neg_integer()) -> {value, {none | unicode:unicode_binary(), non_neg_integer(), [banks_fetch_bank:transaction()]}} | {error, invalid_cursor}.
get_last_transactions(CursorOpt, N) ->
  gen_server:call(?MODULE, {get_last_transactions, CursorOpt, N}, ?LONG_TIMEOUT).

%% @doc Update transaction
-spec update_transaction(banks_fetch_bank:bank_id(), banks_fetch_bank:client_id(), banks_fetch_bank:account_id(), banks_fetch_bank:transaction_id(),
                         undefined | calendar:date(), undefined | banks_fetch_bank:mapping_period(), undefined | non_neg_integer(), undefined | non_neg_integer(), undefined | [non_neg_integer()],
                         undefined | float()
                        ) -> {ok, banks_fetch_bank:transaction()} | {error, any()}.
update_transaction(BankId, AccountId, ClientId, TransactionId, Date, Period, StoreId, BudgetId, CategoriesIds, Amount) ->
  gen_server:call(?MODULE, {update_transaction, BankId, AccountId, ClientId, TransactionId, Date, Period, StoreId, BudgetId, CategoriesIds, Amount}, ?LONG_TIMEOUT).

%% @doc Split a transaction. Useful when a transaction merged different kind of payment
-spec split_transaction(banks_fetch_bank:bank_id(), banks_fetch_bank:client_id(), banks_fetch_bank:account_id(), banks_fetch_bank:transaction_id()) -> {ok, [banks_fetch_bank:transaction()]} | {error, not_found}.
split_transaction(BankId, AccountId, ClientId, TransactionId) ->
  gen_server:call(?MODULE, {split_transaction, BankId, AccountId, ClientId, TransactionId}, ?LONG_TIMEOUT).


%% @doc Compute total amounts in purse from transactions.
-spec aggregate_amounts_for_purse(calendar:date(), calendar:datetime(), banks_fetch_bank:client_id(), [{banks_fetch_bank:bank_id(), banks_fetch_bank:client_id(), banks_fetch_bank:account_id()}]) -> {value, float()}.
aggregate_amounts_for_purse(StartDate, UntilDate, PurseId, SourcesList) ->
  gen_server:call(?MODULE, {aggregate_amounts_for_purse, StartDate, UntilDate, PurseId, SourcesList}, ?LONG_TIMEOUT).

%% @doc Get new transactions (withdrawals) from sources. They will be included into purse.
-spec get_new_transactions_for_purse(calendar:date(), calendar:datetime(), banks_fetch_bank:client_id(), [{banks_fetch_bank:bank_id(), banks_fetch_bank:client_id(), banks_fetch_bank:account_id()}]) -> {value, [banks_fetch_bank:transaction()]}.
get_new_transactions_for_purse(StartDate, UntilDate, PurseId, SourcesList) ->
  gen_server:call(?MODULE, {get_new_transactions_for_purse, StartDate, UntilDate, PurseId, SourcesList}, ?LONG_TIMEOUT).

%% @doc Copy an existing withdrawal transaction to purse account, and recompute purse account values
-spec copy_withdrawal_transaction_to_purse(banks_fetch_bank:bank_id(), banks_fetch_bank:client_id(), banks_fetch_bank:account_id(), banks_fetch_bank:transaction_id(), 
                                          banks_fetch_bank:client_id()) -> ok.
copy_withdrawal_transaction_to_purse(BankId, ClientId, AccountId, TransactionId, PurseId) ->
  gen_server:call(?MODULE, {copy_withdrawal_transaction_to_purse, BankId, ClientId, AccountId, TransactionId, PurseId}, ?LONG_TIMEOUT).


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
handle_call(get_budgets, _From, #state{ } = State0) ->
  R = do_get_budgets(State0),
  {reply, R, State0};
handle_call(get_categories, _From, #state{ } = State0) ->
  R = do_get_categories(State0),
  {reply, R, State0};
handle_call(get_stores, _From, #state{ } = State0) ->
  R = do_get_stores(State0),
  {reply, R, State0};
handle_call({insert_store, StoreName}, _From, #state{ } = State0) ->
  R = do_insert_store_with_name(StoreName, State0),
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
handle_call(get_all_accounts, _From, #state{ } = State0) ->
  R = do_get_all_accounts(State0),
  {reply, R, State0};
handle_call({get_accounts, BankId, ClientId}, _From, #state{ } = State0) ->
  R = do_get_accounts(BankId, ClientId, State0),
  {reply, R, State0};
handle_call({get_account_balance_history, BankId, ClientId, AccountId, Nbr}, _From, #state{ } = State0) ->
  R = do_get_account_balance_history(BankId, ClientId, AccountId, Nbr, State0),
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
handle_call({get_last_transactions, CursorOpt, N}, _From, #state{ } = State0) ->
  R = do_get_last_transactions(CursorOpt, N, State0),
  {reply, R, State0};
handle_call({update_transaction, BankId, ClientId, AccountId, TransactionId, Date, Period, StoreId, BudgetId, CategoriesIds, Amount}, _From, #state{ } = State0) ->
  R = do_update_transaction(BankId, ClientId, AccountId, TransactionId, Date, Period, StoreId, BudgetId, CategoriesIds, Amount, State0),
  {reply, R, State0};
handle_call({split_transaction, BankId, AccountId, ClientId, TransactionId}, _From, #state{ } = State0) ->
  R = do_split_transaction(BankId, AccountId, ClientId, TransactionId, State0),
  {reply, R, State0};
handle_call({aggregate_amounts_for_purse, StartDate, UntilDate, PurseId, SourcesList}, _From, #state{ } = State0) ->
  R = do_aggregate_amounts_for_purse(StartDate, UntilDate, PurseId, SourcesList, State0),
  {reply, R, State0};
handle_call({get_new_transactions_for_purse, StartDate, UntilDate, PurseId, SourcesList}, _From, #state{ } = State0) ->
  R = do_get_new_transactions_for_purse(StartDate, UntilDate, PurseId, SourcesList, State0),
  {reply, R, State0};
handle_call({copy_withdrawal_transaction_to_purse, BankId, ClientId, AccountId, TransactionId, PurseId}, _From, #state{ } = State0) ->
  R = do_copy_withdrawal_transaction_to_purse(BankId, ClientId, AccountId, TransactionId, PurseId, State0),
  {reply, R, State0};

handle_call(get_mappings, _From, #state{ } = State0) ->
  R = do_get_mappings(State0),
  {reply, R, State0};
handle_call(apply_mappings, _From, #state{ } = State0) ->
  R = do_apply_mappings(State0),
  {reply, R, State0};
handle_call({upgrade_mappings, Budgets, Categories, Stores, Mappings}, _From, #state{ } = State0) ->
  R = do_upgrade_mappings(Budgets, Categories, Stores, Mappings, State0),
  {reply, R, State0};
handle_call({insert_mapping, Pattern, BudgetId, CategoriesIds, StoreId, FixDate, Period}, _From, #state{ } = State0) ->
  R = do_insert_mapping_with_values(Pattern, BudgetId, CategoriesIds, StoreId, FixDate, Period, State0),
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
      List1 = [ #{ id => Id, name => Name } || {Id, Name} <- List0 ],
      {value, List1}
  end.

%%
%% @doc Get budgets from database
%%
-spec do_get_budgets(#state{}) -> {value, [banks_fetch_bank:budget()]}.
do_get_budgets(#state{ connection = Connection }) ->
  case pgsql_connection:simple_query(<<"SELECT id, name FROM budgets;">>, Connection) of
    {{select, _N}, List0} ->
      List1 = [ #{ id => Id, name => Name } || {Id, Name} <- List0 ],
      {value, List1}
  end.

-spec do_insert_budget(banks_fetch_bank:budget(), #state{}) -> ok.
do_insert_budget(#{ id := Id, name := Name }, #state{ connection = Connection }) ->
  case pgsql_connection:extended_query(<<"INSERT INTO budgets(id, name) VALUES($1,$2);">>, [Id, Name], Connection) of
    {{insert,_,1},[]} ->
      ok
  end.

-spec do_delete_budgets([non_neg_integer()], #state{}) -> ok.
do_delete_budgets([], #state{}) ->
  ok;
do_delete_budgets(IdList, #state{ connection = Connection }) ->
  case pgsql_connection:simple_query(list_to_binary([<<"DELETE FROM budgets where id IN (">>, id_list_to_string(IdList), <<");">>]), Connection) of
    {{delete,_N},[]} ->
      ok
  end.


%%
%% @doc Get categories from database
%%
-spec do_get_categories(#state{}) -> {value, [banks_fetch_bank:category()]}.
do_get_categories(#state{ connection = Connection }) ->
  % WARNING: it is not enough to ensure that up category is defined before a category
  case pgsql_connection:simple_query(<<"SELECT id, name, up_category_id FROM categories ORDER BY up_category_id NULLS FIRST;">>, Connection) of
    {{select, _N}, List0} ->
      List1 = [ #{ id => Id, name => Name, up_category_id => if is_integer(CategoryUpId) -> CategoryUpId; true -> none end } || {Id, Name, CategoryUpId} <- List0 ],
      {value, List1}
  end.

%%
%% @doc Insert new category
%%
do_insert_category(#{ id := Id, name := Name, up_category_id := UpId }, #state{ connection = Connection }) ->
  case pgsql_connection:extended_query(<<"INSERT INTO categories(id, name, up_category_id) VALUES($1,$2,$3);">>, [Id, Name, none_to_null(UpId)], Connection) of
    {{insert,_,1},[]} ->
      ok
  end.

%%
%% @doc Delete categories from id list
%%
-spec do_delete_categories([non_neg_integer()], #state{}) -> ok.
do_delete_categories([], #state{}) ->
  ok;
do_delete_categories(IdList, #state{ connection = Connection }) ->
  case pgsql_connection:simple_query(list_to_binary([<<"DELETE FROM categories where id IN (">>, id_list_to_string(IdList), <<");">>]), Connection) of
    {{delete,_N},[]} ->
      ok
  end.


%%
%% @doc Get stores from database
%%
-spec do_get_stores(#state{}) -> {value, [banks_fetch_bank:store()]}.
do_get_stores(#state{ connection = Connection }) ->
  case pgsql_connection:simple_query(<<"SELECT id, name FROM stores;">>, Connection) of
    {{select, _N}, List0} ->
      List1 = [ #{ id => Id, name => Name } || {Id, Name} <- List0 ],
      {value, List1}
  end.

%%
%% @doc Insert new store
%%
do_insert_store(#{ id := Id, name := Name }, #state{ connection = Connection }) ->
  case pgsql_connection:extended_query(<<"INSERT INTO stores(id, name) VALUES($1,$2);">>, [Id, Name], Connection) of
    {{insert,_,1},[]} ->
      ok
  end.

-spec do_insert_store_with_name(unicode:unicode_binary(), #state{}) -> {ok, banks_fetch_bank:store()} | {error, already_inserted}.
do_insert_store_with_name(Name, #state{ connection = Connection }) ->
  case pgsql_connection:extended_query(<<"INSERT INTO stores(name) VALUES($1) RETURNING id;">>, [Name], Connection) of
    {{insert,_,1},[{Id}]} ->
      {ok, #{ id => Id, name => Name }};
    {error, Error} ->
      true = pgsql_error:is_integrity_constraint_violation(Error),
      {error, already_inserted}
  end.


%%
%% @doc Delete stores from id list
%%
-spec do_delete_stores([non_neg_integer()], #state{}) -> ok.
do_delete_stores([], #state{}) ->
  ok;
do_delete_stores(IdList, #state{ connection = Connection }) ->
  case pgsql_connection:simple_query(list_to_binary([<<"DELETE FROM stores where id IN (">>, id_list_to_string(IdList), <<");">>]), Connection) of
    {{delete,_N},[]} ->
      ok
  end.

%%
%% @doc Insert new mapping
%%
-spec do_insert_mapping(banks_fetch_bank:mapping(), #state{}) -> ok.
do_insert_mapping(#{ id := Id, pattern := Pattern, fix_date := FixDate, period := Period, budget_id := BudgetId, categories_ids := CategoriesIds, store_id := StoreId }, #state{ connection = Connection }) ->
  case pgsql_connection:extended_query(<<"INSERT INTO mappings(id, pattern, fix_date, period, budget_id, categories_ids, store_id) VALUES($1,$2,$3,$4,$5,$6,$7);">>,
                                       [Id, Pattern, convert_to_sql_fix_date(FixDate), convert_to_sql_period(Period), none_to_null(BudgetId), convert_to_sql_categories_ids(CategoriesIds), none_to_null(StoreId)], Connection) of
    {{insert,_,1},[]} ->
      ok
  end.

-spec do_insert_mapping_with_values(unicode:unicode_binary(), none | non_neg_integer(), none | [non_neg_integer()], none | non_neg_integer(),
                                    banks_fetch_bank:mapping_fix_date(), banks_fetch_bank:mapping_period(), #state{}) -> {ok, banks_fetch_bank:mapping()} | {error, already_inserted}.
do_insert_mapping_with_values(Pattern, BudgetId, CategoriesIds, StoreId, FixDate, Period, #state{ connection = Connection }) ->
  case pgsql_connection:extended_query(<<"INSERT INTO mappings(pattern, fix_date, period, budget_id, categories_ids, store_id) VALUES($1,$2,$3,$4,$5,$6) RETURNING id;">>,
                                       [Pattern, convert_to_sql_fix_date(FixDate), convert_to_sql_period(Period), none_to_null(BudgetId), convert_to_sql_categories_ids(CategoriesIds), none_to_null(StoreId)], Connection) of
    {{insert,_,1},[{Id}]} ->
      {ok, #{ id => Id, pattern => Pattern, budget_id => BudgetId, categories_ids => CategoriesIds, store_id => StoreId, fix_date => FixDate, period => Period }};
    {error, Error} ->
      true = pgsql_error:is_integrity_constraint_violation(Error),
      {error, already_inserted}
  end.


%%
%% @doc Delete mappings from id list
%%
-spec do_delete_mappings([non_neg_integer()], #state{}) -> ok.
do_delete_mappings([], #state{}) ->
  ok;
do_delete_mappings(IdList, #state{ connection = Connection }) ->
  case pgsql_connection:simple_query(list_to_binary([<<"DELETE FROM mappings where id IN (">>, id_list_to_string(IdList), <<");">>]), Connection) of
    {{delete,_N},[]} ->
      ok
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
%% @doc Get all accounts
%%
-spec do_get_all_accounts(#state{}) -> {value, [banks_fetch_bank:bank()]}.
do_get_all_accounts(#state{ connection = Connection }) ->
  case pgsql_connection:simple_query(<<"SELECT distinct on (bank_id, client_id, account_id) bank_id, client_id, account_id, balance, number, owner, ownership, type, name FROM accounts ORDER BY bank_id, client_id, account_id, fetching_at DESC;">>, Connection) of
    {{select, _N}, AccountsSQL} ->
      {value, [ account_sql_to_map(AccountSQL) || AccountSQL <- AccountsSQL]}
  end.

%%
%% @doc Get accounts
%%
-spec do_get_accounts(banks_fetch_bank:bank_id(), banks_fetch_bank:client_id(), #state{}) -> {value, [banks_fetch_bank:account()]}.
do_get_accounts(BankId, ClientId, #state{ connection = Connection }) ->
  {{select, _Nbr}, Accounts} = pgsql_connection:extended_query(<<"SELECT distinct on (bank_id, client_id, account_id) bank_id, client_id, account_id, balance, number, owner, ownership, type, name FROM accounts WHERE bank_id = $1 and client_id = $2 ORDER BY bank_id, client_id, account_id, fetching_at DESC">>, [BankId, ClientId], Connection),
  {value, [ account_sql_to_map(AccountSQL) || AccountSQL <- Accounts ]}.

-spec account_sql_to_map(tuple()) -> banks_fetch_bank:account().
account_sql_to_map({BankId, ClientId, AccountId, Balance, Number, Owner, {e_account_ownership, Ownership}, {e_account_type, Type}, Name}) ->
  #{ id => AccountId, bank_id => BankId, client_id => ClientId, balance => Balance, number => Number, owner => Owner, ownership => binary_to_atom(Ownership), type => binary_to_atom(Type), name => Name }.

%%
%% @doc Get account balances history (in reverse order)
%%
-spec do_get_account_balance_history(banks_fetch_bank:bank_id(), banks_fetch_bank:client_id(), banks_fetch_bank:account_id(), non_neg_integer(), #state{}) -> {value, [{calendar:datetime(), float()}]}.
do_get_account_balance_history(BankId, ClientId, AccountId, Nbr, #state{ connection = Connection }) ->
  {{select, _Nbr}, History} = pgsql_connection:extended_query(<<"SELECT fetching_at, balance FROM accounts WHERE bank_id = $1 AND client_id = $2 AND account_id = $3 ORDER BY bank_id, client_id, fetching_at DESC LIMIT $4">>, [BankId, ClientId, AccountId, Nbr], Connection),
  {value, History}.


%%
%% @doc Store transactions
%%
-spec do_store_transactions(banks_fetch_bank:bank_id(), banks_fetch_bank:client_id(), banks_fetch_bank:account_id(), calendar:datetime(), [banks_fetch_bank:transaction()], #state{}) -> ok.
do_store_transactions({bank_id, BankIdValue}, {client_id, ClientIdValue}, {account_id, AccountIdValue}, FetchingAt, TransactionsList, #state{ connection = Connection }) ->
  {'begin', []} = pgsql_connection:simple_query(<<"BEGIN TRANSACTION">>, Connection),
  lists:foldl(fun(#{ id := TransactionId, accounting_date := AccountingDate, effective_date := EffectiveDate, amount := Amount, description := Description, type := Type } = Transaction, Position) ->
                  ExtSplitOfId = case maps:get('ext_split_of_id', Transaction, undefined) of
                                   undefined -> null;
                                   {transaction_id, SplitOfId} -> SplitOfId
                                 end,
                  ExtSplitted = maps:get('ext_splitted', Transaction, false),
                    {{insert, _, 1}, []} = pgsql_connection:extended_query(<<"INSERT INTO transactions(bank_id, client_id, account_id, fetching_at, fetching_position, transaction_id, accounting_date, effective_date, amount, description, type, ext_split_of_id, ext_splitted) VALUES($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13);">>, [BankIdValue, ClientIdValue, AccountIdValue, FetchingAt, Position, TransactionId, AccountingDate, EffectiveDate, Amount, Description, atom_to_binary(Type,'utf8'), ExtSplitOfId, ExtSplitted], Connection),
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
-spec do_get_last_transactions(none | unicode:unicode_binary(), non_neg_integer(), #state{}) -> {value, {none | unicode:unicode_binary(), non_neg_integer(), [banks_fetch_bank:transaction()]}} | {error, invalid_cursor}.
do_get_last_transactions(Cursor, N, #state{ connection = Connection }) ->
  case decode_cursor(Cursor, Connection) of
    {error, _} = Err ->
      Err;
    none ->
      {value, {none, 0, []}};
    {WhereClause, StartId, Offset, Total} ->
      case pgsql_connection:extended_query(list_to_binary([<<"SELECT transaction_id, bank_id, client_id, account_id, accounting_date, effective_date, amount, description, type, ext_mapping_id, ext_date, ext_period, ext_budget_id, ext_categories_ids, ext_store_id, ext_splitted, ext_split_of_id FROM transactions WHERE ">>, WhereClause, <<" ORDER BY effective_date DESC, bank_id, client_id, account_id, fetching_at DESC, fetching_position ASC, ext_split_of_id NULLS FIRST, transaction_id OFFSET $1 LIMIT $2;">>]), [Offset, N, StartId], Connection) of
        {{select, Count}, List0} ->
          List1 = [ #{ id => TransactionId, accounting_date => AccountingDate, effective_date => EffectiveDate, amount => Amount, description => Description, type => binary_to_atom(Type,'utf8'),
                       bank_id => {bank_id, BankIdVal}, client_id => {client_id, ClientIdVal}, account_id => {account_id, AccountIdVal},
                       ext_mapping_id => null_to_undefined(MappingId), ext_date => null_to_undefined(Date), ext_period => convert_from_sql_period(OptPeriod, undefined), ext_budget_id => null_to_undefined(BudgetId),
                       ext_categories_ids => case CategoriesIds of {array, A} -> A; _ -> undefined end, ext_store_id => null_to_undefined(StoreId),
                       ext_splitted => Splitted, ext_split_of_id => convert_from_sql_transaction_id(SplitOfId) } ||
                    {TransactionId, BankIdVal, ClientIdVal, AccountIdVal, AccountingDate, EffectiveDate, Amount, Description, {e_transaction_type, Type}, MappingId, Date, OptPeriod, BudgetId, CategoriesIds, StoreId, Splitted, SplitOfId} <- List0 ],
          NewOffset = Offset+Count,
          NewCursor = if NewOffset >= Total -> none;
                         true -> base64:encode(list_to_binary([integer_to_binary(StartId), <<":">>, integer_to_binary(Offset+Count), <<":">>, integer_to_binary(Total)]))
                      end,
          {value, {NewCursor, Total, List1}}
      end
  end.

decode_cursor(none, Connection) ->
  case pgsql_connection:simple_query(<<"SELECT max(id), count(*) FROM transactions">>, Connection) of
    {{select, 1}, [{null, 0}]} -> none;
    {{select, 1}, [{MaxId, Count0}]} -> {<<"id <= $3">>, MaxId, 0, Count0}
  end;
decode_cursor(Cursor, _Connection) ->
  try binary:split(base64:decode(Cursor),<<":">>, [global]) of
    [StartIdBin, OffsetBin, CountBin] -> {<<"id < $3">>, binary_to_integer(StartIdBin), binary_to_integer(OffsetBin), binary_to_integer(CountBin)}
  catch
    error:function_clause -> {error, invalid_cursor}
  end.

null_to_undefined(null) -> undefined;
null_to_undefined(V) -> V.

undefined_to_null(undefined) -> null;
undefined_to_null(V) -> V.

do_update_transaction({bank_id, BankIdVal}, {client_id, ClientIdVal}, {account_id, AccountIdVal}, {transaction_id, TransactionIdVal}, Date, Period, StoreId, BudgetId, CategoriesIds, NewAmount, #state{ connection = Connection }) ->
  {'begin', []} = pgsql_connection:simple_query(<<"BEGIN TRANSACTION">>, Connection),

  Query = <<"UPDATE transactions SET ext_mapping_id = -1, ext_date = $5, ext_period = $6, ext_store_id = $7, ext_budget_id = $8, ext_categories_ids = $9 WHERE bank_id = $1 and client_id = $2 and account_id = $3 and transaction_id = $4 RETURNING accounting_date, effective_date, amount, description, type, ext_date, ext_period, ext_budget_id, ext_store_id, ext_categories_ids, ext_splitted, ext_split_of_id, amount;">>,
  Parameters = [BankIdVal, ClientIdVal, AccountIdVal, TransactionIdVal,
                undefined_to_null(Date), convert_to_sql_period(Period), undefined_to_null(StoreId), undefined_to_null(BudgetId), convert_to_sql_categories_ids(CategoriesIds)
               ],
  case pgsql_connection:extended_query(Query, Parameters, Connection) of
    {{update, 1}, [{AccountingDate, EffectiveDate, Amount, Description, {e_transaction_type, Type}, ExtDate, ExtPeriod, ExtBudgetId, ExtStoreId, ExtCategoriesIds, ExtSplitted, ExtSplitOfId0, OldAmount}]} ->
      ExtSplitOfId = convert_from_sql_transaction_id(ExtSplitOfId0),
      Transaction = #{ id => TransactionIdVal, accounting_date => AccountingDate, effective_date => EffectiveDate, amount => Amount, description => Description, type => binary_to_atom(Type,'utf8'),
                       bank_id => {bank_id, BankIdVal}, client_id => {client_id, ClientIdVal}, account_id => {account_id, AccountIdVal}, ext_mapping_id => -1,
                       ext_date => null_to_undefined(ExtDate), ext_period => convert_from_sql_period(ExtPeriod, 'undefined'), ext_budget_id => null_to_undefined(ExtBudgetId), ext_store_id => null_to_undefined(ExtStoreId),
                       ext_categories_ids => convert_from_sql_categories_ids(ExtCategoriesIds, 'undefined'), ext_splitted => ExtSplitted, ext_split_of_id => ExtSplitOfId },
      IsRemaining = re:run(TransactionIdVal, <<"-REM$">>, [{capture,none}]) =:= match,
      if NewAmount =:= undefined ->
           {'commit', []} = pgsql_connection:simple_query(<<"COMMIT">>, Connection),
           {ok, Transaction};
         ExtSplitOfId =:= none ->
           {'rollback', []} = pgsql_connection:simple_query(<<"ROLLBACK">>, Connection),
           {error, not_subtransaction};
         IsRemaining ->
           {'rollback', []} = pgsql_connection:simple_query(<<"ROLLBACK">>, Connection),
           {error, remaining_subtransaction};
         true ->
           % We can update amount only for subtransactions, except the last one whom amount is recomputed according to all subtransactions amount to match main transaction amount
          case pgsql_connection:extended_query(<<"SELECT transaction_id, amount FROM transactions WHERE bank_id = $1 AND client_id = $2 AND account_id = $3 AND ext_split_of_id is not null AND transaction_id = regexp_replace($4, '-[0-9]{3}','-REM') LIMIT 1">>, [BankIdVal, ClientIdVal, AccountIdVal, TransactionIdVal], Connection) of
            {{select, 1}, [{RemainingTransactionIdVal, RemainingAmount}]} ->
              NewRemainingAmount = RemainingAmount - (NewAmount - OldAmount),
              {{update,1},[]} = pgsql_connection:extended_query(<<"UPDATE transactions SET amount = $5 WHERE bank_id = $1 AND client_id = $2 AND account_id = $3 AND transaction_id = $4">>, [BankIdVal, ClientIdVal, AccountIdVal, RemainingTransactionIdVal, NewRemainingAmount], Connection),
              {{update,1},[]} = pgsql_connection:extended_query(<<"UPDATE transactions SET amount = $5 WHERE bank_id = $1 AND client_id = $2 AND account_id = $3 AND transaction_id = $4">>, [BankIdVal, ClientIdVal, AccountIdVal, TransactionIdVal, NewAmount], Connection),
              {'commit', []} = pgsql_connection:simple_query(<<"COMMIT">>, Connection),
              Transaction2 = maps:put(amount, NewAmount, Transaction),
              {ok, Transaction2}
          end
      end
  end.

% @doc If transaction does not have any sub-transactions yet, add 2 sub-transactions with 0.0 and transaction amount as amounts.
% If transaction already has subtransaction, add 1 sub-transaction with 0.0 as amount before the last sub-transaction.
% Last sub-transaction amount can not be modified directly. It is always the remaining amount after deducing all sub-transactions amount from main transaction.
% @end
do_split_transaction({bank_id, BankIdValue} = BankId, {client_id, ClientIdValue} = ClientId, {account_id, AccountIdValue} = AccountId, {transaction_id, TransactionIdValue} = TransactionId, #state{ connection = Connection } = State0) ->
  case pgsql_connection:extended_query(<<"SELECT transaction_id, fetching_at, accounting_date, effective_date, amount, type, ext_mapping_id, ext_date, ext_period, ext_budget_id, ext_categories_ids, ext_store_id FROM transactions WHERE bank_id = $1 and client_id = $2 and account_id = $3 and (transaction_id = $4 or ext_split_of_id = $4) ORDER BY ext_split_of_id NULLS FIRST, transaction_id ASC;">>, [BankIdValue, ClientIdValue, AccountIdValue, TransactionIdValue], Connection) of
    {{select, 0}, []} ->
      {error, not_found};
    % No subtransactions yet
    {{select, 1}, [{TransactionIdValue, FetchingAt, AccountingDate, EffectiveDate, Amount, {e_transaction_type, Type}, ExtMappingId, ExtDate, ExtPeriod, ExtBudgetId, ExtCategoriesIds, ExtStoreId}]} ->
      case pgsql_connection:extended_query(<<"UPDATE transactions SET ext_splitted = true WHERE bank_id = $1 and client_id = $2 and account_id = $3 and transaction_id = $4;">>,
                                           [BankIdValue, ClientIdValue, AccountIdValue, TransactionIdValue], Connection) of
        {{update, 1}, _} ->
          First = <<"-001">>,
          SubTransaction1 = #{ id => <<TransactionIdValue/binary, First/binary>>,
                               bank_id => BankId,
                               account_id => AccountId,
                               client_id => ClientId,
                               accounting_date => AccountingDate,
                               effective_date => EffectiveDate,
                               amount => 0.0,
                               description => <<"no description">>,
                               type => binary_to_atom(Type,'utf8'),
                               ext_mapping_id => null_to_undefined(ExtMappingId),
                               ext_date => null_to_undefined(ExtDate),
                               ext_period => convert_from_sql_period(ExtPeriod, 'undefined'),
                               ext_budget_id => null_to_undefined(ExtBudgetId),
                               ext_categories_ids => convert_from_sql_categories_ids(ExtCategoriesIds, undefined),
                               ext_store_id => null_to_undefined(ExtStoreId),
                               ext_split_of_id => TransactionId,
                               ext_splitted => false
                             },
          Rem = <<"-REM">>,
          SubTransactionRem  = #{ id => <<TransactionIdValue/binary, Rem/binary>>,
                                  bank_id => BankId,
                                  account_id => AccountId,
                                  client_id => ClientId,
                                  accounting_date => AccountingDate,
                                  effective_date => EffectiveDate,
                                  amount => Amount,
                                  description => <<"no description">>,
                                  type => binary_to_atom(Type,'utf8'),
                                  ext_mapping_id => null_to_undefined(ExtMappingId),
                                  ext_date => null_to_undefined(ExtDate),
                                  ext_period => convert_from_sql_period(ExtPeriod, 'undefined'),
                                  ext_budget_id => null_to_undefined(ExtBudgetId),
                                  ext_categories_ids => convert_from_sql_categories_ids(ExtCategoriesIds, undefined),
                                  ext_store_id => null_to_undefined(ExtStoreId),
                                  ext_split_of_id => TransactionId,
                                  ext_splitted => false
                                },
          TransactionsList = [SubTransaction1, SubTransactionRem],
          case do_store_transactions(BankId, ClientId, AccountId, FetchingAt, TransactionsList, State0) of
            ok -> {ok, TransactionsList}
          end
      end;
    % Has already subtransactions
    {{select, _Total}, [MainTransaction|SubTransactions]} ->
      {TransactionIdValue, FetchingAt, AccountingDate, EffectiveDate, _Amount, {e_transaction_type, Type}, ExtMappingId, ExtDate, ExtPeriod, ExtBudgetId, ExtCategoriesIds, ExtStoreId} = MainTransaction,
      [_RemainingSubTransaction, PreviousSubTransaction | _OtherSubTransactions] = lists:reverse(SubTransactions),

      PreviousSubTransactionId = element(1,PreviousSubTransaction),
      PreviousSubTransactionIdCount = case re:run(PreviousSubTransactionId, <<".*-([0-9]{3})">>, [{capture, all_but_first, list}]) of
                                        {match, [PreviousCountStr]} -> list_to_integer(PreviousCountStr)
                                      end,
      NewSubTransactionId = iolist_to_binary(io_lib:format("~s-~3.10.0B", [TransactionIdValue, PreviousSubTransactionIdCount+1])),
      NewSubTransaction = #{ id => NewSubTransactionId,
                             bank_id => BankId,
                             account_id => AccountId,
                             client_id => ClientId,
                             accounting_date => AccountingDate,
                             effective_date => EffectiveDate,
                             amount => 0.0,
                             description => <<"no description">>,
                             type => binary_to_atom(Type,'utf8'),
                             ext_mapping_id => null_to_undefined(ExtMappingId),
                             ext_date => null_to_undefined(ExtDate),
                             ext_period => convert_from_sql_period(ExtPeriod, 'undefined'),
                             ext_budget_id => null_to_undefined(ExtBudgetId),
                             ext_categories_ids => convert_from_sql_categories_ids(ExtCategoriesIds, undefined),
                             ext_store_id => null_to_undefined(ExtStoreId),
                             ext_split_of_id => TransactionId,
                             ext_splitted => false
                           },
      TransactionsList = [NewSubTransaction],
      case do_store_transactions(BankId, ClientId, AccountId, FetchingAt, TransactionsList, State0) of
        ok -> {ok, [NewSubTransaction]}
      end
  end.

do_get_new_transactions_for_purse(StartDate, UntilDate, PurseId, SourcesList, #state{ connection = Connection }) ->
  List0 = do_get_new_transactions_for_purse_aux(StartDate, UntilDate, PurseId, SourcesList, Connection, []),
  List1 = [ #{ id => TransactionId, accounting_date => AccountingDate, effective_date => EffectiveDate, amount => Amount, description => Description, type => binary_to_atom(Type,'utf8'),
               bank_id => {bank_id, BankIdVal}, client_id => {client_id, ClientIdVal}, account_id => {account_id, AccountIdVal} } ||
            {TransactionId, BankIdVal, ClientIdVal, AccountIdVal, AccountingDate, EffectiveDate, Amount, Description, {e_transaction_type, Type}} <- List0 ],
  {value, List1}.

do_get_new_transactions_for_purse_aux(_, _, _, [], _, Acc) ->
  Acc;
do_get_new_transactions_for_purse_aux(StartDate, UntilDate, {client_id, TargetClientId} = PurseId, [{{bank_id, SourceBankId}, {client_id, SourceClientId}, {account_id, SourceAccountId}}|Next], Connection, Acc) ->
  case pgsql_connection:extended_query(<<"SELECT 'purse-' || source.bank_id || '-' || source.client_id || '-' || source.account_id || '-' || source.transaction_id, 'purse', $4, 'purse', accounting_date, effective_date, - amount, description, type FROM transactions source where bank_id = $1 AND client_id = $2 AND account_id = $3 AND description ~ '^RETRAIT DAB' AND effective_date >= $6 AND NOT exists(SELECT 1 from transactions target WHERE target.bank_id = 'purse' AND target.client_id = $4 AND target.account_id = 'purse' AND target.transaction_id = 'purse-' || source.bank_id || '-' || source.client_id || '-' || source.account_id || '-' || source.transaction_id LIMIT 1) AND fetching_at < $5">>, [SourceBankId, SourceClientId, SourceAccountId, TargetClientId, UntilDate, StartDate], Connection) of
    {{select, _N}, List0} ->
      do_get_new_transactions_for_purse_aux(StartDate, UntilDate, PurseId, Next, Connection, List0++Acc)
  end.

do_aggregate_amounts_for_purse(StartDate, UntilDate, {client_id, TargetClientId} = PurseId, SourcesList, #state{ connection = Connection }) ->
  % Sum all amounts in purse
  case pgsql_connection:extended_query(<<"SELECT coalesce(sum(amount), 0.0) FROM transactions source where bank_id = 'purse' AND client_id = $1 AND account_id = 'purse' AND effective_date >= $2 AND fetching_at < $3">>, [TargetClientId, StartDate, UntilDate], Connection) of
    {{select, 1}, [{SumAmounts}]} ->
      Amount = do_aggregate_amounts_for_purse_aux(StartDate, UntilDate, PurseId, SourcesList, Connection, SumAmounts),
      {value, Amount}
  end.

do_aggregate_amounts_for_purse_aux(_, _, _, [], _, Acc) ->
  Acc;
do_aggregate_amounts_for_purse_aux(StartDate, UntilDate, {client_id, TargetClientId} = PurseId, [{{bank_id, SourceBankId}, {client_id, SourceClientId}, {account_id, SourceAccountId}}|Next], Connection, Acc) ->
  % Sum all amounts of transactions which will be inserted in purse
  case pgsql_connection:extended_query(<<"SELECT - coalesce(sum(amount), 0.0) FROM transactions source where bank_id = $1 AND client_id = $2 AND account_id = $3 AND description ~ '^RETRAIT DAB' AND effective_date >= $6 AND NOT exists(SELECT 1 from transactions target WHERE target.bank_id = 'purse' AND target.client_id = $4 AND target.account_id = 'purse' AND target.transaction_id = 'purse-' || source.bank_id || '-' || source.client_id || '-' || source.account_id || '-' || source.transaction_id LIMIT 1) AND fetching_at < $5">>, [SourceBankId, SourceClientId, SourceAccountId, TargetClientId, UntilDate, StartDate], Connection) of
    {{select, 1}, [{SumAmounts}]} ->
      do_aggregate_amounts_for_purse_aux(StartDate, UntilDate, PurseId, Next, Connection, Acc+SumAmounts)
  end.


do_copy_withdrawal_transaction_to_purse(BankId, ClientId, AccountId, TransactionId, PurseId, #state{ connection = Connection } = State) ->
  case pgsql_connection:extended_query(<<"INSERT INTO transactions(bank_id, client_id, account_id, fetching_at, transaction_id, accounting_date, effective_date, amount, description, type, fetching_position, ext_date) SELECT 'purse', $1, 'purse', NOW(), 'purse-' || bank_id || '-' || client_id || '-' || account_id || '-' || transaction_id, accounting_date, effective_date, -amount, description, type, fetching_position, ext_date FROM transactions WHERE bank_id = $2 AND client_id = $3 AND account_id = $4 AND transaction_id = $5 RETURNING amount, ext_date">>, [PurseId, BankId, ClientId, AccountId, TransactionId], Connection) of
    {{insert, 0, 1}, [{Amount, ExtDate}]} ->
      do_recompute_purse_account_values(PurseId, Amount, ExtDate, State);
    {error, Error} ->
      true = pgsql_error:is_integrity_constraint_violation(Error),
      {error, already_copied}
  end.

do_recompute_purse_account_values(PurseId, TransactionAmount, TransactionDate, #state{ connection = Connection }) ->
  error_logger:info_msg("Amount = ~p, Date = ~p", [TransactionAmount, TransactionDate]),

  case pgsql_connection:extended_query(<<"UPDATE accounts SET balance = balance + $1 WHERE bank_id = 'purse' AND client_id = $2 AND account_id = 'purse' AND fetching_at >= $3">>,
                                       [TransactionAmount, PurseId, TransactionDate], Connection) of
    {{update, _}, []} ->
      ok
  end.

%%
%% @doc Get all mappings
%%
-spec do_get_mappings(#state{}) -> {value, [any()]}.
do_get_mappings(#state{ connection = Connection }) ->
  case pgsql_connection:simple_query(<<"SELECT id, pattern, fix_date, period, budget_id, categories_ids, store_id FROM mappings;">>, Connection) of
    {{select, _N}, MappingsSQL} ->
      {value, [ #{ id => Id, pattern => Pattern, fix_date => convert_from_sql_fix_date(FixDate), period => convert_from_sql_period(Period, 'none'),
                   budget_id => null_to_none(BudgetId), categories_ids => convert_from_sql_categories_ids(CategoriesIds, 'none'), store_id => null_to_none(StoreId) } ||
                {Id, Pattern, FixDate, Period, BudgetId, CategoriesIds, StoreId} <- MappingsSQL ]}
  end.

do_apply_mappings(#state{ connection = Connection }) ->
  % It will trigger postgres function which analyses transactions
  ok = lager:info("Apply mappings"),
  {{'update', N}, []} = pgsql_connection:extended_query(<<"UPDATE transactions SET description = description">>, [], Connection),
  ok = lager:info("Number of transactions updated: ~p", [N]),
  ok.

do_upgrade_mappings(Budgets, Categories, Stores, Mappings, #state{ connection = Connection } = State) ->
  try
    {'begin', []} = pgsql_connection:extended_query(<<"BEGIN TRANSACTION">>, [], Connection),
    upgrade_entries(Budgets, fun do_get_budgets/1, fun do_insert_budget/2, fun do_delete_budgets/2, State),
    upgrade_entries(Categories, fun do_get_categories/1, fun do_insert_category/2, fun do_delete_categories/2, State),
    upgrade_entries(Stores, fun do_get_stores/1, fun do_insert_store/2, fun do_delete_stores/2, State),
    upgrade_entries(Mappings, fun do_get_mappings/1, fun do_insert_mapping/2, fun do_delete_mappings/2, State),
    {'commit', []} = pgsql_connection:extended_query(<<"COMMIT">>, [], Connection),
    do_apply_mappings(State)
  catch
    E:V:S ->
      ok = lager:warning("Unable to upgrade mappings : ~p\n~p", [{E,V}, S]),
      {'rollback', []} = pgsql_connection:extended_query(<<"ROLLBACK">>, [], Connection),
      {error, unable_to_upgrade_mappings}
  end.

upgrade_entries(EntriesUpgrade, LoadFun, InsertFun, DeleteFun, State) ->
  {value, EntriesStorage} = LoadFun(State),
  {NewEntries, RemovedEntriesId} = compare_json_storage(EntriesUpgrade, EntriesStorage),
  ok = DeleteFun(RemovedEntriesId, State),
  lists:foreach(fun(NewEntry) -> ok = InsertFun(NewEntry, State) end, NewEntries),
  ok.


compare_json_storage(DataUpgrade, DataStorage) ->
  DataUpgradeSorted = sort_by_id(DataUpgrade),
  DataStorageSorted = sort_by_id(DataStorage),
  compare_json_storage_aux(DataUpgradeSorted, DataStorageSorted, {[], []}).


compare_json_storage_aux([], [], Acc) ->
  % Finished
  Acc;
compare_json_storage_aux([Upgrade|NextUpgrade], [], {AccNew, AccRemove}) ->
  % Upgrade data remaining, add it
  compare_json_storage_aux(NextUpgrade, [], {[Upgrade|AccNew], AccRemove});

compare_json_storage_aux([], [#{ id := StorageId }|NextStorage], {AccNew, AccRemove}) ->
  if StorageId >= 1000000 -> % Keep custom entry
       compare_json_storage_aux([], NextStorage, {AccNew, AccRemove});
     true -> % Storage data remaining, remove it
       compare_json_storage_aux([], NextStorage, {AccNew, [StorageId|AccRemove]})
  end;

compare_json_storage_aux([Upgrade|NextUpgrade], [Storage|NextStorage], {AccNew, AccRemove}) when Upgrade =:= Storage ->
  % Same data, do nothing
  compare_json_storage_aux(NextUpgrade, NextStorage, {AccNew, AccRemove});

compare_json_storage_aux([#{ id := UpgradeId } = Upgrade|NextUpgrade], [#{ id := StorageId }|NextStorage], {AccNew, AccRemove}) when UpgradeId =:= StorageId ->
  % Same id but different content, remove old and add new
  compare_json_storage_aux(NextUpgrade, NextStorage, {[Upgrade|AccNew],[StorageId|AccRemove]});

compare_json_storage_aux([#{ id := UpgradeId } = Upgrade|NextUpgrade], [#{ id := StorageId }=Storage|NextStorage], {AccNew, AccRemove}) when UpgradeId < StorageId ->
  % Upgrade not found in storage, add it
  compare_json_storage_aux(NextUpgrade, [Storage|NextStorage], {[Upgrade|AccNew],AccRemove});

compare_json_storage_aux([#{ id := UpgradeId } = Upgrade|NextUpgrade], [#{ id := StorageId }|NextStorage], {AccNew, AccRemove}) when UpgradeId > StorageId ->
  % Storage not found in upgrade, remove it
  compare_json_storage_aux([Upgrade|NextUpgrade], NextStorage, {AccNew,[StorageId|AccRemove]}).

sort_by_id(MapsList) ->
  lists:sort(fun(#{ id := Id1 }, #{ id := Id2 }) -> Id1 < Id2 end, MapsList).

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

%% Functions to convert data from sql to erlang (and erlang to sql)

%% @doc Transform none to null
none_to_null(none) -> null;
none_to_null(Other) -> Other.

%% @doc Transform null to none
null_to_none(null) -> none;
null_to_none(O) -> O.

%% @doc List of integer to string
id_list_to_string([FirstId | IdList]) ->
  integer_to_list(FirstId) ++ [ ","++integer_to_list(Id) || Id <- IdList ].

convert_to_sql_fix_date(O) -> atom_to_binary(O, 'utf8').

convert_from_sql_fix_date({e_fix_date, F}) -> binary_to_atom(F, 'utf8').

convert_to_sql_categories_ids(none) -> null;
convert_to_sql_categories_ids(undefined) -> null;
convert_to_sql_categories_ids(L) -> {array, L}.

convert_from_sql_categories_ids(null, Default) -> Default;
convert_from_sql_categories_ids({array, List}, _Default) -> List.

convert_to_sql_period(undefined) -> null;
convert_to_sql_period(P) -> atom_to_binary(P, 'utf8').

convert_from_sql_period(null, Default) -> Default;
convert_from_sql_period({e_period, P}, _Default) -> binary_to_atom(P, 'utf8').

convert_from_sql_transaction_id(null) -> none;
convert_from_sql_transaction_id(TransactionId) -> {transaction_id, TransactionId}.
