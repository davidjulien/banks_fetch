%%
%% @doc This gen_server manages access to a bank account. It is started with a bank id, a client id and a client credential.
%% Every 4 hours, gen_server tries to connect to the bank with given credential to fetch accounts and to fetch new transactions for each account since last fetched transaction.
%% Accounts and transactions are the stored in database.
%% In case of error, it tries again to connect one hour later.
%% @end
-module(banks_fetch_client_server).
-behaviour(gen_server).

-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-export([
         accounts/1
        ]).

-record(state, {
          bank_id :: banks_fetch_bank:bank_id(),
          bank_module :: module(),
          client_id :: banks_fetch_bank:client_id(),
          client_credential :: banks_fetch_bank:client_credential(_),
          accounts :: [banks_fetch_bank:account()]
         }).


accounts(PID) ->
  gen_server:call(PID, accounts).


start_link({bank_id, BankIdValue} = BankId, {client_id, ClientIdValue} = ClientId, ClientCredential) ->
  gen_server:start_link({local, list_to_atom(binary_to_list(BankIdValue) ++ "_" ++ binary_to_list(ClientIdValue))}, ?MODULE, {BankId, ClientId, ClientCredential}, []).

init({BankId, ClientId, ClientCredential}) ->
  {bank_id, BankIdValue} = BankId,
  self() ! fetch_data,
  {ok, #state{
          bank_id = BankId,
          bank_module = list_to_atom("banks_fetch_bank_" ++ binary_to_list(BankIdValue)),
          client_id = ClientId,
          client_credential = ClientCredential,
          accounts = []}}.

handle_call(accounts, _From, #state{ accounts = Accounts} = State0) ->
  {reply, Accounts, State0}.

handle_cast(_, State0) ->
  {noreply, State0}.

handle_info(fetch_data, State0) ->
  State1 = do_fetch_data(State0),
  {noreply, State1}.

%%
%%
-spec do_fetch_data(#state{}) -> #state{}.
do_fetch_data(#state{ bank_module = BankModule, bank_id = BankId, client_id = ClientId, client_credential = ClientCredential } = State0) ->
  ok = lager:info("~p/~p/~p : fetch data", [BankModule, BankId, ClientId]),
  case banks_fetch_bank:connect(BankModule, ClientId, ClientCredential) of
    {error, _} = Err ->
      ok = lager:warning("~p/~p/~p : fetch data error : ~p", [BankModule, BankId, ClientId, Err]),
      case Err of
        {error, network_error} -> % In case of network errors, try again an hour later
          {ok, _} = timer:send_after(1*60*60*1000, fetch_data),
          State0;
        {error, _} -> % Do nothing for other cases
          State0
      end;
    {ok, Auth} ->
      FetchingAt = calendar:universal_time(),
      {ok, Accounts} =  banks_fetch_bank:fetch_accounts(BankModule, Auth),
      banks_fetch_storage:store_accounts(BankId, ClientId, FetchingAt, Accounts),

      % Fetch transactions for each account,
      % Get last transaction id for each account to stop fetching
      {value, LastTransactionsIdList} = banks_fetch_storage:get_last_transactions_id(BankId, ClientId),

      lists:foreach(fun(#{ id := AccountIdValue } = _Account) ->
                        AccountId = {account_id, AccountIdValue},
                        FirstCallOrLastFetchedTransactionId = case lists:keyfind(AccountId, 1, LastTransactionsIdList) of
                                                                false -> first_call;
                                                                {_, LastTransactionId} -> LastTransactionId
                                                              end,
                        {ok, Transactions} = banks_fetch_bank:fetch_transactions(BankModule, Auth, AccountId, FirstCallOrLastFetchedTransactionId),
                        ok = banks_fetch_storage:store_transactions(BankId, ClientId, AccountId, FetchingAt, Transactions)
                    end, Accounts),

      {ok, _} = timer:send_after(4*60*60*1000, fetch_data),

      ok = lager:info("~p/~p/~p : fetch data ok", [BankModule, BankId, ClientId]),
      State0#state{ accounts = Accounts }
  end.
