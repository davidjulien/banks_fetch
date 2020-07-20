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
  case banks_fetch_bank:connect(BankModule, ClientId, ClientCredential) of
    {error, _} ->
      State0;
    {ok, Auth} ->
      FetchingAt = calendar:universal_time(),
      {ok, Accounts} =  banks_fetch_bank:fetch_accounts(BankModule, Auth),
      banks_fetch_storage:store_accounts(BankId, ClientId, FetchingAt, Accounts),

      State0#state{ accounts = Accounts }
  end.
