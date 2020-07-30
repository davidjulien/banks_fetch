-module(banks_fetch_client_manager).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-export([
         add_client/3,

         get_clients_pids/0
        ]).

-record(state, {
          clients_pids :: [{banks_fetch_bank:bank_id(), banks_fetch_bank:client_id(), pid()}]
         }).

-spec add_client(banks_fetch_bank:bank_id(), banks_fetch_bank:client_id(), banks_fetch_bank:client_credential(any())) -> ok | {error, already_defined}.
add_client(BankId, ClientId, ClientCredential) ->
  gen_server:call(?MODULE, {add_client, BankId, ClientId, ClientCredential}).

get_clients_pids() ->
  gen_server:call(?MODULE, get_clients_pids).

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, none, []).

init(none) ->
  process_flag(trap_exit, true), % because we want to receive a message when a client server is terminated to remove it from manager state
  self() ! load_clients,
  {ok, #state{ clients_pids = [] } }.

handle_call({add_client, BankId, ClientId, ClientCredential}, _From, #state{} = State0) ->
  {R, State1} = do_add_client(BankId, ClientId, ClientCredential, State0),
  {reply, R, State1};
handle_call(get_clients_pids, _From, #state{ clients_pids = ClientsPIDs } = State0) ->
  {reply, ClientsPIDs, State0}.

handle_cast(_, State0) ->
  {noreply, State0}.

handle_info(load_clients, State0) ->
  {value, Clients} = banks_fetch_storage:get_clients(),
  ClientsPIDs = lists:foldl(fun({BankId, ClientId, ClientCredential}, Acc) ->
                               {ok, PID} = banks_fetch_client_server_sup:start_child(BankId, ClientId, ClientCredential),
                               link(PID), % link process to receive exit signal for client servers
                               [{BankId, ClientId, PID}|Acc]
                           end, [], Clients),
  State1 = State0#state{ clients_pids = ClientsPIDs },
  {noreply, State1};

handle_info({'EXIT',ClientPid,_Reason}, State0) ->
  State1 = do_handle_exit(ClientPid, State0),
  {noreply, State1}.

%%
%% Internal functions
%%

-spec do_add_client(banks_fetch_bank:bank_id(), banks_fetch_bank:client_id(), banks_fetch_bank:client_credential(any()), #state{}) -> {ok|{error, already_defined}, #state{}}.
do_add_client({bank_id, BankIdValue} = BankId, {client_id, ClientIdValue} = ClientId, ClientCredential, #state{ clients_pids = ClientsPids0 } = State0) ->
  case banks_fetch_storage:insert_client(BankId, ClientId, ClientCredential) of
    {error, already_inserted} ->
      ok = lager:warning("Client ~p/~s already defined", [BankIdValue, ClientIdValue]),
      {{error, already_defined}, State0};
    ok ->
      {ok, PID} = banks_fetch_client_server_sup:start_child(BankId, ClientId, ClientCredential),
      State1 = State0#state{ clients_pids = [{BankId, ClientId, PID}|ClientsPids0] },
      {ok, State1}
  end.

%% @doc Process exit signal from client servers
-spec do_handle_exit(pid(), #state{}) -> #state{}.
do_handle_exit(ClientPid, #state{ clients_pids = ClientsPIDs0 } = State0) ->
  ClientsPIDs1 = lists:keydelete(ClientPid, 3, ClientsPIDs0),
  State0#state{ clients_pids = ClientsPIDs1 }.
