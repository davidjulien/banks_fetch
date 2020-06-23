-module(banks_fetch_client_manager).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-export([
         get_clients_pids/0
        ]).

-record(state, {
          clients_pids :: [{banks_fetch_bank:bank_id(), banks_fetch_bank:client_id(), pid()}]
         }).

get_clients_pids() ->
  gen_server:call(?MODULE, get_clients_pids).

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, none, []).

init(none) ->
  self() ! load_clients,
  {ok, #state{ clients_pids = [] } }.

handle_call(get_clients_pids, _From, #state{ clients_pids = ClientsPIDs } = State0) ->
  {reply, ClientsPIDs, State0}.

handle_cast(_, State0) ->
  {noreply, State0}.

handle_info(load_clients, State0) ->
  {value, Clients} = banks_fetch_storage:get_clients(),
  ClientsPIDs = lists:foldl(fun({BankId, ClientId, ClientCredential}, Acc) ->
                               {ok, PID} = banks_fetch_client_server_sup:start_child(BankId, ClientId, ClientCredential),
                               [{BankId, ClientId, PID}|Acc]
                           end, [], Clients),
  State1 = State0#state{ clients_pids = ClientsPIDs },
  {noreply, State1}.

%%
%%
