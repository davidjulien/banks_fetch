%%%-------------------------------------------------------------------
%% @doc Supervisor for banks_fetch_client_server
%% @end
%%%-------------------------------------------------------------------

-module(banks_fetch_client_server_sup).

-behaviour(supervisor).

%% API
-export([
         start_link/0,
         start_child/3
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec start_child(banks_fetch_bank:bank_id(), banks_fetch_bank:client_id(), banks_fetch_bank:client_credential(any())) -> supervisor:startchild_ret().
start_child(BankId, ClientId, ClientCredential) ->
  supervisor:start_child(banks_fetch_client_server_sup, [BankId, ClientId, ClientCredential]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  SupervisorSpec = #{
    strategy => simple_one_for_one,
    intensity => 1,
    period => 60
   },
  ChildSpec = #{
    id => banks_fetch_client_server,
    start => {banks_fetch_client_server, start_link, []},
    restart => temporary, % don't restart a client server in case of crash in order to prevent account locking
    shutdown => 1,
    type => worker,
    modules => [banks_fetch_client_server]
   },
  {ok, { SupervisorSpec, [ChildSpec]} }.

%%====================================================================
%% Internal functions
%%====================================================================
