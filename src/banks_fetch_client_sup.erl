%%%-------------------------------------------------------------------
%% @doc Supervisor for banks_fetch_client_server_sup and banks_fetch_client_manager
%% @end
%%%-------------------------------------------------------------------

-module(banks_fetch_client_sup).

-behaviour(supervisor).

%% API
-export([
         start_link/0
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  SupervisorSpec = #{
    strategy => one_for_all,
    intensity => 1,
    period => 60
   },
  ClientManagerSpec = #{
    id => banks_fetch_client_manager,
    start => {banks_fetch_client_manager, start_link, []},
    restart => permanent,
    shutdown => 1,
    type => worker,
    modules => [banks_fetch_client_manager]
   },
  ClientServerSupSpec = #{
    id => banks_fetch_client_server_sup,
    start => {banks_fetch_client_server_sup, start_link, []},
    restart => permanent,
    shutdown => 1,
    type => supervisor,
    modules => [banks_fetch_client_server_sup]
   },
  {ok, { SupervisorSpec, [ClientManagerSpec,ClientServerSupSpec]} }.

%%====================================================================
%% Internal functions
%%====================================================================
