%%%-------------------------------------------------------------------
%% @doc banks_fetch top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(banks_fetch_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, DatabaseParameters} = application:get_env(banks_fetch, storage),
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 60},
    ClientSupSpec = #{
      id => banks_fetch_client_sup,
      start => {banks_fetch_client_sup, start_link, []},
      restart => permanent,
      shutdown => 1,
      type => supervisor,
      modules => [banks_fetch_client_sup]
     },
    StorageSup = #{
      id => banks_fetch_storage,
      start => {banks_fetch_storage, start_link, [DatabaseParameters]},
      restart => permanent,
      shutdown => 1,
      type => worker,
      modules => [banks_fetch_storage]
     },
    ApiParameters = [{callback, banks_fetch_api}, {port, 2020}],
    ApiSpec = #{
      id  => banks_fetch_api,
      start => {elli, start_link, [ApiParameters]},
      restart => permanent,
      type => worker,
      modules => [elli, banks_fetch_api]
     },
    {ok, {SupFlags, [StorageSup, ClientSupSpec, ApiSpec]}}.

%%====================================================================
%% Internal functions
%%====================================================================
