%%%-------------------------------------------------------------------
%% @doc banks_fetch top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(banks_fetch_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

% CREATE ROLE banks_fetch_user WITH LOGIN;
% CREATE DATABASE banks_fetch;
% GRANT ALL PRIVILEGES ON DATABASE banks_fetch TO banks_fetch_user;
% ALTER DATABASE banks_fetch OWNER TO banks_fetch_user;
-define(DATABASE_PARAMS, {"banks_fetch","banks_fetch_user",""}).

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
      start => {banks_fetch_storage, start_link, [?DATABASE_PARAMS]},
      restart => permanent,
      shutdown => 1,
      type => worker,
      modules => [banks_fetch_storage]
     },
    {ok, {SupFlags, [StorageSup, ClientSupSpec]}}.

%%====================================================================
%% Internal functions
%%====================================================================
