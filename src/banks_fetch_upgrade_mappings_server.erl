%%
%% This module upgrades budgets, categories, stores and mappings.
%% It fetches a resource file every 24h to update these tables.
%%
-module(banks_fetch_upgrade_mappings_server).
-behaviour(gen_server).
-include_lib("common_test/include/ct.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-export([
         last_fetch/1
        ]).

-define(MAPPINGS_FILE, "https://raw.githubusercontent.com/davidjulien/banks_fetch/master/resources/mappings.json").

-record(state, {
          last_fetch :: none | calendar:datetime()
         }).

-spec last_fetch(pid()) -> none | calendar:datetime().
last_fetch(PID) ->
  gen_server:call(PID, last_fetch).


start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, none, []).

init(none) ->
  banks_fetch_http:setup_monitoring("raw.githubusercontent.com"),
  self() ! fetch_mappings,
  {ok, #state{
          last_fetch = none
         }}.

handle_call(last_fetch, _From, #state{ last_fetch = LastFetch } = State0) ->
  {reply, LastFetch, State0}.

handle_cast(_, State0) ->
  {noreply, State0}.

handle_info(fetch_mappings, State0) ->
  State1 = do_fetch_mappings(State0),
  {noreply, State1}.

%%
%%
-spec do_fetch_mappings(#state{}) -> #state{}.
do_fetch_mappings(#state{} = State0) ->
  ok = lager:info("Fetch mappings file from github: ~s", [?MAPPINGS_FILE]),
  FetchingDatetime = calendar:universal_time(),
  case banks_fetch_http:request(get, {?MAPPINGS_FILE, []}, [], []) of
    {ok, {{_Version0, 200, _ReasonPhrase0}, _Headers0, Body}} ->
      JSON = jsx:decode(list_to_binary(Body)),
      Budgets = convert_from_json(<<"budgets">>, JSON),
      Categories = convert_from_json(<<"categories">>, JSON),
      Stores = convert_from_json(<<"stores">>, JSON),
      Mappings = convert_from_json(<<"mappings">>, JSON),
      ok = banks_fetch_storage:upgrade_mappings(Budgets, Categories, Stores, Mappings),

      % Fetch again in 24 hours
      {ok, _} = timer:send_after(24*60*60*1000, fetch_mappings),

      State0#state{ last_fetch = FetchingDatetime }
  end.

convert_from_json(Key, JSON) ->
  {_, List} = lists:keyfind(Key, 1, JSON),
  List2 = lists:foldl(fun(T, L) ->
                  % Convert each item to a map
                  R = lists:foldl(fun({K,V}, M) ->
                                      CV = case K of
                                             <<"period">> -> binary_to_atom(V, 'utf8');
                                             <<"fix_date">> -> binary_to_atom(V, 'utf8');
                                             <<"up_category_id">> when V =:= null -> none;
                                             _ -> V
                                           end,
                                      maps:put(binary_to_atom(K,'utf8'), CV, M)
                              end, maps:new(), T),
                  [R|L]
              end, [], List),
  lists:reverse(List2).
