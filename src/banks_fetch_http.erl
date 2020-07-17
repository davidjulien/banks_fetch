%%%-------------------------------------------------------------------
%% @doc Wrapper around httpc module. It allows to monitor http requests
%% and to switch to another http module if necessary.
%% @end
%%%-------------------------------------------------------------------

-module(banks_fetch_http).

-export([
         setup/0,
         setup_monitoring/1,
         set_options/1,
         request/4
        ]).

-type http_options() :: [http_option()].
-type http_option() :: {timeout, non_neg_integer()}.
-type options() :: [].
-type http_version() :: string().
-type status_code() :: integer().
-type reason_phrase() :: string().
-type status_line() :: {http_version(), status_code(), reason_phrase()}.
-type header() :: {string(), string()}.
-type headers() :: [header()].
-type url() :: string().
-type content_type() :: string().
-type body() :: string().
-type request() :: {url(), headers()} | {url(), headers(), content_type(), body()}.

% Status code having a special metric
-define(STATUS_CODE_MONITORED, [200,404]).

setup() ->
  setup_monitoring("all").

%% @doc This function setup monitoring metrics for a specific hostname
setup_monitoring(Host) ->
  MonitoringNamespace = convert_host(Host),

  % Declare http calls metrics
  _ = prometheus_counter:declare(
        [{name, namespace_to_prometheus_key(MonitoringNamespace, "requests_call_count")},
         {help, "Http requests on "++Host++" count"}]),
  _ = prometheus_counter:declare(
        [{name, namespace_to_prometheus_key(MonitoringNamespace, "requests_ok_count")},
         {help, "Http requests on "++Host++" returning ok count"}]),
  _ = prometheus_counter:declare(
        [{name, namespace_to_prometheus_key(MonitoringNamespace, "requests_error_count")},
         {help, "Http requests on "++Host++" returning error count"}]),

  % Declare http status metrics
  lists:foreach(fun(StatusCode) ->
                    _ = prometheus_counter:declare(
                          [{name, namespace_to_prometheus_key(MonitoringNamespace, "results_status_"++integer_to_list(StatusCode)++"_count")},
                           {help, "Http requests on "++Host++" returning status "++integer_to_list(StatusCode)++" count"}])
                end, ?STATUS_CODE_MONITORED),
  _ = prometheus_counter:declare(
        [{name, namespace_to_prometheus_key(MonitoringNamespace, "results_status_other_count")},
         {help, "Http requests on "++Host++" returning other status count"}]),

  ok.


-spec set_options([{cookies, enabled}]) -> ok | {error, Reason :: term()}.
set_options(Options) ->
  httpc:set_options(Options).

%% @doc HTTP request with monitoring. For each request, we increment number of requests globally and for request's domain. We also monitor
%% result status (ok, error, status code)
%% @end
-spec request(get | post, request(), http_options(), options()) -> {ok, {status_line(), headers(), Body :: string() | binary()}} | {error, Reason :: term()}.
request(Method, Request, HttpOptions, Options) ->
  MonitoringNamespace = monitoring_namespace(Request),

  prometheus_counter_inc("all", "requests_call_count"),
  prometheus_counter_inc(MonitoringNamespace, "requests_call_count"),

  R = httpc:request(Method, Request, HttpOptions, Options),

  case R of
    {ok, HttpResult} ->
      prometheus_counter_inc("all", "requests_ok_count"),
      prometheus_counter_inc(MonitoringNamespace, "requests_ok_count"),
      case HttpResult of
        {{_,StatusCode,_},_,_} ->
          case lists:member(StatusCode, ?STATUS_CODE_MONITORED) of
            true ->
              prometheus_counter_inc("all", "results_status_" ++ integer_to_list(StatusCode) ++ "_count"),
              prometheus_counter_inc(MonitoringNamespace, "results_status_" ++ integer_to_list(StatusCode) ++ "_count");
            false ->
              prometheus_counter_inc("all", "results_status_other_count"),
              prometheus_counter_inc(MonitoringNamespace, "results_status_other_count")
          end
      end;
    {error, _} ->
      prometheus_counter_inc("all", "requests_error_count"),
      prometheus_counter_inc(MonitoringNamespace, "requests_error_count")
  end,
  R.

%% @doc Increment counter for a specific monitoring namespace and extension
prometheus_counter_inc(MonitoringNamespace, Extension) ->
  prometheus_counter:inc(namespace_to_prometheus_key(MonitoringNamespace, Extension)).

namespace_to_prometheus_key(MonitoringNamespace, Extension) ->
  list_to_atom("http_" ++ MonitoringNamespace++"_"++Extension).

%% @doc Build monitoring space from url (hostname with '.' replaced by '_')
monitoring_namespace(Request) ->
  URL = element(1, Request),
  #{ host := Host } = uri_string:parse(URL),
  convert_host(Host).

convert_host(Host) ->
  re:replace(Host, "\\.", "_", [global, {return, list}]).
