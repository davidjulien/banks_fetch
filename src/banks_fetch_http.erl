-module(banks_fetch_http).

-export([
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

-spec set_options([{cookies, enabled}]) -> ok | {error, Reason :: term()}.
set_options(Options) ->
  httpc:set_options(Options).

-spec request(get | post, request(), http_options(), options()) -> {ok, {status_line(), headers(), Body :: string() | binary()}} | {error, Reason :: term()}.
request(Method, Request, HttpOptions, Options) ->
  httpc:request(Method, Request, HttpOptions, Options).
