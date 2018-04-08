-module(cache_server_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-define(DEFAULT_DROP_INTERVAL, 60).

start(_Type, _Args) ->
    DropInterval = application:get_env(cache_server, drop_interval, ?DEFAULT_DROP_INTERVAL),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/[...]", web_api_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_http(http, 10, [{port, 8080}], [
        {env, [{dispatch, Dispatch}]}
    ]),
	cache_server_sup:start_link(DropInterval).

stop(_State) ->
	ok.
