-module(cache_server_sup).
-behaviour(supervisor).

-export([start_link/0, start_link/1]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_link(DropInterval) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [DropInterval]).

init([DropInterval]) ->
    CacheServerSpec = {'cache_server',
        {'cache_server', start_link, [[{drop_interval, DropInterval}]]},
        permanent,
        5000,
        worker,
        [cache_server]
    },
	Procs = [CacheServerSpec],
	{ok, {{one_for_one, 1, 5}, Procs}}.
