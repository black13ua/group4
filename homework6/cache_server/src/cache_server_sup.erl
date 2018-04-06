-module(cache_server_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    CacheServerSpec = {'cache_server',
        {'cache_server', start_link, [[{drop_interval, 30}]]},
        permanent,
        5000,
        worker,
        [cache_server]
    },
	Procs = [CacheServerSpec],
	{ok, {{one_for_one, 1, 5}, Procs}}.
