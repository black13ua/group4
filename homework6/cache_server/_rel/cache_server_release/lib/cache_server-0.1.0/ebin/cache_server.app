{application, 'cache_server', [
	{description, "Cache Server with WEB API"},
	{vsn, "0.1.0"},
	{modules, ['cache_server','cache_server_app','cache_server_sup','web_api_handler']},
	{registered, [cache_server_sup]},
	{applications, [kernel,stdlib,cowboy,jsx]},
	{mod, {cache_server_app, []}},
	{env, []}
]}.