-module(web_api_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    HasBody = cowboy_req:has_body(Req2),
    {ok, Req3} = web_api_response(Method, HasBody, Req2),
{ok, Req3, State}.

web_api_response(<<"POST">>, true, Req) ->
    {ok, Body, _Req2} = cowboy_req:body(Req),
    io:format("Body: ~p~n", [Body]),
    case jsx:is_json(Body) of
        true ->
            Decoded = jsx:decode(Body),
            io:format("JSON Decoded: ~p~n", [Decoded]),
            cache_server_request(Decoded),
            cowboy_req:reply(200, [], <<"Allowed body">>, Req);
        false ->
            io:format("JSON Not Valid: ~p~n", [Body]),
            cowboy_req:reply(400, [], <<"Not Valid JSON">>, Req)
    end;
web_api_response(<<"POST">>, false, Req) ->
    cowboy_req:reply(400, [], <<"Missing body">>, Req);
web_api_response(_, _, Req) ->
    %% Method not allowed.
    cowboy_req:reply(405, Req).

cache_server_request([{<<"action">>, <<"lookup">>}, {<<"key">>, Key}]) ->
    Val = cache_server:lookup(Key),
    io:format("Lookup KEY: ~p~n", [Val]);
cache_server_request([Some, _]) ->
    io:format("Some POST: ~p~n", [Some]).
%cache_server_request(lookup_by_date) ->
%cache_server_request(insert) ->

terminate(_Reason, _Req, _State) ->
ok.
