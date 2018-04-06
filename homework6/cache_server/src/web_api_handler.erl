-module(web_api_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    HasBody = cowboy_req:has_body(Req2),
    io:format("Req: ~p~n", [Req2]),
    {ok, Req3} = web_api_response(Method, HasBody, Req2),
{ok, Req3, State}.

web_api_response(<<"POST">>, true, Req) ->
    {ok, PostVals, _Req2} = cowboy_req:body_qs(Req),
    io:format("Vals: ~p~n", [PostVals]),
    case jsx:is_json(PostVals) of
        true ->
            _Decoded = jsx:decode(PostVals),
            cowboy_req:reply(200, [], <<"Allowed body.">>, Req);
        false ->
            io:format("JSON Not Valid: ~p~n", [PostVals]),
            cowboy_req:reply(400, [], <<"Not Valid JSON">>, Req)
    end;
web_api_response(<<"POST">>, false, Req) ->
    cowboy_req:reply(400, [], <<"Missing body.">>, Req);
web_api_response(_, _, Req) ->
    %% Method not allowed.
    cowboy_req:reply(405, Req).

%cache_server_request(lookup) ->
%cache_server_request(lookup_by_date) ->
%cache_server_request(insert) ->

terminate(_Reason, _Req, _State) ->
ok.
