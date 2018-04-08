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
    case jsx:is_json(Body) of
        true ->
            Decoded = jsx:decode(Body),
            {ok, Reply} = cache_server_request(Decoded),
            ReplyJson = jsx:encode([{result, Reply}]),
            cowboy_req:reply(200, [], ReplyJson, Req);
        false ->
            cowboy_req:reply(400, [], <<"Not Valid JSON">>, Req)
    end;
web_api_response(<<"POST">>, false, Req) ->
    cowboy_req:reply(400, [], <<"Missing body">>, Req);
web_api_response(_, _, Req) ->
    cowboy_req:reply(405, Req).

cache_server_request([{<<"action">>, <<"lookup">>}, {<<"key">>, Key}]) ->
    cache_server:lookup(Key);
cache_server_request([{<<"action">>, <<"insert">>}, {<<"key">>, Key}, {<<"value">>, Val}]) ->
    {ok, cache_server:insert(Key, Val)};
cache_server_request([{<<"action">>, <<"lookup_by_date">>}, {<<"date_from">>, DateFrom}, {<<"date_to">>, DateTo}]) ->
    {ok, DateFromErl} = web_datetime_to_erlang(DateFrom),
    {ok, DateToErl} = web_datetime_to_erlang(DateTo),
    cache_server:lookup_by_date(DateFromErl, DateToErl);
cache_server_request([_Some, _]) ->
    {ok, <<"API Method not Implemented!">>}.

web_datetime_to_erlang(Datetime) ->
    [Date, Time] = binary:split(Datetime, <<" ">>, [trim]),
    [Year, Month, Day] = [erlang:binary_to_integer(X) || X <- binary:split(Date, <<"/">>, [global])],
    [Hour, Minute, Second] = [erlang:binary_to_integer(X) || X <- binary:split(Time, <<":">>, [global])],
    {ok, {{Year, Month, Day}, {Hour, Minute, Second}}}.

terminate(_Reason, _Req, _State) ->
ok.
