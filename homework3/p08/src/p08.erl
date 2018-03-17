-module(p08).
-export([compress/1]).


compress(L) ->
    compress(L, []).

compress([H|T], [H|Acc]) ->
    compress(T, [H|Acc]);
compress([H|T], Acc) ->
    compress(T, [H|Acc]);
compress([], Acc) ->
    lists:reverse(Acc).
