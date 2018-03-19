-module(p10).
-export([encode/1]).


encode([H|T]) ->
    encode(T, 1, [H], []).

encode([H|T], N, [H|_AccT], Acc) ->
    encode(T, N+1, [H], Acc);
encode([H|T], N, [S|_AccT], Acc) ->
    encode(T, 1, [H], [{N,S}|Acc]);
encode([], N, [S|_AccT], Acc) ->
    lists:reverse([{N,S}|Acc]).
