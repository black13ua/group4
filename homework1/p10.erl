-module(p10).
-export([encode/1]).


encode(L) ->
    encode(L, 1, [], []).

encode([H|T], N, [H=S|AccT], Acc) ->
    encode(T, N+1, [H], Acc);
encode([H|T], N, [S|AccT], Acc) ->
    encode(T, 1, [H], [{N,S}|Acc]);
encode([H|T], N, [], Acc) ->
    encode(T, 1, [H], Acc);
encode([], N, [S|AccT], Acc) ->
    p05:reverse([{N,S}|Acc]).
