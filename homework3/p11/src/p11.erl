-module(p11).
-export([encode_modified/1]).


encode_modified(L) ->
    encode_modified(L, 0, [], []).

encode_modified([H|T], N, [H=_S|_AccT], Acc) ->
    encode_modified(T, N+1, [H], Acc);
encode_modified([H|T], 1, [S|_AccT], Acc) ->
    encode_modified(T, 1, [H], [S|Acc]);
encode_modified([H|T], N, [S|_AccT], Acc) ->
    encode_modified(T, 1, [H], [{N,S}|Acc]);
encode_modified([H|T], _N, [], Acc) ->
    encode_modified(T, 1, [H], Acc);
encode_modified([], 1, [S|_AccT], Acc) ->
    lists:reverse([S|Acc]);
encode_modified([], N, [S|_AccT], Acc) ->
    lists:reverse([{N,S}|Acc]).
