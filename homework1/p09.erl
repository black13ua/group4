-module(p09).
-export([pack/1]).


pack(L) ->
    pack(L, [], []).

pack([H|T], [H=HL|AccL], Acc) ->
    pack(T, [H,HL|AccL], Acc);
pack([H|T], [], Acc) ->
    pack(T, [H], Acc);
pack([H|T], AccL, Acc) ->
    pack(T, [H], [AccL|Acc]);
pack([], AccL, Acc) ->
    p05:reverse([AccL|Acc]).
