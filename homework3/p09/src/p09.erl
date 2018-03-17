-module(p09).
-export([pack/1]).


pack(L) ->
    pack(L, [], []).

pack([H|T], [H|AccT], Acc) ->
    pack(T, [H,H|AccT], Acc);
pack([H|T], AccT, Acc) ->
    pack(T, [H], [AccT|Acc]);
pack([], AccT, Acc) ->
    [_Empty|List] = lists:reverse([AccT|Acc]),
    List.
