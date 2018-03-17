-module(p09).
-export([pack/1]).


pack([H|T]) ->
    pack(T, [H], []);
pack([]) ->
    [].

pack([H|T], [H|AccT], Acc) ->
    pack(T, [H,H|AccT], Acc);
pack([H|T], AccT, Acc) ->
    pack(T, [H], [AccT|Acc]);
pack([], AccT, Acc) ->
    p05:reverse([AccT|Acc]).

