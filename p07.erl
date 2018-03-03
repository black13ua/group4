-module(p07).
-export([flatten/1]).


flatten(L) ->
    flatten(L, []).

flatten([[]|T], Acc) ->
    flatten(T, Acc);
flatten([[H1|[H2|T2]]|T], Acc) ->
    flatten([H2,T2|T], [H1|Acc]);
flatten([[H1|T1]|T], Acc) ->
    flatten([T1|T], [H1|Acc]);
flatten([H|T], Acc) ->
    flatten(T, [H|Acc]);
flatten([], Acc) ->
    p05:reverse(Acc).
