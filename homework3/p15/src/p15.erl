-module(p15).
-export([replicate/2]).


replicate(L, N) ->
    replicate(L, N, 1, []).

replicate([], _N, _C, Acc) ->
    lists:reverse(Acc);
replicate([H|T], N, _C=N, Acc) ->
    replicate(T, N, 1, [H|Acc]);
replicate([H|T], N, C, Acc) ->
    replicate([H|T], N, C+1, [H|Acc]).
