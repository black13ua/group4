-module(p02).
-export([but_last/1]).

but_last([H1,H2|[]]) ->
    [H1,H2];
but_last([_H|T]) ->
    but_last(T).
