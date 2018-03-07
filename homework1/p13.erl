-module(p13).
-export([decode/1]).


decode(L) ->
    decode(L, []).

decode([{0, _}|T], Acc) ->
    decode(T, Acc);
decode([{N, Letter}|T], Acc) ->
    decode([{N-1, Letter}|T], [Letter|Acc]);
decode([], Acc) ->
    p05:reverse(Acc).
