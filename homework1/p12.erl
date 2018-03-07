-module(p12).
-export([decode_modified/1]).


decode_modified(L) ->
    decode_modified(L, []).
decode_modified([{0, _}|T], Acc) ->
    decode_modified(T, Acc);
decode_modified([{N, Letter}|T], Acc) ->
    decode_modified([{N-1, Letter}|T], [Letter|Acc]);
decode_modified([Letter|T], Acc) ->
    decode_modified(T, [Letter|Acc]);
decode_modified([], Acc) ->
    p05:reverse(Acc).
