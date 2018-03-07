-module(p08).
-export([compress/1]).


compress(L) ->
    compress(L, []).

compress([HL|T], [HL = HA|Acc]) ->
    compress(T, [HA|Acc]);
compress([HL|T], Acc) ->
    compress(T, [HL|Acc]);
compress([], Acc) ->
    p05:reverse(Acc).
