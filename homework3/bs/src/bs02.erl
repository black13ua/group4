-module(bs02).
-export([words/1]).


words(Bin) ->
    words(Bin, <<>>, []).

words(<<" ", Rest/binary>>, AccBin, Acc) ->
    words(Rest, <<>>, [AccBin|Acc]);
words(<<X, Rest/binary>>, AccBin, Acc) ->
    words(Rest, <<AccBin/binary, X>>, Acc);
words(<<>>, AccBin, Acc) ->
    lists:reverse([AccBin|Acc]).
