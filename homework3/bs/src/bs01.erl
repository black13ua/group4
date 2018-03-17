-module(bs01).
-export([first_word/1]).


first_word(Bin) ->
    first_word(Bin, <<>>).

first_word(<<" ", _Rest/binary>>, Acc) ->
    Acc;
first_word(<<X, Rest/binary>>, Acc) ->
    first_word(<<Rest/binary>>, <<Acc/binary, X>>);
first_word(<<>>, Acc) ->
    Acc.
