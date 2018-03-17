-module(bs03).
-export([split/2]).


split(Bin, Delimeter) ->
    DelimeterBin = unicode:characters_to_binary(Delimeter),
    Len = erlang:length(Delimeter),
    split(DelimeterBin, Len, Bin, <<>>, []).

split(D, Len, Bin, AccBin, Acc) ->
    case Bin of
        <<D:Len/binary, Rest/binary>> ->
            split(D, Len, Rest, <<>>, [AccBin|Acc]);
        <<X, Rest/binary>> ->
            split(D, Len, Rest, <<AccBin/binary, X>>, Acc);
        <<>> ->
            lists:reverse([AccBin|Acc])
    end.
