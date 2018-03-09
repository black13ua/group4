-module(bs04).
-export([decode_xml/1]).


decode_xml(Bin) ->
    [_, Header|T] = bs03:split(Bin, "<"),
    decode_xml(T, Header, []).

decode_xml([<<"/", _Rest/binary>>|T], Header, Acc) ->
    decode_xml(T, Header, Acc);
decode_xml([H|T], Header, Acc) ->
    [Item, Text] = bs03:split(H, ">"),
    decode_xml(T, Header, [{Item, [], [Text]}|Acc]);
decode_xml([], Header, Acc) ->
    {Header, [], lists:reverse(Acc)}.

