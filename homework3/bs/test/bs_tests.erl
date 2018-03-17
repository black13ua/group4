-module(bs_tests).

-include_lib("eunit/include/eunit.hrl").

empty_test() ->
    ?assertEqual(<<>>, bs01:first_word(<<>>)),
    ?assertEqual(<<>>, bs01:first_word(<<"">>)),
    ?assertEqual([<<>>], bs02:words(<<>>)),
    ?assertEqual([<<>>], bs02:words(<<"">>)),
    ?assertEqual([<<>>], bs03:split(<<>>, ":")),
    ?assertEqual([<<>>], bs03:split(<<"">>, ":")).

functional_test() ->
    ?assertEqual(<<"Some">>, bs01:first_word(<<"Some Words!">>)),
    ?assertEqual([<<"Some">>,<<"Words!">>], bs02:words(<<"Some Words!">>)),
    ?assertEqual([<<"Partner_ID">>,<<"Address">>,<<"Person">>], bs03:split(<<"Partner_ID;Address;Person">>,";")).

xml_test() ->
    XML = <<"<note><to>Tove</to><from>Jani</from><heading>Reminder</heading><body>Don't forget me this weekend!</body></note>">>,
    Result = {<<"note">>,[], [
               {<<"to">>,[],[<<"Tove">>]},
	       {<<"from">>,[],[<<"Jani">>]},
               {<<"heading">>,[],[<<"Reminder">>]},
	       {<<"body">>,[],[<<"Don't forget me this weekend!">>]}
	     ]},
    ?assertEqual(Result, bs04:decode_xml(XML)).
