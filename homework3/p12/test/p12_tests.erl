-module(p12_tests).

-include_lib("eunit/include/eunit.hrl").

decode_empty_test() ->
    ?assertEqual([], p12:decode_modified([])).

decode_test() ->
    ?assertEqual([b,c], p12:decode_modified([b,c])),
    ?assertEqual([b,c,c,c,z], p12:decode_modified([b,{3,c},z])).
