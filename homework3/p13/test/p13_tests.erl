-module(p13_tests).

-include_lib("eunit/include/eunit.hrl").

decode_empty_test() ->
    ?assertEqual([], p13:decode([])).

decode_test() ->
    ?assertEqual([a,f,f,f], p13:decode([{1,a},{3,f}])),
    ?assertEqual([a,a,a,f,z,z], p13:decode([{3,a},{1,f},{2,z}])).
