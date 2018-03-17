-module(p04_tests).

-include_lib("eunit/include/eunit.hrl").

element_at_test() ->
    ?assertEqual(0, p04:len([])),
    ?assertEqual(1, p04:len([7])),
    ?assertEqual(3, p04:len([0,2,9])).
