-module(p03_tests).

-include_lib("eunit/include/eunit.hrl").

element_at_test() ->
    ?assertEqual(7, p03:element_at([7], 1)),
    ?assertEqual(9, p03:element_at([0,2,9], 3)).
