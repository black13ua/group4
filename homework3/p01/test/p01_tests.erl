-module(p01_tests).

-include_lib("eunit/include/eunit.hrl").

last_test() ->
    ?assertEqual(1, p01:last([1])),
    ?assertEqual(3, p01:last([1,2,3])).
