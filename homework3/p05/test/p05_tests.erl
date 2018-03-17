-module(p05_tests).

-include_lib("eunit/include/eunit.hrl").

reverse_at_test() ->
    ?assertEqual([2,7], p05:reverse([7,2])),
    ?assertEqual([9,2,0], p05:reverse([0,2,9])).
