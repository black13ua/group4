-module(p02_tests).

-include_lib("eunit/include/eunit.hrl").

but_last_test() ->
    ?assertEqual([3,5], p02:but_last([1,2,3,5])),
    ?assertEqual([x,z], p02:but_last([a,b,c,d,x,z])),
    ?assertException(error, function_clause, p02:but_last([])),
    ?assertException(error, function_clause, p02:but_last([a])).
