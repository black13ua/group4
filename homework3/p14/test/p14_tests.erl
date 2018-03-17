-module(p14_tests).

-include_lib("eunit/include/eunit.hrl").

duplicate_empty_test() ->
    ?assertEqual([], p14:duplicate([])).

duplicate_test() ->
    ?assertEqual([f,f,f,f], p14:duplicate([f,f])),
    ?assertEqual([a,a,f,f,z,z], p14:duplicate([a,f,z])).
