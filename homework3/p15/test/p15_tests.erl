-module(p15_tests).

-include_lib("eunit/include/eunit.hrl").

replicate_empty_test() ->
    ?assertEqual([], p15:replicate([],3)).

replicate_test() ->
    ?assertEqual([f,f,f,z,z,z], p15:replicate([f,z],3)),
    ?assertEqual([a,a,f,f,z,z], p15:replicate([a,f,z],2)).
