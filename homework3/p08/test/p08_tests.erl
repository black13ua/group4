-module(p08_tests).

-include_lib("eunit/include/eunit.hrl").

compress_empty_test() ->
    ?assertEqual([], p08:compress([])).

compress_test() ->
    ?assertEqual([a,b], p08:compress([a,a,b])),
    ?assertEqual([a,b,c], p08:compress([a,a,b,c,c,c,c])).
