-module(p07_tests).

-include_lib("eunit/include/eunit.hrl").

flatten_empty_test() ->
    ?assertEqual([], p07:flatten([])).

flatten_test() ->
    ?assertEqual([1,2,3], p07:flatten([1,[2,[3]]])),
    ?assertEqual([1,2,3,4,x], p07:flatten([[],[1,[2,[3],4]],[x]])).
