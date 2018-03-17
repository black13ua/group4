-module(p09_tests).

-include_lib("eunit/include/eunit.hrl").

pack_empty_test() ->
    ?assertEqual([], p09:pack([])).

pack_test() ->
    ?assertEqual([[a,a],[b]], p09:pack([a,a,b])),
    ?assertEqual([[a,a],[b],[c],[z]], p09:pack([a,a,b,c,z])).
