-module(p10_tests).

-include_lib("eunit/include/eunit.hrl").

encode_empty_test() ->
    ?assertException(error, function_clause, p10:encode([])).

encode_test() ->
    ?assertEqual([{2,a},{1,b}], p10:encode([a,a,b])),
    ?assertEqual([{2,a},{1,b},{1,c},{1,z}], p10:encode([a,a,b,c,z])).
