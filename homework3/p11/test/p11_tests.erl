-module(p11_tests).

-include_lib("eunit/include/eunit.hrl").

encode_empty_test() ->
    ?assertException(error, function_clause, p11:encode_modified([])).

encode_test() ->
    ?assertEqual([{2,a},b], p11:encode_modified([a,a,b])),
    ?assertEqual([a,{3,c},f,z,x,{2,d},h], p11:encode_modified([a,c,c,c,f,z,x,d,d,h])).
