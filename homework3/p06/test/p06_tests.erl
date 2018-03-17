-module(p06_tests).

-include_lib("eunit/include/eunit.hrl").

is_palindrome_test() ->
    ?assertEqual(true, p06:is_palindrome([1])),
    ?assertEqual(false, p06:is_palindrome([1,2,3])),
    ?assertEqual(true, p06:is_palindrome([1,2,3,2,1])),
    ?assertEqual(true, p06:is_palindrome_v2([1])),
    ?assertEqual(false, p06:is_palindrome_v2([1,2,3])),
    ?assertEqual(true, p06:is_palindrome_v2([1,2,3,2,1])).
