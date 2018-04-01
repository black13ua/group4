-module(my_cache_tests).

-include_lib("eunit/include/eunit.hrl").

create_test() ->
    ?assertEqual(ok, my_cache:create()).

insert_test() ->
    ?assertEqual(ok, my_cache:insert(key1, val1, 5)),
    ?assertEqual(ok, my_cache:insert(key2, val2, 10)),
    ?assertEqual(ok, my_cache:insert(key1Expired, val1Expired, -1)),
    ?assertEqual(ok, my_cache:insert(key2Expired, val2Expired, -2)).

lookup_test() ->
    ?assertEqual({ok, val1}, my_cache:lookup(key1)),
    ?assertEqual({ok, val2}, my_cache:lookup(key2)),
    ?assertEqual({error, not_found}, my_cache:lookup(key3)),
    ?assertEqual({error, expired}, my_cache:lookup(key1Expired)),
    ?assertEqual({error, expired}, my_cache:lookup(key2Expired)).

delete_test() ->
    ?assertEqual(ok, my_cache:delete_obsolete()),
    ?assertEqual({ok, val1}, my_cache:lookup(key1)),
    ?assertEqual({ok, val2}, my_cache:lookup(key2)),
    ?assertEqual({error, not_found}, my_cache:lookup(key1Expired)),
    ?assertEqual({error, not_found}, my_cache:lookup(key2Expired)).

