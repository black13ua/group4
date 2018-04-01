-module(my_cache).
-export([create/0, delete_obsolete/0, insert/3, lookup/1]).


create() ->
    case ets:info(?MODULE) of
        undefined -> 
            ets:new(?MODULE, [ordered_set, named_table]),
            ok;
        _ -> {error, table_present}
    end.

insert(K, V, TTL) ->
    Now = erlang:system_time(seconds),
    case ets:insert(?MODULE, {K, {V, TTL, Now + TTL}}) of
        true ->
            ok;
        _ ->
            {error, key_not_inserted}
    end. 

lookup(K) ->
    Now = erlang:system_time(seconds),
    case ets:lookup(?MODULE, K) of
        [{K, {V,_TTL,Expire}}] when Now < Expire ->
            {ok, V};
        [_|_] ->
            {error, expired};
        _ ->
            {error, not_found}
    end.

delete_obsolete() ->
    Now = erlang:system_time(seconds),
    First = ets:first(?MODULE),
    delete_obsolete(First, Now).

delete_obsolete('$end_of_table', _Now) ->
    ok;
delete_obsolete(K, Now) ->
    case ets:lookup(?MODULE, K) of
        [{K, {_V, _TTL, Expire}}] when Now > Expire ->
            Next = ets:next(?MODULE, K),
            ets:delete(?MODULE, K),
            delete_obsolete(Next, Now);
        _ ->
            Next = ets:next(?MODULE, K),
            delete_obsolete(Next, Now)
    end.
