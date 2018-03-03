-module(p06).
-export([is_palindrome/1, is_palindrome_v2/1]).


is_palindrome(L) ->
    is_palindrome(L, []).

is_palindrome([_H|_T = Acc], Acc) ->
    true;
is_palindrome([H|T], Acc) ->
    is_palindrome(T, [H|Acc]);
is_palindrome([], _Acc) ->
    false.


is_palindrome_v2(L) ->
    is_palindrome_v2(L, p05:reverse(L)).

is_palindrome_v2(L, L=_LT) ->
    true;
is_palindrome_v2(_, _) ->
    false.
