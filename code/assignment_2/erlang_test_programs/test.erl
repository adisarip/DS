-module(test).
-export([info/0, avg/1, sum/1, len/1]).
-export([bump/1, bump2/1, is_member/2, rev/1, count/2]).

info() -> io:format("This module is for trying stuff in Erlang.~n").

avg([]) -> io:format("List is empty.~n");
avg(List) -> sum(List) / len(List).

sum(List) -> sum_acc(List, 0).
sum_acc([], Sum) ->
    io:format("~w~n", [Sum]);
sum_acc(List, Sum) ->
    [H|T] = List,
    sum_acc(T, Sum + list_to_integer(atom_to_list(H))).

len([]) -> 0;
len([_|T]) -> 1 + len(T).

bump([]) -> [];
bump([H|T]) -> [H+1 | bump(T)].

bump2(List) -> bump_acc(List, []).
bump_acc([], Acc) -> Acc;
bump_acc([H|T], Acc) -> bump_acc(T, Acc ++ [H+1]).

rev(List) -> rev_acc(List, []).
rev_acc([], Acc) -> io:format("~w~n", [Acc]);
rev_acc([H | T], Acc) -> rev_acc(T, [H | Acc]).

is_member(_, []) -> false;
is_member(H, [H|_]) -> true;
is_member(H, [_|T]) -> is_member(H,T).

count(X,V) ->
    if
        X < V-1  -> io:format("X=~p V=~p~n", [X,V]), count(X+1, V);
        X == V-1 -> io:format("X==V-1==~p~n", [X])
    end.
