-module(graph).
-export([info/0, start/1]).

info() -> io:format("Computing shortest paths in a weighted graph in Erlang.~n").

% foreach loop construct to traverse a list
foreach(List) -> foreach(List, 0).
foreach([], Index) -> Index;
foreach([H|T], Index) ->
    % Do Something with the element stored in H
    io:format("List[~w]:~w~n", [Index, H]),
    foreach(T, Index+1).


start(L) -> foreach(L).
