-module(boolean).
-export([b_not/1, b_and/2, b_or/2, b_nand/2]).

% implementing b_not/1
b_not(X)  -> X == false.

% implementing b_and/2
b_and(X,Y) -> {X,Y} == {true, true}.

% implementing b_or/2
b_or(X,Y) -> b_not({X,Y} == {false, false}).

% implementing b_nand/2
b_nand(X,Y) -> b_not(b_and(X,Y)).

