:- use_module(library(clpfd)).

ogrodzenie(D, X, Y, P) :-
    [X, Y] ins 0..D,
    X + 2*Y #= D, P #= X*Y,
    labeling([max(P)], [X, Y]).

