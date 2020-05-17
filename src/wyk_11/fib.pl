:- use_module(library(clpfd)).

fib(0).
fib(1).
fib(F) :-
    fib(1, 1, F).

fib(F1, F2, F) :-
    F #> F2,
    F3 #= F1+F2,
    (   F #= F3
    ;   fib(F2, F3, F)).

