suma_dodatnich(S) :-
    read(X),
    suma_dodatnich(X, 0, S).

suma_dodatnich(end_of_file, S, S) :-
    !.
suma_dodatnich(X, S1, S) :-
    (   X > 0
    ->  S2 is S1+X
    ;   S2 is S1),
    read(Y),
    suma_dodatnich(Y, S2, S).

