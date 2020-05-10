main(N) :-
    drukuj(S2),
    podwajanie(S1, S2),
    generowanie(1, N, S1).

generowanie(I, J, S) :-
    (   I =< J
    ->  S = [I | T],
        I1 is I+1,
        generowanie(I1, J, T)
    ;   S = []).

podwajanie(S1, S2) :-
    freeze(S1,
           (   S1 = [H1 | T1]
           ->  H2 is 2*H1,
               S2 = [H2 | T2],
               podwajanie(T1, T2)
           ;   S2 = [])).

drukuj(S) :-
    freeze(S,
           (   S = [H | T]
           ->  writeln(H),
               drukuj(T)
           ;   true)).
