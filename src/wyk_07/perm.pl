perm([], []).
perm([A | B], C) :-
    select(A, C, D),
    perm(B, D).

