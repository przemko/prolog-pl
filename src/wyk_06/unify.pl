unify(V, T) :-
    var(V), !,
    bind(V, T).
unify(T, V) :-
    var(V), !,
    bind(V, T).
unify(T1, T2) :-
    T1 =.. [Fun | Args1],
    T2 =.. [Fun | Args2],
    unify_args(Args1, Args2).

unify_args([], []).
unify_args([T1 | A1], [T2 | A2]) :-
    unify(T1, T2),
    unify_args(A1, A2).

bind(V, T) :-
    V = T.

