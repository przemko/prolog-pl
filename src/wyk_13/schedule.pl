:- use_module(library(clpfd)).

tasks([ %D  R
        [2, 1],
        [3, 2],
        [4, 2],
        [3, 3],
        [3, 1],
        [3, 4],
        [5, 2]]).

resources(5).


schedule(H, Ss, MS) :-
    tasks(L),
    resources(R),
    MS in 0..H,
    mt(L, H, T, Ss, MS),
    cumulative(T, [limit(R)]),
    once(labeling([min(MS), ff], [MS | Ss])).

mt([], _, [], [], _).
mt([[D, R] | L1], H, [task(S, D, E, R, _) | L2],
   [S | L3], MS) :-
    S in 0..H,
    E #= S + D, MS #>= E,
    mt(L1, H, L2, L3, MS).
