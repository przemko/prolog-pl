:- use_module(library(clpfd)).

hetmany(N, P) :-
	length(P, N),
	P ins 1..N,
	all_distinct(P),
	bezpieczna(P).

bezpieczna([]).
bezpieczna([I | L]) :-
	bezpieczna(L, I, 1),
	bezpieczna(L).

bezpieczna([], _, _).
bezpieczna([J | L], I, K) :-
	abs(I-J) #\= K,
	K1 is K+1,
	bezpieczna(L, I, K1).

