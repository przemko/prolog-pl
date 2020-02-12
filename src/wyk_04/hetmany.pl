hetmany(N, P) :-
	numlist(1, N, L),
	perm(L, P),
	dobra(P).

perm([], []).
perm(L1, [X | L3]) :-
	select(X, L1, L2),
	perm(L2, L3).

dobra(X) :-
	\+ zła(X).

zła(X) :-
	append(_, [Wi | L1], X),
	append(L2, [Wj | _], L1),
	length(L2, K),
	abs(Wi - Wj) =:= K + 1.


