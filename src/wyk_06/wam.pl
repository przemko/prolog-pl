:- dynamic store/2.

store(0, str(1)).    % f(
store(1, f/2).       %
store(2, ref(2)).    %   A,
store(3, str(4)).    %	 g(
store(4, g/1).       %
store(5, ref(2)).    %     A))
store(6, str(7)).    % f(
store(7, f/2).       %
store(8, str(10)).   %	 a,
store(9, ref(9)).    %	 B)
store(10, a/0).      %

unify(A1, A2) :-
	dref(A1, D1),
	dref(A2, D2),
	unify2(D1, D2).

unify2(D, D) :- !.
unify2(D1, D2) :-
	store(D1, S1),
	store(D2, S2),
	S1 =.. [T1, V1],
	S2 =.. [T2, V2],
	unify3(D1, T1, V1, D2, T2, V2).

unify3(D1, ref, _, D2, _, _) :- !,
	bind(D1, D2).
unify3(D1, _, _, D2, ref, _) :- !,
	bind(D2, D1).
unify3(_, _, V1, _, _, V2) :-
	store(V1, F/N),
	store(V2, F/N),
	A1 is V1+1,
	A2 is V2+1,
	unify4(N, A1, A2).

unify4(0, _, _) :- !.
unify4(N, A1, A2) :-
	N1 is N-1,
	unify(A1, A2),
	NA1 is A1+1,
	NA2 is A2+1,
	unify4(N1, NA1, NA2).

dref(A, R) :-
	store(A, ref(V)),
	A =\= V, !,
	dref(V, R).
dref(A, A).

bind(A1, A2) :-
	retract(store(A1, _)),
	assert(store(A1, ref(A2))).

