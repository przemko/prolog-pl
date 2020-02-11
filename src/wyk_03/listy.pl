jest_listą([A | B]) :-
	jest_listą(B).
jest_listą([]).

słaba_jest_listą([]).
słaba_jest_listą([_ | _]).

dllisty([], 0).
dllisty([G | O], N) :-
	dllisty(O, N1),
	N is N1+1.

dllisty2(L, N) :-
	listaakum(L, 0, N).

listaakum([], A, A).
listaakum([G | O], A, N) :-
	A1 is A+1,
	listaakum(O, A1, N).

