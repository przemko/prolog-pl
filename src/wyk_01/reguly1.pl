lubi(jan, X) :-
	lubi(X, wino).
lubi(jan, X) :-
	lubi(X, wino),
	lubi(X, jedzenie).
lubi(jan, X) :-
	kobieta(X),
	lubi(X, wino).

