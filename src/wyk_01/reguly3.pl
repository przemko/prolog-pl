złodziej(jan).

lubi(maria, czekolada).
lubi(maria, wino).
lubi(jan, X) :-
	lubi(X, wino).

może_ukraść(X, Y) :-
	złodziej(X),
	lubi(X, Y).

