mężczyzna(albert).
mężczyzna(edward).

kobieta(alicja).
kobieta(wiktoria).

rodzice(edward, wiktoria, albert).
rodzice(alicja, wiktoria, albert).

siostra(X, Y) :-
	kobieta(X),
	rodzice(X, M, O),
	rodzice(Y, M, O).

