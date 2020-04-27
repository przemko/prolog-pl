% Poprawiona analiza zdań języka naturalnego.
%
zdanie(X, Z) :-
	fraza_rzecz(X, Y),
	fraza_czas(Y, Z).

fraza_rzecz(X, Z) :-
	przedimek(X, Y),
	rzeczownik(Y, Z).

fraza_czas(X, Z) :-
	czasownik(X, Y),
	fraza_rzecz(Y, Z).
fraza_czas(X, Y) :-
	czasownik(X, Y).

przedimek([the | X], X).

rzeczownik([apple | X], X).
rzeczownik([man | X], X).

czasownik([eats | X], X).
czasownik([sings | X], X).
