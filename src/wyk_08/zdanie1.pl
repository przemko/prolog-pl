% Naiwna analiza zdań języka naturalnego.
%
zdanie(X) :-
	append(Y, Z, X),
	fraza_rzecz(Y),
	fraza_czas(Z).

fraza_rzecz(X) :-
	append(Y, Z, X),
	przedimek(Y),
	rzeczownik(Z).

fraza_czas(X) :-
	append(Y, Z, X),
	czasownik(Y),
	fraza_rzecz(Z).
fraza_czas(X) :-
	czasownik(X).

przedimek([the]).

rzeczownik([apple]).
rzeczownik([man]).

czasownik([eats]).
czasownik([sings]).
