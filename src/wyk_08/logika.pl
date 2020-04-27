% Gramatyka metamorficzna ZDANIE <--> FORMUŁA RACHUNKU PREDYKATÓW
%
:- op(900, xfx, =>).
:- op(800, xfy, &).
:- op(550, xfy, :).

zdanie(P) --> fraza_rzecz(X, P1, P), fraza_czas(X, P1).

fraza_rzecz(X, P1, P) -->
	przedimek(X, P2, P1, P), rzeczownik(X, P3),
	klauzula_wzgl(X, P3, P2).
fraza_rzecz(X, P, P) --> rzeczownik_wlasny(X).

fraza_czas(X, P) -->
	czasownik_przech(X, Y, P1), fraza_rzecz(Y, P1, P).
fraza_czas(X, P) --> czasownik_nieprzech(X, P).

klauzula_wzgl(X, P1, P1&P2) --> [that], fraza_czas(X, P2).
klauzula_wzgl(_, P, P) --> [].

przedimek(X, P1, P2, all(X):(P1=>P2)) --> [every].
przedimek(X, P1, P2, exists(X):(P1&P2)) --> [a].

rzeczownik(X, man(X)) --> [man].
rzeczownik(X, woman(X)) --> [woman].

rzeczownik_wlasny(john) --> [john].

czasownik_przech(X, Y, loves(X,Y)) --> [loves].
czasownik_przech(X, Y, knows(X,Y)) --> [knows].

czasownik_nieprzech(X, lives(X)) --> [lives].
