% czytaj(X) czyta standardowe wejście i wydziela słowa jako ciągi
% niebiałych znaków. Oddaje X będące listą przeczytanych słów.
%
czytaj(X) :-
	get_char(C),
	czytaj_dalej(C, X).

czytaj_dalej(end_of_file, []) :-
	!.
czytaj_dalej(C1, X) :-
	biały(C1),
	!,
	get_char(C2),
	czytaj_dalej(C2, X).
czytaj_dalej(C1, [H | T]) :-
	czytaj_słowo(C1, C2, '', H),
	czytaj_dalej(C2, T).


czytaj_słowo(end_of_file, end_of_file, N, N) :-
	!.
czytaj_słowo(C, C, N, N) :-
	biały(C),
	!.
czytaj_słowo(C1, C3, N1, N) :-
	atom_concat(N1, C1, N2),
	get_char(C2),
	czytaj_słowo(C2, C3, N2, N).

biały(' ').
biały('\t').
biały('\n').
