% populacja w milionach w roku 1976
populacja(usa, 203).
populacja(indie, 548).
populacja(chiny, 800).
populacja(brazylia, 108).

% obszar kraju w milionach mil kwadratowych
obszar(usa, 3).
obszar(indie, 1).
obszar(chiny, 4).
obszar(brazylia, 3).

% gęstość zaludnienia
gęstość(X, Y) :-
	populacja(X, P),
	obszar(X, A),
	Y is P/A.

