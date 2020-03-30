pp([H | T], I) :-
    !,
    J is I+3,
    pp(H, J),
    ppx(T, J).
pp(X, I) :-
    tab(I),
    write(X),
    nl.

ppx([], _).
ppx([H | T], I) :-
    pp(H, I),
    ppx(T, I).

