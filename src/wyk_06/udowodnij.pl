% Interpreter podzbioru Prologu w Prologu:
%
% ?- udowodnij(app(X, Y, [1, 2, 3])).
% X = [],
% Y = [1, 2, 3] ;
% X = [1],
% Y = [2, 3] ;
% X = [1, 2],
% Y = [3] ;
% X = [1, 2, 3],
% Y = [] ;
% false.
%
% ?- udowodnij(app([[1,2], [3], [4, 5, 6]], X)).
% X = [1, 2, 3, 4, 5, 6].
%
udowodnij(true) :- !.
udowodnij((G1, G2)) :- !,
    udowodnij(G1),
    udowodnij(G2).
udowodnij(A) :-
    clause(A, B),
    udowodnij(B).

app([], []).
app([L1 | L2], L3) :-
    app(L1, L4, L3),
    app(L2, L4).

app([], X, X).
app([X | L1], L2, [X | L3]) :-
    app(L1, L2, L3).
