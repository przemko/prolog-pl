:- module(interpreter, [interpreter/1, interpreter/2]).

% Interpreter prostego języka imperatywnego w Prologu.
%
% interpreter(+PROGRAM) wykonaniu programu
%
%    PROGRAM = []
%    PROGRAM = [INTRUKCJA | PROGRAM]
%
% INSTRUKCJA = read(ID)
% INSTRUKCJA = write(WYRAŻENIE)
% INSTRUKCJA = assign(ID, WYRAŻENIE)
% INSTRUKCJA = if(WARUNEK, INSTRUKCJE)
% INSTRUKCJA = if(WARUNEK, INSTRUKCJE, INSTRUKCJE)
% INSTRUKCJA = while(WARUNE, INSTRUKCJE)
%
%  WYRAŻENIE = int(NUM)
%  WYRAŻENIE = id(ID)
%  WYRAŻENIE = WYRAŻENIE + WYRAŻENIE
%  WYRAŻENIE = WYRAŻENIE - WYRAŻENIE
%  WYRAŻENIE = WYRAŻENIE * WYRAŻENIE
%  WYRAŻENIE = WYRAŻENIE / WYRAŻENIE
%  WYRAŻENIE = WYRAŻENIE mod WYRAŻENIE
%
%    WARUNEK = WYRAŻENIE =:= WYRAŻENIE
%    WARUNEK = WYRAŻENIE =\= WYRAŻENIE
%    WARUNEK = WYRAŻENIE < WYRAŻENIE
%    WARUNEK = WYRAŻENIE > WYRAŻENIE
%    WARUNEK = WYRAŻENIE >= WYRAŻENIE
%    WARUNEK = WYRAŻENIE =< WYRAŻENIE
%    WARUNEK = (WARUNEK, WARUNEK)
%    WARUNEK = (WARUNEK; WARUNEK)
%
interpreter(PROGRAM) :-
    interpreter(PROGRAM, []).

% interpreter(+PROGRAM, +ASOCJACJE) wykonanie programu w danym stanie
%
% ASOCJACJE = []
% ASOCJACJE = [ASOCJACJA | ASOCJACJE]
% ASOCJACJA = ID = NUM
%
interpreter([], _).
interpreter([read(ID) | INSTRUKCJE], ASOC) :- !,
    read(N),
    integer(N),
    podstaw(ASOC, ID, N, ASOC1),
    interpreter(INSTRUKCJE, ASOC1).
interpreter([write(W) | INSTRUKCJE], ASOC) :- !,
    wartość(W, ASOC, WART),
    write(WART), nl,
    interpreter(INSTRUKCJE, ASOC).
interpreter([assign(ID, W) | INSTRUKCJE], ASOC) :- !,
    wartość(W, ASOC, WAR),
    podstaw(ASOC, ID, WAR, ASOC1),
    interpreter(INSTRUKCJE, ASOC1).
interpreter([if(C, P) | INSTRUKCJE], ASOC) :- !,
    interpreter([if(C, P, []) | INSTRUKCJE], ASOC).
interpreter([if(C, P1, P2) | INSTRUKCJE], ASOC) :- !,
    (   prawda(C, ASOC)
    ->  append(P1, INSTRUKCJE, DALEJ)
    ;   append(P2, INSTRUKCJE, DALEJ)),
    interpreter(DALEJ, ASOC).
interpreter([while(C, P) | INSTRUKCJE], ASOC) :- !,
    append(P, [while(C, P)], DALEJ),
    interpreter([if(C, DALEJ) | INSTRUKCJE], ASOC).

podstaw([], ID, N, [ID = N]).
podstaw([ID = _ | ASOC], ID, N, [ID = N | ASOC]) :- !.
podstaw([ID1 = W1 | ASOC1], ID, N, [ID1 = W1 | ASOC2]) :-
    podstaw(ASOC1, ID, N, ASOC2).

pobierz([ID = N | _], ID, N) :- !.
pobierz([_ | ASOC], ID, N) :-
    pobierz(ASOC, ID, N).

wartość(int(N), _, N).
wartość(id(ID), ASOC, N) :-
    pobierz(ASOC, ID, N).
wartość(W1+W2, ASOC, N) :-
    wartość(W1, ASOC, N1),
    wartość(W2, ASOC, N2),
    N is N1+N2.
wartość(W1-W2, ASOC, N) :-
    wartość(W1, ASOC, N1),
    wartość(W2, ASOC, N2),
    N is N1-N2.
wartość(W1*W2, ASOC, N) :-
    wartość(W1, ASOC, N1),
    wartość(W2, ASOC, N2),
    N is N1*N2.
wartość(W1/W2, ASOC, N) :-
    wartość(W1, ASOC, N1),
    wartość(W2, ASOC, N2),
    N2 =\= 0,
    N is N1 div N2.
wartość(W1 mod W2, ASOC, N) :-
    wartość(W1, ASOC, N1),
    wartość(W2, ASOC, N2),
    N2 =\= 0,
    N is N1 mod N2.

prawda(W1 =:= W2, ASOC) :-
    wartość(W1, ASOC, N1),
    wartość(W2, ASOC, N2),
    N1 =:= N2.
prawda(W1 =\= W2, ASOC) :-
    wartość(W1, ASOC, N1),
    wartość(W2, ASOC, N2),
    N1 =\= N2.
prawda(W1 < W2, ASOC) :-
    wartość(W1, ASOC, N1),
    wartość(W2, ASOC, N2),
    N1 < N2.
prawda(W1 > W2, ASOC) :-
    wartość(W1, ASOC, N1),
    wartość(W2, ASOC, N2),
    N1 > N2.
prawda(W1 >= W2, ASOC) :-
    wartość(W1, ASOC, N1),
    wartość(W2, ASOC, N2),
    N1 >= N2.
prawda(W1 =< W2, ASOC) :-
    wartość(W1, ASOC, N1),
    wartość(W2, ASOC, N2),
    N1 =< N2.
prawda((W1, W2), ASSOC) :-
    prawda(W1, ASSOC),
    prawda(W2, ASSOC).
prawda((W1; W2), ASSOC) :-
    (   prawda(W1, ASSOC),
	!
    ;   prawda(W2, ASSOC)).
