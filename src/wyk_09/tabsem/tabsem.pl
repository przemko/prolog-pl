/*

    tabsem.pl - method of analytic tableaux for the first order logic
    Copyright (C) 2012-2013  Przemyslaw Kobylanski

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

    Author: Przemyslaw Kobylanski <przemko@pwr.wroc.pl>

*/

% tabsem.pl
%
% Metoda tabel semantycznych dla rachunku predykatow
%
% Eksportowane predykaty:
%
%   prove/3  dowodzenie formuly
%   prove/4  dowodzenie formuly z limitem glebokosci
%   model/4  konstrukcja modelu
%   model/5  konstrukcja mdelu z limitem glebokosci
%
% Importowane moduly:
%
%   ordsets  operacje mnogosciowe na listach
%
% Autor: Przemyslaw Kobylanski <przemko@pwr.wroc.pl>
%
% Historia kolejnych wersji:
%
% 3 dopisano sprawdzenie czy formula jest zamknieta (2012.11.22)
% 2 zmieniono skladnie formul i poprawiono build_table/6 (2012.11.16)
% 1 pierwsza dzialajaca wersja (2012.11.15)
%
% TODO:
%
% null
%
:- module(tabsem, [prove/3, model/4, prove/4, model/5]).

version(3).

:- use_module(library(ordsets)).

% prove(+ClosedFormula, -Answer, -Tree)
%
prove(A, Answer, T) :-
	build_tree(not(A), T, S),
	(   S = closed
	->  Answer = true
	;   Answer = not(true)).

% model(+ClosedFormula, -Answer, -Tree, -Model)
%
model(A, Answer, T, M) :-
	build_tree(A, T, S), !,
	(   S = closed
	->  Answer = false,
	    M = []
	;   Answer = not(false),
	    interpretation(T, M)).

% prove(+Limit, +ClosedFormula, -Answer, -Tree)
%
prove(L, A, Answer, T) :-
	build_tree(L, not(A), T, S),
	(   S = closed
	->  Answer = true
	;   (   S = open
	    ->	Answer = not(true)
	    ;	Answer = unknown)).

% model(+Limit, +ClosedFormula, -Answer, -Tree, -Model)
%
model(L, A, Answer, T, M) :-
	build_tree(L, A, T, S), !,
	(   S = closed
	->  Answer = false,
	    M = []
	;   (   S = open
	    ->	Answer = not(false),
		interpretation(T, M)
	    ;	Answer = unknown,
		M = [])).

% build_tree(+ClosedFormula, -Tree, -Status)
%
build_tree(A, T, S) :-
	closed(A),
	constants(A, C1),
	(   C1 = []
	->  C = [0]
	;   C = C1),
	build_table([A], [A], C, T, S).

% build_tree(+Limit, +ClosedFormula, -Tree, -Status)
%
build_tree(L, A, T, S) :-
	closed(A),
	constants(A, C1),
	(   C1 = []
	->  C = [0]
	;   C = C1),
	build_table(L, [A], [A], C, T, S).


% build_table(+ListOfClosedForms, +History, +ListOfConstants,
%             -Tree, -Status)
%
build_table(Forms, _, _, closed(Forms), closed) :-
	compl(Forms), !.
build_table(Forms, Past, Constants, tree(Forms, alpha(A), Son), Status) :-
	select(A, Forms, Rest),
	alpha(A, A1), !,
	ord_union([A1], Rest, NewForms),
	ord_union([A1], Past, Present),
	build_table(NewForms, Present, Constants, Son, Status).
build_table(Forms, Past, Constants, tree(Forms, alpha(A), Son), Status) :-
	select(A, Forms, Rest),
	alpha(A, A1, A2), !,
	ord_union([A1, A2], Rest, NewForms),
	ord_union([A1, A2], Past, Present),
	build_table(NewForms, Present, Constants, Son, Status).
build_table(Forms, Past, Constants, tree(Forms, beta(A), Son1, Son2), Status) :-
	select(A, Forms, Rest),
	beta(A, A1, A2), !,
	ord_union([A1], Rest, NewForms1),
	ord_union([A2], Rest, NewForms2),
	ord_union([A1], Past, Present1),
	ord_union([A2], Past, Present2),
	build_table(NewForms1, Present1, Constants, Son1, Status1),
	build_table(NewForms2, Present2, Constants, Son2, Status2),
	status(Status1, Status2, Status).
build_table(Forms, Past, Constants, tree(Forms, delta(A), Son), Status) :-
	select(A, Forms, Rest),
	is_delta(A), !,
	copy_term(A, B),
	delta(B, X, B1),
	new_constant(Constants, C),
	X = C,
	ord_union([B1], Rest, NewForms),
	ord_union([B1], Past, Present),
	build_table(NewForms, Present, [C | Constants], Son, Status).
build_table(Forms, Past, Constants, Tree, Status) :-
	gammas(Forms, Gammas, Rest), !,
	copy_term(Gammas, NewGammas),
	populate(NewGammas, Constants, NewForms1),
	ord_subtract(NewForms1, Past, NewForms2),
	%my_ord_subtract(NewForms1, Past, NewForms2),
	(   NewForms2 = []
	->  Tree = open(Forms, Constants),
	    Status = open
	;   Tree = tree(Forms, gamma(Gammas, Constants), Son),
	    ord_union(NewForms2, Rest, NewForms3),
	    ord_union(Gammas, NewForms3, NewForms),
	    ord_union(NewForms2, Past, Present),
	    build_table(NewForms, Present, Constants, Son, Status)).

% build_table(+Limit, +History, +ListOfClosedForms, +ListOfConstants,
%             -Tree, -Status)
%
build_table(_, Forms, _, _, closed(Forms), closed) :-
	compl(Forms), !.
build_table(Limit, Forms, Past, Constants, tree(Forms, alpha(A), Son), Status) :-
	select(A, Forms, Rest),
	alpha(A, A1), !,
	ord_union([A1], Rest, NewForms),
	ord_union([A1], Past, Present),
	build_table(Limit, NewForms, Present, Constants, Son, Status).
build_table(Limit, Forms, Past, Constants, tree(Forms, alpha(A), Son), Status) :-
	select(A, Forms, Rest),
	alpha(A, A1, A2), !,
	ord_union([A1, A2], Rest, NewForms),
	ord_union([A1, A2], Past, Present),
	build_table(Limit, NewForms, Present, Constants, Son, Status).
build_table(Limit, Forms, Past, Constants, tree(Forms, beta(A), Son1, Son2),
	    Status) :-
	select(A, Forms, Rest),
	beta(A, A1, A2), !,
	ord_union([A1], Rest, NewForms1),
	ord_union([A2], Rest, NewForms2),
	ord_union([A1], Past, Present1),
	ord_union([A2], Past, Present2),
	build_table(Limit, NewForms1, Present1, Constants, Son1, Status1),
	build_table(Limit, NewForms2, Present2, Constants, Son2, Status2),
	status(Status1, Status2, Status).
build_table(Limit, Forms, Past, Constants, tree(Forms, delta(A), Son), Status) :-
	select(A, Forms, Rest),
	is_delta(A), !,
	copy_term(A, B),
	delta(B, X, B1),
	new_constant(Constants, C),
	X = C,
	ord_union([B1], Rest, NewForms),
	ord_union([B1], Past, Present),
	build_table(Limit, NewForms, Present, [C | Constants], Son, Status).
build_table(Limit, Forms, Past, Constants, Tree, Status) :-
	gammas(Forms, Gammas, Rest), !,
	copy_term(Gammas, NewGammas),
	populate(NewGammas, Constants, NewForms1),
	ord_subtract(NewForms1, Past, NewForms2),
	(   NewForms2 = []
	->  Tree = open(Forms, Constants),
	    Status = open
	;   (    Limit > 0
	    ->	Limit1 is Limit - 1,
		ord_union(NewForms2, Rest, NewForms3),
		 ord_union(Gammas, NewForms3, NewForms),
		 ord_union(NewForms2, Past, Present),
		 Tree = tree(Forms, gamma(Gammas, Constants), Son),
		 build_table(Limit1, NewForms, Present, Constants, Son, Status)
	    ;   Tree = unknown(Forms),
		Status = unknown)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%	R U L E S
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

alpha(not(not(A)), A).

alpha(and(A1, A2), A1, A2).
alpha(not(or(A1, A2)), not(A1), not(A2)).
alpha(not(imp(A1, A2)), A1, not(A2)).
alpha(equ(A1, A2), imp(A1, A2), imp(A2, A1)).

beta(or(A1, A2), A1, A2).
beta(not(and(A1, A2)), not(A1), not(A2)).
beta(imp(A1, A2), not(A1), A2).
beta(not(equ(A1, A2)), not(imp(A1, A2)), not(imp(A2, A1))).

gamma(all(X, A), X, A).
gamma(not(ex(X, A)), X, not(A)).

delta(ex(X, A), X, A).
delta(not(all(X, A)), X, not(A)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%	U T I L I T I E S
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

interpretation(open(Forms, Domain), model(Domain, PositiveLiterals)) :-
	gammas(Forms, _, Literals),
	positive(Literals, PositiveLiterals).
interpretation(tree(_, _, T), L) :-
	interpretation(T, L).
interpretation(tree(_, _, T1, T2), L) :-
	(   interpretation(T1, L)
	;   interpretation(T2, L)).

positive([], []).
positive([not(_) | L1], L2) :-
	!,
	positive(L1, L2).
positive([A | L1], [A | L2]) :-
	positive(L1, L2).

populate([], _, []).
populate([A | L], C, F) :-
	populate2(C, A, F1a),
	list_to_ord_set(F1a, F1),
	populate(L, C, F2),
	ord_union(F1, F2, F).

populate2([], _, []).
populate2([C | L1], A, [B1 | L2]) :-
	copy_term(A, B),
	gamma(B, X, B1),
	X = C,
	populate2(L1, A, L2).

gammas([], [], []).
gammas([A | L], [A | L1], L2) :-
	is_gamma(A), !,
	gammas(L, L1, L2).
gammas([A | L], L1, [A | L2]) :-
	gammas(L, L1, L2).

is_delta(ex(_, _)).
is_delta(not(all(_, _))).

is_gamma(all(_, _)).
is_gamma(not(ex(_, _))).

new_constant(X, C) :-
	repeat,
	nat(C),
	\+ member(C, X), !.

nat(0).
nat(N) :-
	nat(N1),
	N is N1+1.

status(closed, closed, closed) :- !.
status(open, _, open) :- !.
status(_, open, open) :- !.
status(_, _, unknown).

closed(A) :-
	closed(A, []).

closed(A, _) :-
	var(A),
	!, fail.
closed(not(A1), L) :- !,
	closed(A1, L).
closed(and(A1, A2), L) :- !,
	closed(A1, L),
	closed(A2, L).
closed(or(A1, A2), L) :- !,
	closed(A1, L),
	closed(A2, L).
closed(imp(A1, A2), L) :- !,
	closed(A1, L),
	closed(A2, L).
closed(equ(A1, A2), L) :- !,
	closed(A1, L),
	closed(A2, L).
closed(all(X, A), L) :- !,
	closed(A, [X | L]).
closed(ex(X, A), L) :- !,
	closed(A, [X | L]).
closed(A, L) :-
	A =.. [_ | Args],
	closed2(Args, L).

closed2([], _).
closed2([A | L1], L) :-
	atom(A), !,
	closed2(L1, L).
closed2([A | L1], L) :-
	var(A),
	find_var(L, A),
	closed2(L1, L).

find_var([X | _], A) :-
	X == A, !.
find_var([_ | L], A) :-
	find_var(L, A).


constants(not(A), C) :-
	!,
	constants(A, C).
constants(and(A1, A2), C) :-
	!,
	constants(A1, C1),
	constants(A2, C2),
	ord_union(C1, C2, C).
constants(or(A1, A2), C) :-
	!,
	constants(A1, C1),
	constants(A2, C2),
	ord_union(C1, C2, C).
constants(imp(A1, A2), C) :-
	!,
	constants(A1, C1),
	constants(A2, C2),
	ord_union(C1, C2, C).
constants(equ(A1, A2), C) :-
	!,
	constants(A1, C1),
	constants(A2, C2),
	ord_union(C1, C2, C).
constants(all(_, A), C) :-
	!,
	constants(A, C).
constants(ex(_, A), C) :-
	!,
	constants(A, C).
constants(P, []) :-
	atom(P), !.
constants(P, C) :-
	P =.. [_ | Args],
	constants2(Args, C).

constants2([], []).
constants2([A | L], C) :-
	var(A), !,
	constants2(L, C).
constants2([A | L], C) :-
	atom(A), !,
	constants2(L, C1),
	ord_union([A], C1, C).

compl(Forms) :-
	append(_, [A1 | L], Forms),
	literal(A1),
	member(A2, L),
	literal(A2),
	compl(A1, A2).

literal(not(A)) :-
	!,
	atom_formula(A).
literal(A) :-
	atom_formula(A).

atom_formula(not(_)) :-
	!, fail.
atom_formula(and(_, _)) :-
	!, fail.
atom_formula(or(_, _)) :-
	!, fail.
atom_formula(imp(_, _)) :-
	!, fail.
atom_formula(equ(_, _)) :-
	!, fail.
atom_formula(all(_, _)) :-
	!, fail.
atom_formula(ex(_, _)) :-
	!, fail.
atom_formula(_).

compl(not(A), A).
compl(A, not(A)).

leaf([]).
leaf([A | L]) :-
	literal(A), !,
	leaf(L).
leaf([A | L]) :-
	gamma(A, _, _), !,
	leaf(L).


