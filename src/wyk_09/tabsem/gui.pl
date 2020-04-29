% gui.pl
%
% Interfejs graficzny dla modulu tabsem.
%
% Eksportowane predykaty:
%
%   make_window/0 tworzenie okna aplikacji i komunikacja z modulem
%                 tabsem.pl
%
% Importowane moduly:
%
%   tabsem  metoda tabel semantycznych dla rachunku predykatow
%   pce     biblioteka XPCE do tworzenia interfejsu graficznego
%
% Autor: Przemyslaw Kobylanski <przemko@pwr.wroc.pl>
%
% Historia kolejnych wersji:
%
% 9 dodano dymki z objasnieniami wezlow drzewa (2012.11.22)
% 8 dodano reakcje na zla skladnie formuly (2012.11.22)
% 7 dodano dymki z pomoca (2012.11.20)
% 6 prezentacja wezlow w postaci wycentrowanych formul (2012.11.18)
% 5 dodano eksport drzewa w formacie Postscript (2012.11.17)
% 4 dodano predykaty fix_vars/[2,3] (2012.11.17)
% 3 dodano przyklady (2012.11.17)
% 2 dodano rysowanie drzewa dla konstrukcji modelu (2012.11.16)
% 1 dodano rysowanie drzewa dla dowodu (2012.11.16)
% 0 poczatek pracy nad interfejsem (2012.11.16)
%
% TODO
%
% - dopisac odczytywanie relacji w predykacie model_to_atom/3
%

:- module(gui, [make_window/1]).

:- use_module(library(pce)).
:- use_module(library(help_message)).

version(9).

:- use_module(tabsem).

make_window(_Arg) :-
	new(Dialog1, dialog),
	new(Picture, picture),
	new(Dialog2, dialog),
	tabsem:version(VER1),
	version(VER2),
	atomic_list_concat(
	    ['Method of analytic tableaux for the first order logic ',
	     '(Version ', VER1, '.', VER2, ')'], Title),
	new(Window, frame(Title)),
	send(Window, append, Dialog1),
	send(Picture, below, Dialog1),
	send(Dialog2, below, Picture),
	make_dialog2(Dialog2, Status),
	make_dialog1(Dialog1, Picture, Status, Window),
	make_picture(Picture),
	send(Window, open).

make_dialog1(Dialog, Picture, Status, Window) :-
	new(Formula, text_item(formula, '')),
	send(Formula, help_message, tag, 'Input formula'),
	send(Formula, value_font, font(courier, roman, 14)),
	send(Formula, width, 120),
	new(Examples, menu(examples, cycle)),
	send(Examples, help_message, tag, 'Select formula from examples'),
	send(Examples, value_font, font(courier, roman, 14)),
	send(Examples, message, message(Formula, selection, Examples?selection)),
	fill_examples(Examples),
	new(Slider, slider(limit, 0, 10, 0)),
	send(Slider, help_message, tag, 'Select limit'),
	new(Inc, button('+1', message(Slider, selection, 1+Slider?selection))),
	send(Inc, help_message, tag, 'Icrease limit'),
	new(Postscript,
	    button(postscript,
		   message(@prolog, save_postscript, Picture))),
	send(Postscript, help_message, tag, 'Save tree in EPS file'),
	send(Postscript, active, off),
	new(Prove,
	    button(prove,
		   message(@prolog, start_prove,
			   Formula, Slider, Picture, Status, Postscript))),
	send(Prove, help_message, tag, 'Prove formula'),
	new(Model,
	    button(model,
		   message(@prolog, start_model,
			   Formula, Slider, Picture, Status, Postscript))),
	send(Model, help_message, tag, 'Search for model'),
	new(About, button(about)),
	send(About, help_message, tag, 'About application'),
	send(About, message, message(@prolog, open_about, About)),
	new(Quit, button(quit, message(Window, destroy))),
	send(Quit, help_message, tag, 'Close window and quit application'),
	send(Dialog, append, Formula),
	send(Examples, below, Formula),
	send(Slider, below, Examples),
	send(Inc, right, Slider),
	send(Prove, below, Slider),
	send(Model, right, Prove),
	send(Postscript, right, Model),
	send(About, right, Postscript),
	send(Quit, right, About).

fill_examples(E) :-
	send(E, append, 'equ(p, not(not(p)))'),
	send(E, append, 'equ(p, and(p, p))'),
	send(E, append, 'equ(p, or(p, p))'),
	send(E, append, 'equ(or(p, q), or(q, p))'),
	send(E, append, 'equ(and(p, q), and(q, p))'),
	send(E, append, 'equ(equ(p, q), equ(q, p))'),
	send(E, append, 'equ(imp(p, q), or(not(p), q))'),
	send(E, append, 'equ(imp(p, q), imp(not(q), not(p)))'),
	send(E, append, 'equ(or(p, or(q, r)), or(or(p, q), r))'),
	send(E, append, 'equ(and(p, and(q, r)), and(and(p, q), r))'),
	send(E, append, 'equ(equ(p, equ(q, r)), equ(equ(p, q), r))'),
	send(E, append, 'equ(or(p, and(q, r)), and(or(p, q), or(p, r)))'),
	send(E, append, 'equ(and(p, or(q, r)), or(and(p, q), and(p, r)))'),
	send(E, append, 'equ(and(p, or(p, q)), p)'),
	send(E, append, 'equ(or(p, and(p, q)), p)'),
	send(E, append, 'equ(equ(equ(p, q), q), p)'),
	send(E, append, 'equ(all(X, p(X)), not(ex(X, not(p(X)))))'),
	send(E, append, 'equ(ex(X, p(X)), not(all(X, not(p(X)))))'),
	send(E, append, 'equ(all(X, all(Y, p(X, Y))), all(Y, all(X, p(X, Y))))'),
	send(E, append, 'equ(ex(X, ex(Y, p(X, Y))), ex(Y, ex(X, p(X, Y))))'),
	send(E, append, 'imp(ex(X, all(Y, p(X, Y))), all(Y, ex(X, p(X, Y))))'),
	send(E, append, 'equ(or(ex(X, p(X)), q), ex(X, or(p(X), q)))'),
	send(E, append, 'equ(or(all(X, p(X)), q), all(X, or(p(X), q)))'),
	send(E, append, 'equ(and(ex(X, p(X)), q), ex(X, and(p(X), q)))'),
	send(E, append, 'equ(and(all(X, p(X)), q), all(X, and(p(X), q)))'),
	send(E, append, 'equ(or(q, ex(X, p(X))), ex(X, or(q, p(X))))'),
	send(E, append, 'equ(or(q, all(X, p(X))), all(X, or(q, p(X))))'),
	send(E, append, 'equ(and(q, ex(X, p(X))), ex(X, and(q, p(X))))'),
	send(E, append, 'equ(and(q, all(X, p(X))), all(X, and(q, p(X))))'),
	send(E, append, 'equ(all(X, imp(q, p(X))), imp(q, all(X, p(X))))'),
	send(E, append, 'equ(all(X, imp(p(X), q)), imp(ex(X, p(X)), q))'),
	send(E, append, 'equ(ex(X, or(p(X), q(X))), or(ex(X, p(X)), ex(X, q(X))))'),
	send(E, append, 'equ(all(X, and(p(X), q(X))), and(all(X, p(X)), all(X, q(X))))'),
	send(E, append, 'imp(or(all(X, p(X)), all(X, q(X))), all(X, or(p(X), q(X))))'),
	send(E, append, 'imp(ex(X, and(p(X), q(X))), and(ex(X, p(X)), ex(X, q(X))))'),
	send(E, append, 'imp(all(X, equ(p(X), q(X))), equ(all(X, p(X)), all(X, q(X))))'),
	send(E, append, 'imp(all(X, equ(p(X), q(X))), equ(ex(X, p(X)), ex(X, q(X))))'),
	send(E, append, 'equ(ex(X, imp(p(X), q(X))), imp(all(X, p(X)), ex(X, q(X))))'),
	send(E, append, 'imp(imp(ex(X, p(X)), all(X, q(X))), all(X, imp(p(X), q(X))))'),
	send(E, append, 'imp(all(X, or(p(X), q(X))), or(all(X, p(X)), ex(X, q(X))))'),
	send(E, append, 'imp(all(X, imp(p(X), q(X))), imp(all(X, p(X)), all(X, q(X))))'),
	send(E, append, 'imp(all(X, imp(p(X), q(X))), imp(ex(X, p(X)), ex(X, q(X))))'),
	send(E, append, 'imp(all(X, imp(p(X), q(X))), imp(all(X, p(X)), ex(X, q(X))))').

start_prove(Formula, Slider, Picture, Status, Postscript) :-
	send(Status, selection, ''),
	send(Picture, clear),
	send(Postscript, active, @off),
	get(Formula, selection, AForm),
	get(Slider, selection, Limit),
	catch(atom_to_term(AForm, Form, _), _,
	      (	  send(Status, selection, 'Syntax error'),
		  fail)),
	send(Postscript, active, off),
	send(Status, selection, 'Looking for the proof...'),
	send(@display, dispatch),
	(   prove(Limit, Form, ANS, TREE)
	->  draw_tree(TREE, Picture),
	    info_prove(ANS, Status),
	    send(Postscript, active, @on)
	;   send(Status, selection,
		 'Formula is not closed or some atomic subformula has wrong argument (numeric or compound).')).

info_prove(true, Status) :-
	send(Status, selection,
	     'Formula is true. All branches are closed (red).').
info_prove(not(true), Status) :-
	send(Status, selection,
	     'Formula is not true. There is an open leaf (green) for its negation.').
info_prove(unknown, Status) :-
	send(Status, selection,
	     'Some branches were pruned (yellow leaves). Increase limit and try again.').

start_model(Formula, Slider, Picture, Status, Postscript) :-
	send(Status, selection, ''),
	send(Picture, clear),
	send(Postscript, active, @off),
	get(Formula, selection, AForm),
	get(Slider, selection, Limit),
	catch(atom_to_term(AForm, Form, _), _,
	      (	  send(Status, selection, 'Syntax error'),
		  fail)),
	send(Status, selection, 'Looking for the model...'),
	send(@display, dispatch),
	(   model(Limit, Form, ANS, TREE, _)
	->  draw_tree(TREE, Picture),
	    info_model(ANS, Status),
	    send(Postscript, active, @on)
	;   send(Status, selection,
		 'Formula is not closed or some atomic subformula has wrong argument (numeric or compound).')).

info_model(false, Status) :-
	send(Status, selection,
	     'Formula is false and has no model. All branches are closed (red).').
info_model(not(false), Status) :-
	send(Status, selection,
	     'Formula is not false. There is an open leaf (green) with a model.').
info_model(unknown, Status) :-
	send(Status, selection,
	     'Some branches were pruned (yellow leaves). Increase limit and try again.').

:- pce_autoload(finder, library(find_file)).
:- pce_global(@finder, new(finder)).

save_postscript(Picture) :-
	get(@finder, file, @off, '.eps', FileName),
        new(File, file(FileName)),
        send(File, open, write),
        send(File, append, Picture?postscript),
        send(File, close),
        send(File, done).

% kolor RED lisc zamkniety
% kolor GREEN lisc otwarty
% kolor YELLOW przerwano eksploracje galezi
%

draw_tree(TREE, Picture) :-
	make_tree(TREE, Root),
	new(Tree, tree(Root)),
	send(Tree, recogniser, new(move_gesture)),
	send(Tree, direction, vertical),
	send(Tree, neighbour_gap, 10),
	send(Picture, display, Tree).

make_tree(unknown(Forms), Node) :-
	term_to_atom2(Forms, Atom),
	new(Node, node(text(Atom, center))),
	send(Node, help_message, tag,
	     'Pruned branch (increase limit and try again)'),
	send(Node, colour, yellow).
make_tree(closed(Forms), Node) :-
	term_to_atom2(Forms, Atom),
	new(Node, node(text(Atom, center))),
	send(Node, help_message, tag, 'Closed leaf'),
	send(Node, colour, darkred).
make_tree(open(Forms, Domain), Node) :-
	term_to_atom2(Forms, Atom),
	new(Node, node(text(Atom, center))),
	model_to_atom(Domain, Forms, Atom2),
	send(Node, help_message, tag, Atom2),
	send(Node, colour, darkgreen).
make_tree(tree(Forms, Rule, SubTree), Node) :-
	term_to_atom2(Forms, Atom),
	new(Node, node(text(Atom, center))),
	rule_to_atom(Rule, Atom2),
	send(Node, help_message, tag, Atom2),
	make_tree(SubTree, Son),
	send(Node, son, Son).
make_tree(tree(Forms, Rule, SubTree1, SubTree2), Node) :-
	term_to_atom2(Forms, Atom),
	new(Node, node(text(Atom, center))),
	rule_to_atom(Rule, Atom2),
	send(Node, help_message, tag, Atom2),
	make_tree(SubTree1, Son1),
	make_tree(SubTree2, Son2),
	send(Node, son, Son1),
	send(Node, son, Son2).

term_to_atom2(Forms, Atom) :-
	copy_term(Forms, Copy),
	fix_vars(Copy, 1),
	term_to_atom3(Copy, Atom).

term_to_atom3([], '').
term_to_atom3([F], A) :-
	term_to_atom(F, A).
term_to_atom3([F1, F2 | L], A) :-
	term_to_atom(F1, A1),
	term_to_atom3([F2 | L], A2),
	atomic_list_concat([A1, '\n', A2], A).

fix_vars([], _).
fix_vars([F | L], N) :-
	fix_vars(F, N, N1),
	fix_vars(L, N1).

fix_vars(all(X, F), N, N2) :-
	var(X),
	!,
	atom_concat(x, N, X),
	N1 is N+1,
	fix_vars(F, N1, N2).
fix_vars(all(_, F), N, N1) :-
	!,
	fix_vars(F, N, N1).
fix_vars(ex(X, F), N, N2) :-
	var(X),
	!,
	atom_concat(x, N, X),
	N1 is N+1,
	fix_vars(F, N1, N2).
fix_vars(ex(_, F), N, N1) :-
	!,
	fix_vars(F, N, N1).
fix_vars(not(F), N, N1) :-
	!,
	fix_vars(F, N, N1).
fix_vars(and(F1, F2), N, N2) :-
	!,
	fix_vars(F1, N, N1),
	fix_vars(F2, N1, N2).
fix_vars(or(F1, F2), N, N2) :-
	!,
	fix_vars(F1, N, N1),
	fix_vars(F2, N1, N2).
fix_vars(imp(F1, F2), N, N2) :-
	!,
	fix_vars(F1, N, N1),
	fix_vars(F2, N1, N2).
fix_vars(equ(F1, F2), N, N2) :-
	!,
	fix_vars(F1, N, N1),
	fix_vars(F2, N1, N2).
fix_vars(_, N, N).

rule_to_atom(alpha(_), Atom) :-
	Atom = 'Rule alpha'.
rule_to_atom(beta(_), Atom) :-
	Atom = 'Rule beta'.
rule_to_atom(delta(_), Atom) :-
	Atom = 'Rule delta'.
rule_to_atom(gamma(_, _), Atom) :-
	Atom = 'Rule gamma'.


model_to_atom(Domain, _Literals, Atom) :-
	sort(Domain, Sorted),
	term_to_atom(Sorted, Atom1),
	atomic_list_concat(['D = ', Atom1, '\n', 'tu beda relacje'], Atom).

open_about(About) :-
	send(About, active, no),
	new(Dialog, dialog(about)),
	tabsem:version(VER1),
	version(VER2),
	atomic_list_concat(
	    ['Method of analytic tableaux for the first order logic ',
	     '(Version ', VER1, '.', VER2, ')'], Text),
	send(Dialog, append,
	     label(lab1, Text)),
	send(Dialog, append,
	     label(
		 lab2,
		 'Copyright (c) 2012 Przemyslaw Kobylanski, Wroclaw University of Technology')),
	new(Close, button(close, message(@prolog, close_about, About, Dialog))),
	send(Close, help_message, tag, 'Close window'),
	send(Dialog, append, Close),
	send(Dialog, open).

close_about(About, Dialog) :-
	send(About, active, yes),
	send(Dialog, destroy).

make_picture(Picture) :-
	send(Picture, background, colour(darkgray)).
	%send(Picture, help_message, tag, 'Canvas for drawing').

make_dialog2(Dialog, Status) :-
	new(Status, label),
	send(Status, help_message, tag, 'Status line'),
	send(Status, font, font(courier, roman, 14)),
	send(Dialog, append, Status),
	send(Status, selection, '').




