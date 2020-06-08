:- module(pseudocode, [
              file_parser/4,
              parser/4]).

% Pseudokod przypomina jezyk Ada ale nie ma pustej instrukcji null.
%
% BLOCK =
% BLCOK = STATEMENT; BLOCK
%
% STATEMENT = ID := EXPR
% STATEMENT = if COND then BLOCK end if
% STATEMENT = if COND then BLOCK else BLOCK end if
% STATEMENT = while COND loop BLOCK end loop
%
% EXPR = ID
% EXPR = INT
% EXPR = EXPR + EXPR
% EXPR = EXPR - EXPR
% EXPR = EXPR * EXPR
% EXPR = EXPR div EXPR
% EXPR = EXPR mod EXPR
% EXPR = EXPR rem EXPR
%
% COND = EXPR = EXPR
% COND = EXPR /= EXPR
% COND = EXPR < EXPR
% COND = EXPR > EXPR
% COND = EXPR >= EXPR
% COND = EXPR <= EXPR
% COND = COND and COND
% COND = COND or COND
% COND = not COND
%

file_parser(FileName, PRE, BLOCK, POST) :-
    read_file_to_codes(FileName, TXT, [extensions(['.pc'])]),
    parser(TXT, PRE, BLOCK, POST).

parser(Text, Pre, Block, Post) :-
    phrase(lex(Tokens), Text),
    phrase(program(Pre, Block, Post), Tokens).

% L E X E R
%
lex([]) --> ``.
lex(Tokens) --> white, !, lex(Tokens).
lex([Token | Tokens]) --> alpha(Word), {key_id(Word, Token), !}, lex(Tokens).
lex([int(N) | Tokens]) --> int(N), !, lex(Tokens).
lex([symb(is) | Tokens]) --> `:=`, lex(Tokens).
lex([symb('(') | Tokens]) --> `(`, lex(Tokens).
lex([symb(')') | Tokens]) --> `)`, lex(Tokens).
lex([symb(;) | Tokens]) --> `;`, lex(Tokens).
lex([symb(+) | Tokens]) --> `+`, lex(Tokens).
lex([symb(-) | Tokens]) --> `-`, lex(Tokens).
lex([symb(*) | Tokens]) --> `*`, lex(Tokens).
lex([symb(=<) | Tokens]) --> `<=`, lex(Tokens).
lex([symb(>=) | Tokens]) --> `>=`, lex(Tokens).
lex([symb(=\=) | Tokens]) --> `/=`, lex(Tokens).
lex([symb(<) | Tokens]) --> `<`, lex(Tokens).
lex([symb(>) | Tokens]) --> `>`, lex(Tokens).
lex([symb(=:=) | Tokens]) --> `=`, lex(Tokens).

white --> ` `.
white --> `\t`.
white --> `\n`.
white --> `--`, drop.

alpha(Word) --> letter(L), alpha(L, Word).

alpha(Word1, Word) --> letter(L), {atom_concat(Word1, L, Word2), !}, alpha(Word2, Word).
alpha(Word, Word) --> ``.

key_id(if, key(if)) :- !.
key_id(then, key(then)) :- !.
key_id(else, key(else)) :- !.
key_id(end, key(end)) :- !.
key_id(while, key(while)) :- !.
key_id(loop, key(loop)) :- !.
key_id(and, key(and)) :- !.
key_id(or, key(or)) :- !.
key_id(not, key(not)) :- !.
key_id(true, key(true)) :- !.
key_id(pre, key(pre)) :- !.
key_id(post, key(post)) :- !.
key_id(div, key(div)) :- !.
key_id(mod, key(mod)) :- !.
key_id(rem, key(rem)) :- !.
key_id(X, id(X)).

int(N) --> digit(D), int2(D, N).

int2(I, N) --> digit(D), {I1 is 10*I + D, !}, int2(I1, N).
int2(N, N) --> ``.

digit(0) --> `0`.
digit(1) --> `1`.
digit(2) --> `2`.
digit(3) --> `3`.
digit(4) --> `4`.
digit(5) --> `5`.
digit(6) --> `6`.
digit(7) --> `7`.
digit(8) --> `8`.
digit(9) --> `9`.

letter(a) --> `a`.
letter(b) --> `b`.
letter(c) --> `c`.
letter(d) --> `d`.
letter(e) --> `e`.
letter(f) --> `f`.
letter(g) --> `g`.
letter(h) --> `h`.
letter(i) --> `i`.
letter(j) --> `j`.
letter(k) --> `k`.
letter(l) --> `l`.
letter(m) --> `m`.
letter(n) --> `n`.
letter(o) --> `o`.
letter(p) --> `p`.
letter(q) --> `q`.
letter(r) --> `r`.
letter(s) --> `s`.
letter(t) --> `t`.
letter(u) --> `u`.
letter(v) --> `v`.
letter(w) --> `w`.
letter(x) --> `x`.
letter(y) --> `y`.
letter(z) --> `z`.

drop --> `\n`, !.
drop --> [_], drop, !.
drop --> ``.

% P A R S E R
%

program(Pre, Block, Post) -->
    [key(pre)], condition(Pre), [symb(;)],
    block(Block),
    [key(post)], condition(Post), [symb(;)].

block([ST | BL]) -->
    statement(ST), [symb(;)],
    block(BL), !.
block([]) --> [].

statement(ID is EXPR) -->
    [id(ID), symb(is)], expression(EXPR).
statement(if(COND, BL)) -->
    [key(if)], condition(COND), [key(then)],
    block(BL),
    [key(end), key(if)].
statement(if(COND, BL1, BL2)) -->
    [key(if)], condition(COND), [key(then)],
    block(BL1),
    [key(else)],
    block(BL2),
    [key(end), key(if)].
statement(while(COND, BL)) -->
    [key(while)], condition(COND), [key(loop)],
    block(BL),
    [key(end), key(loop)].

expression(S + E) --> skladnik(S), [symb(+)], expression(E).
expression(S - E) --> skladnik(S), [symb(-)], expression(E).
expression(S) --> skladnik(S).

skladnik(F * S) --> czynnik(F), [symb(*)], skladnik(S).
skladnik(F div S) --> czynnik(F), [key(div)], skladnik(S).
skladnik(F mod S) --> czynnik(F), [key(mod)], skladnik(S).
skladnik(F rem S) --> czynnik(F), [key(rem)], skladnik(S).
skladnik(F) --> czynnik(F).

czynnik(ID) --> [id(ID)].
czynnik(N) --> [int(N)].
czynnik(E) --> [symb('(')], expression(E), [symb(')')].


condition(not(S)) --> [key(not)], skladnik_logiczny(S).
condition(or(S, C)) --> skladnik_logiczny(S), [key(or)], condition(C).
condition(C) --> relation(C).
condition(S) --> skladnik_logiczny(S).

skladnik_logiczny(and(F, S)) --> czynnik_logiczny(F), [key(and)], skladnik_logiczny(S).
skladnik_logiczny(F) --> czynnik_logiczny(F).

czynnik_logiczny(true) --> [key(true)].
czynnik_logiczny(F) --> [symb('(')], condition(F), [symb(')')].

relation(A =:= B) --> expression(A), [symb(=:=)], expression(B).
relation(A =\= B) --> expression(A), [symb(=\=)], expression(B).
relation(A < B) --> expression(A), [symb(<)], expression(B).
relation(A > B) --> expression(A), [symb(>)], expression(B).
relation(A =< B) --> expression(A), [symb(=<)], expression(B).
relation(A >= B) --> expression(A), [symb(>=)], expression(B).





