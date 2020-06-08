% refute.pl
%
% (c) 2020 Przemysław Kobylański - All rights reserved
%
:- module(refute, [
              refute_with_inference_limit/6,
              refute_with_inference_limit/7,
              refute_with_inference_limit/8,
              refute_with_time_limit/5,
              refute_with_time_limit/6,
              refute_with_time_limit/7,
              refute/4,
              refute/5,
              refute/6]).

:- use_module(library(clpfd)).

min_int(-128).

max_int(127).

%      BLOCK ::= []
%      BLOCK ::= [COMMAND | BLOCK]
%    COMMAND ::= RIGHT is EXPRESSION
%    COMMAND ::= if(CONDITION, BLOCK)
%    COMMAND ::= if(CONDITION, BLOCK, BLOCK)
%    COMMAND ::= while(CONDITION, BLOCK)
%  CONDITION ::= true
%  CONDITION ::= and(CONDITION, CONDITION)
%  CONDITION ::= or(CONDITION, CONDITION)
%  CONDITION ::= not(CONDITION)
%  CONDITION ::= EXPRESSION =:= EXPRESSION
%  CONDITION ::= EXPRESSION =\= EXPRESSION
%  CONDITION ::= EXPRESSION < EXPRESSION
%  CONDITION ::= EXPRESSION > EXPRESSION
%  CONDITION ::= EXPRESSION =< EXPRESSION
%  CONDITION ::= EXPRESSION >= EXPRESSION
% EXPRESSION ::= ID
% EXPRESSION ::= INT
% EXPRESSION ::= EXPRESSION + EXPRESSION
% EXPRESSION ::= EXPRESSION - EXPRESSION
% EXPRESSION ::= EXPRESSION * EXPRESSION
% EXPRESSION ::= EXPRESSION / EXPRESSION
%         ID ::= ATOM
% ...

refute_with_inference_limit(PRE, BLOCK, POST,
                            LIMIT, ASSOC, RESULT) :-
        call_with_inference_limit(refute(PRE, BLOCK, POST, ASSOC),
                                  LIMIT,
                                  RESULT).

refute_with_inference_limit(PRE, BLOCK, POST,
                            LIMIT, ASSOC, PATH, RESULT) :-
        call_with_inference_limit(refute(PRE, BLOCK, POST, ASSOC, PATH),
                                  LIMIT,
                                  RESULT).

refute_with_inference_limit(PRE, BLOCK, POST,
                            LIMIT, ASSOC, PATH, CONSTR, RESULT) :-
        call_with_inference_limit(refute(PRE, BLOCK, POST, ASSOC, PATH, CONSTR),
                                  LIMIT,
                                  RESULT).

refute_with_time_limit(PRE, BLOCK, POST,
                       LIMIT, ASSOC) :-
        wrapper(call_with_time_limit(LIMIT, refute(PRE, BLOCK, POST, ASSOC))).

refute_with_time_limit(PRE, BLOCK, POST,
                       LIMIT, ASSOC, PATH) :-
        wrapper(call_with_time_limit(LIMIT, refute(PRE, BLOCK, POST, ASSOC, PATH))).

refute_with_time_limit(PRE, BLOCK, POST,
                       LIMIT, ASSOC, PATH, CONSTR) :-
        wrapper(call_with_time_limit(LIMIT, refute(PRE, BLOCK, POST, ASSOC, PATH, CONSTR))).

wrapper(GOAL) :-
        catch(GOAL, _, fail).

% refute(+PRE, +BLOCK, +POST, -STATE)

refute(PRE, BLOCK, POST, STATE) :-
        refute(PRE, BLOCK, POST, STATE, _, _).

% refute(+PRE, +BLOCK, +POST, -STATE, -PATH)

refute(PRE, BLOCK, POST, STATE, PATH) :-
        refute(PRE, BLOCK, POST, STATE, PATH, _).

% refute(+PRE, +BLOCK, +POST, -STATE, -PATH, -WEAKEST_PRECONDITION)

refute(PRE, BLOCK, POST, ASSOC, PATH2, CONSTR) :-
        length(PATH, _N),
	%(N mod 100 =:= 0 -> write(try(path_length(N))), nl;   true),
        path(BLOCK, PATH),
        %write(try(path(PATH))), nl,
        append([assert(PRE) | PATH], [assert(not(POST))], PATH2),
        weakest_precondition(PATH2, CONSTR),
        %write(try(solve(CONSTR))), nl,
        solve(CONSTR, ASSOC).

% weakest_precondition(+PATH, -CONSTRAINT)

weakest_precondition([], true).
weakest_precondition([comment(_) | P], NEW) :-
        !,
        weakest_precondition(P, NEW).
weakest_precondition([assert(W) | P], and(W, OLD)) :-
        !,
        weakest_precondition(P, OLD).
weakest_precondition([R is E | P], NEW) :-
        weakest_precondition(P, OLD),
        subst(OLD, R, E, NEW).

% solve(+CONSTRAINT, -STATE)

solve(CONSTR, STATE) :-
        variables(CONSTR, [], STATE, [], VARS),
%        write(try(constraint(CONSTR))), nl,
        constraint(CONSTR, STATE),
%        write(trop(VARS)), nl,
        labeling([], VARS).

variables(true, ASSOC, ASSOC, VARS, VARS) :-
        !.
variables(and(A, B), ASSOC1, ASSOC, VARS1, VARS) :-
        !,
        variables(A, ASSOC1, ASSOC2, VARS1, VARS2),
        variables(B, ASSOC2, ASSOC, VARS2, VARS).
variables(or(A, B), ASSOC1, ASSOC, VARS1, VARS) :-
        !,
        variables(A, ASSOC1, ASSOC2, VARS1, VARS2),
        variables(B, ASSOC2, ASSOC, VARS2, VARS).
variables(not(A), ASSOC1, ASSOC, VARS1, VARS) :-
        !,
        variables(A, ASSOC1, ASSOC, VARS1, VARS).
variables(A =:= B, ASSOC1, ASSOC, VARS1, VARS) :-
        !,
        variables2(A, ASSOC1, ASSOC2, VARS1, VARS2),
        variables2(B, ASSOC2, ASSOC, VARS2, VARS).
variables(A =\= B, ASSOC1, ASSOC, VARS1, VARS) :-
        !,
        variables2(A, ASSOC1, ASSOC2, VARS1, VARS2),
        variables2(B, ASSOC2, ASSOC, VARS2, VARS).
variables(A < B, ASSOC1, ASSOC, VARS1, VARS) :-
        !,
        variables2(A, ASSOC1, ASSOC2, VARS1, VARS2),
        variables2(B, ASSOC2, ASSOC, VARS2, VARS).
variables(A > B, ASSOC1, ASSOC, VARS1, VARS) :-
        !,
        variables2(A, ASSOC1, ASSOC2, VARS1, VARS2),
        variables2(B, ASSOC2, ASSOC, VARS2, VARS).
variables(A =< B, ASSOC1, ASSOC, VARS1, VARS) :-
        !,
        variables2(A, ASSOC1, ASSOC2, VARS1, VARS2),
        variables2(B, ASSOC2, ASSOC, VARS2, VARS).
variables(A >= B, ASSOC1, ASSOC, VARS1, VARS) :-
        !,
        variables2(A, ASSOC1, ASSOC2, VARS1, VARS2),
        variables2(B, ASSOC2, ASSOC, VARS2, VARS).

variables2(A + B, ASSOC1, ASSOC, VARS1, VARS) :-
        !,
        variables2(A, ASSOC1, ASSOC2, VARS1, VARS2),
        variables2(B, ASSOC2, ASSOC, VARS2, VARS).
variables2(A - B, ASSOC1, ASSOC, VARS1, VARS) :-
        !,
        variables2(A, ASSOC1, ASSOC2, VARS1, VARS2),
        variables2(B, ASSOC2, ASSOC, VARS2, VARS).
variables2(A * B, ASSOC1, ASSOC, VARS1, VARS) :-
        !,
        variables2(A, ASSOC1, ASSOC2, VARS1, VARS2),
        variables2(B, ASSOC2, ASSOC, VARS2, VARS).
variables2(A div B, ASSOC1, ASSOC, VARS1, VARS) :-
        !,
        variables2(A, ASSOC1, ASSOC2, VARS1, VARS2),
        variables2(B, ASSOC2, ASSOC, VARS2, VARS).
variables2(A mod B, ASSOC1, ASSOC, VARS1, VARS) :-
        !,
        variables2(A, ASSOC1, ASSOC2, VARS1, VARS2),
        variables2(B, ASSOC2, ASSOC, VARS2, VARS).
variables2(A rem B, ASSOC1, ASSOC, VARS1, VARS) :-
        !,
        variables2(A, ASSOC1, ASSOC2, VARS1, VARS2),
        variables2(B, ASSOC2, ASSOC, VARS2, VARS).
variables2(A, ASSOC1, ASSOC, VARS1, VARS) :-
        atom(A), !,
        insert(A, ASSOC1, ASSOC, VARS1, VARS).
variables2(_, ASSOC, ASSOC, VARS, VARS).

insert(A, [], [A=X], [], [X]) :-
        !,
        min_int(MIN),
        max_int(MAX),
        X in MIN..MAX.
insert(A, [A = X | L1], [A = X | L1], [X | L2], [X | L2]) :-
        !.
insert(A, [T | L1], [T | L2], [X | L3], [X | L4]) :-
        insert(A, L1, L2, L3, L4).

constraint(true, _) :-
        !.
constraint(and(A, B), ASSOC) :-
        !,
        constraint(A, ASSOC),
        constraint(B, ASSOC).
constraint(or(A, B), ASSOC) :- % zrobic to lepiej niz alternatywa ';'
        !,
        (   constraint(A, ASSOC)
        ;   constraint(B, ASSOC)).
constraint(not(A), ASSOC) :-
        !,
        negation(A, B),
        constraint(B, ASSOC).
constraint(A =:= B, ASSOC) :-
        !,
        eval(A, ASSOC, A1),
        eval(B, ASSOC, B1),
        A1 #= B1.
constraint(A =\= B, ASSOC) :-
        !,
        eval(A, ASSOC, A1),
        eval(B, ASSOC, B1),
        A1 #\= B1.
constraint(A < B, ASSOC) :-
        !,
        eval(A, ASSOC, A1),
        eval(B, ASSOC, B1),
        A1 #< B1.
constraint(A =< B, ASSOC) :-
        !,
        eval(A, ASSOC, A1),
        eval(B, ASSOC, B1),
        A1 #=< B1.
constraint(A > B, ASSOC) :-
        !,
        eval(A, ASSOC, A1),
        eval(B, ASSOC, B1),
        A1 #> B1.
constraint(A >= B, ASSOC) :-
        !,
        eval(A, ASSOC, A1),
        eval(B, ASSOC, B1),
        A1 #>= B1.

eval(A+B, ASSOC, C) :-
        !,
        eval(A, ASSOC, A1),
        eval(B, ASSOC, B1),
        C #= A1+B1.
eval(A-B, ASSOC, C) :-
        !,
        eval(A, ASSOC, A1),
        eval(B, ASSOC, B1),
        C #= A1-B1.
eval(A*B, ASSOC, C) :-
        !,
        eval(A, ASSOC, A1),
        eval(B, ASSOC, B1),
        C #= A1*B1.
eval(A div B, ASSOC, C) :-
        !,
        eval(A, ASSOC, A1),
        eval(B, ASSOC, B1),
        C #= A1 div B1.
eval(A mod B, ASSOC, C) :-
        !,
        eval(A, ASSOC, A1),
        eval(B, ASSOC, B1),
        C #= A1 mod B1.
eval(A rem B, ASSOC, C) :-
        !,
        eval(A, ASSOC, A1),
        eval(B, ASSOC, B1),
        C #= A1 rem B1.
eval(A, ASSOC, X) :-
        atom(A), !,
        member(A=X, ASSOC), !.
eval(X, _, X).

negation(true, false).
negation(false, true).
negation(and(A, B), or(A1, B1)) :-
        !,
        negation(A, A1),
        negation(B, B1).
negation(or(A, B), and(A1, B1)) :-
        !,
        negation(A, A1),
        negation(B, B1).
negation(not(A), A).
negation(A =:= B, A =\= B).
negation(A =\= B, A =:= B).
negation(A < B, A >= B).
negation(A =< B, A > B).
negation(A > B, A =< B).
negation(A >= B, A < B).

subst(and(A, B), R, E, and(A1, B1)) :-
        !,
        subst(A, R, E, A1),
        subst(B, R, E, B1).
subst(or(A, B), R, E, or(A1, B1)) :-
        !,
        subst(A, R, E, A1),
        subst(B, R, E, B1).
subst(not(A), R, E, not(A1)) :-
        !,
        subst(A, R, E, A1).
subst(A =:= B, R, E, A1 =:= B1) :-
        !,
        subst2(A, R, E, A1),
        subst2(B, R, E, B1).
subst(A =\= B, R, E, A1 =\= B1) :-
        !,
        subst2(A, R, E, A1),
        subst2(B, R, E, B1).
subst(A < B, R, E, A1 < B1) :-
        !,
        subst2(A, R, E, A1),
        subst2(B, R, E, B1).
subst(A > B, R, E, A1 > B1) :-
        !,
        subst2(A, R, E, A1),
        subst2(B, R, E, B1).
subst(A =< B, R, E, A1 =< B1) :-
        !,
        subst2(A, R, E, A1),
        subst2(B, R, E, B1).
subst(A >= B, R, E, A1 >= B1) :-
        !,
        subst2(A, R, E, A1),
        subst2(B, R, E, B1).
subst(true, _, _, true).

subst2(A+B, R, E, A1+B1) :-
        !,
        subst2(A, R, E, A1),
        subst2(B, R, E, B1).
subst2(A-B, R, E, A1-B1) :-
        !,
        subst2(A, R, E, A1),
        subst2(B, R, E, B1).
subst2(A*B, R, E, A1*B1) :-
        !,
        subst2(A, R, E, A1),
        subst2(B, R, E, B1).
subst2(A div B, R, E, A1 div B1) :-
        !,
        subst2(A, R, E, A1),
        subst2(B, R, E, B1).
subst2(A mod B, R, E, A1 mod B1) :-
        !,
        subst2(A, R, E, A1),
        subst2(B, R, E, B1).
subst2(A rem B, R, E, A1 rem B1) :-
        !,
        subst2(A, R, E, A1),
        subst2(B, R, E, B1).

subst2(A^B, R, E, A1^B1) :-
        !,
        subst2(A, R, E, A1),
        subst2(B, R, E, B1).
subst2(R, R, E, E) :-
        !.
subst2(A, _, _, A).

% path(+Block, ?Path)

path(B, P) :-
        path(B, P, []).

path([], P, P).
path([C | B], P1, P) :-
        path2(C, P1, P2),
        path(B, P2, P).

path2(R is E, [R is E | P], P).
path2(if(W, B), P1, P) :-
        path2(if(W, B, []), P1, P).
path2(if(W, B, _), [assert(W) | P1], P) :-
        path(B, P1, P).
path2(if(W, _, B), [assert(not(W)) | P1], P) :-
        path(B, P1, P).
path2(while(W, _), [assert(not(W)) | P], P).
path2(while(W, B), [assert(W) | P1], P) :-
        path(B, P1, P2),
        path2(while(W, B), P2, P).
