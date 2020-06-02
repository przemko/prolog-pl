:- use_module(library(clpfd)).

odcinek(X) :-
    automaton(X, [source(q0), sink(q8)],
              [arc(q0, 0, q0),
               arc(q0, 1, q1), arc(q1, 1, q2),
               arc(q2, 1, q3), arc(q3, 1, q4),
               arc(q4, 1, q5), arc(q5, 1, q6),
               arc(q6, 1, q7), arc(q7, 1, q8),
               arc(q8, 0, q8)]).

