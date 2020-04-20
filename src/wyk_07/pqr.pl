p(a).
p(b).
p(c).

q(b).
q(c).
q(d).

r1(X) :- p(X), q(Y), X = Y.

r2(X) :- p(X), q(X).


