arc(a, b).
arc(a, c).
arc(b, c).
arc(b, e).
arc(d, b).
arc(d, c).
arc(d, e).
arc(d, f).
arc(e, f).

path(X, Y) :- arc(X, Y).
path(X, Z) :- arc(X, Y), path(Y, Z).

