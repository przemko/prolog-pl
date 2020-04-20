bit(0).
bit(1).

bits1([]).
bits1([X | Y]) :- bit(X), bits1(Y).

bits2([]).
bits2([X | Y]) :- bits2(Y), bit(X).

