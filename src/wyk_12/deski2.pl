:- use_module(library(clpfd)).

deski(N, N1, N2, N3, Sposoby, Odpad) :-
    Sposoby = [S1, S2, S3],
    Sposoby ins 0..N,
    Deski #= S1 + S2 + S3,
    Deski #=< N,
    W1 #= S1 + 4*S3,
    W2 #= 2*S2,
    W3 #= 2*S1 + S2 + S3,
    W1 #>= N1, W2 #>= N2, W3 #>= N3,
    Odpad #= (W1 - N1) + 2*(W2 - N2) + 3*(W3 - N3),
    once(labeling([min(Deski)], Sposoby)).
