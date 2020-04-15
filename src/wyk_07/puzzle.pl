% Zagadka: co będzie efektem wywołania puzzle(N),
% dla całkowitego N >= 0?
%
puzzle(0) :-
    throw(0).
puzzle(N) :-
    catch((N1 is N-1, puzzle(N1)),
          M,
          (M1 is M+1, throw(M1))).

