%     SEND
%     MORE
% + -------
%    MONEY
%
%  1. Cyfry S,E,N,D,M,O,R,Y są parami różne.
%  2. Liczby nie zaczynają się od zera.
%
% ?- time(solve1(X)).
% % 878,558,137 inferences, 74.980 CPU in 74.982 seconds (100% CPU, 11717253 Lips)
% X = [[9, 5, 6, 7], [1, 0, 8, 5], [1, 0, 6, 5, 2]] ;
% % 33,939,529 inferences, 2.910 CPU in 2.910 seconds (100% CPU, 11661681 Lips)
% false.
%
% ?- time(solve2(X)).
% % 7,380 inferences, 0.003 CPU in 0.003 seconds (100% CPU, 2282184 Lips)
% X = [[9, 5, 6, 7], [1, 0, 8, 5], [1, 0, 6, 5, 2]] ;
% % 1,999 inferences, 0.001 CPU in 0.001 seconds (100% CPU, 2038546 Lips)
% false.
%
%                        | solve1/1 | solve2/1 |  Przyspieszenie
%  ==============================================================
%   Pierwsze rozwiązanie | 74.980 s |  0.003 s | 25 tysięcy razy
%  Wszystkie rozwiązania | 77.890 s |  0.004 s | 19 tysięcy razy
%
:- use_module(library(clpfd)).

% Prolog bez więzów:
%
solve1([[S,E,N,D],[M,O,R,E],[M,O,N,E,Y]]) :-
    digit(S),
    digit(E),
    digit(N),
    digit(D),
    digit(M),
    digit(O),
    digit(R),
    digit(Y),
    S =\= E, S =\= N, S =\= D, S =\= M, S =\= O, S =\= R, S =\= Y,
    E =\= N, E =\= D, E =\= M, E =\= O, E =\= R, E =\= Y,
    N =\= D, N =\= M, N =\= O, N =\= R, N =\= Y,
    D =\= M, D =\= O, D =\= R, D =\= Y,
    M =\= O, M =\= R, M =\= Y,
    O =\= R, O =\= Y,
    R =\= Y,
    S > 0, M > 0,
              1000*S + 100*E + 10*N + D +
              1000*M + 100*O + 10*R + E =:=
    10000*M + 1000*O + 100*N + 10*E + Y.

digit(0).
digit(1).
digit(2).
digit(3).
digit(4).
digit(5).
digit(6).
digit(7).
digit(8).
digit(9).

% Prolog z więzami:
%
solve2([[S,E,N,D],[M,O,R,E],[M,O,N,E,Y]]) :-
    Vars = [S, E, N, D, M, O, R, Y],
    Vars ins 0..9,
    all_different(Vars),
    S #> 0, M #> 0,
              1000*S + 100*E + 10*N + D +
              1000*M + 100*O + 10*R + E #=
    10000*M + 1000*O + 100*N + 10*E + Y,
    label(Vars).











