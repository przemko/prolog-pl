main(N) :-
    drukuj(S2),
    sito(S1, S2),
    generowanie(2, N, S1).

sito(In, Out) :-
    freeze(In,
           (   czytaj(In, N, In_)
           ->  pisz(N, Out, Out_),
               filtruj(N, In_, Out1),
               sito(Out1, Out_)
           ;   zamknij(Out))).

filtruj(N, In, Out) :-
    freeze(In,
           (   czytaj(In, I, In_)
           ->  (   I mod N =:= 0
               ->  filtruj(N, In_, Out)
               ;   pisz(I, Out, Out_),
                   filtruj(N, In_, Out_))
           ;   zamknij(Out))).

czytaj([H | T], H, T).

pisz(H, [H | T], T).

zamknij([]).

generowanie(I, J, S) :-
    (   I =< J
    ->  S = [I | T],
        I1 is I+1,
        generowanie(I1, J, T)
    ;   S = []).

drukuj(S) :-
    freeze(S,
           (   S = [H | T]
           ->  writeln(H),
               drukuj(T)
           ;   true)).
