:- module(solver, [solve/2]).

solve(Tiles, Board) :-
    board(Board),
    insert(Board, Tiles).

board([[_ ,E1,S1,_ ], [_ ,E2,S2,W2], [_ ,_ ,S3,W3],
       [N4,E4,S4,_ ], [N5,E5,S5,W5], [N6,_ ,S6,W6],
       [N7,E7,_ ,_ ], [N8,E8,_ ,W8], [N9,_ ,_ ,W9]]) :-
    E1 = W2, E2 = W3,
    S1 = N4, S2 = N5, S3 = N6,
    E4 = W5, E5 = W6,
    S4 = N7, S5 = N8, S6 = N9,
    E7 = W8, E8 = W9.

insert([], []).
insert([B | Bs], Tiles) :-
    select(B, Tiles, Rest),
    insert(Bs, Rest).

