:- use_module(tiles).
:- use_module(solver).
:- use_module(gui).

% solve(+Nazwa_Przykładu), na przykład solve(t3)
solve(ID) :-
    tiles(ID, Tiles),
    time(solve(Tiles, Board)),
    show(Board, 3).








