:- module(gui, [show/2]).

:- use_module(library(pce)).

color(0, black,  white).
color(1, brown,  white).
color(2, red,    white).
color(3, orange, white).
color(4, yellow, black).
color(5, green,  black).
color(6, blue,   white).
color(7, indigo, white).
color(8, gray,   black).
color(9, white,  black).

show(Board, N) :-
    Size is N * 52+1,
    new(Window, window('Tetravex Solver', size(Size, Size))),
    draw_board(Board, N, Window, 0),
    send(Window, open).

draw_board([], _, _, _).
draw_board([B | Bs], N, Window, Y) :-
    length(Row, N),
    append(Row, Rest, [B | Bs]),
    draw_row(Row, Window, 0, Y),
    Y1 is Y + 52,
    draw_board(Rest, N, Window, Y1).

draw_row([], _, _, _).
draw_row([[N, E, S, W] | Row], Window, X, Y) :-
    color(N, C1, C11),
    color(E, C2, C22),
    color(S, C3, C33),
    color(W, C4, C44),
    new(P1, path),
    send(P1, append, point(X, Y)),
    send(P1, append, point(X+50, Y)),
    send(P1, append, point(X+25, Y+25)),
    new(P2, path),
    send(P2, append, point(X+50, Y)),
    send(P2, append, point(X+50, Y+50)),
    send(P2, append, point(X+25, Y+25)),
    new(P3, path),
    send(P3, append, point(X+25, Y+25)),
    send(P3, append, point(X+50, Y+50)),
    send(P3, append, point(X, Y+50)),
    new(P4, path),
    send(P4, append, point(X, Y)),
    send(P4, append, point(X+25, Y+25)),
    send(P4, append, point(X, Y+50)),
    send(P1, fill_pattern, colour(C1)),
    send(P2, fill_pattern, colour(C2)),
    send(P3, fill_pattern, colour(C3)),
    send(P4, fill_pattern, colour(C4)),
    new(T1, text(N)),
    send(T1, colour, C11),
    new(T2, text(E)),
    send(T2, colour, C22),
    new(T3, text(S)),
    send(T3, colour, C33),
    new(T4, text(W)),
    send(T4, colour, C44),
    send(Window, display, P1),
    send(Window, display, P2),
    send(Window, display, P3),
    send(Window, display, P4),
    send(Window, display, T1, point(X+21, Y+1)),
    send(Window, display, T2, point(X+37, Y+16)),
    send(Window, display, T3, point(X+21, Y+30)),
    send(Window, display, T4, point(X+4, Y+16)),
    X1 is X + 52,
    draw_row(Row, Window, X1, Y).














