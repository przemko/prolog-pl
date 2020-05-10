main :-
    thread_create(gracz, Id1, [detached(true)]),
    thread_create(gracz, Id2, [detached(true)]),
    thread_send_message(Id1, przeciwnik(Id2)),
    thread_send_message(Id2, przeciwnik(Id1)),
    thread_send_message(Id1, ping).

gracz :-
    thread_get_message(przeciwnik(Id)),
    gracz(Id).

gracz(Id) :-
    thread_get_message(M1),
    writeln(M1),
    odbicie(M1, M2),
    thread_send_message(Id, M2),
    gracz(Id).

odbicie(ping, pong).
odbicie(pong, ping).

