main1 :-
    thread_create(powtarzaj, _, [detached(true)]),
    thread_create(powtarzaj, _, [detached(true)]).

powtarzaj :-
    odliczaj(9), nl,
    powtarzaj.

main2 :-
    mutex_create(Mutex),
    thread_create(powtarzaj(Mutex), _, [detached(true)]),
    thread_create(powtarzaj(Mutex), _, [detached(true)]).

powtarzaj(Mutex) :-
    with_mutex(Mutex, (odliczaj(9), nl)),
    powtarzaj(Mutex).

odliczaj(0).
odliczaj(N) :-
    N > 0,
    write(N),
    N1 is N-1,
    odliczaj(N1).




