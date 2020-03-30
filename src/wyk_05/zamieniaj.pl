% zamieniaj/0 kopiuje standardowe wejście na standardowy wyjście
% zamieniając litery a na litery b
% 
zamieniaj :-
    get_char(C),
    dalej(C).

dalej(end_of_file) :-
    !.
dalej(a) :-
    !,
    put_char(b),
    zamieniaj.
dalej(X) :-
    put_char(X),
    zamieniaj.
