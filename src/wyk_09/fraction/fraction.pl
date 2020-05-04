:- use_module(library(pce)).

:- pce_begin_class(fraction, object, "Ułamek").

variable(numerator,   int, get, "Licznik").
variable(denominator, int, get, "Mianownik").

initialise(F, Num:int, Den:int) :->
    send(F, slot, numerator, Num),
    send(F, slot, denominator, Den).

numerator(F, Num:int) :->
    "Ustalenie wartości licznika"::
    send(F, slot, numerator, Num).

denominator(F, Num:int) :->
    "Ustalenie wartości mianownika"::
    send(F, slot, denominator, Num).

add(F, Arg:fraction) :->
    "Dodanie drugiego ułamka (można lepiej)"::
    send(F, numerator,
         F?numerator*Arg?denominator+F?denominator*Arg?numerator),
    send(F, denominator,
         F?denominator*Arg?denominator).

value(F, Value:real) :<-
    "Pobranie wartości"::
    get(F, numerator, Num),
    get(F, denominator, Den),
    Value is Num/Den.

:- pce_end_class.


test(V1, V2) :-
    new(F1, fraction(1, 3)),
    new(F2, fraction(2, 3)),
    send(F1, add, F2),
    get(F1, value, V1),
    get(F2, value, V2).



