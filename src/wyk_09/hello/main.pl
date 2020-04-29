% main.pl
:- use_module(library(pce)).
:- use_module(library(help_message)).

main :-
    pce_main_loop(open_window).

open_window(_Argv) :-
    new(Window, dialog('')),
    new(Hello, button(hello, 
                      message(@prolog, writeln, 'Hello!'))),
    new(Quit, button(quit,
                     message(Window,  destroy))),
    send(Hello, help_message, tag, 'kliknij aby sie przywitać'),
    send(Quit, help_message, tag, 'kliknij aby zakończyć'),
    send(Window, append, Hello),
    send(Hello, left, Quit),
    send(Window, open).

