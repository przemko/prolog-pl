% main.pl
%
% Integracja modulu tabsem z jego interfejsem graficznym
%
% Importowane moduly:
%
%   tabsem  metoda tabel semantycznych dla rachunku predykatow
%   gui	    interfejs graficzny do modulu tabsem
%
% Autor: Przemyslaw Kobylanski <przemko@pwr.wroc.pl>
%


:- use_module(library(pce)).
:- use_module(tabsem).
:- use_module(gui).

main :-
	tabsem:version(VER1),
	gui:version(VER2),
        write('Method of analytic tableaux for the first order logic (Version '),
	write(VER1), write('.'), write(VER2),
	write(')'),
        nl,
        write('Copyright (c) 2012 Przemyslaw Kobylanski, '),
        write('Wroclaw University of Technology'),
        nl,
        nl,
        pce_main_loop(make_window).
