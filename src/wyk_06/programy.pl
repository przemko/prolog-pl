:- module(programy, [program/2]).

% Przykłady prostych programów:
%
% obliczenie sumy liczb od 1 do N:
% read N;
% SUM := 0;
% while N > 0 do
%   SUM := SUM + N;
%   N := N - 1;
% od;
% write SUM;
%
% ?- program(p1, X), interpreter(X).
% |: 10.
% 55
%  X = [read('N'), assign('SUM', int(0)), while(id('N')>int(0),
% [assign('SUM', id('SUM')+id('N')), assign('N', id('N')-int(1))]),
% write(id('SUM'))].
%
program(
    p1,
    [
	read('N'),
	assign('SUM', int(0)),
	while(id('N') > int(0),
	      [
		  assign('SUM', id('SUM') + id('N')),
		  assign('N', id('N') - int(1))]),
	write(id('SUM'))]).

% obliczenie ilorazu D i reszty R z dzielenia M przez N:
% read M;
% read N;
% D := 0;
% R := M;
% while R > N do
%   D := D + 1;
%   R := R - N;
% od;
% write D;
% write R;
%
% ?- program(p2, X), interpreter(X).
% |: 123.
% |: 32.
% 3
% 27
%  X = [read('M'), read('N'), assign('D', int(0)), assign('R', id('M')),
% while(id('R')>id('N'), [assign('D', id(...)+int(...)), assign('R', ...
% - ...)]), write(id('D')), write(id('R'))].
%
% ?- X is 3*32+27.
% X = 123.
%
program(
    p2,
    [
	read('M'),
	read('N'),
	assign('D', int(0)),
	assign('R', id('M')),
	while(id('R') > id('N'),
	      [
		  assign('D', id('D') + int(1)),
		  assign('R', id('R') - id('N'))]),
	write(id('D')),
	write(id('R'))]).

% obliczenie sumy dwóch wczytanych liczb:
% read A;
% read B;
% X := A;
% Y := B;
% while Y > 0 do
%   X := X + 1;
%   Y := Y - 1;
% od;
% write X;
%
% ?- program(p3, X), interpreter(X).
% |: 2.
% |: 2.
% 4
% X = [read('A'), read('B'), assign('X', id('A')), assign('Y',
%  id('B')), while(id('Y')>int(0), [assign('X', id(...)+int(...)),
%  assign('Y', ... - ...)]), write(id('X'))].
%
program(
    p3,
    [
	read('A'),
	read('B'),
	assign('X', id('A')),
	assign('Y', id('B')),
	while(id('Y') > int(0),
	      [
		  assign('X', id('X') + int(1)),
		  assign('Y', id('Y') - int(1))]),
	write(id('X'))]).

% program Turinga obliczajacy N!
% read N;
% U := 1;
% R := 1;
% while R < N do
%   V := U;
%   S := 1;
%   while S <= R do
%     U := U + V;
%     S := S + 1;
%   od;
%   R := R + 1;
% od;
% write U;
%
% ?- program(p4, X), interpreter(X).
% |: 10.
% 3628800
% X = [read('N'), assign('U', int(1)), assign('R', int(1)),
%  while(id('R')<id('N'), [assign('V', id('U')), assign('S',
%  int(1)), while(... =< ..., [...|...]), assign(..., ...)]),
%  write(id('U'))].
%
program(
    p4,
    [
	read('N'),
	assign('U', int(1)),
	assign('R', int(1)),
	while(id('R') < id('N'),
	      [
		  assign('V', id('U')),
		  assign('S', int(1)),
		  while(id('S') =< id('R'),
			[
			    assign('U', id('U') + id('V')),
			    assign('S', id('S') + int(1))]),
		  assign('R', id('R') + int(1))]),
	write(id('U'))]).
