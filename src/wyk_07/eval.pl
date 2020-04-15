% ?- eval(2+2, X).
% X = 4.
%
% ?- eval(sqrt(-1), X).
% X = 1.5NaN.
%
% ?- eval(1/0, X).
% X = 1.5NaN.
%
% ?- eval(sqrt(-1), X), eval(X+1, Y).
% X = Y, Y = 1.5NaN.

eval(Expression, Value) :-
    catch(Value is Expression, _, Value is nan).

