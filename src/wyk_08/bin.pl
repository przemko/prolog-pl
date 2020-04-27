% Gramatyka metamorficzna CIĄG BITÓW --> WARTOŚĆ DZIESIĘTNA
%
bin(X) --> bin(0, X).
bin(X, X) --> ``.
bin(X, Z) --> `0`, {Y is 2*X}, bin(Y, Z).
bin(X, Z) --> `1`, {Y is 2*X+1}, bin(Y, Z).
