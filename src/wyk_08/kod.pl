% Gramatyka metamorficzna TERM <--> KOD
%
kod(a) --> `0`.
kod(f(X, Y)) --> `1`, kod(X), kod(Y).
