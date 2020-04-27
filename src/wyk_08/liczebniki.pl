% Gramatyka metamorficzna LICZEBNIK --> WARTOŚĆ DZIESIĘTNA
%
number(0) --> [zero].
number(N) --> xxx(N).

xxx(N) -->
	digit(D), [hundred], rest_xxx(N1), {N is D*100 + N1}.
xxx(N) -->
	xx(N).

rest_xxx(0) --> [].
rest_xxx(N) --> [and], xx(N).

xx(N) --> digit(N).
xx(N) --> teen(N).
xx(N) --> tens(T), rest_xx(N1), {N is T+N1}.

rest_xx(0) --> [].
rest_xx(N) --> digit(N).

digit(1) --> [one].
digit(2) --> [two].
digit(3) --> [three].
digit(4) --> [four].
digit(5) --> [five].
digit(6) --> [six].
digit(7) --> [seven].
digit(8) --> [eight].
digit(9) --> [nine].

teen(10) --> [ten].
teen(11) --> [eleven].
teen(12) --> [twelve].
teen(13) --> [thirteen].
teen(14) --> [fourteen].
teen(15) --> [fifteen].
teen(16) --> [sixteen].
teen(17) --> [seventeen].
teen(18) --> [eighteen].
teen(19) --> [nineteen].

tens(20) --> [twenty].
tens(30) --> [thirty].
tens(40) --> [forty].
tens(50) --> [fifty].
tens(60) --> [sixty].
tens(70) --> [seventy].
tens(80) --> [eighty].
tens(90) --> [ninety].
