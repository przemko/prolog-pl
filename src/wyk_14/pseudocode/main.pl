:- use_module(pseudocode).
:- use_module(refute).

analyze(FileName, State) :-
	file_parser(FileName, PRE, BLOCK, POST),
	refute(PRE, BLOCK, POST, State).

