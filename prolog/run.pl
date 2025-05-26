
:- module(run, []). 

% run mastermind server and test
:- use_module('./mastermindserver'). 
:- use_module('./mastermind'). 
:- use_module('./mastermindtest'). 





% Main predicate to start the server
:- initialization(main).

main :-
	debug(http), 
	debug(mastermind),
	%testAll,
	source_file(main, Path),
	print(Path),
	start_server.

start:-
	debug.