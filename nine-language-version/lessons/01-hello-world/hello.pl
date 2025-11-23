% hello.pl
% A simple Prolog program that prints a greeting
:- initialization(main).

main :-
    write('Hello, World!'), nl,
    halt.
