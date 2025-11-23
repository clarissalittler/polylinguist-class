#!/usr/bin/env swipl
% Lesson 3: Control Flow in Prolog
% Demonstrates declarative logic, pattern matching, and backtracking

:- initialization(main, main).

main :-
    writeln('=== Prolog Control Flow ==='),
    nl,

    % 1. Pattern matching and facts
    writeln('1. Pattern Matching (declarative rules):'),
    describe_age(20, AgeDesc),
    format('  Age 20: ~w~n', [AgeDesc]),
    describe_age(15, TeenDesc),
    format('  Age 15: ~w~n', [TeenDesc]),
    describe_age(10, ChildDesc),
    format('  Age 10: ~w~n', [ChildDesc]),

    % 2. Conditionals with if-then-else
    nl,
    writeln('2. Conditionals (if-then-else):'),
    Age = 20,
    (Age >= 18 ->
        Status = 'Adult'
    ;
        Status = 'Minor'
    ),
    format('  Status: ~w~n', [Status]),

    % 3. "Loops" via recursion and backtracking
    nl,
    writeln('3. Recursion (declarative iteration):'),
    writeln('  Count to 5:'),
    write('   '),
    print_numbers(0, 5),
    nl,

    writeln('  Iterate list:'),
    Fruits = ['apple', 'banana', 'cherry'],
    print_list(Fruits),

    % 4. Backtracking (Prolog's unique control flow)
    nl,
    writeln('4. Backtracking (finding all solutions):'),
    writeln('  All numbers 1-5 divisible by 2:'),
    write('   '),
    forall(
        (between(1, 5, N), 0 is N mod 2),
        (write(' '), write(N))
    ),
    nl,

    % 5. Boolean logic
    nl,
    writeln('5. Boolean Logic:'),
    X = 5,
    Y = 10,
    format('  X=~w, Y=~w~n', [X, Y]),
    (X > 3, Y < 20 ->
        writeln('  X > 3 AND Y < 20: true')
    ;
        writeln('  X > 3 AND Y < 20: false')
    ),
    (X > 10 ; Y > 5 ->
        writeln('  X > 10 OR Y > 5: true')
    ;
        writeln('  X > 10 OR Y > 5: false')
    ),
    (\+ X = Y ->
        writeln('  NOT (X = Y): true')
    ;
        writeln('  NOT (X = Y): false')
    ),

    % 6. FizzBuzz
    nl,
    writeln('6. FizzBuzz (1-20):'),
    write(' '),
    forall(
        between(1, 20, I),
        (fizzbuzz(I, Result), write(' '), write(Result))
    ),
    nl,

    % 7. Pattern matching on structures
    nl,
    writeln('7. Pattern Matching on Structures:'),
    describe_point(point(0, 0), P1Desc),
    format('  point(0, 0): ~w~n', [P1Desc]),
    describe_point(point(0, 5), P2Desc),
    format('  point(0, 5): ~w~n', [P2Desc]),
    describe_point(point(3, 4), P3Desc),
    format('  point(3, 4): ~w~n', [P3Desc]),

    % 8. Pattern matching on lists
    nl,
    writeln('8. Pattern Matching on Lists:'),
    describe_list([], L1Desc),
    format('  []: ~w~n', [L1Desc]),
    describe_list([1], L2Desc),
    format('  [1]: ~w~n', [L2Desc]),
    describe_list([1, 2, 3], L3Desc),
    format('  [1,2,3]: ~w~n', [L3Desc]),

    halt(0).

% Pattern matching with rules (declarative conditionals)
describe_age(Age, 'Adult') :- Age >= 18.
describe_age(Age, 'Teenager') :- Age >= 13, Age < 18.
describe_age(Age, 'Child') :- Age < 13.

% Recursion for "looping"
print_numbers(Current, Limit) :-
    Current < Limit,
    !,  % Cut to prevent backtracking
    write(' '), write(Current),
    Next is Current + 1,
    print_numbers(Next, Limit).
print_numbers(_, _).  % Base case

% Print list recursively
print_list([]).
print_list([H|T]) :-
    format('    ~w~n', [H]),
    print_list(T).

% FizzBuzz
fizzbuzz(N, 'FizzBuzz') :- 0 is N mod 15, !.
fizzbuzz(N, 'Fizz') :- 0 is N mod 3, !.
fizzbuzz(N, 'Buzz') :- 0 is N mod 5, !.
fizzbuzz(N, N).

% Pattern matching on structures
describe_point(point(0, 0), 'Origin') :- !.
describe_point(point(0, Y), Desc) :-
    !, format(atom(Desc), 'Y-axis at ~w', [Y]).
describe_point(point(X, 0), Desc) :-
    !, format(atom(Desc), 'X-axis at ~w', [X]).
describe_point(point(X, Y), Desc) :-
    format(atom(Desc), 'Point at (~w, ~w)', [X, Y]).

% Pattern matching on lists
describe_list([], 'Empty list') :- !.
describe_list([_], 'Single element') :- !.
describe_list([_, _], 'Two elements') :- !.
describe_list([X, Y|_], Desc) :-
    format(atom(Desc), 'Starts with ~w and ~w', [X, Y]).
