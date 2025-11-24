#!/usr/bin/env swipl
% Lesson 4: Functions in Prolog
% Note: Prolog doesn't have "functions" in the traditional sense
% Instead, it has predicates (relations) that succeed or fail

:- initialization(main, main).

% ============================================
% 1. Basic Predicates (Prolog's "functions")
% ============================================

% Predicate with output parameter
greet(Name, Greeting) :-
    format(atom(Greeting), 'Hello, ~w!', [Name]).

% Arithmetic predicate
add(X, Y, Result) :-
    Result is X + Y.

square(X, Result) :-
    Result is X * X.

% ============================================
% 2. Pattern Matching (Core Feature)
% ============================================

% Pattern matching on values
describe_number(0, 'Zero').
describe_number(1, 'One').
describe_number(2, 'Two').
describe_number(_, 'Many').

% Pattern matching on lists
list_length([], 0).
list_length([_|T], Length) :-
    list_length(T, TailLength),
    Length is TailLength + 1.

% ============================================
% 3. Recursive Predicates
% ============================================

% Factorial
factorial(0, 1).
factorial(N, Result) :-
    N > 0,
    N1 is N - 1,
    factorial(N1, Result1),
    Result is N * Result1.

% Sum of list
sum_list([], 0).
sum_list([H|T], Sum) :-
    sum_list(T, TailSum),
    Sum is H + TailSum.

% ============================================
% 4. Higher-Order Predicates
% ============================================

% Map-like operation
map_list(_, [], []).
map_list(Predicate, [H|T], [MappedH|MappedT]) :-
    call(Predicate, H, MappedH),
    map_list(Predicate, T, MappedT).

% Filter-like operation
filter_list(_, [], []).
filter_list(Predicate, [H|T], [H|Filtered]) :-
    call(Predicate, H),
    !,
    filter_list(Predicate, T, Filtered).
filter_list(Predicate, [_|T], Filtered) :-
    filter_list(Predicate, T, Filtered).

% Predicates for use with higher-order predicates
double(X, Result) :- Result is X * 2.
is_even(X) :- 0 is X mod 2.

% ============================================
% 5. Accumulator Pattern (Tail Recursion)
% ============================================

% Factorial with accumulator (tail recursive)
factorial_acc(N, Result) :-
    factorial_acc(N, 1, Result).

factorial_acc(0, Acc, Acc).
factorial_acc(N, Acc, Result) :-
    N > 0,
    N1 is N - 1,
    Acc1 is Acc * N,
    factorial_acc(N1, Acc1, Result).

% Apply function N times
apply_n_times(_, 0, X, X).
apply_n_times(Predicate, N, X, Result) :-
    N > 0,
    call(Predicate, X, X1),
    N1 is N - 1,
    apply_n_times(Predicate, N1, X1, Result).

% ============================================
% 6. Multiple Solutions (Backtracking)
% ============================================

% Generate multiple solutions
number_between(Low, High, Low) :- Low =< High.
number_between(Low, High, N) :-
    Low < High,
    Low1 is Low + 1,
    number_between(Low1, High, N).

% ============================================
% 7. Conditional Logic
% ============================================

describe_age(Age, Description) :-
    (Age < 13 ->
        Description = 'Child'
    ; Age < 18 ->
        Description = 'Teenager'
    ;
        Description = 'Adult'
    ).

% Using guards (more declarative)
describe_age_guards(Age, 'Child') :- Age < 13, !.
describe_age_guards(Age, 'Teenager') :- Age < 18, !.
describe_age_guards(_, 'Adult').

% ============================================
% 8. Predicates as "Closures" (using extra parameters)
% ============================================

% Create a "multiplier" by partially applying
multiply_by(Factor, X, Result) :-
    Result is X * Factor.

% Helper to create specific multipliers
times_two(X, Result) :- multiply_by(2, X, Result).
times_three(X, Result) :- multiply_by(3, X, Result).

% ============================================
% 9. Pure Logic Programming
% ============================================

% Append lists (can work forwards and backwards!)
append_lists([], L, L).
append_lists([H|T], L2, [H|Result]) :-
    append_lists(T, L2, Result).

% Reverse list
reverse_list(List, Reversed) :-
    reverse_acc(List, [], Reversed).

reverse_acc([], Acc, Acc).
reverse_acc([H|T], Acc, Reversed) :-
    reverse_acc(T, [H|Acc], Reversed).

% ============================================
% Main Program
% ============================================

main :-
    writeln('=== Prolog Functions (Predicates) ==='),
    nl,

    % 1. Basic predicates
    writeln('1. Basic Predicates:'),
    greet('Alice', Greeting),
    format('  greet(Alice): ~w~n', [Greeting]),
    add(5, 3, Sum),
    format('  add(5, 3): ~w~n', [Sum]),
    square(7, Squared),
    format('  square(7): ~w~n', [Squared]),

    % 2. Pattern matching
    nl,
    writeln('2. Pattern Matching:'),
    describe_number(0, Desc0),
    format('  describe_number(0): ~w~n', [Desc0]),
    describe_number(2, Desc2),
    format('  describe_number(2): ~w~n', [Desc2]),
    describe_number(5, Desc5),
    format('  describe_number(5): ~w~n', [Desc5]),

    % 3. Recursive predicates
    nl,
    writeln('3. Recursive Predicates:'),
    factorial(5, Fact5),
    format('  factorial(5): ~w~n', [Fact5]),
    sum_list([1, 2, 3, 4, 5], ListSum),
    format('  sum_list([1,2,3,4,5]): ~w~n', [ListSum]),

    % 4. Higher-order predicates
    nl,
    writeln('4. Higher-Order Predicates:'),
    map_list(double, [1, 2, 3, 4, 5], Doubled),
    format('  map_list(double, [1,2,3,4,5]): ~w~n', [Doubled]),
    filter_list(is_even, [1, 2, 3, 4, 5], Evens),
    format('  filter_list(is_even, [1,2,3,4,5]): ~w~n', [Evens]),

    % 5. Tail recursion
    nl,
    writeln('5. Tail Recursion (accumulator):'),
    factorial_acc(5, FactAcc),
    format('  factorial_acc(5): ~w~n', [FactAcc]),

    % 6. Apply N times
    nl,
    writeln('6. Apply Predicate N Times:'),
    apply_n_times(double, 3, 5, Result3),
    format('  apply_n_times(double, 3, 5): ~w~n', [Result3]),

    % 7. Multiple solutions (backtracking)
    nl,
    writeln('7. Multiple Solutions (backtracking):'),
    writeln('  All numbers between 1 and 5:'),
    write('   '),
    forall(
        number_between(1, 5, N),
        (write(' '), write(N))
    ),
    nl,

    % 8. Conditional logic
    nl,
    writeln('8. Conditional Logic:'),
    describe_age(10, AgeDesc10),
    format('  describe_age(10): ~w~n', [AgeDesc10]),
    describe_age(15, AgeDesc15),
    format('  describe_age(15): ~w~n', [AgeDesc15]),
    describe_age(25, AgeDesc25),
    format('  describe_age(25): ~w~n', [AgeDesc25]),

    % 9. "Closures" (predicates with fixed parameters)
    nl,
    writeln('9. Closures (partial application):'),
    times_two(5, TimesTwo5),
    format('  times_two(5): ~w~n', [TimesTwo5]),
    times_three(5, TimesThree5),
    format('  times_three(5): ~w~n', [TimesThree5]),

    % 10. Bidirectional predicates
    nl,
    writeln('10. Bidirectional Predicates:'),
    append_lists([1, 2], [3, 4], Appended),
    format('  append_lists([1,2], [3,4]): ~w~n', [Appended]),

    % Reverse direction!
    append_lists(First, Second, [1, 2, 3]),
    format('  append_lists(X, Y, [1,2,3]): X=~w, Y=~w (one solution)~n', [First, Second]),

    reverse_list([1, 2, 3, 4, 5], Reversed),
    format('  reverse_list([1,2,3,4,5]): ~w~n', [Reversed]),

    % 11. Prolog's unique features
    nl,
    writeln('11. Prolog Special Features:'),
    writeln('  - Logic programming: describe what, not how'),
    writeln('  - Backtracking: automatic search for solutions'),
    writeln('  - Unification: pattern matching and variable binding'),
    writeln('  - Bidirectional predicates: work forwards and backwards'),

    halt(0).
