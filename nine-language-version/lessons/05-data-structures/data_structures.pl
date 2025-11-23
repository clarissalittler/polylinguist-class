#!/usr/bin/env swipl
% Lesson 5: Data Structures in Prolog
% Demonstrates lists, terms, and logical structures

:- initialization(main, main).

main :-
    writeln('=== Prolog Data Structures ==='),
    nl,

    % 1. Lists (immutable)
    writeln('1. Lists (Immutable):'),
    Numbers = [1, 2, 3, 4, 5],
    format('  List: ~w~n', [Numbers]),

    % Pattern matching with lists
    [Head|Tail] = Numbers,
    format('  Head: ~w, Tail: ~w~n', [Head, Tail]),

    % List operations
    length(Numbers, Len),
    format('  Length: ~w~n', [Len]),

    reverse(Numbers, Reversed),
    format('  Reversed: ~w~n', [Reversed]),

    % 2. List construction
    nl,
    writeln('2. List Construction:'),
    NewList = [0|Numbers],  % Prepend
    format('  Prepend 0: ~w~n', [NewList]),

    append(Numbers, [6], WithSix),
    format('  Append 6: ~w~n', [WithSix]),

    append([1, 2], [3, 4], Combined),
    format('  Append [1,2] and [3,4]: ~w~n', [Combined]),

    % 3. List predicates
    nl,
    writeln('3. List Predicates:'),
    (member(3, Numbers) ->
        writeln('  member(3, list): true')
    ;
        writeln('  member(3, list): false')
    ),

    nth0(0, Numbers, First),
    format('  nth0(0): ~w~n', [First]),

    % 4. Recursive list operations
    nl,
    writeln('4. Recursive List Operations:'),
    sum_list(Numbers, Sum),
    format('  sum_list: ~w~n', [Sum]),

    % Custom map
    map_double(Numbers, Doubled),
    format('  map_double: ~w~n', [Doubled]),

    % Custom filter
    filter_evens([1, 2, 3, 4, 5, 6], Evens),
    format('  filter_evens: ~w~n', [Evens]),

    % 5. Terms and structures
    nl,
    writeln('5. Terms and Structures:'),
    Person = person('Alice', 30, 'NYC'),
    format('  Person term: ~w~n', [Person]),

    % Pattern matching on structures
    person(Name, Age, City) = Person,
    format('  Destructured: ~w, ~w, ~w~n', [Name, Age, City]),

    % 6. Key-value pairs (association lists)
    nl,
    writeln('6. Association Lists (Key-Value):'),
    Dict = [name-'Bob', age-25, city-'SF'],
    format('  Dict: ~w~n', [Dict]),

    % Lookup
    (member(name-N, Dict) ->
        format('  Lookup name: ~w~n', [N])
    ;
        writeln('  Name not found')
    ),

    % 7. Nested lists
    nl,
    writeln('7. Nested Lists:'),
    Matrix = [[1, 2, 3], [4, 5, 6], [7, 8, 9]],
    format('  Matrix: ~w~n', [Matrix]),

    nth0(1, Matrix, Row),
    nth0(2, Row, Element),
    format('  matrix[1][2]: ~w~n', [Element]),

    % 8. Difference lists (advanced)
    nl,
    writeln('8. Difference Lists (Efficient Append):'),
    writeln('  Difference lists enable O(1) append'),
    DiffList1 = [1, 2, 3|Tail]-Tail,
    format('  Diff list: ~w~n', [DiffList1]),

    % 9. Unification (Prolog's power)
    nl,
    writeln('9. Unification (Pattern Matching):'),
    Point = point(X, Y),
    Point = point(3, 4),
    format('  Unified point(X,Y) with point(3,4): X=~w, Y=~w~n', [X, Y]),

    % 10. Key insights
    nl,
    writeln('10. Key Insights:'),
    writeln('  - Lists are fundamental to Prolog'),
    writeln('  - Pattern matching via unification'),
    writeln('  - Lists are immutable'),
    writeln('  - Terms provide structured data'),
    writeln('  - Recursive processing is natural'),
    writeln('  - No traditional "update" - create new structures'),

    halt(0).

% Helper predicates

% Sum list
sum_list([], 0).
sum_list([H|T], Sum) :-
    sum_list(T, RestSum),
    Sum is H + RestSum.

% Map double
map_double([], []).
map_double([H|T], [H2|T2]) :-
    H2 is H * 2,
    map_double(T, T2).

% Filter evens
filter_evens([], []).
filter_evens([H|T], [H|FilteredT]) :-
    0 is H mod 2,
    !,
    filter_evens(T, FilteredT).
filter_evens([_|T], FilteredT) :-
    filter_evens(T, FilteredT).
