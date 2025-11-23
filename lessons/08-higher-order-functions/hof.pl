% Lesson 8: Higher-Order Functions in Prolog
%
% Prolog is logic-based, not functional, but still supports HOFs!
% - Predicates can take other predicates as arguments
% - maplist, include, exclude are built-in HOFs
% - call/N for calling predicates dynamically
% - findall, bagof, setof for collecting solutions
%
% Different paradigm, similar concepts!

:- initialization(main).

% ====================
% 1. Predicates as Arguments
% ====================

% Apply a unary predicate twice
apply_twice(Pred, X, Result) :-
    call(Pred, X, Temp),
    call(Pred, Temp, Result).

% Apply n times
apply_n_times(_, 0, X, X).
apply_n_times(Pred, N, X, Result) :-
    N > 0,
    call(Pred, X, Temp),
    N1 is N - 1,
    apply_n_times(Pred, N1, Temp, Result).

% Example predicates
add_one(X, Y) :- Y is X + 1.
double_it(X, Y) :- Y is X * 2.
square_it(X, Y) :- Y is X * X.

% ====================
% 2. Maplist - Transform Each Element
% ====================

demonstrate_maplist :-
    Numbers = [1, 2, 3, 4, 5],

    % maplist/3 - apply predicate to each element
    maplist(double_it, Numbers, Doubled),
    format('   Doubled: ~w~n', [Doubled]),

    % maplist/3 with lambda-like syntax (library)
    maplist([X,Y]>>(Y is X * X), Numbers, Squared),
    format('   Squared: ~w~n', [Squared]).

% ====================
% 3. Include/Exclude - Filter Elements
% ====================

is_even(X) :- 0 is X mod 2.
is_odd(X) :- 1 is X mod 2.
is_positive(X) :- X > 0.

demonstrate_filter :-
    Numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],

    % include = filter (keep matching)
    include(is_even, Numbers, Evens),
    format('   Evens: ~w~n', [Evens]),

    % exclude = inverse filter
    exclude(is_even, Numbers, Odds),
    format('   Odds: ~w~n', [Odds]),

    % Complex predicate
    include([X]>>(is_odd(X), X > 5), Numbers, BigOdds),
    format('   Big odds: ~w~n', [BigOdds]).

% ====================
% 4. Foldl - Reduce/Fold
% ====================

% Sum accumulator
sum_acc(X, Acc, Result) :- Result is Acc + X.

% Product accumulator
product_acc(X, Acc, Result) :- Result is Acc * X.

% Max accumulator
max_acc(X, Acc, Result) :- Result is max(X, Acc).

demonstrate_fold :-
    Numbers = [1, 2, 3, 4, 5],

    % foldl(Pred, List, Initial, Final)
    foldl(sum_acc, Numbers, 0, Sum),
    format('   Sum: ~w~n', [Sum]),

    foldl(product_acc, Numbers, 1, Product),
    format('   Product: ~w~n', [Product]),

    foldl(max_acc, Numbers, 0, Max),
    format('   Max: ~w~n', [Max]).

% ====================
% 5. Higher-Order Predicates
% ====================

% Apply predicate to all elements and collect results
apply_to_all(_, [], []).
apply_to_all(Pred, [H|T], [RH|RT]) :-
    call(Pred, H, RH),
    apply_to_all(Pred, T, RT).

% Filter with predicate
filter_list(_, [], []).
filter_list(Pred, [H|T], [H|RT]) :-
    call(Pred, H),
    !,
    filter_list(Pred, T, RT).
filter_list(Pred, [_|T], RT) :-
    filter_list(Pred, T, RT).

% ====================
% 6. Findall - Collecting Solutions
% ====================

% Define some facts
person(alice, 30).
person(bob, 25).
person(charlie, 35).
person(diana, 28).

age_category(Age, Category) :-
    (Age >= 30 -> Category = adult ; Category = young).

demonstrate_findall :-
    % Collect all names
    findall(Name, person(Name, _), Names),
    format('   All names: ~w~n', [Names]),

    % Collect ages over 25
    findall(Age, (person(_, Age), Age > 25), Ages),
    format('   Ages > 25: ~w~n', [Ages]),

    % Complex query with transformation
    findall(Category, (person(_, Age), age_category(Age, Category)), Categories),
    format('   Categories: ~w~n', [Categories]).

% ====================
% 7. Composition
% ====================

% Compose two predicates
compose(F, G, X, Z) :-
    call(G, X, Y),
    call(F, Y, Z).

demonstrate_composition :-
    compose(double_it, add_one, 5, Result),
    format('   compose(double, add_one, 5) = ~w~n', [Result]).

% ====================
% 8. Currying Simulation
% ====================

% Partial application simulation
multiply(X, Y, Result) :- Result is X * Y.

% Create a "specialized" version
times_three(Y, Result) :- multiply(3, Y, Result).
times_five(Y, Result) :- multiply(5, Y, Result).

demonstrate_currying :-
    times_three(7, R1),
    times_five(4, R2),
    format('   times_three(7) = ~w~n', [R1]),
    format('   times_five(4) = ~w~n', [R2]).

% ====================
% 9. All/Any (Like all/any in other languages)
% ====================

all_satisfy(_, []).
all_satisfy(Pred, [H|T]) :-
    call(Pred, H),
    all_satisfy(Pred, T).

any_satisfy(Pred, [H|_]) :-
    call(Pred, H), !.
any_satisfy(Pred, [_|T]) :-
    any_satisfy(Pred, T).

demonstrate_all_any :-
    Numbers = [1, 2, 3, 4, 5],

    (all_satisfy(is_positive, Numbers) ->
        format('   All positive: true~n') ;
        format('   All positive: false~n')),

    (any_satisfy(is_even, Numbers) ->
        format('   Any even: true~n') ;
        format('   Any even: false~n')).

% ====================
% 10. Partition - Split List
% ====================

partition_list(_, [], [], []).
partition_list(Pred, [H|T], [H|True], False) :-
    call(Pred, H),
    !,
    partition_list(Pred, T, True, False).
partition_list(Pred, [H|T], True, [H|False]) :-
    partition_list(Pred, T, True, False).

demonstrate_partition :-
    Numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
    partition_list(is_even, Numbers, Evens, Odds),
    format('   Partition evens: ~w~n', [Evens]),
    format('   Partition odds: ~w~n', [Odds]).

% ====================
% 11. Custom Map Implementation
% ====================

custom_map(_, [], []).
custom_map(Pred, [H|T], [RH|RT]) :-
    call(Pred, H, RH),
    custom_map(Pred, T, RT).

custom_filter(_, [], []).
custom_filter(Pred, [H|T], [H|RT]) :-
    call(Pred, H),
    !,
    custom_filter(Pred, T, RT).
custom_filter(Pred, [_|T], RT) :-
    custom_filter(Pred, T, RT).

custom_foldl(_, [], Acc, Acc).
custom_foldl(Pred, [H|T], Acc, Result) :-
    call(Pred, H, Acc, NewAcc),
    custom_foldl(Pred, T, NewAcc, Result).

demonstrate_custom :-
    Numbers = [1, 2, 3, 4, 5],

    custom_map(double_it, Numbers, Doubled),
    format('   custom_map: ~w~n', [Doubled]),

    custom_filter(is_even, Numbers, Evens),
    format('   custom_filter: ~w~n', [Evens]),

    custom_foldl(sum_acc, Numbers, 0, Sum),
    format('   custom_foldl: ~w~n', [Sum]).

% ====================
% 12. Real-World Example: Data Processing
% ====================

% Process user data
process_user([Name, Age], [ProcessedName, Category]) :-
    downcase_atom(Name, ProcessedName),
    (Age >= 18 -> Category = adult ; Category = minor).

demonstrate_pipeline :-
    Users = [[' ALICE ', 25], ['BOB', 17], ['CHARLIE ', 30]],

    % Filter active users (all in this case)
    % Map to process
    maplist(process_user, Users, Processed),
    format('   Processed users: ~w~n', [Processed]).

% ====================
% Main Demonstration
% ====================

main :-
    writeln('=== Higher-Order Functions in Prolog ===\n'),

    % 1. Predicates as arguments
    writeln('1. Predicates as Arguments:'),
    apply_twice(add_one, 5, R1),
    format('   apply_twice(add_one, 5) = ~w~n', [R1]),
    apply_n_times(double_it, 3, 2, R2),
    format('   apply_n_times(double_it, 3, 2) = ~w~n', [R2]),

    % 2. Maplist
    nl, writeln('2. Maplist - Transform Each Element:'),
    demonstrate_maplist,

    % 3. Include/Exclude
    nl, writeln('3. Include/Exclude - Filter Elements:'),
    demonstrate_filter,

    % 4. Foldl
    nl, writeln('4. Foldl - Combine to Single Value:'),
    demonstrate_fold,

    % 5. Findall
    nl, writeln('5. Findall - Collect Solutions:'),
    demonstrate_findall,

    % 6. Composition
    nl, writeln('6. Function Composition:'),
    demonstrate_composition,

    % 7. Currying
    nl, writeln('7. Partial Application (Currying):'),
    demonstrate_currying,

    % 8. All/Any
    nl, writeln('8. All/Any Predicates:'),
    demonstrate_all_any,

    % 9. Partition
    nl, writeln('9. Partition:'),
    demonstrate_partition,

    % 10. Custom implementations
    nl, writeln('10. Custom HOF Implementations:'),
    demonstrate_custom,

    % 11. Pipeline
    nl, writeln('11. Real-World Data Pipeline:'),
    demonstrate_pipeline,

    nl, writeln('12. Prolog\'s Unique Approach:'),
    writeln('   - Predicates as arguments (not functions)'),
    writeln('   - call/N for dynamic predicate calling'),
    writeln('   - maplist, include, exclude built-in'),
    writeln('   - findall for collecting solutions'),
    writeln('   - Logic-based, but supports functional concepts!'),

    halt(0).

:- catch(main, E, (writeln(E), halt(1))).
