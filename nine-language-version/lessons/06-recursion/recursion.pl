% Lesson 6: Recursion in Prolog
%
% Prolog is a logic programming language where recursion is FUNDAMENTAL.
% All iteration is done through recursion. Prolog has tail call optimization.
% Rules are defined recursively with base cases and recursive cases.

% ====================
% 1. Simple Recursion
% ====================

% Factorial
factorial(0, 1).  % Base case
factorial(N, Result) :-
    N > 0,
    N1 is N - 1,
    factorial(N1, SubResult),
    Result is N * SubResult.

% Tail-recursive factorial (more efficient)
factorial_tail(N, Result) :-
    factorial_tail(N, 1, Result).

factorial_tail(0, Acc, Acc).  % Base case
factorial_tail(N, Acc, Result) :-
    N > 0,
    N1 is N - 1,
    Acc1 is N * Acc,
    factorial_tail(N1, Acc1, Result).

% ====================
% 2. Fibonacci
% ====================

% Fibonacci (inefficient)
fibonacci(0, 0).
fibonacci(1, 1).
fibonacci(N, Result) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fibonacci(N1, F1),
    fibonacci(N2, F2),
    Result is F1 + F2.

% ====================
% 3. List Recursion
% ====================

% Sum of list
sum_list([], 0).  % Base case: empty list sums to 0
sum_list([Head|Tail], Sum) :-
    sum_list(Tail, SubSum),
    Sum is Head + SubSum.

% Length of list
list_length([], 0).
list_length([_|Tail], Length) :-
    list_length(Tail, SubLength),
    Length is SubLength + 1.

% Reverse list
reverse_list([], []).
reverse_list([Head|Tail], Reversed) :-
    reverse_list(Tail, ReversedTail),
    append(ReversedTail, [Head], Reversed).

% Tail-recursive reverse (more efficient)
reverse_tail(List, Reversed) :-
    reverse_tail(List, [], Reversed).

reverse_tail([], Acc, Acc).
reverse_tail([Head|Tail], Acc, Reversed) :-
    reverse_tail(Tail, [Head|Acc], Reversed).

% Maximum element
max_element([X], X).  % Base case: single element
max_element([Head|Tail], Max) :-
    max_element(Tail, TailMax),
    (Head > TailMax -> Max = Head ; Max = TailMax).

% ====================
% 4. List Membership
% ====================

% Check if element is in list
member_recursive(X, [X|_]).  % Base case: X is head
member_recursive(X, [_|Tail]) :-
    member_recursive(X, Tail).  % Recursive: check tail

% ====================
% 5. List Append
% ====================

% Append two lists
append_recursive([], List, List).  % Base case
append_recursive([Head|Tail], List2, [Head|Result]) :-
    append_recursive(Tail, List2, Result).

% ====================
% 6. Quicksort
% ====================

% Quicksort
quicksort([], []).  % Base case: empty list
quicksort([Pivot|Rest], Sorted) :-
    partition(Pivot, Rest, Smaller, Larger),
    quicksort(Smaller, SortedSmaller),
    quicksort(Larger, SortedLarger),
    append(SortedSmaller, [Pivot|SortedLarger], Sorted).

% Partition list around pivot
partition(_, [], [], []).
partition(Pivot, [X|Rest], [X|Smaller], Larger) :-
    X < Pivot,
    partition(Pivot, Rest, Smaller, Larger).
partition(Pivot, [X|Rest], Smaller, [X|Larger]) :-
    X >= Pivot,
    partition(Pivot, Rest, Smaller, Larger).

% ====================
% 7. Tower of Hanoi
% ====================

% Hanoi solver - prints moves
hanoi(1, Source, Target, _) :-
    format('   Move disk 1 from ~w to ~w~n', [Source, Target]).

hanoi(N, Source, Target, Auxiliary) :-
    N > 1,
    N1 is N - 1,
    hanoi(N1, Source, Auxiliary, Target),
    format('   Move disk ~w from ~w to ~w~n', [N, Source, Target]),
    hanoi(N1, Auxiliary, Target, Source).

% ====================
% 8. Tree Operations
% ====================

% Tree represented as: tree(Value, Left, Right) or nil
% Example: tree(5, tree(3, tree(1, nil, nil), tree(4, nil, nil)), tree(8, nil, tree(9, nil, nil)))

% Tree height
tree_height(nil, 0).
tree_height(tree(_, Left, Right), Height) :-
    tree_height(Left, LeftHeight),
    tree_height(Right, RightHeight),
    MaxHeight is max(LeftHeight, RightHeight),
    Height is MaxHeight + 1.

% Tree sum
tree_sum(nil, 0).
tree_sum(tree(Value, Left, Right), Sum) :-
    tree_sum(Left, LeftSum),
    tree_sum(Right, RightSum),
    Sum is Value + LeftSum + RightSum.

% Inorder traversal
inorder(nil, []).
inorder(tree(Value, Left, Right), Result) :-
    inorder(Left, LeftList),
    inorder(Right, RightList),
    append(LeftList, [Value|RightList], Result).

% ====================
% 9. Mutual Recursion
% ====================

% Check if even/odd using mutual recursion
is_even(0).
is_even(N) :-
    N > 0,
    N1 is N - 1,
    is_odd(N1).

is_odd(N) :-
    N > 0,
    not(is_even(N)).

% ====================
% 10. GCD
% ====================

% Greatest Common Divisor (Euclid's algorithm)
gcd_recursive(A, 0, A).
gcd_recursive(A, B, GCD) :-
    B > 0,
    R is A mod B,
    gcd_recursive(B, R, GCD).

% ====================
% 11. Range Generation
% ====================

% Generate list from Low to High
range(High, High, [High]).
range(Low, High, [Low|Rest]) :-
    Low < High,
    Next is Low + 1,
    range(Next, High, Rest).

% ====================
% 12. Map/Filter
% ====================

% Map: apply predicate to each element
map_list(_, [], []).
map_list(Pred, [Head|Tail], [NewHead|NewTail]) :-
    call(Pred, Head, NewHead),
    map_list(Pred, Tail, NewTail).

% Filter: keep elements that satisfy predicate
filter_list(_, [], []).
filter_list(Pred, [Head|Tail], [Head|FilteredTail]) :-
    call(Pred, Head),
    filter_list(Pred, Tail, FilteredTail).
filter_list(Pred, [_|Tail], FilteredTail) :-
    filter_list(Pred, Tail, FilteredTail).

% ====================
% Test Predicates
% ====================

% Run all tests
run_tests :-
    writeln('=== Recursion Examples in Prolog ==='),
    nl,

    % Factorial
    writeln('1. Factorial:'),
    factorial(5, F1),
    format('   factorial(5) = ~w~n', [F1]),
    factorial_tail(5, F2),
    format('   factorial_tail(5) = ~w~n', [F2]),

    % Fibonacci
    nl,
    writeln('2. Fibonacci:'),
    fibonacci(10, Fib),
    format('   fibonacci(10) = ~w~n', [Fib]),

    % List operations
    nl,
    writeln('3. List Operations:'),
    Numbers = [1, 2, 3, 4, 5],
    sum_list(Numbers, Sum),
    format('   sum_list(~w) = ~w~n', [Numbers, Sum]),
    list_length(Numbers, Len),
    format('   list_length(~w) = ~w~n', [Numbers, Len]),
    reverse_tail(Numbers, Rev),
    format('   reverse_list(~w) = ~w~n', [Numbers, Rev]),
    max_element(Numbers, Max),
    format('   max_element(~w) = ~w~n', [Numbers, Max]),

    % Quicksort
    nl,
    writeln('4. Quicksort:'),
    Unsorted = [3, 6, 8, 10, 1, 2, 1],
    quicksort(Unsorted, Sorted),
    format('   quicksort(~w) = ~w~n', [Unsorted, Sorted]),

    % Tower of Hanoi
    nl,
    writeln('5. Tower of Hanoi (3 disks):'),
    hanoi(3, a, c, b),

    % Tree
    nl,
    writeln('6. Binary Tree:'),
    Tree = tree(5,
                tree(3,
                     tree(1, nil, nil),
                     tree(4, nil, nil)),
                tree(8,
                     nil,
                     tree(9, nil, nil))),
    tree_height(Tree, Height),
    format('   tree_height() = ~w~n', [Height]),
    tree_sum(Tree, TreeSum),
    format('   tree_sum() = ~w~n', [TreeSum]),
    inorder(Tree, Inorder),
    format('   inorder_traversal() = ~w~n', [Inorder]),

    % Mutual recursion
    nl,
    writeln('7. Mutual Recursion:'),
    (is_even(10) -> Even10 = true ; Even10 = false),
    format('   is_even(10) = ~w~n', [Even10]),
    (is_even(7) -> Even7 = true ; Even7 = false),
    format('   is_even(7) = ~w~n', [Even7]),

    % GCD
    nl,
    writeln('8. Greatest Common Divisor:'),
    gcd_recursive(48, 18, GCD1),
    format('   gcd(48, 18) = ~w~n', [GCD1]),
    gcd_recursive(100, 35, GCD2),
    format('   gcd(100, 35) = ~w~n', [GCD2]),

    % Range
    nl,
    writeln('9. Range Generation:'),
    range(1, 10, R),
    format('   range(1, 10) = ~w~n', [R]).

% To run: swipl -s recursion.pl -g run_tests -t halt
