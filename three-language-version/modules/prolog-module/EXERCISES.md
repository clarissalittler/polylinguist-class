# Prolog Module Exercises

## Warmup Exercises

### Exercise P.W1: Simple Facts and Queries
Create a database of students and courses:
```prolog
% Add facts:
% student(Name).
% course(CourseName).
% enrolled(Student, Course).

% Then answer these queries:
% - Who is enrolled in cs101?
% - What courses is alice enrolled in?
% - Are bob and alice in any same course?
```

### Exercise P.W2: Simple Rules
```prolog
% Given facts about animals:
animal(dog).
animal(cat).
animal(bird).
animal(fish).

has_legs(dog).
has_legs(cat).
has_legs(bird).

can_fly(bird).

lives_in_water(fish).

% Write rules for:
% mammal(X) - dogs and cats are mammals
% pet(X) - all these animals are pets
% land_animal(X) - has legs and doesn't live in water
```

### Exercise P.W3: Arithmetic
```prolog
% Write these predicates:

% double(X, Y) - Y is X * 2
double(X, Y) :- ...

% abs_value(X, Y) - Y is absolute value of X
abs_value(X, Y) :- ...

% between(X, Low, High) - true if Low <= X <= High
between(X, Low, High) :- ...

% sum_to_n(N, Sum) - Sum is 1+2+...+N
sum_to_n(N, Sum) :- ...
```

---

## Standard Exercises

### Exercise P.S1: Family Tree
Extend this family database:
```prolog
parent(tom, mary).
parent(tom, john).
parent(mary, ann).
parent(mary, pat).
parent(pat, jim).
parent(pat, eve).

male(tom).
male(john).
male(jim).
male(pat).

female(mary).
female(ann).
female(eve).
```

Write rules for:
```prolog
father(X, Y)       % X is father of Y
mother(X, Y)       % X is mother of Y
grandparent(X, Y)  % X is grandparent of Y
grandmother(X, Y)  % X is grandmother of Y
grandfather(X, Y)  % X is grandfather of Y
sibling(X, Y)      % X and Y are siblings (share a parent)
brother(X, Y)      % X is brother of Y
sister(X, Y)       % X is sister of Y
uncle(X, Y)        % X is uncle of Y
aunt(X, Y)         % X is aunt of Y
cousin(X, Y)       % X and Y are cousins
ancestor(X, Y)     % X is ancestor of Y
```

### Exercise P.S2: List Basics
Implement without using built-ins:
```prolog
% my_length(List, Length)
my_length([], 0).
my_length([_|T], N) :- ...

% my_member(X, List) - X is in List
my_member(X, [X|_]).
my_member(X, [_|T]) :- ...

% my_append(L1, L2, Result)
my_append([], L, L).
my_append([H|T], L, [H|R]) :- ...

% my_reverse(List, Reversed)
my_reverse([], []).
my_reverse([H|T], R) :- ...

% my_last(X, List) - X is last element
my_last(X, [X]).
my_last(X, [_|T]) :- ...
```

### Exercise P.S3: More List Operations
```prolog
% my_nth(N, List, X) - X is Nth element (0-indexed)
my_nth(0, [H|_], H).
my_nth(N, [_|T], X) :- ...

% my_sum(List, Sum)
my_sum([], 0).
my_sum([H|T], Sum) :- ...

% my_max(List, Max) - assumes non-empty list
my_max([X], X).
my_max([H|T], Max) :- ...

% remove_all(X, List, Result) - remove all occurrences of X
remove_all(_, [], []).
remove_all(X, [X|T], R) :- ...
remove_all(X, [H|T], [H|R]) :- ...

% flatten(NestedList, FlatList)
flatten([], []).
flatten([H|T], F) :- ...
```

### Exercise P.S4: Sorting
```prolog
% insert_sorted(X, SortedList, Result) - insert X in sorted position
insert_sorted(X, [], [X]).
insert_sorted(X, [H|T], [X,H|T]) :- X =< H.
insert_sorted(X, [H|T], [H|R]) :- X > H, ...

% insertion_sort(List, Sorted)
insertion_sort([], []).
insertion_sort([H|T], Sorted) :- ...

% Bonus: implement quicksort
quicksort([], []).
quicksort([H|T], Sorted) :- ...
```

### Exercise P.S5: Permutations and Combinations
```prolog
% permutation(List, Perm) - Perm is a permutation of List
permutation([], []).
permutation(L, [H|T]) :- ...

% subset(Set, Subset) - Subset is a subset of Set
subset([], []).
subset([H|T], [H|S]) :- ...
subset([_|T], S) :- ...

% select_n(N, List, Selected) - select N elements from List
select_n(0, _, []).
select_n(N, [H|T], [H|S]) :- ...
```

---

## Advanced Exercises

### Exercise P.A1: Graph Problems
```prolog
% Given a graph:
edge(a, b).
edge(a, c).
edge(b, d).
edge(c, d).
edge(d, e).

% Write:
% path(X, Y, Path) - Path is a path from X to Y
path(X, X, [X]).
path(X, Y, [X|P]) :- ...

% path_no_cycle(X, Y, Path) - path without revisiting nodes
% Hint: track visited nodes

% connected(X, Y) - X and Y are connected
connected(X, Y) :- ...

% all_paths(X, Y, Paths) - find all paths from X to Y
% Hint: use findall/3
```

### Exercise P.A2: Arithmetic Puzzles
```prolog
% cryptarithmetic: SEND + MORE = MONEY
% Each letter is a digit 0-9, all different
% S and M cannot be 0

send_more_money(S, E, N, D, M, O, R, Y) :-
    % Generate and test approach
    ...
```

### Exercise P.A3: N-Queens
```prolog
% Place N queens on NxN board so none attack each other
% Represent solution as list of column positions

queens(N, Queens) :-
    length(Queens, N),
    % Queens are on different columns (1 to N)
    % Queens are on different diagonals
    ...

% no_attack(Q, Qs, D) - Q doesn't attack any queen in Qs
% D is the diagonal distance
no_attack(_, [], _).
no_attack(Q, [Q1|Qs], D) :- ...
```

### Exercise P.A4: Simple Interpreter
Build an interpreter for a simple expression language:
```prolog
% Expressions:
% num(N) - number literal
% add(E1, E2) - addition
% sub(E1, E2) - subtraction
% mul(E1, E2) - multiplication
% var(X) - variable reference
% let(X, E1, E2) - let X = E1 in E2

% eval(Expr, Env, Value) - evaluate Expr in environment Env
eval(num(N), _, N).
eval(add(E1, E2), Env, V) :- ...
eval(var(X), Env, V) :- ...
eval(let(X, E1, E2), Env, V) :- ...

% Example:
% ?- eval(let(x, num(5), add(var(x), num(3))), [], V).
% V = 8.
```

### Exercise P.A5: Type Checker
```prolog
% Type checking for a simple language
% Types: int, bool, fun(T1, T2)
% Expressions: num(N), true, false, add(E1,E2), if(C,T,E),
%              var(X), lambda(X, T, E), app(E1, E2)

% typecheck(Expr, Env, Type) - Expr has Type in environment Env

typecheck(num(_), _, int).
typecheck(true, _, bool).
typecheck(false, _, bool).
typecheck(add(E1, E2), Env, int) :- ...
typecheck(if(C, T, E), Env, Type) :- ...
typecheck(var(X), Env, Type) :- ...
typecheck(lambda(X, T, E), Env, fun(T, T2)) :- ...
typecheck(app(E1, E2), Env, T2) :- ...
```

---

## Challenge Exercises

### Exercise P.C1: Sudoku Solver
```prolog
% Represent a Sudoku as a 9x9 list of lists
% 0 represents empty cells

sudoku(Puzzle, Solution) :-
    % Solution has same structure
    % All rows have digits 1-9 with no repeats
    % All columns have digits 1-9 with no repeats
    % All 3x3 boxes have digits 1-9 with no repeats
    ...
```

### Exercise P.C2: Natural Language
Build a simple natural language parser:
```prolog
% sentence(S) - S is a valid sentence
% noun_phrase(NP) - NP is a valid noun phrase
% verb_phrase(VP) - VP is a valid verb phrase

% Vocabulary
det(the).
det(a).
noun(cat).
noun(dog).
noun(mat).
verb(sat).
verb(ate).
prep(on).

% Grammar rules using difference lists
sentence(S0, S) :-
    noun_phrase(S0, S1),
    verb_phrase(S1, S).

% Example:
% ?- sentence([the, cat, sat, on, the, mat], []).
% true.
```

### Exercise P.C3: Constraint Logic Programming
Using CLP(FD) for constraint solving:
```prolog
:- use_module(library(clpfd)).

% Magic square: 3x3 grid where all rows, columns, diagonals sum to same value
magic_square([[A,B,C],[D,E,F],[G,H,I]]) :-
    Vars = [A,B,C,D,E,F,G,H,I],
    Vars ins 1..9,
    all_different(Vars),
    % All rows sum to S
    % All columns sum to S
    % Both diagonals sum to S
    ...
```

---

## Debugging Exercise

### Exercise P.D1: Fix the Logic
These predicates have bugs. Find and fix them:

```prolog
% Bug 1: Should return length of list, but doesn't work
bad_length([], N) :- N = 0.
bad_length([_|T], N) :- bad_length(T, N1), N = N1 + 1.

% Bug 2: Should reverse a list
bad_reverse([], []).
bad_reverse([H|T], R) :- bad_reverse(T, RT), append(H, RT, R).

% Bug 3: Should find maximum, but infinite loops
bad_max([X], X).
bad_max([H|T], H) :- bad_max(T, M), H > M.
bad_max([H|T], M) :- bad_max(T, M), H =< M.

% Bug 4: Should check if list is sorted
bad_sorted([]).
bad_sorted([_]).
bad_sorted([A,B|T]) :- A < B, bad_sorted(T).
```

---

## Reflection Questions

1. How does Prolog's approach to computation differ from Python's or Haskell's?
2. What kinds of problems are natural to express in Prolog?
3. How does unification compare to pattern matching in Haskell?
4. What surprised you most about logic programming?
5. When would you choose Prolog for a real project?
