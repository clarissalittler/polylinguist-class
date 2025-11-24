# Prolog Module
## Logic Programming and Declarative Thinking

**Duration:** 1-2 weeks (Weeks 9-10)
**Prerequisites:** Lessons 1-8, especially Recursion
**Languages:** Prolog (SWI-Prolog)

---

## Overview

Prolog (1972) is a **logic programming** language where you describe *what* you want, not *how* to compute it. Instead of writing step-by-step instructions, you declare facts and rules, and Prolog finds solutions through logical inference.

### Why Learn Prolog?

1. **Completely different paradigm**: Not imperative, not functional—logical
2. **Declarative thinking**: Describe the problem, let the computer solve it
3. **Pattern matching and unification**: Core concepts used in many languages
4. **AI and expert systems**: Historical importance, still used today
5. **Mind-expanding**: Changes how you think about computation

### What Makes Prolog Different?

| Feature | Imperative (Python/C++) | Functional (Haskell) | Logic (Prolog) |
|---------|------------------------|---------------------|----------------|
| Focus | How to compute | What to compute (functions) | What is true |
| Control | Explicit loops/branches | Recursion | Automatic search |
| Variables | Mutable storage | Immutable bindings | Logic variables (unification) |
| Output | Return values | Return values | Query success/failure + bindings |

---

## Installation

### Install SWI-Prolog

**macOS:**
```bash
brew install swi-prolog
```

**Linux:**
```bash
sudo apt-get install swi-prolog
```

**Windows:**
Download from https://www.swi-prolog.org/download/

### Verify Installation
```bash
swipl --version
```

---

## Part 1: Facts and Queries (Day 1)

### Facts

Facts are basic statements that are true:

```prolog
% family.pl - Facts about a family

% parent(Parent, Child) means Parent is a parent of Child
parent(tom, mary).
parent(tom, john).
parent(mary, ann).
parent(mary, pat).
parent(pat, jim).

% male(Person) means Person is male
male(tom).
male(john).
male(jim).
male(pat).

% female(Person) means Person is female
female(mary).
female(ann).
```

### Queries

Load the file and ask questions:

```prolog
?- [family].        % Load the file
true.

?- parent(tom, mary).   % Is tom a parent of mary?
true.

?- parent(tom, ann).    % Is tom a parent of ann?
false.

?- parent(tom, X).      % Who are tom's children?
X = mary ;              % Press ; for more solutions
X = john.

?- parent(X, ann).      % Who is ann's parent?
X = mary.

?- parent(X, Y).        % All parent-child pairs
X = tom, Y = mary ;
X = tom, Y = john ;
X = mary, Y = ann ;
...
```

---

## Part 2: Rules (Day 1-2)

### Defining Rules

Rules express relationships in terms of other relationships:

```prolog
% A grandparent is a parent of a parent
grandparent(X, Z) :-
    parent(X, Y),
    parent(Y, Z).

% A sibling shares a parent
sibling(X, Y) :-
    parent(P, X),
    parent(P, Y),
    X \= Y.  % X not equal to Y

% An ancestor is a parent, or a parent of an ancestor
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :-
    parent(X, Z),
    ancestor(Z, Y).
```

### Using Rules

```prolog
?- grandparent(tom, ann).
true.

?- grandparent(tom, X).
X = ann ;
X = pat ;
X = jim.

?- sibling(mary, john).
true.

?- ancestor(tom, jim).
true.
```

---

## Part 3: How Prolog Works (Day 2)

### Unification

Prolog matches terms through **unification**:

```prolog
?- X = 5.           % X unifies with 5
X = 5.

?- foo(X, 3) = foo(2, Y).   % Pattern matching
X = 2, Y = 3.

?- [H|T] = [1, 2, 3].       % List destructuring
H = 1, T = [2, 3].

?- [A, B, C] = [1, 2, 3].
A = 1, B = 2, C = 3.
```

### Backtracking

Prolog automatically tries alternatives when a path fails:

```prolog
% Given:
color(red).
color(green).
color(blue).

?- color(X).
X = red ;    % Press ; to see next
X = green ;
X = blue.
```

Prolog:
1. Tries `color(red)` → succeeds, reports X = red
2. On `;`, **backtracks** and tries `color(green)` → succeeds
3. On `;`, backtracks and tries `color(blue)` → succeeds
4. No more alternatives

### Cut (!)

The **cut** (`!`) prevents backtracking:

```prolog
first_solution(X) :-
    color(X),
    !.  % Don't try other colors

?- first_solution(X).
X = red.  % Only one solution, no backtracking
```

---

## Part 4: Lists (Day 2-3)

### List Syntax

```prolog
% Empty list
[]

% Non-empty list
[1, 2, 3]

% Head|Tail notation
[H|T]       % H is first element, T is rest
[1|[2,3]]   % Same as [1, 2, 3]
[1,2|[3]]   % Same as [1, 2, 3]
```

### List Operations

```prolog
% Length of a list
my_length([], 0).
my_length([_|T], N) :-
    my_length(T, N1),
    N is N1 + 1.

% Membership
my_member(X, [X|_]).
my_member(X, [_|T]) :- my_member(X, T).

% Append two lists
my_append([], L, L).
my_append([H|T], L, [H|R]) :-
    my_append(T, L, R).

% Reverse a list
my_reverse([], []).
my_reverse([H|T], R) :-
    my_reverse(T, RT),
    my_append(RT, [H], R).
```

### Using List Predicates

```prolog
?- my_length([a, b, c], N).
N = 3.

?- my_member(b, [a, b, c]).
true.

?- my_member(X, [a, b, c]).
X = a ; X = b ; X = c.

?- my_append([1, 2], [3, 4], R).
R = [1, 2, 3, 4].

% Prolog can run predicates "backwards"!
?- my_append(X, Y, [1, 2, 3]).
X = [], Y = [1, 2, 3] ;
X = [1], Y = [2, 3] ;
X = [1, 2], Y = [3] ;
X = [1, 2, 3], Y = [].
```

---

## Part 5: Arithmetic (Day 3)

### The `is` Operator

```prolog
?- X is 3 + 4.
X = 7.

?- X is 10 / 3.
X = 3.3333333333333335.

?- X is 10 // 3.    % Integer division
X = 3.

?- X is 10 mod 3.
X = 1.

?- X is 2 ** 10.    % Exponentiation
X = 1024.
```

**Important**: `is` evaluates the right side. `=` does unification!

```prolog
?- X = 3 + 4.       % Unification, not arithmetic
X = 3+4.            % X is the term 3+4

?- X is 3 + 4.      % Arithmetic evaluation
X = 7.
```

### Comparison

```prolog
?- 3 < 5.
true.

?- 3 > 5.
false.

?- 3 =:= 3.         % Arithmetic equality
true.

?- 3 =\= 5.         % Arithmetic inequality
true.

?- 3 >= 3.
true.
```

### Example: Factorial

```prolog
factorial(0, 1).
factorial(N, F) :-
    N > 0,
    N1 is N - 1,
    factorial(N1, F1),
    F is N * F1.

?- factorial(5, F).
F = 120.
```

### Example: Fibonacci

```prolog
fib(0, 0).
fib(1, 1).
fib(N, F) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fib(N1, F1),
    fib(N2, F2),
    F is F1 + F2.

?- fib(10, F).
F = 55.
```

---

## Part 6: Classic Problems (Day 3-4)

### Map Coloring

Color a map so no adjacent regions have the same color:

```prolog
color(red).
color(green).
color(blue).
color(yellow).

% Map with regions A, B, C, D where:
% A-B, A-C, A-D, B-C, B-D adjacent

color_map(A, B, C, D) :-
    color(A), color(B), color(C), color(D),
    A \= B, A \= C, A \= D,
    B \= C, B \= D.

?- color_map(A, B, C, D).
A = red, B = green, C = blue, D = blue ;
...
```

### N-Queens Problem

Place N queens on an NxN chessboard so none attack each other:

```prolog
% Simplified version
queens(N, Qs) :-
    length(Qs, N),
    board(Qs, N),
    safe(Qs).

board([], _).
board([Q|Qs], N) :-
    between(1, N, Q),
    board(Qs, N).

safe([]).
safe([Q|Qs]) :-
    safe(Qs),
    no_attack(Q, Qs, 1).

no_attack(_, [], _).
no_attack(Q, [Q1|Qs], D) :-
    Q =\= Q1,
    abs(Q - Q1) =\= D,
    D1 is D + 1,
    no_attack(Q, Qs, D1).

?- queens(4, Qs).
Qs = [2, 4, 1, 3] ;
Qs = [3, 1, 4, 2].
```

### Path Finding

Find paths in a graph:

```prolog
edge(a, b).
edge(b, c).
edge(c, d).
edge(a, d).
edge(b, d).

path(X, X, [X]).
path(X, Y, [X|Path]) :-
    edge(X, Z),
    path(Z, Y, Path).

?- path(a, d, P).
P = [a, d] ;
P = [a, b, c, d] ;
P = [a, b, d].
```

---

## Part 7: Prolog vs Other Languages (Day 4)

### Comparing Factorial

**Python (Imperative):**
```python
def factorial(n):
    result = 1
    for i in range(1, n+1):
        result *= i
    return result
```

**Haskell (Functional):**
```haskell
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

**Prolog (Logic):**
```prolog
factorial(0, 1).
factorial(N, F) :-
    N > 0,
    N1 is N - 1,
    factorial(N1, F1),
    F is N * F1.
```

Key differences:
- Prolog defines **relations**, not functions
- The "answer" is a variable binding, not a return value
- Can query in multiple "directions"

### Running "Backwards"

```prolog
% Define addition as a relation
add(0, Y, Y).
add(s(X), Y, s(Z)) :- add(X, Y, Z).
% s(X) means "successor of X", so s(s(0)) = 2

?- add(s(s(0)), s(s(s(0))), R).   % 2 + 3 = ?
R = s(s(s(s(s(0))))).             % 5

?- add(X, s(s(0)), s(s(s(0)))).   % X + 2 = 3, what's X?
X = s(0).                          % 1

?- add(s(0), Y, s(s(s(0)))).      % 1 + Y = 3, what's Y?
Y = s(s(0)).                       % 2
```

This "bidirectional" nature is unique to logic programming!

---

## Exercises

### Exercise P1: Family Relations
Extend the family database with:
```prolog
% Define these predicates:
mother(X, Y)      % X is mother of Y
father(X, Y)      % X is father of Y
grandmother(X, Y) % X is grandmother of Y
uncle(X, Y)       % X is uncle of Y
cousin(X, Y)      % X and Y are cousins
```

### Exercise P2: List Operations
Implement without using built-ins:
```prolog
my_last(X, L)           % X is the last element of L
my_nth(N, L, X)         % X is the Nth element of L (0-indexed)
my_sum(L, S)            % S is the sum of list L
my_max(L, M)            % M is the maximum of list L
my_flatten(L, F)        % F is L flattened
```

### Exercise P3: Sorting
Implement:
```prolog
my_sort(L, S)           % S is L sorted (any algorithm)
insert_sorted(X, L, R)  % R is L with X inserted in order
```

### Exercise P4: Puzzles
Solve this puzzle:
```
A farmer has a fox, a chicken, and a bag of grain.
They need to cross a river in a boat that holds the farmer
and one item. The fox will eat the chicken if left alone,
and the chicken will eat the grain if left alone.
How does the farmer get everything across?
```

Represent the state and write rules to find a solution.

### Exercise P5: Simple Database
Create a database of books:
```prolog
book(Title, Author, Year, Genre).
```
Add facts and write queries for:
- All books by a specific author
- All books in a genre
- Books published after a certain year
- Authors who wrote in multiple genres

---

## Key Takeaways

1. **Prolog is declarative**: Describe *what*, not *how*
2. **Unification matches patterns**: Variables become bound through matching
3. **Backtracking searches for solutions**: Prolog tries alternatives automatically
4. **Relations are bidirectional**: Same predicate can be queried multiple ways
5. **Recursion is natural**: Most Prolog programs are recursive

---

## When to Use Prolog

**Good for:**
- Rule-based systems (expert systems, business rules)
- Natural language processing
- Symbolic AI and reasoning
- Constraint satisfaction problems
- Database queries (Prolog inspired SQL!)
- Puzzles and games

**Not great for:**
- Number crunching
- GUI applications
- Systems programming
- Large-scale data processing

---

## Resources

- **Learn Prolog Now!**: http://www.learnprolognow.org/ (free book)
- **SWI-Prolog Documentation**: https://www.swi-prolog.org/pldoc/
- **Adventure in Prolog**: Interactive tutorial
- **The Art of Prolog**: Classic textbook

---

## Reflection

After learning Prolog, you'll never think about computation the same way. The idea that you can describe problems and let the computer find solutions is powerful—and it appears in modified forms in many modern tools:
- SQL is declarative like Prolog
- Pattern matching in Haskell, Rust, and Scala
- Constraint solvers in optimization
- Logic in type systems

Even if you never write Prolog professionally, understanding logic programming expands your computational thinking toolkit.
