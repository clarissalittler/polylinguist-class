# Lab 27: Logic Programming with Prolog

**Quarter 3, Week 7**
**Duration:** 90 minutes
**Format:** Exploratory, individual with discussion

## Overview

Prolog is radically different from everything you've learned. Instead of telling the computer HOW to solve a problem, you describe WHAT the problem is. The computer figures out the solution. This is logic programming.

## Objectives

By the end of this lab, you will:
- [ ] Write basic Prolog facts and rules
- [ ] Query a knowledge base
- [ ] Understand unification and backtracking
- [ ] Solve puzzles declaratively

## Setup

- Install SWI-Prolog: https://www.swi-prolog.org/
- Create file: `lab27.pl`
- Open in the Prolog console

---

## Part 1: Facts and Queries (20 minutes)

### Activity 1.1: Stating Facts

In Prolog, you start by stating facts about the world:

```prolog
% Facts about people (lowercase = atoms)
person(alice).
person(bob).
person(charlie).

% Facts about relationships
parent(alice, bob).     % Alice is a parent of Bob
parent(alice, charlie). % Alice is a parent of Charlie
parent(bob, diana).     % Bob is a parent of Diana

% Facts with properties
age(alice, 50).
age(bob, 25).
age(charlie, 22).
age(diana, 5).
```

### Activity 1.2: Asking Questions

Load your file and query:

```prolog
?- person(alice).
true.

?- person(eve).
false.

?- parent(alice, bob).
true.

?- parent(bob, charlie).
false.
```

### Activity 1.3: Variables (Finding Things)

Variables start with uppercase:

```prolog
?- parent(alice, X).
X = bob ;    % Press ; for more answers
X = charlie.

?- parent(X, diana).
X = bob.

?- parent(X, Y).
X = alice, Y = bob ;
X = alice, Y = charlie ;
X = bob, Y = diana.

?- age(alice, A).
A = 50.
```

### Activity 1.4: Your Turn - Animal Facts

Create facts about animals:

```prolog
% animal(Name).
% has_legs(Animal, NumLegs).
% can_fly(Animal).
% lives_in(Animal, Habitat).

% Example facts:
animal(dog).
animal(eagle).
animal(fish).
has_legs(dog, 4).
has_legs(eagle, 2).
has_legs(fish, 0).
can_fly(eagle).
lives_in(dog, land).
lives_in(eagle, air).
lives_in(fish, water).
```

Write queries to find:
1. All animals
2. Animals that can fly
3. Animals with 4 legs
4. Where the eagle lives

### ✅ Checkpoint 1

Verify:
- [ ] Can state facts
- [ ] Can query with constants
- [ ] Can query with variables

---

## Part 2: Rules (25 minutes)

### Activity 2.1: Defining Rules

Rules define relationships in terms of other relationships:

```prolog
% A grandparent rule
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
% Read as: X is grandparent of Z IF X is parent of Y AND Y is parent of Z

% Test it
?- grandparent(alice, diana).
true.

?- grandparent(X, diana).
X = alice.
```

### Activity 2.2: More Family Rules

```prolog
% Sibling: same parent, different person
sibling(X, Y) :-
    parent(P, X),
    parent(P, Y),
    X \= Y.  % X is not equal to Y

% Ancestor: parent, or parent of ancestor
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

% Test
?- sibling(bob, charlie).
true.

?- ancestor(alice, diana).
true.

?- ancestor(X, diana).
X = bob ;
X = alice.
```

### Activity 2.3: Practice - Define These Rules

```prolog
% 1. Define: child(X, Y) - X is a child of Y
child(X, Y) :- ???.

% 2. Define: mother(X, Y) - X is mother of Y (add gender facts first)
female(alice).
male(bob).
male(charlie).
female(diana).

mother(X, Y) :- ???.

% 3. Define: cousin(X, Y) - X and Y are cousins
cousin(X, Y) :- ???.
```

### Activity 2.4: Arithmetic

```prolog
% Prolog uses 'is' for arithmetic evaluation
?- X is 3 + 4.
X = 7.

?- X is 10 / 2.
X = 5.

% Define a rule using arithmetic
older(X, Y) :- age(X, AgeX), age(Y, AgeY), AgeX > AgeY.

?- older(alice, bob).
true.

% Age difference
age_diff(X, Y, Diff) :-
    age(X, AgeX),
    age(Y, AgeY),
    Diff is abs(AgeX - AgeY).

?- age_diff(alice, diana, D).
D = 45.
```

### ✅ Checkpoint 2

Verify:
- [ ] Can write rules with :-
- [ ] Understand recursive rules (ancestor)
- [ ] Can use arithmetic

---

## Part 3: Lists and Recursion (25 minutes)

### Activity 3.1: List Basics

```prolog
% Lists use [Head|Tail] syntax
?- [H|T] = [1, 2, 3, 4].
H = 1,
T = [2, 3, 4].

?- [A, B|Rest] = [1, 2, 3, 4].
A = 1,
B = 2,
Rest = [3, 4].

?- [X|_] = [hello, world].
X = hello.
```

### Activity 3.2: List Operations

```prolog
% Member: is X in the list?
member(X, [X|_]).           % X is the head
member(X, [_|T]) :- member(X, T).  % X is in the tail

?- member(3, [1, 2, 3, 4]).
true.

?- member(X, [a, b, c]).
X = a ;
X = b ;
X = c.

% Length of a list
len([], 0).
len([_|T], N) :- len(T, N1), N is N1 + 1.

?- len([1, 2, 3], L).
L = 3.

% Append two lists
app([], L, L).
app([H|T], L, [H|R]) :- app(T, L, R).

?- app([1, 2], [3, 4], Result).
Result = [1, 2, 3, 4].

% Magic: append can also split!
?- app(X, Y, [1, 2, 3]).
X = [], Y = [1, 2, 3] ;
X = [1], Y = [2, 3] ;
X = [1, 2], Y = [3] ;
X = [1, 2, 3], Y = [].
```

### Activity 3.3: More List Functions

```prolog
% Reverse a list
rev([], []).
rev([H|T], R) :- rev(T, RT), app(RT, [H], R).

% More efficient reverse with accumulator
rev_acc(L, R) :- rev_acc(L, [], R).
rev_acc([], Acc, Acc).
rev_acc([H|T], Acc, R) :- rev_acc(T, [H|Acc], R).

?- rev([1, 2, 3], R).
R = [3, 2, 1].

% Sum of a list
sum([], 0).
sum([H|T], S) :- sum(T, S1), S is H + S1.

?- sum([1, 2, 3, 4], S).
S = 10.

% Max of a list
max_list([X], X).
max_list([H|T], Max) :-
    max_list(T, MaxT),
    Max is max(H, MaxT).

?- max_list([3, 1, 4, 1, 5, 9], M).
M = 9.
```

### Activity 3.4: Practice - List Functions

```prolog
% 1. Define: last(X, L) - X is the last element of L
last(X, L) :- ???.

% 2. Define: nth(N, L, X) - X is the Nth element (1-indexed)
nth(1, [H|_], H).
nth(N, [_|T], X) :- ???.

% 3. Define: remove(X, L, R) - R is L with X removed (first occurrence)
remove(X, L, R) :- ???.
```

### ✅ Checkpoint 3

Verify:
- [ ] Understand list pattern matching
- [ ] Can write recursive list functions
- [ ] Completed practice problems

---

## Part 4: Puzzles! (15 minutes)

### Activity 4.1: The Zebra Puzzle (Simplified)

```prolog
% There are 3 houses in a row (1, 2, 3).
% Each has a different color, nationality, and pet.

% Facts we'll use
color(red). color(blue). color(green).
nationality(english). nationality(spanish). nationality(japanese).
pet(dog). pet(cat). pet(fish).

% A house is: house(Color, Nationality, Pet)

% Solve the puzzle
puzzle(Houses) :-
    Houses = [house(_, _, _), house(_, _, _), house(_, _, _)],

    % The English person lives in the red house
    member(house(red, english, _), Houses),

    % The Spanish person has a dog
    member(house(_, spanish, dog), Houses),

    % The Japanese person lives in house 2
    Houses = [_, house(_, japanese, _), _],

    % The green house is immediately to the right of the blue house
    nextto(house(blue, _, _), house(green, _, _), Houses),

    % All colors are different
    member(house(red, _, _), Houses),
    member(house(blue, _, _), Houses),
    member(house(green, _, _), Houses),

    % All pets are different
    member(house(_, _, dog), Houses),
    member(house(_, _, cat), Houses),
    member(house(_, _, fish), Houses).

% nextto helper
nextto(X, Y, [X, Y|_]).
nextto(X, Y, [_|T]) :- nextto(X, Y, T).

% Query: ?- puzzle(H).
```

### Activity 4.2: Number Puzzle

```prolog
% Find X, Y, Z such that:
% X + Y + Z = 10
% X * Y * Z = 24
% X < Y < Z
% All are positive integers

puzzle_nums(X, Y, Z) :-
    between(1, 10, X),
    between(1, 10, Y),
    between(1, 10, Z),
    X < Y,
    Y < Z,
    X + Y + Z =:= 10,
    X * Y * Z =:= 24.

?- puzzle_nums(X, Y, Z).
% X = 2, Y = 3, Z = 4
```

### Activity 4.3: Family Tree Query

```prolog
% Given this family tree, write queries:

parent(tom, mary).
parent(tom, james).
parent(mary, ann).
parent(mary, fred).
parent(james, susan).

% Find all grandchildren of tom
% Find all people with siblings
% Find the family tree depth
```

---

## Part 5: How Prolog Works (5 minutes)

### Unification

Prolog matches patterns through unification:

```prolog
?- foo(X, b) = foo(a, Y).
X = a, Y = b.

?- [H|T] = [1, 2, 3].
H = 1, T = [2, 3].
```

### Backtracking

When Prolog hits a dead end, it backtracks:

```prolog
?- member(X, [1, 2, 3]), X > 1.
X = 2 ;
X = 3.
% Prolog tried X=1, failed X>1, backtracked, tried X=2, succeeded, etc.
```

---

## Challenges

### Challenge 1: Sudoku Solver

Write a Prolog program that solves 4x4 Sudoku puzzles.

### Challenge 2: Path Finding

Given a graph as edges, find all paths between two nodes.

### Challenge 3: Natural Language

Parse simple English sentences like "the cat sat on the mat."

---

## Wrap-Up

**Key takeaways:**

1. **Declarative** - Describe WHAT, not HOW
2. **Facts** - State what's true
3. **Rules** - Define relationships
4. **Unification** - Pattern matching
5. **Backtracking** - Try alternatives

**Prolog is great for:**
- Knowledge bases
- Expert systems
- Constraint solving
- Natural language processing
- Logic puzzles

**Paradigm comparison:**

| Imperative | Functional | Logic |
|------------|------------|-------|
| How to compute | What to compute | What is true |
| Statements | Expressions | Relations |
| Variables change | Immutable | Unification |

**Next lab:** Rust Programming - safety through ownership!
