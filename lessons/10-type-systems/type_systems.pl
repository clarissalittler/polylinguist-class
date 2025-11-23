% Lesson 10: Type Systems in Prolog
%
% Prolog features:
% - Untyped (no static type system)
% - Types checked at runtime via unification
% - Type checking predicates (integer/1, atom/1, etc.)
% - Logical types (terms, atoms, numbers, lists)
% - Some Prologs have type extensions (Mercury, SWI type checking)
%
% This demonstrates Prolog's approach to types.

:- initialization(main).

% ====================
% 1. Dynamic Typing
% ====================

increment(X, Y) :- Y is X + 1.

add(X, Y, Z) :- Z is X + Y.

% ====================
% 2. Type Checking Predicates
% ====================

% Built-in type checking
check_types(X) :-
    (   integer(X)
    ->  format('   ~w is an integer~n', [X])
    ;   atom(X)
    ->  format('   ~w is an atom~n', [X])
    ;   is_list(X)
    ->  format('   ~w is a list~n', [X])
    ;   format('   ~w is something else~n', [X])
    ).

% ====================
% 3. Compound Terms (Product Types)
% ====================

% Point as a compound term
point(X, Y) :- number(X), number(Y).

distance(point(X1, Y1), point(X2, Y2), D) :-
    D is sqrt((X2 - X1)^2 + (Y2 - Y1)^2).

% Person as a compound term
person(name(Name), age(Age)) :-
    atom(Name),
    integer(Age).

is_adult(person(_, age(Age))) :- Age >= 18.

% ====================
% 4. Sum Types (Via Functors)
% ====================

% Shape can be circle, rectangle, or triangle
shape(circle(R)) :- number(R), R > 0.
shape(rectangle(W, H)) :- number(W), number(H), W > 0, H > 0.
shape(triangle(A, B, C)) :- number(A), number(B), number(C).

area(circle(R), A) :-
    A is pi * R * R.
area(rectangle(W, H), A) :-
    A is W * H.
area(triangle(A, B, C), Area) :-
    S is (A + B + C) / 2,
    Area is sqrt(S * (S - A) * (S - B) * (S - C)).

% ====================
% 5. Lists (Polymorphic)
% ====================

% Generic list operations
safe_head([], nothing).
safe_head([H|_], just(H)).

safe_tail([], nothing).
safe_tail([_|T], just(T)).

list_sum([], 0).
list_sum([H|T], Sum) :-
    list_sum(T, RestSum),
    Sum is H + RestSum.

% ====================
% 6. Option Pattern
% ====================

% Maybe type simulation
maybe(nothing).
maybe(just(_)).

describe_maybe(nothing, "nothing").
describe_maybe(just(X), Description) :-
    format(string(Description), "just ~w", [X]).

% ====================
% 7. Type Guards (Runtime Checks)
% ====================

% Safe division with type checking
safe_divide(X, Y, just(Result)) :-
    number(X),
    number(Y),
    Y =\= 0,
    Result is X / Y.
safe_divide(_, 0, nothing).
safe_divide(X, Y, error("Arguments must be numbers")) :-
    (\+ number(X) ; \+ number(Y)).

% ====================
% 8. Generic Predicates
% ====================

% Works with any type
identity(X, X).

% Polymorphic pair
pair(X, Y, pair(X, Y)).

first_of_pair(pair(X, _), X).
second_of_pair(pair(_, Y), Y).

swap_pair(pair(X, Y), pair(Y, X)).

% ====================
% 9. Type Annotations (Comments)
% ====================

% In practice, types are documented in comments:
% increment(+Integer, -Integer)
% add(+Integer, +Integer, -Integer)
% safe_divide(+Number, +Number, -Maybe(Number))

% ====================
% 10. Typed Prolog Extensions
% ====================

% Some Prolog dialects support type declarations:
% :- type number ---> integer ; float.
% :- type maybe(T) ---> nothing ; just(T).
% :- pred safe_divide(number, number, maybe(number)).

% ====================
% Demonstration Predicates
% ====================

demonstrate_basic :-
    writeln('1. Basic Operations:'),
    increment(5, R1),
    format('   increment(5) = ~w~n', [R1]),
    add(3, 7, R2),
    format('   add(3, 7) = ~w~n', [R2]).

demonstrate_type_checks :-
    writeln('\n2. Type Checking Predicates:'),
    check_types(42),
    check_types(hello),
    check_types([1, 2, 3]).

demonstrate_compound :-
    writeln('\n3. Compound Terms:'),
    P = person(name('Alice'), age(30)),
    format('   ~w~n', [P]),
    (   is_adult(P)
    ->  writeln('   is_adult() = true')
    ;   writeln('   is_adult() = false')
    ).

demonstrate_shapes :-
    writeln('\n4. Sum Types (Shapes):'),
    Shapes = [circle(5), rectangle(4, 6), triangle(3, 4, 5)],
    forall(member(S, Shapes),
           (area(S, A), format('   ~w, area = ~2f~n', [S, A]))).

demonstrate_lists :-
    writeln('\n5. Lists (Polymorphic):'),
    safe_head([1, 2, 3], H1),
    safe_head([], H2),
    format('   safe_head([1,2,3]) = ~w~n', [H1]),
    format('   safe_head([]) = ~w~n', [H2]),
    list_sum([1, 2, 3, 4, 5], Sum),
    format('   sum([1,2,3,4,5]) = ~w~n', [Sum]).

demonstrate_maybe :-
    writeln('\n6. Option Pattern:'),
    describe_maybe(just(42), D1),
    describe_maybe(nothing, D2),
    format('   ~w~n', [D1]),
    format('   ~w~n', [D2]).

demonstrate_safe_divide :-
    writeln('\n7. Safe Division (Type Guards):'),
    safe_divide(10, 2, R1),
    safe_divide(10, 0, R2),
    format('   safe_divide(10, 2) = ~w~n', [R1]),
    format('   safe_divide(10, 0) = ~w~n', [R2]).

demonstrate_generic :-
    writeln('\n8. Generic Predicates:'),
    identity(42, I1),
    identity(hello, I2),
    format('   identity(42) = ~w~n', [I1]),
    format('   identity(hello) = ~w~n', [I2]),
    pair(1, "one", P),
    swap_pair(P, S),
    format('   swap(~w) = ~w~n', [P, S]).

% ====================
% Main
% ====================

main :-
    writeln('=== Type Systems in Prolog ===\n'),

    demonstrate_basic,
    demonstrate_type_checks,
    demonstrate_compound,
    demonstrate_shapes,
    demonstrate_lists,
    demonstrate_maybe,
    demonstrate_safe_divide,
    demonstrate_generic,

    nl,
    writeln('=== Prolog Type System Features ==='),
    writeln('- Untyped (no compile-time type checking)'),
    writeln('- Types checked at runtime via unification'),
    writeln('- Type predicates: integer/1, atom/1, is_list/1, etc.'),
    writeln('- Compound terms for structured data'),
    writeln('- Functors simulate sum types'),
    writeln('- Lists are polymorphic'),
    writeln('- Some dialects have type extensions (Mercury)'),
    writeln('- Type information in comments/documentation'),
    writeln('- Logical types: terms, atoms, numbers'),
    writeln('- Very flexible, no type safety'),

    halt(0).

:- catch(main, E, (writeln(E), halt(1))).
