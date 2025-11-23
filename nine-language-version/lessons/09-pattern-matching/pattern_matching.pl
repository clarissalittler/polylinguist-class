% Lesson 9: Pattern Matching in Prolog
%
% Prolog has pattern matching at its core!
% - Unification is pattern matching
% - Clause heads are patterns
% - Pattern matching happens in queries
% - Backtracking explores all matches
% - Destructuring lists, terms, and compounds
%
% Pattern matching is THE fundamental operation in Prolog!

:- initialization(main).

% ====================
% 1. Basic Patterns (Clause Heads)
% ====================

describe_number(0, "zero").
describe_number(1, "one").
describe_number(2, "two").
describe_number(N, Result) :-
    format(string(Result), "many: ~w", [N]).

% ====================
% 2. Tuple Patterns (Compound Terms)
% ====================

describe_point(point(0, 0), "origin").
describe_point(point(0, Y), Result) :-
    format(string(Result), "on y-axis at y=~w", [Y]).
describe_point(point(X, 0), Result) :-
    format(string(Result), "on x-axis at x=~w", [X]).
describe_point(point(X, Y), Result) :-
    format(string(Result), "point at (~w, ~w)", [X, Y]).

% ====================
% 3. List Patterns
% ====================

describe_list([], "empty list").
describe_list([X], Result) :-
    format(string(Result), "single element: ~w", [X]).
describe_list([X, Y], Result) :-
    format(string(Result), "two elements: ~w, ~w", [X, Y]).
describe_list([First|Rest], Result) :-
    format(string(Result), "first: ~w, rest: ~w", [First, Rest]).

% ====================
% 4. Algebraic Data Types (Terms)
% ====================

% Shapes
area(circle(R), Area) :-
    Area is pi * R * R.
area(rectangle(W, H), Area) :-
    Area is W * H.
area(triangle(A, B, C), Area) :-
    S is (A + B + C) / 2,
    Area is sqrt(S * (S - A) * (S - B) * (S - C)).

describe_shape(circle(R), Result) :-
    format(string(Result), "Circle with radius ~w", [R]).
describe_shape(rectangle(W, H), Result) :-
    format(string(Result), "Rectangle ~wx~w", [W, H]).
describe_shape(triangle(A, B, C), Result) :-
    format(string(Result), "Triangle with sides ~w, ~w, ~w", [A, B, C]).

% ====================
% 5. Option Patterns
% ====================

describe_maybe(nothing, "nothing").
describe_maybe(just(X), Result) :-
    format(string(Result), "just ~w", [X]).

% ====================
% 6. Result Patterns
% ====================

describe_result(ok(Val), Result) :-
    format(string(Result), "success: ~w", [Val]).
describe_result(error(Err), Result) :-
    format(string(Result), "error: ~w", [Err]).

% ====================
% 7. Guards (Conditions in Body)
% ====================

classify(N, "negative") :- N < 0.
classify(0, "zero").
classify(N, "small positive") :- N > 0, N < 10.
classify(N, "medium positive") :- N >= 10, N < 100.
classify(N, "large positive") :- N >= 100.

% ====================
% 8. Multiple Clauses (OR patterns)
% ====================

is_weekend("saturday").
is_weekend("sunday").
is_weekend(Day) :-
    downcase_atom(Day, Lower),
    (Lower = saturday ; Lower = sunday).

% ====================
% 9. Expression Evaluator
% ====================

eval_expr(num(N), N).
eval_expr(add(L, R), Result) :-
    eval_expr(L, LVal),
    eval_expr(R, RVal),
    Result is LVal + RVal.
eval_expr(mul(L, R), Result) :-
    eval_expr(L, LVal),
    eval_expr(R, RVal),
    Result is LVal * RVal.
eval_expr(neg(E), Result) :-
    eval_expr(E, Val),
    Result is -Val.

% ====================
% 10. Nested Patterns
% ====================

describe_user(user(Name, Age, active), Result) :-
    Age >= 18,
    !,
    format(string(Result), "Active adult: ~w", [Name]).
describe_user(user(Name, _, active), Result) :-
    !,
    format(string(Result), "Active minor: ~w", [Name]).
describe_user(user(Name, _, inactive), Result) :-
    format(string(Result), "Inactive: ~w", [Name]).

% ====================
% 11. Destructuring in Queries
% ====================

% Extract components
first_of_three([A, _, _], A).
second_of_three([_, B, _], B).
third_of_three([_, _, C], C).

% Pattern in head and body
sum_of_pair([A, B], Sum) :-
    Sum is A + B.

% ====================
% 12. Complex Pattern Matching
% ====================

% Tree patterns
tree_height(empty, 0).
tree_height(node(_, Left, Right), Height) :-
    tree_height(Left, LH),
    tree_height(Right, RH),
    Height is 1 + max(LH, RH).

tree_contains(node(Val, _, _), Val) :- !.
tree_contains(node(_, Left, _), Val) :-
    tree_contains(Left, Val), !.
tree_contains(node(_, _, Right), Val) :-
    tree_contains(Right, Val).

% ====================
% 13. Anonymous Variables (_)
% ====================

has_three_elements([_, _, _]).
first_element([First|_], First).
last_two([_|[A, B]], [A, B]).

% ====================
% 14. As-Patterns (Unification)
% ====================

% Capture whole and parts
analyze_pair(Pair, Pair, First, Second) :-
    Pair = [First, Second].

% ====================
% 15. State Machine
% ====================

traffic_light_next(_, emergency, red).
traffic_light_next(red, timer, green).
traffic_light_next(green, timer, yellow).
traffic_light_next(yellow, timer, red).
traffic_light_next(State, _, State).  % No change for unknown actions

% ====================
% 16. Custom Data Structures
% ====================

% Command patterns
process_command(quit, "Quitting...").
process_command(move(Direction), Result) :-
    format(string(Result), "Moving ~w", [Direction]).
process_command(attack(Target, Damage), Result) :-
    format(string(Result), "Attacking ~w for ~w damage", [Target, Damage]).

% ====================
% 17. Partial Matching with Unbound Variables
% ====================

% Find matching elements
find_circle(circle(_), yes).
find_circle(_, no).

find_large_circle(circle(R), yes) :- R > 10.
find_large_circle(_, no).

% ====================
% 18. Difference Lists Pattern
% ====================

% Efficient list concatenation
append_dl(A-B, B-C, A-C).

% ====================
% 19. Record-like Patterns
% ====================

% Using compound terms as records
person_name(person(Name, _, _), Name).
person_age(person(_, Age, _), Age).
person_city(person(_, _, City), City).

adult(person(_, Age, _)) :- Age >= 18.

% ====================
% 20. Conditional Patterns with ->
% ====================

grade_letter(Score, Grade) :-
    (   Score >= 90 -> Grade = 'A'
    ;   Score >= 80 -> Grade = 'B'
    ;   Score >= 70 -> Grade = 'C'
    ;   Score >= 60 -> Grade = 'D'
    ;   Grade = 'F'
    ).

% ====================
% Main Demonstration
% ====================

demonstrate_basic :-
    writeln('1. Basic Patterns (Clause Heads):'),
    forall(member(N, [0, 1, 2, 5]),
           (describe_number(N, R), format('   ~w -> ~w~n', [N, R]))).

demonstrate_tuples :-
    writeln('\n2. Tuple Patterns:'),
    Points = [point(0, 0), point(0, 5), point(3, 0), point(2, 3)],
    forall(member(P, Points),
           (describe_point(P, R), format('   ~w -> ~w~n', [P, R]))).

demonstrate_lists :-
    writeln('\n3. List Patterns:'),
    Lists = [[], [1], [1, 2], [1, 2, 3, 4]],
    forall(member(L, Lists),
           (describe_list(L, R), format('   ~w -> ~w~n', [L, R]))).

demonstrate_shapes :-
    writeln('\n4. Algebraic Data Types (Shapes):'),
    Shapes = [circle(5), rectangle(4, 6), triangle(3, 4, 5)],
    forall(member(S, Shapes),
           (describe_shape(S, D), area(S, A),
            format('   ~w~n', [D]),
            format('   area = ~2f~n', [A]))).

demonstrate_options :-
    writeln('\n5. Option Patterns:'),
    describe_maybe(just(42), R1),
    describe_maybe(nothing, R2),
    format('   ~w~n', [R1]),
    format('   ~w~n', [R2]).

demonstrate_results :-
    writeln('\n6. Result Patterns:'),
    describe_result(ok(100), R1),
    describe_result(error("failed"), R2),
    format('   ~w~n', [R1]),
    format('   ~w~n', [R2]).

demonstrate_guards :-
    writeln('\n7. Guards (Conditions):'),
    forall(member(N, [-5, 0, 3, 50, 500]),
           (classify(N, R), format('   ~w -> ~w~n', [N, R]))).

demonstrate_weekend :-
    writeln('\n8. Multiple Clauses (OR patterns):'),
    forall(member(Day, ["Monday", "Saturday", "Sunday"]),
           (   is_weekend(Day)
           ->  format('   ~w is weekend? true~n', [Day])
           ;   format('   ~w is weekend? false~n', [Day])
           )).

demonstrate_expr :-
    writeln('\n9. Expression Evaluator:'),
    Expr1 = mul(add(num(2), num(3)), num(4)),
    eval_expr(Expr1, R1),
    format('   (2 + 3) * 4 = ~w~n', [R1]),
    Expr2 = neg(add(num(5), num(3))),
    eval_expr(Expr2, R2),
    format('   -(5 + 3) = ~w~n', [R2]).

demonstrate_users :-
    writeln('\n10. Nested Patterns (Users):'),
    Users = [user("Alice", 25, active), user("Bob", 16, active), user("Charlie", 30, inactive)],
    forall(member(U, Users),
           (describe_user(U, R), format('   ~w~n', [R]))).

demonstrate_trees :-
    writeln('\n11. Tree Patterns:'),
    Tree = node(5, node(3, empty, empty), node(7, empty, node(9, empty, empty))),
    tree_height(Tree, H),
    format('   Tree height: ~w~n', [H]),
    (   tree_contains(Tree, 7)
    ->  writeln('   Contains 7? true')
    ;   writeln('   Contains 7? false')
    ),
    (   tree_contains(Tree, 4)
    ->  writeln('   Contains 4? true')
    ;   writeln('   Contains 4? false')
    ).

demonstrate_commands :-
    writeln('\n12. Command Patterns:'),
    Commands = [quit, move(north), attack(orc, 10)],
    forall(member(C, Commands),
           (process_command(C, R), format('   ~w -> ~w~n', [C, R]))).

demonstrate_grades :-
    writeln('\n13. Conditional Patterns:'),
    forall(member(Score, [95, 85, 75, 65, 55]),
           (grade_letter(Score, G), format('   ~w -> ~w~n', [Score, G]))).

demonstrate_state_machine :-
    writeln('\n14. State Machine (Traffic Light):'),
    foldl(
        [Action, CurrentState, NextState]>>(
            traffic_light_next(CurrentState, Action, NextState),
            format('   After ~w: ~w~n', [Action, NextState])
        ),
        [timer, timer, timer, emergency],
        red,
        _
    ).

main :-
    writeln('=== Pattern Matching in Prolog ===\n'),

    demonstrate_basic,
    demonstrate_tuples,
    demonstrate_lists,
    demonstrate_shapes,
    demonstrate_options,
    demonstrate_results,
    demonstrate_guards,
    demonstrate_weekend,
    demonstrate_expr,
    demonstrate_users,
    demonstrate_trees,
    demonstrate_commands,
    demonstrate_grades,
    demonstrate_state_machine,

    nl,
    writeln('=== Prolog Pattern Matching Notes ==='),
    writeln('- Pattern matching IS unification'),
    writeln('- Clause heads are patterns'),
    writeln('- Automatic backtracking over all matches'),
    writeln('- Anonymous variables (_) for ignored parts'),
    writeln('- Guards in clause bodies'),
    writeln('- Compound terms for algebraic data types'),
    writeln('- List patterns with [Head|Tail]'),
    writeln('- Most natural pattern matching of any language!'),

    halt(0).

:- catch(main, E, (writeln(E), halt(1))).
