% Lesson 7: Object-Oriented Programming in Prolog
%
% Prolog is a logic programming language - fundamentally different from OOP!
% However, we can model object-oriented concepts using:
% - Facts (to represent objects and their properties)
% - Rules (to represent methods/behaviors)
% - Predicates (to query object state)
%
% This demonstrates how different paradigms can model similar concepts.

:- initialization(main).

% ====================
% 1. Objects as Facts (Basic "Class")
% ====================

% person(Name, Age) - represents a Person object
person(alice, 30).
person(bob, 25).
person(charlie, 35).

% "Method" - introduce yourself
introduce(Person) :-
    person(Person, Age),
    format('   Hi, I\'m ~w, ~w years old~n', [Person, Age]).

% "Method" - check if adult
is_adult(Person) :-
    person(Person, Age),
    Age >= 18.

% ====================
% 2. "Inheritance" with Type Hierarchy
% ====================

% Base "class" - animal
animal(buddy, dog, canine).
animal(whiskers, cat, feline).
animal(max, dog, canine).
animal(tweety, bird, avian).

% "Subclass" properties
dog_breed(buddy, 'Golden Retriever').
dog_breed(max, 'German Shepherd').

cat_indoor(whiskers, true).

% "Method" - polymorphic speak
speak(Name, Sound) :-
    animal(Name, dog, _),
    format('   ~w says Woof!~n', [Name]).

speak(Name, Sound) :-
    animal(Name, cat, _),
    format('   ~w says Meow!~n', [Name]).

speak(Name, Sound) :-
    animal(Name, bird, _),
    format('   ~w says Tweet!~n', [Name]).

speak(Name, Sound) :-
    animal(Name, Type, _),
    \+ member(Type, [dog, cat, bird]),
    format('   ~w says Some generic animal sound~n', [Name]).

% "Method" - sleep (default implementation for all animals)
sleep(Name) :-
    animal(Name, _, _),
    format('   ~w is sleeping... Zzz~n', [Name]).

% Dog-specific "method"
fetch(Name) :-
    animal(Name, dog, _),
    format('   ~w is fetching the ball!~n', [Name]).

% Cat-specific "method"
scratch(Name) :-
    animal(Name, cat, _),
    format('   ~w is scratching the furniture!~n', [Name]).

% ====================
% 3. Shapes with Computed Properties
% ====================

% shape(ID, Type, Properties)
shape(circle1, circle, [radius-5]).
shape(rect1, rectangle, [width-4, height-6]).
shape(tri1, triangle, [side_a-3, side_b-4, side_c-5]).

% "Method" - calculate area (polymorphic)
area(ShapeID, Area) :-
    shape(ShapeID, circle, Props),
    member(radius-R, Props),
    Area is pi * R * R.

area(ShapeID, Area) :-
    shape(ShapeID, rectangle, Props),
    member(width-W, Props),
    member(height-H, Props),
    Area is W * H.

area(ShapeID, Area) :-
    shape(ShapeID, triangle, Props),
    member(side_a-A, Props),
    member(side_b-B, Props),
    member(side_c-C, Props),
    S is (A + B + C) / 2,
    Area is sqrt(S * (S - A) * (S - B) * (S - C)).

% "Method" - calculate perimeter
perimeter(ShapeID, Perim) :-
    shape(ShapeID, circle, Props),
    member(radius-R, Props),
    Perim is 2 * pi * R.

perimeter(ShapeID, Perim) :-
    shape(ShapeID, rectangle, Props),
    member(width-W, Props),
    member(height-H, Props),
    Perim is 2 * (W + H).

perimeter(ShapeID, Perim) :-
    shape(ShapeID, triangle, Props),
    member(side_a-A, Props),
    member(side_b-B, Props),
    member(side_c-C, Props),
    Perim is A + B + C.

% "Method" - describe shape
describe_shape(ShapeID) :-
    shape(ShapeID, Type, _),
    area(ShapeID, A),
    perimeter(ShapeID, P),
    format('   ~w: area=~2f, perimeter=~2f~n', [Type, A, P]).

% ====================
% 4. Encapsulation with Dynamic Predicates
% ====================

:- dynamic account/3.  % account(ID, AccountNumber, Balance)
:- dynamic transaction/2.  % transaction(ID, List)

% Initialize account counter
:- dynamic account_counter/1.
account_counter(0).

% "Constructor" - create new account
new_account(AccountNumber, InitialBalance, ID) :-
    retract(account_counter(Counter)),
    ID is Counter + 1,
    assert(account_counter(ID)),
    assert(account(ID, AccountNumber, InitialBalance)),
    assert(transaction(ID, [])).

% "Method" - deposit
deposit(ID, Amount) :-
    Amount > 0,
    retract(account(ID, AccNum, Balance)),
    NewBalance is Balance + Amount,
    assert(account(ID, AccNum, NewBalance)),
    retract(transaction(ID, Trans)),
    format(atom(Msg), 'Deposit: +$~2f', [Amount]),
    append(Trans, [Msg], NewTrans),
    assert(transaction(ID, NewTrans)).

% "Method" - withdraw
withdraw(ID, Amount) :-
    account(ID, AccNum, Balance),
    Amount > 0,
    Amount =< Balance,
    retract(account(ID, AccNum, Balance)),
    NewBalance is Balance - Amount,
    assert(account(ID, AccNum, NewBalance)),
    retract(transaction(ID, Trans)),
    format(atom(Msg), 'Withdrawal: -$~2f', [Amount]),
    append(Trans, [Msg], NewTrans),
    assert(transaction(ID, NewTrans)).

% "Method" - get balance
get_balance(ID, Balance) :-
    account(ID, _, Balance).

% "Method" - get transactions
get_transactions(ID, Trans) :-
    transaction(ID, Trans).

% ====================
% 5. Temperature Conversion (Functional-style)
% ====================

temperature(celsius, C, C).
temperature(fahrenheit, F, C) :- C is (F - 32) * 5 / 9.
temperature(kelvin, K, C) :- C is K - 273.15.

to_fahrenheit(C, F) :- F is C * 9 / 5 + 32.
to_kelvin(C, K) :- K is C + 273.15.
is_freezing(C) :- C =< 0.

% ====================
% 6. Composition
% ====================

% engine(EngineID, Horsepower, Running)
:- dynamic engine/3.
:- dynamic engine_counter/1.
engine_counter(0).

% Create new engine
new_engine(Horsepower, ID) :-
    retract(engine_counter(Counter)),
    ID is Counter + 1,
    assert(engine_counter(ID)),
    assert(engine(ID, Horsepower, false)).

% Start engine
start_engine(ID) :-
    retract(engine(ID, HP, _)),
    assert(engine(ID, HP, true)),
    format('Engine starting... ~whp engine now running', [HP]).

% Stop engine
stop_engine(ID) :-
    retract(engine(ID, HP, _)),
    assert(engine(ID, HP, false)),
    write('Engine stopped').

% car(CarID, Brand, Model, EngineID) - composition!
:- dynamic car/4.
:- dynamic car_counter/1.
car_counter(0).

% Create new car
new_car(Brand, Model, Horsepower, CarID) :-
    new_engine(Horsepower, EngineID),
    retract(car_counter(Counter)),
    CarID is Counter + 1,
    assert(car_counter(CarID)),
    assert(car(CarID, Brand, Model, EngineID)).

% Start car
start_car(CarID) :-
    car(CarID, Brand, Model, EngineID),
    format('   ~w ~w: ', [Brand, Model]),
    start_engine(EngineID),
    nl.

% Stop car
stop_car(CarID) :-
    car(CarID, Brand, Model, EngineID),
    format('   ~w ~w: ', [Brand, Model]),
    stop_engine(EngineID),
    nl.

% ====================
% 7. Design Pattern: Factory
% ====================

create_animal(dog, Name) :-
    assertz(animal(Name, dog, canine)),
    assertz(dog_breed(Name, 'Mixed')).

create_animal(cat, Name) :-
    assertz(animal(Name, cat, feline)),
    assertz(cat_indoor(Name, true)).

% ====================
% 8. Multiple "Interfaces" with Multiple Predicates
% ====================

% Duck can do multiple things
duck(donald).

% Duck implements Animal
animal(donald, duck, waterfowl).

% Duck-specific abilities
can_fly(donald).
can_swim(donald).
can_quack(donald).

% "Methods" for different abilities
fly(Name) :-
    can_fly(Name),
    format('   ~w is flying through the air~n', [Name]).

swim(Name) :-
    can_swim(Name),
    format('   ~w is swimming in water~n', [Name]).

quack(Name) :-
    can_quack(Name),
    format('   ~w says Quack!~n', [Name]).

% ====================
% 9. "Singleton" with Facts
% ====================

:- dynamic singleton_data/1.
singleton_data([]).

get_singleton_data(Data) :-
    singleton_data(Data).

add_singleton_data(Item) :-
    retract(singleton_data(Data)),
    append(Data, [Item], NewData),
    assert(singleton_data(NewData)).

% ====================
% 10. Point with Operations
% ====================

point(p1, 3, 4).
point(p2, 1, 2).

% "Methods"
add_points(P1, P2, point(X, Y)) :-
    point(P1, X1, Y1),
    point(P2, X2, Y2),
    X is X1 + X2,
    Y is Y1 + Y2.

subtract_points(P1, P2, point(X, Y)) :-
    point(P1, X1, Y1),
    point(P2, X2, Y2),
    X is X1 - X2,
    Y is Y1 - Y2.

distance_from_origin(P, Dist) :-
    point(P, X, Y),
    Dist is sqrt(X * X + Y * Y).

% ====================
% Main Demonstration
% ====================

main :-
    writeln('=== Object-Oriented Programming in Prolog ==='),
    nl,

    % 1. Basic "class"
    writeln('1. Objects as Facts:'),
    introduce(alice),
    (is_adult(alice) -> writeln('   Alice is an adult') ; writeln('   Alice is not an adult')),

    % 2. "Inheritance" and polymorphism
    nl,
    writeln('2. "Inheritance" and Polymorphism:'),
    speak(buddy, _),
    speak(whiskers, _),
    speak(max, _),
    fetch(buddy),
    scratch(whiskers),

    % 3. Shapes
    nl,
    writeln('3. Shapes (Polymorphic Methods):'),
    describe_shape(circle1),
    describe_shape(rect1),
    describe_shape(tri1),

    % 4. Encapsulation (Bank Account)
    nl,
    writeln('4. Encapsulation (Bank Account):'),
    new_account('ACC001', 1000, AccID),
    get_balance(AccID, InitBal),
    format('   Initial balance: $~2f~n', [InitBal]),
    deposit(AccID, 500),
    get_balance(AccID, Bal1),
    format('   After deposit: $~2f~n', [Bal1]),
    withdraw(AccID, 200),
    get_balance(AccID, Bal2),
    format('   After withdrawal: $~2f~n', [Bal2]),
    get_transactions(AccID, Trans),
    format('   Transactions: ~w~n', [Trans]),

    % 5. Temperature
    nl,
    writeln('5. Temperature Conversion:'),
    temperature(celsius, 0, C1),
    to_fahrenheit(C1, F1),
    format('   0°C = ~1f°F~n', [F1]),
    temperature(fahrenheit, 32, C2),
    format('   32°F = ~1f°C~n', [C2]),
    temperature(kelvin, 273.15, C3),
    format('   273.15K = ~1f°C~n', [C3]),
    (is_freezing(0) -> writeln('   Is 0°C freezing? true') ; writeln('   Is 0°C freezing? false')),

    % 6. Composition
    nl,
    writeln('6. Composition:'),
    new_car('Toyota', 'Camry', 200, CarID),
    start_car(CarID),
    stop_car(CarID),

    % 7. Multiple "interfaces"
    nl,
    writeln('7. Multiple Abilities (Duck):'),
    quack(donald),
    fly(donald),
    swim(donald),

    % 8. Points
    nl,
    writeln('8. Point Operations:'),
    point(p1, X1, Y1),
    point(p2, X2, Y2),
    format('   p1 = point(~w, ~w)~n', [X1, Y1]),
    format('   p2 = point(~w, ~w)~n', [X2, Y2]),
    add_points(p1, p2, point(X3, Y3)),
    format('   p1 + p2 = point(~w, ~w)~n', [X3, Y3]),
    subtract_points(p1, p2, point(X4, Y4)),
    format('   p1 - p2 = point(~w, ~w)~n', [X4, Y4]),
    distance_from_origin(p1, Dist),
    format('   p1 distance from origin: ~2f~n', [Dist]),

    % 9. Singleton
    nl,
    writeln('9. Singleton Pattern:'),
    add_singleton_data(item1),
    get_singleton_data(Data1),
    format('   Singleton data: ~w~n', [Data1]),

    % 10. Factory
    nl,
    writeln('10. Factory Pattern:'),
    create_animal(dog, rover),
    create_animal(cat, mittens),
    speak(rover, _),
    speak(mittens, _),

    nl,
    writeln('11. Key Takeaway:'),
    writeln('   Prolog models OOP concepts through logic:'),
    writeln('   - Facts represent object state'),
    writeln('   - Rules represent methods/behavior'),
    writeln('   - Pattern matching provides polymorphism'),
    writeln('   - Dynamic predicates allow state changes'),

    % Exit successfully
    halt(0).

% Handle errors gracefully
:- catch(main, E, (writeln(E), halt(1))).
