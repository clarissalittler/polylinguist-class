## Lesson 10: Type Systems - Exercises

This file contains exercises for understanding and working with different type systems across languages.

## Level 1: Basic Types and Type Checking

### Exercise 1: Type Conversions
Implement safe type conversion functions:
- String to Int (return Option/Maybe)
- Int to String
- Float to Int (with rounding)
- Handle conversion errors appropriately

**Concepts**: Type conversions, error handling, Option types

### Exercise 2: Type Predicates
Write functions that check types at runtime:
- is_numeric(value) → bool
- is_string(value) → bool
- is_collection(value) → bool
- describe_type(value) → string

**Concepts**: Runtime type checking, typeof/instanceof

### Exercise 3: Strong vs Weak Typing
Demonstrate the difference:
- In a weakly-typed language (C, JavaScript): show implicit coercions
- In a strongly-typed language (Python, Rust): show where coercions fail
- Document the behavior differences

**Concepts**: Type coercion, strong vs weak typing

### Exercise 4: Primitive Type Operations
Implement type-safe arithmetic:
- Add only integers
- Multiply only floats
- Concatenate only strings
- Return appropriate error for wrong types

**Concepts**: Type safety, primitive types

### Exercise 5: Boolean Logic Types
Create a truth table evaluator:
- AND, OR, NOT operations
- Type signature: `Bool -> Bool -> Bool`
- Handle only boolean inputs

**Concepts**: Boolean types, type constraints

## Level 2: Composite Types

### Exercise 6: Tuple Operations
Implement tuple utilities:
- swap: (A, B) → (B, A)
- first: (A, B) → A
- second: (A, B) → B
- zip: [A], [B] → [(A, B)]

**Concepts**: Tuple types, generic functions

### Exercise 7: Record/Struct Types
Define a Person record with:
- name: String
- age: Int
- email: Optional<String>

Implement:
- is_adult: Person → Bool
- update_email: Person, String → Person
- full_info: Person → String

**Concepts**: Structs/records, optional fields

### Exercise 8: Tagged Unions
Create a Shape type:
```
Shape = Circle(radius)
      | Rectangle(width, height)
      | Triangle(a, b, c)
```

Implement:
- area: Shape → Float
- perimeter: Shape → Float
- is_square: Shape → Bool (true if rectangle with equal sides)

**Concepts**: Sum types, pattern matching, algebraic data types

### Exercise 9: Array/List Types
Implement typed list functions:
- safe_get: List<T>, Int → Option<T>
- all_same_type: List<Any> → Bool
- type_filter: List<Any>, Type → List<T>

**Concepts**: Collection types, generics

### Exercise 10: Dictionary/Map Types
Create a type-safe key-value store:
- Dict<K, V> where K must be hashable
- get: Dict<K, V>, K → Option<V>
- keys: Dict<K, V> → List<K>
- values: Dict<K, V> → List<V>

**Concepts**: Map types, generic constraints

## Level 3: Generics and Polymorphism

### Exercise 11: Generic Box
Implement a generic Box<T>:
- new: T → Box<T>
- get: Box<T> → T
- map: Box<T>, (T → U) → Box<U>
- flat_map: Box<T>, (T → Box<U>) → Box<U>

**Concepts**: Generics, functors, monads

### Exercise 12: Generic Pair
Create Pair<A, B> with:
- map_first: Pair<A, B>, (A → C) → Pair<C, B>
- map_second: Pair<A, B>, (B → C) → Pair<A, C>
- map_both: Pair<A, B>, (A → C), (B → D) → Pair<C, D>

**Concepts**: Parametric polymorphism, type parameters

### Exercise 13: Bounded Generics
Implement functions with type bounds:
- max_value<T: Ord>(T, T) → T
- describe<T: Display>(T) → String
- sort<T: Ord>(List<T>) → List<T>

**Concepts**: Trait bounds, type constraints

### Exercise 14: Higher-Kinded Types
Implement map for different containers:
- map_option: Option<T>, (T → U) → Option<U>
- map_list: List<T>, (T → U) → List<U>
- map_result: Result<T, E>, (T → U) → Result<U, E>

**Concepts**: Functors, higher-kinded types

### Exercise 15: Variance
Explore covariance and contravariance:
- List<Cat> vs List<Animal> (covariant?)
- Function<Animal → T> vs Function<Cat → T> (contravariant in parameter?)
- Demonstrate with concrete examples

**Concepts**: Variance, subtyping

## Level 4: Option and Result Types

### Exercise 16: Option Chain
Implement Option chaining:
- and_then: Option<T>, (T → Option<U>) → Option<U>
- or_else: Option<T>, (→ Option<T>) → Option<T>
- unwrap_or: Option<T>, T → T
- map_or: Option<T>, U, (T → U) → U

**Concepts**: Option type, monadic operations

### Exercise 17: Result Handling
Implement Result operations:
- map: Result<T, E>, (T → U) → Result<U, E>
- map_err: Result<T, E>, (E → F) → Result<T, F>
- and_then: Result<T, E>, (T → Result<U, E>) → Result<U, E>
- unwrap_or_else: Result<T, E>, (E → T) → T

**Concepts**: Result type, error handling

### Exercise 18: Convert Between Types
Implement conversions:
- option_to_result: Option<T>, E → Result<T, E>
- result_to_option: Result<T, E> → Option<T>
- list_to_option: List<T> → Option<T> (first element)
- partition_results: List<Result<T, E>> → (List<T>, List<E>)

**Concepts**: Type conversions, pattern matching

### Exercise 19: Option Combinator Library
Build a library of Option combinators:
- all: List<Option<T>> → Option<List<T>> (Some if all Some)
- any: List<Option<T>> → Option<T> (first Some)
- filter: Option<T>, (T → Bool) → Option<T>
- flatten: Option<Option<T>> → Option<T>

**Concepts**: Combinators, functional programming

### Exercise 20: Railway-Oriented Programming
Implement function composition for Results:
- compose: (A → Result<B, E>), (B → Result<C, E>) → (A → Result<C, E>)
- chain multiple operations that can fail
- collect all errors or return success

**Concepts**: Result chaining, composition

## Level 5: Advanced Type Systems

### Exercise 21: Newtype Pattern
Create distinct types from the same underlying type:
```
UserId = Int
ProductId = Int
```

Ensure you can't accidentally mix them up.
Implement safe conversions if needed.

**Concepts**: Newtype, type safety

### Exercise 22: Phantom Types
Use phantom types to track state:
```
State<S, T> where S is never used in the data

State<Locked, Resource>
State<Unlocked, Resource>
```

Only allow unlock on Locked, lock on Unlocked.

**Concepts**: Phantom types, compile-time state tracking

### Exercise 23: Type-Level Programming
Use types to enforce constraints:
- NonEmpty list that can't be empty
- Positive numbers that must be > 0
- Email string that matches pattern
- Use types to make illegal states unrepresentable

**Concepts**: Type-level constraints, smart constructors

### Exercise 24: GADTs/Dependent Types
Create a type-safe expression evaluator:
```
Expr<T> where T is the result type

LitInt: Int → Expr<Int>
LitBool: Bool → Expr<Bool>
Add: Expr<Int>, Expr<Int> → Expr<Int>
If: Expr<Bool>, Expr<T>, Expr<T> → Expr<T>
```

Can't add booleans or use int as condition!

**Concepts**: GADTs, type safety

### Exercise 25: Type Classes/Traits
Define a type class hierarchy:
```
class Eq a where
    equals: a → a → Bool

class Eq a => Ord a where
    compare: a → a → Ordering

class Show a where
    show: a → String
```

Implement for several types.

**Concepts**: Type classes, inheritance

## Level 6: Practical Applications

### Exercise 26: Type-Safe Builder
Create a builder pattern where types ensure all fields are set:
```
UserBuilder<Name, Email, Age>

UserBuilder<(), (), ()>  // Nothing set
  .name("Alice")        // UserBuilder<String, (), ()>
  .email("a@b.com")     // UserBuilder<String, String, ()>
  .age(30)              // UserBuilder<String, String, Int>
  .build()              // User (only works if all set!)
```

**Concepts**: Phantom types, type-state pattern

### Exercise 27: Type-Safe Configuration
Build a configuration system where:
- Required fields must be set (compile-time check if possible)
- Optional fields have defaults
- Types validated at load time
- Invalid configs rejected

**Concepts**: Type validation, smart constructors

### Exercise 28: Typed Query Builder
Create a type-safe database query builder:
- Select fields (types known)
- Where clauses (type-checked)
- Joins (type-safe)
- Result type matches selected fields

**Concepts**: Type-level programming, DSLs

### Exercise 29: Protocol/Interface Implementation
Define protocols for:
- Drawable (has draw method)
- Comparable (has compare method)
- Iterable (can iterate)

Implement for multiple types.
Write generic functions using these protocols.

**Concepts**: Structural typing, protocols

### Exercise 30: Type-Safe Event System
Create an event system where:
- Event types are known at compile time
- Handlers match event types
- Can't register wrong handler for event
- Type-safe dispatch

**Concepts**: Type safety, event systems

## Bonus Challenges

### Challenge 1: Type Inference Engine
Implement a simple Hindley-Milner type inference algorithm for a mini language.

### Challenge 2: Type Checker
Build a type checker for a simple expression language with:
- Literals (int, bool, string)
- Variables
- Functions
- If expressions
- Let bindings

### Challenge 3: Gradual Typing System
Create a system that mixes typed and untyped code safely.

### Challenge 4: Effect System
Implement a type system that tracks effects (IO, State, Exceptions) in types.

### Challenge 5: Linear Types
Implement a system with linear types where values must be used exactly once.

## Testing Your Solutions

For each exercise:
1. **Type errors**: Try to create type errors and ensure they're caught
2. **Edge cases**: Test with empty inputs, nulls, edge values
3. **Composition**: Combine your functions in interesting ways
4. **Refactoring**: Can you make illegal states unrepresentable?

## Language-Specific Notes

### Python
- Use type hints (typing module)
- mypy for static checking
- dataclasses for structs
- TypeVar for generics

### Haskell
- Full type inference
- Type classes for constraints
- GADTs for advanced types
- Phantom types with data type promotion

### Rust
- Ownership types
- Trait bounds
- Associated types
- Lifetime parameters

### Java
- Generics with type erasure
- Interfaces for polymorphism
- Records for data classes
- Sealed classes for sum types

### JavaScript
- TypeScript for static types
- JSDoc for hints
- Runtime checks with typeof
- Duck typing

### Ruby
- Duck typing
- RBS for type signatures
- Sorbet for static checking
- Contracts for runtime checks

### C
- Static but weak
- typedef for aliases
- Tagged unions for sum types
- void* for "generics"

### Racket
- Typed Racket for static types
- Contracts for runtime checks
- Occurrence typing

### Prolog
- Untyped
- Type predicates for runtime checks
- Mercury for typed Prolog

## Reflection Questions

1. What guarantees does your language's type system provide?
2. What errors can be caught at compile time vs runtime?
3. How does the type system affect your design?
4. What are the trade-offs of your language's approach?
5. When would you choose static vs dynamic typing?
6. How do you handle null/absence safely?
7. What patterns make illegal states unrepresentable?

---

**Remember**: Type systems are tools to help you write correct code. Use them to:
- Catch errors early
- Document intent
- Guide design
- Enable refactoring
- Provide guarantees

Different type systems have different strengths. Understanding them helps you choose the right tool for the job.
