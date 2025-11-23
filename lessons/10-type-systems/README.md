# Lesson 10: Type Systems

## Introduction

A **type system** is a set of rules that assigns types to the various constructs of a computer program—such as variables, expressions, functions, and modules—and checks that these types are used correctly. Type systems help catch errors early, document code, and enable optimizations.

This lesson explores the spectrum of type systems across our nine languages, from dynamic to static, from weak to strong, and from simple to sophisticated.

## Why Type Systems Matter

1. **Error Detection**: Catch mistakes before runtime
2. **Documentation**: Types serve as machine-checked documentation
3. **Performance**: Enable compiler optimizations
4. **Refactoring**: Make large-scale changes safer
5. **Tooling**: Enable better IDE support (autocomplete, navigation)
6. **Design**: Force you to think about data structures

## Classification of Type Systems

### Static vs Dynamic Typing

**Static Typing**: Types are checked at compile time
- Examples: Java, C, Rust, Haskell
- Errors caught before execution
- Requires type annotations (or inference)
- Better tooling and performance

**Dynamic Typing**: Types are checked at runtime
- Examples: Python, JavaScript, Ruby
- More flexible, faster prototyping
- Errors may only appear during execution
- Less boilerplate

### Strong vs Weak Typing

**Strong Typing**: The language prevents (or warns about) type errors
- Examples: Python, Haskell, Rust
- Type errors cause exceptions or compile-time errors
- Fewer implicit conversions

**Weak Typing**: The language allows more implicit type conversions
- Examples: JavaScript, C
- More implicit coercions
- Can lead to subtle bugs

**The Spectrum**:
```
Weak ←―――――――――――――――――――――――――→ Strong
C → JavaScript → Ruby → Python → Java → Rust → Haskell
```

### Type Inference

Some statically-typed languages can deduce types automatically:
- **Haskell**: Full type inference (Hindley-Milner)
- **Rust**: Local type inference
- **Java 10+**: `var` for local variables
- **Python**: Optional type hints with inference tools

## Core Type System Concepts

### 1. Primitive Types

Basic built-in types:
- **Integers**: `int`, `i32`, `Integer`
- **Floating point**: `float`, `double`, `f64`
- **Booleans**: `bool`, `boolean`, `Bool`
- **Characters**: `char`, `Char`
- **Strings**: `str`, `String`

### 2. Composite Types

Types built from other types:

**Tuples**: Fixed-size collections of different types
```python
# Python
point: tuple[int, int] = (3, 4)
```

```rust
// Rust
let point: (i32, i32) = (3, 4);
```

**Arrays/Lists**: Collections of the same type
```java
// Java
int[] numbers = {1, 2, 3, 4, 5};
```

```haskell
-- Haskell
numbers :: [Int]
numbers = [1, 2, 3, 4, 5]
```

**Structs/Records**: Named fields with types
```rust
// Rust
struct Point {
    x: i32,
    y: i32,
}
```

```haskell
-- Haskell
data Point = Point { x :: Int, y :: Int }
```

### 3. Function Types

Functions have types too:

```haskell
-- Haskell: function from Int to Int
increment :: Int -> Int
increment x = x + 1

-- Function taking two Ints, returning Int
add :: Int -> Int -> Int
add x y = x + y
```

```rust
// Rust
fn increment(x: i32) -> i32 {
    x + 1
}

fn add(x: i32, y: i32) -> i32 {
    x + y
}
```

```python
# Python with type hints
def increment(x: int) -> int:
    return x + 1

def add(x: int, y: int) -> int:
    return x + y
```

### 4. Algebraic Data Types (ADTs)

Sum types (OR) and product types (AND):

**Sum Types** (tagged unions, variants):
```haskell
-- Haskell
data Shape = Circle Double
           | Rectangle Double Double
           | Triangle Double Double Double
```

```rust
// Rust
enum Shape {
    Circle { radius: f64 },
    Rectangle { width: f64, height: f64 },
    Triangle { a: f64, b: f64, c: f64 },
}
```

**Product Types** (structs, tuples):
```haskell
-- Haskell
data Point = Point Int Int
-- or
data Point = Point { x :: Int, y :: Int }
```

### 5. Generic Types (Parametric Polymorphism)

Types that work with any type:

```haskell
-- Haskell
-- A list that can hold any type
data List a = Nil | Cons a (List a)

-- Maybe type: either Just a value or Nothing
data Maybe a = Nothing | Just a
```

```rust
// Rust
enum Option<T> {
    None,
    Some(T),
}

enum Result<T, E> {
    Ok(T),
    Err(E),
}
```

```java
// Java
class Box<T> {
    private T value;
    public Box(T value) { this.value = value; }
    public T getValue() { return value; }
}
```

```python
# Python 3.9+
from typing import Generic, TypeVar

T = TypeVar('T')

class Box(Generic[T]):
    def __init__(self, value: T):
        self.value = value

    def get_value(self) -> T:
        return self.value
```

### 6. Type Classes and Traits

Constrained polymorphism - generics with requirements:

**Haskell Type Classes**:
```haskell
-- Define a type class
class Printable a where
    toString :: a -> String

-- Implement for a type
instance Printable Int where
    toString x = show x

-- Use with constraint
printValue :: Printable a => a -> IO ()
printValue x = putStrLn (toString x)
```

**Rust Traits**:
```rust
// Define a trait
trait Printable {
    fn to_string(&self) -> String;
}

// Implement for a type
impl Printable for i32 {
    fn to_string(&self) -> String {
        format!("{}", self)
    }
}

// Use with trait bound
fn print_value<T: Printable>(x: T) {
    println!("{}", x.to_string());
}
```

**Java Interfaces**:
```java
interface Printable {
    String toString();
}

class MyClass implements Printable {
    public String toString() {
        return "MyClass";
    }
}
```

### 7. Subtyping and Variance

How generic types relate to each other:

**Invariance**: `Box<Cat>` is not a subtype of `Box<Animal>`
**Covariance**: `List<Cat>` is a subtype of `List<Animal>` (safe for immutable)
**Contravariance**: `Consumer<Animal>` is a subtype of `Consumer<Cat>`

```java
// Java wildcards
List<? extends Animal> animals;  // Covariant
List<? super Cat> catConsumers;  // Contravariant
```

### 8. Type Inference

The compiler figures out types automatically:

```haskell
-- Haskell: full inference
add x y = x + y  -- Inferred: Num a => a -> a -> a

numbers = [1, 2, 3]  -- Inferred: [Integer]
```

```rust
// Rust: local inference
let x = 5;        // Inferred: i32
let numbers = vec![1, 2, 3];  // Inferred: Vec<i32>
```

```python
# Python: runtime types, but mypy can infer
x = 5  # mypy infers: int
numbers = [1, 2, 3]  # mypy infers: List[int]
```

### 9. Null Safety

How type systems handle absence of values:

**Option/Maybe Types** (safe):
```haskell
-- Haskell
data Maybe a = Nothing | Just a

safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide x y = Just (x `div` y)
```

```rust
// Rust: no null, use Option
fn safe_divide(x: i32, y: i32) -> Option<i32> {
    if y == 0 {
        None
    } else {
        Some(x / y)
    }
}
```

**Nullable Types** (less safe):
```java
// Java: null is a value of any reference type
String name = null;  // Compiles, but can cause NullPointerException
```

**Python/JavaScript**: `None`/`null`/`undefined` are values

### 10. Gradual Typing

Mix static and dynamic typing:

**Python Type Hints**:
```python
def greet(name: str) -> str:
    return f"Hello, {name}!"

# Optional, not enforced at runtime
# But tools like mypy can check them
```

**TypeScript** (JavaScript with types):
```typescript
function greet(name: string): string {
    return `Hello, ${name}!`;
}

// Compiles to JavaScript, types removed
```

## Type Systems by Language

### Python (Dynamically Typed with Optional Hints)

```python
# No type annotations needed
def add(x, y):
    return x + y

# Optional type hints (Python 3.5+)
def add_typed(x: int, y: int) -> int:
    return x + y

# Generic types
from typing import List, Dict, Optional, Union

def process(items: List[int]) -> Optional[int]:
    if not items:
        return None
    return sum(items)

# Type variables for generics
from typing import TypeVar
T = TypeVar('T')

def first(items: List[T]) -> Optional[T]:
    return items[0] if items else None
```

### JavaScript (Dynamically Typed)

```javascript
// No static types
function add(x, y) {
    return x + y;
}

// Can add anything
add(1, 2);        // 3
add("1", "2");    // "12"
add(1, "2");      // "12" (implicit coercion)

// TypeScript adds static types
function add(x: number, y: number): number {
    return x + y;
}
```

### Java (Statically Typed, Nominal)

```java
// Must declare types
public int add(int x, int y) {
    return x + y;
}

// Generics (since Java 5)
public <T> T identity(T x) {
    return x;
}

// Interfaces for polymorphism
interface Comparable<T> {
    int compareTo(T other);
}

// Type inference (Java 10+)
var numbers = List.of(1, 2, 3);  // Inferred: List<Integer>
```

### C (Weakly Typed, Static)

```c
// Must declare types
int add(int x, int y) {
    return x + y;
}

// No generics (use void* and casting)
void* identity(void* x) {
    return x;
}

// Lots of implicit conversions
int x = 3.14;  // Truncates to 3
char* str = 65;  // Allowed but dangerous
```

### Ruby (Dynamically Typed, Duck Typing)

```ruby
# No type annotations
def add(x, y)
  x + y
end

# Works with any types that support +
add(1, 2)        # 3
add("a", "b")    # "ab"
add([1], [2])    # [1, 2]

# Ruby 3 adds RBS (optional type signatures)
# In .rbs file:
# def add: (Integer, Integer) -> Integer
```

### Haskell (Statically Typed, Inferred)

```haskell
-- Type inference
add x y = x + y
-- Inferred: Num a => a -> a -> a

-- Explicit types
add :: Int -> Int -> Int
add x y = x + y

-- Polymorphic
identity :: a -> a
identity x = x

-- Type classes
class Eq a where
    (==) :: a -> a -> Bool

-- Algebraic data types
data Maybe a = Nothing | Just a
data Either a b = Left a | Right b
```

### Racket (Dynamically Typed, Gradual)

```racket
;; No types by default
(define (add x y)
  (+ x y))

;; Typed Racket dialect
#lang typed/racket
(: add (-> Integer Integer Integer))
(define (add x y)
  (+ x y))

;; Polymorphic
(: identity (All (A) (-> A A)))
(define (identity x) x)
```

### Rust (Statically Typed, Inferred, Ownership)

```rust
// Type inference
let x = 5;  // i32
let y = 3.14;  // f64

// Explicit types
fn add(x: i32, y: i32) -> i32 {
    x + y
}

// Generics with trait bounds
fn print_it<T: std::fmt::Display>(x: T) {
    println!("{}", x);
}

// Ownership types
fn take_ownership(s: String) {}  // Takes ownership
fn borrow(s: &String) {}  // Borrows immutably
fn borrow_mut(s: &mut String) {}  // Borrows mutably
```

### Prolog (Untyped, Logic-Based)

```prolog
% No static type system
% Types checked at runtime via unification

% Works with any terms
add(X, Y, Z) :- Z is X + Y.

% Can add type checking with predicates
integer_add(X, Y, Z) :-
    integer(X),
    integer(Y),
    Z is X + Y.
```

## Advanced Topics

### Dependent Types

Types that depend on values:

```haskell
-- Not in standard Haskell, but in languages like Idris:
-- Vector with length in the type
data Vect : Nat -> Type -> Type where
    Nil  : Vect 0 a
    (::) : a -> Vect n a -> Vect (S n) a

-- Function that only accepts vectors of the same length
zipVect : Vect n a -> Vect n b -> Vect n (a, b)
```

### Higher-Kinded Types

Types that abstract over type constructors:

```haskell
-- Functor: types that can be mapped over
class Functor f where
    fmap :: (a -> b) -> f a -> f b

-- Works for Maybe, List, etc.
instance Functor Maybe where
    fmap f Nothing = Nothing
    fmap f (Just x) = Just (f x)
```

### Phantom Types

Type parameters that don't appear in the definition:

```haskell
-- Track state in types
data State s a = State a

-- Can only transition locked to unlocked
unlock :: State Locked a -> State Unlocked a
unlock (State x) = State x
```

## Common Type System Patterns

### 1. The Option/Maybe Pattern

Represent optional values safely:

```rust
// Rust
fn find_user(id: u32) -> Option<User> {
    // Returns Some(user) or None
}

// Must handle both cases
match find_user(42) {
    Some(user) => println!("Found: {}", user.name),
    None => println!("Not found"),
}
```

### 2. The Result/Either Pattern

Represent success or failure:

```rust
// Rust
fn divide(x: i32, y: i32) -> Result<i32, String> {
    if y == 0 {
        Err("Division by zero".to_string())
    } else {
        Ok(x / y)
    }
}

// Must handle both cases
match divide(10, 2) {
    Ok(result) => println!("Result: {}", result),
    Err(e) => println!("Error: {}", e),
}
```

### 3. Newtype Pattern

Create distinct types from the same underlying type:

```haskell
-- Haskell
newtype UserId = UserId Int
newtype ProductId = ProductId Int

-- Can't accidentally mix them up
getUser :: UserId -> User
getUser (UserId id) = ...

-- Type error: ProductId is not UserId
getUser (ProductId 123)  -- ERROR!
```

### 4. Builder Pattern with Types

Use types to enforce correct construction:

```rust
// Rust
struct UserBuilder<Name, Email> {
    name: Name,
    email: Email,
}

impl UserBuilder<(), ()> {
    fn new() -> Self {
        UserBuilder { name: (), email: () }
    }
}

impl<E> UserBuilder<(), E> {
    fn name(self, name: String) -> UserBuilder<String, E> {
        UserBuilder { name, email: self.email }
    }
}

// Type ensures both name and email are set
```

## Type System Trade-offs

### Static vs Dynamic

**Static Advantages**:
- Catch errors early
- Better tooling
- Performance optimizations
- Self-documenting

**Dynamic Advantages**:
- Faster prototyping
- More flexible
- Less boilerplate
- Easier metaprogramming

### Weak vs Strong

**Weak Advantages**:
- More implicit conversions
- Less verbose

**Strong Advantages**:
- Fewer bugs from implicit coercions
- More predictable behavior

## Best Practices

1. **Use the strongest type system your language offers**
   - Python: Use type hints
   - JavaScript: Consider TypeScript
   - Java: Use generics, avoid raw types

2. **Make illegal states unrepresentable**
   - Use enums instead of strings for fixed sets
   - Use types to enforce invariants

3. **Prefer sum types over nulls**
   - Use Option/Maybe instead of null
   - Use Result/Either instead of exceptions

4. **Use type inference where available**
   - Don't repeat yourself
   - But add annotations for public APIs

5. **Design with types**
   - Think about types before implementation
   - Use types to guide design

## Summary

Type systems exist on a spectrum:
- **Weak → Strong**: How strict about type errors
- **Dynamic → Static**: When types are checked
- **Simple → Sophisticated**: Expressiveness of the type system

Each point on the spectrum has trade-offs. The key is understanding:
1. What your language offers
2. What guarantees you need
3. What complexity you can afford

## Further Reading

- **Books**:
  - "Types and Programming Languages" by Benjamin Pierce
  - "Programming in Haskell" by Graham Hutton
  - "The Rust Programming Language" (official book)

- **Papers**:
  - "Principal type-schemes for functional programs" (Hindley-Milner)
  - "Theorems for free!" by Philip Wadler

- **Online**:
  - Rust book on ownership and types
  - TypeScript handbook
  - Haskell wiki on type system features

## Next Steps

With this final lesson complete, you now have a foundation in:
1. Basic Syntax and Variables
2. Data Types
3. Control Flow
4. Functions
5. Data Structures
6. Recursion
7. Object-Oriented Programming
8. Higher-Order Functions
9. Pattern Matching
10. Type Systems

These concepts form the core of programming across paradigms and languages. Continue practicing by:
- Implementing the exercises
- Exploring the practice projects
- Building your own programs
- Learning new languages and comparing their approaches

Happy coding!
