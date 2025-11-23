# Lesson 9: Pattern Matching

## Overview

Pattern matching is a powerful control flow mechanism that allows you to:
- Check a value against multiple patterns
- Destructure complex data structures
- Extract values from nested structures
- Write more declarative, concise code

Think of it as "switch/case on steroids" - but it's much more powerful!

## Learning Objectives

By the end of this lesson, you will:
- Understand what pattern matching is and why it's useful
- Match against literals, types, and structures
- Destructure data structures with patterns
- Use guards and conditions in patterns
- Recognize pattern matching across different languages
- Compare pattern matching with traditional conditionals

## What is Pattern Matching?

**Pattern matching** tests a value against one or more patterns and executes code based on which pattern matches.

### Simple Example (Haskell)

```haskell
describe :: Int -> String
describe 0 = "zero"
describe 1 = "one"
describe 2 = "two"
describe n = "many: " ++ show n
```

### Comparison with if/else

```python
# Traditional if/else
def describe(n):
    if n == 0:
        return "zero"
    elif n == 1:
        return "one"
    elif n == 2:
        return "two"
    else:
        return f"many: {n}"

# Python 3.10+ pattern matching
def describe(n):
    match n:
        case 0:
            return "zero"
        case 1:
            return "one"
        case 2:
            return "two"
        case _:
            return f"many: {n}"
```

**Benefits:**
- More concise
- Exhaustiveness checking (compiler ensures all cases covered)
- Destructuring built-in
- Less error-prone

## Types of Pattern Matching

### 1. Literal Patterns

Match exact values:

```rust
match number {
    0 => println!("zero"),
    1 => println!("one"),
    2 => println!("two"),
    _ => println!("other"),  // _ is wildcard
}
```

### 2. Variable Patterns

Bind matched values to variables:

```haskell
greet :: String -> String
greet name = "Hello, " ++ name  -- 'name' binds to the argument
```

### 3. Constructor Patterns

Match against data constructors:

```haskell
data Color = Red | Green | Blue

describeColor :: Color -> String
describeColor Red = "It's red"
describeColor Green = "It's green"
describeColor Blue = "It's blue"
```

### 4. Tuple/List Patterns

Destructure tuples and lists:

```haskell
-- Tuple matching
point :: (Int, Int) -> String
point (0, 0) = "origin"
point (0, y) = "on y-axis"
point (x, 0) = "on x-axis"
point (x, y) = "general point"

-- List matching
listDescription :: [Int] -> String
listDescription [] = "empty"
listDescription [x] = "one element"
listDescription [x, y] = "two elements"
listDescription (x:xs) = "many elements"  -- Head:tail pattern
```

### 5. Record/Object Patterns

Destructure structured data:

```python
# Python 3.10+
match person:
    case {"name": n, "age": 18}:
        print(f"{n} just became an adult!")
    case {"name": n, "age": age} if age < 18:
        print(f"{n} is a minor")
    case {"name": n}:
        print(f"Hello, {n}")
```

### 6. Type Patterns

Match based on type:

```rust
enum Value {
    Int(i32),
    Float(f64),
    Text(String),
}

match value {
    Value::Int(n) => println!("Integer: {}", n),
    Value::Float(f) => println!("Float: {}", f),
    Value::Text(s) => println!("Text: {}", s),
}
```

## Destructuring

**Destructuring** extracts values from complex structures in one step:

### Tuples

```python
# Python
x, y = (10, 20)
print(x)  # 10

# With pattern matching (Python 3.10+)
match point:
    case (0, 0):
        print("Origin")
    case (x, 0):
        print(f"On X-axis at {x}")
    case (0, y):
        print(f"On Y-axis at {y}")
    case (x, y):
        print(f"Point at ({x}, {y})")
```

### Lists/Arrays

```haskell
-- Haskell
case list of
    [] -> "empty"
    [x] -> "one: " ++ show x
    [x, y] -> "two: " ++ show x ++ ", " ++ show y
    (x:y:rest) -> "many, starting with " ++ show x
```

### Nested Structures

```rust
match nested {
    Some((x, Some(y))) => println!("x={}, y={}", x, y),
    Some((x, None)) => println!("x={}, no y", x),
    None => println!("nothing"),
}
```

## Guards and Conditions

Add conditions to patterns:

```haskell
classify :: Int -> String
classify n
    | n < 0 = "negative"
    | n == 0 = "zero"
    | n < 10 = "small positive"
    | n < 100 = "medium positive"
    | otherwise = "large positive"
```

```rust
match number {
    n if n < 0 => println!("negative"),
    0 => println!("zero"),
    n if n < 10 => println!("small"),
    _ => println!("large"),
}
```

```python
# Python 3.10+
match value:
    case n if n < 0:
        print("negative")
    case 0:
        print("zero")
    case n if n < 10:
        print("small")
    case _:
        print("large")
```

## Exhaustiveness Checking

Many languages check if all cases are covered:

```rust
enum TrafficLight {
    Red,
    Yellow,
    Green,
}

// Compiler error if missing a case!
match light {
    TrafficLight::Red => stop(),
    TrafficLight::Yellow => slow(),
    // Missing Green - won't compile!
}
```

This catches bugs at compile time!

## Pattern Matching vs Switch/Case

| Feature | Pattern Matching | Switch/Case |
|---------|------------------|-------------|
| Destructuring | ✓ Yes | ✗ Usually no |
| Type matching | ✓ Yes | ✗ No |
| Guards/conditions | ✓ Yes | ✗ Limited |
| Exhaustiveness checking | ✓ Often | ✗ Rarely |
| Nested patterns | ✓ Yes | ✗ No |

## Language Support

### Native Pattern Matching

**Haskell, OCaml, F#, Rust, Scala, Elixir**
- Core language feature
- Very powerful
- Exhaustiveness checking

**Python 3.10+**
- `match` statement
- Structural pattern matching
- Recent addition

**Rust**
- `match` expression
- Exhaustive by default
- Very powerful

### Limited Support

**JavaScript (TC39 Proposal)**
- Under consideration
- Not yet standardized

**Java (since 17)**
- Pattern matching for `instanceof`
- Switch expressions
- Evolving feature

### Workarounds Needed

**C, Ruby (limited), Prolog (different paradigm)**
- Must use if/else or case
- Can simulate with functions
- More verbose

## Common Patterns

### Option/Maybe Pattern

```rust
match option {
    Some(value) => println!("Got: {}", value),
    None => println!("Got nothing"),
}
```

```haskell
case maybe of
    Just x -> "Got " ++ show x
    Nothing -> "Got nothing"
```

### Result/Either Pattern

```rust
match result {
    Ok(value) => println!("Success: {}", value),
    Err(error) => println!("Error: {}", error),
}
```

### List Patterns

```haskell
-- First element
case list of
    (x:_) -> x
    [] -> error "empty list"

-- First two elements
case list of
    (x:y:_) -> (x, y)
    _ -> error "need at least 2 elements"

-- Last element (inefficient!)
case reverse list of
    (x:_) -> x
    [] -> error "empty list"
```

## Practical Examples

### 1. Binary Tree Traversal

```haskell
data Tree a = Empty | Node a (Tree a) (Tree a)

height :: Tree a -> Int
height Empty = 0
height (Node _ left right) = 1 + max (height left) (height right)

contains :: Eq a => a -> Tree a -> Bool
contains _ Empty = False
contains x (Node val left right)
    | x == val = True
    | otherwise = contains x left || contains x right
```

### 2. Expression Evaluator

```rust
enum Expr {
    Num(i32),
    Add(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
}

fn eval(expr: Expr) -> i32 {
    match expr {
        Expr::Num(n) => n,
        Expr::Add(left, right) => eval(*left) + eval(*right),
        Expr::Mul(left, right) => eval(*left) * eval(*right),
    }
}
```

### 3. State Machine

```rust
enum State {
    Idle,
    Running(u32),  // With progress
    Paused(u32),
    Done,
}

fn next_state(state: State, action: Action) -> State {
    match (state, action) {
        (State::Idle, Action::Start) => State::Running(0),
        (State::Running(p), Action::Pause) => State::Paused(p),
        (State::Paused(p), Action::Resume) => State::Running(p),
        (State::Running(p), Action::Progress) => State::Running(p + 1),
        (State::Running(100), _) => State::Done,
        _ => state,  // No change for invalid transitions
    }
}
```

### 4. JSON Parsing

```python
# Python 3.10+
def process_json(data):
    match data:
        case {"type": "user", "name": name, "age": age}:
            return User(name, age)
        case {"type": "product", "name": name, "price": price}:
            return Product(name, price)
        case {"error": message}:
            raise Exception(message)
        case _:
            raise Exception("Unknown format")
```

## Best Practices

### 1. Order Matters

```haskell
-- WRONG - wildcard catches everything
classify n = "number"  -- This always matches!
classify 0 = "zero"    -- Never reached!

-- RIGHT - specific first, general last
classify 0 = "zero"
classify n = "number"
```

### 2. Use Wildcard for Defaults

```rust
match value {
    1 => "one",
    2 => "two",
    _ => "other",  // Catches everything else
}
```

### 3. Avoid Deep Nesting

```haskell
-- BAD
case x of
    Just (Left (Right (Just y))) -> ...

-- BETTER - break it up
case x of
    Just val -> case val of
        Left inner -> case inner of
            Right result -> case result of
                Just y -> ...
```

### 4. Use Guards for Complex Conditions

```rust
match (x, y) {
    (a, b) if a == b => "equal",
    (a, b) if a > b => "first larger",
    _ => "second larger",
}
```

## Pattern Matching in Different Languages

### Haskell (Best-in-Class)

```haskell
-- Function clauses
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- case expressions
case list of
    [] -> 0
    (x:xs) -> x + sum xs

-- Guards
classify n
    | n < 0 = "negative"
    | n == 0 = "zero"
    | otherwise = "positive"
```

### Rust

```rust
match value {
    0 => "zero",
    1 | 2 | 3 => "small",  // Multiple patterns
    4..=10 => "medium",     // Range pattern
    n if n > 10 => "large",
    _ => "negative",
}
```

### Python 3.10+

```python
match point:
    case (0, 0):
        return "origin"
    case (x, 0):
        return f"x-axis at {x}"
    case (0, y):
        return f"y-axis at {y}"
    case (x, y):
        return f"point ({x}, {y})"
```

### Racket

```racket
(match value
  [0 "zero"]
  [1 "one"]
  [(list x y) (format "pair: ~a, ~a" x y)]
  [_ "other"])
```

### Prolog (Different: Unification)

```prolog
factorial(0, 1).
factorial(N, Result) :-
    N > 0,
    N1 is N - 1,
    factorial(N1, SubResult),
    Result is N * SubResult.
```

## When to Use Pattern Matching

**Good fit:**
- ✓ Working with algebraic data types (enums, unions)
- ✓ Parsing and data transformation
- ✓ State machines
- ✓ Tree/recursive data structure traversal
- ✓ Option/Result handling
- ✓ Multiple related conditions

**Not ideal:**
- ✗ Simple single condition (use if)
- ✗ Range checking (unless language supports range patterns)
- ✗ When you need fallthrough (use if/else)

## Common Pitfalls

### 1. Non-Exhaustive Patterns

```rust
// Warning: non-exhaustive pattern!
match day {
    "Monday" => 1,
    "Tuesday" => 2,
    // Missing other days!
}
```

### 2. Unreachable Patterns

```haskell
-- Warning: second clause unreachable
f 0 = "zero"
f _ = "other"
f 1 = "one"  -- Never reached!
```

### 3. Variable Shadowing

```haskell
x = 10
case y of
    x -> x + 1  -- 'x' here shadows outer 'x'!
```

## Summary

**Key Takeaways:**
- Pattern matching is powerful control flow + destructuring
- Much more expressive than switch/case
- Native to functional languages, spreading to mainstream
- Enables exhaustiveness checking
- Makes code more declarative and safer

**Pattern Matching = Switch + Destructuring + Type Checking + Safety**

---

Let's see how different languages handle pattern matching!
