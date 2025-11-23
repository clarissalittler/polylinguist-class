# Lesson 4: Functions

## Learning Objectives

By the end of this lesson, you will be able to:

1. Define and call functions in multiple programming languages
2. Understand different parameter-passing mechanisms
3. Explain scope, closures, and variable capture
4. Distinguish between pure and impure functions
5. Use functions as first-class values
6. Write and use anonymous functions (lambdas)
7. Compose functions to build complex behavior

## Introduction

Functions are the fundamental building blocks of abstraction in programming. They allow us to:
- **Reuse** code without duplication
- **Abstract** complex operations behind simple names
- **Organize** programs into logical units
- **Test** isolated pieces of functionality

Different languages have wildly different approaches to functions, reflecting their underlying paradigms.

## Defining Functions

### Python: def Keyword

```python
# Basic function
def greet(name):
    return f"Hello, {name}!"

# Function with multiple parameters
def add(x, y):
    return x + y

# Function with default parameters
def power(base, exponent=2):
    return base ** exponent

# No explicit return (returns None)
def print_greeting(name):
    print(f"Hello, {name}!")

# Call functions
print(greet("Alice"))      # "Hello, Alice!"
print(add(5, 3))           # 8
print(power(5))            # 25 (uses default exponent=2)
print(power(5, 3))         # 125
```

### JavaScript: function Keyword and Arrow Functions

```javascript
// Function declaration
function greet(name) {
    return `Hello, ${name}!`;
}

// Function expression
const add = function(x, y) {
    return x + y;
};

// Arrow function (ES6)
const multiply = (x, y) => x * y;

// Arrow function with block
const power = (base, exponent = 2) => {
    return base ** exponent;
};

// Single parameter, no parentheses
const square = x => x * x;

// Call functions
console.log(greet("Alice"));   // "Hello, Alice!"
console.log(add(5, 3));        // 8
console.log(multiply(4, 5));   // 20
console.log(square(7));        // 49
```

### C: Function Declarations and Definitions

```c
#include <stdio.h>

// Function declaration (prototype)
int add(int x, int y);
double power(double base, int exponent);

// Function definition
int add(int x, int y) {
    return x + y;
}

double power(double base, int exponent) {
    double result = 1.0;
    for (int i = 0; i < exponent; i++) {
        result *= base;
    }
    return result;
}

// Void function (no return value)
void greet(char* name) {
    printf("Hello, %s!\n", name);
}

int main() {
    printf("%d\n", add(5, 3));        // 8
    printf("%.2f\n", power(5.0, 3));  // 125.00
    greet("Alice");                   // "Hello, Alice!"
    return 0;
}
```

**C has no default parameters or function overloading!**

### Java: Methods in Classes

```java
public class Functions {
    // Static method (class-level function)
    public static String greet(String name) {
        return "Hello, " + name + "!";
    }

    public static int add(int x, int y) {
        return x + y;
    }

    // Overloading: same name, different parameters
    public static double add(double x, double y) {
        return x + y;
    }

    // Method with default-like behavior (overloading)
    public static double power(double base) {
        return power(base, 2);  // Call the two-parameter version
    }

    public static double power(double base, int exponent) {
        return Math.pow(base, exponent);
    }

    // Void method
    public static void printGreeting(String name) {
        System.out.println("Hello, " + name + "!");
    }

    public static void main(String[] args) {
        System.out.println(greet("Alice"));
        System.out.println(add(5, 3));
        printGreeting("Bob");
    }
}
```

### Haskell: Pure Functions with Type Signatures

```haskell
-- Type signature (optional but recommended)
greet :: String -> String
greet name = "Hello, " ++ name ++ "!"

-- Multiple parameters
add :: Int -> Int -> Int
add x y = x + y

-- Note: In Haskell, all functions take one parameter!
-- "add x y" is actually (add x) y - currying

-- Function with guards
power :: Double -> Int -> Double
power base exponent
    | exponent == 0 = 1
    | exponent > 0  = base * power base (exponent - 1)
    | otherwise     = 1 / power base (-exponent)

-- Anonymous function (lambda)
square :: Int -> Int
square = \x -> x * x

-- Or point-free style
square' :: Int -> Int
square' = (^ 2)

-- Using functions
main :: IO ()
main = do
    putStrLn $ greet "Alice"           -- "Hello, Alice!"
    print $ add 5 3                    -- 8
    print $ power 5.0 3                -- 125.0
    print $ square 7                   -- 49
```

**Key Haskell feature:** All functions are pure (no side effects) except those in IO monad.

### Rust: Functions with Type Annotations

```rust
// Basic function
fn greet(name: &str) -> String {
    format!("Hello, {}!", name)
}

// Multiple parameters
fn add(x: i32, y: i32) -> i32 {
    x + y  // No semicolon = implicit return
}

// Explicit return
fn subtract(x: i32, y: i32) -> i32 {
    return x - y;  // With semicolon, need return keyword
}

// Generic function
fn max<T: PartialOrd>(a: T, b: T) -> T {
    if a > b { a } else { b }
}

// Function with no return value (returns unit type ())
fn print_greeting(name: &str) {
    println!("Hello, {}!", name);
}

// Anonymous function (closure)
let square = |x: i32| x * x;

fn main() {
    println!("{}", greet("Alice"));
    println!("{}", add(5, 3));
    println!("{}", square(7));
}
```

### Ruby: Methods with Implicit Returns

```ruby
# Basic method
def greet(name)
  "Hello, #{name}!"  # Implicit return (last expression)
end

# Multiple parameters
def add(x, y)
  x + y
end

# Default parameters
def power(base, exponent = 2)
  base ** exponent
end

# Explicit return
def subtract(x, y)
  return x - y
end

# Lambda (anonymous function)
square = lambda { |x| x * x }
# Or stabby lambda
square = ->(x) { x * x }

# Blocks (Ruby's special syntax)
[1, 2, 3].map { |x| x * 2 }  # [2, 4, 6]

# Usage
puts greet("Alice")
puts add(5, 3)
puts square.call(7)
```

### Racket: Functions as First-Class Values

```racket
; Basic function
(define (greet name)
  (string-append "Hello, " name "!"))

; Multiple parameters
(define (add x y)
  (+ x y))

; Lambda (anonymous function)
(define square
  (lambda (x) (* x x)))

; Or shorthand
(define (square x) (* x x))

; Higher-order function
(define (apply-twice f x)
  (f (f x)))

; Usage
(displayln (greet "Alice"))
(displayln (add 5 3))
(displayln (square 7))
(displayln (apply-twice square 2))  ; square(square(2)) = 16
```

## Parameters and Arguments

### Terminology

- **Parameter:** Variable in function definition
- **Argument:** Actual value passed when calling

```python
def greet(name):  # 'name' is a parameter
    return f"Hello, {name}!"

greet("Alice")    # "Alice" is an argument
```

### Positional vs Named Arguments

#### Python: Supports Both

```python
def describe_person(name, age, city):
    return f"{name} is {age} years old and lives in {city}"

# Positional arguments
print(describe_person("Alice", 30, "NYC"))

# Named arguments (keyword arguments)
print(describe_person(age=30, city="NYC", name="Alice"))

# Mixed (positional first)
print(describe_person("Alice", city="NYC", age=30))
```

#### JavaScript: Positional Only (but can simulate with objects)

```javascript
function describePerson(name, age, city) {
    return `${name} is ${age} years old and lives in ${city}`;
}

// Positional
console.log(describePerson("Alice", 30, "NYC"));

// Simulating named with object
function describePersonNamed({name, age, city}) {
    return `${name} is ${age} years old and lives in ${city}`;
}
console.log(describePersonNamed({age: 30, city: "NYC", name: "Alice"}));
```

### Default Parameters

```python
# Python
def greet(name, greeting="Hello"):
    return f"{greeting}, {name}!"

print(greet("Alice"))              # "Hello, Alice!"
print(greet("Bob", "Hi"))          # "Hi, Bob!"
```

```javascript
// JavaScript
function greet(name, greeting = "Hello") {
    return `${greeting}, ${name}!`;
}

console.log(greet("Alice"));       // "Hello, Alice!"
console.log(greet("Bob", "Hi"));   // "Hi, Bob!"
```

```haskell
-- Haskell: No default parameters, but can use multiple definitions
greet :: String -> String
greet name = greetWith "Hello" name

greetWith :: String -> String -> String
greetWith greeting name = greeting ++ ", " ++ name ++ "!"
```

### Variadic Functions (Variable Number of Arguments)

```python
# Python: *args and **kwargs
def sum_all(*numbers):
    return sum(numbers)

def print_info(**kwargs):
    for key, value in kwargs.items():
        print(f"{key}: {value}")

print(sum_all(1, 2, 3, 4, 5))      # 15
print_info(name="Alice", age=30)   # name: Alice, age: 30
```

```javascript
// JavaScript: rest parameters
function sumAll(...numbers) {
    return numbers.reduce((acc, n) => acc + n, 0);
}

console.log(sumAll(1, 2, 3, 4, 5));  // 15
```

```c
// C: stdarg.h for variadic functions
#include <stdarg.h>

int sum(int count, ...) {
    va_list args;
    va_start(args, count);

    int total = 0;
    for (int i = 0; i < count; i++) {
        total += va_arg(args, int);
    }

    va_end(args);
    return total;
}

// Usage: sum(5, 1, 2, 3, 4, 5)
```

## Scope and Closures

### Scope: Where Variables Are Visible

```python
# Python: LEGB rule (Local, Enclosing, Global, Built-in)
x = "global"

def outer():
    x = "enclosing"

    def inner():
        x = "local"
        print(x)  # "local"

    inner()
    print(x)  # "enclosing"

outer()
print(x)  # "global"
```

### Closures: Functions that Capture Environment

```python
# Python: Closure example
def make_multiplier(factor):
    def multiply(x):
        return x * factor  # Captures 'factor' from outer scope
    return multiply

times_two = make_multiplier(2)
times_three = make_multiplier(3)

print(times_two(5))    # 10
print(times_three(5))  # 15
```

```javascript
// JavaScript: Classic closure
function makeCounter() {
    let count = 0;  // Private variable

    return function() {
        count++;
        return count;
    };
}

const counter = makeCounter();
console.log(counter());  // 1
console.log(counter());  // 2
console.log(counter());  // 3
```

```haskell
-- Haskell: All functions are closures naturally
makeMultiplier :: Int -> (Int -> Int)
makeMultiplier factor = \x -> x * factor

timesTwo = makeMultiplier 2
timesThree = makeMultiplier 3

-- timesTwo 5  == 10
-- timesThree 5 == 15
```

### Scope Pitfalls

```javascript
// Classic JavaScript pitfall (before let/const)
for (var i = 0; i < 3; i++) {
    setTimeout(function() {
        console.log(i);  // Prints 3, 3, 3 (not 0, 1, 2)
    }, 100);
}

// Fixed with let (block scope)
for (let i = 0; i < 3; i++) {
    setTimeout(function() {
        console.log(i);  // Prints 0, 1, 2
    }, 100);
}
```

## Pure vs Impure Functions

### Pure Functions

A function is **pure** if:
1. Same inputs always produce same output (deterministic)
2. No side effects (doesn't modify external state)

```python
# Pure: Same input, same output, no side effects
def add(x, y):
    return x + y

# Pure: Deterministic, no side effects
def square(x):
    return x * x

# Pure: Even with complex logic
def factorial(n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)
```

### Impure Functions

```python
# Impure: Depends on external state
total = 0
def add_to_total(x):
    global total
    total += x
    return total

# Impure: Non-deterministic (different output for same input)
import random
def get_random():
    return random.randint(1, 10)

# Impure: Side effect (I/O)
def greet(name):
    print(f"Hello, {name}!")  # Side effect: prints to console

# Impure: Modifies input
def append_item(lst, item):
    lst.append(item)  # Mutates the list
    return lst
```

### Why Purity Matters

**Advantages of pure functions:**
- **Testable:** Easy to verify correctness
- **Cacheable:** Can memoize results
- **Parallelizable:** No race conditions
- **Predictable:** Easier to reason about

**Haskell enforces purity:**
```haskell
-- This won't compile: can't do I/O in pure function
-- greet :: String -> String
-- greet name = print ("Hello, " ++ name)  -- ERROR!

-- Must use IO type to indicate side effects
greet :: String -> IO ()
greet name = putStrLn ("Hello, " ++ name)  -- OK
```

## Functions as First-Class Values

In many languages, functions can be:
- Assigned to variables
- Passed as arguments
- Returned from functions
- Stored in data structures

### Passing Functions as Arguments (Higher-Order Functions)

```python
# Python: map, filter, reduce
numbers = [1, 2, 3, 4, 5]

# Map: apply function to each element
squared = list(map(lambda x: x ** 2, numbers))
# [1, 4, 9, 16, 25]

# Filter: keep elements matching predicate
evens = list(filter(lambda x: x % 2 == 0, numbers))
# [2, 4]

# Custom higher-order function
def apply_twice(f, x):
    return f(f(x))

def increment(x):
    return x + 1

print(apply_twice(increment, 5))  # 7
```

```javascript
// JavaScript: Array methods
const numbers = [1, 2, 3, 4, 5];

// Map
const squared = numbers.map(x => x ** 2);
// [1, 4, 9, 16, 25]

// Filter
const evens = numbers.filter(x => x % 2 === 0);
// [2, 4]

// Reduce
const sum = numbers.reduce((acc, x) => acc + x, 0);
// 15

// Custom higher-order function
function applyTwice(f, x) {
    return f(f(x));
}

const increment = x => x + 1;
console.log(applyTwice(increment, 5));  // 7
```

```haskell
-- Haskell: Functions as values is natural
numbers = [1, 2, 3, 4, 5]

-- Map
squared = map (^ 2) numbers
-- [1, 4, 9, 16, 25]

-- Filter
evens = filter even numbers
-- [2, 4]

-- Fold (reduce)
total = foldl (+) 0 numbers
-- 15

-- Custom higher-order function
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

increment :: Int -> Int
increment x = x + 1

-- applyTwice increment 5 == 7
```

### Returning Functions

```python
# Python: Function factory
def make_adder(n):
    def adder(x):
        return x + n
    return adder

add_five = make_adder(5)
print(add_five(10))  # 15
```

```javascript
// JavaScript: Currying
function multiply(x) {
    return function(y) {
        return x * y;
    };
}

const double = multiply(2);
const triple = multiply(3);

console.log(double(5));  // 10
console.log(triple(5));  // 15
```

```haskell
-- Haskell: All functions are curried automatically
multiply :: Int -> Int -> Int
multiply x y = x * y

-- Partial application
double = multiply 2
triple = multiply 3

-- double 5 == 10
-- triple 5 == 15
```

## Function Composition

Combining simple functions to create complex behavior.

```haskell
-- Haskell: Composition operator (.)
square :: Int -> Int
square x = x * x

double :: Int -> Int
double x = x * 2

-- Compose: square after double
squareAfterDouble :: Int -> Int
squareAfterDouble = square . double

-- squareAfterDouble 3 == square (double 3) == square 6 == 36
```

```python
# Python: Manual composition
def compose(f, g):
    return lambda x: f(g(x))

square = lambda x: x ** 2
double = lambda x: x * 2

square_after_double = compose(square, double)
print(square_after_double(3))  # 36
```

```javascript
// JavaScript: Composition
const compose = (f, g) => x => f(g(x));

const square = x => x ** 2;
const double = x => x * 2;

const squareAfterDouble = compose(square, double);
console.log(squareAfterDouble(3));  // 36
```

## Recursion Revisited

Functions can call themselves. (More in Lesson 6!)

```python
# Python: Factorial
def factorial(n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)
```

```haskell
-- Haskell: Factorial
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

## Anonymous Functions (Lambdas)

Short functions without names.

| Language   | Lambda Syntax |
|------------|---------------|
| Python     | `lambda x: x * 2` |
| JavaScript | `x => x * 2` or `function(x) { return x * 2; }` |
| Java       | `x -> x * 2` (Java 8+) |
| Ruby       | `lambda { \|x\| x * 2 }` or `->(x) { x * 2 }` |
| Haskell    | `\x -> x * 2` |
| Rust       | `\|x\| x * 2` |
| Racket     | `(lambda (x) (* x 2))` |

## Comparison Table: Function Features

| Language   | Default Params | First-Class | Closures | Pure by Default | Currying |
|------------|----------------|-------------|----------|-----------------|----------|
| Python     | Yes            | Yes         | Yes      | No              | Manual   |
| JavaScript | Yes            | Yes         | Yes      | No              | Manual   |
| C          | No             | No (pointers) | No     | No              | No       |
| Java       | No (overload)  | Yes (8+)    | Yes (8+) | No              | Manual   |
| Ruby       | Yes            | Yes         | Yes      | No              | Manual   |
| Haskell    | No             | Yes         | Yes      | Yes             | Automatic|
| Racket     | Yes            | Yes         | Yes      | No              | Manual   |
| Rust       | No             | Yes         | Yes      | No              | Manual   |

## Key Insights

### 1. Functions Are Fundamental
Every language has functions, but they differ in:
- Syntax and declaration style
- Parameter mechanisms
- Scope rules
- Purity constraints

### 2. First-Class Functions Enable Powerful Abstractions
Being able to pass functions around enables:
- Higher-order functions (map, filter, reduce)
- Callbacks and event handlers
- Functional programming patterns

### 3. Closures Capture Context
Functions can "remember" variables from their defining scope, enabling:
- Private variables
- Factory functions
- Stateful behavior without classes

### 4. Purity Enables Reasoning
Pure functions are:
- Easier to test
- Safer for concurrency
- More predictable

### 5. Paradigm Differences
- **Imperative (C):** Functions are procedures that execute statements
- **OOP (Java):** Functions are methods attached to classes
- **Functional (Haskell):** Functions are mathematical transformations
- **Multi-paradigm (Python, JavaScript):** Functions are flexible

## Exercises

### Exercise 1: Temperature Converter

Write functions to convert between Celsius and Fahrenheit:
- `celsius_to_fahrenheit(c)`
- `fahrenheit_to_celsius(f)`

Formulas:
- F = C × 9/5 + 32
- C = (F - 32) × 5/9

Implement in at least 2 languages.

### Exercise 2: Calculator

Write a calculator with functions for:
- add(x, y)
- subtract(x, y)
- multiply(x, y)
- divide(x, y)

Handle division by zero appropriately.

### Exercise 3: String Utilities

Write functions:
- `reverse_string(s)` - reverse a string
- `is_palindrome(s)` - check if string reads same forwards and backwards
- `count_vowels(s)` - count number of vowels

### Exercise 4: Higher-Order Functions

Write a function `apply_n_times(f, x, n)` that applies function `f` to `x` n times.

Example: `apply_n_times(lambda x: x * 2, 5, 3)` → 40 (5 × 2 × 2 × 2)

### Exercise 5: Closure Practice

Write a function `make_counter()` that returns a counter function. Each call to the counter increments and returns the count.

```python
counter1 = make_counter()
counter2 = make_counter()
print(counter1())  # 1
print(counter1())  # 2
print(counter2())  # 1
print(counter1())  # 3
```

### Exercise 6: Function Composition

Write a `compose` function that takes two functions and returns their composition.

Then use it to compose:
- `square` (x → x²)
- `increment` (x → x+1)

What is `compose(square, increment)(5)`?

### Exercise 7: Currying

Write a curried `add` function that works like:
```python
add(3)(5)  # Returns 8
```

### Exercise 8: Pure vs Impure

For each function, determine if it's pure or impure and why:
1. `def double(x): return x * 2`
2. `def print_double(x): print(x * 2)`
3. `def append_to_list(lst, x): lst.append(x); return lst`
4. `def get_length(lst): return len(lst)`
5. `def get_current_time(): return time.time()`

## Discussion Questions

1. **Why do some languages (C, Java before 8) not treat functions as first-class values?**

2. **What are the tradeoffs between named and default parameters?**

3. **How do closures enable encapsulation without classes?**

4. **Why does Haskell enforce purity? What do we lose? What do we gain?**

5. **When would you prefer an anonymous function over a named one?**

6. **How does automatic currying (Haskell) differ from manual currying (JavaScript)?**

## Going Deeper

### Resources
- Explore higher-order functions in functional languages
- Learn about memoization and lazy evaluation
- Study the λ-calculus (theoretical foundation of functions)

### Next Lesson Preview

In Lesson 5, we'll explore **Data Structures**: arrays, lists, maps, trees, and the crucial distinction between mutable and immutable data.

## Summary

Functions are the heart of programming:
- **All languages have functions**, but with different features
- **Pure functions** are predictable and testable
- **First-class functions** enable powerful abstractions
- **Closures** capture environment for stateful behavior
- **Composition** builds complex behavior from simple parts

Understanding functions deeply makes you a better programmer in any language!
