# Lesson 4: Functions

## Learning Objectives

By the end of this lesson, you will be able to:

1. Define and call functions in Python, C++, and Haskell
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

**Key features:**
- `def` keyword for definition
- No type annotations required (but optional with type hints)
- Default parameter values
- Implicit `None` return if no return statement

---

### C++: Function Declarations and Definitions

```cpp
#include <iostream>
#include <string>
#include <functional>

// Function declaration (prototype)
std::string greet(const std::string& name);
int add(int x, int y);
double power(double base, int exponent = 2);

// Function definition
std::string greet(const std::string& name) {
    return "Hello, " + name + "!";
}

int add(int x, int y) {
    return x + y;
}

// Default parameter (only in declaration)
double power(double base, int exponent) {
    double result = 1.0;
    for (int i = 0; i < exponent; i++) {
        result *= base;
    }
    return result;
}

// Void function (no return value)
void print_greeting(const std::string& name) {
    std::cout << "Hello, " << name << "!" << std::endl;
}

// Function overloading (same name, different parameters)
double add(double x, double y) {
    return x + y;
}

int main() {
    std::cout << greet("Alice") << std::endl;
    std::cout << add(5, 3) << std::endl;
    std::cout << power(5.0) << std::endl;  // Uses default exponent
    std::cout << power(5.0, 3) << std::endl;
    return 0;
}
```

**Key features:**
- Explicit type declarations required
- Default parameters supported
- Function overloading (same name, different types/parameters)
- Pass by value, reference, or pointer
- Can return values or `void`

---

### Haskell: Pure Functions with Type Signatures

```haskell
-- Type signature (optional but recommended)
greet :: String -> String
greet name = "Hello, " ++ name ++ "!"

-- Multiple parameters (curried automatically)
add :: Int -> Int -> Int
add x y = x + y

-- Note: In Haskell, all functions take one parameter!
-- "add x y" is actually (add x) y - automatic currying

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

**Key features:**
- Type signatures optional but highly recommended
- All functions are pure (no side effects) except those in IO monad
- Automatic currying (all functions take one argument)
- Guards for conditional logic
- Pattern matching in function definitions

---

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

#### C++: Positional Only (but can simulate with structs)

```cpp
std::string describePerson(const std::string& name, int age, const std::string& city) {
    return name + " is " + std::to_string(age) + " years old and lives in " + city;
}

// Positional only
std::cout << describePerson("Alice", 30, "NYC") << std::endl;

// Can simulate named parameters with structs:
struct PersonInfo {
    std::string name;
    int age;
    std::string city;
};

std::string describePersonNamed(const PersonInfo& info) {
    return info.name + " is " + std::to_string(info.age) +
           " years old and lives in " + info.city;
}

// Usage with designated initializers (C++20):
describePersonNamed({.name="Alice", .age=30, .city="NYC"});
```

#### Haskell: Positional Only

```haskell
describePerson :: String -> Int -> String -> String
describePerson name age city =
    name ++ " is " ++ show age ++ " years old and lives in " ++ city

-- Can only call with positional arguments
-- describePerson "Alice" 30 "NYC"
```

### Default Parameters

**Python:**
```python
def greet(name, greeting="Hello"):
    return f"{greeting}, {name}!"

print(greet("Alice"))              # "Hello, Alice!"
print(greet("Bob", "Hi"))          # "Hi, Bob!"
```

**C++:**
```cpp
std::string greet(const std::string& name, const std::string& greeting = "Hello") {
    return greeting + ", " + name + "!";
}

std::cout << greet("Alice") << std::endl;       // "Hello, Alice!"
std::cout << greet("Bob", "Hi") << std::endl;   // "Hi, Bob!"
```

**Haskell (No default parameters, but can use multiple definitions):**
```haskell
-- Version without greeting (uses default)
greet :: String -> String
greet name = greetWith "Hello" name

-- Version with custom greeting
greetWith :: String -> String -> String
greetWith greeting name = greeting ++ ", " ++ name ++ "!"
```

### Variadic Functions (Variable Number of Arguments)

**Python:** *args and **kwargs
```python
def sum_all(*numbers):
    return sum(numbers)

def print_info(**kwargs):
    for key, value in kwargs.items():
        print(f"{key}: {value}")

print(sum_all(1, 2, 3, 4, 5))      # 15
print_info(name="Alice", age=30)   # name: Alice, age: 30
```

**C++:** Variadic templates or initializer_list
```cpp
#include <initializer_list>
#include <numeric>

// Using initializer_list
int sumAll(std::initializer_list<int> numbers) {
    return std::accumulate(numbers.begin(), numbers.end(), 0);
}

// C++11 variadic template
template<typename... Args>
void printInfo(Args... args) {
    // Fold expression (C++17)
    ((std::cout << args << " "), ...);
    std::cout << std::endl;
}

// Usage
sumAll({1, 2, 3, 4, 5});  // 15
printInfo("Alice", 30, "NYC");
```

**Haskell:** Use lists or tuples
```haskell
-- Accept a list of any length
sumAll :: [Int] -> Int
sumAll = sum

-- sumAll [1, 2, 3, 4, 5] == 15
```

---

## Scope and Closures

### Scope: Where Variables Are Visible

**Python: LEGB Rule (Local, Enclosing, Global, Built-in)**
```python
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

**C++: Block Scope**
```cpp
int x = 1;  // Global scope

void outer() {
    int x = 2;  // Function scope (shadows global)

    {
        int x = 3;  // Block scope (shadows function)
        std::cout << x << std::endl;  // 3
    }

    std::cout << x << std::endl;  // 2
}
```

**Haskell: Lexical Scope with Shadowing**
```haskell
x = "global"

outer =
    let x = "enclosing"
        inner =
            let x = "local"
            in putStrLn x  -- "local"
    in do
        inner
        putStrLn x  -- "enclosing"
```

### Closures: Functions that Capture Environment

**Python:**
```python
def make_multiplier(factor):
    def multiply(x):
        return x * factor  # Captures 'factor' from outer scope
    return multiply

times_two = make_multiplier(2)
times_three = make_multiplier(3)

print(times_two(5))    # 10
print(times_three(5))  # 15
```

**C++:**
```cpp
#include <functional>

std::function<int(int)> makeMultiplier(int factor) {
    // Lambda captures 'factor' by value
    return [factor](int x) { return x * factor; };
}

auto timesTwo = makeMultiplier(2);
auto timesThree = makeMultiplier(3);

std::cout << timesTwo(5) << std::endl;    // 10
std::cout << timesThree(5) << std::endl;  // 15
```

**Haskell:**
```haskell
-- All functions are closures naturally
makeMultiplier :: Int -> (Int -> Int)
makeMultiplier factor = \x -> x * factor

timesTwo = makeMultiplier 2
timesThree = makeMultiplier 3

-- timesTwo 5  == 10
-- timesThree 5 == 15
```

---

## Pure vs Impure Functions

### Pure Functions

A function is **pure** if:
1. Same inputs always produce same output (deterministic)
2. No side effects (doesn't modify external state, no I/O)

**Pure examples:**
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

**Impure examples:**
```python
# Impure: Depends on external state
total = 0
def add_to_total(x):
    global total
    total += x
    return total

# Impure: Non-deterministic
import random
def get_random():
    return random.randint(1, 10)

# Impure: Side effect (I/O)
def greet(name):
    print(f"Hello, {name}!")  # Side effect

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
- **Composable:** Can combine safely

**Haskell enforces purity:**
```haskell
-- This won't compile: can't do I/O in pure function
-- greet :: String -> String
-- greet name = print ("Hello, " ++ name)  -- ERROR!

-- Must use IO type to indicate side effects
greet :: String -> IO ()
greet name = putStrLn ("Hello, " ++ name)  -- OK

-- Pure functions can't have side effects
pureAdd :: Int -> Int -> Int
pureAdd x y = x + y
```

---

## Functions as First-Class Values

In Python, C++ (modern), and Haskell, functions can be:
- Assigned to variables
- Passed as arguments
- Returned from functions
- Stored in data structures

### Passing Functions as Arguments (Higher-Order Functions)

**Python:**
```python
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

**C++:**
```cpp
#include <vector>
#include <algorithm>
#include <functional>

std::vector<int> numbers = {1, 2, 3, 4, 5};

// Transform (like map)
std::vector<int> squared(numbers.size());
std::transform(numbers.begin(), numbers.end(), squared.begin(),
               [](int x) { return x * x; });
// {1, 4, 9, 16, 25}

// Filter (copy_if)
std::vector<int> evens;
std::copy_if(numbers.begin(), numbers.end(), std::back_inserter(evens),
             [](int x) { return x % 2 == 0; });
// {2, 4}

// Custom higher-order function
template<typename T, typename Func>
T applyTwice(Func f, T x) {
    return f(f(x));
}

auto increment = [](int x) { return x + 1; };
std::cout << applyTwice(increment, 5) << std::endl;  // 7
```

**Haskell:**
```haskell
numbers = [1, 2, 3, 4, 5]

-- Map
squared = map (^ 2) numbers
-- [1, 4, 9, 16, 25]

-- Filter
evens = filter even numbers
-- [2, 4]

-- Custom higher-order function
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

increment :: Int -> Int
increment x = x + 1

-- applyTwice increment 5 == 7
```

### Returning Functions

**Python:**
```python
def make_adder(n):
    def adder(x):
        return x + n
    return adder

add_five = make_adder(5)
print(add_five(10))  # 15
```

**C++:**
```cpp
std::function<int(int)> makeAdder(int n) {
    return [n](int x) { return x + n; };
}

auto addFive = makeAdder(5);
std::cout << addFive(10) << std::endl;  # 15
```

**Haskell (with automatic currying):**
```haskell
-- All functions are curried automatically
add :: Int -> Int -> Int
add x y = x + y

-- Partial application creates new function
addFive :: Int -> Int
addFive = add 5

-- addFive 10 == 15
```

---

## Function Composition

Combining simple functions to create complex behavior.

**Haskell: Composition operator (.)**
```haskell
square :: Int -> Int
square x = x * x

double :: Int -> Int
double x = x * 2

-- Compose: square after double
squareAfterDouble :: Int -> Int
squareAfterDouble = square . double

-- squareAfterDouble 3 == square (double 3) == square 6 == 36
```

**Python: Manual composition**
```python
def compose(f, g):
    return lambda x: f(g(x))

square = lambda x: x ** 2
double = lambda x: x * 2

square_after_double = compose(square, double)
print(square_after_double(3))  # 36
```

**C++: Composition**
```cpp
template<typename F, typename G>
auto compose(F f, G g) {
    return [f, g](auto x) { return f(g(x)); };
}

auto square = [](int x) { return x * x; };
auto doubleVal = [](int x) { return x * 2; };

auto squareAfterDouble = compose(square, doubleVal);
std::cout << squareAfterDouble(3) << std::endl;  // 36
```

---

## Anonymous Functions (Lambdas)

Short functions without names.

| Language | Lambda Syntax              | Example                |
|----------|----------------------------|------------------------|
| Python   | `lambda params: expr`      | `lambda x: x * 2`      |
| C++      | `[capture](params) { }`    | `[](int x) { return x * 2; }` |
| Haskell  | `\params -> expr`          | `\x -> x * 2`          |

**Python lambdas:**
```python
double = lambda x: x * 2
add = lambda x, y: x + y
is_even = lambda x: x % 2 == 0

# Often used with map, filter
squared = list(map(lambda x: x ** 2, [1, 2, 3]))
```

**C++ lambdas:**
```cpp
auto double_val = [](int x) { return x * 2; };
auto add = [](int x, int y) { return x + y; };
auto is_even = [](int x) { return x % 2 == 0; };

// Capture by value [=] or by reference [&]
int factor = 10;
auto multiply_by_factor = [factor](int x) { return x * factor; };
```

**Haskell lambdas:**
```haskell
double = \x -> x * 2
add = \x y -> x + y
isEven = \x -> even x

-- Often used with map, filter
squared = map (\x -> x ^ 2) [1, 2, 3]
```

---

## Comparison Table: Function Features

| Feature            | Python | C++    | Haskell  |
|--------------------|--------|--------|----------|
| Default Params     | Yes    | Yes    | No (use multiple defs) |
| First-Class        | Yes    | Yes    | Yes      |
| Closures           | Yes    | Yes    | Yes      |
| Pure by Default    | No     | No     | Yes      |
| Auto Currying      | No     | No     | Yes      |
| Function Overload  | No     | Yes    | No       |
| Lambdas            | Yes    | Yes    | Yes      |
| Type Inference     | N/A    | auto   | Excellent|

---

## Key Insights

### 1. Functions Are Fundamental
Every language has functions, but they differ in syntax, features, and philosophy.

### 2. First-Class Functions Enable Powerful Abstractions
- Higher-order functions (map, filter, reduce)
- Callbacks and event handlers
- Functional programming patterns

### 3. Closures Capture Context
- Private variables
- Factory functions
- Stateful behavior without classes

### 4. Purity Enables Reasoning
Pure functions are easier to test, safer for concurrency, and more predictable.

### 5. Paradigm Differences
- **Imperative (Python, C++):** Functions can have side effects
- **Functional (Haskell):** Functions are mathematical transformations
- **Type Systems:** Static (C++, Haskell) vs Dynamic (Python)

---

## Exercises

See [EXERCISES.md](EXERCISES.md) for hands-on practice with:
- Temperature converters
- Calculator functions
- String utilities
- Higher-order functions
- Closures and state
- Function composition
- Pure vs impure function identification

---

## Discussion Questions

1. **Why do some languages (C) not treat functions as first-class values?**

2. **What are the tradeoffs between named and default parameters?**

3. **How do closures enable encapsulation without classes?**

4. **Why does Haskell enforce purity? What do we lose? What do we gain?**

5. **When would you prefer an anonymous function over a named one?**

6. **How does automatic currying (Haskell) differ from manual currying (Python/C++)?**

7. **What are the advantages of C++'s function overloading?**

---

## Looking Ahead

You're mastering three powerful languages:
- **Python:** Flexible, multi-paradigm, easy to prototype
- **C++:** Performance-focused, statically-typed, systems programming
- **Haskell:** Pure functional, strong types, mathematical elegance

Later in the course, you'll encounter:
- **Racket:** Functional with Scheme/Lisp heritage
- **Rust:** Systems language with memory safety
- **Prolog:** Logic programming with declarative functions

---

## Next Lesson

In Lesson 5, we'll explore **Data Structures**: arrays, lists, maps, trees, and the crucial distinction between mutable and immutable data.

---

## Summary

Functions are the heart of programming:
- **All languages have functions**, but with different features and philosophies
- **Pure functions** are predictable, testable, and composable
- **First-class functions** enable powerful abstractions like map and filter
- **Closures** capture environment for stateful behavior
- **Composition** builds complex behavior from simple parts

Understanding functions deeply makes you a better programmer in any language!
