# Lesson 3: Control Flow

## Learning Objectives

By the end of this lesson, you will be able to:

1. Write conditional statements (if/else) in Python, C++, and Haskell
2. Understand the difference between statements and expressions
3. Use various loop constructs (for, while, recursion-based)
4. Explain boolean logic and truthiness across languages
5. Compare imperative loops with functional alternatives
6. Recognize when different control structures are idiomatic

## Introduction

Control flow determines the order in which code executes. While all programming languages need ways to make decisions and repeat operations, they express these concepts very differently. Understanding these differences reveals fundamental paradigm distinctions between imperative, systems, and functional programming.

### Key Concepts

**Conditionals** allow programs to make decisions based on boolean conditions.

**Loops** allow programs to repeat operations, but not all languages use traditional loops.

**Expressions vs Statements**: Some languages treat control flow as expressions (returning values), others as statements (performing actions).

## Conditionals: Making Decisions

### Basic If/Else

All languages support conditional logic, but the syntax and philosophy vary dramatically.

#### Python: Statements with Clean Syntax

```python
age = 20

if age >= 18:
    print("Adult")
    print("Can vote")
elif age >= 13:
    print("Teenager")
else:
    print("Child")

# Ternary expression
status = "Adult" if age >= 18 else "Minor"
```

**Key features:**
- Indentation-based blocks (no braces!)
- `elif` for chained conditions
- Ternary operator for simple cases
- Statements that perform actions

**Philosophy:** Clean, readable syntax that enforces consistency through indentation.

---

#### C++: Statements with Braces

```cpp
int age = 20;

if (age >= 18) {
    std::cout << "Adult" << std::endl;
    std::cout << "Can vote" << std::endl;
} else if (age >= 13) {
    std::cout << "Teenager" << std::endl;
} else {
    std::cout << "Child" << std::endl;
}

// Ternary expression
std::string status = (age >= 18) ? "Adult" : "Minor";

// C++17: if with initializer
if (int score = calculate_score(); score > 90) {
    std::cout << "Excellent: " << score << std::endl;
}
```

**Key features:**
- C-style braces for blocks
- `else if` for chained conditions
- Ternary operator common
- Can declare variables in if condition (C++17+)

**Philosophy:** Explicit syntax with performance and control as primary goals.

---

#### Haskell: Expressions, Not Statements

```haskell
age = 20

-- If expression (returns a value)
status = if age >= 18
         then "Adult"
         else "Minor"

-- Guards (preferred Haskell style)
description
  | age >= 18 = "Adult"
  | age >= 13 = "Teenager"
  | otherwise = "Child"

-- If in function definition
ageCategory age =
  if age >= 18
  then "Adult"
  else if age >= 13
       then "Teenager"
       else "Child"
```

**Key features:**
- `if` is an expression, **always** returns a value
- `else` is mandatory (expression must have a value)
- Guards (`|`) are more idiomatic and readable
- `otherwise` is just `True`, used for final case
- Immutability means no side effects in branches

**Philosophy:** Everything is an expression; computation over commands.

---

### Pattern Matching and Case Expressions

Pattern matching is a powerful alternative to if/else chains, especially in Haskell.

#### Haskell: Pattern Matching

```haskell
-- Pattern matching on values
describeNumber :: Int -> String
describeNumber 0 = "Zero"
describeNumber 1 = "One"
describeNumber 2 = "Two"
describeNumber _ = "Many"

-- Case expression
describeAge age = case age of
  n | n < 13    -> "Child"
    | n < 18    -> "Teenager"
    | otherwise -> "Adult"

-- Pattern matching on data structures
describeList :: [Int] -> String
describeList [] = "Empty"
describeList [x] = "Singleton: " ++ show x
describeList [x, y] = "Pair: " ++ show x ++ " and " ++ show y
describeList (x:y:rest) = "List starting with " ++ show x ++ ", " ++ show y
```

**When to use pattern matching:**
- Deconstructing data structures
- Handling multiple specific cases
- Working with algebraic data types

---

#### Python: Pattern Matching (3.10+)

```python
# Python 3.10+ added structural pattern matching
def describe_point(point):
    match point:
        case (0, 0):
            return "Origin"
        case (0, y):
            return f"Y-axis at {y}"
        case (x, 0):
            return f"X-axis at {x}"
        case (x, y):
            return f"Point at ({x}, {y})"
```

---

#### C++: Switch Statements

```cpp
// Traditional switch (integers/chars only)
int day = 3;
std::string dayType;

switch (day) {
    case 0:
    case 6:
        dayType = "Weekend";
        break;
    case 1:
    case 2:
    case 3:
    case 4:
    case 5:
        dayType = "Weekday";
        break;
    default:
        dayType = "Invalid day";
}

// C++17: switch with initializer
switch (int result = calculate(); result) {
    case 0:
        std::cout << "Zero" << std::endl;
        break;
    case 1:
        std::cout << "One" << std::endl;
        break;
    default:
        std::cout << "Other: " << result << std::endl;
}
```

**Note:** C++ switch is limited to integral types. For complex pattern matching, use if-else chains or consider external libraries.

---

## Loops: Repetition

Traditional imperative languages use loops, while functional languages prefer recursion and higher-order functions.

### For Loops

#### Python: Iterate Over Sequences

```python
# Range-based loop
for i in range(5):
    print(i)  # 0, 1, 2, 3, 4

# Iterate over collection
fruits = ["apple", "banana", "cherry"]
for fruit in fruits:
    print(fruit)

# With index
for i, fruit in enumerate(fruits):
    print(f"{i}: {fruit}")

# List comprehension (functional style)
squares = [x * x for x in range(10)]
```

**Philosophy:** Iterate over iterables, not indices. "Pythonic" code avoids manual index management.

---

#### C++: Multiple Loop Styles

```cpp
// Traditional for loop
for (int i = 0; i < 5; i++) {
    std::cout << i << std::endl;
}

// Range-based for loop (C++11)
std::vector<std::string> fruits = {"apple", "banana", "cherry"};
for (const auto& fruit : fruits) {
    std::cout << fruit << std::endl;
}

// With index (traditional way)
for (size_t i = 0; i < fruits.size(); i++) {
    std::cout << i << ": " << fruits[i] << std::endl;
}

// Iterators
for (auto it = fruits.begin(); it != fruits.end(); ++it) {
    std::cout << *it << std::endl;
}
```

**Philosophy:** Multiple styles for different needs; performance is key.

---

#### Haskell: No Traditional Loops!

Haskell doesn't have traditional loops. Use recursion, higher-order functions, or list comprehensions:

```haskell
-- Recursion
printNumbers :: Int -> IO ()
printNumbers n
  | n < 5     = do
      print n
      printNumbers (n + 1)
  | otherwise = return ()

-- Higher-order function
printNumbersHOF = mapM_ print [0..4]

-- List comprehension
squares = [x * x | x <- [0..9]]

-- Map function
doubled = map (*2) [1, 2, 3, 4, 5]

-- Filter function
evens = filter even [1..10]
```

**Philosophy:** Recursion and higher-order functions replace loops; emphasis on transformation over iteration.

---

### While Loops

#### Python

```python
count = 0
while count < 5:
    print(count)
    count += 1

# While with break
while True:
    user_input = input("Enter 'quit' to exit: ")
    if user_input == 'quit':
        break
```

#### C++

```cpp
int count = 0;
while (count < 5) {
    std::cout << count << std::endl;
    count++;
}

// Do-while (executes at least once)
int i = 0;
do {
    std::cout << i << std::endl;
    i++;
} while (i < 5);
```

#### Haskell: Use Recursion

```haskell
-- Recursive equivalent of while loop
whileLoop :: Int -> IO ()
whileLoop count
  | count < 5 = do
      print count
      whileLoop (count + 1)
  | otherwise = return ()
```

---

## Boolean Logic and Truthiness

### Boolean Operators

| Language | AND  | OR   | NOT  | Equality | Inequality |
|----------|------|------|------|----------|------------|
| Python   | and  | or   | not  | ==       | !=         |
| C++      | &&   | \|\| | !    | ==       | !=         |
| Haskell  | &&   | \|\| | not  | ==       | /=         |

### Truthiness: What Counts as True?

Different languages have different rules for what values are "truthy" in boolean contexts.

#### Python: False, None, 0, Empty

```python
# Falsy values: False, None, 0, 0.0, "", [], {}, ()
if []:
    print("Won't print")  # Empty list is falsy

if [1, 2, 3]:
    print("Will print")  # Non-empty list is truthy

# Useful in practice
numbers = [1, 2, 3]
if numbers:  # Check if list is not empty
    print(f"First number: {numbers[0]}")
```

**Falsy values in Python:**
- `False`
- `None`
- Numeric zero: `0`, `0.0`, `0j`
- Empty sequences: `""`, `[]`, `()`
- Empty mappings: `{}`

---

#### C++: Zero is False, Non-Zero is True

```cpp
// 0 is false, any non-zero is true
if (0) {
    std::cout << "Won't print" << std::endl;
}

if (5) {
    std::cout << "Will print" << std::endl;  // Any non-zero is true
}

if (nullptr) {
    std::cout << "Won't print" << std::endl;  // nullptr is false
}

// Modern C++: explicit bool checks preferred
int* ptr = nullptr;
if (ptr != nullptr) {  // More explicit
    std::cout << *ptr << std::endl;
}
```

**Note:** While C++ allows implicit conversions to bool, modern style prefers explicit comparisons for clarity.

---

#### Haskell: Only Bool Type

```haskell
-- Haskell has no "truthiness" - only True and False
-- This won't compile:
-- if 5 then "yes" else "no"  -- ERROR! Type mismatch

-- Must use explicit boolean:
if 5 > 0 then "yes" else "no"  -- OK

-- No implicit conversions
-- if [] then ... -- ERROR!
-- Use: if null [] then ...
```

**Philosophy:** No implicit conversions prevents bugs. If you want to check if a list is empty, use `null []` explicitly.

---

## Classic Example: FizzBuzz

FizzBuzz demonstrates control flow beautifully. The rules:
- Print numbers 1 to 100
- For multiples of 3, print "Fizz"
- For multiples of 5, print "Buzz"
- For multiples of both 3 and 5, print "FizzBuzz"

### Python

```python
for i in range(1, 101):
    if i % 15 == 0:
        print("FizzBuzz")
    elif i % 3 == 0:
        print("Fizz")
    elif i % 5 == 0:
        print("Buzz")
    else:
        print(i)
```

**Characteristics:** Straightforward, readable, uses statements.

---

### C++

```cpp
for (int i = 1; i <= 100; i++) {
    if (i % 15 == 0) {
        std::cout << "FizzBuzz" << std::endl;
    } else if (i % 3 == 0) {
        std::cout << "Fizz" << std::endl;
    } else if (i % 5 == 0) {
        std::cout << "Buzz" << std::endl;
    } else {
        std::cout << i << std::endl;
    }
}
```

**Characteristics:** Similar to Python but with explicit types and semicolons.

---

### Haskell: Functional Approach

```haskell
fizzbuzz :: Int -> String
fizzbuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 3 == 0  = "Fizz"
  | n `mod` 5 == 0  = "Buzz"
  | otherwise       = show n

main :: IO ()
main = mapM_ (putStrLn . fizzbuzz) [1..100]
```

**Characteristics:** Separates logic (fizzbuzz function) from iteration (mapM_). Highly composable.

**Alternative pattern matching approach:**
```haskell
fizzbuzz :: Int -> String
fizzbuzz n = case (n `mod` 3, n `mod` 5) of
  (0, 0) -> "FizzBuzz"
  (0, _) -> "Fizz"
  (_, 0) -> "Buzz"
  _      -> show n
```

---

## Comparison Table: Control Flow Features

| Feature            | Python              | C++                 | Haskell            |
|--------------------|---------------------|---------------------|--------------------|
| **If is...**       | Statement           | Statement           | Expression         |
| **Loops**          | for, while          | for, while, do-while| Recursion only     |
| **Pattern Match**  | Limited (3.10+)     | switch (limited)    | Yes, powerful      |
| **Truthiness**     | Flexible            | 0 = false           | Only Bool          |
| **Paradigm**       | Imperative          | Imperative/OOP      | Functional         |
| **Break/Continue** | Yes                 | Yes                 | N/A                |
| **Guards**         | No                  | No                  | Yes                |

---

## Key Insights

### Statements vs Expressions

**Imperative languages** (Python, C++) treat control flow as statements that perform actions:
```python
if x > 0:
    print("Positive")  # Performs an action
```

**Functional languages** (Haskell) treat control flow as expressions that return values:
```haskell
sign = if x > 0 then "Positive" else "Negative"  -- Returns a value
```

This reflects a fundamental paradigm difference:
- **Imperative:** Programs are sequences of commands changing state
- **Functional:** Programs are expressions computing values

### Loops vs Recursion

**Imperative languages** favor loops for repetition:
```cpp
for (int i = 0; i < n; i++) {
    sum += i;
}
```

**Functional languages** favor recursion:
```haskell
sumTo n
  | n <= 0    = 0
  | otherwise = n + sumTo (n - 1)
```

**Trade-offs:**
- Loops: Often more efficient (no stack overhead), familiar to most programmers
- Recursion: More composable, mathematically elegant, required in pure functional languages
- Haskell optimizes tail recursion, making it as efficient as loops

### Truthiness Trade-offs

**Flexible truthiness** (Python):
- **Pros:** Convenient, less boilerplate
- **Cons:** Can hide bugs, implicit behavior

**Strict typing** (Haskell, Modern C++):
- **Pros:** Prevents errors, explicit intent
- **Cons:** More verbose

## Exercises

See [EXERCISES.md](EXERCISES.md) for hands-on practice with:
- Conditional statements in all three languages
- Loop variations and recursion
- Pattern matching
- FizzBuzz and other classic problems
- Converting between imperative and functional styles

## Discussion Questions

1. **Why doesn't Haskell have traditional loops?** What does this reveal about the functional paradigm?

2. **Compare truthiness in Python vs C++.** Which approach do you prefer and why?

3. **Is if/else more readable than pattern matching, or vice versa?** When might you choose each?

4. **Haskell treats `if` as expressions. What advantages does this provide?**

5. **When would you prefer a for loop over recursion? When would you prefer recursion?**

6. **How does Python's significant whitespace affect code organization compared to C++'s braces?**

## Looking Ahead

In this course, you're mastering three core languages:
- **Python** for rapid development and data science
- **C++** for performance and systems programming
- **Haskell** for pure functional programming

Later, you'll also encounter:
- **Racket** (Lisp-family functional language)
- **C** (low-level systems programming)
- **Rust** (modern systems language with pattern matching)
- **Prolog** (logic programming with backtracking)

Each brings unique approaches to control flow!

## Next Lesson

In Lesson 4, we'll explore **Functions**: how to define reusable code, parameters and arguments, scope and closures, and the crucial difference between pure and impure functions.

## Summary

Control flow is fundamental to all programming, but languages express it in radically different ways:

- **Python:** Clean imperative style with flexible truthiness
- **C++:** Explicit control with multiple loop types and performance focus
- **Haskell:** Pure functional with expressions, guards, and pattern matching

Understanding these differences helps you:
- Choose the right tool for the problem
- Write idiomatic code in each language
- Appreciate the trade-offs different paradigms make
- Think about problems from multiple perspectives

Next up: Functions and abstraction!
