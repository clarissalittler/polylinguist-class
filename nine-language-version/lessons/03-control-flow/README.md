# Lesson 3: Control Flow

## Learning Objectives

By the end of this lesson, you will be able to:

1. Write conditional statements (if/else) in multiple languages
2. Understand the difference between statements and expressions
3. Use various loop constructs (for, while, recursion-based)
4. Explain boolean logic and truthiness across languages
5. Compare imperative loops with functional alternatives
6. Recognize when different control structures are idiomatic

## Introduction

Control flow determines the order in which code executes. While all programming languages need ways to make decisions and repeat operations, they express these concepts very differently. Understanding these differences reveals fundamental paradigm distinctions.

### Key Concepts

**Conditionals** allow programs to make decisions based on boolean conditions.

**Loops** allow programs to repeat operations, but not all languages use traditional loops.

**Expressions vs Statements**: Some languages treat control flow as expressions (returning values), others as statements (performing actions).

## Conditionals: Making Decisions

### Basic If/Else

All languages support conditional logic, but the syntax and philosophy vary.

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
- Indentation-based blocks
- `elif` for chained conditions
- Ternary operator for simple cases

#### JavaScript: Statements with Braces

```javascript
let age = 20;

if (age >= 18) {
    console.log("Adult");
    console.log("Can vote");
} else if (age >= 13) {
    console.log("Teenager");
} else {
    console.log("Child");
}

// Ternary expression
const status = age >= 18 ? "Adult" : "Minor";
```

**Key features:**
- C-style braces for blocks
- `else if` for chained conditions
- Ternary operator common

#### C: Statements, Explicit Types

```c
int age = 20;

if (age >= 18) {
    printf("Adult\n");
    printf("Can vote\n");
} else if (age >= 13) {
    printf("Teenager\n");
} else {
    printf("Child\n");
}

// Ternary operator
char* status = (age >= 18) ? "Adult" : "Minor";
```

**Key features:**
- Braces required for multi-statement blocks
- Conditions must be explicit boolean expressions
- No automatic string conversion

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
```

**Key features:**
- `if` is an expression, always returns a value
- Guards (`|`) are more idiomatic
- `otherwise` is just `True`, used for final case
- Immutability means no side effects in branches

#### Rust: Expressions with Safety

```rust
let age = 20;

// If as expression
let status = if age >= 18 {
    "Adult"
} else {
    "Minor"
};

// If as statement
if age >= 18 {
    println!("Adult");
    println!("Can vote");
} else if age >= 13 {
    println!("Teenager");
} else {
    println!("Child");
}
```

**Key features:**
- `if` is an expression (like Haskell)
- Can use for both value and side effects
- All branches must return same type

#### Ruby: Everything is an Expression

```ruby
age = 20

# If as expression
status = if age >= 18
           "Adult"
         else
           "Minor"
         end

# Postfix if (very Ruby)
puts "Can vote" if age >= 18

# Unless (negative if)
puts "Cannot vote" unless age >= 18
```

**Key features:**
- `if` always returns a value
- Postfix conditionals for simple cases
- `unless` for negative conditions

### Pattern Matching and Case Expressions

Some languages offer more powerful alternatives to if/else chains.

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
```

#### Rust: Match Expressions

```rust
fn describe_number(n: i32) -> &'static str {
    match n {
        0 => "Zero",
        1 => "One",
        2 => "Two",
        _ => "Many",
    }
}

// Match with guards
fn describe_age(age: i32) -> &'static str {
    match age {
        n if n < 13 => "Child",
        n if n < 18 => "Teenager",
        _ => "Adult",
    }
}
```

#### Racket: Cond Expression

```racket
(define (describe-age age)
  (cond
    [(< age 13) "Child"]
    [(< age 18) "Teenager"]
    [else "Adult"]))
```

#### Prolog: Declarative Rules

```prolog
% Facts and rules define logic
describe_age(Age, 'Child') :- Age < 13.
describe_age(Age, 'Teenager') :- Age >= 13, Age < 18.
describe_age(Age, 'Adult') :- Age >= 18.

% Query: describe_age(20, Status).
% Returns: Status = 'Adult'
```

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
```

**Philosophy:** Iterate over iterables, not indices

#### JavaScript: Multiple Styles

```javascript
// Traditional for loop
for (let i = 0; i < 5; i++) {
    console.log(i);
}

// For-of (ES6)
const fruits = ["apple", "banana", "cherry"];
for (const fruit of fruits) {
    console.log(fruit);
}

// forEach method
fruits.forEach((fruit, i) => {
    console.log(`${i}: ${fruit}`);
});
```

**Philosophy:** Multiple styles for different needs

#### C: Traditional Counting Loop

```c
// Classic for loop
for (int i = 0; i < 5; i++) {
    printf("%d\n", i);
}

// Iterate array
char* fruits[] = {"apple", "banana", "cherry"};
int length = 3;
for (int i = 0; i < length; i++) {
    printf("%s\n", fruits[i]);
}
```

**Philosophy:** Explicit index-based iteration

#### Java: Traditional and Enhanced

```java
// Traditional for loop
for (int i = 0; i < 5; i++) {
    System.out.println(i);
}

// Enhanced for (for-each)
String[] fruits = {"apple", "banana", "cherry"};
for (String fruit : fruits) {
    System.out.println(fruit);
}
```

#### Ruby: Blocks and Iterators

```ruby
# Times method
5.times do |i|
  puts i
end

# Each method
fruits = ["apple", "banana", "cherry"]
fruits.each do |fruit|
  puts fruit
end

# Each with index
fruits.each_with_index do |fruit, i|
  puts "#{i}: #{fruit}"
end
```

**Philosophy:** Methods with blocks, not loops

#### Rust: Ranges and Iterators

```rust
// Range iteration
for i in 0..5 {
    println!("{}", i);
}

// Iterate over collection
let fruits = vec!["apple", "banana", "cherry"];
for fruit in &fruits {
    println!("{}", fruit);
}

// With enumerate
for (i, fruit) in fruits.iter().enumerate() {
    println!("{}: {}", i, fruit);
}
```

### While Loops

#### Python

```python
count = 0
while count < 5:
    print(count)
    count += 1
```

#### JavaScript

```javascript
let count = 0;
while (count < 5) {
    console.log(count);
    count++;
}
```

#### C

```c
int count = 0;
while (count < 5) {
    printf("%d\n", count);
    count++;
}
```

#### Haskell: No Traditional Loops!

Haskell doesn't have traditional loops. Use recursion or higher-order functions:

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
```

#### Racket: Recursion and Named Let

```racket
; Recursion
(define (print-numbers n)
  (when (< n 5)
    (displayln n)
    (print-numbers (+ n 1))))

; Named let (tail recursion)
(let loop ([n 0])
  (when (< n 5)
    (displayln n)
    (loop (+ n 1))))
```

## Boolean Logic and Truthiness

### Boolean Operators

All languages support AND, OR, and NOT, but syntax varies:

| Language   | AND  | OR   | NOT  | Equality | Inequality |
|------------|------|------|------|----------|------------|
| Python     | and  | or   | not  | ==       | !=         |
| JavaScript | &&   | \|\| | !    | === (!== for strict) | !== |
| C          | &&   | \|\| | !    | ==       | !=         |
| Java       | &&   | \|\| | !    | ==       | !=         |
| Ruby       | &&   | \|\| | !    | ==       | !=         |
| Haskell    | &&   | \|\| | not  | ==       | /=         |
| Rust       | &&   | \|\| | !    | ==       | !=         |
| Racket     | and  | or   | not  | equal?   | (not (equal? ...)) |

### Truthiness: What Counts as True?

Different languages have different rules for what values are "truthy" in boolean contexts.

#### Python: False, None, 0, Empty

```python
# Falsy values: False, None, 0, 0.0, "", [], {}, ()
if []:
    print("Won't print")

if [1, 2, 3]:
    print("Will print")  # Non-empty list is truthy
```

#### JavaScript: Complex Truthiness

```javascript
// Falsy: false, null, undefined, 0, NaN, ""
if ("") {
    console.log("Won't print");
}

if ("hello") {
    console.log("Will print");  // Non-empty string is truthy
}

// Watch out!
if ([]) {
    console.log("Will print!");  // Empty array is truthy in JS
}
```

#### C: Zero is False, Non-Zero is True

```c
// 0 is false, any non-zero is true
if (0) {
    printf("Won't print\n");
}

if (5) {
    printf("Will print\n");  // Any non-zero is true
}

if (NULL) {
    printf("Won't print\n");  // NULL is 0
}
```

#### Haskell: Only Bool Type

```haskell
-- Haskell has no "truthiness" - only True and False
-- This won't compile:
-- if 5 then "yes" else "no"  -- ERROR!

-- Must use explicit boolean:
if 5 > 0 then "yes" else "no"  -- OK
```

#### Rust: Only bool Type

```rust
// Like Haskell, only actual bool values
// This won't compile:
// if 5 { println!("no"); }  // ERROR!

// Must use explicit boolean:
if 5 > 0 { println!("yes"); }  // OK
```

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

### JavaScript

```javascript
for (let i = 1; i <= 100; i++) {
    if (i % 15 === 0) {
        console.log("FizzBuzz");
    } else if (i % 3 === 0) {
        console.log("Fizz");
    } else if (i % 5 === 0) {
        console.log("Buzz");
    } else {
        console.log(i);
    }
}
```

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

### Rust: Pattern Matching

```rust
fn fizzbuzz(n: i32) -> String {
    match (n % 3, n % 5) {
        (0, 0) => "FizzBuzz".to_string(),
        (0, _) => "Fizz".to_string(),
        (_, 0) => "Buzz".to_string(),
        _ => n.to_string(),
    }
}

fn main() {
    for i in 1..=100 {
        println!("{}", fizzbuzz(i));
    }
}
```

### Racket: Recursive and Functional

```racket
#lang racket

(define (fizzbuzz n)
  (cond
    [(= (modulo n 15) 0) "FizzBuzz"]
    [(= (modulo n 3) 0) "Fizz"]
    [(= (modulo n 5) 0) "Buzz"]
    [else (number->string n)]))

(define (fizzbuzz-range start end)
  (when (<= start end)
    (displayln (fizzbuzz start))
    (fizzbuzz-range (+ start 1) end)))

(fizzbuzz-range 1 100)
```

## Comparison Table: Control Flow Features

| Language   | If is... | Loops | Pattern Matching | Truthiness |
|------------|----------|-------|------------------|------------|
| Python     | Statement | for, while | Limited (3.10+) | Flexible |
| JavaScript | Statement | for, while, for-of | No | Very flexible |
| C          | Statement | for, while, do-while | No | 0 = false |
| Java       | Statement | for, while, enhanced-for | Limited | Only bool |
| Ruby       | Expression | Iterators | No | nil, false only |
| Haskell    | Expression | No loops! | Yes, powerful | Only Bool |
| Racket     | Expression | Recursion | Yes | #f only |
| Prolog     | Declarative | Backtracking | Yes, core feature | N/A |
| Rust       | Expression | for, while, loop | Yes, powerful | Only bool |

## Key Insights

### Statements vs Expressions

**Imperative languages** (C, Java, JavaScript) treat control flow as statements that perform actions.

**Functional languages** (Haskell, Racket) and some modern languages (Rust, Ruby) treat control flow as expressions that return values.

This reflects a fundamental paradigm difference:
- **Imperative:** Programs are sequences of commands changing state
- **Functional:** Programs are expressions computing values

### Loops vs Recursion

**Imperative languages** favor loops for repetition (for, while).

**Functional languages** favor recursion and higher-order functions (map, filter).

Both accomplish the same goals, but:
- Loops are often more efficient (no stack overhead)
- Recursion is more composable and mathematically elegant
- Modern functional languages optimize tail recursion

### Truthiness

Languages make different tradeoffs:
- **Flexible** (Python, JavaScript): Convenient but can hide bugs
- **Strict** (Haskell, Rust): More verbose but prevents errors

## Exercises

### Exercise 1: Even or Odd (Warmup)

Write a program that takes a number and prints whether it's even or odd.

Implement in at least 3 languages from different paradigms.

### Exercise 2: Grade Calculator

Write a program that converts a numeric score (0-100) to a letter grade:
- 90-100: A
- 80-89: B
- 70-79: C
- 60-69: D
- Below 60: F

Implement in:
- An imperative language using if/else
- Haskell using guards
- Rust using match

### Exercise 3: Sum of Numbers

Write a program that computes the sum of numbers from 1 to N.

Implement using:
- A for loop (Python, JavaScript, or C)
- A while loop (any language)
- Recursion (Haskell or Racket)
- A higher-order function (Haskell's `sum` and `[1..n]`)

### Exercise 4: FizzBuzz Variations

Modify FizzBuzz:
1. Use different numbers (e.g., 7 and 11 instead of 3 and 5)
2. Add a third condition (e.g., multiples of 7 print "Boom")
3. Implement in a language you haven't used yet

### Exercise 5: Collatz Sequence

Implement the Collatz conjecture sequence:
- If n is even: divide by 2
- If n is odd: multiply by 3 and add 1
- Repeat until n = 1

Print the sequence for a starting number.

Challenge: Count how many steps it takes to reach 1.

### Exercise 6: Pattern Matching Exploration

For languages with pattern matching (Haskell, Rust, Prolog):

Write a function that describes a playing card:
- Input: suit (hearts, diamonds, clubs, spades) and rank (1-13)
- Output: description (e.g., "Ace of Spades", "Queen of Hearts")

Use pattern matching to handle special cases (Ace, Jack, Queen, King).

## Discussion Questions

1. **Why doesn't Haskell have traditional loops?** What does this reveal about the functional paradigm?

2. **Compare truthiness in Python vs JavaScript.** Which approach do you prefer and why?

3. **Is if/else more readable than pattern matching, or vice versa?** When might you choose each?

4. **Rust and Haskell treat `if` as expressions. What advantages does this provide?**

5. **How does Prolog's declarative approach to control flow differ fundamentally from imperative loops?**

6. **Why do some languages (Python, Ruby) provide multiple ways to express conditionals (if, ternary, postfix)?**

## Going Deeper

### Resources

- **For pattern matching:** Explore Haskell's case expressions and Rust's match
- **For loops:** Compare Python's for-in with C's indexed loops
- **For recursion:** Try rewriting imperative loops as recursive functions

### Next Lesson Preview

In Lesson 4, we'll explore **Functions**: how to define reusable code, parameters and arguments, scope and closures, and the crucial difference between pure and impure functions.

## Summary

Control flow is fundamental to all programming, but languages express it in radically different ways:

- **Imperative languages** use statements, loops, and mutable state
- **Functional languages** use expressions, recursion, and immutability
- **Logic languages** use declarative rules and backtracking

Understanding these differences helps you:
- Choose the right tool for the problem
- Write idiomatic code in each language
- Appreciate the tradeoffs different paradigms make

Next up: Functions and abstraction!
