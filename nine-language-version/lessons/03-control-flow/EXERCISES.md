# Lesson 3: Control Flow - Exercises

## Instructions

Complete these exercises to practice control flow concepts across different languages. For each exercise, try implementing in at least 2-3 languages from different paradigms.

## Exercise 1: Even or Odd (Warmup)

**Difficulty:** Easy

Write a program that:
- Takes a number as input (or use a hard-coded value)
- Prints "Even" if the number is even
- Prints "Odd" if the number is odd

**Languages to try:** Python, JavaScript, C, or any language you choose

**Bonus:** Implement using:
- An if/else statement
- A ternary operator (if available)
- Pattern matching (Haskell, Rust)

---

## Exercise 2: Grade Calculator

**Difficulty:** Easy

Write a program that converts a numeric score (0-100) to a letter grade:
- 90-100: A
- 80-89: B
- 70-79: C
- 60-69: D
- Below 60: F

**Implementation requirements:**

1. **Python or JavaScript:** Use if/elif/else
2. **Haskell:** Use guards
3. **Rust:** Use match with guards

**Test cases:**
- Score 95 → A
- Score 85 → B
- Score 72 → C
- Score 55 → F

---

## Exercise 3: Sum of Numbers

**Difficulty:** Easy to Medium

Write a program that computes the sum of numbers from 1 to N (e.g., N=10: 1+2+3+...+10 = 55).

**Implementation requirements - try all three:**

1. **Imperative (Python, JavaScript, or C):** Use a for loop
2. **While loop:** Use a while loop in any language
3. **Recursive (Haskell or Racket):** Use recursion
4. **Functional (Haskell):** Use `sum [1..n]` or equivalent

**Test cases:**
- N=5 → 15
- N=10 → 55
- N=100 → 5050

**Discussion:** Which implementation is clearest? Most efficient?

---

## Exercise 4: Multiplication Table

**Difficulty:** Medium

Write a program that prints a multiplication table for a given number N.

Example for N=5:
```
5 x 1 = 5
5 x 2 = 10
5 x 3 = 15
...
5 x 10 = 50
```

**Implementation approaches:**

1. **Imperative:** Use a for loop
2. **Functional:** Use map or list comprehension
3. **Recursive:** Define a recursive function

**Languages to try:** Any 2-3 from different paradigms

---

## Exercise 5: FizzBuzz Variations

**Difficulty:** Medium

### Part A: Classic FizzBuzz

Implement FizzBuzz for numbers 1-100:
- Multiples of 3: print "Fizz"
- Multiples of 5: print "Buzz"
- Multiples of both: print "FizzBuzz"
- Otherwise: print the number

**Implement in at least 2 languages you haven't used yet.**

### Part B: Custom FizzBuzz

Modify FizzBuzz to use different numbers:
- Multiples of 7: print "Boom"
- Multiples of 11: print "Bang"
- Multiples of both 7 and 11: print "BoomBang"

### Part C: Triple FizzBuzz

Add a third condition:
- Multiples of 3: "Fizz"
- Multiples of 5: "Buzz"
- Multiples of 7: "Boom"
- Combine for multiples (e.g., 15 → "FizzBuzz", 21 → "FizzBoom", 35 → "BuzzBoom", 105 → "FizzBuzzBoom")

**Challenge:** How does your implementation scale? Is it easy to add more conditions?

---

## Exercise 6: Collatz Sequence

**Difficulty:** Medium

The Collatz conjecture says that starting from any positive integer:
- If n is even: divide by 2
- If n is odd: multiply by 3 and add 1
- Repeat until n = 1

Write a program that:
1. Prints the Collatz sequence for a starting number
2. Counts how many steps it takes to reach 1

Example for N=10:
```
10 → 5 → 16 → 8 → 4 → 2 → 1
Steps: 6
```

**Test cases:**
- N=10 → 6 steps
- N=19 → 20 steps
- N=27 → 111 steps

**Implement using:**
- A while loop (Python, JavaScript, C)
- Recursion (Haskell, Racket)

**Challenge:** Find the number between 1 and 100 that takes the most steps.

---

## Exercise 7: Number Guessing Game

**Difficulty:** Medium

Write an interactive number guessing game:
1. Computer picks a random number between 1 and 100
2. User guesses
3. Computer responds with "Too high", "Too low", or "Correct!"
4. Count the number of guesses

**Implementation notes:**
- Use loops for repeated guessing
- Use conditionals to provide feedback
- Stop when user guesses correctly

**Languages:** Choose an imperative language with easy I/O (Python, JavaScript, Ruby)

**Bonus:** Limit the number of guesses (e.g., 7 attempts)

---

## Exercise 8: Prime Number Checker

**Difficulty:** Medium

Write a program that determines if a number is prime.

A prime number is only divisible by 1 and itself.

**Algorithm:**
- Check if n < 2 (not prime)
- Check if n == 2 (prime)
- Check if n is divisible by any number from 2 to sqrt(n)

**Test cases:**
- 2 → Prime
- 7 → Prime
- 9 → Not prime (divisible by 3)
- 17 → Prime
- 20 → Not prime (divisible by 2, 4, 5)

**Implement in:**
- An imperative language using a loop
- Haskell using recursion or list comprehensions

**Challenge:** Print all prime numbers from 1 to 100

---

## Exercise 9: Pattern Matching Practice

**Difficulty:** Medium

For languages with pattern matching (Haskell, Rust, Racket, Prolog):

### Part A: Playing Cards

Write a function that describes a playing card:
- Input: rank (1-13) and suit (hearts, diamonds, clubs, spades)
- Output: description

Example:
- (1, hearts) → "Ace of Hearts"
- (11, spades) → "Jack of Spades"
- (12, diamonds) → "Queen of Diamonds"
- (13, clubs) → "King of Clubs"
- (5, hearts) → "5 of Hearts"

**Use pattern matching for special ranks** (Ace, Jack, Queen, King)

### Part B: List Patterns

Write a function that describes a list based on its structure:
- Empty list → "Empty"
- One element → "Singleton: <element>"
- Two elements → "Pair: <first> and <second>"
- Three or more → "List starting with <first>, <second>, <third>, ..."

**Languages:** Haskell, Rust, or Racket

---

## Exercise 10: Truth Table Generator

**Difficulty:** Hard

Write a program that generates truth tables for boolean expressions.

### Part A: Basic Operations

Generate truth tables for:
- AND (p ∧ q)
- OR (p ∨ q)
- NOT (¬p)
- XOR (p ⊕ q)
- IMPLIES (p → q)

Example output for AND:
```
p     | q     | p AND q
------|-------|--------
false | false | false
false | true  | false
true  | false | false
true  | true  | true
```

### Part B: Complex Expressions

Generate truth table for: (p ∧ q) ∨ (¬p ∧ r)

**Implementation tips:**
- Use nested loops to iterate all combinations
- Use boolean logic operators
- Format output as a table

**Languages:** Any language with good boolean support

---

## Exercise 11: Pascal's Triangle

**Difficulty:** Hard

Generate Pascal's Triangle up to N rows.

Example for N=5:
```
    1
   1 1
  1 2 1
 1 3 3 1
1 4 6 4 1
```

**Implementation approaches:**

1. **Iterative:** Use nested loops
2. **Recursive:** Each element is sum of two elements above it
3. **Functional:** Use list operations

**Languages:** Try at least one imperative and one functional language

**Hint:** Each row can be computed from the previous row

---

## Exercise 12: Calendar Month

**Difficulty:** Hard

Write a program that prints a calendar for a given month and year.

Example for January 2025:
```
   January 2025
Su Mo Tu We Th Fr Sa
          1  2  3  4
 5  6  7  8  9 10 11
12 13 14 15 16 17 18
19 20 21 22 23 24 25
26 27 28 29 30 31
```

**Requirements:**
- Calculate which day of the week the month starts on
- Handle months with different numbers of days
- Handle leap years for February

**Hint:** You may use library functions to determine the day of the week

**Languages:** Python, JavaScript, or any language with date libraries

---

## Challenge Projects

### Challenge 1: Game of Life

Implement Conway's Game of Life for a small grid (10x10).

Rules:
- Any live cell with 2-3 live neighbors survives
- Any dead cell with exactly 3 live neighbors becomes alive
- All other cells die or stay dead

**Display several generations.**

### Challenge 2: Text-Based Adventure

Create a simple text adventure game with:
- Multiple rooms
- Items to collect
- Choices that affect the outcome
- Use conditionals and state management

### Challenge 3: Sorting Visualizer

Implement a sorting algorithm (bubble sort, insertion sort) that:
- Prints each step of the sorting process
- Uses loops and conditionals
- Shows comparisons and swaps

---

## Reflection Questions

After completing the exercises, consider:

1. **How did control flow differ between imperative and functional languages?**

2. **When did pattern matching feel more natural than if/else?**

3. **Which language made recursion easiest? Why?**

4. **Did any language's approach to truthiness surprise you?**

5. **For the same problem, which language felt most natural? Which was most concise?**

6. **How does Prolog's backtracking differ from traditional loops?**

---

## Solutions

Solutions for all exercises will be provided in a separate directory. Try to complete exercises on your own first!

Remember: The goal is not just to make it work, but to understand how each language approaches control flow differently.
