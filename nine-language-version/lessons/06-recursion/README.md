# Lesson 6: Recursion

## Overview

Recursion is a fundamental programming technique where a function calls itself to solve a problem by breaking it down into smaller, similar subproblems. It's essential in functional programming and provides elegant solutions to many problems.

## Learning Objectives

By the end of this lesson, you will:
- Understand how recursion works
- Recognize base cases and recursive cases
- Implement recursive algorithms in multiple paradigms
- Compare recursion with iteration
- Understand tail recursion and its optimization
- See how different languages handle recursion

## What is Recursion?

**Recursion** is when a function calls itself, either directly or indirectly.

### Key Components

Every recursive function needs:

1. **Base Case**: The stopping condition that prevents infinite recursion
2. **Recursive Case**: The part that breaks the problem down and calls itself
3. **Progress**: Each recursive call must move toward the base case

### Simple Example: Countdown

```python
def countdown(n):
    if n <= 0:           # Base case
        print("Blast off!")
    else:                # Recursive case
        print(n)
        countdown(n - 1) # Recursive call (getting closer to base case)
```

## Recursion vs Iteration

Many problems can be solved with either recursion or iteration (loops).

### Factorial: Iterative Approach

```python
def factorial_iterative(n):
    result = 1
    for i in range(1, n + 1):
        result *= i
    return result
```

### Factorial: Recursive Approach

```python
def factorial_recursive(n):
    if n <= 1:              # Base case
        return 1
    else:                   # Recursive case
        return n * factorial_recursive(n - 1)
```

**Trace of `factorial_recursive(5)`:**
```
factorial(5) = 5 * factorial(4)
             = 5 * (4 * factorial(3))
             = 5 * (4 * (3 * factorial(2)))
             = 5 * (4 * (3 * (2 * factorial(1))))
             = 5 * (4 * (3 * (2 * 1)))
             = 5 * (4 * (3 * 2))
             = 5 * (4 * 6)
             = 5 * 24
             = 120
```

## Classic Recursive Problems

### 1. Fibonacci Sequence

Each number is the sum of the two preceding ones: 0, 1, 1, 2, 3, 5, 8, 13, ...

```
fib(0) = 0
fib(1) = 1
fib(n) = fib(n-1) + fib(n-2)
```

### 2. Sum of List

```
sum([]) = 0
sum([x, ...rest]) = x + sum(rest)
```

### 3. Binary Search (on sorted list)

```
search([], target) = not found
search([middle, ...], target):
    if target == middle: found
    if target < middle: search(left half)
    if target > middle: search(right half)
```

### 4. Tree Traversal

Recursion naturally models hierarchical structures like trees.

## Tail Recursion

**Tail recursion** is when the recursive call is the last operation in the function.

### Not Tail Recursive (Regular Recursion)

```python
def factorial(n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)  # Must multiply after recursive call
```

### Tail Recursive

```python
def factorial_tail(n, accumulator=1):
    if n <= 1:
        return accumulator
    return factorial_tail(n - 1, n * accumulator)  # Recursive call is last operation
```

**Why it matters:**
- Tail-recursive functions can be optimized by compilers
- Can be converted to iteration automatically (Tail Call Optimization)
- Prevents stack overflow for deep recursion
- Haskell, Scheme/Racket, and some others optimize tail calls
- Python and JavaScript (mostly) don't optimize tail calls

## Recursion in Different Paradigms

### Functional Languages (Haskell, Racket)
- **Primary approach** to repetition
- No loops, only recursion
- Tail call optimization built-in
- Pattern matching makes base cases clean

### Imperative Languages (Python, C, Java)
- **Both recursion and loops** available
- Iteration often preferred for simple cases
- Recursion used for tree/graph problems
- Stack depth limitations

### Systems Languages (Rust, C)
- Recursion possible but stack space limited
- Iteration often preferred for performance
- Careful with stack overflow

### Logic Languages (Prolog)
- **Recursion is fundamental**
- Backtracking + recursion = powerful search
- No explicit loops

## Common Pitfalls

### 1. Missing Base Case
```python
def infinite(n):
    return infinite(n - 1)  # Never stops!
```

### 2. Not Making Progress
```python
def stuck(n):
    if n == 0:
        return 0
    return stuck(n)  # Doesn't get closer to base case!
```

### 3. Multiple Recursive Calls (Inefficiency)
```python
def fib(n):
    if n <= 1:
        return n
    return fib(n-1) + fib(n-2)  # Recalculates same values many times!
```

**Solution:** Use memoization or dynamic programming

## When to Use Recursion

**Good uses:**
- Tree and graph traversal
- Divide-and-conquer algorithms (merge sort, quicksort)
- Problems with recursive structure (parsing, evaluation)
- Mathematical sequences (factorial, Fibonacci)
- Backtracking problems (Sudoku, N-Queens)

**When iteration might be better:**
- Simple loops over a range
- Tail-recursive functions in languages without TCO
- Very deep recursion (stack overflow risk)
- Performance-critical code

## Stack Overflow

Recursion uses the call stack. Each recursive call adds a frame.

```python
# This will crash with RecursionError in Python
def infinite_recursion(n):
    return infinite_recursion(n + 1)
```

**Python's default recursion limit:** ~1000 calls
**Solutions:**
- Rewrite iteratively
- Use tail recursion (in languages with TCO)
- Increase recursion limit (not recommended)
- Use dynamic programming

## Mutual Recursion

Functions can call each other recursively:

```python
def is_even(n):
    if n == 0:
        return True
    return is_odd(n - 1)

def is_odd(n):
    if n == 0:
        return False
    return is_even(n - 1)
```

## Tree Recursion

When a function makes multiple recursive calls:

```python
def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n-1) + fibonacci(n-2)  # Two recursive calls
```

**Call tree for `fib(5)`:**
```
                    fib(5)
                   /      \
              fib(4)        fib(3)
             /     \        /     \
        fib(3)   fib(2)  fib(2)  fib(1)
        /   \    /   \   /   \
    fib(2) fib(1) ...  ...  ...
```

Many redundant calculations!

## Higher-Order Recursive Functions

Recursion combined with higher-order functions:

```python
def map_recursive(f, lst):
    """Apply function f to each element"""
    if not lst:
        return []
    return [f(lst[0])] + map_recursive(f, lst[1:])

def filter_recursive(pred, lst):
    """Keep only elements where pred is True"""
    if not lst:
        return []
    if pred(lst[0]):
        return [lst[0]] + filter_recursive(pred, lst[1:])
    return filter_recursive(pred, lst[1:])
```

## Examples Across Languages

We'll implement several classic recursive algorithms:

1. **Factorial** - Simple recursion
2. **Fibonacci** - Tree recursion
3. **Sum of list** - List recursion
4. **Reverse list** - List manipulation
5. **Binary search** - Divide and conquer
6. **Quicksort** - Divide and conquer sorting
7. **Tower of Hanoi** - Multiple recursive calls
8. **Tree traversal** - Recursive data structures

## Language Files

- `recursion.py` - Python (supports recursion, no TCO)
- `recursion.js` - JavaScript (supports recursion, limited TCO)
- `recursion.c` - C (supports recursion, manual memory)
- `RecursionDemo.java` - Java (supports recursion, no TCO)
- `recursion.rb` - Ruby (supports recursion, no TCO)
- `recursion.hs` - Haskell (recursion primary, has TCO)
- `recursion.rkt` - Racket (recursion primary, has TCO)
- `recursion.pl` - Prolog (recursion fundamental)
- `recursion.rs` - Rust (supports recursion, careful with stack)

## Key Takeaways

1. **Every recursive function needs a base case** to prevent infinite recursion
2. **Each recursive call must make progress** toward the base case
3. **Recursion is natural for tree/graph problems** and hierarchical data
4. **Functional languages prefer recursion** over iteration
5. **Tail recursion can be optimized** in some languages
6. **Stack overflow is a real concern** - be mindful of recursion depth
7. **Some recursive solutions are inefficient** - consider memoization/DP

## Next Steps

After this lesson, you'll understand:
- How to think recursively
- When recursion is the right tool
- How different paradigms approach recursion
- Optimization techniques (tail recursion, memoization)

This completes the core programming concepts. Next lessons might cover:
- Object-Oriented Programming
- Error Handling and I/O
- Concurrency and Parallelism
- Advanced Functional Programming

---

Let's see recursion in action across all 9 languages!
