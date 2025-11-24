# Lesson 8: Higher-Order Functions - Exercises

## Instructions

Complete these exercises to practice higher-order functions in Python, C++, and Haskell. Focus on functional thinking: transforming data through composed functions rather than explicit loops.

**Target Languages:** Python, C++ (modern), Haskell

---

## Exercise 1: Basic Higher-Order Functions (Warmup)

**Difficulty:** Easy
**Recommended Languages:** All three

Implement these basic higher-order functions from scratch (without using built-ins):

**1. myMap** - Apply function to each element
```python
# Python
def my_map(func, items):
    # Your implementation
    pass

# Test
assert my_map(lambda x: x * 2, [1, 2, 3]) == [2, 4, 6]
```

**2. myFilter** - Keep elements that satisfy predicate
```python
def my_filter(pred, items):
    # Your implementation
    pass

# Test
assert my_filter(lambda x: x % 2 == 0, [1, 2, 3, 4, 5]) == [2, 4]
```

**3. myFold/myReduce** - Combine elements with binary function
```python
def my_fold(func, items, initial):
    # Your implementation
    pass

# Test
assert my_fold(lambda acc, x: acc + x, [1, 2, 3, 4, 5], 0) == 15
```

**Challenge:** Implement in C++ using templates and in Haskell with explicit recursion.

---

## Exercise 2: Function Composition

**Difficulty:** Easy to Medium
**Recommended Languages:** All three

**Part A:** Implement a `compose` function that combines two functions.

```python
# Python
def compose(f, g):
    """Return function that applies g then f: f(g(x))"""
    # Your implementation
    pass

# Test
add_one = lambda x: x + 1
double = lambda x: x * 2
add_then_double = compose(double, add_one)
assert add_then_double(5) == 12  # (5 + 1) * 2
```

**Part B:** Implement a `pipe` function that composes left-to-right.

```python
def pipe(*functions):
    """Apply functions left to right"""
    # Your implementation
    pass

# Test
transform = pipe(
    lambda x: x + 1,
    lambda x: x * 2,
    lambda x: x - 3
)
assert transform(5) == 9  # ((5 + 1) * 2) - 3
```

**Part C:** Create a data processing pipeline:
- Filter out negative numbers
- Square each number
- Sum the results

Test with: `[-2, -1, 0, 1, 2, 3, 4, 5]` → Expected: 55

---

## Exercise 3: Closures and State

**Difficulty:** Medium
**Recommended Languages:** Python, C++

Create these closure-based functions:

**1. Counter Factory**
```python
def make_counter(start=0, step=1):
    """
    Returns a counter function that increments by step each call.
    """
    # Your implementation
    pass

# Test
counter1 = make_counter(0, 1)
counter2 = make_counter(10, 5)
assert counter1() == 1
assert counter1() == 2
assert counter2() == 15
assert counter1() == 3
```

**2. Accumulator**
```python
def make_accumulator(initial=0):
    """
    Returns a function that accumulates values.
    """
    # Your implementation
    pass

# Test
acc = make_accumulator(0)
assert acc(5) == 5
assert acc(3) == 8
assert acc(-2) == 6
```

**3. Function Cache/Memoizer**
```python
def memoize(func):
    """
    Returns a memoized version of func that caches results.
    """
    # Your implementation
    pass

# Test
@memoize
def slow_function(n):
    # Simulates expensive computation
    return n * n

assert slow_function(5) == 25
# Second call should use cache (verify with timing or call counting)
```

---

## Exercise 4: Practical Data Transformations

**Difficulty:** Medium
**Recommended Languages:** All three

Given this dataset of students:

```python
students = [
    {"name": "Alice", "age": 20, "grades": [85, 90, 88]},
    {"name": "Bob", "age": 22, "grades": [78, 85, 80]},
    {"name": "Charlie", "age": 21, "grades": [92, 95, 90]},
    {"name": "Diana", "age": 20, "grades": [88, 86, 90]},
    {"name": "Eve", "age": 23, "grades": [75, 80, 78]}
]
```

Using only higher-order functions (map, filter, reduce), solve:

**1.** Get names of students with average grade >= 85
**2.** Calculate average age of students with average grade >= 85
**3.** Get list of all grades (flattened)
**4.** Find highest single grade across all students
**5.** Group students by age (return dict/map)

**No explicit loops allowed!** Use only functional transformations.

---

## Exercise 5: Currying and Partial Application

**Difficulty:** Medium
**Recommended Languages:** Haskell (native), Python (manual)

**Part A (Haskell):** Create these curried functions:

```haskell
-- 1. Add three numbers (fully curried)
add3 :: Int -> Int -> Int -> Int
add3 = ?

-- Test
assert (add3 1 2 3 == 6)
assert (add3 1 2 $ 3 == 6)
assert ((add3 1) 2 3 == 6)

-- 2. Create partially applied versions
add10 :: Int -> Int -> Int
add10 = add3 10

add10and5 :: Int -> Int
add10and5 = add10 5

-- Test
assert (add10and5 3 == 18)
```

**Part B (Python):** Manually curry a function:

```python
# Curry this function:
def multiply(a, b, c):
    return a * b * c

# Create curried version
curried_multiply = ?

# Should work like:
assert curried_multiply(2)(3)(4) == 24
times_six = curried_multiply(2)(3)
assert times_six(4) == 24
```

**Part C:** Use `functools.partial` to create specialized functions:

```python
from functools import partial

def power(base, exponent):
    return base ** exponent

# Create: square, cube, and quad functions using partial
square = ?
cube = ?
quad = ?

assert square(5) == 25
assert cube(3) == 27
assert quad(2) == 16
```

---

## Exercise 6: Function Combinators

**Difficulty:** Hard
**Recommended Languages:** Python, Haskell

Implement these useful combinators:

**1. `apply_n`** - Apply function n times
```python
def apply_n(n, func):
    """Returns function that applies func n times"""
    # Your implementation
    pass

# Test
add_one = lambda x: x + 1
add_five = apply_n(5, add_one)
assert add_five(10) == 15
```

**2. `alternator`** - Alternate between two functions
```python
def alternator(f, g):
    """Returns function that alternates between f and g each call"""
    # Your implementation (needs closure state)
    pass

# Test
alt = alternator(lambda x: x + 1, lambda x: x * 2)
assert alt(5) == 6   # Uses f
assert alt(5) == 10  # Uses g
assert alt(5) == 6   # Uses f again
```

**3. `until`** - Apply function until predicate is true
```python
def until(predicate, func, initial):
    """
    Apply func to initial, then to result, etc.
    until predicate returns True
    """
    # Your implementation
    pass

# Test
result = until(lambda x: x > 100, lambda x: x * 2, 1)
assert result == 128  # 1 -> 2 -> 4 -> 8 -> 16 -> 32 -> 64 -> 128
```

---

## Exercise 7: Map-Filter-Reduce Pipeline

**Difficulty:** Medium
**Recommended Languages:** All three

**Problem:** Process a list of transactions to find total spending on specific categories.

```python
transactions = [
    {"id": 1, "amount": 50.00, "category": "food"},
    {"id": 2, "amount": 30.00, "category": "transport"},
    {"id": 3, "amount": 20.00, "category": "food"},
    {"id": 4, "amount": 100.00, "category": "entertainment"},
    {"id": 5, "amount": 15.00, "category": "food"},
    {"id": 6, "amount": 25.00, "category": "transport"},
    {"id": 7, "amount": 200.00, "category": "rent"},
]
```

Using map-filter-reduce pipeline:

**1.** Total spending on "food"
**2.** Average transaction amount for "transport"
**3.** List of categories with spending > $50
**4.** Transform: add 10% tax to all amounts, return new list of dicts

**Constraint:** Use only map/filter/reduce. No explicit loops!

---

## Exercise 8: Custom Sorting with Higher-Order Functions

**Difficulty:** Medium
**Recommended Languages:** All three

Given this list of items:

```python
items = [
    ("apple", 3, 1.50),      # (name, quantity, price_per_unit)
    ("banana", 5, 0.80),
    ("orange", 2, 2.00),
    ("grape", 10, 0.50),
    ("mango", 1, 3.00)
]
```

Create a higher-order `sort_by` function:

```python
def sort_by(key_func):
    """
    Returns a sorting function that sorts by key_func.
    """
    # Your implementation
    pass

# Usage:
by_name = sort_by(lambda item: item[0])
by_quantity = sort_by(lambda item: item[1])
by_total_price = sort_by(lambda item: item[1] * item[2])

sorted_by_total = by_total_price(items)
# Should sort by quantity * price_per_unit
```

**Bonus:** Create a `sort_by_multiple` that can sort by multiple keys (like SQL ORDER BY).

---

## Exercise 9: Lazy Evaluation and Infinite Sequences

**Difficulty:** Hard
**Recommended Language:** Haskell (best), Python (generators)

**Haskell:**

Create these infinite lists using higher-order functions:

```haskell
-- 1. Infinite list of fibonacci numbers
fibs :: [Integer]
fibs = ?

-- Test: take 10 fibs should be [0,1,1,2,3,5,8,13,21,34]

-- 2. Infinite list of prime numbers
primes :: [Integer]
primes = ?

-- Test: take 10 primes should be [2,3,5,7,11,13,17,19,23,29]

-- 3. Use takeWhile and map to get first 5 primes > 20
firstFivePrimesAfter20 :: [Integer]
firstFivePrimesAfter20 = ?
```

**Python (generators):**

```python
def fibonacci():
    """Generator for infinite fibonacci sequence"""
    # Your implementation
    pass

def primes():
    """Generator for infinite prime sequence"""
    # Your implementation
    pass

# Test
fib_gen = fibonacci()
first_10_fibs = [next(fib_gen) for _ in range(10)]
assert first_10_fibs == [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
```

---

## Exercise 10: Function Decorators (Python) / Wrappers (C++)

**Difficulty:** Medium to Hard
**Recommended Languages:** Python, C++

**Python:**

Create these decorators:

**1. `@timer`** - Prints execution time
```python
import time

def timer(func):
    # Your implementation
    pass

@timer
def slow_function():
    time.sleep(1)
    return "done"

# Should print: "slow_function took 1.00 seconds"
```

**2. `@call_counter`** - Counts function calls
```python
def call_counter(func):
    # Your implementation (needs closure)
    pass

@call_counter
def my_func():
    pass

my_func()
my_func()
my_func()
# Should be able to get: my_func.call_count == 3
```

**3. `@retry`** - Retry function on failure
```python
def retry(max_attempts=3):
    def decorator(func):
        # Your implementation
        pass
    return decorator

@retry(max_attempts=5)
def unreliable_function():
    import random
    if random.random() < 0.7:
        raise Exception("Failed!")
    return "Success!"
```

**C++:**

Create a function wrapper that times execution:

```cpp
template<typename Func>
auto makeTimer(Func func) {
    // Return wrapped function that times execution
    // Your implementation
}
```

---

## Exercise 11: Functional Error Handling

**Difficulty:** Hard
**Recommended Languages:** Haskell (Maybe/Either), Python (Option pattern)

**Python:**

Implement a Maybe monad:

```python
class Maybe:
    """Represents a value that might not exist"""

    @staticmethod
    def just(value):
        """Wrap a value"""
        # Your implementation
        pass

    @staticmethod
    def nothing():
        """Represent absence of value"""
        # Your implementation
        pass

    def map(self, func):
        """Apply func if value exists"""
        # Your implementation
        pass

    def flatmap(self, func):
        """Chain operations that return Maybe"""
        # Your implementation
        pass

# Usage:
def safe_divide(a, b):
    if b == 0:
        return Maybe.nothing()
    return Maybe.just(a / b)

result = (Maybe.just(10)
          .flatmap(lambda x: safe_divide(x, 2))
          .map(lambda x: x + 1))

# Should be Maybe.just(6.0)
```

**Haskell:**

Use Maybe to safely chain operations:

```haskell
-- Chain these safely:
-- 1. Parse string to Int
-- 2. Divide by 2
-- 3. Add 10
-- 4. Convert to String

safeParse :: String -> Maybe Int
safeDiv :: Int -> Maybe Int  -- divide by 2, or Nothing if odd
addTen :: Int -> Int

process :: String -> Maybe String
process = ?

-- Test:
-- process "20" should be Just "20"  -- (20 / 2) + 10 = 20
-- process "abc" should be Nothing
-- process "13" should be Nothing (odd number)
```

---

## Exercise 12: Point-Free Style

**Difficulty:** Medium (Conceptual)
**Recommended Language:** Haskell (best suited)

Rewrite these functions in point-free style (no explicit parameters):

```haskell
-- 1. Double all numbers
doubleAll :: [Int] -> [Int]
doubleAll xs = map (*2) xs
-- Rewrite without xs

-- 2. Sum of squares
sumOfSquares :: [Int] -> Int
sumOfSquares xs = sum (map (^2) xs)
-- Rewrite without xs

-- 3. Filter then map
processData :: [Int] -> [Int]
processData xs = map (+1) (filter even xs)
-- Rewrite without xs

-- 4. Composition chain
transform :: Int -> Int
transform x = (*2) ((+1) (abs x))
-- Rewrite without x
```

**Question:** Is point-free always better? When does it hurt readability?

---

## Exercise 13: Building a Query DSL

**Difficulty:** Hard
**Recommended Languages:** Python, Haskell

Build a mini query language for data using higher-order functions:

```python
class Query:
    def __init__(self, data):
        self.data = data

    def filter(self, predicate):
        """Filter elements"""
        # Your implementation
        return self

    def map(self, transform):
        """Transform elements"""
        # Your implementation
        return self

    def reduce(self, func, initial):
        """Reduce to single value"""
        # Your implementation
        pass

    def take(self, n):
        """Take first n elements"""
        # Your implementation
        return self

    def sort_by(self, key_func):
        """Sort by key"""
        # Your implementation
        return self

    def get(self):
        """Execute query and return results"""
        # Your implementation
        pass

# Usage:
result = (Query([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
          .filter(lambda x: x % 2 == 0)
          .map(lambda x: x * x)
          .sort_by(lambda x: -x)
          .take(3)
          .get())

# Should return [100, 64, 36] (squares of [10, 8, 6])
```

---

## Exercise 14: Comparing Paradigms

**Difficulty:** Medium (Conceptual)
**Recommended Languages:** All three

**Problem:** Find the sum of products of pairs where both numbers are even.

Input: `[1, 2, 3, 4, 5, 6]`
Pairs: `[(1,2), (1,3), (1,4), ..., (5,6)]`
Even pairs: `[(2,4), (2,6), (4,6)]`
Products: `[8, 12, 24]`
Sum: `44`

**Implement in three styles:**

**1. Imperative (Python):**
```python
def sum_even_products_imperative(numbers):
    # Use explicit loops
    pass
```

**2. Functional (Python):**
```python
def sum_even_products_functional(numbers):
    # Use only map/filter/reduce
    pass
```

**3. Haskell:**
```haskell
sumEvenProducts :: [Int] -> Int
sumEvenProducts = ?
-- Use list comprehension or functional combinators
```

**Reflection:** Which approach is most readable? Most concise? Easiest to test?

---

## Exercise 15: Advanced Composition

**Difficulty:** Very Hard
**Recommended Language:** Haskell (or Python for challenge)

Implement function composition that works with functions of different arities:

**Haskell:**
```haskell
-- Normal composition works with unary functions
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)

-- Implement composition that can handle:
-- f :: c -> d
-- g :: a -> b -> c
-- composeGen f g :: a -> b -> d

composeGen :: (c -> d) -> (a -> b -> c) -> a -> b -> d
composeGen = ?

-- Test:
add :: Int -> Int -> Int
add x y = x + y

double :: Int -> Int
double x = x * 2

addThenDouble :: Int -> Int -> Int
addThenDouble = composeGen double add

-- addThenDouble 3 5 should be 16 ((3 + 5) * 2)
```

---

## Reflection Questions

After completing these exercises:

1. **Readability:** When does functional style improve readability? When does it hurt?

2. **Performance:** Are there performance differences between imperative and functional approaches? When?

3. **Debugging:** How do you debug composed functions? Is it harder than imperative code?

4. **State:** How do you handle state in functional style? Is it always better to avoid it?

5. **Language Comparison:** Which language made higher-order functions most natural? Why?

6. **Practical Use:** Which patterns will you use in real projects? Which are academic exercises?

---

**Completion Goal:** Complete exercises 1-10 minimum. Advanced students should tackle 11-15.

**Time Estimate:**
- Exercises 1-5: 3-4 hours
- Exercises 6-10: 4-5 hours
- Exercises 11-15: 5-6 hours

Remember: Higher-order functions are about **abstraction** and **composition**. Think in terms of transformations, not loops!
