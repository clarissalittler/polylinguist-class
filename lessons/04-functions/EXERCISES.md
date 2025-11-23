# Lesson 4: Functions - Exercises

## Instructions

Complete these exercises to practice functions, closures, and functional programming concepts across different languages. For each exercise, try implementing in at least 2-3 languages from different paradigms.

---

## Exercise 1: Temperature Converter (Warmup)

**Difficulty:** Easy

Write two functions:
- `celsius_to_fahrenheit(c)` - converts Celsius to Fahrenheit
- `fahrenheit_to_celsius(f)` - converts Fahrenheit to Celsius

**Formulas:**
- F = C × 9/5 + 32
- C = (F - 32) × 5/9

**Test cases:**
- 0°C = 32°F
- 100°C = 212°F
- 32°F = 0°C
- 98.6°F = 37°C

**Languages to try:** Python, JavaScript, C, or any language

---

## Exercise 2: Simple Calculator

**Difficulty:** Easy

Write calculator functions:
- `add(x, y)`
- `subtract(x, y)`
- `multiply(x, y)`
- `divide(x, y)` - handle division by zero appropriately

**Bonus:** Create a higher-order function `calculate(operation, x, y)` that takes an operation function as a parameter.

**Languages:** Try at least one statically-typed and one dynamically-typed language

---

## Exercise 3: String Utilities

**Difficulty:** Easy to Medium

Write the following string manipulation functions:

1. `reverse_string(s)` - reverses a string
2. `is_palindrome(s)` - checks if a string reads the same forwards and backwards
3. `count_vowels(s)` - counts the number of vowels in a string

**Test cases:**
- `reverse_string("hello")` → "olleh"
- `is_palindrome("racecar")` → true
- `is_palindrome("hello")` → false
- `count_vowels("hello world")` → 3

**Languages:** Try in both imperative and functional languages

---

## Exercise 4: Apply Function N Times

**Difficulty:** Medium

Write a function `apply_n_times(f, x, n)` that applies function `f` to value `x` exactly `n` times.

**Example:**
```python
double = lambda x: x * 2
apply_n_times(double, 5, 3)  # 5 → 10 → 20 → 40
```

**Test cases:**
- `apply_n_times(double, 5, 0)` → 5
- `apply_n_times(double, 5, 1)` → 10
- `apply_n_times(double, 5, 3)` → 40
- `apply_n_times(increment, 10, 5)` → 15

**Implement using:**
- A loop (Python, JavaScript, C)
- Recursion (Haskell, Racket)

---

## Exercise 5: Counter Factory (Closures)

**Difficulty:** Medium

Write a function `make_counter()` that returns a counter function. Each call to the counter increments and returns the count.

**Example:**
```python
counter1 = make_counter()
counter2 = make_counter()
print(counter1())  # 1
print(counter1())  # 2
print(counter2())  # 1  # Independent counter!
print(counter1())  # 3
```

**Requirements:**
- Each counter should be independent
- Count should start at 0
- Use closures to maintain state

**Languages:** Python, JavaScript, Haskell, Ruby

**Discussion:** How does Haskell handle stateful closures (hint: it doesn't, by default)?

---

## Exercise 6: Function Composition

**Difficulty:** Medium

Write a `compose(f, g)` function that returns the composition of two functions.

The composition `compose(f, g)` should return a new function that, when called with `x`, returns `f(g(x))`.

**Example:**
```python
square = lambda x: x * x
increment = lambda x: x + 1

square_then_increment = compose(increment, square)
square_then_increment(5)  # square(5) = 25, increment(25) = 26
```

**Test cases:**
- `compose(increment, square)(5)` → 26
- `compose(square, increment)(5)` → 36
- `compose(double, double)(5)` → 20

**Bonus:** Write a `compose_many` function that composes any number of functions.

**Languages:** Try in Python, JavaScript, and Haskell (Haskell has built-in `.` operator!)

---

## Exercise 7: Currying

**Difficulty:** Medium

### Part A: Manual Currying

Write a curried version of `add` that works like this:
```python
add(3)(5)  # Returns 8
```

### Part B: Generic Curry Function

Write a `curry` function that converts a two-parameter function into a curried function:
```python
def multiply(x, y):
    return x * y

curried_multiply = curry(multiply)
times_five = curried_multiply(5)
times_five(10)  # Returns 50
```

**Languages:** JavaScript, Python, Haskell (automatic!)

**Discussion:** How does Haskell's automatic currying compare to manual currying?

---

## Exercise 8: Filter and Map

**Difficulty:** Medium

### Part A: Implement Your Own

Implement your own versions of `map` and `filter` functions:

```python
def my_map(func, lst):
    # Apply func to each element of lst
    pass

def my_filter(pred, lst):
    # Keep only elements where pred returns true
    pass
```

**Test cases:**
```python
my_map(square, [1, 2, 3, 4])  # [1, 4, 9, 16]
my_filter(is_even, [1, 2, 3, 4, 5])  # [2, 4]
```

### Part B: Implement Reduce

Implement your own `reduce` (fold) function:
```python
def my_reduce(func, lst, initial):
    # Combine elements using func, starting with initial
    pass
```

**Test cases:**
```python
my_reduce(add, [1, 2, 3, 4], 0)  # 10
my_reduce(multiply, [1, 2, 3, 4], 1)  # 24
```

**Languages:** Implement in at least 2 languages

**Bonus:** Make reduce work without an initial value

---

## Exercise 9: Function Pipeline

**Difficulty:** Medium to Hard

Create a pipeline function that chains multiple functions together:

```python
pipeline = compose_many([
    lambda x: x + 1,
    lambda x: x * 2,
    lambda x: x - 3
])

pipeline(5)  # (((5 + 1) * 2) - 3) = 9
```

**Alternative syntax (more readable):**
```python
result = pipe(
    5,
    lambda x: x + 1,  # 6
    lambda x: x * 2,  # 12
    lambda x: x - 3   # 9
)
```

**Bonus:** Support both left-to-right (`pipe`) and right-to-left (`compose`) composition.

---

## Exercise 10: Memoization

**Difficulty:** Hard

Write a `memoize` function that caches the results of expensive function calls.

```python
def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)

# Without memoization: very slow for large n
fibonacci(35)  # Takes several seconds

# With memoization: much faster
fibonacci_memo = memoize(fibonacci)
fibonacci_memo(35)  # Fast!
fibonacci_memo(35)  # Even faster (cached)!
```

**Requirements:**
- Cache should be specific to the memoized function
- Support functions with one parameter
- Bonus: Support multiple parameters

**Languages:** Python, JavaScript

**Discussion:** Why does memoization require purity? What happens if you memoize an impure function?

---

## Exercise 11: Partial Application

**Difficulty:** Medium to Hard

Write a `partial` function that partially applies arguments to a function:

```python
def greet(greeting, name):
    return f"{greeting}, {name}!"

say_hello = partial(greet, "Hello")
say_goodbye = partial(greet, "Goodbye")

say_hello("Alice")    # "Hello, Alice!"
say_goodbye("Bob")    # "Goodbye, Bob!"
```

**Test cases:**
```python
add_five = partial(add, 5)
add_five(10)  # 15

multiply_by_two = partial(multiply, 2)
multiply_by_two(7)  # 14
```

**Bonus:** Support partial application from the right (last arguments fixed)

---

## Exercise 12: Pure vs Impure

**Difficulty:** Medium (Conceptual)

For each of the following functions, determine if it's pure or impure and explain why:

1. ```python
   def add(x, y):
       return x + y
   ```

2. ```python
   def print_sum(x, y):
       result = x + y
       print(result)
       return result
   ```

3. ```python
   counter = 0
   def increment():
       global counter
       counter += 1
       return counter
   ```

4. ```python
   def get_length(lst):
       return len(lst)
   ```

5. ```python
   def append_to_list(lst, item):
       lst.append(item)
       return lst
   ```

6. ```python
   def get_current_time():
       import time
       return time.time()
   ```

7. ```python
   import random
   def roll_dice():
       return random.randint(1, 6)
   ```

**Task:** For each impure function, rewrite it as a pure function.

---

## Exercise 13: Factorial with Comparison

**Difficulty:** Medium

Implement factorial in multiple styles:

1. **Iterative:** Using a loop
2. **Recursive:** Using recursion
3. **Functional:** Using reduce/fold

**Example implementations:**
```python
# Iterative
def factorial_iterative(n):
    result = 1
    for i in range(1, n + 1):
        result *= i
    return result

# Recursive
def factorial_recursive(n):
    if n <= 1:
        return 1
    return n * factorial_recursive(n - 1)

# Functional
from functools import reduce
def factorial_functional(n):
    return reduce(lambda acc, x: acc * x, range(1, n + 1), 1)
```

**Test all three with:**
- factorial(0) → 1
- factorial(5) → 120
- factorial(10) → 3628800

**Discussion:** Which implementation is clearest? Most efficient? Most elegant?

---

## Exercise 14: Higher-Order Calculator

**Difficulty:** Hard

Build a calculator that uses higher-order functions:

```python
def calculator(operation_name):
    operations = {
        'add': lambda x, y: x + y,
        'subtract': lambda x, y: x - y,
        'multiply': lambda x, y: x * y,
        'divide': lambda x, y: x / y if y != 0 else None
    }
    return operations.get(operation_name)

# Usage
add_func = calculator('add')
add_func(5, 3)  # 8

multiply_func = calculator('multiply')
multiply_func(4, 7)  # 28
```

**Extensions:**

1. Add more operations (power, modulo, max, min)
2. Support chaining: `calculator('add')(5)(3)`
3. Add a `calculate` function that takes operation name and arguments:
   ```python
   calculate('add', 5, 3)  # 8
   ```

---

## Exercise 15: Function Decorators (Python/JavaScript)

**Difficulty:** Hard

**Python:**
Write a decorator that logs function calls:

```python
def log_calls(func):
    def wrapper(*args, **kwargs):
        print(f"Calling {func.__name__} with {args}, {kwargs}")
        result = func(*args, **kwargs)
        print(f"{func.__name__} returned {result}")
        return result
    return wrapper

@log_calls
def add(x, y):
    return x + y

add(5, 3)
# Output:
# Calling add with (5, 3), {}
# add returned 8
```

**Extensions:**
1. Write a `@time_it` decorator that measures execution time
2. Write a `@retry(n)` decorator that retries a function n times on failure
3. Write a `@cache` decorator that memoizes results

---

## Challenge Projects

### Challenge 1: Build Your Own Reduce

Implement `reduce` from scratch without using any built-in reduce/fold functions. It should work for any binary function.

```python
reduce(add, [1, 2, 3, 4], 0)  # ((((0 + 1) + 2) + 3) + 4) = 10
reduce(multiply, [2, 3, 4], 1)  # (((1 * 2) * 3) * 4) = 24
```

### Challenge 2: Point-Free Style

Rewrite the following function in point-free style (without explicit parameters):

```python
# With explicit parameter
def sum_of_squares_of_evens(numbers):
    return sum(map(lambda x: x * x, filter(lambda x: x % 2 == 0, numbers)))

# Point-free style (Haskell)
sumOfSquaresOfEvens = sum . map (^2) . filter even
```

Try this in Haskell, then attempt a version in Python or JavaScript.

### Challenge 3: Y Combinator

Research and implement the Y combinator, which allows recursion without explicit self-reference:

```python
Y = lambda f: (lambda x: f(lambda v: x(x)(v)))(lambda x: f(lambda v: x(x)(v)))

factorial = Y(lambda f: lambda n: 1 if n == 0 else n * f(n - 1))
factorial(5)  # 120
```

**Warning:** This is very advanced! Understand how it works conceptually before implementing.

---

## Reflection Questions

After completing the exercises, consider:

1. **How do closures enable encapsulation without classes?**

2. **What are the advantages of pure functions for testing and reasoning?**

3. **When is currying useful in practice?**

4. **How does Haskell's automatic currying compare to manual currying in Python/JavaScript?**

5. **What's the relationship between map/filter/reduce and for loops?**

6. **Why is function composition powerful? When would you use it?**

7. **How do different languages handle closures differently?**

---

## Solutions

Solutions for all exercises will be provided in a separate directory. Try to complete exercises on your own first!

Remember: The goal is not just to make it work, but to understand how functions enable abstraction and composition across different programming paradigms.
