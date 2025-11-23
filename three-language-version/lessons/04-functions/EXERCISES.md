# Lesson 4: Functions - Exercises

## Instructions

Complete these exercises to practice functions, closures, and functional programming concepts across Python, C++, and Haskell. For each exercise, try implementing in at least 2-3 languages to appreciate the different approaches.

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

**Implement in all three languages.**

---

## Exercise 2: Simple Calculator

**Difficulty:** Easy

Write calculator functions:
- `add(x, y)`
- `subtract(x, y)`
- `multiply(x, y)`
- `divide(x, y)` - handle division by zero appropriately

**Python example:**
```python
def divide(x, y):
    if y == 0:
        return None  # or raise an exception
    return x / y
```

**C++ example:**
```cpp
#include <optional>

std::optional<double> divide(double x, double y) {
    if (y == 0) {
        return std::nullopt;
    }
    return x / y;
}
```

**Haskell example:**
```haskell
divide :: Double -> Double -> Maybe Double
divide x 0 = Nothing
divide x y = Just (x / y)
```

**Bonus:** Create a higher-order function `calculate(operation, x, y)` that takes an operation function as a parameter.

---

## Exercise 3: String Utilities

**Difficulty:** Easy to Medium

Write the following string manipulation functions:

1. `reverse_string(s)` - reverses a string
2. `is_palindrome(s)` - checks if a string reads the same forwards and backwards (ignore case and spaces)
3. `count_vowels(s)` - counts the number of vowels in a string

**Test cases:**
- `reverse_string("hello")` → "olleh"
- `is_palindrome("racecar")` → true
- `is_palindrome("A man a plan a canal Panama")` → true (ignoring spaces and case)
- `is_palindrome("hello")` → false
- `count_vowels("hello world")` → 3

**Python hints:**
```python
def reverse_string(s):
    return s[::-1]  # Or use reversed()

def is_palindrome(s):
    cleaned = s.lower().replace(" ", "")
    return cleaned == cleaned[::-1]
```

**C++ hints:**
```cpp
#include <algorithm>
#include <cctype>

std::string reverseString(const std::string& s) {
    return std::string(s.rbegin(), s.rend());
}
```

**Haskell hints:**
```haskell
reverseString :: String -> String
reverseString = reverse

isPalindrome :: String -> Bool
isPalindrome s = cleaned == reverse cleaned
  where cleaned = map toLower (filter (/= ' ') s)
```

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
- A loop (Python, C++)
- Recursion (Haskell)

**Python:**
```python
def apply_n_times(f, x, n):
    result = x
    for _ in range(n):
        result = f(result)
    return result
```

**C++:**
```cpp
template<typename T, typename Func>
T applyNTimes(Func f, T x, int n) {
    T result = x;
    for (int i = 0; i < n; i++) {
        result = f(result);
    }
    return result;
}
```

**Haskell:**
```haskell
applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes 0 f x = x
applyNTimes n f x = applyNTimes (n - 1) f (f x)
```

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

**Python:**
```python
def make_counter():
    count = 0

    def increment():
        nonlocal count
        count += 1
        return count

    return increment
```

**C++:**
```cpp
#include <functional>

std::function<int()> makeCounter() {
    // Capture count by reference (needs shared state)
    auto count = std::make_shared<int>(0);
    return [count]() {
        (*count)++;
        return *count;
    };
}
```

**Haskell (using State monad or IO):**
```haskell
-- Haskell doesn't have mutable state by default
-- Option 1: Using IO and IORef
import Data.IORef

makeCounter :: IO (IO Int)
makeCounter = do
    countRef <- newIORef 0
    return $ do
        modifyIORef' countRef (+1)
        readIORef countRef

-- Option 2: Return a list of infinite increments
makeCounterPure :: [Int]
makeCounterPure = [1..]
```

**Discussion:** How does Haskell handle stateful closures differently from Python and C++?

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

**Python:**
```python
def compose(f, g):
    return lambda x: f(g(x))
```

**C++:**
```cpp
template<typename F, typename G>
auto compose(F f, G g) {
    return [f, g](auto x) { return f(g(x)); };
}
```

**Haskell (built-in with . operator):**
```haskell
-- Already built-in as (.)
-- But here's how to implement it:
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \x -> f (g x)

-- Or point-free:
compose f g = f . g

-- Using built-in:
squareThenIncrement = increment . square
```

**Bonus:** Write a `compose_many` function that composes any number of functions.

**Python:**
```python
from functools import reduce

def compose_many(*funcs):
    return reduce(compose, funcs)
```

---

## Exercise 7: Currying

**Difficulty:** Medium

### Part A: Manual Currying

Write a curried version of `add` that works like this:
```python
add(3)(5)  # Returns 8
```

**Python:**
```python
def add(x):
    def inner(y):
        return x + y
    return inner
```

**C++:**
```cpp
auto add(int x) {
    return [x](int y) { return x + y; };
}
```

**Haskell (automatic!):**
```haskell
-- All functions are automatically curried!
add :: Int -> Int -> Int
add x y = x + y

-- Can call as: add 3 5
-- Or partially: let addThree = add 3
--               addThree 5  -- Returns 8
```

### Part B: Generic Curry Function

Write a `curry` function that converts a two-parameter function into a curried function:

**Python:**
```python
def curry(f):
    def curried(x):
        def inner(y):
            return f(x, y)
        return inner
    return curried

def multiply(x, y):
    return x * y

curried_multiply = curry(multiply)
times_five = curried_multiply(5)
times_five(10)  # Returns 50
```

**Discussion:** How does Haskell's automatic currying compare to manual currying?

---

## Exercise 8: Filter and Map Implementation

**Difficulty:** Medium

### Part A: Implement Your Own

Implement your own versions of `map` and `filter` functions:

**Python:**
```python
def my_map(func, lst):
    result = []
    for item in lst:
        result.append(func(item))
    return result

def my_filter(pred, lst):
    result = []
    for item in lst:
        if pred(item):
            result.append(item)
    return result
```

**C++:**
```cpp
template<typename T, typename Func>
std::vector<T> myMap(Func f, const std::vector<T>& vec) {
    std::vector<T> result;
    for (const auto& item : vec) {
        result.push_back(f(item));
    }
    return result;
}

template<typename T, typename Pred>
std::vector<T> myFilter(Pred pred, const std::vector<T>& vec) {
    std::vector<T> result;
    for (const auto& item : vec) {
        if (pred(item)) {
            result.push_back(item);
        }
    }
    return result;
}
```

**Haskell:**
```haskell
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter pred (x:xs)
    | pred x    = x : myFilter pred xs
    | otherwise = myFilter pred xs
```

**Test cases:**
```python
my_map(lambda x: x * x, [1, 2, 3, 4])  # [1, 4, 9, 16]
my_filter(lambda x: x % 2 == 0, [1, 2, 3, 4, 5])  # [2, 4]
```

### Part B: Implement Reduce

**Python:**
```python
def my_reduce(func, lst, initial):
    accumulator = initial
    for item in lst:
        accumulator = func(accumulator, item)
    return accumulator
```

**C++:**
```cpp
template<typename T, typename Func>
T myReduce(Func f, const std::vector<T>& vec, T initial) {
    T accumulator = initial;
    for (const auto& item : vec) {
        accumulator = f(accumulator, item);
    }
    return accumulator;
}
```

**Haskell:**
```haskell
myFold :: (b -> a -> b) -> b -> [a] -> b
myFold _ acc [] = acc
myFold f acc (x:xs) = myFold f (f acc x) xs
```

**Test cases:**
```python
my_reduce(lambda acc, x: acc + x, [1, 2, 3, 4], 0)  # 10
my_reduce(lambda acc, x: acc * x, [1, 2, 3, 4], 1)  # 24
```

---

## Exercise 9: Identifying Pure vs Impure

**Difficulty:** Easy

For each function, determine if it's pure or impure and explain why:

1. `def double(x): return x * 2`
2. `def print_double(x): print(x * 2)`
3. `def append_to_list(lst, x): lst.append(x); return lst`
4. `def get_length(lst): return len(lst)`
5. `def get_current_time(): return time.time()`
6. `def sqrt(x): return x ** 0.5`
7. `def read_file(path): return open(path).read()`

**Answers:**
1. **Pure** - Deterministic, no side effects
2. **Impure** - Side effect (I/O)
3. **Impure** - Modifies input (mutation)
4. **Pure** - Deterministic, doesn't modify input
5. **Impure** - Non-deterministic (depends on when called)
6. **Pure** - Deterministic, no side effects
7. **Impure** - Side effect (I/O), non-deterministic (file can change)

**Discussion:**
- Why is purity important for testing?
- Can impure functions be made pure? How?
- What are the trade-offs?

---

## Exercise 10: Partial Application

**Difficulty:** Medium

Write a `partial` function that partially applies arguments to a function:

**Python:**
```python
def partial(f, *fixed_args):
    def wrapper(*args):
        return f(*fixed_args, *args)
    return wrapper

def greet(greeting, name):
    return f"{greeting}, {name}!"

say_hello = partial(greet, "Hello")
say_goodbye = partial(greet, "Goodbye")

say_hello("Alice")    # "Hello, Alice!"
say_goodbye("Bob")    # "Goodbye, Bob!"
```

**C++:**
```cpp
#include <functional>

// std::bind provides partial application
auto greet = [](const std::string& greeting, const std::string& name) {
    return greeting + ", " + name + "!";
};

auto sayHello = std::bind(greet, "Hello", std::placeholders::_1);
auto sayGoodbye = std::bind(greet, "Goodbye", std::placeholders::_1);

sayHello("Alice");    // "Hello, Alice!"
sayGoodbye("Bob");    // "Goodbye, Bob!"
```

**Haskell (automatic partial application):**
```haskell
greet :: String -> String -> String
greet greeting name = greeting ++ ", " ++ name ++ "!"

-- Partial application is automatic!
sayHello = greet "Hello"
sayGoodbye = greet "Goodbye"

-- sayHello "Alice"  -- "Hello, Alice!"
```

---

## Exercise 11: Factorial Comparison

**Difficulty:** Easy to Medium

Implement factorial in all three languages using different approaches:

**Python - Iterative:**
```python
def factorial_iterative(n):
    result = 1
    for i in range(1, n + 1):
        result *= i
    return result
```

**Python - Recursive:**
```python
def factorial_recursive(n):
    if n <= 1:
        return 1
    return n * factorial_recursive(n - 1)
```

**C++ - Iterative:**
```cpp
long long factorial(int n) {
    long long result = 1;
    for (int i = 1; i <= n; i++) {
        result *= i;
    }
    return result;
}
```

**Haskell - Recursive:**
```haskell
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

**Haskell - Using fold:**
```haskell
factorial :: Int -> Int
factorial n = product [1..n]
```

**Discussion:**
- Which implementation is clearest?
- Which is most efficient?
- How does tail recursion optimization affect Haskell?

---

## Challenge Projects

### Challenge 1: Memoization

Write a `memoize` function that caches the results of expensive function calls.

**Python:**
```python
def memoize(f):
    cache = {}

    def memoized(n):
        if n not in cache:
            cache[n] = f(n)
        return cache[n]

    return memoized

def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)

fibonacci_memo = memoize(fibonacci)
fibonacci_memo(100)  # Fast!
```

**Discussion:** Why does memoization require purity?

### Challenge 2: Function Pipeline

Create a pipeline function that chains multiple functions:

**Python:**
```python
def pipe(value, *functions):
    result = value
    for f in functions:
        result = f(result)
    return result

result = pipe(
    5,
    lambda x: x + 1,  # 6
    lambda x: x * 2,  # 12
    lambda x: x - 3   # 9
)
```

### Challenge 3: Type-Safe Calculator (C++)

Implement a calculator using std::function and std::variant for type safety.

---

## Reflection Questions

1. **How do closures in Python compare to C++ lambdas?**

2. **Why does Haskell enforce purity while Python doesn't?**

3. **When would you prefer function composition over just calling functions sequentially?**

4. **How does automatic currying (Haskell) change how you think about functions?**

5. **What are the trade-offs between mutable and immutable approaches to stateful functions?**

---

## Going Further

- **Python:** Explore decorators, functools, and itertools
- **C++:** Study std::function, function templates, and perfect forwarding
- **Haskell:** Learn about monads, functors, and applicatives

---

Remember: The goal is not just to make it work, but to understand how each language approaches functions differently!
