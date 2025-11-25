# Lab 4: Function Factory

**Quarter 1, Week 4**
**Duration:** 90 minutes
**Format:** Pair programming

## Overview

Functions are the building blocks of organized code. In this lab, you'll learn to write, compose, and reason about functions across all three languages.

## Objectives

By the end of this lab, you will:
- [ ] Write functions with parameters and return values
- [ ] Understand scope and local variables
- [ ] Compose functions together
- [ ] Recognize pure vs impure functions

## Setup

- Partner up
- Create folder: `lab04-functions/`
- Have all three language environments ready

---

## Part 1: Function Basics (20 minutes)

### Activity 1.1: Your First Functions

Write a function that takes a name and returns a greeting.

**Python** (`greet.py`):
```python
def greet(name):
    return f"Hello, {name}!"

# Test it
print(greet("Alice"))
print(greet("Bob"))
```

**C++** (`greet.cpp`):
```cpp
#include <iostream>
#include <string>
using namespace std;

string greet(string name) {
    return "Hello, " + name + "!";
}

int main() {
    cout << greet("Alice") << endl;
    cout << greet("Bob") << endl;
    return 0;
}
```

**Haskell** (`greet.hs`):
```haskell
greet :: String -> String
greet name = "Hello, " ++ name ++ "!"

main :: IO ()
main = do
    putStrLn (greet "Alice")
    putStrLn (greet "Bob")
```

### Activity 1.2: Multiple Parameters

Write a function that calculates the area of a rectangle.

**Python:**
```python
def rectangle_area(width, height):
    return width * height

print(rectangle_area(5, 3))  # Should print 15
```

**Your task:** Implement in C++ and Haskell.

**Haskell note:** Multiple parameters look different!
```haskell
rectangleArea :: Double -> Double -> Double
rectangleArea width height = width * height
```

### Activity 1.3: Return Multiple Values

Some functions need to return multiple things.

**Python** (tuples):
```python
def min_max(numbers):
    return min(numbers), max(numbers)

lowest, highest = min_max([3, 1, 4, 1, 5, 9])
print(f"Min: {lowest}, Max: {highest}")
```

**C++** (pair or struct):
```cpp
#include <iostream>
#include <vector>
#include <algorithm>
#include <utility>
using namespace std;

pair<int, int> minMax(vector<int> numbers) {
    int minVal = *min_element(numbers.begin(), numbers.end());
    int maxVal = *max_element(numbers.begin(), numbers.end());
    return make_pair(minVal, maxVal);
}

int main() {
    vector<int> nums = {3, 1, 4, 1, 5, 9};
    auto [lowest, highest] = minMax(nums);
    cout << "Min: " << lowest << ", Max: " << highest << endl;
    return 0;
}
```

**Haskell** (tuples):
```haskell
minMax :: [Int] -> (Int, Int)
minMax numbers = (minimum numbers, maximum numbers)

main :: IO ()
main = do
    let (lowest, highest) = minMax [3, 1, 4, 1, 5, 9]
    putStrLn $ "Min: " ++ show lowest ++ ", Max: " ++ show highest
```

### ✅ Checkpoint 1

Verify with your partner:
- [ ] Greeting function works in all 3 languages
- [ ] Rectangle area function works in at least 2 languages

---

## Part 2: Scope and Variables (20 minutes)

### Activity 2.1: Local vs Global

Predict what these programs print, then run them.

**Python:**
```python
x = 10  # Global

def modify():
    x = 20  # Local - creates new variable!
    print(f"Inside: x = {x}")

modify()
print(f"Outside: x = {x}")
```

**Prediction:** Inside: ___ Outside: ___

**C++:**
```cpp
#include <iostream>
using namespace std;

int x = 10;  // Global

void modify() {
    int x = 20;  // Local - shadows global
    cout << "Inside: x = " << x << endl;
}

int main() {
    modify();
    cout << "Outside: x = " << x << endl;
    return 0;
}
```

**Prediction:** Inside: ___ Outside: ___

**Haskell:**
```haskell
x :: Int
x = 10  -- Top-level binding

modify :: Int
modify = let x = 20  -- Local binding
         in x

main :: IO ()
main = do
    putStrLn $ "Modify returns: " ++ show modify
    putStrLn $ "x is still: " ++ show x
```

### Activity 2.2: Modifying Global State

**Python** - using `global`:
```python
counter = 0

def increment():
    global counter
    counter += 1
    return counter

print(increment())  # 1
print(increment())  # 2
print(increment())  # 3
```

**Discussion:** Why might this be problematic? What's the alternative?

**Alternative - pure function:**
```python
def increment_pure(counter):
    return counter + 1

count = 0
count = increment_pure(count)  # 1
count = increment_pure(count)  # 2
count = increment_pure(count)  # 3
print(count)
```

### Activity 2.3: Haskell's Purity

In Haskell, functions can't modify global state. Try this:

```haskell
-- This won't work like Python/C++:
-- counter = 0
-- increment = counter += 1  -- ERROR!

-- Instead, we thread state through:
increment :: Int -> Int
increment n = n + 1

main :: IO ()
main = do
    let count0 = 0
    let count1 = increment count0
    let count2 = increment count1
    let count3 = increment count2
    print count3  -- 3
```

**Discussion:** What are the advantages of Haskell's approach?

### ✅ Checkpoint 2

Discuss with your partner:
- [ ] Can explain scope in Python and C++
- [ ] Understand why Haskell doesn't allow mutable global state

---

## Part 3: Function Composition (25 minutes)

### Activity 3.1: Building Pipelines

Create functions that work together.

**Python:**
```python
def double(x):
    return x * 2

def add_one(x):
    return x + 1

def square(x):
    return x * x

# Compose them:
result = square(add_one(double(5)))
print(result)  # square(add_one(10)) = square(11) = 121

# Or more readable:
def process(x):
    x = double(x)
    x = add_one(x)
    x = square(x)
    return x

print(process(5))  # 121
```

**C++:**
```cpp
#include <iostream>
using namespace std;

int doubleNum(int x) { return x * 2; }
int addOne(int x) { return x + 1; }
int square(int x) { return x * x; }

int main() {
    int result = square(addOne(doubleNum(5)));
    cout << result << endl;  // 121
    return 0;
}
```

**Haskell** (function composition with `.`):
```haskell
double :: Int -> Int
double x = x * 2

addOne :: Int -> Int
addOne x = x + 1

square :: Int -> Int
square x = x * x

-- Compose with (.)
process :: Int -> Int
process = square . addOne . double

-- Or use ($) for application
main :: IO ()
main = do
    print $ square $ addOne $ double 5  -- 121
    print $ process 5                    -- 121
```

### Activity 3.2: String Processing Pipeline

Create a pipeline that:
1. Removes leading/trailing whitespace
2. Converts to lowercase
3. Replaces spaces with underscores

**Python:**
```python
def clean_string(s):
    s = s.strip()
    s = s.lower()
    s = s.replace(" ", "_")
    return s

print(clean_string("  Hello World  "))  # "hello_world"
```

**Your task:** Implement in C++ and Haskell.

**Hints:**
- C++: Use `<algorithm>` and `<cctype>`, or write simple loops
- Haskell: `import Data.Char (toLower)`, use `map` and list functions

### Activity 3.3: Build a Calculator

Create a simple calculator using functions.

**Python:**
```python
def add(a, b):
    return a + b

def subtract(a, b):
    return a - b

def multiply(a, b):
    return a * b

def divide(a, b):
    if b == 0:
        return None
    return a / b

def calculate(a, op, b):
    operations = {
        '+': add,
        '-': subtract,
        '*': multiply,
        '/': divide
    }
    if op in operations:
        return operations[op](a, b)
    return None

# Test
print(calculate(10, '+', 5))   # 15
print(calculate(10, '-', 5))   # 5
print(calculate(10, '*', 5))   # 50
print(calculate(10, '/', 5))   # 2.0
print(calculate(10, '/', 0))   # None
```

**Discussion:** Notice how we stored functions in a dictionary. This is "functions as first-class values."

### ✅ Checkpoint 3

Verify:
- [ ] Can compose functions in at least 2 languages
- [ ] Calculator works in Python

---

## Part 4: Pure vs Impure Functions (15 minutes)

### Activity 4.1: Identify Pure Functions

A **pure function**:
- Always returns the same output for the same input
- Has no side effects (doesn't modify external state, print, etc.)

**Classify these Python functions:**

```python
import random

counter = 0

def add(a, b):
    return a + b

def increment_counter():
    global counter
    counter += 1
    return counter

def get_random():
    return random.randint(1, 100)

def greet(name):
    print(f"Hello, {name}!")

def greet_pure(name):
    return f"Hello, {name}!"

def square(x):
    return x * x

def get_length(lst):
    return len(lst)
```

Fill in the table:

| Function | Pure? | Why? |
|----------|-------|------|
| `add` | | |
| `increment_counter` | | |
| `get_random` | | |
| `greet` | | |
| `greet_pure` | | |
| `square` | | |
| `get_length` | | |

### Activity 4.2: Benefits of Pure Functions

**Test this:**
```python
# Pure function - easy to test
def add(a, b):
    return a + b

assert add(2, 3) == 5
assert add(-1, 1) == 0
assert add(0, 0) == 0
print("All tests passed!")

# Pure functions can be called in any order
x = add(1, 2)
y = add(3, 4)
# We can swap the order and get same results!

# Pure functions can be cached (memoized)
from functools import lru_cache

@lru_cache
def slow_pure(n):
    # Imagine this takes a long time
    result = 0
    for i in range(n):
        result += i
    return result

print(slow_pure(10000))  # Slow first time
print(slow_pure(10000))  # Fast! Cached result
```

### Activity 4.3: Haskell's Approach

In Haskell, all functions are pure by default. IO actions are clearly marked:

```haskell
-- Pure function - no IO in type
add :: Int -> Int -> Int
add a b = a + b

-- Impure (does IO) - IO in type
greet :: String -> IO ()
greet name = putStrLn ("Hello, " ++ name ++ "!")

-- The type tells you everything!
```

**Discussion:** Why might forcing purity be valuable?

### ✅ Checkpoint 4

Discuss:
- [ ] Can identify pure vs impure functions
- [ ] Understand at least one benefit of pure functions

---

## Part 5: Challenge - Build a Math Library (10 minutes if time)

Create a mini math library with these functions:
- `factorial(n)` - n!
- `is_prime(n)` - check if prime
- `fibonacci(n)` - nth fibonacci number
- `gcd(a, b)` - greatest common divisor

Implement in at least 2 languages.

**Python starter:**
```python
def factorial(n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)

def is_prime(n):
    if n < 2:
        return False
    for i in range(2, int(n**0.5) + 1):
        if n % i == 0:
            return False
    return True

def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n-1) + fibonacci(n-2)

def gcd(a, b):
    while b:
        a, b = b, a % b
    return a

# Test
print(factorial(5))     # 120
print(is_prime(17))     # True
print(fibonacci(10))    # 55
print(gcd(48, 18))      # 6
```

---

## Extensions

### Extension 1: Default Parameters

```python
def greet(name, greeting="Hello"):
    return f"{greeting}, {name}!"

print(greet("Alice"))              # Hello, Alice!
print(greet("Bob", "Howdy"))       # Howdy, Bob!
```

How do you do this in C++ and Haskell?

### Extension 2: Variable Arguments

```python
def sum_all(*numbers):
    return sum(numbers)

print(sum_all(1, 2, 3))        # 6
print(sum_all(1, 2, 3, 4, 5))  # 15
```

### Extension 3: Lambda Functions

```python
# Named function
def square(x):
    return x * x

# Lambda (anonymous function)
square_lambda = lambda x: x * x

# Both work the same
print(square(5))        # 25
print(square_lambda(5)) # 25
```

Try lambdas in C++ (`[]` syntax) and Haskell (`\x -> ...` syntax).

---

## Wrap-Up

**Key takeaways:**

1. **Functions** encapsulate reusable logic
2. **Scope** determines where variables are visible
3. **Pure functions** have no side effects and are easier to test
4. **Composition** lets you build complex behavior from simple parts
5. **Haskell** encourages purity; Python and C++ allow both styles

**Next lab:** We'll work with data structures - lists, dictionaries, and more!
