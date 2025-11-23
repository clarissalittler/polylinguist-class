# Lesson 8: Higher-Order Functions

## Overview

Higher-order functions are functions that can:
1. **Take other functions as arguments**, or
2. **Return functions as results**, or
3. **Both!**

This powerful concept enables:
- Code reuse and abstraction
- Function composition
- Declarative programming style
- Elegant solutions to complex problems
- Building domain-specific capabilities

This lesson explores how Python, C++, and Haskell support higher-order functions, from Haskell's native functional approach to C++'s modern templates and lambdas, to Python's dynamic flexibility.

## Learning Objectives

By the end of this lesson, you will:
- Understand what makes a function "higher-order"
- Use built-in higher-order functions (map, filter, reduce/fold)
- Create your own higher-order functions
- Understand closures and lexical scope
- Use partial application and currying
- Compose functions for elegant solutions
- Compare imperative vs functional approaches
- Understand function objects and functors in C++

## What is a Higher-Order Function?

A **higher-order function** treats functions as first-class values - they can be passed around like any other data.

**Python Example:**
```python
# Takes a function as argument
def apply_twice(func, x):
    return func(func(x))

def add_one(x):
    return x + 1

result = apply_twice(add_one, 5)  # 7
# Explanation: add_one(add_one(5)) = add_one(6) = 7
```

**C++ Example:**
```cpp
// Takes a function as argument
template<typename Func, typename T>
T applyTwice(Func func, T x) {
    return func(func(x));
}

int addOne(int x) {
    return x + 1;
}

int result = applyTwice(addOne, 5);  // 7

// Or with lambda
auto result2 = applyTwice([](int x) { return x + 1; }, 5);
```

**Haskell Example:**
```haskell
-- Takes a function as argument
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

addOne :: Int -> Int
addOne x = x + 1

result = applyTwice addOne 5  -- 7

-- More concise
result' = applyTwice (+1) 5
```

## First-Class Functions

For higher-order functions to work, functions must be **first-class citizens**:

**Python:**
```python
# Assign to variables
f = lambda x: x * 2
square = lambda x: x ** 2

# Pass as arguments
numbers = [1, 2, 3, 4, 5]
doubled = list(map(f, numbers))

# Return from functions
def make_multiplier(n):
    return lambda x: x * n

times_three = make_multiplier(3)

# Store in data structures
operations = [f, square, times_three]
for op in operations:
    print(op(5))  # 10, 25, 15
```

**C++:**
```cpp
#include <functional>
#include <vector>

// Assign to variables (function pointers or std::function)
auto f = [](int x) { return x * 2; };
std::function<int(int)> square = [](int x) { return x * x; };

// Pass as arguments (templates or std::function)
std::vector<int> numbers = {1, 2, 3, 4, 5};
std::vector<int> doubled;
std::transform(numbers.begin(), numbers.end(),
               std::back_inserter(doubled), f);

// Return from functions
auto makeMultiplier(int n) {
    return [n](int x) { return x * n; };
}

auto timesThree = makeMultiplier(3);

// Store in data structures
std::vector<std::function<int(int)>> operations = {f, square, timesThree};
for (auto& op : operations) {
    std::cout << op(5) << std::endl;  // 10, 25, 15
}
```

**Haskell:**
```haskell
-- Assign to variables (natural)
f = \x -> x * 2
square = \x -> x * x

-- Pass as arguments (natural)
numbers = [1, 2, 3, 4, 5]
doubled = map f numbers

-- Return from functions (natural)
makeMultiplier :: Int -> (Int -> Int)
makeMultiplier n = \x -> x * n

timesThree = makeMultiplier 3

-- Store in data structures
operations = [f, square, timesThree]
results = map (\op -> op 5) operations  -- [10, 25, 15]
```

## The Classic Trio: Map, Filter, Reduce

### Map - Transform Each Element

**Concept:** Apply a function to every element in a collection.

**Python:**
```python
numbers = [1, 2, 3, 4, 5]

# Imperative approach
doubled_imp = []
for n in numbers:
    doubled_imp.append(n * 2)

# Functional with map
doubled_func = list(map(lambda x: x * 2, numbers))

# List comprehension (Pythonic)
doubled_comp = [x * 2 for x in numbers]

# All produce: [2, 4, 6, 8, 10]
```

**C++:**
```cpp
std::vector<int> numbers = {1, 2, 3, 4, 5};

// Imperative approach
std::vector<int> doubledImp;
for (int n : numbers) {
    doubledImp.push_back(n * 2);
}

// Functional with std::transform
std::vector<int> doubledFunc;
std::transform(numbers.begin(), numbers.end(),
               std::back_inserter(doubledFunc),
               [](int x) { return x * 2; });

// Ranges (C++20)
auto doubledRange = numbers | std::views::transform([](int x) { return x * 2; });
```

**Haskell:**
```haskell
numbers = [1, 2, 3, 4, 5]

-- Built-in map function
doubled = map (*2) numbers

-- List comprehension
doubled' = [x * 2 | x <- numbers]

-- Both produce: [2, 4, 6, 8, 10]

-- Custom map implementation
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs
```

**Visualization:**
```
[1, 2, 3, 4, 5]
 ↓  ↓  ↓  ↓  ↓   (apply *2 to each)
[2, 4, 6, 8, 10]
```

### Filter - Select Elements

**Concept:** Keep only elements that satisfy a predicate (boolean function).

**Python:**
```python
numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

# Imperative
evens_imp = []
for n in numbers:
    if n % 2 == 0:
        evens_imp.append(n)

# Functional with filter
evens_func = list(filter(lambda x: x % 2 == 0, numbers))

# List comprehension
evens_comp = [x for x in numbers if x % 2 == 0]

# All produce: [2, 4, 6, 8, 10]
```

**C++:**
```cpp
std::vector<int> numbers = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

// Imperative
std::vector<int> evensImp;
for (int n : numbers) {
    if (n % 2 == 0) {
        evensImp.push_back(n);
    }
}

// Functional with std::copy_if
std::vector<int> evensFunc;
std::copy_if(numbers.begin(), numbers.end(),
             std::back_inserter(evensFunc),
             [](int x) { return x % 2 == 0; });

// Ranges (C++20)
auto evensRange = numbers | std::views::filter([](int x) { return x % 2 == 0; });
```

**Haskell:**
```haskell
numbers = [1..10]

-- Built-in filter
evens = filter even numbers

-- List comprehension
evens' = [x | x <- numbers, even x]

-- Both produce: [2, 4, 6, 8, 10]

-- Custom filter implementation
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter pred (x:xs)
    | pred x    = x : myFilter pred xs
    | otherwise = myFilter pred xs
```

**Visualization:**
```
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
 ✗  ✓  ✗  ✓  ✗  ✓  ✗  ✓  ✗  ✓   (keep only evens)
[   2,    4,    6,    8,    10]
```

### Reduce/Fold - Combine Elements

**Concept:** Combine all elements into a single value using a binary function.

**Python:**
```python
from functools import reduce

numbers = [1, 2, 3, 4, 5]

# Imperative
sum_imp = 0
for n in numbers:
    sum_imp += n

# Functional with reduce
sum_func = reduce(lambda acc, x: acc + x, numbers, 0)

# Built-in (preferred for common operations)
sum_builtin = sum(numbers)

# All produce: 15

# Product example
product = reduce(lambda acc, x: acc * x, numbers, 1)  # 120
```

**C++:**
```cpp
std::vector<int> numbers = {1, 2, 3, 4, 5};

// Imperative
int sumImp = 0;
for (int n : numbers) {
    sumImp += n;
}

// Functional with std::accumulate
int sumFunc = std::accumulate(numbers.begin(), numbers.end(), 0,
                               [](int acc, int x) { return acc + x; });

// Simplified (uses default +)
int sumSimple = std::accumulate(numbers.begin(), numbers.end(), 0);

// All produce: 15

// Product example
int product = std::accumulate(numbers.begin(), numbers.end(), 1,
                              [](int acc, int x) { return acc * x; });  // 120
```

**Haskell:**
```haskell
numbers = [1, 2, 3, 4, 5]

-- Left fold (foldl) - processes from left to right
sumLeft = foldl (+) 0 numbers  -- 15

-- Right fold (foldr) - processes from right to left
sumRight = foldr (+) 0 numbers  -- 15

-- For (+), direction doesn't matter, but for (-) it does!
diffLeft = foldl (-) 0 numbers   -- -15: ((((0-1)-2)-3)-4)-5
diffRight = foldr (-) 0 numbers  -- 3:   1-(2-(3-(4-(5-0))))

-- Product
product' = foldl (*) 1 numbers  -- 120

-- Custom fold implementation
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs
```

**Visualization (sum):**
```
[1, 2, 3, 4, 5]
 ↓
 1 + 2 = 3
       ↓
       3 + 3 = 6
             ↓
             6 + 4 = 10
                   ↓
                   10 + 5 = 15
```

## Creating Higher-Order Functions

### Functions That Take Functions

**Python:**
```python
def apply_to_each(func, items):
    """Apply function to each item in list"""
    return [func(item) for item in items]

def compose(f, g):
    """Compose two functions: compose(f, g)(x) = f(g(x))"""
    return lambda x: f(g(x))

# Usage
numbers = [1, 2, 3, 4, 5]
squares = apply_to_each(lambda x: x ** 2, numbers)  # [1, 4, 9, 16, 25]

add_one = lambda x: x + 1
times_two = lambda x: x * 2
add_then_double = compose(times_two, add_one)
result = add_then_double(5)  # (5 + 1) * 2 = 12
```

**C++:**
```cpp
// Function template taking a function
template<typename Func, typename T>
std::vector<T> applyToEach(Func func, const std::vector<T>& items) {
    std::vector<T> result;
    for (const auto& item : items) {
        result.push_back(func(item));
    }
    return result;
}

// Compose two functions
template<typename F, typename G>
auto compose(F f, G g) {
    return [=](auto x) { return f(g(x)); };
}

// Usage
std::vector<int> numbers = {1, 2, 3, 4, 5};
auto squares = applyToEach([](int x) { return x * x; }, numbers);

auto addOne = [](int x) { return x + 1; };
auto timesTwo = [](int x) { return x * 2; };
auto addThenDouble = compose(timesTwo, addOne);
int result = addThenDouble(5);  // 12
```

**Haskell:**
```haskell
-- Built-in map already does this!
applyToEach :: (a -> b) -> [a] -> [b]
applyToEach = map

-- Compose is built-in as (.)
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \x -> f (g x)

-- Usage
numbers = [1, 2, 3, 4, 5]
squares = applyToEach (^2) numbers  -- or just: map (^2) numbers

addOne = (+1)
timesTwo = (*2)
addThenDouble = timesTwo . addOne  -- Using built-in composition
-- or: addThenDouble = compose timesTwo addOne

result = addThenDouble 5  -- 12
```

### Functions That Return Functions

**Python:**
```python
def make_adder(n):
    """Returns a function that adds n to its argument"""
    def adder(x):
        return x + n
    return adder

def make_power(exponent):
    """Returns a function that raises to given exponent"""
    return lambda x: x ** exponent

# Usage
add_five = make_adder(5)
print(add_five(10))  # 15

square = make_power(2)
cube = make_power(3)
print(square(5))  # 25
print(cube(5))   # 125
```

**C++:**
```cpp
// Returns a lambda that captures n
auto makeAdder(int n) {
    return [n](int x) { return x + n; };
}

// Returns a lambda that captures exponent
auto makePower(int exponent) {
    return [exponent](int x) {
        return static_cast<int>(std::pow(x, exponent));
    };
}

// Usage
auto addFive = makeAdder(5);
std::cout << addFive(10) << std::endl;  // 15

auto square = makePower(2);
auto cube = makePower(3);
std::cout << square(5) << std::endl;  // 25
std::cout << cube(5) << std::endl;    // 125
```

**Haskell:**
```haskell
-- Returns a function
makeAdder :: Int -> (Int -> Int)
makeAdder n = \x -> x + n

-- More concise with currying (covered later)
makeAdder' :: Int -> Int -> Int
makeAdder' n x = x + n

makePower :: Int -> (Int -> Int)
makePower exponent = \x -> x ^ exponent

-- Usage
addFive = makeAdder 5
addFive 10  -- 15

square = makePower 2
cube = makePower 3
square 5  -- 25
cube 5    -- 125
```

## Closures

A **closure** is a function that captures variables from its enclosing scope.

**Python:**
```python
def make_counter():
    count = 0  # Captured by closure

    def increment():
        nonlocal count  # Modify captured variable
        count += 1
        return count

    return increment

counter1 = make_counter()
counter2 = make_counter()

print(counter1())  # 1
print(counter1())  # 2
print(counter2())  # 1 (separate closure)
print(counter1())  # 3
```

**C++:**
```cpp
auto makeCounter() {
    // Shared state via shared_ptr
    auto count = std::make_shared<int>(0);

    return [count]() mutable {
        (*count)++;
        return *count;
    };
}

auto counter1 = makeCounter();
auto counter2 = makeCounter();

std::cout << counter1() << std::endl;  // 1
std::cout << counter1() << std::endl;  // 2
std::cout << counter2() << std::endl;  // 1
std::cout << counter1() << std::endl;  // 3
```

**Haskell:**
```haskell
-- Pure functions can't have mutable state
-- But we can use State monad or return new state

makeCounter :: Int -> (Int, Int -> (Int, Int -> (Int, Int -> ())))
-- This gets complex quickly!

-- Better: Use State monad or explicit state passing
import Control.Monad.State

type Counter = State Int

increment :: Counter Int
increment = do
    count <- get
    let newCount = count + 1
    put newCount
    return newCount

-- Usage
runState (increment >> increment >> increment) 0  -- (3, 3)

-- Or simpler: just pass state explicitly
incrementPure :: Int -> (Int, Int)
incrementPure count = (count + 1, count + 1)
```

## Partial Application and Currying

### Currying

**Definition:** Transforming a function that takes multiple arguments into a sequence of functions each taking a single argument.

**Haskell (Native Currying):**
```haskell
-- All Haskell functions are curried by default!
add :: Int -> Int -> Int
add x y = x + y

-- These are equivalent:
result1 = add 3 5        -- 8
result2 = (add 3) 5      -- 8

-- Partial application
add3 :: Int -> Int
add3 = add 3

result3 = add3 5         -- 8

-- Type signature shows currying:
-- add :: Int -> (Int -> Int)
-- Function that takes Int and returns function from Int to Int
```

**Python (Manual Currying):**
```python
# Python doesn't curry by default
def add(x, y):
    return x + y

# Manual currying
def curried_add(x):
    def inner(y):
        return x + y
    return inner

# Or with lambda
curried_add = lambda x: lambda y: x + y

# Usage
add3 = curried_add(3)
result = add3(5)  # 8

# Library support
from functools import partial

add3_partial = partial(add, 3)
result = add3_partial(5)  # 8
```

**C++:**
```cpp
// Manual currying with lambdas
auto add = [](int x) {
    return [x](int y) {
        return x + y;
    };
};

auto add3 = add(3);
int result = add3(5);  // 8

// Using std::bind for partial application
#include <functional>

int addNormal(int x, int y) {
    return x + y;
}

auto add3Bind = std::bind(addNormal, 3, std::placeholders::_1);
int result2 = add3Bind(5);  // 8
```

### Benefits of Currying

```haskell
-- Easy to create specialized functions
map (+ 3) [1, 2, 3]        -- [4, 5, 6]
filter (> 5) [1, 6, 3, 8]  -- [6, 8]

-- Point-free style (no explicit parameters)
addThenDouble :: Int -> Int
addThenDouble = (*2) . (+1)

-- Function composition becomes natural
processNumbers :: [Int] -> [Int]
processNumbers = map (*2) . filter even . map (+1)
-- Reads right to left: add 1, filter evens, double
```

## Function Composition

**Concept:** Combining functions to create new functions.

**Python:**
```python
def compose(*functions):
    """Compose multiple functions right to left"""
    def inner(arg):
        result = arg
        for f in reversed(functions):
            result = f(result)
        return result
    return inner

# Usage
add_one = lambda x: x + 1
square = lambda x: x ** 2
negate = lambda x: -x

# Compose: negate(square(add_one(x)))
transform = compose(negate, square, add_one)
print(transform(5))  # -(5+1)^2 = -36

# Pipeline style (left to right)
def pipe(*functions):
    """Compose multiple functions left to right"""
    def inner(arg):
        result = arg
        for f in functions:
            result = f(result)
        return result
    return inner

transform2 = pipe(add_one, square, negate)
print(transform2(5))  # Same result: -36
```

**C++:**
```cpp
// Compose two functions
template<typename F, typename G>
auto compose(F f, G g) {
    return [=](auto x) { return f(g(x)); };
}

// Variadic compose
template<typename F>
auto composeMulti(F f) {
    return f;
}

template<typename F, typename... Rest>
auto composeMulti(F f, Rest... rest) {
    return [=](auto x) { return f(composeMulti(rest...)(x)); };
}

// Usage
auto addOne = [](int x) { return x + 1; };
auto square = [](int x) { return x * x; };
auto negate = [](int x) { return -x; };

auto transform = composeMulti(negate, square, addOne);
std::cout << transform(5) << std::endl;  // -36
```

**Haskell:**
```haskell
-- Built-in composition operator (.)
addOne = (+1)
square = (^2)
negate' = negate

-- Right to left composition
transform = negate' . square . addOne
result = transform 5  -- -36

-- Explicit composition
transform' = \x -> negate' (square (addOne x))

-- Multiple composition
processData = map (*2) . filter even . map (+1)
result = processData [1, 2, 3, 4, 5]  -- [4, 6, 8, 12]

-- Pipeline style with (&) or (>>>)
import Control.Arrow ((>>>))

transform'' = addOne >>> square >>> negate'
```

## Common Higher-Order Patterns

### Sorting with Custom Comparators

**Python:**
```python
people = [
    {"name": "Alice", "age": 30},
    {"name": "Bob", "age": 25},
    {"name": "Charlie", "age": 35}
]

# Sort by age
sorted_by_age = sorted(people, key=lambda p: p["age"])

# Sort by name
sorted_by_name = sorted(people, key=lambda p: p["name"])

# Custom comparison
from functools import cmp_to_key

def compare_people(p1, p2):
    if p1["age"] != p2["age"]:
        return p1["age"] - p2["age"]
    return -1 if p1["name"] < p2["name"] else 1

sorted_custom = sorted(people, key=cmp_to_key(compare_people))
```

**C++:**
```cpp
struct Person {
    std::string name;
    int age;
};

std::vector<Person> people = {
    {"Alice", 30},
    {"Bob", 25},
    {"Charlie", 35}
};

// Sort by age
std::sort(people.begin(), people.end(),
          [](const Person& a, const Person& b) {
              return a.age < b.age;
          });

// Sort by name
std::sort(people.begin(), people.end(),
          [](const Person& a, const Person& b) {
              return a.name < b.name;
          });
```

**Haskell:**
```haskell
import Data.List (sortBy)
import Data.Ord (comparing)

data Person = Person { name :: String, age :: Int } deriving (Show)

people = [
    Person "Alice" 30,
    Person "Bob" 25,
    Person "Charlie" 35
    ]

-- Sort by age
sortedByAge = sortBy (comparing age) people

-- Sort by name
sortedByName = sortBy (comparing name) people

-- Custom comparison
sortedCustom = sortBy (\p1 p2 -> compare (age p1) (age p2)) people
```

### Memoization (Caching Function Results)

**Python:**
```python
from functools import lru_cache

# Without memoization
def fib_slow(n):
    if n <= 1:
        return n
    return fib_slow(n - 1) + fib_slow(n - 2)

# With memoization
@lru_cache(maxsize=None)
def fib_fast(n):
    if n <= 1:
        return n
    return fib_fast(n - 1) + fib_fast(n - 2)

# Manual memoization
def memoize(func):
    cache = {}
    def wrapper(*args):
        if args not in cache:
            cache[args] = func(*args)
        return cache[args]
    return wrapper

@memoize
def fib_manual(n):
    if n <= 1:
        return n
    return fib_manual(n - 1) + fib_manual(n - 2)
```

**C++:**
```cpp
#include <unordered_map>

// Manual memoization
template<typename Result, typename... Args>
class Memoize {
private:
    std::function<Result(Args...)> func;
    mutable std::unordered_map<std::tuple<Args...>, Result> cache;

public:
    Memoize(std::function<Result(Args...)> f) : func(f) {}

    Result operator()(Args... args) const {
        auto key = std::make_tuple(args...);
        auto it = cache.find(key);
        if (it != cache.end()) {
            return it->second;
        }
        Result result = func(args...);
        cache[key] = result;
        return result;
    }
};

// Usage is complex due to recursion
// Better to use explicit cache
std::unordered_map<int, long long> fibCache;

long long fib(int n) {
    if (n <= 1) return n;
    if (fibCache.count(n)) return fibCache[n];
    fibCache[n] = fib(n - 1) + fib(n - 2);
    return fibCache[n];
}
```

**Haskell:**
```haskell
-- Lazy evaluation provides automatic memoization!
fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- Create infinite list of fibonacci numbers
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Access with: fibs !! n
-- Automatically memoized due to lazy evaluation!

-- Explicit memoization with Data.Function.Memoize
import Data.Function.Memoize

fibMemo :: Int -> Integer
fibMemo = memoize fib'
  where
    fib' 0 = 0
    fib' 1 = 1
    fib' n = fibMemo (n - 1) + fibMemo (n - 2)
```

## Comparing Approaches: Imperative vs Functional

**Problem:** Calculate sum of squares of even numbers

**Python Imperative:**
```python
numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

result = 0
for n in numbers:
    if n % 2 == 0:
        square = n ** 2
        result += square

print(result)  # 220
```

**Python Functional:**
```python
from functools import reduce

numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

result = reduce(
    lambda acc, x: acc + x,
    map(lambda x: x ** 2,
        filter(lambda x: x % 2 == 0, numbers)),
    0
)

# Or more Pythonic
result = sum(x ** 2 for x in numbers if x % 2 == 0)

print(result)  # 220
```

**C++ Imperative:**
```cpp
std::vector<int> numbers = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

int result = 0;
for (int n : numbers) {
    if (n % 2 == 0) {
        int square = n * n;
        result += square;
    }
}

std::cout << result << std::endl;  // 220
```

**C++ Functional:**
```cpp
std::vector<int> numbers = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

int result = std::accumulate(
    numbers.begin(), numbers.end(), 0,
    [](int acc, int n) {
        return n % 2 == 0 ? acc + n * n : acc;
    }
);

// Or with ranges (C++20)
auto result = numbers
    | std::views::filter([](int n) { return n % 2 == 0; })
    | std::views::transform([](int n) { return n * n; })
    | std::ranges::fold_left(0, std::plus<>());
```

**Haskell Functional:**
```haskell
numbers = [1..10]

result = sum $ map (^2) $ filter even numbers

-- Or with function composition
result = sum . map (^2) . filter even $ numbers

-- Or with list comprehension
result = sum [x^2 | x <- numbers, even x]

-- All produce: 220
```

## Advanced: Function Combinators

**Haskell excels at function combinators:**
```haskell
-- Common combinators
-- id: identity function
id :: a -> a
id x = x

-- const: constant function
const :: a -> b -> a
const x _ = x

-- flip: flip argument order
flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

-- ($): function application (low precedence)
($) :: (a -> b) -> a -> b
f $ x = f x

-- (&): reverse function application
(&) :: a -> (a -> b) -> b
x & f = f x

-- Usage examples
result = map (+1) $ filter even [1..10]
result' = [1..10] & filter even & map (+1)  -- Pipeline style
```

## Language Comparison

| Feature | Python | C++ | Haskell |
|---------|--------|-----|---------|
| **Lambda Syntax** | `lambda x: x + 1` | `[](int x) { return x + 1; }` | `\x -> x + 1` |
| **Built-in map** | `map(f, list)` | `std::transform` | `map f list` |
| **Built-in filter** | `filter(p, list)` | `std::copy_if` | `filter p list` |
| **Built-in reduce** | `reduce(f, list, init)` | `std::accumulate` | `foldl f init list` |
| **Currying** | Manual/`functools.partial` | Manual/`std::bind` | Native |
| **Composition** | Manual | Manual | Native `(.)` |
| **Type inference** | Runtime only | Full (C++14+) | Full |
| **Performance** | Dynamic dispatch | Can inline/optimize | Can optimize heavily |

## Key Takeaways

1. **Higher-order functions treat functions as data** - pass them, return them, store them

2. **Map/filter/reduce are fundamental patterns** - master these and you master functional thinking

3. **Closures capture scope** - enables powerful patterns like counters, memoization, factories

4. **Currying enables partial application** - create specialized functions from general ones

5. **Composition builds complexity from simplicity** - combine small functions into larger ones

6. **Functional style emphasizes what over how** - describe transformations, not implementation steps

7. **Python: flexible and pragmatic** - supports functional style alongside imperative

8. **C++: powerful but verbose** - modern C++ has excellent functional features

9. **Haskell: functional by design** - all the benefits of functional programming built-in

## Looking Ahead

While we focused on Python, C++, and Haskell, other languages offer interesting functional features:

- **JavaScript**: First-class functions, closures, array methods
- **Rust**: Iterators, closures, functional combinators
- **Java**: Streams API (since Java 8)
- **Ruby**: Blocks and procs

## Exercises

See `EXERCISES.md` for hands-on practice with higher-order functions in Python, C++, and Haskell.

---

**Next Lesson**: Pattern Matching - where we explore how to deconstruct and analyze data structures elegantly.
