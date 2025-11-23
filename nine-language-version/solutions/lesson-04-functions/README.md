# Lesson 4: Functions - Solution Guide

This guide provides example solutions for the Functions exercises.

## General Notes

- **Multiple correct solutions exist**: Your solution may differ and still be correct
- **Focus on understanding**: Don't just copy-paste; understand why each solution works
- **Language idioms matter**: Each language has its own conventions and style
- **Try before looking**: Attempt each exercise before checking solutions
- **Paradigm awareness**: Notice how different paradigms approach the same problem

---

## Exercise 1: Temperature Converter (Warmup)

**Task:** Write `celsius_to_fahrenheit(c)` and `fahrenheit_to_celsius(f)` functions

### Python Solution

```python
def celsius_to_fahrenheit(c):
    return c * 9/5 + 32

def fahrenheit_to_celsius(f):
    return (f - 32) * 5/9

# Test cases
print(celsius_to_fahrenheit(0))      # 32.0
print(celsius_to_fahrenheit(100))    # 212.0
print(fahrenheit_to_celsius(32))     # 0.0
print(fahrenheit_to_celsius(98.6))   # 37.0
```

**Run:** `python3 temperature.py`

### JavaScript Solution

```javascript
function celsiusToFahrenheit(c) {
    return c * 9/5 + 32;
}

function fahrenheitToCelsius(f) {
    return (f - 32) * 5/9;
}

// Arrow function alternative
const celsiusToFahrenheitArrow = c => c * 9/5 + 32;
const fahrenheitToCelsiusArrow = f => (f - 32) * 5/9;

// Test cases
console.log(celsiusToFahrenheit(0));      // 32
console.log(celsiusToFahrenheit(100));    // 212
console.log(fahrenheitToCelsius(32));     // 0
console.log(fahrenheitToCelsius(98.6));   // 37
```

**Run:** `node temperature.js`

### C Solution

```c
#include <stdio.h>

double celsius_to_fahrenheit(double c) {
    return c * 9.0/5.0 + 32.0;
}

double fahrenheit_to_celsius(double f) {
    return (f - 32.0) * 5.0/9.0;
}

int main() {
    printf("%.1f\n", celsius_to_fahrenheit(0));      // 32.0
    printf("%.1f\n", celsius_to_fahrenheit(100));    // 212.0
    printf("%.1f\n", fahrenheit_to_celsius(32));     // 0.0
    printf("%.1f\n", fahrenheit_to_celsius(98.6));   // 37.0
    return 0;
}
```

**Compile and run:**
```bash
gcc temperature.c -o temperature
./temperature
```

### Haskell Solution

```haskell
celsiusToFahrenheit :: Double -> Double
celsiusToFahrenheit c = c * 9/5 + 32

fahrenheitToCelsius :: Double -> Double
fahrenheitToCelsius f = (f - 32) * 5/9

main :: IO ()
main = do
    print $ celsiusToFahrenheit 0      -- 32.0
    print $ celsiusToFahrenheit 100    -- 212.0
    print $ fahrenheitToCelsius 32     -- 0.0
    print $ fahrenheitToCelsius 98.6   -- 37.0
```

**Run:** `runhaskell temperature.hs`

**Key Insights:**
- Simple mathematical transformations make good pure functions
- Type annotations in C and Haskell prevent integer division errors
- All versions are pure (same input → same output, no side effects)

---

## Exercise 2: Simple Calculator

**Task:** Write `add`, `subtract`, `multiply`, `divide` functions

### Python Solution

```python
def add(x, y):
    return x + y

def subtract(x, y):
    return x - y

def multiply(x, y):
    return x * y

def divide(x, y):
    if y == 0:
        return None  # or raise ValueError("Division by zero")
    return x / y

# Bonus: Higher-order calculate function
def calculate(operation, x, y):
    return operation(x, y)

# Test
print(add(5, 3))           # 8
print(subtract(10, 4))     # 6
print(multiply(6, 7))      # 42
print(divide(15, 3))       # 5.0
print(divide(10, 0))       # None

# Using higher-order function
print(calculate(add, 5, 3))       # 8
print(calculate(multiply, 4, 5))  # 20
```

### JavaScript Solution

```javascript
function add(x, y) {
    return x + y;
}

function subtract(x, y) {
    return x - y;
}

function multiply(x, y) {
    return x * y;
}

function divide(x, y) {
    if (y === 0) {
        return null;  // or throw new Error("Division by zero")
    }
    return x / y;
}

// Arrow function versions
const addArrow = (x, y) => x + y;
const subtractArrow = (x, y) => x - y;
const multiplyArrow = (x, y) => x * y;
const divideArrow = (x, y) => y === 0 ? null : x / y;

// Bonus: Higher-order calculate
function calculate(operation, x, y) {
    return operation(x, y);
}

// Test
console.log(add(5, 3));           // 8
console.log(calculate(multiply, 4, 5));  // 20
```

### Java Solution

```java
public class Calculator {
    public static int add(int x, int y) {
        return x + y;
    }

    public static int subtract(int x, int y) {
        return x - y;
    }

    public static int multiply(int x, int y) {
        return x * y;
    }

    public static Double divide(double x, double y) {
        if (y == 0) {
            return null;  // or throw ArithmeticException
        }
        return x / y;
    }

    // Overloaded versions for doubles
    public static double add(double x, double y) {
        return x + y;
    }

    public static void main(String[] args) {
        System.out.println(add(5, 3));           // 8
        System.out.println(subtract(10, 4));     // 6
        System.out.println(multiply(6, 7));      // 42
        System.out.println(divide(15.0, 3.0));   // 5.0
        System.out.println(divide(10.0, 0.0));   // null
    }
}
```

**Key Insights:**
- Division by zero handling varies by language
- Java uses method overloading instead of dynamic typing
- Higher-order functions treat functions as first-class values

---

## Exercise 3: String Utilities

**Task:** Write `reverse_string`, `is_palindrome`, `count_vowels`

### Python Solution (Imperative)

```python
def reverse_string(s):
    return s[::-1]

def is_palindrome(s):
    return s == s[::-1]

def count_vowels(s):
    vowels = "aeiouAEIOU"
    count = 0
    for char in s:
        if char in vowels:
            count += 1
    return count

# Alternative functional approach
def count_vowels_functional(s):
    vowels = "aeiouAEIOU"
    return sum(1 for char in s if char in vowels)

# Test
print(reverse_string("hello"))      # "olleh"
print(is_palindrome("racecar"))     # True
print(is_palindrome("hello"))       # False
print(count_vowels("hello world"))  # 3
```

### JavaScript Solution

```javascript
function reverseString(s) {
    return s.split('').reverse().join('');
}

// Alternative: manual loop
function reverseStringManual(s) {
    let result = '';
    for (let i = s.length - 1; i >= 0; i--) {
        result += s[i];
    }
    return result;
}

function isPalindrome(s) {
    return s === reverseString(s);
}

function countVowels(s) {
    const vowels = 'aeiouAEIOU';
    let count = 0;
    for (let char of s) {
        if (vowels.includes(char)) {
            count++;
        }
    }
    return count;
}

// Functional approach using filter
function countVowelsFunctional(s) {
    const vowels = 'aeiouAEIOU';
    return s.split('').filter(char => vowels.includes(char)).length;
}

// Test
console.log(reverseString("hello"));      // "olleh"
console.log(isPalindrome("racecar"));     // true
console.log(isPalindrome("hello"));       // false
console.log(countVowels("hello world"));  // 3
```

### Haskell Solution (Functional)

```haskell
import Data.Char (toLower)

reverseString :: String -> String
reverseString = reverse  -- Built-in reverse function

isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

countVowels :: String -> Int
countVowels s = length $ filter isVowel s
  where
    isVowel c = toLower c `elem` "aeiou"

-- Alternative using list comprehension
countVowelsAlt :: String -> Int
countVowelsAlt s = length [c | c <- s, toLower c `elem` "aeiou"]

main :: IO ()
main = do
    putStrLn $ reverseString "hello"      -- "olleh"
    print $ isPalindrome "racecar"        -- True
    print $ isPalindrome "hello"          -- False
    print $ countVowels "hello world"     -- 3
```

**Key Insights:**
- Python's slicing syntax makes string reversal trivial
- JavaScript requires split/reverse/join for strings
- Haskell's functional approach uses filter naturally
- Palindrome checking builds on reverse function (composition!)

---

## Exercise 4: Apply Function N Times

**Task:** Write `apply_n_times(f, x, n)` that applies function `f` to value `x` exactly `n` times

### Python Solution (Loop)

```python
def apply_n_times(f, x, n):
    result = x
    for _ in range(n):
        result = f(result)
    return result

# Test
double = lambda x: x * 2
increment = lambda x: x + 1

print(apply_n_times(double, 5, 0))      # 5
print(apply_n_times(double, 5, 1))      # 10
print(apply_n_times(double, 5, 3))      # 40
print(apply_n_times(increment, 10, 5))  # 15
```

### Python Solution (Recursive)

```python
def apply_n_times_recursive(f, x, n):
    if n == 0:
        return x
    return apply_n_times_recursive(f, f(x), n - 1)

# Alternative: apply f first, then recurse
def apply_n_times_recursive_alt(f, x, n):
    if n == 0:
        return x
    return f(apply_n_times_recursive_alt(f, x, n - 1))
```

### JavaScript Solution

```javascript
// Iterative
function applyNTimes(f, x, n) {
    let result = x;
    for (let i = 0; i < n; i++) {
        result = f(result);
    }
    return result;
}

// Recursive
function applyNTimesRecursive(f, x, n) {
    if (n === 0) return x;
    return applyNTimesRecursive(f, f(x), n - 1);
}

// Test
const double = x => x * 2;
const increment = x => x + 1;

console.log(applyNTimes(double, 5, 0));      // 5
console.log(applyNTimes(double, 5, 1));      // 10
console.log(applyNTimes(double, 5, 3));      // 40
console.log(applyNTimes(increment, 10, 5));  // 15
```

### Haskell Solution (Recursive)

```haskell
applyNTimes :: (a -> a) -> a -> Int -> a
applyNTimes f x 0 = x
applyNTimes f x n = applyNTimes f (f x) (n - 1)

-- Alternative using iterate (more idiomatic)
applyNTimesIdiomatic :: (a -> a) -> a -> Int -> a
applyNTimesIdiomatic f x n = iterate f x !! n

-- Test
double :: Int -> Int
double x = x * 2

increment :: Int -> Int
increment x = x + 1

main :: IO ()
main = do
    print $ applyNTimes double 5 0      -- 5
    print $ applyNTimes double 5 1      -- 10
    print $ applyNTimes double 5 3      -- 40
    print $ applyNTimes increment 10 5  -- 15
```

### Racket Solution

```racket
#lang racket

(define (apply-n-times f x n)
  (if (= n 0)
      x
      (apply-n-times f (f x) (- n 1))))

; Test
(define (double x) (* x 2))
(define (increment x) (+ x 1))

(displayln (apply-n-times double 5 0))      ; 5
(displayln (apply-n-times double 5 1))      ; 10
(displayln (apply-n-times double 5 3))      ; 40
(displayln (apply-n-times increment 10 5))  ; 15
```

**Key Insights:**
- Iterative approach uses a loop to accumulate result
- Recursive approach naturally expresses the problem
- Haskell's `iterate` function is built for this pattern
- Type signature `(a -> a) -> a -> Int -> a` shows function is polymorphic

---

## Exercise 5: Counter Factory (Closures)

**Task:** Write `make_counter()` that returns independent counter functions

### Python Solution

```python
def make_counter():
    count = 0  # Captured by closure

    def counter():
        nonlocal count  # Modify outer scope variable
        count += 1
        return count

    return counter

# Test
counter1 = make_counter()
counter2 = make_counter()
print(counter1())  # 1
print(counter1())  # 2
print(counter2())  # 1  # Independent!
print(counter1())  # 3
```

### JavaScript Solution

```javascript
function makeCounter() {
    let count = 0;  // Private variable captured by closure

    return function() {
        count++;
        return count;
    };
}

// Arrow function version
const makeCounterArrow = () => {
    let count = 0;
    return () => ++count;
};

// Test
const counter1 = makeCounter();
const counter2 = makeCounter();
console.log(counter1());  // 1
console.log(counter1());  // 2
console.log(counter2());  // 1
console.log(counter1());  // 3
```

### Ruby Solution

```ruby
def make_counter
  count = 0  # Captured by lambda

  lambda do
    count += 1
  end
end

# Test
counter1 = make_counter
counter2 = make_counter
puts counter1.call  # 1
puts counter1.call  # 2
puts counter2.call  # 1
puts counter1.call  # 3
```

### Haskell Discussion

**Haskell doesn't have mutable closures by default!** Pure functions can't maintain state.

However, we can simulate this with the State monad or IO:

```haskell
import Data.IORef

makeCounter :: IO (IO Int)
makeCounter = do
    countRef <- newIORef 0  -- Create mutable reference
    return $ do
        count <- readIORef countRef
        let newCount = count + 1
        writeIORef countRef newCount
        return newCount

-- Test
main :: IO ()
main = do
    counter1 <- makeCounter
    counter2 <- makeCounter
    print =<< counter1  -- 1
    print =<< counter1  -- 2
    print =<< counter2  -- 1
    print =<< counter1  -- 3
```

**Key Insights:**
- Closures capture variables from enclosing scope
- JavaScript/Python make this natural and easy
- Haskell requires explicit IO or State monad for mutation
- Each counter maintains independent state
- This demonstrates encapsulation without classes!

---

## Exercise 6: Function Composition

**Task:** Write `compose(f, g)` that returns `f(g(x))`

### Python Solution

```python
def compose(f, g):
    return lambda x: f(g(x))

# Test functions
square = lambda x: x * x
increment = lambda x: x + 1
double = lambda x: x * 2

# Compose functions
square_then_increment = compose(increment, square)
increment_then_square = compose(square, increment)

print(square_then_increment(5))  # 26 (square 5 = 25, then +1 = 26)
print(increment_then_square(5))  # 36 (5+1 = 6, then square = 36)
print(compose(double, double)(5))  # 20 (5*2=10, 10*2=20)
```

### Bonus: Compose Many

```python
from functools import reduce

def compose_many(functions):
    """Compose multiple functions right-to-left"""
    return lambda x: reduce(lambda acc, f: f(acc), reversed(functions), x)

# Alternative: explicit loop
def compose_many_alt(functions):
    def composed(x):
        result = x
        for f in reversed(functions):
            result = f(result)
        return result
    return composed

# Test
pipeline = compose_many([increment, double, square])
print(pipeline(5))  # square(5)=25, double(25)=50, increment(50)=51
```

### JavaScript Solution

```javascript
const compose = (f, g) => x => f(g(x));

// Test functions
const square = x => x * x;
const increment = x => x + 1;
const double = x => x * 2;

// Compose
const squareThenIncrement = compose(increment, square);
const incrementThenSquare = compose(square, increment);

console.log(squareThenIncrement(5));  // 26
console.log(incrementThenSquare(5));  // 36
console.log(compose(double, double)(5));  // 20
```

### Bonus: Compose Many (JavaScript)

```javascript
const composeMany = (...functions) =>
    x => functions.reduceRight((acc, f) => f(acc), x);

// Or left-to-right (pipe)
const pipe = (...functions) =>
    x => functions.reduce((acc, f) => f(acc), x);

// Test
const pipeline = composeMany(increment, double, square);
console.log(pipeline(5));  // 51
```

### Haskell Solution

```haskell
-- Haskell has built-in composition with (.)
square :: Int -> Int
square x = x * x

increment :: Int -> Int
increment x = x + 1

double :: Int -> Int
double x = x * 2

-- Using built-in composition operator
squareThenIncrement :: Int -> Int
squareThenIncrement = increment . square

incrementThenSquare :: Int -> Int
incrementThenSquare = square . increment

-- Manual implementation
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \x -> f (g x)

-- Compose many (using foldr)
composeMany :: [a -> a] -> (a -> a)
composeMany = foldr (.) id

main :: IO ()
main = do
    print $ squareThenIncrement 5  -- 26
    print $ incrementThenSquare 5  -- 36
    print $ (double . double) 5    -- 20
```

**Key Insights:**
- Composition is reading right-to-left: `compose(f, g)` means "g then f"
- Haskell has first-class composition operator (.)
- Pipe (left-to-right) is often more readable for data transformations
- Composition enables building complex functions from simple ones

---

## Exercise 7: Currying

**Task:** Write curried `add` and generic `curry` function

### Part A: Manual Currying

#### Python

```python
def add(x):
    def inner(y):
        return x + y
    return inner

# Alternative: lambda
add_lambda = lambda x: lambda y: x + y

# Test
print(add(3)(5))  # 8
add_five = add(5)
print(add_five(10))  # 15
```

#### JavaScript

```javascript
function add(x) {
    return function(y) {
        return x + y;
    };
}

// Arrow function version
const addArrow = x => y => x + y;

// Test
console.log(add(3)(5));  // 8
const addFive = add(5);
console.log(addFive(10));  // 15
```

#### Haskell

```haskell
-- All Haskell functions are automatically curried!
add :: Int -> Int -> Int
add x y = x + y

-- These are equivalent:
-- add 3 5
-- (add 3) 5

-- Partial application is natural
addFive :: Int -> Int
addFive = add 5

main :: IO ()
main = do
    print $ add 3 5      -- 8
    print $ addFive 10   -- 15
```

### Part B: Generic Curry Function

#### Python

```python
def curry(f):
    """Convert a two-argument function into a curried function"""
    def curried(x):
        def inner(y):
            return f(x, y)
        return inner
    return curried

# Test
def multiply(x, y):
    return x * y

curried_multiply = curry(multiply)
times_five = curried_multiply(5)
print(times_five(10))  # 50
print(curried_multiply(3)(7))  # 21
```

#### JavaScript

```javascript
function curry(f) {
    return function(x) {
        return function(y) {
            return f(x, y);
        };
    };
}

// Arrow function version
const curryArrow = f => x => y => f(x, y);

// Test
function multiply(x, y) {
    return x * y;
}

const curriedMultiply = curry(multiply);
const timesFive = curriedMultiply(5);
console.log(timesFive(10));  // 50
console.log(curriedMultiply(3)(7));  // 21
```

#### Advanced: Auto-Curry (JavaScript)

```javascript
function autoCurry(f, arity = f.length) {
    return function curried(...args) {
        if (args.length >= arity) {
            return f(...args);
        }
        return (...moreArgs) => curried(...args, ...moreArgs);
    };
}

// Test
function sum(a, b, c) {
    return a + b + c;
}

const curriedSum = autoCurry(sum);
console.log(curriedSum(1)(2)(3));  // 6
console.log(curriedSum(1, 2)(3));  // 6
console.log(curriedSum(1)(2, 3));  // 6
```

**Key Insights:**
- Currying transforms f(x, y) into f(x)(y)
- Enables partial application naturally
- Haskell does this automatically for all functions
- JavaScript/Python require manual currying
- Auto-curry is powerful but complex

---

## Exercise 8: Filter and Map

**Task:** Implement custom `my_map`, `my_filter`, and `my_reduce`

### Part A: Map and Filter

#### Python

```python
def my_map(func, lst):
    """Apply func to each element"""
    result = []
    for item in lst:
        result.append(func(item))
    return result

def my_filter(pred, lst):
    """Keep only elements where pred returns True"""
    result = []
    for item in lst:
        if pred(item):
            result.append(item)
    return result

# Test
square = lambda x: x ** 2
is_even = lambda x: x % 2 == 0

print(my_map(square, [1, 2, 3, 4]))      # [1, 4, 9, 16]
print(my_filter(is_even, [1, 2, 3, 4, 5]))  # [2, 4]
```

#### JavaScript

```javascript
function myMap(func, lst) {
    const result = [];
    for (const item of lst) {
        result.push(func(item));
    }
    return result;
}

function myFilter(pred, lst) {
    const result = [];
    for (const item of lst) {
        if (pred(item)) {
            result.push(item);
        }
    }
    return result;
}

// Test
const square = x => x ** 2;
const isEven = x => x % 2 === 0;

console.log(myMap(square, [1, 2, 3, 4]));        // [1, 4, 9, 16]
console.log(myFilter(isEven, [1, 2, 3, 4, 5]));  // [2, 4]
```

#### Haskell

```haskell
-- Implement from scratch
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter pred (x:xs)
    | pred x    = x : myFilter pred xs
    | otherwise = myFilter pred xs

-- Test
square :: Int -> Int
square x = x * x

isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

main :: IO ()
main = do
    print $ myMap square [1, 2, 3, 4]        -- [1,4,9,16]
    print $ myFilter isEven [1, 2, 3, 4, 5]  -- [2,4]
```

### Part B: Reduce

#### Python

```python
def my_reduce(func, lst, initial):
    """Combine elements using func, starting with initial"""
    accumulator = initial
    for item in lst:
        accumulator = func(accumulator, item)
    return accumulator

# Bonus: Without initial value
def my_reduce_no_initial(func, lst):
    if not lst:
        raise ValueError("reduce of empty sequence with no initial value")
    accumulator = lst[0]
    for item in lst[1:]:
        accumulator = func(accumulator, item)
    return accumulator

# Test
add = lambda x, y: x + y
multiply = lambda x, y: x * y

print(my_reduce(add, [1, 2, 3, 4], 0))       # 10
print(my_reduce(multiply, [1, 2, 3, 4], 1))  # 24
print(my_reduce_no_initial(add, [1, 2, 3, 4]))  # 10
```

#### JavaScript

```javascript
function myReduce(func, lst, initial) {
    let accumulator = initial;
    for (const item of lst) {
        accumulator = func(accumulator, item);
    }
    return accumulator;
}

// Bonus: Without initial
function myReduceNoInitial(func, lst) {
    if (lst.length === 0) {
        throw new Error("Reduce of empty array with no initial value");
    }
    let accumulator = lst[0];
    for (let i = 1; i < lst.length; i++) {
        accumulator = func(accumulator, lst[i]);
    }
    return accumulator;
}

// Test
const add = (x, y) => x + y;
const multiply = (x, y) => x * y;

console.log(myReduce(add, [1, 2, 3, 4], 0));       // 10
console.log(myReduce(multiply, [1, 2, 3, 4], 1));  // 24
```

#### Haskell

```haskell
-- Left fold (like reduce)
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

-- Right fold
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ acc [] = acc
myFoldr f acc (x:xs) = f x (myFoldr f acc xs)

-- Test
main :: IO ()
main = do
    print $ myFoldl (+) 0 [1, 2, 3, 4]       -- 10
    print $ myFoldl (*) 1 [1, 2, 3, 4]       -- 24
    print $ myFoldr (+) 0 [1, 2, 3, 4]       -- 10
```

**Key Insights:**
- Map transforms each element independently
- Filter selects elements based on predicate
- Reduce combines all elements into single value
- These are the fundamental building blocks of functional programming
- Haskell uses pattern matching naturally for recursion

---

## Exercise 9: Function Pipeline

**Task:** Create pipeline function that chains operations

### Python Solution

```python
def compose_many(functions):
    """Right-to-left composition"""
    def pipeline(x):
        result = x
        for f in reversed(functions):
            result = f(result)
        return result
    return pipeline

def pipe(x, *functions):
    """Left-to-right pipe (more readable)"""
    result = x
    for f in functions:
        result = f(result)
    return result

# Test functions
add_one = lambda x: x + 1
double = lambda x: x * 2
subtract_three = lambda x: x - 3

# Right-to-left
pipeline = compose_many([
    lambda x: x + 1,
    lambda x: x * 2,
    lambda x: x - 3
])
print(pipeline(5))  # ((5 - 3) * 2) + 1 = 5

# Left-to-right (more intuitive)
result = pipe(
    5,
    lambda x: x + 1,  # 6
    lambda x: x * 2,  # 12
    lambda x: x - 3   # 9
)
print(result)  # 9
```

### JavaScript Solution

```javascript
// Right-to-left compose
const composeMany = (...functions) =>
    x => functions.reduceRight((acc, f) => f(acc), x);

// Left-to-right pipe
const pipe = (x, ...functions) =>
    functions.reduce((acc, f) => f(acc), x);

// Alternative: pipe returns function
const pipeFunc = (...functions) =>
    x => functions.reduce((acc, f) => f(acc), x);

// Test
const pipeline = pipeFunc(
    x => x + 1,  // 6
    x => x * 2,  // 12
    x => x - 3   // 9
);

console.log(pipeline(5));  // 9

// Direct pipe
const result = pipe(
    5,
    x => x + 1,
    x => x * 2,
    x => x - 3
);
console.log(result);  // 9
```

### Haskell Solution

```haskell
-- Right-to-left composition (built-in)
pipeline1 :: Int -> Int
pipeline1 = (+1) . (*2) . (subtract 3)

-- Custom pipe (left-to-right)
(|>) :: a -> (a -> b) -> b
x |> f = f x

-- Chain multiple pipes
pipeline2 :: Int -> Int
pipeline2 x = x
    |> (+1)      -- 6
    |> (*2)      -- 12
    |> (subtract 3)  -- 9

-- Or define pipe combinator
pipe :: [a -> a] -> a -> a
pipe fs x = foldl (\acc f -> f acc) x fs

main :: IO ()
main = do
    print $ pipeline1 5      -- Depends on operation order
    print $ pipeline2 5      -- 9
```

**Key Insights:**
- Pipe (left-to-right) is more intuitive for data flow
- Compose (right-to-left) matches mathematical notation
- Both are useful in different contexts
- Reduce/fold makes implementation elegant

---

## Exercise 10: Memoization

**Task:** Write `memoize` function that caches results

### Python Solution

```python
def memoize(f):
    """Cache results of function calls"""
    cache = {}

    def memoized(n):
        if n not in cache:
            cache[n] = f(n)
        return cache[n]

    return memoized

# Test with fibonacci
def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)

# Without memoization: very slow for large n
# fibonacci(35)  # Takes several seconds

# With memoization
fibonacci_memo = memoize(fibonacci)
print(fibonacci_memo(35))  # Fast!
print(fibonacci_memo(35))  # Even faster (cached)!

# Alternative: Using decorator
def memoize_decorator(f):
    cache = {}
    def memoized(*args):
        if args not in cache:
            cache[args] = f(*args)
        return cache[args]
    return memoized

@memoize_decorator
def fib(n):
    if n <= 1:
        return n
    return fib(n - 1) + fib(n - 2)

print(fib(100))  # Works efficiently!
```

### Python with functools.lru_cache

```python
from functools import lru_cache

@lru_cache(maxsize=None)
def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)

print(fibonacci(100))  # Fast and built-in!
```

### JavaScript Solution

```javascript
function memoize(f) {
    const cache = {};

    return function(n) {
        if (!(n in cache)) {
            cache[n] = f(n);
        }
        return cache[n];
    };
}

// Test
function fibonacci(n) {
    if (n <= 1) return n;
    return fibonacci(n - 1) + fibonacci(n - 2);
}

const fibonacciMemo = memoize(fibonacci);
console.log(fibonacciMemo(35));  // Much faster!

// Bonus: Multi-argument memoization
function memoizeMulti(f) {
    const cache = new Map();

    return function(...args) {
        const key = JSON.stringify(args);
        if (!cache.has(key)) {
            cache.set(key, f(...args));
        }
        return cache.get(key);
    };
}
```

### Discussion: Purity Requirement

```python
# Pure function: safe to memoize
def pure_square(x):
    return x * x

# Impure function: DANGEROUS to memoize!
import random
def impure_random(x):
    return x + random.randint(1, 10)

# If we memoize impure_random:
memoized_random = memoize(impure_random)
print(memoized_random(5))  # e.g., 12
print(memoized_random(5))  # Still 12! (cached, not random anymore!)
```

**Key Insights:**
- Memoization trades memory for speed
- Only works correctly for pure functions
- Impure functions lose their behavior when memoized
- Cache key must uniquely identify inputs
- Python's `lru_cache` is production-ready

---

## Exercise 11: Partial Application

**Task:** Write `partial` function

### Python Solution

```python
def partial(func, *fixed_args):
    """Fix some arguments of a function"""
    def partial_func(*remaining_args):
        return func(*fixed_args, *remaining_args)
    return partial_func

# Test
def greet(greeting, name):
    return f"{greeting}, {name}!"

say_hello = partial(greet, "Hello")
say_goodbye = partial(greet, "Goodbye")

print(say_hello("Alice"))    # "Hello, Alice!"
print(say_goodbye("Bob"))    # "Goodbye, Bob!"

# More examples
def add(x, y):
    return x + y

def multiply(x, y):
    return x * y

add_five = partial(add, 5)
print(add_five(10))  # 15

multiply_by_two = partial(multiply, 2)
print(multiply_by_two(7))  # 14
```

### Python with functools.partial

```python
from functools import partial as functools_partial

def greet(greeting, name):
    return f"{greeting}, {name}!"

say_hello = functools_partial(greet, "Hello")
print(say_hello("Alice"))  # "Hello, Alice!"
```

### Bonus: Right Partial

```python
def partial_right(func, *fixed_args):
    """Fix arguments from the right"""
    def partial_func(*remaining_args):
        return func(*remaining_args, *fixed_args)
    return partial_func

# Test
def divide(x, y):
    return x / y

divide_by_two = partial_right(divide, 2)
print(divide_by_two(10))  # 10 / 2 = 5.0
```

### JavaScript Solution

```javascript
function partial(func, ...fixedArgs) {
    return function(...remainingArgs) {
        return func(...fixedArgs, ...remainingArgs);
    };
}

// Arrow version
const partialArrow = (func, ...fixedArgs) =>
    (...remainingArgs) => func(...fixedArgs, ...remainingArgs);

// Test
function greet(greeting, name) {
    return `${greeting}, ${name}!`;
}

const sayHello = partial(greet, "Hello");
const sayGoodbye = partial(greet, "Goodbye");

console.log(sayHello("Alice"));    // "Hello, Alice!"
console.log(sayGoodbye("Bob"));    // "Goodbye, Bob!"
```

### Haskell (Already Has This!)

```haskell
-- Haskell has natural partial application
greet :: String -> String -> String
greet greeting name = greeting ++ ", " ++ name ++ "!"

sayHello :: String -> String
sayHello = greet "Hello"  -- Partial application!

sayGoodbye :: String -> String
sayGoodbye = greet "Goodbye"

main :: IO ()
main = do
    putStrLn $ sayHello "Alice"    -- "Hello, Alice!"
    putStrLn $ sayGoodbye "Bob"    -- "Goodbye, Bob!"
```

**Key Insights:**
- Partial application creates specialized functions
- Similar to currying but more flexible
- Haskell does this automatically
- Python's functools.partial is production-ready
- Useful for creating reusable, configured functions

---

## Exercise 12: Pure vs Impure

**Task:** Classify functions and rewrite impure ones

### Classifications

1. **`def add(x, y): return x + y`**
   - **PURE**: Same inputs always produce same output, no side effects

2. **`def print_sum(x, y): result = x + y; print(result); return result`**
   - **IMPURE**: Has side effect (prints to console)

3. **`counter = 0; def increment(): global counter; counter += 1; return counter`**
   - **IMPURE**: Modifies global state, non-deterministic (output depends on how many times called)

4. **`def get_length(lst): return len(lst)`**
   - **PURE**: Same input always produces same output, no side effects

5. **`def append_to_list(lst, item): lst.append(item); return lst`**
   - **IMPURE**: Mutates input (side effect), even though it returns a value

6. **`def get_current_time(): import time; return time.time()`**
   - **IMPURE**: Non-deterministic (different output each call), depends on external state

7. **`import random; def roll_dice(): return random.randint(1, 6)`**
   - **IMPURE**: Non-deterministic (different output for same inputs)

### Pure Rewrites

```python
# 2. print_sum → pure version separates calculation and I/O
def add(x, y):
    return x + y

# Use it like:
# result = add(5, 3)
# print(result)  # I/O separate from calculation

# 3. increment → pass state explicitly
def increment(counter):
    return counter + 1

# Use it like:
# counter = 0
# counter = increment(counter)  # Returns new value

# 5. append_to_list → return new list
def append_to_list_pure(lst, item):
    return lst + [item]  # Creates new list

# Or more explicit:
def append_to_list_pure_explicit(lst, item):
    new_list = lst.copy()
    new_list.append(item)
    return new_list

# 6. get_current_time → pass time as parameter
def format_time(time_value):
    # Do something with the time
    return time_value

# Use it like:
# import time
# current_time = time.time()  # Impure operation at boundary
# formatted = format_time(current_time)  # Pure function

# 7. roll_dice → pass random generator
def roll_dice_pure(random_source):
    return random_source.randint(1, 6)

# Or: accept the random number directly
def process_roll(roll_value):
    # Do something with the roll
    return roll_value

# Use it like:
# import random
# roll = random.randint(1, 6)  # Impure at boundary
# result = process_roll(roll)  # Pure processing
```

**Key Insights:**
- Pure functions separate calculation from effects
- Push impurity to the edges (boundaries) of your program
- Passing state explicitly makes dependencies clear
- Immutability (creating new values) maintains purity
- Pure core with impure shell is a common pattern

---

## Exercise 13: Factorial with Comparison

**Task:** Implement factorial iteratively, recursively, and functionally

### Iterative Solution

```python
def factorial_iterative(n):
    result = 1
    for i in range(1, n + 1):
        result *= i
    return result

# Test
print(factorial_iterative(0))   # 1
print(factorial_iterative(5))   # 120
print(factorial_iterative(10))  # 3628800
```

```javascript
function factorialIterative(n) {
    let result = 1;
    for (let i = 1; i <= n; i++) {
        result *= i;
    }
    return result;
}

console.log(factorialIterative(5));  // 120
```

### Recursive Solution

```python
def factorial_recursive(n):
    if n <= 1:
        return 1
    return n * factorial_recursive(n - 1)

print(factorial_recursive(5))  # 120
```

```javascript
function factorialRecursive(n) {
    if (n <= 1) return 1;
    return n * factorialRecursive(n - 1);
}

console.log(factorialRecursive(5));  // 120
```

```haskell
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Or with guards
factorial' :: Int -> Int
factorial' n
    | n <= 1    = 1
    | otherwise = n * factorial' (n - 1)
```

### Functional (Using Reduce)

```python
from functools import reduce

def factorial_functional(n):
    if n == 0:
        return 1
    return reduce(lambda acc, x: acc * x, range(1, n + 1), 1)

print(factorial_functional(5))  # 120
```

```javascript
function factorialFunctional(n) {
    if (n === 0) return 1;
    return [...Array(n).keys()]
        .map(i => i + 1)
        .reduce((acc, x) => acc * x, 1);
}

console.log(factorialFunctional(5));  // 120
```

```haskell
-- Using fold
factorialFold :: Int -> Int
factorialFold n = foldl (*) 1 [1..n]

-- Using product
factorialProduct :: Int -> Int
factorialProduct n = product [1..n]
```

### Comparison

| Approach   | Pros | Cons |
|------------|------|------|
| Iterative  | Fast, efficient, clear state | More verbose, imperative style |
| Recursive  | Elegant, matches mathematical definition | Stack overflow risk for large n |
| Functional | Declarative, composable | May be less efficient, less familiar |

**Discussion Points:**
- **Clearest**: Subjective, but many find recursive most readable
- **Most efficient**: Iterative (no stack overhead)
- **Most elegant**: Recursive (matches mathematical definition)
- **Most functional**: Using product/reduce

---

## Exercise 14: Higher-Order Calculator

**Task:** Build calculator using higher-order functions

### Python Solution

```python
def calculator(operation_name):
    """Return function for given operation"""
    operations = {
        'add': lambda x, y: x + y,
        'subtract': lambda x, y: x - y,
        'multiply': lambda x, y: x * y,
        'divide': lambda x, y: x / y if y != 0 else None,
        'power': lambda x, y: x ** y,
        'modulo': lambda x, y: x % y,
        'max': lambda x, y: max(x, y),
        'min': lambda x, y: min(x, y)
    }
    return operations.get(operation_name)

# Usage
add_func = calculator('add')
print(add_func(5, 3))  # 8

multiply_func = calculator('multiply')
print(multiply_func(4, 7))  # 28

# Extension 1: More operations (already included above)
print(calculator('power')(2, 10))  # 1024

# Extension 2: Support chaining
def calculator_curried(operation_name):
    def get_operation(x):
        def apply(y):
            op = calculator(operation_name)
            return op(x, y) if op else None
        return apply
    return get_operation

print(calculator_curried('add')(5)(3))  # 8

# Extension 3: calculate helper
def calculate(operation_name, x, y):
    op = calculator(operation_name)
    return op(x, y) if op else None

print(calculate('add', 5, 3))  # 8
print(calculate('multiply', 4, 7))  # 28
```

### JavaScript Solution

```javascript
function calculator(operationName) {
    const operations = {
        add: (x, y) => x + y,
        subtract: (x, y) => x - y,
        multiply: (x, y) => x * y,
        divide: (x, y) => y !== 0 ? x / y : null,
        power: (x, y) => x ** y,
        modulo: (x, y) => x % y,
        max: (x, y) => Math.max(x, y),
        min: (x, y) => Math.min(x, y)
    };
    return operations[operationName];
}

// Usage
const addFunc = calculator('add');
console.log(addFunc(5, 3));  // 8

// Extension 2: Chaining
const calculatorCurried = operationName =>
    x => y => calculator(operationName)(x, y);

console.log(calculatorCurried('add')(5)(3));  // 8

// Extension 3: calculate helper
const calculate = (operationName, x, y) => {
    const op = calculator(operationName);
    return op ? op(x, y) : null;
};

console.log(calculate('multiply', 4, 7));  // 28
```

**Key Insights:**
- Functions stored in data structures (dictionary/object)
- Returns functions as values
- Demonstrates first-class functions
- Currying enables method chaining

---

## Exercise 15: Function Decorators

**Task:** Write decorators for logging, timing, retry, and caching

### Python: log_calls Decorator

```python
def log_calls(func):
    """Log function calls and results"""
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

### Extension 1: time_it Decorator

```python
import time

def time_it(func):
    """Measure execution time"""
    def wrapper(*args, **kwargs):
        start = time.time()
        result = func(*args, **kwargs)
        end = time.time()
        print(f"{func.__name__} took {end - start:.6f} seconds")
        return result
    return wrapper

@time_it
def slow_function():
    time.sleep(1)
    return "Done"

slow_function()
# Output: slow_function took 1.000123 seconds
```

### Extension 2: retry Decorator

```python
def retry(n):
    """Retry function n times on failure"""
    def decorator(func):
        def wrapper(*args, **kwargs):
            last_exception = None
            for attempt in range(n):
                try:
                    return func(*args, **kwargs)
                except Exception as e:
                    last_exception = e
                    print(f"Attempt {attempt + 1} failed: {e}")
            raise last_exception
        return wrapper
    return decorator

@retry(3)
def flaky_function():
    import random
    if random.random() < 0.7:
        raise ValueError("Random failure!")
    return "Success"

# Will retry up to 3 times
flaky_function()
```

### Extension 3: cache Decorator

```python
def cache(func):
    """Memoize function results"""
    cached_results = {}

    def wrapper(*args):
        if args not in cached_results:
            cached_results[args] = func(*args)
        return cached_results[args]

    return wrapper

@cache
def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)

print(fibonacci(100))  # Fast!
```

### JavaScript: Decorator Pattern

```javascript
// log_calls
function logCalls(func) {
    return function(...args) {
        console.log(`Calling ${func.name} with`, args);
        const result = func(...args);
        console.log(`${func.name} returned`, result);
        return result;
    };
}

const add = logCalls((x, y) => x + y);
add(5, 3);

// time_it
function timeIt(func) {
    return function(...args) {
        const start = Date.now();
        const result = func(...args);
        const end = Date.now();
        console.log(`${func.name} took ${end - start}ms`);
        return result;
    };
}

// cache
function cache(func) {
    const cached = new Map();
    return function(...args) {
        const key = JSON.stringify(args);
        if (!cached.has(key)) {
            cached.set(key, func(...args));
        }
        return cached.get(key);
    };
}
```

**Key Insights:**
- Decorators wrap functions to add behavior
- Don't modify original function
- Can be stacked: `@decorator1 @decorator2 def func()`
- Python has special syntax, JavaScript uses manual wrapping
- Powerful for cross-cutting concerns (logging, timing, caching)

---

## Challenge Projects

### Challenge 1: Build Your Own Reduce

```python
def my_reduce(func, lst, initial=None):
    """Full-featured reduce implementation"""
    if not lst and initial is None:
        raise TypeError("reduce() of empty sequence with no initial value")

    iterator = iter(lst)

    if initial is None:
        accumulator = next(iterator)
    else:
        accumulator = initial

    for item in iterator:
        accumulator = func(accumulator, item)

    return accumulator

# Test
add = lambda x, y: x + y
multiply = lambda x, y: x * y

print(my_reduce(add, [1, 2, 3, 4], 0))       # 10
print(my_reduce(multiply, [2, 3, 4], 1))     # 24
print(my_reduce(add, [1, 2, 3, 4]))          # 10 (no initial)
```

### Challenge 2: Point-Free Style

```haskell
-- Haskell: Point-free style
sumOfSquaresOfEvens :: [Int] -> Int
sumOfSquaresOfEvens = sum . map (^2) . filter even

-- Or with explicit composition
sumOfSquaresOfEvens' :: [Int] -> Int
sumOfSquaresOfEvens' = (sum .) . (map (^2) .) . filter even
```

```python
# Python: Approximating point-free style
from functools import reduce

# With explicit parameter
def sum_of_squares_of_evens(numbers):
    return sum(map(lambda x: x * x, filter(lambda x: x % 2 == 0, numbers)))

# More point-free (using composition)
def compose(*functions):
    def composed(x):
        for f in reversed(functions):
            x = f(x)
        return x
    return composed

is_even = lambda x: x % 2 == 0
square = lambda x: x * x

sum_of_squares_of_evens_pf = compose(
    sum,
    lambda nums: map(square, nums),
    lambda nums: filter(is_even, nums)
)

print(sum_of_squares_of_evens_pf([1, 2, 3, 4, 5, 6]))  # 56
```

### Challenge 3: Y Combinator

```python
# Y Combinator (fixed-point combinator)
Y = lambda f: (lambda x: f(lambda v: x(x)(v)))(lambda x: f(lambda v: x(x)(v)))

# Factorial using Y combinator
factorial = Y(lambda f: lambda n: 1 if n == 0 else n * f(n - 1))

print(factorial(5))  # 120

# Explanation:
# Y combinator allows recursion without self-reference
# It finds the "fixed point" of a function
# The function passed to Y takes a "recurse" parameter instead of calling itself
```

```javascript
// Y Combinator in JavaScript
const Y = f => (x => f(v => x(x)(v)))(x => f(v => x(x)(v)));

// Factorial
const factorial = Y(f => n => n === 0 ? 1 : n * f(n - 1));

console.log(factorial(5));  // 120

// Fibonacci
const fibonacci = Y(f => n => n <= 1 ? n : f(n - 1) + f(n - 2));

console.log(fibonacci(10));  // 55
```

**Key Insights:**
- Y combinator enables recursion without explicit self-reference
- Shows deep connection between functions and recursion
- Demonstrates power of higher-order functions
- Foundational concept in lambda calculus

---

## Reflection Questions: Answers

1. **How do closures enable encapsulation without classes?**
   - Closures capture private variables from outer scope
   - Only the returned function has access to these variables
   - Provides data hiding and state management
   - Example: `make_counter()` creates private `count` variable

2. **What are the advantages of pure functions for testing and reasoning?**
   - No setup/teardown needed for tests
   - Same inputs always produce same outputs (deterministic)
   - Can test in isolation without mocks
   - Easy to reason about behavior
   - Safe for parallel execution

3. **When is currying useful in practice?**
   - Creating specialized functions from general ones
   - Partial application for configuration
   - Building function pipelines
   - Event handlers with pre-configured data
   - Haskell: natural function composition

4. **How does Haskell's automatic currying compare to manual currying?**
   - Haskell: All functions are curried by default
   - JavaScript/Python: Must manually curry
   - Haskell: Partial application is natural syntax
   - Manual: More verbose but more explicit
   - Haskell approach enables more concise code

5. **What's the relationship between map/filter/reduce and for loops?**
   - Map = loop that transforms each element
   - Filter = loop that selects elements
   - Reduce = loop that accumulates a result
   - Higher-order functions are more declarative
   - Express *what* not *how*

6. **Why is function composition powerful?**
   - Build complex behavior from simple parts
   - Each function does one thing well
   - Reusable components
   - Easy to test individual pieces
   - Matches mathematical thinking

7. **How do different languages handle closures differently?**
   - JavaScript/Python: Natural mutable closures
   - Haskell: Pure closures, requires IO/State for mutation
   - C: No closures (only function pointers)
   - Ruby: Closures with blocks and lambdas
   - Each reflects language's philosophy

---

## Summary

Functions are the foundation of abstraction in programming:

- **Pure functions** are predictable and testable
- **First-class functions** enable powerful abstractions
- **Closures** capture environment for stateful behavior
- **Higher-order functions** (map/filter/reduce) transform data
- **Composition** builds complexity from simplicity
- **Currying and partial application** create specialized functions
- **Decorators** add behavior without modification

Different paradigms approach functions differently:
- **Procedural**: Functions execute steps
- **OOP**: Functions are methods on objects
- **Functional**: Functions are mathematical transformations
- **Multi-paradigm**: Flexible combination

Mastering functions across paradigms makes you a stronger programmer in any language!
