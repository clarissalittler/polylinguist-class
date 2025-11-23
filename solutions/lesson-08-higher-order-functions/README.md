# Lesson 8: Higher-Order Functions - Solution Guide

This guide provides example solutions for the Higher-Order Functions exercises.

## General Notes

- **Higher-order functions**: Functions that take functions as arguments or return functions
- **Core trio**: map, filter, reduce are the foundation of functional programming
- **Composition**: Building complex behavior from simple functions
- **Closures**: Functions that capture their environment
- **Language differences**: Some languages are more functional than others

---

## Exercise 1: Basic Map

**Task:** Use map to transform lists

### Python Solution

```python
numbers = [1, 2, 3, 4, 5]

# 1. Double each number
doubled = list(map(lambda x: x * 2, numbers))
print(doubled)  # [2, 4, 6, 8, 10]

# 2. Square each number
squared = list(map(lambda x: x ** 2, numbers))
print(squared)  # [1, 4, 9, 16, 25]

# 3. Convert to strings
strings = list(map(str, numbers))
print(strings)  # ['1', '2', '3', '4', '5']

# Using list comprehensions (more Pythonic)
doubled_lc = [x * 2 for x in numbers]
squared_lc = [x ** 2 for x in numbers]
strings_lc = [str(x) for x in numbers]
```

### JavaScript Solution

```javascript
const numbers = [1, 2, 3, 4, 5];

// 1. Double each number
const doubled = numbers.map(x => x * 2);
console.log(doubled);  // [2, 4, 6, 8, 10]

// 2. Square each number
const squared = numbers.map(x => x ** 2);
console.log(squared);  // [1, 4, 9, 16, 25]

// 3. Convert to strings
const strings = numbers.map(String);
// Or: const strings = numbers.map(x => String(x));
console.log(strings);  // ['1', '2', '3', '4', '5']

// Using for loop (less elegant)
const doubledLoop = [];
for (const num of numbers) {
    doubledLoop.push(num * 2);
}
```

### Haskell Solution

```haskell
numbers :: [Int]
numbers = [1, 2, 3, 4, 5]

-- 1. Double each number
doubled :: [Int]
doubled = map (*2) numbers  -- [2,4,6,8,10]

-- 2. Square each number
squared :: [Int]
squared = map (^2) numbers  -- [1,4,9,16,25]

-- 3. Convert to strings
strings :: [String]
strings = map show numbers  -- ["1","2","3","4","5"]

-- Using list comprehensions
doubled' = [x * 2 | x <- numbers]
squared' = [x ^ 2 | x <- numbers]
```

**Key Insights:**
- Map transforms each element independently
- More declarative than loops
- Python needs `list()` wrapper for map
- Haskell naturally works with lazy sequences

---

## Exercise 2: Filter Practice

**Task:** Filter lists based on predicates

### Python Solution

```python
numbers = [1, -5, 3, -2, 8, -9, 4, 6, -1, 10]

# 1. Only positive numbers
positive = list(filter(lambda x: x > 0, numbers))
print(positive)  # [1, 3, 8, 4, 6, 10]

# 2. Only even numbers
even = list(filter(lambda x: x % 2 == 0, numbers))
print(even)  # [-2, 8, -9, 4, 6, 10] - wait, -9 shouldn't be here!
# Actually: [-2, 8, 4, 6, 10]

# 3. Numbers greater than 5
greater_than_5 = list(filter(lambda x: x > 5, numbers))
print(greater_than_5)  # [8, 6, 10]

# 4. Positive AND even
positive_even = list(filter(lambda x: x > 0 and x % 2 == 0, numbers))
print(positive_even)  # [8, 4, 6, 10]

# Using list comprehensions
positive_lc = [x for x in numbers if x > 0]
even_lc = [x for x in numbers if x % 2 == 0]
greater_5_lc = [x for x in numbers if x > 5]
pos_even_lc = [x for x in numbers if x > 0 and x % 2 == 0]
```

### JavaScript Solution

```javascript
const numbers = [1, -5, 3, -2, 8, -9, 4, 6, -1, 10];

// 1. Only positive
const positive = numbers.filter(x => x > 0);
console.log(positive);  // [1, 3, 8, 4, 6, 10]

// 2. Only even
const even = numbers.filter(x => x % 2 === 0);
console.log(even);  // [-2, 8, 4, 6, 10]

// 3. Greater than 5
const greaterThan5 = numbers.filter(x => x > 5);
console.log(greaterThan5);  // [8, 6, 10]

// 4. Positive AND even
const positiveEven = numbers.filter(x => x > 0 && x % 2 === 0);
console.log(positiveEven);  // [8, 4, 6, 10]
```

### Ruby Solution

```ruby
numbers = [1, -5, 3, -2, 8, -9, 4, 6, -1, 10]

# 1. Only positive
positive = numbers.select { |x| x > 0 }
puts positive.inspect  # [1, 3, 8, 4, 6, 10]

# 2. Only even
even = numbers.select { |x| x.even? }
# Or: even = numbers.select { |x| x % 2 == 0 }
puts even.inspect  # [-2, 8, 4, 6, 10]

# 3. Greater than 5
greater_than_5 = numbers.select { |x| x > 5 }
puts greater_than_5.inspect  # [8, 6, 10]

# 4. Positive AND even
positive_even = numbers.select { |x| x > 0 && x.even? }
puts positive_even.inspect  # [8, 4, 6, 10]
```

**Key Insights:**
- Filter selects elements matching a predicate
- Combine conditions with `and`/`&&` for multiple criteria
- Ruby uses `select` instead of `filter`
- More readable than loops with conditions

---

## Exercise 3: Reduce/Fold

**Task:** Use reduce to aggregate values

### Python Solution

```python
from functools import reduce

numbers = [1, 2, 3, 4, 5]

# 1. Sum
total = reduce(lambda acc, x: acc + x, numbers, 0)
print(f"Sum: {total}")  # 15

# Built-in is clearer for sum
total_builtin = sum(numbers)

# 2. Product
product = reduce(lambda acc, x: acc * x, numbers, 1)
print(f"Product: {product}")  # 120

# 3. Maximum
maximum = reduce(lambda acc, x: max(acc, x), numbers)
# Or just: maximum = max(numbers)
print(f"Maximum: {maximum}")  # 5

# 4. Build string
string = reduce(lambda acc, x: f"{acc}, {x}" if acc else str(x), numbers, "")
print(f"String: {string}")  # "1, 2, 3, 4, 5"

# Better string version
string_better = ", ".join(map(str, numbers))
print(string_better)  # "1, 2, 3, 4, 5"

# Custom reduce implementation
def my_reduce(func, iterable, initial=None):
    it = iter(iterable)
    if initial is None:
        value = next(it)
    else:
        value = initial
    for element in it:
        value = func(value, element)
    return value

# Test custom reduce
print(my_reduce(lambda a, b: a + b, numbers, 0))  # 15
```

### JavaScript Solution

```javascript
const numbers = [1, 2, 3, 4, 5];

// 1. Sum
const sum = numbers.reduce((acc, x) => acc + x, 0);
console.log(`Sum: ${sum}`);  // 15

// 2. Product
const product = numbers.reduce((acc, x) => acc * x, 1);
console.log(`Product: ${product}`);  // 120

// 3. Maximum
const maximum = numbers.reduce((acc, x) => Math.max(acc, x));
// Or: const maximum = Math.max(...numbers);
console.log(`Maximum: ${maximum}`);  // 5

// 4. Build string
const string = numbers.reduce((acc, x, i) =>
    i === 0 ? String(x) : `${acc}, ${x}`, "");
console.log(`String: ${string}`);  // "1, 2, 3, 4, 5"

// Better: use join
const stringBetter = numbers.join(", ");

// Custom reduce
function myReduce(func, array, initial) {
    let acc = initial !== undefined ? initial : array[0];
    let start = initial !== undefined ? 0 : 1;

    for (let i = start; i < array.length; i++) {
        acc = func(acc, array[i]);
    }
    return acc;
}

console.log(myReduce((a, b) => a + b, numbers, 0));  // 15
```

### Haskell Solution

```haskell
numbers :: [Int]
numbers = [1, 2, 3, 4, 5]

-- 1. Sum (using foldl)
total :: Int
total = foldl (+) 0 numbers  -- 15

-- Or use built-in sum
total' = sum numbers

-- 2. Product
product' :: Int
product' = foldl (*) 1 numbers  -- 120

-- Or use built-in product
product'' = product numbers

-- 3. Maximum
maximum' :: Int
maximum' = foldl max (head numbers) (tail numbers)
-- Or: maximum' = maximum numbers

-- 4. Build string (conceptually)
-- Haskell strings are [Char], so this is different
buildString :: [Int] -> String
buildString [] = ""
buildString [x] = show x
buildString (x:xs) = show x ++ ", " ++ buildString xs

main :: IO ()
main = do
    print total      -- 15
    print product'   -- 120
    print maximum'   -- 5
    putStrLn $ buildString numbers  -- "1, 2, 3, 4, 5"
```

**Key Insights:**
- Reduce combines all elements into single value
- Accumulator pattern: start with initial, update with each element
- Left fold vs right fold matters for order
- Many common operations (sum, product) have built-ins

---

## Exercise 4: Function Pipeline

**Task:** Create data processing pipeline

### Python Solution

```python
numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

# Separate steps
evens = [x for x in numbers if x % 2 == 0]
squared = [x ** 2 for x in evens]
result = sum(squared)
print(f"Result: {result}")  # 220

# Single chain (using functional style)
result_chain = sum(
    map(lambda x: x ** 2,
        filter(lambda x: x % 2 == 0, numbers)))
print(f"Result (chain): {result_chain}")  # 220

# Single chain (using list comprehension)
result_lc = sum(x ** 2 for x in numbers if x % 2 == 0)
print(f"Result (comprehension): {result_lc}")  # 220

# Step by step explanation
# Start: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
# After filter (evens): [2, 4, 6, 8, 10]
# After map (squared): [4, 16, 36, 64, 100]
# After reduce (sum): 220
```

### JavaScript Solution

```javascript
const numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

// Separate steps
const evens = numbers.filter(x => x % 2 === 0);
const squared = evens.map(x => x ** 2);
const result = squared.reduce((acc, x) => acc + x, 0);
console.log(`Result: ${result}`);  // 220

// Single chain (method chaining)
const resultChain = numbers
    .filter(x => x % 2 === 0)
    .map(x => x ** 2)
    .reduce((acc, x) => acc + x, 0);
console.log(`Result (chain): ${resultChain}`);  // 220

// Explanation of data flow:
// [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
//   .filter(even) → [2, 4, 6, 8, 10]
//   .map(square) → [4, 16, 36, 64, 100]
//   .reduce(sum) → 220
```

### Haskell Solution

```haskell
numbers :: [Int]
numbers = [1..10]

-- Separate steps
evens :: [Int]
evens = filter even numbers

squared :: [Int]
squared = map (^2) evens

result :: Int
result = sum squared

-- Single chain (function composition)
result' :: Int
result' = sum . map (^2) . filter even $ numbers

-- Or using $ operator for clarity
result'' :: Int
result'' = sum $ map (^2) $ filter even numbers

main :: IO ()
main = print result'  -- 220
```

**Key Insights:**
- Pipelines compose multiple transformations
- Each step is independent and testable
- Method chaining (JavaScript) vs function composition (Haskell)
- More readable than nested loops

---

## Exercise 5: Custom Map/Filter/Reduce

**Task:** Implement map/filter/reduce from scratch

### Python Solution

```python
def my_map(func, lst):
    """Apply function to each element"""
    result = []
    for item in lst:
        result.append(func(item))
    return result

def my_filter(predicate, lst):
    """Keep elements where predicate is True"""
    result = []
    for item in lst:
        if predicate(item):
            result.append(item)
    return result

def my_reduce(func, lst, initial):
    """Combine elements with binary function"""
    acc = initial
    for item in lst:
        acc = func(acc, item)
    return acc

# Test
print(my_map(lambda x: x * 2, [1, 2, 3]))           # [2, 4, 6]
print(my_filter(lambda x: x > 2, [1, 2, 3, 4]))     # [3, 4]
print(my_reduce(lambda a, b: a + b, [1, 2, 3], 0)) # 6

# Recursive implementations
def my_map_rec(func, lst):
    if not lst:
        return []
    return [func(lst[0])] + my_map_rec(func, lst[1:])

def my_filter_rec(predicate, lst):
    if not lst:
        return []
    if predicate(lst[0]):
        return [lst[0]] + my_filter_rec(predicate, lst[1:])
    return my_filter_rec(predicate, lst[1:])

def my_reduce_rec(func, lst, initial):
    if not lst:
        return initial
    return my_reduce_rec(func, lst[1:], func(initial, lst[0]))
```

### JavaScript Solution

```javascript
function myMap(func, array) {
    const result = [];
    for (const item of array) {
        result.push(func(item));
    }
    return result;
}

function myFilter(predicate, array) {
    const result = [];
    for (const item of array) {
        if (predicate(item)) {
            result.push(item);
        }
    }
    return result;
}

function myReduce(func, array, initial) {
    let acc = initial;
    for (const item of array) {
        acc = func(acc, item);
    }
    return acc;
}

// Test
console.log(myMap(x => x * 2, [1, 2, 3]));           // [2, 4, 6]
console.log(myFilter(x => x > 2, [1, 2, 3, 4]));     // [3, 4]
console.log(myReduce((a, b) => a + b, [1, 2, 3], 0)); // 6
```

### Haskell Solution

```haskell
-- Map implementation
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

-- Filter implementation
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter pred (x:xs)
    | pred x    = x : myFilter pred xs
    | otherwise = myFilter pred xs

-- Reduce (foldl) implementation
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

-- Test
main :: IO ()
main = do
    print $ myMap (*2) [1, 2, 3]           -- [2,4,6]
    print $ myFilter (>2) [1, 2, 3, 4]     -- [3,4]
    print $ myFoldl (+) 0 [1, 2, 3]        -- 6
```

**Key Insights:**
- Map/filter/reduce are simple patterns
- Iterative: use loops with accumulator
- Recursive: pattern match on list structure
- Understanding implementation aids usage

---

## Exercise 6: Closure Counter

**Task:** Create counter with closure

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
print(counter2())  # 1  (independent!)
print(counter1())  # 3
```

### JavaScript Solution

```javascript
function makeCounter() {
    let count = 0;  // Private variable

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

**Key Insights:**
- Closures capture surrounding scope
- Each closure has independent state
- Enables private variables without classes
- Fundamental pattern in JavaScript

---

## Exercise 9: Array Processing Challenge

**Task:** Complex pipeline with user objects

### JavaScript Solution

```javascript
const users = [
    { name: 'Alice', age: 25, active: true, score: 85 },
    { name: 'Bob', age: 17, active: false, score: 92 },
    { name: 'Charlie', age: 30, active: true, score: 78 },
    { name: 'Diana', age: 22, active: true, score: 95 },
    { name: 'Eve', age: 19, active: true, score: 88 }
];

const result = users
    .filter(user => user.active)                    // Only active users
    .filter(user => user.age >= 18)                 // 18 or older
    .map(user => ({ name: user.name, score: user.score }))  // Extract name and score
    .sort((a, b) => b.score - a.score)              // Sort by score descending
    .slice(0, 3);                                   // Top 3

console.log(result);
// [
//   { name: 'Diana', score: 95 },
//   { name: 'Eve', score: 88 },
//   { name: 'Alice', score: 85 }
// ]
```

### Python Solution

```python
users = [
    {'name': 'Alice', 'age': 25, 'active': True, 'score': 85},
    {'name': 'Bob', 'age': 17, 'active': False, 'score': 92},
    {'name': 'Charlie', 'age': 30, 'active': True, 'score': 78},
    {'name': 'Diana', 'age': 22, 'active': True, 'score': 95},
    {'name': 'Eve', 'age': 19, 'active': True, 'score': 88}
]

result = (
    # Can't chain in Python, so use nested operations
    sorted(
        [{'name': u['name'], 'score': u['score']}
         for u in users
         if u['active'] and u['age'] >= 18],
        key=lambda u: u['score'],
        reverse=True
    )[:3]
)

print(result)
# [{'name': 'Diana', 'score': 95},
#  {'name': 'Eve', 'score': 88},
#  {'name': 'Alice', 'score': 85}]
```

**Key Insights:**
- Chains of transformations are powerful
- Each step is simple and clear
- Order matters: filter before map to reduce work
- JavaScript's method chaining is elegant for this

---

## Summary

Higher-order functions are the cornerstone of functional programming:

**Core Concepts:**
- **Map**: Transform each element `[a] -> [b]`
- **Filter**: Select elements `[a] -> [a]`
- **Reduce**: Combine elements `[a] -> b`
- **Closures**: Functions capturing environment
- **Composition**: Combine simple functions into complex ones

**Benefits:**
- More declarative than loops
- Easier to reason about
- Composable and reusable
- Less error-prone (no loop counters)
- Natural parallelization

**When to Use:**
- Data transformations
- Pipelines and workflows
- Event handlers and callbacks
- State management without classes

**Language Differences:**
- **JavaScript**: First-class functions, method chaining
- **Python**: Functional features, but list comprehensions often preferred
- **Haskell**: Everything is a higher-order function
- **Ruby**: Blocks and procs make it natural

Master these patterns and you'll write cleaner, more maintainable code!
