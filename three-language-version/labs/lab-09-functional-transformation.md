# Lab 9: Functional Transformation

**Quarter 1, Week 9**
**Duration:** 90 minutes
**Format:** Pair programming

## Overview

Higher-order functions are functions that take other functions as arguments or return functions as results. They're the backbone of functional programming and increasingly common in all languages. This lab will transform how you think about data processing.

## Objectives

By the end of this lab, you will:
- [ ] Use map, filter, and reduce fluently
- [ ] Write lambda expressions
- [ ] Chain operations together
- [ ] Appreciate why functional style can be cleaner

## Setup

- Partner up
- Create folder: `lab09-functional/`
- Files: `transform.py`, `transform.cpp`, `transform.hs`

---

## Part 1: Map - Transform Every Element (20 minutes)

### Activity 1.1: The Problem with Loops

**Traditional approach:**
```python
# Double every number in a list
numbers = [1, 2, 3, 4, 5]
doubled = []
for n in numbers:
    doubled.append(n * 2)
print(doubled)  # [2, 4, 6, 8, 10]
```

**Functional approach:**
```python
numbers = [1, 2, 3, 4, 5]
doubled = list(map(lambda x: x * 2, numbers))
print(doubled)  # [2, 4, 6, 8, 10]

# Or with list comprehension (Pythonic)
doubled = [x * 2 for x in numbers]
```

**The key insight:** We separated "what to do" (double) from "how to iterate" (map handles it).

### Activity 1.2: Map in All Languages

**Python:**
```python
# Using map()
numbers = [1, 2, 3, 4, 5]

# With a regular function
def square(x):
    return x * x

squared = list(map(square, numbers))
print(squared)  # [1, 4, 9, 16, 25]

# With a lambda
squared = list(map(lambda x: x * x, numbers))

# With list comprehension
squared = [x * x for x in numbers]
```

**C++ (using `<algorithm>` and lambdas):**
```cpp
#include <iostream>
#include <vector>
#include <algorithm>
using namespace std;

int main() {
    vector<int> numbers = {1, 2, 3, 4, 5};
    vector<int> squared(numbers.size());

    // transform is C++'s map
    transform(numbers.begin(), numbers.end(), squared.begin(),
              [](int x) { return x * x; });  // lambda

    for (int n : squared) {
        cout << n << " ";  // 1 4 9 16 25
    }
    cout << endl;
    return 0;
}
```

**Haskell (map is native!):**
```haskell
main :: IO ()
main = do
    let numbers = [1, 2, 3, 4, 5]

    -- Using map
    print (map (\x -> x * x) numbers)  -- [1,4,9,16,25]

    -- Even cleaner: map with function
    print (map (^2) numbers)           -- [1,4,9,16,25]

    -- Or with list comprehension
    print [x^2 | x <- numbers]         -- [1,4,9,16,25]
```

### Activity 1.3: Practice Map

**Exercise:** Transform these lists using map:

```python
names = ["alice", "bob", "charlie"]
# TODO: Capitalize each name -> ["Alice", "Bob", "Charlie"]

prices = [10.99, 24.50, 8.75]
# TODO: Add 20% tax -> [13.188, 29.4, 10.5]

words = ["hello", "world", "python"]
# TODO: Get lengths -> [5, 5, 6]
```

### ✅ Checkpoint 1

Verify with partner:
- [ ] Understand how map transforms each element
- [ ] Can write lambdas in at least Python
- [ ] Completed map exercises

---

## Part 2: Filter - Keep What Matters (20 minutes)

### Activity 2.1: Filter Basics

**Traditional:**
```python
numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
evens = []
for n in numbers:
    if n % 2 == 0:
        evens.append(n)
print(evens)  # [2, 4, 6, 8, 10]
```

**Functional:**
```python
numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
evens = list(filter(lambda x: x % 2 == 0, numbers))
print(evens)  # [2, 4, 6, 8, 10]

# List comprehension with condition
evens = [x for x in numbers if x % 2 == 0]
```

### Activity 2.2: Filter in All Languages

**Python:**
```python
words = ["apple", "banana", "cherry", "date", "elderberry"]

# Keep words longer than 5 characters
long_words = list(filter(lambda w: len(w) > 5, words))
print(long_words)  # ['banana', 'cherry', 'elderberry']

# Keep words starting with vowels
def starts_with_vowel(word):
    return word[0].lower() in 'aeiou'

vowel_words = list(filter(starts_with_vowel, words))
print(vowel_words)  # ['apple', 'elderberry']
```

**C++:**
```cpp
#include <iostream>
#include <vector>
#include <algorithm>
using namespace std;

int main() {
    vector<int> numbers = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    vector<int> evens;

    // copy_if is C++'s filter
    copy_if(numbers.begin(), numbers.end(), back_inserter(evens),
            [](int x) { return x % 2 == 0; });

    for (int n : evens) {
        cout << n << " ";  // 2 4 6 8 10
    }
    cout << endl;
    return 0;
}
```

**Haskell:**
```haskell
main :: IO ()
main = do
    let numbers = [1..10]

    -- Using filter
    print (filter even numbers)         -- [2,4,6,8,10]
    print (filter (> 5) numbers)        -- [6,7,8,9,10]

    -- List comprehension with guard
    print [x | x <- numbers, even x]    -- [2,4,6,8,10]
```

### Activity 2.3: Practice Filter

**Exercise:** Filter these lists:

```python
numbers = list(range(1, 21))  # 1 to 20
# TODO: Keep only multiples of 3 -> [3, 6, 9, 12, 15, 18]
# TODO: Keep numbers greater than 10 and less than 15 -> [11, 12, 13, 14]

students = [
    {"name": "Alice", "grade": 85},
    {"name": "Bob", "grade": 72},
    {"name": "Charlie", "grade": 90},
    {"name": "Diana", "grade": 68}
]
# TODO: Keep students with grade >= 80 -> [Alice, Charlie]
```

### ✅ Checkpoint 2

Verify:
- [ ] Understand how filter keeps matching elements
- [ ] Completed filter exercises in Python

---

## Part 3: Reduce - Combine Into One (20 minutes)

### Activity 3.1: Reduce Basics

**Traditional:**
```python
numbers = [1, 2, 3, 4, 5]
total = 0
for n in numbers:
    total += n
print(total)  # 15
```

**Functional:**
```python
from functools import reduce

numbers = [1, 2, 3, 4, 5]
total = reduce(lambda acc, x: acc + x, numbers, 0)
print(total)  # 15
```

**How reduce works:**
```
Initial: acc = 0
Step 1: acc = 0 + 1 = 1
Step 2: acc = 1 + 2 = 3
Step 3: acc = 3 + 3 = 6
Step 4: acc = 6 + 4 = 10
Step 5: acc = 10 + 5 = 15
Result: 15
```

### Activity 3.2: Reduce Examples

**Python:**
```python
from functools import reduce

numbers = [1, 2, 3, 4, 5]

# Sum
total = reduce(lambda acc, x: acc + x, numbers, 0)
print(f"Sum: {total}")  # 15

# Product
product = reduce(lambda acc, x: acc * x, numbers, 1)
print(f"Product: {product}")  # 120

# Max (without using max())
maximum = reduce(lambda acc, x: x if x > acc else acc, numbers)
print(f"Max: {maximum}")  # 5

# Join strings
words = ["Hello", "World", "Python"]
sentence = reduce(lambda acc, w: acc + " " + w, words)
print(sentence)  # "Hello World Python"

# Count occurrences
items = ["a", "b", "a", "c", "a", "b"]
counts = reduce(
    lambda acc, x: {**acc, x: acc.get(x, 0) + 1},
    items,
    {}
)
print(counts)  # {'a': 3, 'b': 2, 'c': 1}
```

**Haskell (foldl and foldr):**
```haskell
main :: IO ()
main = do
    let numbers = [1, 2, 3, 4, 5]

    -- foldl is "fold left" (like reduce)
    print (foldl (+) 0 numbers)     -- 15 (sum)
    print (foldl (*) 1 numbers)     -- 120 (product)
    print (foldl max 0 numbers)     -- 5 (maximum)

    -- foldr is "fold right" (processes from end)
    print (foldr (+) 0 numbers)     -- 15 (same for + )

    -- Haskell's sum is built-in
    print (sum numbers)             -- 15
```

### Activity 3.3: Practice Reduce

**Exercise:**

```python
from functools import reduce

numbers = [3, 1, 4, 1, 5, 9, 2, 6]
# TODO: Find minimum using reduce

words = ["apple", "banana", "cherry"]
# TODO: Find longest word using reduce

grades = [85, 90, 78, 92, 88]
# TODO: Calculate average using reduce (hint: reduce to sum, then divide)
```

### ✅ Checkpoint 3

Verify:
- [ ] Understand how reduce combines a list into one value
- [ ] Completed at least one reduce exercise

---

## Part 4: Chaining Operations (15 minutes)

### Activity 4.1: The Power of Composition

The real power comes from chaining map, filter, and reduce together!

**Problem:** Given a list of products, find the total price of all products on sale (> 20% off).

```python
from functools import reduce

products = [
    {"name": "Widget", "price": 25.00, "discount": 0.10},
    {"name": "Gadget", "price": 50.00, "discount": 0.25},
    {"name": "Gizmo", "price": 30.00, "discount": 0.30},
    {"name": "Thing", "price": 15.00, "discount": 0.05},
    {"name": "Stuff", "price": 40.00, "discount": 0.20},
]

# Chain: filter -> map -> reduce
total = reduce(
    lambda acc, p: acc + p,                    # reduce: sum prices
    map(
        lambda p: p["price"] * (1 - p["discount"]),  # map: calculate sale price
        filter(
            lambda p: p["discount"] > 0.20,    # filter: only big discounts
            products
        )
    ),
    0  # initial value
)

print(f"Total for items >20% off: ${total:.2f}")  # $58.50
```

**More readable version:**

```python
# Step by step
on_sale = filter(lambda p: p["discount"] > 0.20, products)
sale_prices = map(lambda p: p["price"] * (1 - p["discount"]), on_sale)
total = reduce(lambda acc, p: acc + p, sale_prices, 0)

# Or with list comprehension
total = sum(
    p["price"] * (1 - p["discount"])
    for p in products
    if p["discount"] > 0.20
)
```

### Activity 4.2: Haskell's Elegance

```haskell
data Product = Product { name :: String, price :: Double, discount :: Double }

products :: [Product]
products = [
    Product "Widget" 25.00 0.10,
    Product "Gadget" 50.00 0.25,
    Product "Gizmo"  30.00 0.30,
    Product "Thing"  15.00 0.05,
    Product "Stuff"  40.00 0.20
    ]

-- Chained with function composition (.)
salePrice :: Product -> Double
salePrice p = price p * (1 - discount p)

total :: Double
total = sum                        -- reduce with (+)
      . map salePrice              -- transform to prices
      . filter (\p -> discount p > 0.20)  -- filter big discounts
      $ products

main :: IO ()
main = print total  -- 58.5
```

### Activity 4.3: Your Turn - Data Pipeline

**Exercise:** Process student data:

```python
students = [
    {"name": "Alice", "scores": [85, 90, 88]},
    {"name": "Bob", "scores": [72, 68, 75]},
    {"name": "Charlie", "scores": [90, 95, 92]},
    {"name": "Diana", "scores": [65, 70, 68]},
]

# TODO: Create a pipeline that:
# 1. Calculates average score for each student
# 2. Filters to students with average >= 80
# 3. Gets just their names
# 4. Result: ["Alice", "Charlie"]
```

### ✅ Checkpoint 4

Verify:
- [ ] Successfully chained at least 2 operations together
- [ ] Can explain the data flow through the pipeline

---

## Part 5: When to Use What? (10 minutes)

### Activity 5.1: Decision Guide

| Task | Best Tool | Example |
|------|-----------|---------|
| Transform each element | **map** | Square numbers, uppercase strings |
| Keep some elements | **filter** | Even numbers, passing grades |
| Combine into one value | **reduce** | Sum, product, find max |
| Transform & filter | **list comprehension** | `[x*2 for x in nums if x > 0]` |

### Activity 5.2: Refactoring Exercise

Convert these imperative loops to functional style:

```python
# Exercise 1: Convert to map
result = []
for name in names:
    result.append(name.upper())

# Exercise 2: Convert to filter
result = []
for num in numbers:
    if num > 0:
        result.append(num)

# Exercise 3: Convert to map + filter (or list comprehension)
result = []
for word in words:
    if len(word) > 3:
        result.append(word.upper())

# Exercise 4: Convert to reduce
total = 0
for price in prices:
    total += price
```

### Activity 5.3: When NOT to Use Functional Style

Sometimes loops are clearer:

```python
# Hard to read with reduce
def process_with_side_effects(items):
    for item in items:
        print(f"Processing {item}")
        database.save(item)
        send_notification(item)

# When you need to break early
def find_first_match(items, predicate):
    for item in items:
        if predicate(item):
            return item
    return None
```

**Rule of thumb:** If the functional version is harder to read, use a loop!

---

## Challenges

### Challenge 1: Word Frequency Counter

```python
text = """
the quick brown fox jumps over the lazy dog
the dog was not amused by the fox
"""

# Count word frequencies using functional style
# Result: {"the": 4, "dog": 2, "fox": 2, ...}
```

### Challenge 2: Implement Your Own

Implement `my_map`, `my_filter`, and `my_reduce` without using the built-ins:

```python
def my_map(func, lst):
    # Your implementation using only recursion or basic loops
    pass

def my_filter(predicate, lst):
    pass

def my_reduce(func, lst, initial):
    pass
```

### Challenge 3: Parallel Processing Preview

```python
from multiprocessing import Pool

def square(x):
    return x * x

numbers = list(range(1000000))

# map() is sequential
# Pool.map() can run in parallel!
with Pool(4) as p:
    result = p.map(square, numbers)
```

**Discussion:** Why does functional style make parallel processing easier?

---

## Wrap-Up

**Key takeaways:**

1. **map** transforms every element
2. **filter** keeps elements matching a condition
3. **reduce** combines elements into one value
4. **Chaining** creates powerful data pipelines
5. **Lambdas** are small anonymous functions
6. **Haskell** treats these as first-class citizens
7. **Use functional style** when it makes code clearer

**Mental model:** Think of data flowing through a pipeline, being transformed at each step.

**Next lab:** We'll learn to test our code properly with Test-First Development!
