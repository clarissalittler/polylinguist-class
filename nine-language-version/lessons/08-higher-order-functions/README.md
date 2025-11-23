# Lesson 8: Higher-Order Functions

## Overview

Higher-order functions are functions that can:
1. **Take other functions as arguments**, or
2. **Return functions as results**, or
3. **Both!**

This powerful concept enables:
- Code reuse and abstraction
- Function composition
- Declarative programming
- Building domain-specific languages

## Learning Objectives

By the end of this lesson, you will:
- Understand what makes a function "higher-order"
- Use built-in higher-order functions (map, filter, reduce)
- Create your own higher-order functions
- Understand closures and lexical scope
- Use partial application and currying
- Compose functions for elegant solutions
- Compare imperative vs functional approaches

## What is a Higher-Order Function?

A **higher-order function** treats functions as first-class values:

```python
# Takes a function as argument
def apply_twice(func, x):
    return func(func(x))

def add_one(x):
    return x + 1

result = apply_twice(add_one, 5)  # 7
```

```python
# Returns a function
def make_multiplier(n):
    def multiplier(x):
        return x * n
    return multiplier

times_three = make_multiplier(3)
result = times_three(10)  # 30
```

## First-Class Functions

For higher-order functions to work, functions must be **first-class citizens**:

- **Assign to variables:** `f = some_function`
- **Pass as arguments:** `map(f, list)`
- **Return from functions:** `return lambda x: x * 2`
- **Store in data structures:** `funcs = [f1, f2, f3]`

**Languages with first-class functions:**
- Python, JavaScript, Ruby, Rust, Haskell, Racket ✓
- Java (since Java 8), C (function pointers, limited) ~
- C (pre-modern), Prolog (different paradigm) ✗ (sort of)

## The Classic Trio: Map, Filter, Reduce

### Map - Transform Each Element

**Concept:** Apply a function to every element in a collection.

```python
# Imperative
numbers = [1, 2, 3, 4, 5]
doubled = []
for n in numbers:
    doubled.append(n * 2)

# Functional with map
doubled = list(map(lambda x: x * 2, numbers))
# [2, 4, 6, 8, 10]
```

**Visualization:**
```
[1, 2, 3, 4, 5]
 ↓  ↓  ↓  ↓  ↓   (apply *2 to each)
[2, 4, 6, 8, 10]
```

### Filter - Select Elements

**Concept:** Keep only elements that satisfy a predicate (boolean function).

```python
# Imperative
evens = []
for n in numbers:
    if n % 2 == 0:
        evens.append(n)

# Functional with filter
evens = list(filter(lambda x: x % 2 == 0, numbers))
# [2, 4]
```

**Visualization:**
```
[1, 2, 3, 4, 5]
 ✗  ✓  ✗  ✓  ✗   (keep if even)
[2, 4]
```

### Reduce - Combine Into Single Value

**Concept:** Combine all elements using a binary function.

```python
from functools import reduce

# Sum all numbers
total = reduce(lambda acc, x: acc + x, numbers, 0)
# ((((0 + 1) + 2) + 3) + 4) + 5 = 15
```

**Visualization:**
```
[1, 2, 3, 4, 5]
 ↘  ↓  ↓  ↓  ↓
  (0 + 1) = 1
   ↘  ↓  ↓  ↓
    (1 + 2) = 3
     ↘  ↓  ↓
      (3 + 3) = 6
       ↘  ↓
        (6 + 4) = 10
         ↘
          (10 + 5) = 15
```

**Other names:**
- `fold` (Haskell, many functional languages)
- `inject` (Ruby)
- `aggregate` (C#, LINQ)

## Lambdas / Anonymous Functions

**Anonymous functions** (lambdas) are unnamed functions, perfect for one-time use:

| Language | Syntax |
|----------|--------|
| Python | `lambda x: x * 2` |
| JavaScript | `(x) => x * 2` or `function(x) { return x * 2; }` |
| Java | `x -> x * 2` |
| Ruby | `lambda { |x| x * 2 }` or `-> (x) { x * 2 }` |
| Haskell | `\x -> x * 2` |
| Racket | `(lambda (x) (* x 2))` |
| Rust | `|x| x * 2` |
| C | (No direct support, use function pointers) |

**When to use:**
- ✓ Short, simple operations
- ✓ Used once
- ✗ Complex logic (use named functions)
- ✗ Reused multiple times

## Closures

A **closure** is a function that "closes over" variables from its enclosing scope:

```python
def make_counter():
    count = 0  # Enclosed variable

    def increment():
        nonlocal count
        count += 1
        return count

    return increment

counter = make_counter()
print(counter())  # 1
print(counter())  # 2
print(counter())  # 3
```

**Key insight:** `count` persists between calls because `increment` closed over it!

**Use cases:**
- Private state (encapsulation)
- Factory functions
- Event handlers with context
- Memoization

## Partial Application and Currying

### Partial Application

**Fixing some arguments** of a function:

```python
from functools import partial

def power(base, exponent):
    return base ** exponent

square = partial(power, exponent=2)
cube = partial(power, exponent=3)

square(5)  # 25
cube(5)   # 125
```

### Currying

**Transforming** `f(a, b, c)` into `f(a)(b)(c)`:

```javascript
// Regular function
function add(a, b, c) {
    return a + b + c;
}

// Curried version
function addCurried(a) {
    return function(b) {
        return function(c) {
            return a + b + c;
        };
    };
}

addCurried(1)(2)(3)  // 6

// Or with arrow functions
const add = a => b => c => a + b + c;
```

**Haskell curries by default:**
```haskell
add :: Int -> Int -> Int -> Int
add a b c = a + b + c

-- Really means:
add :: Int -> (Int -> (Int -> Int))

addFive = add 5  -- Partial application!
addFive 10 20  -- 35
```

## Function Composition

**Combining functions** to create new functions:

```javascript
const compose = (f, g) => x => f(g(x));

const addOne = x => x + 1;
const double = x => x * 2;

const addOneThenDouble = compose(double, addOne);

addOneThenDouble(5);  // (5 + 1) * 2 = 12
```

**Mathematical notation:** `(f ∘ g)(x) = f(g(x))`

**Haskell has built-in operator:**
```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c

addOneThenDouble = (*2) . (+1)
addOneThenDouble 5  -- 12
```

**Building complex transformations:**
```javascript
const processData = compose(
    formatOutput,
    removeNulls,
    transform,
    validate
);

// Instead of:
// formatOutput(removeNulls(transform(validate(data))))
processData(data)
```

## Common Higher-Order Functions

### Map Variants

```python
# map - transform each element
map(lambda x: x * 2, [1, 2, 3])

# flatMap - transform and flatten
# [[1, 2], [3, 4]] -> [1, 2, 3, 4]
```

### Filter Variants

```python
# filter - keep matching elements
filter(lambda x: x > 0, [-1, 0, 1, 2])

# reject - remove matching elements (Ruby)
# partition - split into [matching, non-matching]
```

### Reduce Variants

```python
# reduce - left-to-right
reduce(lambda acc, x: acc + x, [1, 2, 3], 0)

# reduceRight - right-to-left (JavaScript)
# scan - like reduce but returns intermediate values
```

### Other Common HOFs

```python
# all/every - all elements satisfy predicate?
all(lambda x: x > 0, [1, 2, 3])

# any/some - any element satisfies predicate?
any(lambda x: x > 10, [1, 2, 3])

# take - first n elements
# drop - skip first n elements
# zip - combine two lists element-wise
# groupBy - group elements by a key function
# sortBy - sort using a key function
```

## Declarative vs Imperative

### Imperative (How to do it)

```python
# Find average of positive even numbers
numbers = [1, -2, 3, 4, -5, 6, 7, 8]
sum_evens = 0
count = 0

for n in numbers:
    if n > 0 and n % 2 == 0:
        sum_evens += n
        count += 1

average = sum_evens / count if count > 0 else 0
```

### Declarative (What to compute)

```python
from statistics import mean

positive_evens = filter(lambda x: x > 0 and x % 2 == 0, numbers)
average = mean(positive_evens)
```

**Benefits of declarative:**
- ✓ More concise
- ✓ Clearer intent
- ✓ Less room for bugs
- ✓ Easier to parallelize
- ✗ May be less efficient (but often comparable)

## Real-World Use Cases

### 1. Data Pipelines

```python
data = load_data()
result = (data
    .filter(is_valid)
    .map(normalize)
    .group_by(lambda x: x.category)
    .map(aggregate)
    .sort_by(lambda x: x.score))
```

### 2. Event Handlers

```javascript
button.addEventListener('click', makeHandler(userId, context));

function makeHandler(userId, context) {
    return function(event) {
        // Closure captures userId and context
        processClick(event, userId, context);
    };
}
```

### 3. Configuration

```python
def retry(max_attempts):
    def decorator(func):
        def wrapper(*args, **kwargs):
            for attempt in range(max_attempts):
                try:
                    return func(*args, **kwargs)
                except Exception as e:
                    if attempt == max_attempts - 1:
                        raise
        return wrapper
    return decorator

@retry(3)
def fetch_data():
    # Will retry up to 3 times
    pass
```

### 4. Array Processing

```javascript
const usernames = users
    .filter(u => u.active)
    .map(u => u.name)
    .sort();
```

## Performance Considerations

### Lazy vs Eager Evaluation

**Eager (most languages):**
```python
numbers = range(1, 1000000)
result = map(lambda x: x * 2, numbers)  # Computes all immediately
```

**Lazy (Haskell, Python 3 generators):**
```python
numbers = range(1, 1000000)
result = map(lambda x: x * 2, numbers)  # Computes on demand
first_five = list(result)[:5]  # Only computes 5 elements
```

### When HOFs are Faster

- **Parallelization:** `map` can easily run in parallel
- **Optimization:** Compilers can optimize functional chains
- **Reduced overhead:** No loop bookkeeping

### When Loops are Faster

- **Early termination:** `break` in a loop vs full iteration
- **Complex accumulation:** Multiple values being tracked
- **Micro-optimization:** Avoid function call overhead

**Modern reality:** Difference is often negligible. Write clear code first, optimize later.

## Common Patterns

### 1. Method Chaining

```javascript
array
    .filter(x => x > 0)
    .map(x => x * 2)
    .reduce((sum, x) => sum + x, 0)
```

### 2. Function Pipeline

```python
pipe(
    data,
    filter(is_valid),
    map(transform),
    reduce(aggregate)
)
```

### 3. Callbacks

```javascript
setTimeout(() => console.log("Done!"), 1000);
```

### 4. Decorators (Python, JavaScript)

```python
@cache
@validate_input
def expensive_computation(x):
    return x ** 2
```

## HOFs Across Languages

| Feature | Python | JS | Java | Haskell | Rust |
|---------|--------|----|----|---------|------|
| First-class functions | ✓ | ✓ | ✓ | ✓ | ✓ |
| Lambdas | ✓ | ✓ | ✓ | ✓ | ✓ |
| Closures | ✓ | ✓ | ✓ | ✓ | ✓ |
| Auto-curry | ✗ | ✗ | ✗ | ✓ | ✗ |
| map/filter/reduce | ✓ | ✓ | ✓ (streams) | ✓ | ✓ (iterators) |
| Function composition | Manual | Manual | Manual | Built-in (.) | Manual |

## Common Pitfalls

### 1. Forgetting to Call

```python
# Wrong
result = map(lambda x: x * 2, numbers)  # Returns map object

# Right
result = list(map(lambda x: x * 2, numbers))  # Actually computes
```

### 2. Closure Loop Variable

```javascript
// Wrong - all closures capture same variable
var funcs = [];
for (var i = 0; i < 3; i++) {
    funcs.push(function() { return i; });
}
funcs[0]();  // 3 (not 0!)

// Right - use let or IIFE
for (let i = 0; i < 3; i++) {
    funcs.push(function() { return i; });
}
funcs[0]();  // 0
```

### 3. Overly Complex Chains

```python
# Too much!
result = reduce(lambda a,b: a+b, map(lambda x: x*2, filter(lambda x: x>0, data)), 0)

# Better
positive = filter(lambda x: x > 0, data)
doubled = map(lambda x: x * 2, positive)
result = reduce(lambda a, b: a + b, doubled, 0)

# Or use a named function
result = sum(double(x) for x in data if x > 0)
```

## Language-Specific Examples

### Python
- List comprehensions as alternative
- `functools` module (reduce, partial, etc.)
- Decorators (higher-order functions)

### JavaScript
- Array methods (map, filter, reduce)
- Promises and async/await
- React hooks (HOFs for components)

### Haskell
- Everything is curried by default
- Function composition operator (.)
- Monads (advanced HOFs)

### Rust
- Iterators with HOF methods
- Ownership considerations
- Zero-cost abstractions

### Java
- Streams API (Java 8+)
- Functional interfaces
- Method references

## Why Higher-Order Functions Matter

1. **Abstraction:** Separate "what" from "how"
2. **Reusability:** Generic operations on any data
3. **Composition:** Build complex from simple
4. **Declarative:** Express intent clearly
5. **Testability:** Pure functions easy to test
6. **Parallelization:** Easier to parallelize

## When to Use HOFs

**Good fit:**
- ✓ Transforming collections
- ✓ Data pipelines
- ✓ Building abstractions
- ✓ Callback-heavy code
- ✓ Configuration and setup

**Not ideal:**
- ✗ Performance-critical loops
- ✗ Complex state management
- ✗ When team unfamiliar

## Summary

- **Higher-order functions** take or return functions
- **Map, filter, reduce** are fundamental HOFs
- **Closures** enable private state and factories
- **Composition** builds complex behavior from simple parts
- **Declarative style** often clearer than imperative

**Key Takeaway:** HOFs enable powerful abstractions and cleaner code, transforming how we think about programming from "how to do it" to "what to compute."

---

Let's see how different languages implement higher-order functions!
