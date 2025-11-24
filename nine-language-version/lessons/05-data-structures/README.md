# Lesson 5: Data Structures

## Learning Objectives

By the end of this lesson, you will be able to:

1. Work with arrays and lists in multiple languages
2. Understand the difference between mutable and immutable data structures
3. Use dictionaries/maps for key-value storage
4. Work with tuples and sets
5. Explain structural sharing in persistent data structures
6. Choose appropriate data structures for different problems
7. Understand how different paradigms approach data organization

## Introduction

Data structures organize and store data efficiently. Different programming paradigms have fundamentally different philosophies about data:

- **Imperative languages** favor mutable data structures you modify in place
- **Functional languages** favor immutable data structures you transform into new structures
- **Each approach has tradeoffs** in performance, safety, and ease of reasoning

## Arrays and Lists

### Python: Lists (Dynamic, Mutable)

```python
# Create list
numbers = [1, 2, 3, 4, 5]
fruits = ["apple", "banana", "cherry"]
mixed = [1, "hello", 3.14, True]  # Can mix types

# Access elements (zero-indexed)
print(numbers[0])    # 1
print(numbers[-1])   # 5 (last element)

# Modify list (MUTABLE)
numbers[0] = 10      # [10, 2, 3, 4, 5]
numbers.append(6)    # [10, 2, 3, 4, 5, 6]
numbers.insert(0, 0) # [0, 10, 2, 3, 4, 5, 6]
numbers.pop()        # Remove and return last element

# Slicing
print(numbers[1:4])  # [10, 2, 3]
print(numbers[:3])   # [0, 10, 2]
print(numbers[3:])   # [3, 4, 5]

# List comprehensions
squared = [x * x for x in numbers]
evens = [x for x in numbers if x % 2 == 0]
```

**Key features:**
- Dynamic size (grow/shrink)
- Mutable (modify in place)
- Can hold any types
- Fast random access O(1)
- Slow insertion/deletion at beginning O(n)

### JavaScript: Arrays (Dynamic, Mutable)

```javascript
// Create array
const numbers = [1, 2, 3, 4, 5];
const fruits = ["apple", "banana", "cherry"];

// Access
console.log(numbers[0]);    // 1
console.log(numbers.length); // 5

// Modify (MUTABLE - even though declared with const!)
numbers.push(6);       // [1, 2, 3, 4, 5, 6]
numbers.pop();         // Remove last
numbers.unshift(0);    // Add to beginning
numbers.shift();       // Remove first

// Array methods (functional style)
const squared = numbers.map(x => x * x);
const evens = numbers.filter(x => x % 2 === 0);
const sum = numbers.reduce((acc, x) => acc + x, 0);

// Spread operator (create copy)
const copy = [...numbers];
const combined = [...numbers, ...fruits];
```

**Note:** `const` prevents reassignment, not mutation!

### C: Arrays (Fixed Size, Mutable)

```c
// Fixed-size array
int numbers[5] = {1, 2, 3, 4, 5};
char name[] = "Hello";  // Array of characters

// Access
printf("%d\n", numbers[0]);  // 1

// Modify
numbers[0] = 10;

// No built-in resize, append, or insert
// Must manage memory manually for dynamic arrays

// Get size at compile time
int size = sizeof(numbers) / sizeof(numbers[0]);
```

**Key limitations:**
- Fixed size at compile time
- No automatic resizing
- Manual memory management for dynamic arrays
- Array name decays to pointer

### Haskell: Lists (Immutable, Linked)

```haskell
-- Create list
numbers = [1, 2, 3, 4, 5]
fruits = ["apple", "banana", "cherry"]

-- Access (cons pattern :)
head numbers  -- 1
tail numbers  -- [2, 3, 4, 5]
numbers !! 0  -- 1 (!! for indexing, but O(n)!)

-- "Modify" - create new list (IMMUTABLE)
newList = 0 : numbers  -- Prepend: [0, 1, 2, 3, 4, 5]
-- Can't modify in place!

-- List comprehensions
squared = [x * x | x <- numbers]
evens = [x | x <- numbers, even x]

-- Infinite lists (lazy evaluation!)
naturals = [1..]
take 5 naturals  -- [1, 2, 3, 4, 5]
```

**Key features:**
- Immutable (cannot change)
- Linked list structure
- Fast prepend O(1)
- Slow random access O(n)
- Supports infinite lists (lazy)

### Rust: Vectors (Dynamic, Ownership-aware)

```rust
// Create vector
let mut numbers = vec![1, 2, 3, 4, 5];
let fruits = vec!["apple", "banana", "cherry"];

// Access
println!("{}", numbers[0]);     // 1
println!("{}", numbers.get(0)); // Some(1)

// Modify (requires mut)
numbers.push(6);       // [1, 2, 3, 4, 5, 6]
numbers.pop();         // Some(6)
numbers[0] = 10;

// Immutable version
let numbers = vec![1, 2, 3, 4, 5];
// numbers.push(6);  // ERROR: not mutable

// Iterators (functional style)
let squared: Vec<i32> = numbers.iter().map(|x| x * x).collect();
let evens: Vec<i32> = numbers.iter().filter(|x| *x % 2 == 0).copied().collect();
```

**Key features:**
- Ownership system prevents data races
- Explicit mutability (`mut`)
- Memory safe without garbage collection

## Mutability vs Immutability

### Mutable Data Structures

**Definition:** Data that can be changed in place.

**Example (Python):**
```python
lst = [1, 2, 3]
lst.append(4)  # Modifies lst directly
print(lst)     # [1, 2, 3, 4]

# Aliasing problem
lst1 = [1, 2, 3]
lst2 = lst1      # Both point to same list!
lst2.append(4)
print(lst1)      # [1, 2, 3, 4] - Surprise!
```

**Advantages:**
- Memory efficient (modify in place)
- Sometimes more intuitive
- Better performance for certain operations

**Disadvantages:**
- Harder to reason about (what changed when?)
- Aliasing bugs
- Not thread-safe
- Can't undo changes easily

### Immutable Data Structures

**Definition:** Data that cannot be changed. "Modifications" create new structures.

**Example (Haskell):**
```haskell
lst = [1, 2, 3]
newLst = 4 : lst  -- Creates NEW list: [4, 1, 2, 3]
-- lst unchanged: [1, 2, 3]

-- No aliasing problem - everything is a copy (conceptually)
```

**Advantages:**
- Easy to reason about (values don't change)
- Thread-safe by default
- Can share data safely
- Time travel / undo possible

**Disadvantages:**
- Conceptually creates many copies (expensive?)
- Less intuitive for some operations

**Solution: Structural Sharing**

Immutable languages use clever implementations:

```haskell
-- Adding to front of list
oldList = [2, 3, 4]
newList = 1 : oldList  -- [1, 2, 3, 4]

-- Internally:
-- newList: [1] -> oldList
--                  [2] -> [3] -> [4] -> []
-- Only one new node created!
```

## Dictionaries / Maps / Hash Tables

Key-value storage for efficient lookups.

### Python: Dictionaries

```python
# Create dictionary
person = {
    "name": "Alice",
    "age": 30,
    "city": "NYC"
}

# Access
print(person["name"])           # "Alice"
print(person.get("age"))        # 30
print(person.get("country", "USA"))  # Default value

# Modify (MUTABLE)
person["age"] = 31
person["email"] = "alice@example.com"
del person["city"]

# Iteration
for key in person:
    print(f"{key}: {person[key]}")

for key, value in person.items():
    print(f"{key}: {value}")

# Dictionary comprehension
squared = {x: x*x for x in range(5)}
```

### JavaScript: Objects and Maps

```javascript
// Object (like dict)
const person = {
    name: "Alice",
    age: 30,
    city: "NYC"
};

// Access
console.log(person.name);      // "Alice"
console.log(person["age"]);    // 30

// Modify
person.age = 31;
person.email = "alice@example.com";
delete person.city;

// Map (ES6) - better for key-value storage
const map = new Map();
map.set("name", "Alice");
map.set("age", 30);
map.get("name");  // "Alice"
map.has("age");   // true
map.delete("age");

// Map can use any type as key!
map.set({id: 1}, "value");
```

### Haskell: Maps (Immutable)

```haskell
import qualified Data.Map as Map

-- Create map
person = Map.fromList [("name", "Alice"), ("age", "30"), ("city", "NYC")]

-- Lookup
Map.lookup "name" person  -- Just "Alice"
Map.lookup "country" person  -- Nothing

-- "Modify" - creates new map
updated = Map.insert "age" "31" person
removed = Map.delete "city" person

-- person unchanged!
```

### Rust: HashMap

```rust
use std::collections::HashMap;

// Create HashMap
let mut person = HashMap::new();
person.insert("name", "Alice");
person.insert("age", "30");

// Access
match person.get("name") {
    Some(name) => println!("{}", name),
    None => println!("Not found"),
}

// Modify (requires mut)
person.insert("age", "31");
person.remove("city");

// Immutable version
let person = HashMap::from([
    ("name", "Alice"),
    ("age", "30"),
]);
// person.insert(...);  // ERROR: not mutable
```

## Tuples

Fixed-size, ordered collections (often immutable).

```python
# Python
point = (3, 4)
person = ("Alice", 30, "NYC")

# Unpacking
x, y = point
name, age, city = person

# Named tuples
from collections import namedtuple
Person = namedtuple('Person', ['name', 'age', 'city'])
alice = Person("Alice", 30, "NYC")
print(alice.name)  # "Alice"
```

```haskell
-- Haskell
point :: (Int, Int)
point = (3, 4)

person :: (String, Int, String)
person = ("Alice", 30, "NYC")

-- Pattern matching
(x, y) = point
(name, age, city) = person

-- Access with fst, snd (for pairs only)
fst point  -- 3
snd point  -- 4
```

```rust
// Rust
let point: (i32, i32) = (3, 4);
let person = ("Alice", 30, "NYC");

// Destructuring
let (x, y) = point;
let (name, age, city) = person;

// Access by index
println!("{}", point.0);  // 3
println!("{}", point.1);  // 4
```

## Sets

Unordered collections of unique elements.

```python
# Python
numbers = {1, 2, 3, 4, 5}
fruits = {"apple", "banana", "cherry"}

# Add/remove
numbers.add(6)
numbers.remove(1)
numbers.discard(10)  # Won't error if not present

# Set operations
evens = {2, 4, 6, 8}
odds = {1, 3, 5, 7}
evens | odds  # Union
evens & odds  # Intersection (empty)
evens - odds  # Difference
```

## Comparison Table

| Language   | Arrays/Lists | Mutable? | Dictionary | Sets | Tuples |
|------------|--------------|----------|------------|------|--------|
| Python     | list         | Yes      | dict       | set  | tuple  |
| JavaScript | Array        | Yes      | Object/Map | Set  | -      |
| C          | array[]      | Yes      | -          | -    | -      |
| Java       | ArrayList    | Yes      | HashMap    | HashSet | -   |
| Ruby       | Array        | Yes      | Hash       | Set  | -      |
| Haskell    | [a]          | No       | Map        | Set  | (a,b)  |
| Racket     | list/vector  | vector   | hash       | set  | cons   |
| Rust       | Vec<T>       | mut      | HashMap    | HashSet | (T,U)|

## Key Insights

### 1. Mutability Tradeoffs

**Mutable:**
- Efficient updates
- Aliasing bugs possible
- Thread safety issues

**Immutable:**
- Safe sharing
- Easy reasoning
- Structural sharing avoids copies

### 2. Language Philosophy

- **Imperative (C, Python, Java):** Mutable by default
- **Functional (Haskell, Racket):** Immutable by default
- **Multi-paradigm (Rust):** Explicit choice with `mut`

### 3. Data Structure Choice

- **Arrays/Lists:** Sequential access
- **Maps/Dicts:** Key-value lookup
- **Sets:** Uniqueness, membership testing
- **Tuples:** Fixed-size, heterogeneous

## Exercises

See EXERCISES.md for hands-on practice with data structures!

## Summary

Data structures are fundamental to programming. Different paradigms offer different approaches:
- **Mutable structures** modify in place (efficient, but complex reasoning)
- **Immutable structures** create new versions (safe, parallel-friendly)
- **Choice matters:** Pick the right structure for the task

Understanding these differences makes you a better programmer in any language!
