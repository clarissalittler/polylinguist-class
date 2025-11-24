# Lesson 5: Data Structures

## Learning Objectives

By the end of this lesson, you will be able to:

1. Work with arrays and lists in Python, C++, and Haskell
2. Understand the difference between mutable and immutable data structures
3. Use dictionaries/maps for key-value storage
4. Work with tuples and sets
5. Explain structural sharing in persistent data structures
6. Choose appropriate data structures for different problems
7. Understand how different paradigms approach data organization

## Introduction

Data structures organize and store data efficiently. Different programming paradigms have fundamentally different philosophies about data:

- **Python (Dynamic):** Mutable data structures by default, with high-level abstractions
- **C++ (Systems):** Explicit control over memory and mutability with the STL
- **Haskell (Functional):** Immutable data structures with structural sharing

Each approach has tradeoffs in performance, safety, and ease of reasoning. Understanding these differences is crucial for choosing the right tool for the job.

## Arrays and Lists

### Python: Lists (Dynamic, Mutable)

Python's list is a dynamic, mutable array that can hold any types.

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
- Dynamic size (grow/shrink automatically)
- Mutable (modify in place)
- Can hold any types (heterogeneous)
- Fast random access O(1)
- Slow insertion/deletion at beginning O(n)
- Append is amortized O(1)

### C++: Vectors (Dynamic, Mutable)

C++ provides `std::vector` as a dynamic array from the STL.

```cpp
#include <vector>
#include <iostream>

// Create vector
std::vector<int> numbers = {1, 2, 3, 4, 5};
std::vector<std::string> fruits = {"apple", "banana", "cherry"};

// Access elements (zero-indexed)
std::cout << numbers[0] << std::endl;    // 1
std::cout << numbers.at(0) << std::endl; // 1 (with bounds checking)

// Modify vector (MUTABLE)
numbers[0] = 10;           // {10, 2, 3, 4, 5}
numbers.push_back(6);      // {10, 2, 3, 4, 5, 6}
numbers.insert(numbers.begin(), 0);  // {0, 10, 2, 3, 4, 5, 6}
numbers.pop_back();        // Remove last element

// Size and capacity
std::cout << numbers.size() << std::endl;      // Number of elements
std::cout << numbers.capacity() << std::endl;  // Allocated capacity

// Iteration
for (int x : numbers) {
    std::cout << x << " ";
}
std::cout << std::endl;

// Range-based algorithms
auto squared = numbers;
std::transform(squared.begin(), squared.end(), squared.begin(),
               [](int x) { return x * x; });
```

**Key features:**
- Dynamic size (managed automatically)
- Mutable (modify in place)
- Type-safe (homogeneous - all elements same type)
- Fast random access O(1)
- Automatic memory management (RAII)
- Contiguous memory layout
- Efficient cache performance

### Haskell: Lists (Immutable, Linked)

Haskell's lists are immutable linked lists.

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

-- Common operations
length numbers   -- 5
reverse numbers  -- [5, 4, 3, 2, 1]
sum numbers      -- 15
product numbers  -- 120
```

**Key features:**
- Immutable (cannot change)
- Linked list structure
- Fast prepend O(1)
- Slow random access O(n)
- Supports infinite lists (lazy evaluation)
- Pattern matching support
- Structural sharing for efficiency

## Mutability vs Immutability

### The Aliasing Problem (Mutable)

**Python Example:**
```python
lst1 = [1, 2, 3]
lst2 = lst1      # Both point to same list!
lst2.append(4)
print(lst1)      # [1, 2, 3, 4] - Surprise!

# Fix: explicit copy
lst3 = lst1.copy()  # or lst1[:]
lst3.append(5)
print(lst1)      # [1, 2, 3, 4] - Unchanged
```

**C++ Example:**
```cpp
std::vector<int> vec1 = {1, 2, 3};
std::vector<int>& vec2 = vec1;  // Reference to same vector
vec2.push_back(4);
// vec1 is now {1, 2, 3, 4}

// Fix: explicit copy
std::vector<int> vec3 = vec1;   // Copy constructor
vec3.push_back(5);
// vec1 remains {1, 2, 3, 4}
```

**Advantages of Mutability:**
- Memory efficient (modify in place)
- Sometimes more intuitive
- Better performance for certain operations
- Familiar imperative style

**Disadvantages of Mutability:**
- Harder to reason about (what changed when?)
- Aliasing bugs
- Not thread-safe
- Can't undo changes easily
- Shared mutable state is complex

### Immutability in Haskell

**Example:**
```haskell
lst = [1, 2, 3]
newLst = 4 : lst  -- Creates NEW list: [4, 1, 2, 3]
-- lst unchanged: [1, 2, 3]

-- No aliasing problem - all values are immutable
```

**Advantages of Immutability:**
- Easy to reason about (values don't change)
- Thread-safe by default
- Can share data safely
- Time travel / undo possible
- Referential transparency

**Disadvantages of Immutability:**
- Conceptually creates many copies
- Can be less intuitive for imperative operations
- Different performance characteristics

### Structural Sharing

Haskell uses structural sharing to make immutability efficient:

```haskell
-- Adding to front of list
oldList = [2, 3, 4]
newList = 1 : oldList  -- [1, 2, 3, 4]

-- Internally:
-- newList: [1] -> oldList
--                  [2] -> [3] -> [4] -> []
-- Only one new node created!
```

This means "copying" immutable structures is cheap because they share most of their data.

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
# {0: 0, 1: 1, 2: 4, 3: 9, 4: 16}

# Check membership
if "name" in person:
    print("Name found")
```

**Performance:** O(1) average for lookup, insert, delete

### C++: Maps and Unordered Maps

```cpp
#include <map>
#include <unordered_map>
#include <string>

// std::map - ordered (red-black tree)
std::map<std::string, int> ages;
ages["Alice"] = 30;
ages["Bob"] = 25;
ages["Charlie"] = 35;

// Access
std::cout << ages["Alice"] << std::endl;  // 30

// Check if key exists
if (ages.find("Alice") != ages.end()) {
    std::cout << "Alice found" << std::endl;
}

// Iterate (sorted by key)
for (const auto& [name, age] : ages) {
    std::cout << name << ": " << age << std::endl;
}

// std::unordered_map - hash table
std::unordered_map<std::string, std::string> person;
person["name"] = "Alice";
person["city"] = "NYC";
person.insert({"age", "30"});

// Erase key
person.erase("city");

// Get with default (C++17)
auto country = person.contains("country") ? person["country"] : "USA";
```

**Performance:**
- `std::map`: O(log n) for lookup, insert, delete (sorted)
- `std::unordered_map`: O(1) average for lookup, insert, delete

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

-- person is still unchanged!

-- Iteration
Map.foldWithKey (\k v acc -> (k ++ ": " ++ v) : acc) [] person

-- Check membership
Map.member "name" person  -- True
Map.member "country" person  -- False

-- Size
Map.size person  -- 3
```

**Performance:** O(log n) for lookup, insert, delete (balanced tree)

## Tuples

Fixed-size, ordered collections with heterogeneous types.

### Python: Tuples

```python
# Create tuple
point = (3, 4)
person = ("Alice", 30, "NYC")

# Access by index
print(point[0])  # 3

# Unpacking
x, y = point
name, age, city = person

# Tuples are immutable
try:
    point[0] = 5
except TypeError:
    print("Can't modify tuple")

# Named tuples
from collections import namedtuple
Person = namedtuple('Person', ['name', 'age', 'city'])
alice = Person("Alice", 30, "NYC")
print(alice.name)  # "Alice"
print(alice.age)   # 30

# Use as dictionary keys (lists can't!)
locations = {
    (0, 0): "origin",
    (1, 0): "east",
    (0, 1): "north"
}
```

### C++: Tuples and Pairs

```cpp
#include <tuple>
#include <utility>  // for pair

// Pair (2 elements)
std::pair<int, int> point(3, 4);
std::cout << point.first << ", " << point.second << std::endl;

// Tuple (arbitrary size)
std::tuple<std::string, int, std::string> person("Alice", 30, "NYC");

// Access by index (compile-time)
std::cout << std::get<0>(person) << std::endl;  // "Alice"
std::cout << std::get<1>(person) << std::endl;  // 30

// Structured bindings (C++17)
auto [name, age, city] = person;
std::cout << name << " is " << age << std::endl;

// Make tuple
auto point3d = std::make_tuple(1, 2, 3);

// Tuples can be compared
std::tuple<int, int> p1(1, 2);
std::tuple<int, int> p2(1, 3);
if (p1 < p2) {
    std::cout << "p1 is less than p2" << std::endl;
}
```

### Haskell: Tuples

```haskell
-- Create tuple
point :: (Int, Int)
point = (3, 4)

person :: (String, Int, String)
person = ("Alice", 30, "NYC")

-- Access with fst, snd (for pairs only)
fst point  -- 3
snd point  -- 4

-- Pattern matching (works for any size)
(name, age, city) = person

-- Tuples in function signatures
addPoints :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPoints (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Different sizes are different types
-- (Int, Int) is not the same type as (Int, Int, Int)
```

## Sets

Unordered collections of unique elements.

### Python: Sets

```python
# Create set
numbers = {1, 2, 3, 4, 5}
fruits = {"apple", "banana", "cherry"}

# Add/remove
numbers.add(6)
numbers.add(3)  # Duplicate ignored
numbers.remove(1)  # Error if not present
numbers.discard(10)  # No error if not present

# Set operations
evens = {2, 4, 6, 8}
odds = {1, 3, 5, 7}
print(evens | odds)  # Union: {1, 2, 3, 4, 5, 6, 7, 8}
print(evens & odds)  # Intersection: set()
print(evens - odds)  # Difference: {2, 4, 6, 8}
print(evens ^ odds)  # Symmetric difference

# Membership testing (fast!)
if 3 in numbers:
    print("Found")

# Remove duplicates from list
duplicates = [1, 2, 2, 3, 3, 3, 4]
unique = list(set(duplicates))  # [1, 2, 3, 4]
```

### C++: Sets and Unordered Sets

```cpp
#include <set>
#include <unordered_set>

// std::set - ordered
std::set<int> numbers = {1, 2, 3, 4, 5};
numbers.insert(6);
numbers.insert(3);  // Duplicate ignored

// Check membership
if (numbers.find(3) != numbers.end()) {
    std::cout << "Found 3" << std::endl;
}

// Iterate (sorted)
for (int x : numbers) {
    std::cout << x << " ";
}

// Set operations
std::set<int> evens = {2, 4, 6, 8};
std::set<int> odds = {1, 3, 5, 7};

std::set<int> union_set;
std::set_union(evens.begin(), evens.end(),
               odds.begin(), odds.end(),
               std::inserter(union_set, union_set.begin()));

// std::unordered_set - hash table
std::unordered_set<std::string> fruits = {"apple", "banana"};
fruits.insert("cherry");

// Remove duplicates
std::vector<int> vec = {1, 2, 2, 3, 3, 3, 4};
std::set<int> unique(vec.begin(), vec.end());
```

### Haskell: Sets (Immutable)

```haskell
import qualified Data.Set as Set

-- Create set
numbers = Set.fromList [1, 2, 3, 4, 5]

-- Add/remove (creates new set)
withSix = Set.insert 6 numbers
withoutOne = Set.delete 1 numbers

-- Membership
Set.member 3 numbers  -- True
Set.member 10 numbers  -- False

-- Set operations
evens = Set.fromList [2, 4, 6, 8]
odds = Set.fromList [1, 3, 5, 7]

Set.union evens odds         -- {1, 2, 3, 4, 5, 6, 7, 8}
Set.intersection evens odds  -- {}
Set.difference evens odds    -- {2, 4, 6, 8}

-- Remove duplicates
unique = Set.fromList [1, 2, 2, 3, 3, 3, 4]  -- {1, 2, 3, 4}
```

## Comparison Table

| Feature | Python | C++ | Haskell |
|---------|--------|-----|---------|
| **Lists/Vectors** | list | vector<T> | [a] |
| Mutability | Mutable | Mutable | Immutable |
| Random Access | O(1) | O(1) | O(n) |
| Prepend | O(n) | O(n) | O(1) |
| Append | O(1) amortized | O(1) amortized | O(n) |
| **Dictionaries/Maps** | dict | map/unordered_map | Map |
| Lookup Time | O(1) avg | O(log n) / O(1) avg | O(log n) |
| Ordered | No (3.7+: insertion) | Yes / No | Yes |
| **Sets** | set | set/unordered_set | Set |
| Membership | O(1) avg | O(log n) / O(1) avg | O(log n) |
| **Tuples** | tuple | tuple/pair | (a,b,..) |
| Mutable | No | No | No |
| Size | Dynamic | Fixed (compile) | Fixed (type) |

## Key Insights

### 1. Mutability Tradeoffs

**Mutable (Python, C++):**
- Efficient in-place updates
- Familiar imperative style
- Aliasing bugs possible
- Thread safety requires care
- Useful for performance-critical code

**Immutable (Haskell):**
- Safe sharing across contexts
- Easy to reason about
- Thread-safe by default
- Structural sharing avoids excessive copying
- Enables powerful optimizations

### 2. Performance Characteristics

**Python:**
- Dynamic typing allows flexibility
- Lists are versatile but not always optimal
- Dictionary/set use hash tables (fast!)
- No compile-time optimization

**C++:**
- Type safety catches errors early
- Control over memory layout
- STL containers highly optimized
- Template specialization for performance

**Haskell:**
- Lazy evaluation enables infinite structures
- Immutability enables aggressive optimization
- Pattern matching on structures
- Lists optimized for sequential access

### 3. Choosing the Right Structure

**Arrays/Lists:**
- Use when: Sequential access, random access needed
- Python: Default choice for collections
- C++: `vector` for most needs
- Haskell: Lists for functional processing

**Maps/Dictionaries:**
- Use when: Key-value lookups, associations
- Choose hash table for O(1) average
- Choose tree for sorted iteration

**Sets:**
- Use when: Uniqueness, membership testing
- Efficient duplicate removal
- Set operations (union, intersection)

**Tuples:**
- Use when: Fixed-size, heterogeneous data
- Function return multiple values
- Immutable grouping

### 4. Language Philosophy

- **Python:** Pragmatic, mutable by default, high-level
- **C++:** Control and performance, explicit management
- **Haskell:** Purity and safety, immutable by default

## Looking Ahead

In future lessons, we'll explore:
- **Custom data structures:** Building trees, graphs, and more
- **Type classes:** How Haskell abstracts over containers
- **Iterators and ranges:** C++ mechanisms for traversal
- **Algebraic data types:** Haskell's powerful type system
- **Memory management:** Deep dive into C++ ownership

Understanding data structures is fundamental to programming. Each language offers different tools and philosophies, and knowing when to use each approach makes you a better programmer.

## Exercises

See EXERCISES.md for hands-on practice with data structures!

## Summary

Data structures are the foundation of programming:

- **Python** offers convenient, mutable structures with dynamic typing
- **C++** provides efficient, type-safe containers with explicit control
- **Haskell** uses immutable structures with structural sharing

Key takeaways:
- Mutable structures modify in place (efficient, but complex reasoning)
- Immutable structures create new versions (safe, parallel-friendly)
- Choose the right structure for the task: lists, maps, sets, tuples
- Understand tradeoffs: performance vs safety vs ease of use

Mastering data structures in multiple paradigms deepens your understanding of programming fundamentals.
