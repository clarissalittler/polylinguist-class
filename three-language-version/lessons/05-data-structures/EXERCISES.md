# Lesson 5: Data Structures - Exercises

## Instructions

Complete these exercises to practice working with data structures across Python, C++, and Haskell. Pay special attention to the differences between mutable and immutable approaches.

---

## Exercise 1: List Operations (Warmup)

**Difficulty:** Easy

Implement the following operations on a list/array:

1. Create a list with numbers 1-10
2. Add the number 11 to the end
3. Insert 0 at the beginning
4. Remove the element at index 5
5. Print the length and all elements

**Implement in:** Python (mutable), Haskell (immutable), and C++ (mutable)

**Questions:**
- How does the immutable version (Haskell) differ from the mutable versions?
- Which feels more natural for this task?
- What are the performance implications of each approach?

---

## Exercise 2: FizzBuzz with Arrays

**Difficulty:** Easy

Create a FizzBuzz array:
- Create a list/array of numbers 1-100
- Transform it so multiples of 3 become "Fizz", multiples of 5 become "Buzz", multiples of both become "FizzBuzz"
- Return the new list/array

**Implement in:**
- Python: Use list comprehension
- C++: Use `std::transform` or a range-based for loop
- Haskell: Use list comprehension or `map`

**Bonus:** Compare the code length and readability across the three languages.

---

## Exercise 3: Dictionary/Map Practice

**Difficulty:** Easy to Medium

Create a "phonebook" program:

1. Create a dictionary/map with at least 5 people and their phone numbers
2. Add a new person
3. Update someone's phone number
4. Look up a person's number
5. Delete a person
6. Print all names

**Implement in:** All three languages (Python `dict`, C++ `std::map` or `std::unordered_map`, Haskell `Map`)

**Bonus:** Handle the case where a person doesn't exist gracefully.

**Questions:**
- How do the three languages handle missing keys differently?
- What is the lookup complexity in each implementation?

---

## Exercise 4: Aliasing Bug Hunt

**Difficulty:** Medium (Conceptual)

Debug this Python code:

```python
original = [1, 2, 3]
copy = original
copy.append(4)
print(original)  # What prints? Why?
```

**Tasks:**
1. Explain why `original` changed
2. Fix it using `.copy()` or `[:]`
3. Write a similar example in C++ with references
4. Explain why this problem doesn't exist in Haskell
5. Discuss when aliasing is useful vs dangerous

**Reflection:**
- How does Haskell's immutability prevent this entire class of bugs?
- What compile-time or runtime mechanisms do Python and C++ provide to help?

---

## Exercise 5: Immutable Update Pattern

**Difficulty:** Medium

Given an immutable list in Haskell:
```haskell
numbers = [1, 2, 3, 4, 5]
```

Write functions to:
1. "Update" element at index 2 to be 10
2. "Remove" element at index 3
3. "Insert" 0 at index 0

**Hint:** You'll need to create new lists, not modify the original.

**Extension:**
- Implement similar patterns in Python using list slicing (creating new lists)
- Compare performance: when is creating new lists acceptable vs when is mutation necessary?

---

## Exercise 6: Set Operations

**Difficulty:** Medium

Given two sets:
- Set A: {1, 2, 3, 4, 5}
- Set B: {4, 5, 6, 7, 8}

Compute:
1. Union (A ∪ B)
2. Intersection (A ∩ B)
3. Difference (A - B)
4. Symmetric difference (A △ B) - elements in A or B but not both

**Implement in:** All three languages

**Application:** Use sets to find unique words in a text string. Count how many unique words are in: "the quick brown fox jumps over the lazy dog the fox"

**Questions:**
- Which language has the most concise syntax for set operations?
- How do the implementations differ in performance characteristics?

---

## Exercise 7: Nested Data Structures

**Difficulty:** Medium

Create a data structure representing a classroom:

```python
classroom = {
    "teacher": "Ms. Smith",
    "students": [
        {"name": "Alice", "grades": [90, 85, 88]},
        {"name": "Bob", "grades": [75, 80, 78]},
        {"name": "Charlie", "grades": [95, 92, 94]}
    ]
}
```

Write functions to:
1. Add a new student
2. Add a grade for a specific student
3. Calculate average grade for each student
4. Find the student with the highest average

**Implement in:** Python and C++ (using structs or classes)

**Bonus for Haskell:** Define proper data types for Student and Classroom, then implement the functions.

---

## Exercise 8: Matrix Operations

**Difficulty:** Medium to Hard

Represent a 3x3 matrix as a nested list/array/vector:

```python
matrix = [
    [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9]
]
```

Implement:
1. `get(row, col)` - get element at position
2. `transpose()` - swap rows and columns
3. `addMatrices(A, B)` - element-wise addition
4. `printMatrix()` - nicely formatted output

**Implement in:** All three languages

**Bonus:** Implement matrix multiplication.

**Questions:**
- How does Haskell's immutability affect the implementation?
- What are the type signatures in each language?

---

## Exercise 9: Word Frequency Counter

**Difficulty:** Medium

Write a program that:
1. Takes a string of text
2. Splits it into words
3. Counts how many times each word appears
4. Returns a dictionary/map of word → count
5. Prints words sorted by frequency (descending)

**Example:**
```
Input: "the quick brown fox jumps over the lazy dog the"
Output:
  the: 2
  quick: 1
  brown: 1
  fox: 1
  jumps: 1
  over: 1
  lazy: 1
  dog: 1
```

**Implement in:** All three languages

**Bonus:**
- Ignore case and punctuation
- Find the 5 most common words in a longer text

**Comparison:**
- Which language makes this task easiest?
- What built-in functions does each language provide?

---

## Exercise 10: Shopping Cart

**Difficulty:** Medium to Hard

Create a shopping cart data structure:

```python
# Each item: {"name": str, "price": float, "quantity": int}
cart = []
```

Implement functions:
1. `addItem(cart, name, price, quantity)` - add or update item
2. `removeItem(cart, name)` - remove item
3. `updateQuantity(cart, name, quantity)` - change quantity
4. `calculateTotal(cart)` - sum of price × quantity for all items
5. `applyDiscount(cart, percentage)` - reduce all prices

**Implement in:**
- Python (mutable approach - modify cart in place)
- Haskell (immutable approach - return new cart)
- C++ (either approach, but document which you chose)

**Questions:**
- How does the mutable vs immutable approach affect the function signatures?
- Which is easier to test?
- Which is easier to reason about?

---

## Exercise 11: Vector Operations

**Difficulty:** Medium

Implement common vector operations for 3D vectors (x, y, z):

1. Vector addition
2. Vector subtraction
3. Dot product
4. Cross product
5. Magnitude
6. Normalization

**Representation:**
- Python: Use tuples or lists: `(x, y, z)`
- C++: Use `std::tuple` or create a struct
- Haskell: Use tuples: `(x, y, z)`

**Implement in:** All three languages

**Questions:**
- Should vectors be mutable or immutable?
- How do you represent the type in each language?

---

## Exercise 12: Undo/Redo System

**Difficulty:** Hard

Implement an undo/redo system for a simple text editor:

```python
# State: current text
# History: list of previous states
# Future: list of undone states (for redo)
```

Implement:
1. `setText(state, text)` - update text, save to history
2. `undo(state)` - revert to previous text
3. `redo(state)` - go forward if previously undid
4. Keep last 10 states only (circular buffer)

**Implement in:** Python (mutable) and Haskell (immutable)

**Challenge Questions:**
- Why is this easier with immutable data structures?
- How do you handle the "current position" in the history?
- What happens to the redo stack when you make a new edit after undoing?

---

## Exercise 13: Mutable vs Immutable Performance

**Difficulty:** Hard (Experimentation)

Compare performance of mutable vs immutable approaches:

**Task:** Build a list of 10,000 elements

**Python Mutable:**
```python
import time
start = time.time()
result = []
for i in range(10000):
    result.append(i)
end = time.time()
print(f"Mutable: {end - start} seconds")
```

**Python "Immutable" (creating new lists):**
```python
def build_list(n, current=[]):
    if n == 0:
        return current
    return build_list(n - 1, current + [n])

# Time this
```

**Haskell:**
```haskell
buildList 0 = []
buildList n = n : buildList (n - 1)
```

**C++:**
```cpp
// Mutable
std::vector<int> vec;
for (int i = 0; i < 10000; ++i) {
    vec.push_back(i);
}
```

**Tasks:**
1. Time all approaches
2. Which is faster? Why?
3. When would you choose each?
4. What optimizations are possible?

**Questions:**
- Why is Python's "immutable" version so much slower?
- How does Haskell's structural sharing help?
- What is the memory usage of each approach?

---

## Exercise 14: Type-Safe Configuration

**Difficulty:** Hard

Create a configuration system that is type-safe:

**Requirements:**
- Store configuration key-value pairs
- Different keys have different types (string, int, bool)
- Prevent runtime type errors

**Python Approach:**
Use TypedDict or dataclass

**C++ Approach:**
Use std::map with std::variant or create a type-safe wrapper

**Haskell Approach:**
Use algebraic data types

**Implementation:**
1. Define configuration schema
2. Load configuration
3. Get values with type safety
4. Handle missing keys gracefully

**Questions:**
- Which language provides the best type safety?
- Where are type errors caught (compile-time vs runtime)?
- What are the tradeoffs?

---

## Challenge Projects

### Challenge 1: Implement a HashMap from Scratch

Build a simple hash table using arrays (Python list or C++ vector):
- `put(key, value)` - insert or update
- `get(key)` - retrieve value
- `remove(key)` - delete entry
- Handle collisions using chaining (linked lists)
- Implement resizing when load factor exceeds 0.75

**Implement in:** Python or C++

### Challenge 2: Build a Binary Search Tree

Implement a binary search tree with immutable nodes:
- `insert(value)` - add node (return new tree)
- `search(value)` - find node
- `delete(value)` - remove node (return new tree)
- `inOrderTraversal()` - print sorted

**Implement in:** Haskell (naturally immutable) or Python (creating new nodes)

**Questions:**
- How do you represent tree nodes?
- What are the type signatures?
- How does immutability affect the implementation?

### Challenge 3: LRU Cache

Implement a Least Recently Used cache with:
- `get(key)` - return value, mark as recently used
- `put(key, value)` - insert, evict oldest if capacity exceeded
- Fixed capacity (e.g., 100 items)
- O(1) operations using hash map + doubly-linked list

**Implement in:** C++ or Python

**Questions:**
- What data structures do you need?
- How do you maintain the access order?
- What are the space and time complexities?

---

## Reflection Questions

After completing the exercises, reflect on these questions:

### 1. Mutability
- When is mutability beneficial? When is it dangerous?
- How does each language help you manage mutable state safely?

### 2. Performance
- What are the performance characteristics of each data structure?
- When does immutability have an overhead? When is it free?
- How does Haskell achieve efficiency with immutable data?

### 3. Type Systems
- What role does the type system play in preventing data structure bugs?
- Which language catches the most errors at compile-time?
- How do optional/Maybe types help with missing values?

### 4. Expressiveness
- Which language's approach to data structures feels most natural for you? Why?
- Which language requires the most boilerplate?
- Which language makes common operations easiest?

### 5. Paradigm Differences
- How do imperative and functional paradigms differ in their approach to data?
- What are the tradeoffs between mutability and immutability?
- When would you choose each approach?

---

## Solutions

Solutions are provided in the code files:
- `data_structures.py`
- `data_structures.cpp`
- `data_structures.hs`

Try to complete exercises on your own first! The solution files contain examples and patterns, but your implementations may differ.

---

## Additional Resources

- **Python:** [Python Data Structures Documentation](https://docs.python.org/3/tutorial/datastructures.html)
- **C++:** [C++ STL Containers](https://en.cppreference.com/w/cpp/container)
- **Haskell:** [Learn You a Haskell - Data Structures](http://learnyouahaskell.com/)

Remember: Understanding the tradeoffs between mutable and immutable data structures is crucial for becoming a well-rounded programmer. Each approach has its place, and knowing when to use each is a key skill.
