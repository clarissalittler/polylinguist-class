# Lesson 5: Data Structures - Exercises

## Instructions

Complete these exercises to practice working with data structures across different languages. Pay special attention to the differences between mutable and immutable approaches.

---

## Exercise 1: List Operations (Warmup)

**Difficulty:** Easy

Implement the following operations on a list/array:

1. Create a list with numbers 1-10
2. Add the number 11 to the end
3. Insert 0 at the beginning
4. Remove the element at index 5
5. Print the length and all elements

**Implement in:** Python (mutable), Haskell (immutable)

**Questions:**
- How does the immutable version differ?
- Which feels more natural for this task?

---

## Exercise 2: FizzBuzz with Arrays

**Difficulty:** Easy

Create a FizzBuzz array:
- Create a list/array of numbers 1-100
- Transform it so multiples of 3 become "Fizz", multiples of 5 become "Buzz", multiples of both become "FizzBuzz"
- Return the new list/array

**Implement using:**
- Python list comprehension
- JavaScript map
- Haskell list comprehension

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

**Implement in:** Python, JavaScript, or Java

**Bonus:** Handle the case where a person doesn't exist gracefully.

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
3. Explain when aliasing is useful vs dangerous

**Compare with:** Haskell (no aliasing possible) and Rust (compile-time prevention)

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

**Bonus:** Implement similar patterns in JavaScript using spread operator.

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

**Implement in:** Python, Haskell, or Java

**Application:** Use sets to find unique words in a text file.

---

## Exercise 7: Nested Data Structures

**Difficulty:** Medium

Create a data structure representing a class of students:

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

**Implement in:** Python or JavaScript

---

## Exercise 8: Matrix Operations

**Difficulty:** Medium to Hard

Represent a 3x3 matrix as a nested list/array:
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
3. `add_matrices(A, B)` - element-wise addition
4. `print_matrix()` - nicely formatted output

**Bonus:** Implement matrix multiplication.

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
  the: 3
  quick: 1
  brown: 1
  ...
```

**Implement in:** Any language with dictionaries/maps

**Bonus:** Ignore case and punctuation.

---

## Exercise 10: Shopping Cart

**Difficulty:** Medium to Hard

Create a shopping cart data structure:

```python
# Each item: {"name": str, "price": float, "quantity": int}
cart = []
```

Implement functions:
1. `add_item(cart, name, price, quantity)` - add or update item
2. `remove_item(cart, name)` - remove item
3. `update_quantity(cart, name, quantity)` - change quantity
4. `calculate_total(cart)` - sum of price × quantity for all items
5. `apply_discount(cart, percentage)` - reduce all prices

**Extra:** Implement in both mutable (Python) and immutable (Haskell) styles.

---

## Exercise 11: Graph Representation

**Difficulty:** Hard

Represent a graph (cities and roads between them) using data structures.

**Option 1: Adjacency List**
```python
graph = {
    "NYC": ["Boston", "Philadelphia"],
    "Boston": ["NYC", "Portland"],
    "Philadelphia": ["NYC", "Washington"]
}
```

**Option 2: Edge List**
```python
edges = [
    ("NYC", "Boston"),
    ("NYC", "Philadelphia"),
    ("Boston", "Portland"),
    ...
]
```

Implement:
1. `add_node(graph, city)` - add city
2. `add_edge(graph, city1, city2)` - add road
3. `neighbors(graph, city)` - get connected cities
4. `is_connected(graph, city1, city2)` - check if direct road exists

**Bonus:** Implement breadth-first search to find shortest path.

---

## Exercise 12: Undo/Redo System

**Difficulty:** Hard

Implement an undo/redo system for a text editor:

```python
# State: current text
# History: list of previous states
# Future: list of undone states (for redo)
```

Implement:
1. `set_text(state, text)` - update text, save to history
2. `undo(state)` - revert to previous text
3. `redo(state)` - go forward if undid
4. Keep last 10 states only

**Challenge:** Why is this easier with immutable data structures?

---

## Exercise 13: Mutable vs Immutable Performance

**Difficulty:** Hard (Experimentation)

Compare performance of mutable vs immutable approaches:

**Task:** Build a list of 10,000 elements

**Mutable (Python):**
```python
result = []
for i in range(10000):
    result.append(i)
```

**Immutable (Haskell):**
```haskell
buildList 0 = []
buildList n = n : buildList (n - 1)
```

**Questions:**
1. Time both approaches
2. Which is faster? Why?
3. When would you choose each?

---

## Exercise 14: JSON Parser

**Difficulty:** Hard

Write a function that parses simple JSON into native data structures:

```python
parse_json('{"name": "Alice", "age": 30}')
# Returns: {"name": "Alice", "age": 30}

parse_json('[1, 2, 3]')
# Returns: [1, 2, 3]
```

**Simplified:** Support only:
- Objects: `{"key": "value"}`
- Arrays: `[1, 2, 3]`
- Strings, numbers, booleans

**Hint:** Use a dictionary/map for objects, array/list for arrays.

---

## Challenge Projects

### Challenge 1: Implement Your Own HashMap

Build a simple hash table from scratch using arrays:
- `put(key, value)` - insert or update
- `get(key)` - retrieve value
- `remove(key)` - delete entry
- Handle collisions (chaining or linear probing)

### Challenge 2: Build a Tree

Implement a binary search tree:
- `insert(value)` - add node
- `search(value)` - find node
- `delete(value)` - remove node
- `in_order_traversal()` - print sorted

### Challenge 3: LRU Cache

Implement a Least Recently Used cache with:
- `get(key)` - return value, mark as recently used
- `put(key, value)` - insert, evict oldest if capacity exceeded
- Use a dictionary + doubly-linked list

---

## Reflection Questions

After completing the exercises:

1. **When is mutability beneficial? When is it dangerous?**

2. **How does Haskell achieve efficiency with immutable data?**

3. **What role does the type system play in preventing data structure bugs?**

4. **How does Rust's ownership system prevent common data structure bugs?**

5. **Which language's approach to data structures feels most natural for you? Why?**

---

## Solutions

Solutions will be provided separately. Try to complete exercises on your own first!

Remember: Understanding the tradeoffs between mutable and immutable data structures is crucial for becoming a well-rounded programmer.
