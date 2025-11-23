# Lesson 6: Recursion - Exercises

## Instructions

These exercises will help you master recursive thinking across different programming paradigms. Remember: every recursive function needs a base case and must make progress toward it!

---

## Exercise 1: Power Function

**Difficulty:** Easy

Implement a recursive function to calculate x^n (x to the power of n).

**Tasks:**
1. Implement in Python
2. Implement in a functional language (Haskell or Racket)
3. Test with: `power(2, 10)` should return `1024`

**Base case:** `x^0 = 1`
**Recursive case:** `x^n = x * x^(n-1)`

---

## Exercise 2: Count Down

**Difficulty:** Easy

Write a recursive function that prints numbers from n down to 1.

**Example:**
```
countdown(5)
5
4
3
2
1
Blast off!
```

**Tasks:**
1. Implement in Python
2. Implement in JavaScript
3. What's the base case?

---

## Exercise 3: List Operations

**Difficulty:** Medium

Implement these recursive list operations:

1. `contains(list, element)` - Check if element is in list
2. `count(list, element)` - Count occurrences of element
3. `nth(list, n)` - Get the nth element (0-indexed)

**Tasks:**
- Implement all three in Python
- Implement all three in Haskell or Racket
- Think about edge cases (empty list, n too large, etc.)

---

## Exercise 4: Recursive String Operations

**Difficulty:** Medium

Implement these string functions recursively:

1. `count_char(string, char)` - Count occurrences of a character
2. `remove_char(string, char)` - Remove all occurrences of a character
3. `replace_char(string, old, new)` - Replace all old with new

**Example:**
```python
count_char("hello", 'l')  # 2
remove_char("hello", 'l')  # "heo"
replace_char("hello", 'l', 'r')  # "herro"
```

---

## Exercise 5: Sum of Digits

**Difficulty:** Medium

Write a recursive function to sum the digits of a number.

**Example:**
```
sum_digits(1234) = 1 + 2 + 3 + 4 = 10
sum_digits(987) = 9 + 8 + 7 = 24
```

**Hint:** Use `n % 10` to get last digit, `n // 10` to remove it

**Tasks:**
1. Implement in Python
2. Implement in a statically-typed language (Rust, Haskell, or Java)
3. Test with large numbers

---

## Exercise 6: Merge Sort

**Difficulty:** Hard

Implement merge sort using recursion.

**Algorithm:**
1. If list has 0 or 1 elements, it's sorted (base case)
2. Split list in half
3. Recursively sort each half
4. Merge the sorted halves

**Tasks:**
1. Implement in Python
2. Implement in Haskell (very elegant!)
3. Compare with quicksort - which is better when?

**Bonus:** Analyze time complexity

---

## Exercise 7: Flatten Nested List

**Difficulty:** Hard

Write a recursive function to flatten a nested list.

**Example:**
```python
flatten([1, [2, 3], [4, [5, 6]]])
# Returns: [1, 2, 3, 4, 5, 6]
```

**Tasks:**
1. Implement in Python
2. Handle arbitrary nesting depth
3. How do you check if something is a list vs a single element?

---

## Exercise 8: Generate Permutations

**Difficulty:** Hard

Generate all permutations of a list recursively.

**Example:**
```
permutations([1, 2, 3])
# Returns: [[1,2,3], [1,3,2], [2,1,3], [2,3,1], [3,1,2], [3,2,1]]
```

**Algorithm:**
- Base case: permutations of [] is [[]]
- Recursive: for each element, generate permutations of remaining elements

**Tasks:**
1. Implement in Python or JavaScript
2. Implement in Haskell (hint: use list comprehensions!)
3. How many permutations of n elements? (n!)

---

## Exercise 9: Tree Problems

**Difficulty:** Medium to Hard

Given a binary tree, implement:

1. `count_nodes(tree)` - Count total nodes
2. `count_leaves(tree)` - Count leaf nodes (no children)
3. `tree_contains(tree, value)` - Check if value exists
4. `tree_paths(tree)` - Return all root-to-leaf paths

**Tasks:**
- Implement all four functions
- Test with the example tree from the lesson
- Draw the recursion tree for `tree_paths`

---

## Exercise 10: Greatest Common Divisor (GCD)

**Difficulty:** Medium

Implement Euclid's algorithm for GCD recursively.

**Algorithm:**
```
gcd(a, 0) = a
gcd(a, b) = gcd(b, a mod b)
```

**Example:**
```
gcd(48, 18) = gcd(18, 12) = gcd(12, 6) = gcd(6, 0) = 6
```

**Tasks:**
1. Implement in Python
2. Implement in a functional language
3. Trace execution for gcd(100, 35)
4. Why does this algorithm work?

---

## Exercise 11: Recursion vs Iteration

**Difficulty:** Medium (Conceptual)

For each problem, implement BOTH recursive and iterative solutions:

1. Sum of numbers 1 to n
2. Reverse a string
3. Check if number is prime (trial division)

**Tasks:**
- Compare code clarity
- Compare performance (use timing)
- Which version is easier to understand?
- Which version is faster?

**Questions:**
- When is recursion better than iteration?
- When is iteration better than recursion?

---

## Exercise 12: Tail Recursion Conversion

**Difficulty:** Hard

Convert these regular recursive functions to tail-recursive versions:

1. Sum of list
2. Reverse of list
3. Factorial

**Example:**
```python
# Regular recursion
def sum_list(lst):
    if not lst: return 0
    return lst[0] + sum_list(lst[1:])

# Tail recursive (with accumulator)
def sum_list_tail(lst, acc=0):
    if not lst: return acc
    return sum_list_tail(lst[1:], acc + lst[0])
```

**Tasks:**
- Convert all three to tail-recursive
- In which languages does this improve performance?
- Test in Python vs Haskell vs Racket

---

## Exercise 13: Recursive Tree Construction

**Difficulty:** Hard

Build a binary search tree (BST) recursively.

**Operations:**
1. `insert(tree, value)` - Insert value maintaining BST property
2. `search(tree, value)` - Find if value exists
3. `min_value(tree)` - Find minimum value
4. `max_value(tree)` - Find maximum value

**BST Property:** Left subtree < root < right subtree

**Tasks:**
- Implement all operations
- Build a BST from [5, 3, 7, 1, 4, 6, 9]
- Verify BST property

---

## Exercise 14: Pascal's Triangle

**Difficulty:** Medium

Generate Pascal's triangle recursively.

**Pattern:**
```
Row 0:           1
Row 1:         1   1
Row 2:       1   2   1
Row 3:     1   3   3   1
Row 4:   1   4   6   4   1
```

**Formula:** `pascal(row, col) = pascal(row-1, col-1) + pascal(row-1, col)`

**Tasks:**
1. Implement recursive function to get value at (row, col)
2. Generate entire row n
3. Add memoization to improve performance

---

## Exercise 15: Recursive Backtracking - N-Queens

**Difficulty:** Very Hard (Challenge)

Solve the N-Queens problem: place N queens on an N×N chessboard so no two queens attack each other.

**Approach:**
- Place queens row by row
- For each row, try each column
- Check if placement is safe
- Recursively place remaining queens
- Backtrack if no solution

**Tasks:**
1. Implement solution for 4×4 board
2. Find all solutions (not just one)
3. Try 8×8 board (classic problem)

---

## Challenge Projects

### Challenge 1: Recursive Descent Calculator

Build a calculator that evaluates mathematical expressions recursively:

**Example:**
```
evaluate("2 + 3 * 4") = 14
evaluate("(2 + 3) * 4") = 20
evaluate("10 - 2 * 3") = 4
```

**Requirements:**
- Handle +, -, *, / operators
- Handle parentheses
- Respect operator precedence
- Use recursive descent parsing

### Challenge 2: Maze Solver

Write a recursive backtracking maze solver:

**Input:** 2D grid where 0 = path, 1 = wall
**Output:** Path from start to end

**Algorithm:**
1. If at end, return success
2. Try each direction (up, down, left, right)
3. If direction is valid, recursively explore
4. Backtrack if dead end

### Challenge 3: JSON Parser

Build a simple recursive JSON parser:

**Input:** JSON string
**Output:** Parsed data structure

**Handle:**
- Objects: `{"key": "value"}`
- Arrays: `[1, 2, 3]`
- Nested structures
- Strings, numbers, booleans

---

## Reflection Questions

1. **What makes a good recursive problem?**
   - What characteristics suggest recursion is appropriate?

2. **Base case mistakes:**
   - What happens if you forget the base case?
   - What happens if base case never reached?

3. **Stack overflow:**
   - How deep can recursion go in different languages?
   - How to avoid stack overflow?

4. **Tail recursion:**
   - Which languages optimize tail calls?
   - How to convert regular recursion to tail recursion?

5. **Performance:**
   - When is recursion slower than iteration?
   - How does memoization help?

6. **Paradigm differences:**
   - How does recursion differ in functional vs imperative languages?
   - Which paradigm makes recursion more natural?

---

## Going Further

- **Study:** Dynamic programming (memoized recursion)
- **Explore:** Continuation-passing style (CPS)
- **Learn:** Trampolining (avoiding stack overflow)
- **Practice:** More backtracking problems (Sudoku, graph coloring)
- **Research:** Recursive data structures (linked lists, trees, graphs)

Remember: Recursion is a powerful tool, but not always the best tool. Choose wisely!
