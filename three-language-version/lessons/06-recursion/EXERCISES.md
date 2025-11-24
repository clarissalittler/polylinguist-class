# Lesson 6: Recursion - Exercises

## Instructions

These exercises will help you master recursive thinking across Python, C++, and Haskell. Remember: every recursive function needs a base case and must make progress toward it!

---

## Exercise 1: Power Function (Warmup)

**Difficulty:** Easy

Implement a recursive function to calculate x^n (x to the power of n).

**Base case:** `x^0 = 1`
**Recursive case:** `x^n = x * x^(n-1)`

**Tasks:**
1. Implement in all three languages
2. Test with: `power(2, 10)` should return `1024`
3. Handle negative exponents (bonus)

**Questions:**
- What is the time complexity?
- Can you make this more efficient using x^n = (x^(n/2))^2?

---

## Exercise 2: List Operations

**Difficulty:** Medium

Implement these recursive list operations in all three languages:

1. `contains(list, element)` - Check if element is in list
2. `count(list, element)` - Count occurrences of element
3. `nth(list, n)` - Get the nth element (0-indexed)
4. `maximum(list)` - Find maximum element

**Edge cases to consider:**
- Empty list
- Index out of bounds
- Single element list

---

## Exercise 3: String Recursion

**Difficulty:** Medium

Implement recursively:

1. `countChar(string, char)` - Count occurrences of a character
2. `removeChar(string, char)` - Remove all occurrences of a character
3. `isPalindrome(string)` - Check if string reads same forwards and backwards

**Example:**
```
countChar("hello", 'l')  → 2
removeChar("hello", 'l')  → "heo"
isPalindrome("racecar")  → true
```

---

## Exercise 4: Sum of Digits

**Difficulty:** Medium

Write a recursive function to sum the digits of a number.

**Example:**
```
sumDigits(1234) = 1 + 2 + 3 + 4 = 10
sumDigits(987) = 9 + 8 + 7 = 24
```

**Hint:** Use `n % 10` to get last digit, `n / 10` to remove it

**Implement in:** All three languages

---

## Exercise 5: Fibonacci with Memoization

**Difficulty:** Medium to Hard

Implement Fibonacci with memoization to make it efficient.

**Tasks:**
1. First implement naive Fibonacci
2. Add memoization
3. Compare performance: time fib(35) with and without memoization
4. Implement in all three languages

**Questions:**
- What is the time complexity without memoization?
- What is the time complexity with memoization?

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
2. Implement in C++
3. Implement in Haskell (will be very elegant!)
4. Analyze time complexity: O(n log n)

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
1. Implement in Python (handle arbitrary nesting)
2. In C++, use std::variant or nested vectors
3. In Haskell, use custom data type

---

## Exercise 8: Generate Permutations

**Difficulty:** Hard

Generate all permutations of a list recursively.

**Example:**
```
permutations([1, 2, 3])
→ [[1,2,3], [1,3,2], [2,1,3], [2,3,1], [3,1,2], [3,2,1]]
```

**Algorithm:**
- Base case: permutations of [] is [[]]
- Recursive: for each element, generate permutations of remaining elements

**Implement in:** All three languages

**Questions:**
- How many permutations of n elements? (n!)
- What is the time complexity?

---

## Exercise 9: Binary Tree Operations

**Difficulty:** Medium to Hard

Given a binary tree, implement recursively:

1. `countNodes(tree)` - Count total nodes
2. `countLeaves(tree)` - Count leaf nodes (no children)
3. `height(tree)` - Find tree height
4. `treeContains(tree, value)` - Check if value exists
5. `sumTree(tree)` - Sum all values

**Tasks:**
- Implement all functions in all three languages
- Test with various tree shapes
- Draw recursion tree for visualization

---

## Exercise 10: Greatest Common Divisor

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
1. Implement in all three languages
2. Trace execution for gcd(100, 35)
3. Why does this algorithm work?

---

## Exercise 11: Recursion vs Iteration

**Difficulty:** Medium (Comparative)

For each problem, implement BOTH recursive and iterative solutions:

1. Sum of numbers 1 to n
2. Reverse a list
3. Check if number is prime (trial division)

**Tasks:**
- Implement both versions in Python and C++
- Compare code clarity
- Compare performance (use timing)
- Which version is easier to understand?

**Questions:**
- When is recursion better than iteration?
- When is iteration better than recursion?
- How does Haskell handle this (no iteration)?

---

## Exercise 12: Tail Recursion Conversion

**Difficulty:** Hard

Convert these regular recursive functions to tail-recursive versions using an accumulator:

1. Sum of list
2. Reverse of list
3. Factorial
4. Length of list

**Example:**
```python
# Regular recursion
def sum_list(lst):
    if not lst:
        return 0
    return lst[0] + sum_list(lst[1:])

# Tail recursive (with accumulator)
def sum_list_tail(lst, acc=0):
    if not lst:
        return acc
    return sum_list_tail(lst[1:], acc + lst[0])
```

**Questions:**
- In which languages does this improve performance?
- Test in Python vs C++ (with -O2) vs Haskell
- Does tail recursion use less stack space?

---

## Exercise 13: Quicksort

**Difficulty:** Hard

Implement quicksort recursively.

**Algorithm:**
1. If list has 0 or 1 elements, return it (sorted)
2. Choose pivot (e.g., middle element)
3. Partition: elements < pivot, = pivot, > pivot
4. Recursively sort left and right partitions
5. Concatenate: sorted_left + pivot + sorted_right

**Tasks:**
- Implement in all three languages
- Compare with merge sort
- Analyze time complexity (average vs worst case)

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

## Exercise 15: Tower of Hanoi

**Difficulty:** Hard

Solve the Tower of Hanoi puzzle recursively.

**Rules:**
- Move n disks from source peg to target peg
- Can use auxiliary peg
- Only move one disk at a time
- Never place larger disk on smaller disk

**Algorithm:**
1. Move n-1 disks from source to auxiliary (using target)
2. Move largest disk from source to target
3. Move n-1 disks from auxiliary to target (using source)

**Tasks:**
- Implement in all three languages
- Print each move
- Verify: n disks requires 2^n - 1 moves

---

## Challenge Projects

### Challenge 1: N-Queens Problem

Solve the N-Queens problem using recursive backtracking:
- Place N queens on an N×N chessboard
- No two queens attack each other
- Find all solutions

**Approach:**
- Place queens row by row
- For each row, try each column
- Check if placement is safe (no attacks)
- Recursively place remaining queens
- Backtrack if no solution

### Challenge 2: Recursive Descent Calculator

Build a calculator that evaluates mathematical expressions:

**Example:**
```
evaluate("2 + 3 * 4") = 14
evaluate("(2 + 3) * 4") = 20
```

**Requirements:**
- Handle +, -, *, / operators
- Handle parentheses
- Respect operator precedence
- Use recursive descent parsing

### Challenge 3: Maze Solver

Write a recursive backtracking maze solver:

**Input:** 2D grid (0 = path, 1 = wall)
**Output:** Path from start to end

**Algorithm:**
1. If at end, return success
2. Try each direction (up, down, left, right)
3. If direction is valid and unvisited, recursively explore
4. Mark current cell as part of path
5. Backtrack if dead end

---

## Reflection Questions

1. **Base Cases:**
   - What happens if you forget the base case?
   - What happens if the base case is never reached?

2. **Stack Overflow:**
   - How deep can recursion go in Python? C++? Haskell?
   - How to avoid stack overflow?

3. **Tail Recursion:**
   - Which languages optimize tail calls?
   - How to convert regular recursion to tail recursion?

4. **Performance:**
   - When is recursion slower than iteration?
   - When is recursion faster or clearer?
   - How does memoization help?

5. **Paradigm Differences:**
   - How does recursion differ in imperative vs functional languages?
   - Why does Haskell use recursion instead of loops?
   - Which paradigm makes recursion more natural?

---

## Additional Resources

- **Python:** [Official Recursion Tutorial](https://docs.python.org/3/tutorial/)
- **C++:** Recursion with STL and optimization
- **Haskell:** Learn You a Haskell - Recursion chapter

Remember: Recursion is a powerful tool, but not always the best tool. Choose wisely based on the problem and language!
