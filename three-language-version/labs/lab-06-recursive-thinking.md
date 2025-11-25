# Lab 6: Recursive Thinking

**Quarter 1, Week 6**
**Duration:** 90 minutes
**Format:** Pair programming

## Overview

Recursion is one of the most powerful concepts in computer science. A function that calls itself might seem strange at first, but it's a natural way to solve many problems. This lab will help you think recursively.

## Objectives

By the end of this lab, you will:
- [ ] Understand the anatomy of a recursive function
- [ ] Identify base cases and recursive cases
- [ ] Trace through recursive calls
- [ ] Convert between iterative and recursive solutions
- [ ] Appreciate why Haskell loves recursion

## Setup

- Partner up
- Create folder: `lab06-recursion/`
- Paper and pencil for tracing (seriously - it helps!)

---

## Part 1: Anatomy of Recursion (20 minutes)

### Activity 1.1: The Classic - Factorial

Factorial: `5! = 5 × 4 × 3 × 2 × 1 = 120`

**Iterative approach:**
```python
def factorial_iter(n):
    result = 1
    for i in range(1, n + 1):
        result *= i
    return result
```

**Recursive approach:**
```python
def factorial(n):
    # Base case: when to stop
    if n <= 1:
        return 1
    # Recursive case: call ourselves
    return n * factorial(n - 1)
```

**The two essential parts:**
1. **Base case**: The condition that stops recursion
2. **Recursive case**: The function calling itself with a "smaller" problem

### Activity 1.2: Trace It!

Draw the call stack for `factorial(4)`:

```
factorial(4)
    → 4 * factorial(3)
        → 3 * factorial(2)
            → 2 * factorial(1)
                → 1  (base case!)
            ← 2 * 1 = 2
        ← 3 * 2 = 6
    ← 4 * 6 = 24
← 24
```

**Your turn:** Trace `factorial(5)` on paper. Show each call and return.

### Activity 1.3: Implement in All Languages

**Python** (already done above)

**C++:**
```cpp
#include <iostream>
using namespace std;

int factorial(int n) {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}

int main() {
    cout << factorial(5) << endl;  // 120
    return 0;
}
```

**Haskell** (most natural!):
```haskell
factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n - 1)

main :: IO ()
main = print (factorial 5)  -- 120
```

**Notice:** Haskell uses pattern matching for base cases - very clean!

### ✅ Checkpoint 1

Verify with partner:
- [ ] Can explain base case and recursive case
- [ ] Traced factorial(5) correctly on paper
- [ ] Factorial works in at least 2 languages

---

## Part 2: More Recursive Patterns (25 minutes)

### Activity 2.1: Fibonacci

The Fibonacci sequence: 0, 1, 1, 2, 3, 5, 8, 13, 21...

Each number is the sum of the two before it.

**Python:**
```python
def fib(n):
    if n <= 0:
        return 0
    if n == 1:
        return 1
    return fib(n - 1) + fib(n - 2)

# Test
for i in range(10):
    print(fib(i), end=" ")
# 0 1 1 2 3 5 8 13 21 34
```

**Trace `fib(4)`:**
```
fib(4)
├── fib(3)
│   ├── fib(2)
│   │   ├── fib(1) → 1
│   │   └── fib(0) → 0
│   │   = 1
│   └── fib(1) → 1
│   = 2
└── fib(2)
    ├── fib(1) → 1
    └── fib(0) → 0
    = 1
= 3
```

**Discussion:** Notice how `fib(2)` is calculated twice. This is inefficient! (We'll learn memoization later.)

### Activity 2.2: Sum of a List

Calculate the sum of a list recursively.

**Python:**
```python
def sum_list(lst):
    if len(lst) == 0:  # Base case: empty list
        return 0
    return lst[0] + sum_list(lst[1:])  # First element + sum of rest

print(sum_list([1, 2, 3, 4, 5]))  # 15
```

**Haskell** (very natural for lists!):
```haskell
sumList :: [Int] -> Int
sumList [] = 0                      -- Base case
sumList (x:xs) = x + sumList xs     -- Head + sum of tail

main :: IO ()
main = print (sumList [1, 2, 3, 4, 5])  -- 15
```

**Your task:** Implement in C++ using `vector`.

### Activity 2.3: Length of a List

Count elements without using built-in `len`/`length`.

**Python:**
```python
def length(lst):
    if lst == []:
        return 0
    return 1 + length(lst[1:])
```

**Haskell:**
```haskell
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs
```

**Your turn:** Implement `reverse` recursively.

```python
# Hint: reverse([1,2,3,4]) = reverse([2,3,4]) + [1]
def reverse(lst):
    # Your code here
    pass
```

### Activity 2.4: Power Function

Calculate `x^n` recursively.

**Naive approach:**
```python
def power(x, n):
    if n == 0:
        return 1
    return x * power(x, n - 1)
```

**Efficient approach** (using the fact that `x^n = (x^(n/2))^2`):
```python
def power_fast(x, n):
    if n == 0:
        return 1
    if n % 2 == 0:
        half = power_fast(x, n // 2)
        return half * half
    else:
        return x * power_fast(x, n - 1)
```

**Test both:** Calculate `2^10`. How many multiplications does each use?

### ✅ Checkpoint 2

Verify:
- [ ] Fibonacci works and you understand the double recursion
- [ ] Implemented at least one list function (sum, length, or reverse)
- [ ] Understand the power function

---

## Part 3: Recursive Data Structures (20 minutes)

### Activity 3.1: Binary Tree

Trees are naturally recursive: a tree is either empty or a node with two subtrees.

**Python:**
```python
class TreeNode:
    def __init__(self, value, left=None, right=None):
        self.value = value
        self.left = left
        self.right = right

# Build a tree:
#       1
#      / \
#     2   3
#    / \
#   4   5

tree = TreeNode(1,
    TreeNode(2,
        TreeNode(4),
        TreeNode(5)),
    TreeNode(3))
```

**Recursive operations:**
```python
def tree_sum(node):
    if node is None:
        return 0
    return node.value + tree_sum(node.left) + tree_sum(node.right)

def tree_height(node):
    if node is None:
        return 0
    return 1 + max(tree_height(node.left), tree_height(node.right))

def count_nodes(node):
    if node is None:
        return 0
    return 1 + count_nodes(node.left) + count_nodes(node.right)

print(f"Sum: {tree_sum(tree)}")        # 15
print(f"Height: {tree_height(tree)}")  # 3
print(f"Nodes: {count_nodes(tree)}")   # 5
```

**Haskell** (trees are a natural fit!):
```haskell
data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving Show

tree :: Tree Int
tree = Node 1
    (Node 2 (Node 4 Empty Empty) (Node 5 Empty Empty))
    (Node 3 Empty Empty)

treeSum :: Tree Int -> Int
treeSum Empty = 0
treeSum (Node v left right) = v + treeSum left + treeSum right

treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node _ left right) = 1 + max (treeHeight left) (treeHeight right)

main :: IO ()
main = do
    print (treeSum tree)    -- 15
    print (treeHeight tree) -- 3
```

### Activity 3.2: Tree Traversals

Three ways to visit all nodes:

**In-order** (Left, Node, Right):
```python
def inorder(node):
    if node is None:
        return []
    return inorder(node.left) + [node.value] + inorder(node.right)
```

**Pre-order** (Node, Left, Right):
```python
def preorder(node):
    if node is None:
        return []
    return [node.value] + preorder(node.left) + preorder(node.right)
```

**Post-order** (Left, Right, Node):
```python
def postorder(node):
    if node is None:
        return []
    return postorder(node.left) + postorder(node.right) + [node.value]
```

**Test on our tree:**
```python
print(f"In-order: {inorder(tree)}")     # [4, 2, 5, 1, 3]
print(f"Pre-order: {preorder(tree)}")   # [1, 2, 4, 5, 3]
print(f"Post-order: {postorder(tree)}") # [4, 5, 2, 3, 1]
```

### ✅ Checkpoint 3

Verify:
- [ ] Built the tree structure
- [ ] At least one tree function works (sum, height, or count)
- [ ] Can explain the three traversal orders

---

## Part 4: Recursion vs Iteration (15 minutes)

### Activity 4.1: Convert Iterative to Recursive

**Iterative:**
```python
def count_down_iter(n):
    while n > 0:
        print(n)
        n -= 1
    print("Blast off!")
```

**Convert to recursive:**
```python
def count_down(n):
    if n <= 0:
        print("Blast off!")
    else:
        print(n)
        count_down(n - 1)
```

**Your turn:** Convert this iterative function to recursive:

```python
def find_max_iter(lst):
    if len(lst) == 0:
        return None
    max_val = lst[0]
    for x in lst[1:]:
        if x > max_val:
            max_val = x
    return max_val
```

### Activity 4.2: Convert Recursive to Iterative

**Recursive:**
```python
def gcd(a, b):
    if b == 0:
        return a
    return gcd(b, a % b)
```

**Convert to iterative:**
```python
def gcd_iter(a, b):
    while b != 0:
        a, b = b, a % b
    return a
```

### Activity 4.3: When to Use Which?

Discuss with your partner:

| Scenario | Better Approach | Why? |
|----------|-----------------|------|
| Processing a tree | Recursive | Trees are recursive structures |
| Simple counting | Iterative | Simpler, no stack overhead |
| Divide-and-conquer | Recursive | Natural fit |
| Large n (millions) | Iterative | Avoid stack overflow |

---

## Part 5: Tail Recursion (10 minutes)

### Activity 5.1: The Stack Problem

Regular recursion builds up the stack:
```python
# Each call waits for the next to return
factorial(5)
    5 * factorial(4)
        4 * factorial(3)
            3 * factorial(2)
                2 * factorial(1)
                    1
                2
            6
        24
    120
```

For large n, this can cause stack overflow!

### Activity 5.2: Tail Recursion

In **tail recursion**, the recursive call is the LAST thing that happens:

```python
def factorial_tail(n, accumulator=1):
    if n <= 1:
        return accumulator
    return factorial_tail(n - 1, n * accumulator)
```

**Trace:**
```
factorial_tail(5, 1)
factorial_tail(4, 5)      # 5 * 1
factorial_tail(3, 20)     # 4 * 5
factorial_tail(2, 60)     # 3 * 20
factorial_tail(1, 120)    # 2 * 60
→ 120
```

No stack buildup! (In languages that optimize tail calls.)

**Haskell** (tail-call optimized!):
```haskell
factorialTail :: Int -> Int -> Int
factorialTail n acc
    | n <= 1    = acc
    | otherwise = factorialTail (n - 1) (n * acc)

factorial' :: Int -> Int
factorial' n = factorialTail n 1
```

**Discussion:** Why doesn't Python optimize tail calls? (Design choice by Guido - prefers explicit loops.)

---

## Challenges (If Time Permits)

### Challenge 1: Recursive Binary Search

```python
def binary_search(lst, target, low=0, high=None):
    if high is None:
        high = len(lst) - 1

    if low > high:
        return -1  # Not found

    mid = (low + high) // 2
    if lst[mid] == target:
        return mid
    elif lst[mid] < target:
        return binary_search(lst, target, mid + 1, high)
    else:
        return binary_search(lst, target, low, mid - 1)

# Test
nums = [1, 3, 5, 7, 9, 11, 13]
print(binary_search(nums, 7))   # 3
print(binary_search(nums, 6))   # -1
```

### Challenge 2: Tower of Hanoi

Classic recursion problem!

```python
def hanoi(n, source, target, auxiliary):
    if n == 1:
        print(f"Move disk 1 from {source} to {target}")
        return
    hanoi(n - 1, source, auxiliary, target)
    print(f"Move disk {n} from {source} to {target}")
    hanoi(n - 1, auxiliary, target, source)

hanoi(3, 'A', 'C', 'B')
```

### Challenge 3: Generate All Subsets

```python
def subsets(lst):
    if len(lst) == 0:
        return [[]]
    first = lst[0]
    rest_subsets = subsets(lst[1:])
    with_first = [[first] + s for s in rest_subsets]
    return rest_subsets + with_first

print(subsets([1, 2, 3]))
# [[], [3], [2], [2, 3], [1], [1, 3], [1, 2], [1, 2, 3]]
```

---

## Wrap-Up

**Key takeaways:**

1. **Every recursive function needs a base case** - otherwise infinite loop!
2. **Think about the smaller problem** - how does solving a smaller version help?
3. **Trace on paper** - drawing the call stack helps understanding
4. **Trees and divide-and-conquer** are natural fits for recursion
5. **Haskell loves recursion** - it's the primary way to repeat in functional programming
6. **Tail recursion** avoids stack overflow (in languages that support it)

**Mental model:** "Trust that the recursive call works, then build on it."

**Next lab:** Project showcase - time to present your work!
