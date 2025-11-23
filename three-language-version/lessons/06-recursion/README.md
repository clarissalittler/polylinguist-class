# Lesson 6: Recursion

## Learning Objectives

By the end of this lesson, you will:
- Understand how recursion works
- Recognize base cases and recursive cases
- Implement recursive algorithms in Python, C++, and Haskell
- Compare recursion with iteration
- Understand tail recursion and its optimization
- See how different paradigms approach recursion

## What is Recursion?

**Recursion** is when a function calls itself to solve a problem by breaking it down into smaller, similar subproblems.

### Key Components

Every recursive function needs:

1. **Base Case**: The stopping condition that prevents infinite recursion
2. **Recursive Case**: The part that breaks the problem down and calls itself
3. **Progress**: Each recursive call must move toward the base case

### Simple Example: Countdown

**Python:**
```python
def countdown(n):
    if n <= 0:           # Base case
        print("Blast off!")
    else:                # Recursive case
        print(n)
        countdown(n - 1) # Recursive call (moving toward base case)
```

**C++:**
```cpp
void countdown(int n) {
    if (n <= 0) {       // Base case
        std::cout << "Blast off!" << std::endl;
    } else {            // Recursive case
        std::cout << n << std::endl;
        countdown(n - 1);  // Recursive call
    }
}
```

**Haskell:**
```haskell
countdown :: Int -> IO ()
countdown n
    | n <= 0    = putStrLn "Blast off!"  -- Base case
    | otherwise = do                      -- Recursive case
        print n
        countdown (n - 1)
```

## Recursion vs Iteration

Many problems can be solved with either recursion or iteration (loops).

### Factorial: Iterative Approach

**Python:**
```python
def factorial_iterative(n):
    result = 1
    for i in range(1, n + 1):
        result *= i
    return result
```

**C++:**
```cpp
int factorial_iterative(int n) {
    int result = 1;
    for (int i = 1; i <= n; ++i) {
        result *= i;
    }
    return result;
}
```

**Haskell:**
```haskell
-- Haskell prefers recursion, but can use foldl
factorialIterative :: Int -> Int
factorialIterative n = foldl (*) 1 [1..n]
```

### Factorial: Recursive Approach

**Python:**
```python
def factorial_recursive(n):
    if n <= 1:              # Base case
        return 1
    else:                   # Recursive case
        return n * factorial_recursive(n - 1)
```

**C++:**
```cpp
int factorial_recursive(int n) {
    if (n <= 1) {           // Base case
        return 1;
    }
    return n * factorial_recursive(n - 1);  // Recursive case
}
```

**Haskell:**
```haskell
factorial :: Int -> Int
factorial n
    | n <= 1    = 1                    -- Base case
    | otherwise = n * factorial (n - 1) -- Recursive case
```

**Trace of `factorial(5)`:**
```
factorial(5) = 5 * factorial(4)
             = 5 * (4 * factorial(3))
             = 5 * (4 * (3 * factorial(2)))
             = 5 * (4 * (3 * (2 * factorial(1))))
             = 5 * (4 * (3 * (2 * 1)))
             = 5 * (4 * (3 * 2))
             = 5 * (4 * 6)
             = 5 * 24
             = 120
```

## Classic Recursive Problems

### 1. Fibonacci Sequence

Each number is the sum of the two preceding ones: 0, 1, 1, 2, 3, 5, 8, 13, ...

**Mathematical Definition:**
```
fib(0) = 0
fib(1) = 1
fib(n) = fib(n-1) + fib(n-2)
```

**Python:**
```python
def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n-1) + fibonacci(n-2)
```

**C++:**
```cpp
int fibonacci(int n) {
    if (n <= 1) {
        return n;
    }
    return fibonacci(n - 1) + fibonacci(n - 2);
}
```

**Haskell:**
```haskell
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)
```

**Problem:** This is exponentially slow! Fibonacci(40) takes billions of operations.

### 2. Sum of List

**Python:**
```python
def sum_list(lst):
    if not lst:                    # Base case: empty list
        return 0
    return lst[0] + sum_list(lst[1:])  # First + sum of rest
```

**C++:**
```cpp
int sumList(const std::vector<int>& vec, size_t index = 0) {
    if (index >= vec.size()) {     // Base case
        return 0;
    }
    return vec[index] + sumList(vec, index + 1);
}
```

**Haskell:**
```haskell
sumList :: [Int] -> Int
sumList [] = 0                     -- Base case
sumList (x:xs) = x + sumList xs    -- First + sum of rest
```

### 3. Binary Search

Recursively search a sorted array.

**Python:**
```python
def binary_search(arr, target, low=0, high=None):
    if high is None:
        high = len(arr) - 1

    if low > high:
        return -1  # Not found

    mid = (low + high) // 2

    if arr[mid] == target:
        return mid
    elif arr[mid] > target:
        return binary_search(arr, target, low, mid - 1)
    else:
        return binary_search(arr, target, mid + 1, high)
```

**C++:**
```cpp
int binarySearch(const std::vector<int>& arr, int target,
                 int low, int high) {
    if (low > high) {
        return -1;  // Not found
    }

    int mid = low + (high - low) / 2;  // Avoid overflow

    if (arr[mid] == target) {
        return mid;
    } else if (arr[mid] > target) {
        return binarySearch(arr, target, low, mid - 1);
    } else {
        return binarySearch(arr, target, mid + 1, high);
    }
}
```

**Haskell:**
```haskell
binarySearch :: [Int] -> Int -> Int -> Int -> Int
binarySearch arr target low high
    | low > high = -1
    | arr !! mid == target = mid
    | arr !! mid > target = binarySearch arr target low (mid - 1)
    | otherwise = binarySearch arr target (mid + 1) high
    where mid = (low + high) `div` 2
```

## Tail Recursion

**Tail recursion** is when the recursive call is the last operation in the function.

### Not Tail Recursive (Regular Recursion)

**Python:**
```python
def factorial(n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)  # Must multiply after recursive call
```

**Problem:** The multiplication happens AFTER the recursive call returns, so the stack frame must be preserved.

### Tail Recursive

**Python:**
```python
def factorial_tail(n, accumulator=1):
    if n <= 1:
        return accumulator
    return factorial_tail(n - 1, n * accumulator)  # Last operation is the call
```

**C++:**
```cpp
int factorialTail(int n, int accumulator = 1) {
    if (n <= 1) {
        return accumulator;
    }
    return factorialTail(n - 1, n * accumulator);
}
```

**Haskell:**
```haskell
factorialTail :: Int -> Int -> Int
factorialTail n acc
    | n <= 1    = acc
    | otherwise = factorialTail (n - 1) (n * acc)
```

### Why Tail Recursion Matters

**Tail Call Optimization (TCO):**
- Compilers can convert tail-recursive functions to loops
- Prevents stack overflow for deep recursion
- Uses constant stack space

**Which languages optimize tail calls?**
- **Haskell:** Yes, always
- **C++:** Sometimes (with optimization flags)
- **Python:** No (design decision)

**Implications:**
- In Haskell, prefer tail recursion for efficiency
- In Python, very deep recursion requires iteration
- In C++, depends on compiler optimization

## Recursion in Different Paradigms

### Python: Both Recursion and Iteration

```python
# Recursion available but limited
import sys
print(sys.getrecursionlimit())  # Usually 1000

# Can increase, but not recommended
# sys.setrecursionlimit(10000)

# Iteration often preferred for simple loops
for i in range(10):
    print(i)

# Recursion for tree/graph problems
def traverse_tree(node):
    if node is None:
        return
    process(node)
    traverse_tree(node.left)
    traverse_tree(node.right)
```

**Characteristics:**
- Default recursion limit ~1000
- No tail call optimization
- Recursion used for complex structures
- Iteration preferred for simple cases

### C++: Recursion with Caution

```cpp
// Recursion supported, but stack limited
// Typical stack: 1-8 MB

// Simple recursion
int factorial(int n) {
    return (n <= 1) ? 1 : n * factorial(n - 1);
}

// Can compile with -O2 for potential TCO
// g++ -O2 program.cpp

// Iteration often preferred
for (int i = 0; i < 10; ++i) {
    std::cout << i << std::endl;
}
```

**Characteristics:**
- Stack size limited (system dependent)
- Possible TCO with optimizations
- Iteration often preferred for performance
- Recursion used for divide-and-conquer

### Haskell: Recursion is Primary

```haskell
-- No loops, only recursion and higher-order functions
-- Tail call optimization guaranteed

-- Pattern matching makes base cases clean
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Infinite recursion possible (lazy evaluation)
naturals :: [Int]
naturals = [1..]

-- Tail recursion preferred
factorialTail :: Int -> Int
factorialTail n = go n 1
  where
    go 0 acc = acc
    go n acc = go (n - 1) (n * acc)
```

**Characteristics:**
- Recursion is the primary way to repeat
- Guaranteed tail call optimization
- Pattern matching for elegant base cases
- Lazy evaluation enables infinite structures

## Common Pitfalls

### 1. Missing Base Case

```python
def infinite(n):
    return infinite(n - 1)  # Never stops! Stack overflow!
```

### 2. Not Making Progress

```python
def stuck(n):
    if n == 0:
        return 0
    return stuck(n)  # Doesn't get closer to base case!
```

### 3. Inefficient Tree Recursion

```python
def fib(n):
    if n <= 1:
        return n
    return fib(n-1) + fib(n-2)  # Recalculates same values many times!
```

**Call tree for fib(5):**
```
                    fib(5)
                   /      \
              fib(4)        fib(3)
             /     \        /     \
        fib(3)   fib(2)  fib(2)  fib(1)
        /   \    /   \   /   \
    fib(2) fib(1) ...  ...  ...
```

Many redundant calculations! fib(2) computed 3 times, fib(3) twice.

**Solution: Memoization**

**Python:**
```python
def fib_memo(n, memo=None):
    if memo is None:
        memo = {}
    if n in memo:
        return memo[n]
    if n <= 1:
        return n
    memo[n] = fib_memo(n-1, memo) + fib_memo(n-2, memo)
    return memo[n]
```

**C++:**
```cpp
#include <unordered_map>

int fibMemo(int n, std::unordered_map<int, int>& memo) {
    if (memo.find(n) != memo.end()) {
        return memo[n];
    }
    if (n <= 1) {
        return n;
    }
    memo[n] = fibMemo(n - 1, memo) + fibMemo(n - 2, memo);
    return memo[n];
}
```

**Haskell:**
```haskell
-- Haskell can use arrays for memoization
import Data.Array

fibMemo :: Int -> Int
fibMemo n = fibs ! n
  where
    fibs = array (0, n) [(i, fib i) | i <- [0..n]]
    fib 0 = 0
    fib 1 = 1
    fib i = fibs ! (i - 1) + fibs ! (i - 2)
```

## Tree Recursion Example

Binary tree traversal is naturally recursive.

**Tree Structure:**
```
      5
     / \
    3   8
   / \   \
  1   4   9
```

**Python:**
```python
class TreeNode:
    def __init__(self, value, left=None, right=None):
        self.value = value
        self.left = left
        self.right = right

def inorder(node):
    """Left, Root, Right"""
    if node is None:
        return []
    return inorder(node.left) + [node.value] + inorder(node.right)
```

**C++:**
```cpp
struct TreeNode {
    int value;
    TreeNode* left;
    TreeNode* right;
    TreeNode(int val) : value(val), left(nullptr), right(nullptr) {}
};

void inorder(TreeNode* node, std::vector<int>& result) {
    if (node == nullptr) {
        return;
    }
    inorder(node->left, result);
    result.push_back(node->value);
    inorder(node->right, result);
}
```

**Haskell:**
```haskell
data Tree a = Empty | Node a (Tree a) (Tree a)

inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node val left right) =
    inorder left ++ [val] ++ inorder right
```

## When to Use Recursion

**Good uses:**
- Tree and graph traversal
- Divide-and-conquer algorithms (merge sort, quicksort)
- Problems with recursive structure (parsing, evaluation)
- Mathematical sequences (factorial, Fibonacci with memoization)
- Backtracking problems (Sudoku, N-Queens)

**When iteration might be better:**
- Simple loops over a range
- Tail-recursive functions in languages without TCO (Python)
- Very deep recursion (stack overflow risk)
- Performance-critical code where stack overhead matters

## Comparison Table

| Feature | Python | C++ | Haskell |
|---------|--------|-----|---------|
| **Recursion Support** | Yes | Yes | Primary method |
| **Recursion Limit** | ~1000 (configurable) | Stack size (1-8MB) | No limit (TCO) |
| **Tail Call Optimization** | No | Maybe (with -O2) | Always |
| **Preferred Style** | Iteration for simple, recursion for trees | Iteration preferred | Recursion always |
| **Stack Overflow Risk** | High for deep recursion | Medium | Low (TCO) |
| **Pattern Matching** | No (but has match in 3.10+) | No | Yes (excellent) |
| **Base Case Syntax** | if/else | if/else | Pattern matching |

## Mutual Recursion

Functions can call each other recursively:

**Python:**
```python
def is_even(n):
    if n == 0:
        return True
    return is_odd(n - 1)

def is_odd(n):
    if n == 0:
        return False
    return is_even(n - 1)
```

**C++:**
```cpp
bool isOdd(int n);  // Forward declaration

bool isEven(int n) {
    if (n == 0) return true;
    return isOdd(n - 1);
}

bool isOdd(int n) {
    if (n == 0) return false;
    return isEven(n - 1);
}
```

**Haskell:**
```haskell
isEven :: Int -> Bool
isEven 0 = True
isEven n = isOdd (n - 1)

isOdd :: Int -> Bool
isOdd 0 = False
isOdd n = isEven (n - 1)
```

## Key Takeaways

1. **Every recursive function needs a base case** to prevent infinite recursion
2. **Each recursive call must make progress** toward the base case
3. **Recursion is natural for tree/graph problems** and hierarchical data
4. **Haskell uses recursion as the primary repetition mechanism**
5. **Python and C++ prefer iteration for simple cases**, recursion for complex structures
6. **Tail recursion can be optimized** in some languages (Haskell always, C++ sometimes, Python never)
7. **Stack overflow is a concern** in Python and C++
8. **Memoization solves** inefficient tree recursion problems

## Looking Ahead

Understanding recursion prepares you for:
- **Higher-order functions** (map, filter, reduce built on recursion)
- **Functional programming patterns** (recursion as primary control flow)
- **Algorithm design** (divide-and-conquer, backtracking)
- **Data structures** (trees, graphs naturally recursive)

## Exercises

See EXERCISES.md for hands-on practice with recursion!

## Summary

Recursion is a powerful technique for solving problems by breaking them into smaller subproblems:

- **Python:** Recursion available but limited, iteration often preferred
- **C++:** Recursion supported, use with caution for stack depth
- **Haskell:** Recursion is primary, tail call optimization guaranteed

Understanding when and how to use recursion in each language is essential for effective programming across paradigms.
