# Lesson 6: Recursion - Solution Guide

This guide provides example solutions for the Recursion exercises.

## General Notes

- **Every recursive function needs a base case**: The condition that stops recursion
- **Must make progress toward base case**: Each recursive call should get closer to the base case
- **Think recursively**: Assume the function works for smaller inputs, then solve for current input
- **Try before looking**: Recursion requires practice to master

---

## Exercise 1: Power Function

**Task:** Implement recursive power function x^n

### Python Solution

```python
def power(x, n):
    # Base case: x^0 = 1
    if n == 0:
        return 1
    # Recursive case: x^n = x * x^(n-1)
    return x * power(x, n - 1)

# Test
print(power(2, 10))  # 1024
print(power(5, 3))   # 125
print(power(10, 0))  # 1
```

### Optimized Version (Fast Exponentiation)

```python
def power_fast(x, n):
    """Fast exponentiation using divide-and-conquer"""
    if n == 0:
        return 1
    if n % 2 == 0:
        # Even: x^n = (x^(n/2))^2
        half = power_fast(x, n // 2)
        return half * half
    else:
        # Odd: x^n = x * x^(n-1)
        return x * power_fast(x, n - 1)

# Test
print(power_fast(2, 10))  # 1024
# Much faster for large exponents!
```

### Haskell Solution

```haskell
power :: Int -> Int -> Int
power x 0 = 1
power x n = x * power x (n - 1)

-- Fast version
powerFast :: Int -> Int -> Int
powerFast x 0 = 1
powerFast x n
    | even n    = half * half
    | otherwise = x * powerFast x (n - 1)
  where
    half = powerFast x (n `div` 2)

main :: IO ()
main = do
    print $ power 2 10      -- 1024
    print $ powerFast 2 10  -- 1024
```

### Racket Solution

```racket
#lang racket

(define (power x n)
  (if (= n 0)
      1
      (* x (power x (- n 1)))))

; Test
(displayln (power 2 10))  ; 1024
(displayln (power 5 3))   ; 125
```

**Key Insights:**
- Base case: x^0 = 1
- Recursive case: x^n = x * x^(n-1)
- Fast exponentiation reduces O(n) to O(log n)
- Divide-and-conquer splits problem in half

---

## Exercise 2: Count Down

**Task:** Print numbers from n down to 1

### Python Solution

```python
def countdown(n):
    # Base case
    if n == 0:
        print("Blast off!")
        return

    # Print current number
    print(n)

    # Recursive case
    countdown(n - 1)

# Test
countdown(5)
# Output:
# 5
# 4
# 3
# 2
# 1
# Blast off!
```

### JavaScript Solution

```javascript
function countdown(n) {
    // Base case
    if (n === 0) {
        console.log("Blast off!");
        return;
    }

    // Print current number
    console.log(n);

    // Recursive case
    countdown(n - 1);
}

// Test
countdown(5);
```

### Alternative: Count Up

```python
def countup(n):
    """Count from 1 to n using recursion"""
    if n == 0:
        return
    countup(n - 1)  # Recurse first
    print(n)        # Then print

countup(5)
# Output: 1, 2, 3, 4, 5
```

**Key Insights:**
- Base case: n == 0
- Order matters: print before or after recursive call
- Print before recursion: countdown
- Print after recursion: countup

---

## Exercise 3: List Operations

**Task:** Implement recursive list operations

### Python Solutions

```python
def contains(lst, element):
    """Check if element is in list"""
    # Base case: empty list
    if not lst:
        return False

    # Check first element
    if lst[0] == element:
        return True

    # Recursive case: check rest of list
    return contains(lst[1:], element)

def count(lst, element):
    """Count occurrences of element"""
    # Base case
    if not lst:
        return 0

    # Count in rest of list
    count_in_rest = count(lst[1:], element)

    # Add 1 if first element matches
    if lst[0] == element:
        return 1 + count_in_rest
    else:
        return count_in_rest

def nth(lst, n):
    """Get nth element (0-indexed)"""
    # Edge case: n too large
    if not lst:
        raise IndexError("Index out of range")

    # Base case: n is 0
    if n == 0:
        return lst[0]

    # Recursive case: look in rest
    return nth(lst[1:], n - 1)

# Test
test_list = [1, 2, 3, 4, 2, 5]
print(contains(test_list, 3))      # True
print(contains(test_list, 10))     # False
print(count(test_list, 2))         # 2
print(count(test_list, 7))         # 0
print(nth(test_list, 0))           # 1
print(nth(test_list, 4))           # 2
```

### Haskell Solutions

```haskell
-- Contains
contains :: Eq a => [a] -> a -> Bool
contains [] _ = False
contains (x:xs) element
    | x == element = True
    | otherwise    = contains xs element

-- Count
count :: Eq a => [a] -> a -> Int
count [] _ = 0
count (x:xs) element
    | x == element = 1 + count xs element
    | otherwise    = count xs element

-- Nth element
nth :: [a] -> Int -> a
nth [] _ = error "Index out of range"
nth (x:xs) 0 = x
nth (x:xs) n = nth xs (n - 1)

-- Test
main :: IO ()
main = do
    let testList = [1, 2, 3, 4, 2, 5]
    print $ contains testList 3      -- True
    print $ contains testList 10     -- False
    print $ count testList 2         -- 2
    print $ nth testList 4           -- 2
```

**Key Insights:**
- Pattern: check empty list (base case), then first element, then recurse on rest
- Haskell's pattern matching makes this natural
- Python uses slicing `lst[1:]` to get rest
- Always handle empty list case

---

## Exercise 4: Recursive String Operations

**Task:** String operations using recursion

### Python Solutions

```python
def count_char(string, char):
    """Count occurrences of character"""
    # Base case: empty string
    if not string:
        return 0

    # Check first character
    count_in_rest = count_char(string[1:], char)

    if string[0] == char:
        return 1 + count_in_rest
    else:
        return count_in_rest

def remove_char(string, char):
    """Remove all occurrences of character"""
    # Base case
    if not string:
        return ""

    # Recursive case
    rest = remove_char(string[1:], char)

    if string[0] == char:
        return rest  # Skip this character
    else:
        return string[0] + rest  # Keep this character

def replace_char(string, old, new):
    """Replace all old with new"""
    # Base case
    if not string:
        return ""

    # Recursive case
    rest = replace_char(string[1:], old, new)

    if string[0] == old:
        return new + rest
    else:
        return string[0] + rest

# Test
print(count_char("hello", 'l'))           # 2
print(remove_char("hello", 'l'))          # "heo"
print(replace_char("hello", 'l', 'r'))    # "herro"
print(count_char("mississippi", 's'))     # 4
print(remove_char("mississippi", 's'))    # "miiippi"
```

### Haskell Solutions

```haskell
-- Count character
countChar :: String -> Char -> Int
countChar "" _ = 0
countChar (c:cs) char
    | c == char = 1 + countChar cs char
    | otherwise = countChar cs char

-- Remove character
removeChar :: String -> Char -> String
removeChar "" _ = ""
removeChar (c:cs) char
    | c == char = removeChar cs char
    | otherwise = c : removeChar cs char

-- Replace character
replaceChar :: String -> Char -> Char -> String
replaceChar "" _ _ = ""
replaceChar (c:cs) old new
    | c == old  = new : replaceChar cs old new
    | otherwise = c : replaceChar cs old new

main :: IO ()
main = do
    print $ countChar "hello" 'l'           -- 2
    print $ removeChar "hello" 'l'          -- "heo"
    print $ replaceChar "hello" 'l' 'r'     -- "herro"
```

**Key Insights:**
- Strings are just lists of characters
- Same recursive pattern as list operations
- Build result by combining first element with recursive result
- Haskell's pattern matching on strings is elegant

---

## Exercise 5: Sum of Digits

**Task:** Sum digits of a number recursively

### Python Solution

```python
def sum_digits(n):
    """Sum the digits of a number"""
    # Base case: single digit
    if n < 10:
        return n

    # Recursive case
    last_digit = n % 10
    remaining = n // 10
    return last_digit + sum_digits(remaining)

# Test
print(sum_digits(1234))  # 10
print(sum_digits(987))   # 24
print(sum_digits(5))     # 5
print(sum_digits(12345678))  # 36
```

### Haskell Solution

```haskell
sumDigits :: Int -> Int
sumDigits n
    | n < 10    = n
    | otherwise = (n `mod` 10) + sumDigits (n `div` 10)

main :: IO ()
main = do
    print $ sumDigits 1234       -- 10
    print $ sumDigits 987        -- 24
    print $ sumDigits 12345678   -- 36
```

### Java Solution

```java
public class SumDigits {
    public static int sumDigits(int n) {
        // Base case
        if (n < 10) {
            return n;
        }

        // Recursive case
        int lastDigit = n % 10;
        int remaining = n / 10;
        return lastDigit + sumDigits(remaining);
    }

    public static void main(String[] args) {
        System.out.println(sumDigits(1234));  // 10
        System.out.println(sumDigits(987));   // 24
    }
}
```

**Key Insights:**
- Use `n % 10` to get last digit
- Use `n // 10` to remove last digit
- Process digits right-to-left
- Works for any positive integer

---

## Exercise 6: Merge Sort

**Task:** Implement merge sort recursively

### Python Solution

```python
def merge_sort(lst):
    """Sort list using merge sort"""
    # Base case: 0 or 1 elements
    if len(lst) <= 1:
        return lst

    # Split in half
    mid = len(lst) // 2
    left = lst[:mid]
    right = lst[mid:]

    # Recursively sort each half
    left_sorted = merge_sort(left)
    right_sorted = merge_sort(right)

    # Merge sorted halves
    return merge(left_sorted, right_sorted)

def merge(left, right):
    """Merge two sorted lists"""
    result = []
    i = j = 0

    # Merge while both have elements
    while i < len(left) and j < len(right):
        if left[i] <= right[j]:
            result.append(left[i])
            i += 1
        else:
            result.append(right[j])
            j += 1

    # Add remaining elements
    result.extend(left[i:])
    result.extend(right[j:])

    return result

# Test
print(merge_sort([38, 27, 43, 3, 9, 82, 10]))
# [3, 9, 10, 27, 38, 43, 82]

print(merge_sort([5, 2, 8, 1, 9]))
# [1, 2, 5, 8, 9]
```

### Haskell Solution

```haskell
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
  where
    (left, right) = splitAt (length xs `div` 2) xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

main :: IO ()
main = do
    print $ mergeSort [38, 27, 43, 3, 9, 82, 10]
    -- [3,9,10,27,38,43,82]
```

### Time Complexity Analysis

- **Time:** O(n log n) in all cases
  - Split: O(log n) levels of recursion
  - Merge: O(n) work per level
  - Total: O(n log n)

- **Space:** O(n) for temporary arrays

- **Stable:** Yes (preserves relative order of equal elements)

**Key Insights:**
- Divide-and-conquer algorithm
- Always O(n log n) - very predictable
- Stable sort (unlike quicksort)
- Requires extra space for merging
- Very elegant in functional languages

---

## Exercise 7: Flatten Nested List

**Task:** Flatten arbitrarily nested list

### Python Solution

```python
def flatten(lst):
    """Flatten nested list"""
    result = []

    for item in lst:
        # Check if item is a list
        if isinstance(item, list):
            # Recursively flatten and extend result
            result.extend(flatten(item))
        else:
            # Add single item
            result.append(item)

    return result

# Alternative: more concise
def flatten_concise(lst):
    if not lst:
        return []
    if isinstance(lst[0], list):
        return flatten_concise(lst[0]) + flatten_concise(lst[1:])
    return [lst[0]] + flatten_concise(lst[1:])

# Test
print(flatten([1, [2, 3], [4, [5, 6]]]))
# [1, 2, 3, 4, 5, 6]

print(flatten([[1, 2], [3, [4, [5]]], 6]))
# [1, 2, 3, 4, 5, 6]

print(flatten([]))
# []
```

### Haskell Solution

```haskell
-- Define a type that can be nested
data Nested a = Item a | List [Nested a]

flatten :: Nested a -> [a]
flatten (Item x) = [x]
flatten (List xs) = concatMap flatten xs

-- Example
example :: Nested Int
example = List [Item 1, List [Item 2, Item 3], List [Item 4, List [Item 5, Item 6]]]

main :: IO ()
main = print $ flatten example
-- [1,2,3,4,5,6]
```

**Key Insights:**
- Use `isinstance()` to check if something is a list
- Recursively flatten nested lists
- Pattern: check type, handle base case, recurse
- Python's dynamic typing makes this easier than static typing

---

## Exercise 8: Generate Permutations

**Task:** Generate all permutations of a list

### Python Solution

```python
def permutations(lst):
    """Generate all permutations"""
    # Base case: empty list
    if len(lst) == 0:
        return [[]]

    # Base case: single element
    if len(lst) == 1:
        return [lst]

    result = []

    # For each element
    for i in range(len(lst)):
        # Remove element
        current = lst[i]
        remaining = lst[:i] + lst[i+1:]

        # Get permutations of remaining
        for perm in permutations(remaining):
            # Add current element to front
            result.append([current] + perm)

    return result

# More Pythonic using list comprehension
def permutations_concise(lst):
    if not lst:
        return [[]]

    return [
        [lst[i]] + perm
        for i in range(len(lst))
        for perm in permutations_concise(lst[:i] + lst[i+1:])
    ]

# Test
print(permutations([1, 2, 3]))
# [[1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1]]

print(len(permutations([1, 2, 3, 4])))  # 24 = 4!
```

### Haskell Solution

```haskell
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations xs = [ x:ys | x <- xs, ys <- permutations (delete x xs) ]
  where
    delete y [] = []
    delete y (z:zs)
        | y == z    = zs
        | otherwise = z : delete y zs

-- Or using built-in
import Data.List (permutations)

main :: IO ()
main = do
    print $ permutations [1, 2, 3]
    -- [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
```

### JavaScript Solution

```javascript
function permutations(arr) {
    if (arr.length === 0) return [[]];
    if (arr.length === 1) return [arr];

    const result = [];

    for (let i = 0; i < arr.length; i++) {
        const current = arr[i];
        const remaining = [...arr.slice(0, i), ...arr.slice(i + 1)];

        const perms = permutations(remaining);
        for (const perm of perms) {
            result.push([current, ...perm]);
        }
    }

    return result;
}

console.log(permutations([1, 2, 3]));
// [[1,2,3], [1,3,2], [2,1,3], [2,3,1], [3,1,2], [3,2,1]]
```

**Key Insights:**
- n! permutations of n elements
- For each element, put it first and permute the rest
- Base case: permutations of [] is [[]]
- Classic recursive problem
- Haskell's list comprehensions make it very concise

---

## Exercise 9: Tree Problems

**Task:** Recursive tree operations

### Python Solution

```python
# Tree representation
class TreeNode:
    def __init__(self, value, left=None, right=None):
        self.value = value
        self.left = left
        self.right = right

# 1. Count total nodes
def count_nodes(tree):
    if tree is None:
        return 0
    return 1 + count_nodes(tree.left) + count_nodes(tree.right)

# 2. Count leaf nodes
def count_leaves(tree):
    if tree is None:
        return 0
    # Leaf: no children
    if tree.left is None and tree.right is None:
        return 1
    return count_leaves(tree.left) + count_leaves(tree.right)

# 3. Check if value exists
def tree_contains(tree, value):
    if tree is None:
        return False
    if tree.value == value:
        return True
    return tree_contains(tree.left, value) or tree_contains(tree.right, value)

# 4. Get all root-to-leaf paths
def tree_paths(tree, path=[]):
    if tree is None:
        return []

    # Add current node to path
    current_path = path + [tree.value]

    # If leaf, return path
    if tree.left is None and tree.right is None:
        return [current_path]

    # Recursively get paths from children
    left_paths = tree_paths(tree.left, current_path)
    right_paths = tree_paths(tree.right, current_path)

    return left_paths + right_paths

# Build example tree:
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

# Test
print(count_nodes(tree))        # 5
print(count_leaves(tree))       # 3 (nodes 4, 5, 3)
print(tree_contains(tree, 4))   # True
print(tree_contains(tree, 10))  # False
print(tree_paths(tree))         # [[1, 2, 4], [1, 2, 5], [1, 3]]
```

### Haskell Solution

```haskell
data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Show)

-- 1. Count nodes
countNodes :: Tree a -> Int
countNodes Empty = 0
countNodes (Node _ left right) = 1 + countNodes left + countNodes right

-- 2. Count leaves
countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Node _ Empty Empty) = 1
countLeaves (Node _ left right) = countLeaves left + countLeaves right

-- 3. Contains
treeContains :: Eq a => Tree a -> a -> Bool
treeContains Empty _ = False
treeContains (Node val left right) target
    | val == target = True
    | otherwise     = treeContains left target || treeContains right target

-- 4. All paths
treePaths :: Tree a -> [[a]]
treePaths Empty = []
treePaths (Node val Empty Empty) = [[val]]
treePaths (Node val left right) =
    map (val:) (treePaths left ++ treePaths right)

-- Example tree
exampleTree :: Tree Int
exampleTree = Node 1
    (Node 2
        (Node 4 Empty Empty)
        (Node 5 Empty Empty))
    (Node 3 Empty Empty)

main :: IO ()
main = do
    print $ countNodes exampleTree      -- 5
    print $ countLeaves exampleTree     -- 3
    print $ treeContains exampleTree 4  -- True
    print $ treePaths exampleTree       -- [[1,2,4],[1,2,5],[1,3]]
```

**Key Insights:**
- Trees are naturally recursive structures
- Base case: empty tree
- Recursive case: combine results from left and right subtrees
- Path tracking: pass accumulated path down recursively
- Pattern: process current node, then recurse on children

---

## Exercise 10: Greatest Common Divisor (GCD)

**Task:** Implement Euclid's algorithm recursively

### Python Solution

```python
def gcd(a, b):
    """Euclidean algorithm for GCD"""
    # Base case
    if b == 0:
        return a

    # Recursive case
    return gcd(b, a % b)

# Test
print(gcd(48, 18))   # 6
print(gcd(100, 35))  # 5
print(gcd(17, 5))    # 1
print(gcd(1071, 462))  # 21

# Trace execution for gcd(100, 35):
# gcd(100, 35)
# gcd(35, 30)   # 100 % 35 = 30
# gcd(30, 5)    # 35 % 30 = 5
# gcd(5, 0)     # 30 % 5 = 0
# return 5
```

### Haskell Solution

```haskell
gcd' :: Int -> Int -> Int
gcd' a 0 = a
gcd' a b = gcd' b (a `mod` b)

main :: IO ()
main = do
    print $ gcd' 48 18      -- 6
    print $ gcd' 100 35     -- 5
    print $ gcd' 1071 462   -- 21
```

### Why It Works

The Euclidean algorithm is based on the principle:
- gcd(a, b) = gcd(b, a mod b)
- This is because any common divisor of a and b must also divide (a mod b)
- Eventually b becomes 0, and gcd(a, 0) = a

**Key Insights:**
- One of the oldest algorithms (300 BC)
- Extremely efficient: O(log(min(a, b)))
- Beautiful example of recursive thinking
- Base case: when divisor is 0
- Each step makes problem smaller

---

## Exercise 11: Recursion vs Iteration

**Task:** Compare recursive and iterative solutions

### 1. Sum of Numbers 1 to n

```python
# Recursive
def sum_recursive(n):
    if n == 0:
        return 0
    return n + sum_recursive(n - 1)

# Iterative
def sum_iterative(n):
    total = 0
    for i in range(1, n + 1):
        total += i
    return total

# Formula (best!)
def sum_formula(n):
    return n * (n + 1) // 2

# Test
print(sum_recursive(100))  # 5050
print(sum_iterative(100))  # 5050
print(sum_formula(100))    # 5050

# Timing comparison
import time

n = 1000

start = time.time()
sum_recursive(n)
print(f"Recursive: {time.time() - start:.6f}s")

start = time.time()
sum_iterative(n)
print(f"Iterative: {time.time() - start:.6f}s")

start = time.time()
sum_formula(n)
print(f"Formula: {time.time() - start:.6f}s")
```

### 2. Reverse a String

```python
# Recursive
def reverse_recursive(s):
    if len(s) <= 1:
        return s
    return reverse_recursive(s[1:]) + s[0]

# Iterative
def reverse_iterative(s):
    result = ""
    for char in s:
        result = char + result
    return result

# Pythonic
def reverse_pythonic(s):
    return s[::-1]

# Test
print(reverse_recursive("hello"))   # "olleh"
print(reverse_iterative("hello"))   # "olleh"
print(reverse_pythonic("hello"))    # "olleh"
```

### 3. Check if Number is Prime

```python
# Recursive (helper function)
def is_prime_recursive(n, divisor=2):
    if n < 2:
        return False
    if divisor * divisor > n:
        return True
    if n % divisor == 0:
        return False
    return is_prime_recursive(n, divisor + 1)

# Iterative
def is_prime_iterative(n):
    if n < 2:
        return False
    for i in range(2, int(n ** 0.5) + 1):
        if n % i == 0:
            return False
    return True

# Test
print(is_prime_recursive(17))   # True
print(is_prime_recursive(18))   # False
print(is_prime_iterative(97))   # True
```

### When to Use Each

**Use Recursion When:**
- Problem has natural recursive structure (trees, divide-and-conquer)
- Solution is more elegant and readable
- Working in functional language with tail-call optimization
- Problem involves backtracking

**Use Iteration When:**
- Simple linear traversal
- Performance is critical
- Language lacks tail-call optimization
- Risk of stack overflow

**Key Insights:**
- Iteration is usually faster (no function call overhead)
- Recursion is often clearer for complex problems
- Some problems are naturally recursive (trees, graphs)
- Tail recursion can be optimized by compiler

---

## Exercise 12: Tail Recursion Conversion

**Task:** Convert to tail-recursive form

### 1. Sum of List

```python
# Regular recursion (NOT tail recursive)
def sum_list(lst):
    if not lst:
        return 0
    return lst[0] + sum_list(lst[1:])

# Tail recursive (with accumulator)
def sum_list_tail(lst, acc=0):
    if not lst:
        return acc
    return sum_list_tail(lst[1:], acc + lst[0])

# Test
print(sum_list([1, 2, 3, 4, 5]))       # 15
print(sum_list_tail([1, 2, 3, 4, 5]))  # 15
```

### 2. Reverse of List

```python
# Regular recursion
def reverse(lst):
    if not lst:
        return []
    return reverse(lst[1:]) + [lst[0]]

# Tail recursive
def reverse_tail(lst, acc=[]):
    if not lst:
        return acc
    return reverse_tail(lst[1:], [lst[0]] + acc)

# Test
print(reverse([1, 2, 3, 4]))        # [4, 3, 2, 1]
print(reverse_tail([1, 2, 3, 4]))   # [4, 3, 2, 1]
```

### 3. Factorial

```python
# Regular recursion
def factorial(n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)

# Tail recursive
def factorial_tail(n, acc=1):
    if n <= 1:
        return acc
    return factorial_tail(n - 1, acc * n)

# Test
print(factorial(5))        # 120
print(factorial_tail(5))   # 120
```

### Haskell (Tail Call Optimization)

```haskell
-- Regular
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Tail recursive
factorialTail :: Int -> Int -> Int
factorialTail 0 acc = acc
factorialTail n acc = factorialTail (n - 1) (acc * n)

-- Wrapper
factorial' :: Int -> Int
factorial' n = factorialTail n 1
```

**Key Insights:**
- Tail recursion: recursive call is the last operation
- Accumulator pattern: pass intermediate result as parameter
- Languages with TCO (Haskell, Scheme) optimize tail calls to loops
- Python and JavaScript don't optimize tail calls
- Tail recursion trades stack space for parameter passing

---

## Summary

Recursion is a powerful programming technique:

**Key Principles:**
1. **Always have a base case** - prevents infinite recursion
2. **Make progress toward base case** - each call should be simpler
3. **Assume recursion works** - trust that recursive call handles smaller problem

**Common Patterns:**
- **Linear recursion**: Process list/string element by element
- **Tree recursion**: Recurse on multiple subproblems
- **Divide and conquer**: Split problem in half (merge sort, binary search)
- **Backtracking**: Try possibilities, undo if fails
- **Tail recursion**: Accumulator pattern for efficiency

**When to Use:**
- Trees and graphs (naturally recursive)
- Divide and conquer algorithms
- Backtracking problems
- Functional programming

**When to Avoid:**
- Simple iteration is clearer
- Risk of stack overflow
- Performance critical (unless tail-call optimized)

**Language Differences:**
- **Haskell**: Recursion is the primary iteration mechanism
- **Python/JavaScript**: Iteration often preferred
- **Racket/Scheme**: Tail-call optimization makes recursion efficient

Master recursion to unlock powerful problem-solving techniques!
