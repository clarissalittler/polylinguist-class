# Lesson 12: Algorithm Analysis & Complexity

## Learning Objectives

By the end of this lesson, you will be able to:

1. Understand Big-O notation and why it matters
2. Analyze the time complexity of algorithms
3. Analyze the space complexity of algorithms
4. Distinguish between best, average, and worst-case performance
5. Recognize common complexity classes (O(1), O(log n), O(n), O(n log n), O(n²), O(2ⁿ))
6. Choose algorithms based on performance requirements
7. Understand tradeoffs between time and space complexity

## Introduction

Not all algorithms are created equal. Two programs that produce the same result can have **vastly different performance characteristics**. Understanding algorithm complexity helps you:

- **Write efficient code** that scales to large inputs
- **Choose the right algorithm** for your problem size
- **Predict performance** before implementing
- **Identify bottlenecks** in existing code
- **Make informed tradeoffs** between time, space, and code clarity

## Why Performance Matters

Consider searching for a name in a phone book:

**Linear Search (check every entry):**
- 1,000 entries → up to 1,000 checks
- 1,000,000 entries → up to 1,000,000 checks
- 1,000,000,000 entries → up to 1,000,000,000 checks

**Binary Search (divide and conquer):**
- 1,000 entries → up to 10 checks
- 1,000,000 entries → up to 20 checks
- 1,000,000,000 entries → up to 30 checks

Same result, **completely different performance** at scale!

## What is Big-O Notation?

**Big-O notation** describes how an algorithm's performance scales as input size grows.

### Definition

**O(f(n))** means: As input size n grows, the algorithm's operations grow proportionally to f(n).

- **O(1)**: Constant time - same speed regardless of input size
- **O(log n)**: Logarithmic - doubles input, adds constant work
- **O(n)**: Linear - doubles input, doubles work
- **O(n log n)**: Log-linear - efficient sorting algorithms
- **O(n²)**: Quadratic - nested loops over input
- **O(2ⁿ)**: Exponential - doubles with each input increase

### Key Insight

Big-O describes **worst-case** growth rate, ignoring:
- Constant factors (2n is still O(n))
- Lower-order terms (n² + n is O(n²))
- Implementation details

We care about **how it scales**, not exact counts.

## Common Complexity Classes

### O(1) - Constant Time

**Performance:** Same speed regardless of input size.

**Examples:**
- Array access by index
- Hash table lookup (average case)
- Stack push/pop
- Arithmetic operations

```python
# O(1) - accessing an element
def get_first(arr):
    return arr[0]  # Always one operation

# O(1) - hash table lookup
def lookup(dictionary, key):
    return dictionary[key]  # Average case: one operation
```

**Real-world:** Accessing your bank balance in a database - doesn't matter if the bank has 100 or 100 million customers.

---

### O(log n) - Logarithmic Time

**Performance:** Doubles input size, adds constant work.

**Examples:**
- Binary search in sorted array
- Balanced tree operations
- Finding an element in a sorted structure

```python
# O(log n) - binary search
def binary_search(arr, target):
    left, right = 0, len(arr) - 1

    while left <= right:
        mid = (left + right) // 2

        if arr[mid] == target:
            return mid
        elif arr[mid] < target:
            left = mid + 1
        else:
            right = mid - 1

    return -1  # Not found

# Each iteration halves the search space
# 1000 items → ~10 steps
# 1,000,000 items → ~20 steps
```

**Real-world:** Looking up a word in a dictionary - you don't read every page, you use alphabetical ordering to jump around.

**Why it's fast:** Each step eliminates half the remaining possibilities.

---

### O(n) - Linear Time

**Performance:** Doubles input, doubles work.

**Examples:**
- Linear search
- Iterating through array once
- Finding min/max in unsorted array
- Printing all elements

```python
# O(n) - linear search
def linear_search(arr, target):
    for i, item in enumerate(arr):
        if item == target:
            return i
    return -1

# O(n) - summing array
def sum_array(arr):
    total = 0
    for num in arr:
        total += num
    return total

# O(n) - finding maximum
def find_max(arr):
    if not arr:
        return None

    max_val = arr[0]
    for num in arr:
        if num > max_val:
            max_val = num
    return max_val
```

**Real-world:** Reading every book in a library to find one mention of a topic.

---

### O(n log n) - Log-Linear Time

**Performance:** Better than O(n²), worse than O(n).

**Examples:**
- Efficient sorting algorithms (merge sort, quick sort, heap sort)
- Building a balanced tree from unsorted data

```python
# O(n log n) - merge sort
def merge_sort(arr):
    if len(arr) <= 1:
        return arr

    # Divide
    mid = len(arr) // 2
    left = merge_sort(arr[:mid])    # O(log n) divisions
    right = merge_sort(arr[mid:])

    # Conquer (merge)
    return merge(left, right)       # O(n) merge at each level

def merge(left, right):
    result = []
    i, j = 0, 0

    while i < len(left) and j < len(right):
        if left[i] <= right[j]:
            result.append(left[i])
            i += 1
        else:
            result.append(right[j])
            j += 1

    result.extend(left[i:])
    result.extend(right[j:])
    return result
```

**Real-world:** Organizing a shuffled deck of cards using an efficient strategy.

**Why O(n log n)?** We divide the problem log n times, and do O(n) work at each level.

---

### O(n²) - Quadratic Time

**Performance:** Doubles input, quadruples work.

**Examples:**
- Nested loops over same data
- Simple sorting algorithms (bubble sort, selection sort, insertion sort)
- Comparing all pairs

```python
# O(n²) - nested loop
def find_duplicates(arr):
    duplicates = []
    for i in range(len(arr)):
        for j in range(i + 1, len(arr)):
            if arr[i] == arr[j]:
                duplicates.append(arr[i])
    return duplicates

# O(n²) - bubble sort
def bubble_sort(arr):
    n = len(arr)
    for i in range(n):              # O(n)
        for j in range(0, n - i - 1):  # O(n)
            if arr[j] > arr[j + 1]:
                arr[j], arr[j + 1] = arr[j + 1], arr[j]
    return arr
```

**Real-world:** Comparing every person in a room with every other person (handshakes).

**Problem:** Quickly becomes impractical for large inputs:
- 100 items → 10,000 operations
- 1,000 items → 1,000,000 operations
- 10,000 items → 100,000,000 operations

---

### O(2ⁿ) - Exponential Time

**Performance:** Each additional input doubles the work.

**Examples:**
- Naive recursive Fibonacci
- Generating all subsets
- Brute-force solution exploration

```python
# O(2ⁿ) - naive Fibonacci (VERY SLOW!)
def fib_slow(n):
    if n <= 1:
        return n
    return fib_slow(n - 1) + fib_slow(n - 2)

# Why O(2ⁿ)? Tree of recursive calls doubles at each level
# fib(5) calls fib(4) and fib(3)
# fib(4) calls fib(3) and fib(2)
# fib(3) calls fib(2) and fib(1)
# ... exponential explosion!

# O(2ⁿ) - generating all subsets
def all_subsets(arr):
    if not arr:
        return [[]]

    first = arr[0]
    rest_subsets = all_subsets(arr[1:])

    # For each subset, create two: with and without first element
    with_first = [[first] + subset for subset in rest_subsets]
    return rest_subsets + with_first
```

**Real-world:** Trying every possible combination of a password.

**Problem:** Utterly impractical beyond small inputs:
- n = 10 → 1,024 operations
- n = 20 → 1,048,576 operations
- n = 30 → 1,073,741,824 operations
- n = 40 → 1,099,511,627,776 operations

---

### O(n!) - Factorial Time

**Performance:** Each additional input multiplies work by n.

**Examples:**
- Generating all permutations
- Traveling salesman (brute force)

```python
# O(n!) - all permutations
def permutations(arr):
    if len(arr) <= 1:
        return [arr]

    result = []
    for i in range(len(arr)):
        first = arr[i]
        rest = arr[:i] + arr[i+1:]
        for perm in permutations(rest):
            result.append([first] + perm)

    return result

# 5! = 120
# 10! = 3,628,800
# 20! = 2,432,902,008,176,640,000
```

**Real-world:** Trying every possible route for a traveling salesman visiting n cities.

**Problem:** Impossible beyond tiny inputs.

---

## Complexity Comparison Chart

| Complexity | n=10 | n=100 | n=1,000 | n=10,000 | Example |
|------------|------|-------|---------|----------|---------|
| O(1)       | 1    | 1     | 1       | 1        | Array access |
| O(log n)   | 3    | 7     | 10      | 13       | Binary search |
| O(n)       | 10   | 100   | 1,000   | 10,000   | Linear search |
| O(n log n) | 30   | 700   | 10,000  | 130,000  | Merge sort |
| O(n²)      | 100  | 10,000| 1,000,000| 100,000,000 | Bubble sort |
| O(2ⁿ)      | 1,024| 1.27×10³⁰| ∞    | ∞        | Naive Fibonacci |

**Insight:** At n=1,000, the difference between O(n log n) and O(n²) is 10,000 vs 1,000,000 operations - **100x slower!**

---

## Analyzing Algorithm Complexity

### Step-by-Step Process

1. **Identify basic operations** (comparisons, assignments, arithmetic)
2. **Count how many times operations run** as a function of n
3. **Focus on fastest-growing term**
4. **Drop constants and lower-order terms**

### Example 1: Simple Loop

```python
def print_all(arr):
    for item in arr:  # Runs n times
        print(item)   # O(1) operation
```

**Analysis:**
- Loop runs n times
- Each iteration is O(1)
- Total: **O(n)**

### Example 2: Nested Loops

```python
def print_pairs(arr):
    for i in arr:           # Runs n times
        for j in arr:       # Runs n times per i
            print(i, j)     # O(1) operation
```

**Analysis:**
- Outer loop: n times
- Inner loop: n times for each outer iteration
- Total: n × n = **O(n²)**

### Example 3: Two Sequential Loops

```python
def two_loops(arr):
    for item in arr:      # O(n)
        print(item)

    for item in arr:      # O(n)
        print(item * 2)
```

**Analysis:**
- First loop: O(n)
- Second loop: O(n)
- Total: O(n) + O(n) = O(2n) = **O(n)**
- (Drop the constant 2)

### Example 4: Loop with Halving

```python
def halving_loop(n):
    i = n
    while i > 1:
        print(i)
        i = i // 2  # Halve each time
```

**Analysis:**
- Start: n
- After 1 iteration: n/2
- After 2 iterations: n/4
- After 3 iterations: n/8
- ...
- After k iterations: n / 2^k

When does it stop? When n / 2^k ≤ 1
- So 2^k ≥ n
- k ≥ log₂(n)
- Total: **O(log n)**

### Example 5: Recursive Function

```python
def recursive_sum(arr):
    if len(arr) == 0:
        return 0
    return arr[0] + recursive_sum(arr[1:])
```

**Analysis:**
- Makes n recursive calls (one for each element)
- Each call does O(1) work
- Total: **O(n)**

**But beware!** Slicing `arr[1:]` in Python creates a copy, which is O(n).
So this is actually **O(n²)** in Python!

Better version:
```python
def recursive_sum(arr, index=0):
    if index >= len(arr):
        return 0
    return arr[index] + recursive_sum(arr, index + 1)
# Now truly O(n)
```

---

## Space Complexity

**Space complexity** measures how much memory an algorithm uses.

### O(1) Space - Constant

```python
def sum_array(arr):
    total = 0  # Only one variable
    for num in arr:
        total += num
    return total
# Space: O(1) - regardless of array size
```

### O(n) Space - Linear

```python
def copy_array(arr):
    copy = []
    for item in arr:
        copy.append(item)
    return copy
# Space: O(n) - new array of same size
```

### O(log n) Space - Logarithmic

```python
def binary_search_recursive(arr, target, left=0, right=None):
    if right is None:
        right = len(arr) - 1

    if left > right:
        return -1

    mid = (left + right) // 2

    if arr[mid] == target:
        return mid
    elif arr[mid] < target:
        return binary_search_recursive(arr, target, mid + 1, right)
    else:
        return binary_search_recursive(arr, target, left, mid - 1)

# Space: O(log n) - call stack depth is log n
```

### Time vs Space Tradeoffs

Often you can trade space for time or vice versa:

```python
# Naive Fibonacci: O(2ⁿ) time, O(n) space (call stack)
def fib_slow(n):
    if n <= 1:
        return n
    return fib_slow(n - 1) + fib_slow(n - 2)

# Memoized Fibonacci: O(n) time, O(n) space
def fib_memo(n, memo={}):
    if n in memo:
        return memo[n]
    if n <= 1:
        return n
    memo[n] = fib_memo(n - 1, memo) + fib_memo(n - 2, memo)
    return memo[n]

# Iterative Fibonacci: O(n) time, O(1) space
def fib_iterative(n):
    if n <= 1:
        return n
    a, b = 0, 1
    for _ in range(2, n + 1):
        a, b = b, a + b
    return b
```

**Lesson:** Sometimes using more space (memoization) dramatically improves time!

---

## Best, Average, and Worst Case

Algorithms can have different performance depending on input:

### Quick Sort Example

```python
def quicksort(arr):
    if len(arr) <= 1:
        return arr

    pivot = arr[len(arr) // 2]
    left = [x for x in arr if x < pivot]
    middle = [x for x in arr if x == pivot]
    right = [x for x in arr if x > pivot]

    return quicksort(left) + middle + quicksort(right)
```

- **Best case:** O(n log n) - pivot always divides evenly
- **Average case:** O(n log n) - pivot is reasonably balanced
- **Worst case:** O(n²) - pivot is always smallest/largest (already sorted)

### When Each Matters

- **Worst case:** Guarantees (real-time systems, safety-critical)
- **Average case:** Typical performance
- **Best case:** Usually less useful (except for adaptive algorithms)

---

## Practical Tips

### 1. Recognize Patterns

| Code Pattern | Complexity |
|--------------|------------|
| Single loop | O(n) |
| Nested loops | O(n²) or O(n × m) |
| Halving/doubling | O(log n) |
| Divide and conquer | Often O(n log n) |
| Multiple independent loops | O(n + m) → O(n) if same input |
| Recursive tree | Often exponential |

### 2. Constants Matter in Practice

Big-O ignores constants, but constants matter in reality:

- O(100n) vs O(n) - same Big-O, but 100x difference!
- O(n log n) vs O(n²) - for n=10, might be 30 vs 100 (not huge)
- For small n, simpler O(n²) algorithm might beat complex O(n log n)

**Rule:** Use Big-O for understanding scaling, benchmark for real performance.

### 3. Choose Based on Input Size

| Input Size | Acceptable Complexity |
|------------|----------------------|
| n ≤ 10 | O(n!), O(2ⁿ) might be fine |
| n ≤ 100 | O(n³) tolerable |
| n ≤ 1,000 | O(n²) acceptable |
| n ≤ 10,000 | O(n log n) preferred |
| n ≤ 1,000,000 | O(n) or O(log n) needed |
| n > 1,000,000 | O(n) maximum, O(log n) or O(1) better |

### 4. Optimize When It Matters

**Premature optimization is the root of all evil.** - Donald Knuth

- Start with clear, correct code
- Profile to find bottlenecks
- Optimize the 10% of code that takes 90% of time
- Don't optimize O(n) to O(n/2) - same Big-O!

---

## Examples in Multiple Languages

### Python: Complexity Analysis

```python
# O(1) - constant time
def get_item(lst, index):
    return lst[index]

# O(n) - linear time
def contains(lst, target):
    for item in lst:
        if item == target:
            return True
    return False

# O(n²) - quadratic time
def has_duplicates(lst):
    for i in range(len(lst)):
        for j in range(i + 1, len(lst)):
            if lst[i] == lst[j]:
                return True
    return False

# O(log n) - logarithmic time
def binary_search(sorted_list, target):
    left, right = 0, len(sorted_list) - 1
    while left <= right:
        mid = (left + right) // 2
        if sorted_list[mid] == target:
            return mid
        elif sorted_list[mid] < target:
            left = mid + 1
        else:
            right = mid - 1
    return -1
```

### Java: Complexity Analysis

```java
public class ComplexityExamples {
    // O(1) - constant time
    public static int getItem(int[] arr, int index) {
        return arr[index];
    }

    // O(n) - linear time
    public static boolean contains(int[] arr, int target) {
        for (int item : arr) {
            if (item == target) {
                return true;
            }
        }
        return false;
    }

    // O(n²) - quadratic time
    public static boolean hasDuplicates(int[] arr) {
        for (int i = 0; i < arr.length; i++) {
            for (int j = i + 1; j < arr.length; j++) {
                if (arr[i] == arr[j]) {
                    return true;
                }
            }
        }
        return false;
    }

    // O(log n) - logarithmic time
    public static int binarySearch(int[] sortedArr, int target) {
        int left = 0;
        int right = sortedArr.length - 1;

        while (left <= right) {
            int mid = left + (right - left) / 2;

            if (sortedArr[mid] == target) {
                return mid;
            } else if (sortedArr[mid] < target) {
                left = mid + 1;
            } else {
                right = mid - 1;
            }
        }
        return -1;
    }
}
```

### Haskell: Complexity Analysis

```haskell
-- O(1) - constant time (accessing head)
getFirst :: [a] -> Maybe a
getFirst [] = Nothing
getFirst (x:_) = Just x

-- O(n) - linear time
contains :: Eq a => [a] -> a -> Bool
contains [] _ = False
contains (x:xs) target
    | x == target = True
    | otherwise = contains xs target

-- O(n²) - quadratic time
hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates [] = False
hasDuplicates (x:xs) = x `elem` xs || hasDuplicates xs

-- O(log n) - logarithmic time (on sorted list)
binarySearch :: Ord a => [a] -> a -> Maybe Int
binarySearch lst target = search 0 (length lst - 1)
  where
    search left right
        | left > right = Nothing
        | lst !! mid == target = Just mid
        | lst !! mid < target = search (mid + 1) right
        | otherwise = search left (mid - 1)
      where
        mid = (left + right) `div` 2
```

**Note:** Haskell's `!!` (list indexing) is O(n), not O(1) like array access! Lists are linked lists.

### C: Complexity Analysis

```c
#include <stdbool.h>

// O(1) - constant time
int get_item(int arr[], int index) {
    return arr[index];
}

// O(n) - linear time
bool contains(int arr[], int size, int target) {
    for (int i = 0; i < size; i++) {
        if (arr[i] == target) {
            return true;
        }
    }
    return false;
}

// O(n²) - quadratic time
bool has_duplicates(int arr[], int size) {
    for (int i = 0; i < size; i++) {
        for (int j = i + 1; j < size; j++) {
            if (arr[i] == arr[j]) {
                return true;
            }
        }
    }
    return false;
}

// O(log n) - logarithmic time
int binary_search(int sorted_arr[], int size, int target) {
    int left = 0;
    int right = size - 1;

    while (left <= right) {
        int mid = left + (right - left) / 2;

        if (sorted_arr[mid] == target) {
            return mid;
        } else if (sorted_arr[mid] < target) {
            left = mid + 1;
        } else {
            right = mid - 1;
        }
    }
    return -1;  // Not found
}
```

---

## Key Takeaways

1. **Big-O describes how algorithms scale** with input size
2. **Ignore constants and lower-order terms** - focus on growth rate
3. **Common complexities:** O(1), O(log n), O(n), O(n log n), O(n²), O(2ⁿ)
4. **Nested loops usually mean higher complexity** (but not always!)
5. **Halving/doubling suggests O(log n)**
6. **Space complexity matters too** - sometimes you trade space for time
7. **Best/average/worst cases can differ** - consider what matters for your use
8. **Algorithm choice depends on input size** and performance requirements
9. **Profile before optimizing** - don't guess where the bottleneck is

---

## Discussion Questions

1. Why do we ignore constant factors in Big-O notation? When might constants matter?

2. Can an O(n²) algorithm ever be faster than an O(n log n) algorithm? Under what circumstances?

3. How does the difference between array indexing (O(1)) and list indexing (O(n) in Haskell) affect algorithm design?

4. What are the tradeoffs between the three Fibonacci implementations shown (naive recursive, memoized, iterative)?

5. Why is binary search O(log n)? Can you explain it without using math?

6. If you have a dataset that's "mostly sorted," might a simple O(n²) algorithm like insertion sort beat an O(n log n) algorithm like merge sort?

---

## Next Lesson Preview

In Lesson 13, we'll apply complexity analysis to **sorting and searching algorithms**, implementing and comparing:
- Linear vs binary search
- Bubble sort, selection sort, insertion sort (O(n²))
- Merge sort, quick sort (O(n log n))
- When to use each algorithm

Understanding complexity gives you the tools to choose the right algorithm for the job!
