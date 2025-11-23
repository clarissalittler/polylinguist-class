# Lesson 12: Algorithm Analysis - Exercises

## Exercise 1: Identifying Complexity

For each function below, determine its time complexity. Explain your reasoning.

### 1a. Simple Sum
```python
def sum_array(arr):
    total = 0
    for num in arr:
        total += num
    return total
```

<details>
<summary>Answer</summary>

**O(n)** - Single loop that runs n times, where n is the length of the array. Each iteration does O(1) work.
</details>

---

### 1b. Nested Loop
```python
def print_grid(n):
    for i in range(n):
        for j in range(n):
            print(i, j)
```

<details>
<summary>Answer</summary>

**O(n²)** - Outer loop runs n times, inner loop runs n times for each outer iteration. Total: n × n = n².
</details>

---

### 1c. Three Loops
```python
def three_loops(arr):
    for item in arr:
        print(item)

    for item in arr:
        print(item * 2)

    for item in arr:
        print(item * 3)
```

<details>
<summary>Answer</summary>

**O(n)** - Three sequential loops, each O(n). Total: O(n) + O(n) + O(n) = O(3n) = O(n). We drop the constant.
</details>

---

### 1d. Growing Loop
```python
def growing_loop(n):
    i = 1
    while i < n:
        print(i)
        i = i * 2
```

<details>
<summary>Answer</summary>

**O(log n)** - i doubles each iteration (1, 2, 4, 8, 16, ...). The loop runs log₂(n) times.
</details>

---

### 1e. Triangular Loop
```python
def triangular(n):
    for i in range(n):
        for j in range(i):
            print(i, j)
```

<details>
<summary>Answer</summary>

**O(n²)** - Inner loop runs 0, 1, 2, 3, ..., n-1 times. Total: 0 + 1 + 2 + ... + (n-1) = n(n-1)/2 = O(n²).
</details>

---

### 1f. Multiple Arrays
```python
def process_two(arr1, arr2):
    for item in arr1:
        print(item)

    for item in arr2:
        print(item)
```

<details>
<summary>Answer</summary>

**O(n + m)** where n = len(arr1), m = len(arr2). Can't assume they're the same size. If they are equal, it's O(n).
</details>

---

### 1g. Nested Different Sizes
```python
def nested_different(arr1, arr2):
    for item1 in arr1:
        for item2 in arr2:
            print(item1, item2)
```

<details>
<summary>Answer</summary>

**O(n × m)** where n = len(arr1), m = len(arr2). If equal sizes, it's O(n²).
</details>

---

## Exercise 2: Analyzing Real Code

Analyze the time complexity of these more complex functions:

### 2a. Find Pairs That Sum
```python
def find_pair_sum(arr, target):
    """Find if any two numbers in arr sum to target"""
    for i in range(len(arr)):
        for j in range(i + 1, len(arr)):
            if arr[i] + arr[j] == target:
                return True
    return False
```

What's the complexity? Can you think of a more efficient approach?

<details>
<summary>Answer</summary>

**Complexity:** O(n²) - nested loops check all pairs.

**Better approach:** O(n) using a hash set:
```python
def find_pair_sum_fast(arr, target):
    seen = set()
    for num in arr:
        complement = target - num
        if complement in seen:
            return True
        seen.add(num)
    return False
```
This is O(n) time, O(n) space.
</details>

---

### 2b. Is Sorted
```python
def is_sorted(arr):
    """Check if array is sorted in ascending order"""
    for i in range(len(arr) - 1):
        if arr[i] > arr[i + 1]:
            return False
    return True
```

What's the best, average, and worst case complexity?

<details>
<summary>Answer</summary>

- **Best case:** O(1) - if first two elements are out of order, return immediately
- **Average case:** O(n) - on average, check about half the array
- **Worst case:** O(n) - array is sorted or reverse-sorted, must check all n-1 pairs
</details>

---

### 2c. Recursive Factorial
```python
def factorial(n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)
```

Analyze both time and space complexity.

<details>
<summary>Answer</summary>

- **Time complexity:** O(n) - makes n recursive calls
- **Space complexity:** O(n) - call stack depth is n
</details>

---

### 2d. Remove Duplicates (Inefficient)
```python
def remove_duplicates(arr):
    result = []
    for item in arr:
        if item not in result:  # Contains check
            result.append(item)
    return result
```

What's the complexity? How could it be improved?

<details>
<summary>Answer</summary>

**Complexity:** O(n²) - for each of n items, we check if it's in result (which takes O(n) for lists).

**Better approach:** O(n) using a set:
```python
def remove_duplicates_fast(arr):
    seen = set()
    result = []
    for item in arr:
        if item not in seen:  # O(1) set lookup
            seen.add(item)
            result.append(item)
    return result
```
</details>

---

## Exercise 3: Space Complexity

Determine the space complexity of each function:

### 3a.
```python
def reverse_array(arr):
    result = []
    for i in range(len(arr) - 1, -1, -1):
        result.append(arr[i])
    return result
```

<details>
<summary>Answer</summary>

**O(n)** - creates new array of size n.
</details>

---

### 3b.
```python
def reverse_in_place(arr):
    left = 0
    right = len(arr) - 1
    while left < right:
        arr[left], arr[right] = arr[right], arr[left]
        left += 1
        right -= 1
    return arr
```

<details>
<summary>Answer</summary>

**O(1)** - only uses a constant number of variables (left, right), modifies array in place.
</details>

---

### 3c. Recursive Sum
```python
def sum_recursive(arr, index=0):
    if index >= len(arr):
        return 0
    return arr[index] + sum_recursive(arr, index + 1)
```

<details>
<summary>Answer</summary>

**O(n)** - call stack depth is n (one call per array element).
</details>

---

## Exercise 4: Optimize These Algorithms

For each inefficient function, write a more efficient version and explain the complexity improvement.

### 4a. Inefficient: Find if Value Exists

```python
def contains_slow(arr, target):
    """O(n²) - why?"""
    for i in range(len(arr)):
        for j in range(len(arr)):
            if arr[j] == target and i == j:
                return True
    return False
```

Write an O(n) version.

<details>
<summary>Answer</summary>

```python
def contains_fast(arr, target):
    """O(n)"""
    for item in arr:
        if item == target:
            return True
    return False
```

The original was doing unnecessary nested loops. We only need one pass!
</details>

---

### 4b. Inefficient: Count Occurrences

```python
def count_occurrences_slow(arr, target):
    """Count how many times target appears - O(n²)"""
    count = 0
    for i in range(len(arr)):
        matches = 0
        for j in range(len(arr)):
            if arr[j] == target:
                matches += 1
        if arr[i] == target:
            count = matches
    return count
```

Write an O(n) version.

<details>
<summary>Answer</summary>

```python
def count_occurrences_fast(arr, target):
    """O(n)"""
    count = 0
    for item in arr:
        if item == target:
            count += 1
    return count
```

Original unnecessarily recalculated the count for every element!
</details>

---

## Exercise 5: Real-World Scenario

You're building a search feature for a contacts app with 10,000 contacts.

### 5a. Linear Search
You implement linear search (O(n)). How long will it take to search if each comparison takes 1 microsecond (μs)?

<details>
<summary>Answer</summary>

- Best case: 1 μs (found immediately)
- Average case: 5,000 μs = 5 milliseconds
- Worst case: 10,000 μs = 10 milliseconds
</details>

---

### 5b. Binary Search
If you sort the contacts and use binary search (O(log n)), how long for worst case?

<details>
<summary>Answer</summary>

log₂(10,000) ≈ 13.3 → about 14 comparisons

Worst case: 14 μs = 0.014 milliseconds

**700× faster than linear search worst case!**
</details>

---

### 5c. Hash Table
If you use a hash table (O(1) average), how long?

<details>
<summary>Answer</summary>

Average case: 1 μs (constant time!)

**10,000× faster than linear search worst case!**

But requires O(n) space for the hash table.
</details>

---

## Exercise 6: Implementation Challenge

Implement these functions in at least 2 languages (Python, Java, Haskell, C, or Rust). Analyze their complexity.

### 6a. Find Maximum

Write a function to find the maximum element in an array.

**Requirements:**
- Should work for unsorted arrays
- What's the best possible time complexity? Can you achieve it?

```python
def find_max(arr):
    # Your implementation here
    pass
```

<details>
<summary>Solution & Analysis</summary>

```python
def find_max(arr):
    if not arr:
        return None

    max_val = arr[0]
    for num in arr:
        if num > max_val:
            max_val = num
    return max_val
```

**Time complexity:** O(n) - must look at every element at least once

**Space complexity:** O(1) - only uses one variable

**This is optimal** - you can't find the maximum without examining all elements.
</details>

---

### 6b. Find Second Largest

Write a function to find the second largest element in an array.

```python
def find_second_largest(arr):
    # Your implementation here
    pass
```

<details>
<summary>Solution & Analysis</summary>

```python
def find_second_largest(arr):
    if len(arr) < 2:
        return None

    first = second = float('-inf')

    for num in arr:
        if num > first:
            second = first
            first = num
        elif num > second and num != first:
            second = num

    return second if second != float('-inf') else None
```

**Time complexity:** O(n) - single pass through array

**Space complexity:** O(1) - only two variables

**Note:** Sorting would be O(n log n), slower than necessary!
</details>

---

### 6c. Fibonacci (Three Ways)

Implement Fibonacci three ways with different time/space tradeoffs:

1. **Naive recursive** - simple but slow
2. **Memoized** - cache results
3. **Iterative** - most efficient

Compare their performance for n = 30.

<details>
<summary>Solutions & Analysis</summary>

```python
# 1. Naive recursive - O(2ⁿ) time, O(n) space
def fib_naive(n):
    if n <= 1:
        return n
    return fib_naive(n - 1) + fib_naive(n - 2)

# 2. Memoized - O(n) time, O(n) space
def fib_memo(n, memo={}):
    if n in memo:
        return memo[n]
    if n <= 1:
        return n
    memo[n] = fib_memo(n - 1, memo) + fib_memo(n - 2, memo)
    return memo[n]

# 3. Iterative - O(n) time, O(1) space
def fib_iter(n):
    if n <= 1:
        return n
    a, b = 0, 1
    for _ in range(2, n + 1):
        a, b = b, a + b
    return b
```

| Version | Time | Space | fib(30) calls |
|---------|------|-------|---------------|
| Naive | O(2ⁿ) | O(n) | 2,692,537 |
| Memoized | O(n) | O(n) | 59 |
| Iterative | O(n) | O(1) | 0 (no recursion) |

**Lesson:** Memoization can transform exponential to linear! But iterative is best if you don't need recursion.
</details>

---

## Exercise 7: Challenge Problems

### 7a. Optimize This Code

```python
def mysterious_function(n):
    result = 0
    for i in range(n):
        for j in range(n):
            for k in range(n):
                result += 1
    return result
```

1. What does this function compute?
2. What's its time complexity?
3. Can you compute the same result in O(1) time?

<details>
<summary>Answer</summary>

1. It computes n³ (counts from 0 to n³ - 1)
2. O(n³) - three nested loops
3. Yes! Just return `n ** 3` - O(1)

**Lesson:** Understanding what code *does* can reveal optimization opportunities!
</details>

---

### 7b. Space-Time Tradeoff

Given an array, count how many times each element appears.

```python
def count_frequencies_slow(arr):
    """O(n²) time, O(1) space (not counting output)"""
    result = {}
    for item in arr:
        count = 0
        for other in arr:
            if other == item:
                count += 1
        result[item] = count
    return result
```

Can you do better than O(n²)? What's the tradeoff?

<details>
<summary>Answer</summary>

```python
def count_frequencies_fast(arr):
    """O(n) time, O(n) space"""
    result = {}
    for item in arr:
        result[item] = result.get(item, 0) + 1
    return result
```

**Tradeoff:** We use O(n) space (the dictionary) to achieve O(n) time instead of O(n²).

This is a common pattern: trading space for time!
</details>

---

## Exercise 8: Reflection Questions

1. **Constant Factors:** Give an example where an O(n²) algorithm might be faster than an O(n log n) algorithm in practice.

2. **Worst vs Average:** Why do we usually care more about worst-case complexity than average-case?

3. **Space Matters:** Describe a scenario where you'd choose an O(n²) time, O(1) space algorithm over an O(n) time, O(n) space algorithm.

4. **Real World:** Find a real function in Python's standard library (or another language) and determine its complexity. Check the documentation - is it what you expected?

5. **Premature Optimization:** Explain the saying "Premature optimization is the root of all evil" in your own words. When *should* you optimize?

---

## Bonus: Advanced Exercises

### Bonus 1: Master Theorem

Research the Master Theorem for analyzing divide-and-conquer algorithms. Use it to analyze:
- Merge sort
- Binary search
- Karatsuba multiplication

### Bonus 2: Amortized Analysis

Look up "amortized analysis" and explain why Python's list.append() is considered O(1) even though it sometimes requires resizing.

### Bonus 3: NP-Completeness

Research what "NP-complete" means. Name three famous NP-complete problems. Why do they matter?

---

## Summary

By completing these exercises, you should be able to:
- ✅ Identify time complexity of loops and recursive functions
- ✅ Analyze space complexity
- ✅ Optimize algorithms by reducing complexity
- ✅ Make informed tradeoffs between time and space
- ✅ Choose appropriate algorithms based on input size
- ✅ Recognize common complexity patterns in code

**Next:** Apply this knowledge to sorting and searching algorithms in Lesson 13!
