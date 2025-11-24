# Lesson 12: Algorithm Analysis  

## Overview

Algorithm analysis evaluates efficiency using Big-O notation to understand time and space complexity.

## Learning Objectives

- Understand Big-O, Big-Θ, Big-Ω notation
- Analyze time and space complexity
- Recognize common complexity classes
- Compare algorithmic approaches
- Optimize algorithms based on analysis

## Big-O Notation

**Common Complexities:**
- O(1) - Constant
- O(log n) - Logarithmic
- O(n) - Linear
- O(n log n) - Linearithmic
- O(n²) - Quadratic
- O(2ⁿ) - Exponential

**Examples:**

```python
# O(1) - Constant
def get_first(arr):
    return arr[0]

# O(n) - Linear
def sum_array(arr):
    total = 0
    for x in arr:
        total += x
    return total

# O(n²) - Quadratic
def bubble_sort(arr):
    n = len(arr)
    for i in range(n):
        for j in range(n-1):
            if arr[j] > arr[j+1]:
                arr[j], arr[j+1] = arr[j+1], arr[j]

# O(log n) - Logarithmic
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
    return -1
```

## Space Complexity

**Examples:**

```cpp
// O(1) space
int sum(vector<int>& arr) {
    int total = 0;
    for (int x : arr) total += x;
    return total;
}

// O(n) space
vector<int> double_values(const vector<int>& arr) {
    vector<int> result;
    for (int x : arr) result.push_back(x * 2);
    return result;
}
```

## Analyzing Recursive Algorithms

**Fibonacci (Naive):**
```python
def fib(n):
    if n <= 1:
        return n
    return fib(n-1) + fib(n-2)
# Time: O(2ⁿ), Space: O(n) call stack
```

**Fibonacci (Dynamic Programming):**
```python
def fib_dp(n):
    if n <= 1:
        return n
    dp = [0] * (n + 1)
    dp[1] = 1
    for i in range(2, n + 1):
        dp[i] = dp[i-1] + dp[i-2]
    return dp[n]
# Time: O(n), Space: O(n)
```

## Key Takeaways

1. **Big-O** describes worst-case growth rate
2. **Analyze** both time and space complexity
3. **Trade-offs** often exist between time and space
4. **Constant factors** matter in practice
5. **Choose** algorithms based on problem constraints

See EXERCISES.md for complexity analysis practice.
