# Lab 26: Problem Solving Workshop

**Quarter 3, Week 6**
**Duration:** 90 minutes
**Format:** Individual practice with group discussion

## Overview

Problem-solving is a skill that improves with practice. This lab tackles classic algorithmic problems using techniques like dynamic programming, greedy algorithms, and divide-and-conquer.

## Objectives

By the end of this lab, you will:
- [ ] Apply a systematic problem-solving approach
- [ ] Recognize common problem patterns
- [ ] Implement solutions using dynamic programming
- [ ] Know when to use different techniques

## Setup

- Create folder: `lab26-problems/`
- Files: `problems.py`
- Whiteboard/paper for thinking

---

## Part 1: Problem-Solving Framework (10 minutes)

### The UMPIRE Method

1. **U**nderstand - What is the problem asking?
2. **M**atch - What pattern does this match?
3. **P**lan - How will you solve it?
4. **I**mplement - Write the code
5. **R**eview - Test and optimize
6. **E**valuate - Analyze complexity

### Common Patterns

| Pattern | Use When | Examples |
|---------|----------|----------|
| **Brute Force** | Small inputs, simplicity | Generate all combinations |
| **Greedy** | Local optimal → global optimal | Coin change (some cases) |
| **Divide & Conquer** | Can split into subproblems | Merge sort, binary search |
| **Dynamic Programming** | Overlapping subproblems | Fibonacci, paths, knapsack |
| **Two Pointers** | Array/string traversal | Two sum, palindrome |
| **Sliding Window** | Contiguous subarrays | Max sum subarray |

---

## Part 2: Dynamic Programming Basics (25 minutes)

### Activity 2.1: The Fibonacci Example

**Naive recursive (slow):**
```python
def fib_naive(n):
    """O(2^n) - very slow!"""
    if n <= 1:
        return n
    return fib_naive(n-1) + fib_naive(n-2)
```

Problem: Recalculates same values many times!
```
fib(5)
├── fib(4)
│   ├── fib(3)
│   │   ├── fib(2)
│   │   └── fib(1)
│   └── fib(2)
└── fib(3)
    ├── fib(2)
    └── fib(1)
```

**Memoization (top-down DP):**
```python
def fib_memo(n, memo=None):
    """O(n) with memoization."""
    if memo is None:
        memo = {}
    if n <= 1:
        return n
    if n not in memo:
        memo[n] = fib_memo(n-1, memo) + fib_memo(n-2, memo)
    return memo[n]
```

**Tabulation (bottom-up DP):**
```python
def fib_tab(n):
    """O(n) bottom-up."""
    if n <= 1:
        return n
    dp = [0] * (n + 1)
    dp[1] = 1
    for i in range(2, n + 1):
        dp[i] = dp[i-1] + dp[i-2]
    return dp[n]
```

**Space-optimized:**
```python
def fib_opt(n):
    """O(n) time, O(1) space."""
    if n <= 1:
        return n
    prev2, prev1 = 0, 1
    for _ in range(2, n + 1):
        prev2, prev1 = prev1, prev2 + prev1
    return prev1
```

### Activity 2.2: Climbing Stairs

**Problem:** You can climb 1 or 2 stairs at a time. How many ways to reach step n?

```
n=1: 1 way  (1)
n=2: 2 ways (1+1, 2)
n=3: 3 ways (1+1+1, 1+2, 2+1)
n=4: 5 ways (1+1+1+1, 1+1+2, 1+2+1, 2+1+1, 2+2)
```

**Solution:**
```python
def climb_stairs(n):
    """
    Number of ways to climb n stairs.
    At each step, can climb 1 or 2 stairs.
    """
    if n <= 2:
        return n

    # dp[i] = number of ways to reach step i
    dp = [0] * (n + 1)
    dp[1] = 1
    dp[2] = 2

    for i in range(3, n + 1):
        dp[i] = dp[i-1] + dp[i-2]  # Come from i-1 or i-2

    return dp[n]

# Test
for i in range(1, 8):
    print(f"climb_stairs({i}) = {climb_stairs(i)}")
```

**Your turn:** What if you could climb 1, 2, or 3 stairs at a time?

### Activity 2.3: Coin Change

**Problem:** Given coins of certain denominations, find minimum coins to make amount.

```python
def coin_change(coins, amount):
    """
    Minimum coins needed to make amount.
    Returns -1 if impossible.
    """
    # dp[i] = min coins to make amount i
    dp = [float('inf')] * (amount + 1)
    dp[0] = 0  # 0 coins to make 0

    for i in range(1, amount + 1):
        for coin in coins:
            if coin <= i and dp[i - coin] != float('inf'):
                dp[i] = min(dp[i], dp[i - coin] + 1)

    return dp[amount] if dp[amount] != float('inf') else -1

# Test
print(coin_change([1, 5, 10, 25], 30))  # 2 (25 + 5)
print(coin_change([1, 5, 10, 25], 11))  # 2 (10 + 1)
print(coin_change([2], 3))              # -1 (impossible)
```

### ✅ Checkpoint 1

Verify:
- [ ] Understand memoization vs tabulation
- [ ] Climbing stairs works
- [ ] Coin change works

---

## Part 3: Classic DP Problems (30 minutes)

### Activity 3.1: Longest Common Subsequence

**Problem:** Find the longest subsequence present in both strings.

```
"ABCDGH" and "AEDFHR" → "ADH" (length 3)
"AGGTAB" and "GXTXAYB" → "GTAB" (length 4)
```

```python
def lcs(s1, s2):
    """
    Length of longest common subsequence.
    """
    m, n = len(s1), len(s2)

    # dp[i][j] = LCS of s1[0:i] and s2[0:j]
    dp = [[0] * (n + 1) for _ in range(m + 1)]

    for i in range(1, m + 1):
        for j in range(1, n + 1):
            if s1[i-1] == s2[j-1]:
                dp[i][j] = dp[i-1][j-1] + 1
            else:
                dp[i][j] = max(dp[i-1][j], dp[i][j-1])

    return dp[m][n]

# Test
print(lcs("ABCDGH", "AEDFHR"))  # 3
print(lcs("AGGTAB", "GXTXAYB"))  # 4
```

**Your turn:** Modify to return the actual subsequence, not just length.

### Activity 3.2: 0/1 Knapsack

**Problem:** Given items with weights and values, maximize value within weight capacity.

```python
def knapsack(weights, values, capacity):
    """
    Maximum value that fits in knapsack.
    Each item can be used at most once.
    """
    n = len(weights)

    # dp[i][w] = max value using items 0..i-1 with capacity w
    dp = [[0] * (capacity + 1) for _ in range(n + 1)]

    for i in range(1, n + 1):
        for w in range(capacity + 1):
            # Don't take item i-1
            dp[i][w] = dp[i-1][w]

            # Take item i-1 (if it fits)
            if weights[i-1] <= w:
                dp[i][w] = max(
                    dp[i][w],
                    dp[i-1][w - weights[i-1]] + values[i-1]
                )

    return dp[n][capacity]

# Test
weights = [2, 3, 4, 5]
values = [3, 4, 5, 6]
capacity = 8
print(knapsack(weights, values, capacity))  # 10 (items 1,3: 3+5 weight, 4+6 value)
```

### Activity 3.3: Grid Paths

**Problem:** Count paths from top-left to bottom-right (only right and down moves).

```python
def grid_paths(m, n):
    """
    Number of paths in m x n grid.
    Can only move right or down.
    """
    # dp[i][j] = paths to reach (i, j)
    dp = [[0] * n for _ in range(m)]

    # First row and column have only 1 path each
    for i in range(m):
        dp[i][0] = 1
    for j in range(n):
        dp[0][j] = 1

    # Fill rest
    for i in range(1, m):
        for j in range(1, n):
            dp[i][j] = dp[i-1][j] + dp[i][j-1]

    return dp[m-1][n-1]

# Test
print(grid_paths(3, 3))  # 6
print(grid_paths(3, 7))  # 28
```

**Your turn:** Modify to handle obstacles (some cells blocked).

### ✅ Checkpoint 2

Verify:
- [ ] LCS works
- [ ] Knapsack works
- [ ] Grid paths works

---

## Part 4: Other Techniques (20 minutes)

### Activity 4.1: Two Pointers - Two Sum

```python
def two_sum_sorted(nums, target):
    """
    Find two numbers in sorted array that add to target.
    Returns their indices.
    """
    left, right = 0, len(nums) - 1

    while left < right:
        current_sum = nums[left] + nums[right]

        if current_sum == target:
            return [left, right]
        elif current_sum < target:
            left += 1
        else:
            right -= 1

    return []

# Test
print(two_sum_sorted([2, 7, 11, 15], 9))   # [0, 1]
print(two_sum_sorted([1, 2, 3, 4, 6], 6))  # [1, 3]
```

### Activity 4.2: Sliding Window - Max Sum Subarray

```python
def max_sum_subarray(nums, k):
    """
    Maximum sum of any contiguous subarray of size k.
    """
    if len(nums) < k:
        return 0

    # Initial window sum
    window_sum = sum(nums[:k])
    max_sum = window_sum

    # Slide window
    for i in range(k, len(nums)):
        window_sum += nums[i] - nums[i - k]
        max_sum = max(max_sum, window_sum)

    return max_sum

# Test
print(max_sum_subarray([2, 1, 5, 1, 3, 2], 3))  # 9 (5+1+3)
print(max_sum_subarray([2, 3, 4, 1, 5], 2))     # 7 (3+4)
```

### Activity 4.3: Greedy - Activity Selection

```python
def activity_selection(activities):
    """
    Select maximum non-overlapping activities.
    activities: list of (start, end) tuples.
    """
    # Sort by end time (greedy choice)
    sorted_activities = sorted(activities, key=lambda x: x[1])

    selected = [sorted_activities[0]]
    last_end = sorted_activities[0][1]

    for start, end in sorted_activities[1:]:
        if start >= last_end:  # Non-overlapping
            selected.append((start, end))
            last_end = end

    return selected

# Test
activities = [(1, 4), (3, 5), (0, 6), (5, 7), (3, 9), (5, 9), (6, 10), (8, 11), (8, 12), (2, 14), (12, 16)]
print(activity_selection(activities))
# [(1, 4), (5, 7), (8, 11), (12, 16)]
```

### ✅ Checkpoint 3

Verify:
- [ ] Two pointers technique works
- [ ] Sliding window technique works
- [ ] Greedy activity selection works

---

## Part 5: Practice Problems (5 minutes intro)

### Problem Bank

Try these on your own or in pairs:

1. **Maximum Subarray** (Kadane's algorithm)
   - Find contiguous subarray with largest sum

2. **Edit Distance**
   - Minimum operations to transform one string to another

3. **Longest Increasing Subsequence**
   - Find longest subsequence where elements are increasing

4. **House Robber**
   - Maximum loot without robbing adjacent houses

5. **Word Break**
   - Can string be segmented into dictionary words?

---

## Challenges

### Challenge 1: Matrix Chain Multiplication

Given dimensions of matrices, find optimal order to multiply them.

### Challenge 2: Palindrome Partitioning

Minimum cuts needed to partition string into palindromes.

### Challenge 3: Implement LRU Cache

Design a cache with O(1) get and put operations.

---

## Wrap-Up

**Key takeaways:**

1. **Identify the pattern** before coding
2. **DP** = optimal substructure + overlapping subproblems
3. **Greedy** = local optimal leads to global optimal
4. **Two pointers** = efficient array traversal
5. **Practice** is the only way to improve

**DP approach:**
1. Define what dp[i] means
2. Find the recurrence relation
3. Identify base cases
4. Determine traversal order
5. Optimize space if needed

**Next lab:** Logic Programming with Prolog!
