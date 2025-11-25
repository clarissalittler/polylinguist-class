"""
Lesson 12: Algorithm Analysis - Python Examples
Understanding time and space complexity through practical examples
"""

import time
import random
from typing import List

# =============================================================================
# O(1) - CONSTANT TIME
# =============================================================================

def get_first(items: List[int]) -> int:
    """O(1) - Always one operation regardless of list size"""
    return items[0] if items else None

def get_by_index(items: List[int], index: int) -> int:
    """O(1) - Direct array access"""
    if 0 <= index < len(items):
        return items[index]
    return None

def is_even(n: int) -> bool:
    """O(1) - Single arithmetic operation"""
    return n % 2 == 0

print("=== O(1) Constant Time ===")
data = list(range(1000000))
start = time.perf_counter()
result = get_first(data)
elapsed = time.perf_counter() - start
print(f"get_first on 1,000,000 items: {elapsed:.6f}s")

# =============================================================================
# O(log n) - LOGARITHMIC TIME
# =============================================================================

def binary_search(sorted_list: List[int], target: int) -> int:
    """O(log n) - Halves search space each iteration"""
    left, right = 0, len(sorted_list) - 1
    comparisons = 0

    while left <= right:
        comparisons += 1
        mid = (left + right) // 2
        if sorted_list[mid] == target:
            print(f"  Found in {comparisons} comparisons")
            return mid
        elif sorted_list[mid] < target:
            left = mid + 1
        else:
            right = mid - 1

    print(f"  Not found after {comparisons} comparisons")
    return -1

print("\n=== O(log n) Logarithmic Time ===")
sorted_data = list(range(1000000))
print("Binary search in 1,000,000 items:")
binary_search(sorted_data, 750000)  # ~20 comparisons for 1M items

# =============================================================================
# O(n) - LINEAR TIME
# =============================================================================

def linear_search(items: List[int], target: int) -> int:
    """O(n) - Must check each element"""
    for i, item in enumerate(items):
        if item == target:
            return i
    return -1

def find_max(items: List[int]) -> int:
    """O(n) - Must examine every element"""
    if not items:
        return None
    maximum = items[0]
    for item in items[1:]:
        if item > maximum:
            maximum = item
    return maximum

def sum_all(items: List[int]) -> int:
    """O(n) - Process every element once"""
    total = 0
    for item in items:
        total += item
    return total

print("\n=== O(n) Linear Time ===")
data = list(range(100000))
start = time.perf_counter()
result = sum_all(data)
elapsed = time.perf_counter() - start
print(f"sum_all on 100,000 items: {elapsed:.6f}s")

# =============================================================================
# O(n log n) - LINEARITHMIC TIME
# =============================================================================

def merge_sort(items: List[int]) -> List[int]:
    """O(n log n) - Efficient divide-and-conquer sort"""
    if len(items) <= 1:
        return items

    mid = len(items) // 2
    left = merge_sort(items[:mid])
    right = merge_sort(items[mid:])

    return merge(left, right)

def merge(left: List[int], right: List[int]) -> List[int]:
    """Merge two sorted lists"""
    result = []
    i = j = 0

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

print("\n=== O(n log n) Linearithmic Time ===")
data = [random.randint(0, 10000) for _ in range(10000)]
start = time.perf_counter()
sorted_data = merge_sort(data)
elapsed = time.perf_counter() - start
print(f"merge_sort on 10,000 items: {elapsed:.6f}s")

# =============================================================================
# O(n²) - QUADRATIC TIME
# =============================================================================

def bubble_sort(items: List[int]) -> List[int]:
    """O(n²) - Nested loops comparing all pairs"""
    result = items.copy()
    n = len(result)
    comparisons = 0

    for i in range(n):
        for j in range(0, n - i - 1):
            comparisons += 1
            if result[j] > result[j + 1]:
                result[j], result[j + 1] = result[j + 1], result[j]

    return result

def has_duplicates_naive(items: List[int]) -> bool:
    """O(n²) - Compare each pair"""
    n = len(items)
    for i in range(n):
        for j in range(i + 1, n):
            if items[i] == items[j]:
                return True
    return False

def has_duplicates_efficient(items: List[int]) -> bool:
    """O(n) - Use a set for O(1) lookups"""
    seen = set()
    for item in items:
        if item in seen:
            return True
        seen.add(item)
    return False

print("\n=== O(n²) Quadratic Time ===")
data = [random.randint(0, 1000) for _ in range(1000)]
start = time.perf_counter()
sorted_data = bubble_sort(data)
elapsed = time.perf_counter() - start
print(f"bubble_sort on 1,000 items: {elapsed:.6f}s")

# Compare O(n²) vs O(n) duplicate checking
print("\n=== Comparing O(n²) vs O(n) ===")
data = list(range(5000)) + [2500]  # Has one duplicate

start = time.perf_counter()
has_duplicates_naive(data)
naive_time = time.perf_counter() - start

start = time.perf_counter()
has_duplicates_efficient(data)
efficient_time = time.perf_counter() - start

print(f"Naive O(n²): {naive_time:.6f}s")
print(f"Efficient O(n): {efficient_time:.6f}s")
print(f"Speedup: {naive_time/efficient_time:.1f}x faster")

# =============================================================================
# O(2^n) - EXPONENTIAL TIME
# =============================================================================

def fibonacci_naive(n: int) -> int:
    """O(2^n) - Exponential due to redundant computation"""
    if n <= 1:
        return n
    return fibonacci_naive(n - 1) + fibonacci_naive(n - 2)

def fibonacci_memo(n: int, memo: dict = None) -> int:
    """O(n) - Memoization eliminates redundant work"""
    if memo is None:
        memo = {}
    if n in memo:
        return memo[n]
    if n <= 1:
        return n
    memo[n] = fibonacci_memo(n - 1, memo) + fibonacci_memo(n - 2, memo)
    return memo[n]

print("\n=== O(2^n) vs O(n) - Fibonacci ===")
n = 30

start = time.perf_counter()
result = fibonacci_naive(n)
naive_time = time.perf_counter() - start

start = time.perf_counter()
result = fibonacci_memo(n)
memo_time = time.perf_counter() - start

print(f"fibonacci({n}) naive O(2^n): {naive_time:.6f}s")
print(f"fibonacci({n}) memo O(n): {memo_time:.6f}s")

# =============================================================================
# SPACE COMPLEXITY
# =============================================================================

def space_constant(n: int) -> int:
    """O(1) space - Fixed number of variables"""
    total = 0
    for i in range(n):
        total += i
    return total

def space_linear(n: int) -> List[int]:
    """O(n) space - Creates list proportional to input"""
    return [i * 2 for i in range(n)]

def space_quadratic(n: int) -> List[List[int]]:
    """O(n²) space - Creates n x n matrix"""
    return [[i * j for j in range(n)] for i in range(n)]

print("\n=== Space Complexity ===")
print("O(1) space: Uses fixed variables regardless of n")
print("O(n) space: Creates data structure of size n")
print("O(n²) space: Creates n×n matrix")

# =============================================================================
# AMORTIZED ANALYSIS
# =============================================================================

print("\n=== Amortized Analysis ===")
print("List.append() is O(1) amortized")
print("Occasionally O(n) when resizing, but averages to O(1)")

items = []
for i in range(100000):
    items.append(i)  # O(1) amortized

# =============================================================================
# COMPARISON TABLE
# =============================================================================

print("\n=== Complexity Comparison ===")
print("""
| Complexity | n=10    | n=100   | n=1000   | n=10000    |
|------------|---------|---------|----------|------------|
| O(1)       | 1       | 1       | 1        | 1          |
| O(log n)   | 3       | 7       | 10       | 13         |
| O(n)       | 10      | 100     | 1000     | 10000      |
| O(n log n) | 33      | 664     | 9966     | 132877     |
| O(n²)      | 100     | 10000   | 1000000  | 100000000  |
| O(2^n)     | 1024    | 10^30   | 10^301   | ∞          |
""")

# =============================================================================
# PRACTICAL TIPS
# =============================================================================

print("=== Practical Analysis Tips ===")
print("""
1. Count the loops:
   - Single loop over n items → O(n)
   - Nested loops → O(n²)
   - Loop that halves input → O(log n)

2. Watch for hidden complexity:
   - 'in' operator on list → O(n)
   - 'in' operator on set/dict → O(1)
   - list.sort() → O(n log n)
   - String concatenation in loop → O(n²)

3. Space-time tradeoffs:
   - Caching/memoization trades space for time
   - Sorting first can enable faster algorithms
""")
