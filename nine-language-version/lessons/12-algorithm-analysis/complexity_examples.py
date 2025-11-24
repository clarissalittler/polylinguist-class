"""
Lesson 12: Algorithm Complexity Examples in Python

This file demonstrates various complexity classes with working examples.
Run each function and observe how performance changes with input size.
"""

import time


def time_function(func, *args):
    """Helper to measure execution time"""
    start = time.time()
    result = func(*args)
    end = time.time()
    return result, (end - start) * 1000  # Return result and time in milliseconds


# ============================================================================
# O(1) - CONSTANT TIME
# ============================================================================

def constant_time_access(arr, index):
    """O(1) - Array access is constant time regardless of array size"""
    return arr[index]


def constant_time_hash_lookup(dictionary, key):
    """O(1) - Hash table lookup (average case)"""
    return dictionary.get(key)


# ============================================================================
# O(log n) - LOGARITHMIC TIME
# ============================================================================

def binary_search(sorted_arr, target):
    """
    O(log n) - Binary search on sorted array
    Each iteration eliminates half the remaining elements
    """
    left, right = 0, len(sorted_arr) - 1

    while left <= right:
        mid = (left + right) // 2

        if sorted_arr[mid] == target:
            return mid
        elif sorted_arr[mid] < target:
            left = mid + 1
        else:
            right = mid - 1

    return -1  # Not found


def power_of_two_below(n):
    """
    O(log n) - Find largest power of 2 less than n
    Doubles each iteration
    """
    power = 1
    while power * 2 <= n:
        power *= 2
    return power


# ============================================================================
# O(n) - LINEAR TIME
# ============================================================================

def linear_search(arr, target):
    """O(n) - Must check each element until found"""
    for i, item in enumerate(arr):
        if item == target:
            return i
    return -1


def find_max(arr):
    """O(n) - Must examine every element"""
    if not arr:
        return None

    max_val = arr[0]
    for num in arr:
        if num > max_val:
            max_val = num
    return max_val


def sum_array(arr):
    """O(n) - Single pass through array"""
    total = 0
    for num in arr:
        total += num
    return total


# ============================================================================
# O(n log n) - LOG-LINEAR TIME
# ============================================================================

def merge_sort(arr):
    """
    O(n log n) - Efficient sorting algorithm
    Divides array log(n) times, merges in O(n) at each level
    """
    if len(arr) <= 1:
        return arr

    # Divide
    mid = len(arr) // 2
    left = merge_sort(arr[:mid])
    right = merge_sort(arr[mid:])

    # Conquer (merge)
    return merge(left, right)


def merge(left, right):
    """Helper for merge_sort - O(n) to merge two sorted arrays"""
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


# ============================================================================
# O(n²) - QUADRATIC TIME
# ============================================================================

def bubble_sort(arr):
    """
    O(n²) - Simple but slow sorting
    Nested loops: outer loop n times, inner loop up to n times
    """
    arr = arr.copy()  # Don't modify original
    n = len(arr)

    for i in range(n):
        for j in range(0, n - i - 1):
            if arr[j] > arr[j + 1]:
                arr[j], arr[j + 1] = arr[j + 1], arr[j]

    return arr


def find_duplicates(arr):
    """
    O(n²) - Check all pairs
    For each element, compare with all others
    """
    duplicates = []
    for i in range(len(arr)):
        for j in range(i + 1, len(arr)):
            if arr[i] == arr[j] and arr[i] not in duplicates:
                duplicates.append(arr[i])
    return duplicates


def count_pairs_sum(arr, target):
    """
    O(n²) - Count pairs that sum to target
    Check all possible pairs
    """
    count = 0
    for i in range(len(arr)):
        for j in range(i + 1, len(arr)):
            if arr[i] + arr[j] == target:
                count += 1
    return count


# ============================================================================
# O(2^n) - EXPONENTIAL TIME
# ============================================================================

def fibonacci_slow(n):
    """
    O(2^n) - VERY SLOW! Exponential time
    Each call makes two recursive calls, creating exponential tree
    """
    if n <= 1:
        return n
    return fibonacci_slow(n - 1) + fibonacci_slow(n - 2)


def all_subsets(arr):
    """
    O(2^n) - Generate all possible subsets
    For n elements, there are 2^n subsets
    """
    if not arr:
        return [[]]

    first = arr[0]
    rest_subsets = all_subsets(arr[1:])

    # For each subset, create version with and without first element
    with_first = [[first] + subset for subset in rest_subsets]
    return rest_subsets + with_first


# ============================================================================
# IMPROVED VERSIONS (Better Complexity)
# ============================================================================

def fibonacci_fast(n, memo=None):
    """
    O(n) - Much faster with memoization!
    Cache results to avoid recomputation
    """
    if memo is None:
        memo = {}

    if n in memo:
        return memo[n]

    if n <= 1:
        return n

    memo[n] = fibonacci_fast(n - 1, memo) + fibonacci_fast(n - 2, memo)
    return memo[n]


def fibonacci_iterative(n):
    """
    O(n) time, O(1) space - Most efficient!
    No recursion, minimal memory
    """
    if n <= 1:
        return n

    a, b = 0, 1
    for _ in range(2, n + 1):
        a, b = b, a + b
    return b


def find_pair_sum_fast(arr, target):
    """
    O(n) - Find pair that sums to target using hash set
    Much faster than O(n²) brute force
    """
    seen = set()
    for num in arr:
        complement = target - num
        if complement in seen:
            return (complement, num)
        seen.add(num)
    return None


def remove_duplicates_fast(arr):
    """
    O(n) - Remove duplicates efficiently using set
    Better than O(n²) checking each element
    """
    seen = set()
    result = []
    for item in arr:
        if item not in seen:
            seen.add(item)
            result.append(item)
    return result


# ============================================================================
# DEMONSTRATION & BENCHMARKING
# ============================================================================

def demonstrate_complexity():
    """Show how different complexities perform with increasing input sizes"""

    print("=" * 60)
    print("ALGORITHM COMPLEXITY DEMONSTRATION")
    print("=" * 60)

    # O(1) - Constant time
    print("\n1. O(1) - CONSTANT TIME")
    print("-" * 40)
    for size in [100, 1000, 10000, 100000]:
        arr = list(range(size))
        _, elapsed = time_function(constant_time_access, arr, size // 2)
        print(f"n = {size:6d}: {elapsed:.4f} ms")
    print("→ Notice: Time stays constant regardless of size!")

    # O(log n) - Logarithmic time
    print("\n2. O(log n) - LOGARITHMIC TIME")
    print("-" * 40)
    for size in [100, 1000, 10000, 100000]:
        arr = list(range(size))
        _, elapsed = time_function(binary_search, arr, size - 1)
        print(f"n = {size:6d}: {elapsed:.4f} ms")
    print("→ Notice: Grows very slowly!")

    # O(n) - Linear time
    print("\n3. O(n) - LINEAR TIME")
    print("-" * 40)
    for size in [100, 1000, 10000, 100000]:
        arr = list(range(size))
        _, elapsed = time_function(sum_array, arr)
        print(f"n = {size:6d}: {elapsed:.4f} ms")
    print("→ Notice: 10x size ≈ 10x time")

    # O(n log n) - Log-linear time
    print("\n4. O(n log n) - LOG-LINEAR TIME")
    print("-" * 40)
    for size in [100, 1000, 10000]:
        arr = list(range(size, 0, -1))  # Reverse sorted
        _, elapsed = time_function(merge_sort, arr)
        print(f"n = {size:6d}: {elapsed:.4f} ms")
    print("→ Notice: Grows faster than O(n) but much slower than O(n²)")

    # O(n²) - Quadratic time
    print("\n5. O(n²) - QUADRATIC TIME")
    print("-" * 40)
    for size in [100, 200, 400, 800]:
        arr = list(range(size, 0, -1))
        _, elapsed = time_function(bubble_sort, arr)
        print(f"n = {size:6d}: {elapsed:.4f} ms")
    print("→ Notice: 2x size ≈ 4x time! Gets slow quickly.")

    # O(2^n) - Exponential time
    print("\n6. O(2^n) - EXPONENTIAL TIME")
    print("-" * 40)
    print("Warning: Only testing small values - exponential gets VERY slow!")
    for n in [10, 15, 20, 25]:
        _, elapsed = time_function(fibonacci_slow, n)
        print(f"n = {n:2d}: {elapsed:.4f} ms")
    print("→ Notice: Adding just 5 to n makes it ~32x slower!")

    # Compare Fibonacci implementations
    print("\n7. OPTIMIZATION COMPARISON: Fibonacci")
    print("-" * 40)
    n = 30
    print(f"Computing Fibonacci({n}):\n")

    _, slow_time = time_function(fibonacci_slow, n)
    print(f"Naive recursive O(2^n):  {slow_time:.2f} ms")

    _, fast_time = time_function(fibonacci_fast, n)
    print(f"Memoized O(n):           {fast_time:.2f} ms")

    _, iter_time = time_function(fibonacci_iterative, n)
    print(f"Iterative O(n):          {iter_time:.2f} ms")

    print(f"\nSpeedup: {slow_time/iter_time:.0f}x faster!")


def compare_search():
    """Compare linear vs binary search"""
    print("\n" + "=" * 60)
    print("LINEAR vs BINARY SEARCH")
    print("=" * 60)

    sizes = [1000, 10000, 100000, 1000000]

    print("\nSearching for last element (worst case):\n")
    print(f"{'Size':<10} {'Linear O(n)':<15} {'Binary O(log n)':<15} {'Speedup':<10}")
    print("-" * 60)

    for size in sizes:
        arr = list(range(size))
        target = size - 1

        # Linear search
        _, linear_time = time_function(linear_search, arr, target)

        # Binary search
        _, binary_time = time_function(binary_search, arr, target)

        speedup = linear_time / binary_time if binary_time > 0 else 0

        print(f"{size:<10} {linear_time:>10.4f} ms  {binary_time:>10.4f} ms  {speedup:>8.0f}x")


if __name__ == "__main__":
    # Run demonstrations
    demonstrate_complexity()
    compare_search()

    print("\n" + "=" * 60)
    print("Key Takeaways:")
    print("=" * 60)
    print("1. O(1) and O(log n) scale amazingly well")
    print("2. O(n) is usually acceptable")
    print("3. O(n log n) is good for sorting")
    print("4. O(n²) becomes problematic for large n")
    print("5. O(2^n) is only feasible for tiny inputs")
    print("6. Clever algorithms can dramatically improve performance!")
    print("=" * 60)
