"""
Lesson 13: Sorting and Searching Algorithms - Comprehensive Demo

This file contains working implementations of all major sorting and searching
algorithms discussed in the lesson, with performance benchmarking.
"""

import time
import random


# =============================================================================
# SEARCHING ALGORITHMS
# =============================================================================

def linear_search(arr, target):
    """
    Linear Search: Check every element until found
    Time: O(n), Space: O(1)
    """
    for i, item in enumerate(arr):
        if item == target:
            return i
    return -1


def binary_search(arr, target):
    """
    Binary Search: Divide and conquer on sorted array
    Time: O(log n), Space: O(1)
    REQUIRES: arr must be sorted!
    """
    left = 0
    right = len(arr) - 1

    while left <= right:
        mid = (left + right) // 2

        if arr[mid] == target:
            return mid
        elif arr[mid] < target:
            left = mid + 1
        else:
            right = mid - 1

    return -1


def binary_search_recursive(arr, target, left=0, right=None):
    """Recursive version of binary search"""
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


# =============================================================================
# SORTING ALGORITHMS - O(n²)
# =============================================================================

def bubble_sort(arr):
    """
    Bubble Sort: Repeatedly swap adjacent elements if out of order
    Time: O(n²), Space: O(1)
    Stable: Yes
    """
    arr = arr.copy()
    n = len(arr)

    for i in range(n):
        swapped = False

        for j in range(0, n - i - 1):
            if arr[j] > arr[j + 1]:
                arr[j], arr[j + 1] = arr[j + 1], arr[j]
                swapped = True

        if not swapped:
            break  # Already sorted

    return arr


def selection_sort(arr):
    """
    Selection Sort: Repeatedly find minimum and move to front
    Time: O(n²), Space: O(1)
    Stable: No
    """
    arr = arr.copy()
    n = len(arr)

    for i in range(n):
        min_idx = i
        for j in range(i + 1, n):
            if arr[j] < arr[min_idx]:
                min_idx = j

        arr[i], arr[min_idx] = arr[min_idx], arr[i]

    return arr


def insertion_sort(arr):
    """
    Insertion Sort: Build sorted array by inserting each element in correct position
    Time: O(n²) worst, O(n) best, Space: O(1)
    Stable: Yes
    Best for: Nearly sorted data!
    """
    arr = arr.copy()

    for i in range(1, len(arr)):
        key = arr[i]
        j = i - 1

        while j >= 0 and arr[j] > key:
            arr[j + 1] = arr[j]
            j -= 1

        arr[j + 1] = key

    return arr


# =============================================================================
# SORTING ALGORITHMS - O(n log n)
# =============================================================================

def merge_sort(arr):
    """
    Merge Sort: Divide and conquer sorting
    Time: O(n log n) always, Space: O(n)
    Stable: Yes
    Best for: Guaranteed performance, external sorting
    """
    if len(arr) <= 1:
        return arr

    mid = len(arr) // 2
    left = merge_sort(arr[:mid])
    right = merge_sort(arr[mid:])

    return merge(left, right)


def merge(left, right):
    """Helper for merge_sort: merge two sorted arrays"""
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


def quick_sort(arr):
    """
    Quick Sort: Divide and conquer using partitioning
    Time: O(n log n) average, O(n²) worst, Space: O(log n)
    Stable: No
    Best for: General purpose - usually fastest in practice
    """
    if len(arr) <= 1:
        return arr

    pivot = arr[len(arr) // 2]

    left = [x for x in arr if x < pivot]
    middle = [x for x in arr if x == pivot]
    right = [x for x in arr if x > pivot]

    return quick_sort(left) + middle + quick_sort(right)


def quick_sort_inplace(arr, low=0, high=None):
    """In-place version of quick sort (more memory efficient)"""
    if high is None:
        arr = arr.copy()
        high = len(arr) - 1

    if low < high:
        pi = partition(arr, low, high)
        quick_sort_inplace(arr, low, pi - 1)
        quick_sort_inplace(arr, pi + 1, high)

    return arr


def partition(arr, low, high):
    """Partition helper for quick_sort_inplace"""
    pivot = arr[high]
    i = low - 1

    for j in range(low, high):
        if arr[j] <= pivot:
            i += 1
            arr[i], arr[j] = arr[j], arr[i]

    arr[i + 1], arr[high] = arr[high], arr[i + 1]
    return i + 1


# =============================================================================
# BENCHMARKING AND DEMONSTRATION
# =============================================================================

def benchmark_sorting(sizes=[100, 500, 1000, 2000]):
    """Compare all sorting algorithms"""
    algorithms = [
        ("Bubble Sort", bubble_sort),
        ("Selection Sort", selection_sort),
        ("Insertion Sort", insertion_sort),
        ("Merge Sort", merge_sort),
        ("Quick Sort", quick_sort),
        ("Built-in sorted()", sorted),
    ]

    print("=" * 70)
    print("SORTING ALGORITHM PERFORMANCE COMPARISON")
    print("=" * 70)

    for size in sizes:
        print(f"\nArray size: {size} (random order)")
        print("-" * 70)

        # Generate random array
        arr = [random.randint(1, 1000) for _ in range(size)]

        for name, algorithm in algorithms:
            test_arr = arr.copy()

            start = time.time()
            result = algorithm(test_arr)
            elapsed = (time.time() - start) * 1000

            # Verify correctness
            is_sorted = result == sorted(arr)
            status = "✓" if is_sorted else "✗"

            print(f"{name:25s}: {elapsed:8.2f} ms {status}")


def benchmark_search(size=100000):
    """Compare linear vs binary search"""
    print("\n" + "=" * 70)
    print("SEARCH ALGORITHM PERFORMANCE COMPARISON")
    print("=" * 70)

    arr = list(range(size))
    target = size - 1  # Worst case for linear search

    print(f"\nSearching for {target} in sorted array of {size} elements:")
    print("-" * 70)

    # Linear search
    start = time.time()
    result = linear_search(arr, target)
    linear_time = (time.time() - start) * 1000

    # Binary search
    start = time.time()
    result = binary_search(arr, target)
    binary_time = (time.time() - start) * 1000

    print(f"Linear Search:  {linear_time:8.4f} ms")
    print(f"Binary Search:  {binary_time:8.4f} ms")
    print(f"Speedup:        {linear_time/binary_time:.0f}x faster with binary search!")


def demonstrate_sorting():
    """Visualize how sorting algorithms work"""
    print("\n" + "=" * 70)
    print("SORTING ALGORITHM DEMONSTRATION")
    print("=" * 70)

    test_array = [64, 34, 25, 12, 22, 11, 90]
    print(f"\nOriginal array: {test_array}")
    print("-" * 70)

    algorithms = [
        ("Bubble Sort", bubble_sort),
        ("Selection Sort", selection_sort),
        ("Insertion Sort", insertion_sort),
        ("Merge Sort", merge_sort),
        ("Quick Sort", quick_sort),
    ]

    for name, algorithm in algorithms:
        result = algorithm(test_array)
        print(f"{name:20s}: {result}")


def demonstrate_search():
    """Demonstrate search algorithms"""
    print("\n" + "=" * 70)
    print("SEARCH ALGORITHM DEMONSTRATION")
    print("=" * 70)

    numbers = [64, 34, 25, 12, 22, 11, 90]
    target = 22

    print(f"\nSearching for {target} in {numbers}")
    print("-" * 70)

    # Linear search
    idx = linear_search(numbers, target)
    print(f"Linear Search: Found at index {idx}")

    # Binary search requires sorted array
    sorted_numbers = sorted(numbers)
    print(f"\nSorted array: {sorted_numbers}")
    idx = binary_search(sorted_numbers, target)
    print(f"Binary Search: Found at index {idx}")


def test_correctness():
    """Verify all sorting algorithms produce correct results"""
    print("\n" + "=" * 70)
    print("CORRECTNESS TESTING")
    print("=" * 70)

    test_cases = [
        [64, 34, 25, 12, 22, 11, 90],
        [5, 2, 8, 1, 9],
        [1],
        [],
        [3, 3, 3, 3],
        [5, 4, 3, 2, 1],
        list(range(100, 0, -1)),
    ]

    algorithms = [
        ("Bubble Sort", bubble_sort),
        ("Selection Sort", selection_sort),
        ("Insertion Sort", insertion_sort),
        ("Merge Sort", merge_sort),
        ("Quick Sort", quick_sort),
    ]

    print("\nTesting all algorithms with various inputs...")

    all_passed = True
    for name, algorithm in algorithms:
        passed = 0
        for test in test_cases:
            result = algorithm(test)
            expected = sorted(test)
            if result == expected:
                passed += 1
            else:
                print(f"✗ {name} failed on {test}")
                print(f"  Expected: {expected}")
                print(f"  Got:      {result}")
                all_passed = False

        status = "✓" if passed == len(test_cases) else "✗"
        print(f"{status} {name:20s}: {passed}/{len(test_cases)} tests passed")

    if all_passed:
        print("\n✓ All algorithms passed all tests!")
    else:
        print("\n✗ Some algorithms failed!")


# =============================================================================
# MAIN EXECUTION
# =============================================================================

if __name__ == "__main__":
    print("\n" + "=" * 70)
    print(" " * 15 + "SORTING AND SEARCHING ALGORITHMS")
    print(" " * 20 + "Comprehensive Demo")
    print("=" * 70)

    # Test correctness first
    test_correctness()

    # Demonstrate algorithms
    demonstrate_search()
    demonstrate_sorting()

    # Benchmark performance
    benchmark_search()
    benchmark_sorting(sizes=[100, 500, 1000])

    # Summary
    print("\n" + "=" * 70)
    print("KEY INSIGHTS")
    print("=" * 70)
    print("\n1. Binary search is DRAMATICALLY faster than linear search")
    print("   - But requires sorted data!")
    print("\n2. Simple sorts (bubble, selection, insertion) are O(n²)")
    print("   - OK for small arrays, terrible for large ones")
    print("\n3. Efficient sorts (merge, quick) are O(n log n)")
    print("   - Essential for large datasets")
    print("\n4. Insertion sort is fast for nearly-sorted data")
    print("   - O(n) best case!")
    print("\n5. Built-in sorting is highly optimized")
    print("   - Use it unless you have specific requirements!")
    print("\n6. Algorithm choice matters MORE than language choice")
    print("   - O(n log n) in Python beats O(n²) in C for large n")
    print("=" * 70)
