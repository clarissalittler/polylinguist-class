"""
Lesson 13: Sorting and Searching - Python Examples
Implementations and comparisons of fundamental algorithms
"""

from typing import List, Optional, TypeVar, Callable
import random
import time

T = TypeVar('T')

# =============================================================================
# SEARCHING ALGORITHMS
# =============================================================================

def linear_search(arr: List[T], target: T) -> int:
    """
    Linear Search - O(n)
    Works on any list, sorted or unsorted
    """
    for i, item in enumerate(arr):
        if item == target:
            return i
    return -1


def binary_search(arr: List[T], target: T) -> int:
    """
    Binary Search - O(log n)
    Requires sorted input!
    """
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


def binary_search_recursive(arr: List[T], target: T, left: int = None, right: int = None) -> int:
    """Binary Search - Recursive version"""
    if left is None:
        left = 0
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


def binary_search_first(arr: List[T], target: T) -> int:
    """Find first occurrence of target"""
    left, right = 0, len(arr) - 1
    result = -1

    while left <= right:
        mid = (left + right) // 2
        if arr[mid] == target:
            result = mid
            right = mid - 1  # Keep looking left
        elif arr[mid] < target:
            left = mid + 1
        else:
            right = mid - 1

    return result


print("=== Searching Algorithms ===")
numbers = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]
print(f"Array: {numbers}")
print(f"Linear search for 11: index {linear_search(numbers, 11)}")
print(f"Binary search for 11: index {binary_search(numbers, 11)}")
print(f"Binary search for 10: index {binary_search(numbers, 10)}")

duplicates = [1, 2, 2, 2, 3, 4, 5]
print(f"\nArray with duplicates: {duplicates}")
print(f"First occurrence of 2: index {binary_search_first(duplicates, 2)}")


# =============================================================================
# BASIC SORTING ALGORITHMS - O(n²)
# =============================================================================

def bubble_sort(arr: List[T]) -> List[T]:
    """
    Bubble Sort - O(n²)
    Simple but inefficient
    """
    result = arr.copy()
    n = len(result)

    for i in range(n):
        swapped = False
        for j in range(0, n - i - 1):
            if result[j] > result[j + 1]:
                result[j], result[j + 1] = result[j + 1], result[j]
                swapped = True
        # Optimization: stop if no swaps (already sorted)
        if not swapped:
            break

    return result


def selection_sort(arr: List[T]) -> List[T]:
    """
    Selection Sort - O(n²)
    Finds minimum and places it at the front
    """
    result = arr.copy()
    n = len(result)

    for i in range(n):
        min_idx = i
        for j in range(i + 1, n):
            if result[j] < result[min_idx]:
                min_idx = j
        result[i], result[min_idx] = result[min_idx], result[i]

    return result


def insertion_sort(arr: List[T]) -> List[T]:
    """
    Insertion Sort - O(n²)
    Good for small or nearly-sorted arrays
    """
    result = arr.copy()

    for i in range(1, len(result)):
        key = result[i]
        j = i - 1
        while j >= 0 and result[j] > key:
            result[j + 1] = result[j]
            j -= 1
        result[j + 1] = key

    return result


print("\n=== O(n²) Sorting Algorithms ===")
unsorted = [64, 34, 25, 12, 22, 11, 90]
print(f"Original: {unsorted}")
print(f"Bubble sort: {bubble_sort(unsorted)}")
print(f"Selection sort: {selection_sort(unsorted)}")
print(f"Insertion sort: {insertion_sort(unsorted)}")


# =============================================================================
# EFFICIENT SORTING ALGORITHMS - O(n log n)
# =============================================================================

def merge_sort(arr: List[T]) -> List[T]:
    """
    Merge Sort - O(n log n)
    Divide and conquer, stable sort
    """
    if len(arr) <= 1:
        return arr

    mid = len(arr) // 2
    left = merge_sort(arr[:mid])
    right = merge_sort(arr[mid:])

    return merge(left, right)


def merge(left: List[T], right: List[T]) -> List[T]:
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


def quick_sort(arr: List[T]) -> List[T]:
    """
    Quick Sort - O(n log n) average, O(n²) worst
    In-place version available, this is simpler
    """
    if len(arr) <= 1:
        return arr

    pivot = arr[len(arr) // 2]
    left = [x for x in arr if x < pivot]
    middle = [x for x in arr if x == pivot]
    right = [x for x in arr if x > pivot]

    return quick_sort(left) + middle + quick_sort(right)


def quick_sort_inplace(arr: List[T], low: int = 0, high: int = None) -> None:
    """Quick Sort - In-place version"""
    if high is None:
        high = len(arr) - 1

    if low < high:
        pivot_idx = partition(arr, low, high)
        quick_sort_inplace(arr, low, pivot_idx - 1)
        quick_sort_inplace(arr, pivot_idx + 1, high)


def partition(arr: List[T], low: int, high: int) -> int:
    """Partition for quick sort"""
    pivot = arr[high]
    i = low - 1

    for j in range(low, high):
        if arr[j] <= pivot:
            i += 1
            arr[i], arr[j] = arr[j], arr[i]

    arr[i + 1], arr[high] = arr[high], arr[i + 1]
    return i + 1


print("\n=== O(n log n) Sorting Algorithms ===")
unsorted = [64, 34, 25, 12, 22, 11, 90]
print(f"Original: {unsorted}")
print(f"Merge sort: {merge_sort(unsorted)}")
print(f"Quick sort: {quick_sort(unsorted)}")


# =============================================================================
# HEAP SORT
# =============================================================================

def heap_sort(arr: List[T]) -> List[T]:
    """
    Heap Sort - O(n log n)
    Uses binary heap structure
    """
    result = arr.copy()
    n = len(result)

    # Build max heap
    for i in range(n // 2 - 1, -1, -1):
        heapify(result, n, i)

    # Extract elements from heap
    for i in range(n - 1, 0, -1):
        result[i], result[0] = result[0], result[i]
        heapify(result, i, 0)

    return result


def heapify(arr: List[T], n: int, i: int) -> None:
    """Maintain heap property"""
    largest = i
    left = 2 * i + 1
    right = 2 * i + 2

    if left < n and arr[left] > arr[largest]:
        largest = left
    if right < n and arr[right] > arr[largest]:
        largest = right

    if largest != i:
        arr[i], arr[largest] = arr[largest], arr[i]
        heapify(arr, n, largest)


print(f"Heap sort: {heap_sort(unsorted)}")


# =============================================================================
# COUNTING SORT - O(n + k)
# =============================================================================

def counting_sort(arr: List[int]) -> List[int]:
    """
    Counting Sort - O(n + k)
    Where k is the range of input values
    Only works for non-negative integers!
    """
    if not arr:
        return []

    max_val = max(arr)
    count = [0] * (max_val + 1)

    # Count occurrences
    for num in arr:
        count[num] += 1

    # Build sorted array
    result = []
    for num, cnt in enumerate(count):
        result.extend([num] * cnt)

    return result


print("\n=== Special Sorting Algorithms ===")
integers = [4, 2, 2, 8, 3, 3, 1]
print(f"Counting sort {integers}: {counting_sort(integers)}")


# =============================================================================
# PERFORMANCE COMPARISON
# =============================================================================

def benchmark_sorts():
    """Compare sorting algorithm performance"""
    print("\n=== Performance Comparison ===")

    sizes = [100, 500, 1000]
    algorithms = [
        ("Bubble Sort", bubble_sort),
        ("Insertion Sort", insertion_sort),
        ("Merge Sort", merge_sort),
        ("Quick Sort", quick_sort),
        ("Python sorted()", sorted),
    ]

    for size in sizes:
        print(f"\nArray size: {size}")
        data = [random.randint(0, 10000) for _ in range(size)]

        for name, func in algorithms:
            test_data = data.copy()
            start = time.perf_counter()
            func(test_data)
            elapsed = time.perf_counter() - start
            print(f"  {name}: {elapsed:.6f}s")


# Only run benchmark if not too slow
print("\n=== Quick Benchmark (n=500) ===")
data = [random.randint(0, 10000) for _ in range(500)]

for name, func in [("Bubble", bubble_sort), ("Merge", merge_sort), ("Quick", quick_sort)]:
    test_data = data.copy()
    start = time.perf_counter()
    func(test_data)
    elapsed = time.perf_counter() - start
    print(f"{name} sort: {elapsed:.6f}s")


# =============================================================================
# STABILITY IN SORTING
# =============================================================================

print("\n=== Sorting Stability ===")

# Stable sort preserves relative order of equal elements
students = [
    ("Alice", 85),
    ("Bob", 90),
    ("Charlie", 85),
    ("Diana", 90),
]

# Sort by grade using stable sort
sorted_stable = sorted(students, key=lambda x: x[1])
print(f"Stable sort by grade: {sorted_stable}")
print("Notice: Alice comes before Charlie (both 85) - original order preserved")


# =============================================================================
# CHOOSING THE RIGHT ALGORITHM
# =============================================================================

print("\n=== When to Use Each Algorithm ===")
print("""
Linear Search:
  - Unsorted data
  - Small datasets
  - Single search

Binary Search:
  - SORTED data only
  - Repeated searches
  - Large datasets

Bubble/Selection Sort:
  - Educational purposes
  - Very small arrays (< 20 elements)
  - Nearly sorted (bubble with early termination)

Insertion Sort:
  - Small arrays
  - Nearly sorted data
  - Online sorting (data arrives piece by piece)

Merge Sort:
  - Large datasets
  - Stability required
  - Linked lists (no random access needed)
  - External sorting (files too big for memory)

Quick Sort:
  - General purpose
  - In-memory sorting
  - Average case performance matters

Heap Sort:
  - Guaranteed O(n log n)
  - Limited memory
  - No recursion desired

Counting/Radix Sort:
  - Integer data with limited range
  - Very large datasets
  - When comparison-based O(n log n) is too slow
""")
