# Lab 22: Sort Race

**Quarter 3, Week 2**
**Duration:** 90 minutes
**Format:** Pair programming with benchmarking

## Overview

Sorting is fundamental to computer science. This lab implements multiple sorting algorithms and races them against each other to understand algorithmic complexity in practice.

## Objectives

By the end of this lab, you will:
- [ ] Implement bubble, insertion, and selection sort
- [ ] Implement merge sort and quicksort
- [ ] Benchmark and compare performance
- [ ] Understand why O(n log n) beats O(n²)

## Setup

- Partner up
- Create folder: `lab22-sorting/`
- Files: `sorting.py`, `benchmark.py`

---

## Part 1: O(n²) Sorts (25 minutes)

### Activity 1.1: Bubble Sort

**The idea:** Repeatedly swap adjacent elements if they're in wrong order.

```python
def bubble_sort(arr):
    """
    Bubble sort - O(n²)
    Repeatedly bubble largest element to the end.
    """
    arr = arr.copy()  # Don't modify original
    n = len(arr)

    for i in range(n):
        # Track if any swaps happened
        swapped = False

        for j in range(0, n - i - 1):
            if arr[j] > arr[j + 1]:
                arr[j], arr[j + 1] = arr[j + 1], arr[j]
                swapped = True

        # If no swaps, already sorted
        if not swapped:
            break

    return arr

# Test
test = [64, 34, 25, 12, 22, 11, 90]
print(f"Original: {test}")
print(f"Sorted:   {bubble_sort(test)}")
```

**Trace through:**
```
[64, 34, 25, 12, 22, 11, 90]
Pass 1: [34, 25, 12, 22, 11, 64, 90]  (90 bubbles up)
Pass 2: [25, 12, 22, 11, 34, 64, 90]  (64 in place)
Pass 3: [12, 22, 11, 25, 34, 64, 90]  (34 in place)
...
```

### Activity 1.2: Selection Sort

**The idea:** Find minimum, put it first. Repeat for remaining elements.

```python
def selection_sort(arr):
    """
    Selection sort - O(n²)
    Select minimum and swap to front.
    """
    arr = arr.copy()
    n = len(arr)

    for i in range(n):
        # Find minimum in unsorted portion
        min_idx = i
        for j in range(i + 1, n):
            if arr[j] < arr[min_idx]:
                min_idx = j

        # Swap minimum to front of unsorted portion
        arr[i], arr[min_idx] = arr[min_idx], arr[i]

    return arr

# Test
print(f"Selection: {selection_sort(test)}")
```

### Activity 1.3: Insertion Sort

**The idea:** Build sorted array one element at a time by inserting each element in its correct position.

```python
def insertion_sort(arr):
    """
    Insertion sort - O(n²)
    Insert each element into sorted portion.
    """
    arr = arr.copy()

    for i in range(1, len(arr)):
        key = arr[i]
        j = i - 1

        # Shift elements greater than key
        while j >= 0 and arr[j] > key:
            arr[j + 1] = arr[j]
            j -= 1

        arr[j + 1] = key

    return arr

# Test
print(f"Insertion: {insertion_sort(test)}")
```

### Activity 1.4: Your Turn

Implement in Haskell:

```haskell
-- Insertion sort
insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)
  where
    insert y [] = [y]
    insert y (z:zs)
        | y <= z    = y : z : zs
        | otherwise = z : insert y zs

main :: IO ()
main = print (insertionSort [64, 34, 25, 12, 22, 11, 90])
```

### ✅ Checkpoint 1

Verify:
- [ ] All three O(n²) sorts work
- [ ] Can explain each algorithm's approach

---

## Part 2: O(n log n) Sorts (25 minutes)

### Activity 2.1: Merge Sort

**The idea:** Divide array in half, sort each half, merge sorted halves.

```python
def merge_sort(arr):
    """
    Merge sort - O(n log n)
    Divide and conquer with merging.
    """
    if len(arr) <= 1:
        return arr

    # Divide
    mid = len(arr) // 2
    left = merge_sort(arr[:mid])
    right = merge_sort(arr[mid:])

    # Merge
    return merge(left, right)

def merge(left, right):
    """Merge two sorted arrays."""
    result = []
    i = j = 0

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
print(f"Merge: {merge_sort(test)}")
```

**Visualization:**
```
[64, 34, 25, 12, 22, 11, 90]
         /              \
[64, 34, 25]      [12, 22, 11, 90]
    /     \           /        \
[64, 34] [25]    [12, 22]  [11, 90]
  /   \            /   \      /   \
[64] [34]       [12] [22]  [11] [90]
  \   /            \   /      \   /
[34, 64]        [12, 22]    [11, 90]
      \            /           \
  [25, 34, 64]  [12, 22]    [11, 90]
          \        \           /
      [25, 34, 64] [11, 12, 22, 90]
                \      /
        [11, 12, 22, 25, 34, 64, 90]
```

### Activity 2.2: Quicksort

**The idea:** Pick a pivot, partition around it, recursively sort partitions.

```python
def quicksort(arr):
    """
    Quicksort - O(n log n) average, O(n²) worst
    Partition and conquer.
    """
    if len(arr) <= 1:
        return arr

    # Choose pivot (middle element)
    pivot = arr[len(arr) // 2]

    # Partition
    left = [x for x in arr if x < pivot]
    middle = [x for x in arr if x == pivot]
    right = [x for x in arr if x > pivot]

    return quicksort(left) + middle + quicksort(right)

# Test
print(f"Quick: {quicksort(test)}")
```

### Activity 2.3: Haskell Implementations

```haskell
-- Merge sort
mergeSort :: Ord a => [a] -> [a]
mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs  = merge (mergeSort left) (mergeSort right)
  where
    (left, right) = splitAt (length xs `div` 2) xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

-- Quicksort (elegant!)
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
    quickSort [y | y <- xs, y < x]
    ++ [x]
    ++ quickSort [y | y <- xs, y >= x]

main :: IO ()
main = do
    let test = [64, 34, 25, 12, 22, 11, 90]
    print (mergeSort test)
    print (quickSort test)
```

### ✅ Checkpoint 2

Verify:
- [ ] Merge sort works
- [ ] Quicksort works
- [ ] Can explain divide-and-conquer

---

## Part 3: The Race! (25 minutes)

### Activity 3.1: Benchmarking Setup

```python
import time
import random

def benchmark(sort_func, arr, name):
    """Time a sorting function."""
    arr_copy = arr.copy()
    start = time.perf_counter()
    result = sort_func(arr_copy)
    end = time.perf_counter()
    elapsed = (end - start) * 1000  # Convert to ms

    # Verify it's sorted
    is_sorted = all(result[i] <= result[i+1] for i in range(len(result)-1))

    return {
        "name": name,
        "time_ms": elapsed,
        "sorted": is_sorted
    }

def generate_test_data(n, data_type="random"):
    """Generate test arrays of different types."""
    if data_type == "random":
        return [random.randint(0, n) for _ in range(n)]
    elif data_type == "sorted":
        return list(range(n))
    elif data_type == "reversed":
        return list(range(n, 0, -1))
    elif data_type == "nearly_sorted":
        arr = list(range(n))
        # Swap a few elements
        for _ in range(n // 10):
            i, j = random.randint(0, n-1), random.randint(0, n-1)
            arr[i], arr[j] = arr[j], arr[i]
        return arr
```

### Activity 3.2: Run the Race

```python
def run_race(sizes=[100, 500, 1000, 2000]):
    """Race all sorting algorithms."""
    sorts = [
        (bubble_sort, "Bubble Sort"),
        (selection_sort, "Selection Sort"),
        (insertion_sort, "Insertion Sort"),
        (merge_sort, "Merge Sort"),
        (quicksort, "Quicksort"),
        (sorted, "Python built-in"),  # For comparison
    ]

    for size in sizes:
        print(f"\n{'='*50}")
        print(f"Array size: {size}")
        print(f"{'='*50}")

        test_data = generate_test_data(size)

        results = []
        for sort_func, name in sorts:
            result = benchmark(sort_func, test_data, name)
            results.append(result)

        # Sort by time and display
        results.sort(key=lambda x: x["time_ms"])

        for i, r in enumerate(results, 1):
            status = "✓" if r["sorted"] else "✗"
            print(f"{i}. {r['name']:20} {r['time_ms']:8.2f} ms {status}")

# Run it!
run_race()
```

### Activity 3.3: Analyze Results

**Expected results (approximate):**

| Size | Bubble | Selection | Insertion | Merge | Quick | Built-in |
|------|--------|-----------|-----------|-------|-------|----------|
| 100  | ~1ms   | ~0.5ms    | ~0.3ms    | ~0.1ms| ~0.1ms| ~0.01ms  |
| 1000 | ~100ms | ~50ms     | ~30ms     | ~2ms  | ~1ms  | ~0.1ms   |
| 5000 | ~2.5s  | ~1.2s     | ~0.7s     | ~10ms | ~5ms  | ~0.5ms   |

**Discussion questions:**
1. Why is insertion sort often faster than bubble sort?
2. Why does quicksort sometimes perform poorly?
3. Why is Python's built-in so fast?

### Activity 3.4: Different Data Types

```python
def race_data_types(size=1000):
    """Compare performance on different data patterns."""
    data_types = ["random", "sorted", "reversed", "nearly_sorted"]

    sorts = [
        (insertion_sort, "Insertion"),
        (merge_sort, "Merge"),
        (quicksort, "Quick"),
    ]

    for dtype in data_types:
        print(f"\n{dtype.upper()} data (n={size}):")
        test_data = generate_test_data(size, dtype)

        for sort_func, name in sorts:
            result = benchmark(sort_func, test_data, name)
            print(f"  {name:12} {result['time_ms']:8.2f} ms")

race_data_types()
```

**Key insight:** Insertion sort is fastest on nearly-sorted data!

### ✅ Checkpoint 3

Verify:
- [ ] Benchmark runs successfully
- [ ] Can explain why O(n log n) beats O(n²)
- [ ] Understand best/worst case scenarios

---

## Part 4: Visualization (Optional, 10 minutes)

### Activity 4.1: ASCII Visualization

```python
def visualize_sort(sort_func, arr, name):
    """Show sorting progress."""
    print(f"\n{name}:")
    print_bars(arr, "Initial")

    # For visualization, we'll modify sorts to yield intermediate states
    # This is a simplified visualization
    steps = list(sort_with_steps(arr))
    for i, step in enumerate(steps[::max(1, len(steps)//5)]):  # Show ~5 steps
        print_bars(step, f"Step {i+1}")

    print_bars(sort_func(arr), "Final")

def print_bars(arr, label):
    """Print array as horizontal bars."""
    max_val = max(arr) if arr else 1
    scale = 40 / max_val

    print(f"\n{label}:")
    for val in arr[:10]:  # Show first 10
        bar = "█" * int(val * scale)
        print(f"{val:3} |{bar}")

# Simplified - track bubble sort steps
def bubble_sort_visual(arr):
    arr = arr.copy()
    n = len(arr)
    steps = [arr.copy()]

    for i in range(n):
        for j in range(0, n - i - 1):
            if arr[j] > arr[j + 1]:
                arr[j], arr[j + 1] = arr[j + 1], arr[j]
        steps.append(arr.copy())

    return steps

# Demo
small_test = [8, 4, 2, 9, 3, 6, 1, 7, 5]
steps = bubble_sort_visual(small_test)
for i, step in enumerate(steps):
    print(f"Pass {i}: {step}")
```

---

## Challenges

### Challenge 1: Counting Sort (O(n))

Implement counting sort for integers in a known range:

```python
def counting_sort(arr, max_val=None):
    """
    Counting sort - O(n + k) where k is range
    Only works for non-negative integers.
    """
    if not arr:
        return arr

    if max_val is None:
        max_val = max(arr)

    # Count occurrences
    count = [0] * (max_val + 1)
    for num in arr:
        count[num] += 1

    # Build sorted array
    result = []
    for num, cnt in enumerate(count):
        result.extend([num] * cnt)

    return result
```

### Challenge 2: Hybrid Sort

Implement a hybrid sort that uses insertion sort for small subarrays:

```python
def hybrid_sort(arr, threshold=10):
    """Use quicksort for large arrays, insertion for small."""
    if len(arr) <= threshold:
        return insertion_sort(arr)

    # Quicksort logic here
    pass
```

### Challenge 3: Stable vs Unstable

Demonstrate the difference between stable and unstable sorts:

```python
# Stable sort preserves relative order of equal elements
data = [("Alice", 3), ("Bob", 1), ("Charlie", 3), ("Diana", 2)]
# Sort by number - stable keeps Alice before Charlie
```

---

## Wrap-Up

**Key takeaways:**

1. **O(n²) sorts** (bubble, selection, insertion) are simple but slow
2. **O(n log n) sorts** (merge, quick) scale much better
3. **Best case varies** - insertion sort wins on nearly-sorted data
4. **Trade-offs exist** - merge sort uses extra space, quicksort can be O(n²)
5. **Use built-ins** - they're highly optimized (Timsort in Python)

**Complexity summary:**

| Sort | Best | Average | Worst | Space |
|------|------|---------|-------|-------|
| Bubble | O(n) | O(n²) | O(n²) | O(1) |
| Selection | O(n²) | O(n²) | O(n²) | O(1) |
| Insertion | O(n) | O(n²) | O(n²) | O(1) |
| Merge | O(n log n) | O(n log n) | O(n log n) | O(n) |
| Quick | O(n log n) | O(n log n) | O(n²) | O(log n) |

**Next lab:** Data Structure Implementation - build stacks and queues!
