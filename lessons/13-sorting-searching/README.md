# Lesson 13: Sorting and Searching Algorithms

## Learning Objectives

By the end of this lesson, you will be able to:

1. Implement linear and binary search algorithms
2. Understand and implement common sorting algorithms
3. Compare time and space complexity of different algorithms
4. Choose appropriate algorithms for different scenarios
5. Recognize when to use built-in sort functions vs custom implementations
6. Apply divide-and-conquer strategies
7. Understand stability in sorting algorithms

## Prerequisites

- Lesson 12: Algorithm Analysis & Complexity
- Understanding of Big-O notation
- Familiarity with arrays/lists and recursion

## Introduction

Sorting and searching are two of the most fundamental operations in computer science. Almost every program needs to:
- **Search** for data (find a contact, check if item exists, locate a record)
- **Sort** data (alphabetically, numerically, by priority, by date)

The algorithms you choose can make the difference between a program that's **instant** and one that's **unusably slow**.

---

## Part 1: Searching Algorithms

### Linear Search

**Concept:** Check every element until you find the target.

**Complexity:**
- Time: **O(n)** - may need to check every element
- Space: **O(1)** - no extra space needed

**Pros:** Simple, works on unsorted data
**Cons:** Slow for large datasets

#### Python Implementation

```python
def linear_search(arr, target):
    """
    Search for target in arr.
    Returns index if found, -1 if not found.
    """
    for i, item in enumerate(arr):
        if item == target:
            return i
    return -1

# Example
numbers = [64, 34, 25, 12, 22, 11, 90]
print(linear_search(numbers, 22))  # Output: 4
print(linear_search(numbers, 100)) # Output: -1
```

#### Java Implementation

```java
public static int linearSearch(int[] arr, int target) {
    for (int i = 0; i < arr.length; i++) {
        if (arr[i] == target) {
            return i;
        }
    }
    return -1;
}
```

#### Haskell Implementation

```haskell
linearSearch :: Eq a => [a] -> a -> Maybe Int
linearSearch lst target = search lst 0
  where
    search [] _ = Nothing
    search (x:xs) i
        | x == target = Just i
        | otherwise = search xs (i + 1)
```

**When to use:** Small datasets, unsorted data, or when you need to find all occurrences.

---

### Binary Search

**Concept:** In a **sorted** array, repeatedly divide the search space in half.

**Complexity:**
- Time: **O(log n)** - halves search space each iteration
- Space: **O(1)** iterative, **O(log n)** recursive (call stack)

**Requirements:** Array MUST be sorted first!

**Pros:** Very fast for large sorted datasets
**Cons:** Requires sorted data

#### How It Works

```
Searching for 22 in [11, 12, 22, 25, 34, 64, 90]

Step 1: Check middle (index 3, value 25)
        22 < 25, so search left half

Step 2: Check middle of left half (index 1, value 12)
        22 > 12, so search right of this

Step 3: Check middle (index 2, value 22)
        Found it!
```

#### Python Implementation (Iterative)

```python
def binary_search(arr, target):
    """
    Binary search on sorted array.
    Returns index if found, -1 if not found.
    """
    left = 0
    right = len(arr) - 1

    while left <= right:
        mid = (left + right) // 2

        if arr[mid] == target:
            return mid
        elif arr[mid] < target:
            left = mid + 1  # Search right half
        else:
            right = mid - 1  # Search left half

    return -1  # Not found

# Example (must be sorted!)
numbers = [11, 12, 22, 25, 34, 64, 90]
print(binary_search(numbers, 22))  # Output: 2
print(binary_search(numbers, 100)) # Output: -1
```

#### Python Implementation (Recursive)

```python
def binary_search_recursive(arr, target, left=0, right=None):
    """Recursive version of binary search"""
    if right is None:
        right = len(arr) - 1

    if left > right:
        return -1  # Base case: not found

    mid = (left + right) // 2

    if arr[mid] == target:
        return mid
    elif arr[mid] < target:
        return binary_search_recursive(arr, target, mid + 1, right)
    else:
        return binary_search_recursive(arr, target, left, mid - 1)
```

#### Java Implementation

```java
public static int binarySearch(int[] arr, int target) {
    int left = 0;
    int right = arr.length - 1;

    while (left <= right) {
        int mid = left + (right - left) / 2;  // Avoid overflow

        if (arr[mid] == target) {
            return mid;
        } else if (arr[mid] < target) {
            left = mid + 1;
        } else {
            right = mid - 1;
        }
    }

    return -1;
}
```

**When to use:** Large sorted datasets, dictionaries, phone books, databases with indexes.

---

## Part 2: Sorting Algorithms

### Comparison of Sorting Algorithms

| Algorithm | Best | Average | Worst | Space | Stable? |
|-----------|------|---------|-------|-------|---------|
| Bubble Sort | O(n) | O(n²) | O(n²) | O(1) | Yes |
| Selection Sort | O(n²) | O(n²) | O(n²) | O(1) | No |
| Insertion Sort | O(n) | O(n²) | O(n²) | O(1) | Yes |
| Merge Sort | O(n log n) | O(n log n) | O(n log n) | O(n) | Yes |
| Quick Sort | O(n log n) | O(n log n) | O(n²) | O(log n) | No |

**Stable** means equal elements maintain their original relative order.

---

### Bubble Sort

**Concept:** Repeatedly step through the list, compare adjacent elements, swap if in wrong order.

**Complexity:** O(n²)

**Visualization:**
```
Pass 1: [64, 34, 25, 12, 22] → [34, 25, 12, 22, 64]  (64 bubbles to end)
Pass 2: [34, 25, 12, 22, 64] → [25, 12, 22, 34, 64]  (34 bubbles to end)
Pass 3: [25, 12, 22, 34, 64] → [12, 22, 25, 34, 64]  (25 bubbles to end)
Pass 4: [12, 22, 25, 34, 64] → [12, 22, 25, 34, 64]  (already sorted)
```

#### Python Implementation

```python
def bubble_sort(arr):
    """
    Bubble sort: Repeatedly swap adjacent elements if out of order.
    Time: O(n²), Space: O(1)
    """
    n = len(arr)

    for i in range(n):
        # Flag to optimize: stop if no swaps made (already sorted)
        swapped = False

        # Last i elements are already in place
        for j in range(0, n - i - 1):
            if arr[j] > arr[j + 1]:
                arr[j], arr[j + 1] = arr[j + 1], arr[j]
                swapped = True

        if not swapped:
            break  # Array is sorted

    return arr

# Example
numbers = [64, 34, 25, 12, 22, 11, 90]
print(bubble_sort(numbers))  # [11, 12, 22, 25, 34, 64, 90]
```

**When to use:** Educational purposes, nearly sorted small arrays. **Not recommended for production.**

---

### Selection Sort

**Concept:** Find the minimum element, swap it with the first. Repeat for remaining elements.

**Complexity:** O(n²)

**Visualization:**
```
[64, 25, 12, 22, 11] → Find min (11), swap with first
[11, 25, 12, 22, 64] → Find min in rest (12), swap with second
[11, 12, 25, 22, 64] → Find min in rest (22), swap with third
[11, 12, 22, 25, 64] → Continue until sorted
```

#### Python Implementation

```python
def selection_sort(arr):
    """
    Selection sort: Repeatedly find minimum and move to front.
    Time: O(n²), Space: O(1)
    """
    n = len(arr)

    for i in range(n):
        # Find minimum element in remaining unsorted array
        min_idx = i
        for j in range(i + 1, n):
            if arr[j] < arr[min_idx]:
                min_idx = j

        # Swap minimum with first unsorted element
        arr[i], arr[min_idx] = arr[min_idx], arr[i]

    return arr
```

**When to use:** Small arrays where minimizing swaps matters. **Not recommended for production.**

---

### Insertion Sort

**Concept:** Build sorted array one element at a time, inserting each into its correct position.

**Complexity:** O(n²) worst case, **O(n) best case** (already sorted!)

**Visualization:**
```
Like sorting a hand of playing cards:
[34, 25, 12, 22, 11]

Step 1: [34] | 25, 12, 22, 11  (34 is sorted)
Step 2: [25, 34] | 12, 22, 11  (insert 25)
Step 3: [12, 25, 34] | 22, 11  (insert 12)
Step 4: [12, 22, 25, 34] | 11  (insert 22)
Step 5: [11, 12, 22, 25, 34]   (insert 11)
```

#### Python Implementation

```python
def insertion_sort(arr):
    """
    Insertion sort: Build sorted array by inserting each element in correct position.
    Time: O(n²) worst, O(n) best, Space: O(1)
    """
    for i in range(1, len(arr)):
        key = arr[i]
        j = i - 1

        # Move elements greater than key one position ahead
        while j >= 0 and arr[j] > key:
            arr[j + 1] = arr[j]
            j -= 1

        arr[j + 1] = key

    return arr
```

**When to use:** Small arrays, nearly sorted data, real-time sorting (online algorithm).

---

### Merge Sort

**Concept:** Divide array in half, recursively sort each half, then merge sorted halves.

**Complexity:** **O(n log n)** always!

**Strategy:** Divide and conquer

**Visualization:**
```
[38, 27, 43, 3, 9, 82, 10]

Divide:
[38, 27, 43, 3] | [9, 82, 10]
[38, 27] [43, 3] | [9, 82] [10]
[38] [27] [43] [3] | [9] [82] [10]

Conquer (merge):
[27, 38] [3, 43] | [9, 82] [10]
[3, 27, 38, 43] | [9, 10, 82]
[3, 9, 10, 27, 38, 43, 82]
```

#### Python Implementation

```python
def merge_sort(arr):
    """
    Merge sort: Divide and conquer sorting.
    Time: O(n log n), Space: O(n)
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
    """Merge two sorted arrays into one sorted array"""
    result = []
    i = j = 0

    # Merge while both arrays have elements
    while i < len(left) and j < len(right):
        if left[i] <= right[j]:
            result.append(left[i])
            i += 1
        else:
            result.append(right[j])
            j += 1

    # Append remaining elements
    result.extend(left[i:])
    result.extend(right[j:])

    return result
```

#### Java Implementation

```java
public static int[] mergeSort(int[] arr) {
    if (arr.length <= 1) {
        return arr;
    }

    int mid = arr.length / 2;
    int[] left = Arrays.copyOfRange(arr, 0, mid);
    int[] right = Arrays.copyOfRange(arr, mid, arr.length);

    left = mergeSort(left);
    right = mergeSort(right);

    return merge(left, right);
}

private static int[] merge(int[] left, int[] right) {
    int[] result = new int[left.length + right.length];
    int i = 0, j = 0, k = 0;

    while (i < left.length && j < right.length) {
        if (left[i] <= right[j]) {
            result[k++] = left[i++];
        } else {
            result[k++] = right[j++];
        }
    }

    while (i < left.length) {
        result[k++] = left[i++];
    }

    while (j < right.length) {
        result[k++] = right[j++];
    }

    return result;
}
```

**When to use:** Guaranteed O(n log n), stable sort needed, external sorting (large datasets).

---

### Quick Sort

**Concept:** Pick a pivot, partition array so elements < pivot are left, elements > pivot are right. Recursively sort partitions.

**Complexity:** **O(n log n) average**, O(n²) worst case (already sorted with bad pivot choice)

**Strategy:** Divide and conquer

**Visualization:**
```
[10, 80, 30, 90, 40, 50, 70]  Pivot = 50

Partition: [10, 30, 40] [50] [80, 90, 70]

Recursively sort:
[10, 30, 40] → [10] [30] [40]
[80, 90, 70] → [70] [80] [90]

Final: [10, 30, 40, 50, 70, 80, 90]
```

#### Python Implementation

```python
def quick_sort(arr):
    """
    Quick sort: Divide and conquer using partitioning.
    Time: O(n log n) average, O(n²) worst, Space: O(log n)
    """
    if len(arr) <= 1:
        return arr

    pivot = arr[len(arr) // 2]  # Choose middle as pivot

    # Partition into three parts
    left = [x for x in arr if x < pivot]
    middle = [x for x in arr if x == pivot]
    right = [x for x in arr if x > pivot]

    return quick_sort(left) + middle + quick_sort(right)


# In-place version (more efficient)
def quick_sort_inplace(arr, low=0, high=None):
    """In-place quick sort"""
    if high is None:
        high = len(arr) - 1

    if low < high:
        # Partition and get pivot index
        pi = partition(arr, low, high)

        # Recursively sort elements before and after partition
        quick_sort_inplace(arr, low, pi - 1)
        quick_sort_inplace(arr, pi + 1, high)

    return arr


def partition(arr, low, high):
    """Partition array and return pivot index"""
    pivot = arr[high]  # Choose last element as pivot
    i = low - 1  # Index of smaller element

    for j in range(low, high):
        if arr[j] <= pivot:
            i += 1
            arr[i], arr[j] = arr[j], arr[i]

    arr[i + 1], arr[high] = arr[high], arr[i + 1]
    return i + 1
```

**When to use:** General-purpose sorting, **often the fastest in practice** despite worst case.

---

## When to Use Which Algorithm?

### Decision Tree

```
Is the array small (< 50 elements)?
├─ YES: Insertion sort (simple, efficient for small n)
└─ NO: Continue...

    Is the array nearly sorted?
    ├─ YES: Insertion sort (O(n) best case!)
    └─ NO: Continue...

        Do you need stable sorting?
        ├─ YES: Merge sort (guaranteed O(n log n), stable)
        └─ NO: Quick sort (usually fastest O(n log n))

        Is memory limited?
        └─ YES: Quick sort or in-place heap sort (O(log n) or O(1) space)
```

### Practical Guidelines

| Scenario | Algorithm | Why |
|----------|-----------|-----|
| **Small array** (< 50) | Insertion sort | Simple, low overhead |
| **Nearly sorted** | Insertion sort | O(n) best case |
| **Need stable** | Merge sort | Stable O(n log n) |
| **General purpose** | Quick sort | Fast in practice |
| **Worst-case guarantee** | Merge sort or heap sort | Always O(n log n) |
| **Limited memory** | Quick sort (in-place) | O(log n) space |
| **External sorting** (huge files) | Merge sort | Works with chunks |
| **Built-in library** | Timsort (Python), Introsort (C++) | Hybrid, optimized |

---

## Built-In Sorting Functions

Most languages provide highly optimized built-in sorting. **Use them unless you have a specific reason not to!**

### Python

```python
# Use built-in sorted() or list.sort()
numbers = [64, 34, 25, 12, 22]

# sorted() returns new sorted list
sorted_numbers = sorted(numbers)

# list.sort() sorts in-place
numbers.sort()

# Custom key function
words = ["banana", "pie", "Washington", "book"]
sorted_words = sorted(words, key=str.lower)  # Case-insensitive

# Reverse order
sorted_numbers = sorted(numbers, reverse=True)
```

Python uses **Timsort**: hybrid of merge sort and insertion sort, optimized for real-world data.

### Java

```java
// Arrays.sort() for arrays
int[] numbers = {64, 34, 25, 12, 22};
Arrays.sort(numbers);  // Quicksort for primitives, timsort for objects

// Collections.sort() for lists
List<Integer> list = new ArrayList<>(Arrays.asList(64, 34, 25, 12, 22));
Collections.sort(list);

// Custom comparator
Collections.sort(list, Collections.reverseOrder());
```

### Haskell

```haskell
import Data.List (sort, sortBy)

-- sort uses O(n log n) merge sort
sorted = sort [64, 34, 25, 12, 22]

-- Custom comparison
import Data.Ord (comparing, Down(..))
sortedDesc = sortBy (comparing Down) [64, 34, 25, 12, 22]
```

---

## Key Takeaways

1. **Searching:**
   - Linear search: O(n), works on unsorted data
   - Binary search: O(log n), requires sorted data - **dramatically faster**

2. **Sorting:**
   - Simple sorts (bubble, selection, insertion): O(n²) - good for small/nearly sorted data
   - Efficient sorts (merge, quick): O(n log n) - use for larger datasets
   - Insertion sort: O(n) best case for nearly sorted data!

3. **Stability matters** when sorting objects with multiple fields

4. **Use built-in sorting** unless you have specific requirements

5. **Algorithm choice depends on:**
   - Input size
   - Is data nearly sorted?
   - Memory constraints
   - Need for stability
   - Worst-case guarantees

6. **Understanding algorithms helps you:**
   - Choose the right tool
   - Understand performance characteristics
   - Optimize when necessary

---

## Discussion Questions

1. Why is binary search so much faster than linear search? Can you explain it without math?

2. In what scenarios would you choose insertion sort over quick sort?

3. Why is quick sort often faster than merge sort in practice, despite both being O(n log n)?

4. What does "stable sorting" mean? Give an example where it matters.

5. Python's list.sort() is "stable". Why might this be important when sorting a list of student records by grade?

6. If you need to search a dataset millions of times, what would you do first? (Hint: think about preprocessing)

---

## Next Lesson Preview

In Lesson 14, we'll explore **data structures** like stacks, queues, and linked lists - the building blocks that make efficient algorithms possible!

Understanding sorting and searching gives you the foundation to tackle more complex algorithms and data structures.
