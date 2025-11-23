# Lesson 13: Sorting & Searching - Exercises

## Exercise 1: Implementing Search Algorithms

### 1a. Linear Search Variants

Implement these variants of linear search:

**Task 1:** Find the last occurrence of a target (not the first)
```python
def linear_search_last(arr, target):
    # Your implementation here
    pass
```

**Task 2:** Find all occurrences of a target
```python
def linear_search_all(arr, target):
    # Should return list of indices
    pass
```

**Task 3:** Count how many times target appears
```python
def count_occurrences(arr, target):
    # Your implementation here
    pass
```

<details>
<summary>Solutions</summary>

```python
def linear_search_last(arr, target):
    """Find last occurrence - search from end"""
    for i in range(len(arr) - 1, -1, -1):
        if arr[i] == target:
            return i
    return -1

def linear_search_all(arr, target):
    """Find all occurrences"""
    indices = []
    for i, item in enumerate(arr):
        if item == target:
            indices.append(i)
    return indices

def count_occurrences(arr, target):
    """Count occurrences"""
    count = 0
    for item in arr:
        if item == target:
            count += 1
    return count
```
</details>

---

### 1b. Binary Search Variants

Implement these binary search variants:

**Task 1:** Find the first occurrence in an array with duplicates
```python
def binary_search_first(arr, target):
    # Find first occurrence when there might be duplicates
    pass
```

**Task 2:** Find the last occurrence
```python
def binary_search_last(arr, target):
    # Find last occurrence when there might be duplicates
    pass
```

<details>
<summary>Solutions</summary>

```python
def binary_search_first(arr, target):
    """Find first occurrence in sorted array with duplicates"""
    left, right = 0, len(arr) - 1
    result = -1

    while left <= right:
        mid = (left + right) // 2

        if arr[mid] == target:
            result = mid
            right = mid - 1  # Keep searching left for first occurrence
        elif arr[mid] < target:
            left = mid + 1
        else:
            right = mid - 1

    return result

def binary_search_last(arr, target):
    """Find last occurrence in sorted array with duplicates"""
    left, right = 0, len(arr) - 1
    result = -1

    while left <= right:
        mid = (left + right) // 2

        if arr[mid] == target:
            result = mid
            left = mid + 1  # Keep searching right for last occurrence
        elif arr[mid] < target:
            left = mid + 1
        else:
            right = mid - 1

    return result
```
</details>

---

## Exercise 2: Implementing Sorting Algorithms

Implement each sorting algorithm in at least one language:

### 2a. Bubble Sort

```python
def bubble_sort(arr):
    # Your implementation here
    # Remember to optimize with early stopping if no swaps!
    pass
```

Test with: `[64, 34, 25, 12, 22, 11, 90]`

<details>
<summary>Solution</summary>

```python
def bubble_sort(arr):
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
```
</details>

---

### 2b. Selection Sort

```python
def selection_sort(arr):
    # Your implementation here
    pass
```

<details>
<summary>Solution</summary>

```python
def selection_sort(arr):
    n = len(arr)
    for i in range(n):
        min_idx = i
        for j in range(i + 1, n):
            if arr[j] < arr[min_idx]:
                min_idx = j
        arr[i], arr[min_idx] = arr[min_idx], arr[i]
    return arr
```
</details>

---

### 2c. Insertion Sort

```python
def insertion_sort(arr):
    # Your implementation here
    pass
```

<details>
<summary>Solution</summary>

```python
def insertion_sort(arr):
    for i in range(1, len(arr)):
        key = arr[i]
        j = i - 1
        while j >= 0 and arr[j] > key:
            arr[j + 1] = arr[j]
            j -= 1
        arr[j + 1] = key
    return arr
```
</details>

---

### 2d. Merge Sort

```python
def merge_sort(arr):
    # Your implementation here
    pass

def merge(left, right):
    # Helper function
    pass
```

<details>
<summary>Solution</summary>

```python
def merge_sort(arr):
    if len(arr) <= 1:
        return arr

    mid = len(arr) // 2
    left = merge_sort(arr[:mid])
    right = merge_sort(arr[mid:])

    return merge(left, right)

def merge(left, right):
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
```
</details>

---

### 2e. Quick Sort

```python
def quick_sort(arr):
    # Your implementation here
    pass
```

<details>
<summary>Solution</summary>

```python
def quick_sort(arr):
    if len(arr) <= 1:
        return arr

    pivot = arr[len(arr) // 2]
    left = [x for x in arr if x < pivot]
    middle = [x for x in arr if x == pivot]
    right = [x for x in arr if x > pivot]

    return quick_sort(left) + middle + quick_sort(right)
```
</details>

---

## Exercise 3: Analyzing Sorting Algorithms

For each scenario, choose the best sorting algorithm and explain why:

### 3a. Scenarios

1. **Sorting 10 numbers** entered by user
   <details><summary>Answer</summary>Insertion sort - simple, efficient for small n, low overhead.</details>

2. **Sorting 1 million customer records** by last name
   <details><summary>Answer</summary>Merge sort or Quick sort (built-in) - need O(n log n). Use built-in sort in practice.</details>

3. **Sorting array that's already 90% sorted**
   <details><summary>Answer</summary>Insertion sort - O(n) best case for nearly sorted data!</details>

4. **Sorting with very limited memory**
   <details><summary>Answer</summary>Quick sort (in-place) or Heap sort - O(log n) or O(1) space.</details>

5. **Sorting student records by grade, must preserve original order for tied grades**
   <details><summary>Answer</summary>Merge sort - stable sorting required, guarantees O(n log n).</details>

6. **External sorting: 100GB file, only 2GB RAM**
   <details><summary>Answer</summary>External merge sort - divide into chunks, sort chunks, merge.</details>

---

## Exercise 4: Performance Comparison

Write a program that:
1. Generates random arrays of different sizes (100, 1000, 10000)
2. Times each sorting algorithm
3. Compares performance

```python
import time
import random

def benchmark_sorting():
    sizes = [100, 1000, 5000]
    algorithms = [
        ("Bubble Sort", bubble_sort),
        ("Insertion Sort", insertion_sort),
        ("Merge Sort", merge_sort),
        ("Quick Sort", quick_sort),
        ("Built-in", sorted)
    ]

    for size in sizes:
        print(f"\nArray size: {size}")
        arr = [random.randint(1, 1000) for _ in range(size)]

        for name, algorithm in algorithms:
            test_arr = arr.copy()
            start = time.time()
            algorithm(test_arr)
            elapsed = (time.time() - start) * 1000
            print(f"{name:15s}: {elapsed:8.2f} ms")

# Run it!
benchmark_sorting()
```

**Questions:**
1. Which algorithm is fastest for small arrays?
2. Which algorithm is fastest for large arrays?
3. How does performance change as array size increases?

---

## Exercise 5: Real-World Application

### Phone Book Search

You're building a phone book application with 10,000 contacts.

```python
contacts = [
    {"name": "Alice Smith", "phone": "555-0101"},
    {"name": "Bob Jones", "phone": "555-0102"},
    # ... 10,000 contacts
]
```

**Part A:** Implement linear search by name
```python
def find_contact_linear(contacts, name):
    # Your implementation
    pass
```

**Part B:** Sort contacts by name, then implement binary search
```python
def find_contact_binary(contacts, name):
    # First, sort contacts by name
    # Then implement binary search
    pass
```

**Part C:** Compare performance:
- How long does linear search take to find "Zachary Williams" (last in alphabetical order)?
- How long does binary search take?
- What's the speedup?

<details>
<summary>Solutions</summary>

```python
def find_contact_linear(contacts, name):
    """Linear search: O(n)"""
    for contact in contacts:
        if contact["name"] == name:
            return contact
    return None

def find_contact_binary(contacts, name):
    """Binary search: O(log n) - requires sorted contacts!"""
    # First, ensure sorted
    sorted_contacts = sorted(contacts, key=lambda c: c["name"])

    left, right = 0, len(sorted_contacts) - 1

    while left <= right:
        mid = (left + right) // 2
        mid_name = sorted_contacts[mid]["name"]

        if mid_name == name:
            return sorted_contacts[mid]
        elif mid_name < name:
            left = mid + 1
        else:
            right = mid - 1

    return None

# For 10,000 contacts:
# Linear search: up to 10,000 comparisons
# Binary search: about 14 comparisons
# Speedup: ~700x faster!
```
</details>

---

## Exercise 6: Stability in Sorting

### Understanding Stable Sorting

Given this array of students:
```python
students = [
    {"name": "Alice", "grade": 85},
    {"name": "Bob", "grade": 92},
    {"name": "Charlie", "grade": 85},
    {"name": "David", "grade": 78},
    {"name": "Eve", "grade": 92},
]
```

**Task:** Sort by grade. With stable sorting, students with the same grade should maintain their original order.

**Expected stable result:**
```python
[
    {"name": "David", "grade": 78},
    {"name": "Alice", "grade": 85},   # Alice before Charlie
    {"name": "Charlie", "grade": 85},
    {"name": "Bob", "grade": 92},     # Bob before Eve
    {"name": "Eve", "grade": 92},
]
```

**Questions:**
1. Which sorting algorithms are stable?
2. Why does stability matter here?
3. Implement stable sorting using merge sort

<details>
<summary>Answers</summary>

**1. Stable algorithms:** Merge sort, Insertion sort, Bubble sort
**Unstable:** Quick sort, Selection sort, Heap sort

**2. Why it matters:** When you sort by one field but want to preserve order from a previous sort. Example: Sort by grade, then within each grade, maintain alphabetical order.

**3. Implementation:**
```python
students.sort(key=lambda s: s["grade"])  # Python's sort is stable (Timsort)
```
</details>

---

## Exercise 7: Challenge Problems

### 7a. K-th Largest Element

Find the k-th largest element without fully sorting the array.

```python
def kth_largest(arr, k):
    """
    Find k-th largest element (1 = largest, 2 = second largest, etc.)
    Can you do better than O(n log n)?
    """
    pass
```

<details>
<summary>Hint</summary>
Use a modified quicksort partition! Only recurse into the partition containing the k-th element.
This is called "Quickselect" - O(n) average case!
</details>

---

### 7b. Merge K Sorted Arrays

Given k sorted arrays, merge them into one sorted array.

```python
def merge_k_sorted(arrays):
    """
    Example: [[1,4,7], [2,5,8], [3,6,9]] → [1,2,3,4,5,6,7,8,9]
    """
    pass
```

<details>
<summary>Hint</summary>
Use a min-heap! Keep track of the smallest element from each array.
Time: O(n log k) where n = total elements, k = number of arrays
</details>

---

### 7c. Dutch National Flag

Sort an array containing only 0s, 1s, and 2s in a single pass.

```python
def dutch_flag_sort(arr):
    """
    Example: [2,0,1,2,1,0] → [0,0,1,1,2,2]
    Constraint: Do it in O(n) time, O(1) space, single pass!
    """
    pass
```

<details>
<summary>Solution</summary>

```python
def dutch_flag_sort(arr):
    """Three-way partitioning"""
    low, mid, high = 0, 0, len(arr) - 1

    while mid <= high:
        if arr[mid] == 0:
            arr[low], arr[mid] = arr[mid], arr[low]
            low += 1
            mid += 1
        elif arr[mid] == 1:
            mid += 1
        else:  # arr[mid] == 2
            arr[mid], arr[high] = arr[high], arr[mid]
            high -= 1

    return arr
```
</details>

---

## Exercise 8: Reflection

Answer these conceptually:

1. **Why is binary search O(log n)?** Explain without using math formulas.

2. **Insertion sort vs Quick sort:** When would insertion sort be faster in practice?

3. **Memory-speed tradeoff:** Merge sort uses O(n) extra space. Why might you choose quick sort instead?

4. **Real-world sorting:** Python's `sorted()` uses Timsort (hybrid of merge sort and insertion sort). Why use a hybrid instead of pure merge sort?

5. **Searching first:** If you need to search data many times, what would you do before searching? Why?

---

## Summary Checklist

After completing these exercises, you should be able to:
- ✅ Implement linear and binary search
- ✅ Implement bubble, selection, insertion, merge, and quick sort
- ✅ Analyze time and space complexity of each algorithm
- ✅ Choose appropriate algorithms based on requirements
- ✅ Understand stable vs unstable sorting
- ✅ Optimize search with preprocessing (sorting)
- ✅ Recognize when to use built-in functions vs custom implementations

**Next:** Apply these concepts to data structures in Lesson 14!
