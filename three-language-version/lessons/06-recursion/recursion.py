"""
Lesson 6: Recursion - Python Examples
Demonstrating recursive thinking in Python
"""

# =============================================================================
# BASIC RECURSION: Factorial
# =============================================================================

def factorial(n):
    """Calculate n! = n * (n-1) * ... * 1"""
    if n <= 1:        # Base case
        return 1
    else:             # Recursive case
        return n * factorial(n - 1)

print("=== Factorial ===")
for i in range(6):
    print(f"{i}! = {factorial(i)}")


# =============================================================================
# FIBONACCI SEQUENCE
# =============================================================================

def fibonacci(n):
    """Return nth Fibonacci number (naive recursive)"""
    if n <= 1:        # Base cases: F(0) = 0, F(1) = 1
        return n
    else:
        return fibonacci(n - 1) + fibonacci(n - 2)

# More efficient with memoization
from functools import lru_cache

@lru_cache(maxsize=None)
def fibonacci_memo(n):
    """Fibonacci with memoization - much faster!"""
    if n <= 1:
        return n
    return fibonacci_memo(n - 1) + fibonacci_memo(n - 2)

print("\n=== Fibonacci ===")
print("First 10 Fibonacci numbers:", [fibonacci_memo(i) for i in range(10)])


# =============================================================================
# LIST PROCESSING RECURSIVELY
# =============================================================================

def sum_list(lst):
    """Sum all elements in a list recursively"""
    if not lst:           # Base case: empty list
        return 0
    else:                 # Recursive case: first + sum of rest
        return lst[0] + sum_list(lst[1:])

def length(lst):
    """Calculate length of list recursively"""
    if not lst:
        return 0
    return 1 + length(lst[1:])

def contains(lst, target):
    """Check if target is in list (recursive linear search)"""
    if not lst:
        return False
    if lst[0] == target:
        return True
    return contains(lst[1:], target)

print("\n=== List Processing ===")
numbers = [1, 2, 3, 4, 5]
print(f"sum_list({numbers}) = {sum_list(numbers)}")
print(f"length({numbers}) = {length(numbers)}")
print(f"contains({numbers}, 3) = {contains(numbers, 3)}")
print(f"contains({numbers}, 9) = {contains(numbers, 9)}")


# =============================================================================
# RECURSIVE DATA TRAVERSAL
# =============================================================================

def flatten(nested):
    """Flatten a nested list structure"""
    result = []
    for item in nested:
        if isinstance(item, list):
            result.extend(flatten(item))  # Recursive call for sublists
        else:
            result.append(item)
    return result

print("\n=== Flatten Nested List ===")
nested = [1, [2, 3], [4, [5, 6]], 7]
print(f"flatten({nested}) = {flatten(nested)}")


# =============================================================================
# BINARY SEARCH (Recursive)
# =============================================================================

def binary_search(arr, target, low=0, high=None):
    """Find target in sorted array using binary search"""
    if high is None:
        high = len(arr) - 1

    if low > high:  # Base case: not found
        return -1

    mid = (low + high) // 2

    if arr[mid] == target:      # Found it!
        return mid
    elif arr[mid] < target:     # Search right half
        return binary_search(arr, target, mid + 1, high)
    else:                       # Search left half
        return binary_search(arr, target, low, mid - 1)

print("\n=== Binary Search ===")
sorted_arr = [1, 3, 5, 7, 9, 11, 13, 15]
print(f"Array: {sorted_arr}")
print(f"binary_search(arr, 7) = index {binary_search(sorted_arr, 7)}")
print(f"binary_search(arr, 10) = index {binary_search(sorted_arr, 10)}")


# =============================================================================
# TAIL RECURSION (Python doesn't optimize this, but good practice)
# =============================================================================

def factorial_tail(n, accumulator=1):
    """Tail-recursive factorial - accumulator pattern"""
    if n <= 1:
        return accumulator
    return factorial_tail(n - 1, n * accumulator)

print("\n=== Tail Recursion ===")
print(f"factorial_tail(5) = {factorial_tail(5)}")


# =============================================================================
# TREE RECURSION: Towers of Hanoi
# =============================================================================

def hanoi(n, source, target, auxiliary):
    """Solve Towers of Hanoi puzzle"""
    if n == 1:
        print(f"Move disk 1 from {source} to {target}")
        return

    hanoi(n - 1, source, auxiliary, target)  # Move n-1 disks to auxiliary
    print(f"Move disk {n} from {source} to {target}")
    hanoi(n - 1, auxiliary, target, source)  # Move n-1 disks to target

print("\n=== Towers of Hanoi (3 disks) ===")
hanoi(3, 'A', 'C', 'B')


# =============================================================================
# MUTUAL RECURSION
# =============================================================================

def is_even(n):
    """Check if n is even using mutual recursion"""
    if n == 0:
        return True
    return is_odd(n - 1)

def is_odd(n):
    """Check if n is odd using mutual recursion"""
    if n == 0:
        return False
    return is_even(n - 1)

print("\n=== Mutual Recursion ===")
for i in range(5):
    print(f"is_even({i}) = {is_even(i)}, is_odd({i}) = {is_odd(i)}")
