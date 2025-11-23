#!/usr/bin/env python3
"""
Lesson 6: Recursion in Python

Python supports recursion but has a default limit (~1000 calls).
Python does NOT optimize tail calls, so stack space is a consideration.
"""

# ====================
# 1. Simple Recursion
# ====================

def factorial(n):
    """Calculate n! recursively"""
    if n <= 1:  # Base case
        return 1
    return n * factorial(n - 1)  # Recursive case


def factorial_tail(n, accumulator=1):
    """Tail-recursive factorial (but Python doesn't optimize it)"""
    if n <= 1:
        return accumulator
    return factorial_tail(n - 1, n * accumulator)


# ====================
# 2. Fibonacci
# ====================

def fibonacci(n):
    """Calculate nth Fibonacci number (inefficient - exponential time)"""
    if n <= 1:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)


def fibonacci_memo(n, memo=None):
    """Fibonacci with memoization (efficient - linear time)"""
    if memo is None:
        memo = {}

    if n in memo:
        return memo[n]

    if n <= 1:
        return n

    memo[n] = fibonacci_memo(n - 1, memo) + fibonacci_memo(n - 2, memo)
    return memo[n]


# ====================
# 3. List Recursion
# ====================

def sum_list(lst):
    """Sum all elements in a list recursively"""
    if not lst:  # Empty list - base case
        return 0
    return lst[0] + sum_list(lst[1:])  # First element + sum of rest


def length(lst):
    """Calculate length of list recursively"""
    if not lst:
        return 0
    return 1 + length(lst[1:])


def reverse(lst):
    """Reverse a list recursively"""
    if not lst:
        return []
    return reverse(lst[1:]) + [lst[0]]


def max_element(lst):
    """Find maximum element recursively"""
    if len(lst) == 1:
        return lst[0]

    rest_max = max_element(lst[1:])
    return lst[0] if lst[0] > rest_max else rest_max


# ====================
# 4. String Recursion
# ====================

def reverse_string(s):
    """Reverse a string recursively"""
    if len(s) <= 1:
        return s
    return reverse_string(s[1:]) + s[0]


def is_palindrome(s):
    """Check if string is palindrome recursively"""
    # Remove spaces and convert to lowercase
    s = s.replace(" ", "").lower()

    if len(s) <= 1:
        return True

    if s[0] != s[-1]:
        return False

    return is_palindrome(s[1:-1])


# ====================
# 5. Binary Search
# ====================

def binary_search(arr, target, low=0, high=None):
    """Binary search recursively on sorted array"""
    if high is None:
        high = len(arr) - 1

    if low > high:
        return -1  # Not found

    mid = (low + high) // 2

    if arr[mid] == target:
        return mid
    elif arr[mid] > target:
        return binary_search(arr, target, low, mid - 1)
    else:
        return binary_search(arr, target, mid + 1, high)


# ====================
# 6. Quicksort
# ====================

def quicksort(arr):
    """Sort array using quicksort (divide and conquer)"""
    if len(arr) <= 1:
        return arr

    pivot = arr[len(arr) // 2]
    left = [x for x in arr if x < pivot]
    middle = [x for x in arr if x == pivot]
    right = [x for x in arr if x > pivot]

    return quicksort(left) + middle + quicksort(right)


# ====================
# 7. Tower of Hanoi
# ====================

def hanoi(n, source, target, auxiliary, moves=None):
    """Solve Tower of Hanoi puzzle"""
    if moves is None:
        moves = []

    if n == 1:
        moves.append(f"Move disk 1 from {source} to {target}")
        return moves

    # Move n-1 disks from source to auxiliary
    hanoi(n - 1, source, auxiliary, target, moves)

    # Move largest disk from source to target
    moves.append(f"Move disk {n} from {source} to {target}")

    # Move n-1 disks from auxiliary to target
    hanoi(n - 1, auxiliary, target, source, moves)

    return moves


# ====================
# 8. Tree Traversal
# ====================

class TreeNode:
    """Simple binary tree node"""
    def __init__(self, value, left=None, right=None):
        self.value = value
        self.left = left
        self.right = right


def tree_height(node):
    """Calculate height of tree recursively"""
    if node is None:
        return 0

    left_height = tree_height(node.left)
    right_height = tree_height(node.right)

    return 1 + max(left_height, right_height)


def tree_sum(node):
    """Sum all values in tree recursively"""
    if node is None:
        return 0

    return node.value + tree_sum(node.left) + tree_sum(node.right)


def inorder_traversal(node, result=None):
    """In-order traversal: left, root, right"""
    if result is None:
        result = []

    if node is None:
        return result

    inorder_traversal(node.left, result)
    result.append(node.value)
    inorder_traversal(node.right, result)

    return result


def preorder_traversal(node, result=None):
    """Pre-order traversal: root, left, right"""
    if result is None:
        result = []

    if node is None:
        return result

    result.append(node.value)
    preorder_traversal(node.left, result)
    preorder_traversal(node.right, result)

    return result


def postorder_traversal(node, result=None):
    """Post-order traversal: left, right, root"""
    if result is None:
        result = []

    if node is None:
        return result

    postorder_traversal(node.left, result)
    postorder_traversal(node.right, result)
    result.append(node.value)

    return result


# ====================
# 9. Mutual Recursion
# ====================

def is_even(n):
    """Check if number is even using mutual recursion"""
    if n == 0:
        return True
    return is_odd(n - 1)


def is_odd(n):
    """Check if number is odd using mutual recursion"""
    if n == 0:
        return False
    return is_even(n - 1)


# ====================
# 10. Greatest Common Divisor (Euclid's Algorithm)
# ====================

def gcd(a, b):
    """Calculate GCD using Euclid's algorithm"""
    if b == 0:
        return a
    return gcd(b, a % b)


# ====================
# Tests and Examples
# ====================

def main():
    print("=== Recursion Examples in Python ===\n")

    # Factorial
    print("1. Factorial:")
    print(f"   factorial(5) = {factorial(5)}")
    print(f"   factorial_tail(5) = {factorial_tail(5)}")

    # Fibonacci
    print("\n2. Fibonacci:")
    print(f"   fibonacci(10) = {fibonacci(10)}")
    print(f"   fibonacci_memo(30) = {fibonacci_memo(30)}")

    # List operations
    print("\n3. List Operations:")
    numbers = [1, 2, 3, 4, 5]
    print(f"   sum_list({numbers}) = {sum_list(numbers)}")
    print(f"   length({numbers}) = {length(numbers)}")
    print(f"   reverse({numbers}) = {reverse(numbers)}")
    print(f"   max_element({numbers}) = {max_element(numbers)}")

    # String operations
    print("\n4. String Operations:")
    print(f"   reverse_string('hello') = {reverse_string('hello')}")
    print(f"   is_palindrome('racecar') = {is_palindrome('racecar')}")
    print(f"   is_palindrome('A man a plan a canal Panama') = {is_palindrome('A man a plan a canal Panama')}")

    # Binary search
    print("\n5. Binary Search:")
    sorted_arr = [1, 3, 5, 7, 9, 11, 13, 15]
    print(f"   binary_search({sorted_arr}, 7) = {binary_search(sorted_arr, 7)}")
    print(f"   binary_search({sorted_arr}, 4) = {binary_search(sorted_arr, 4)}")

    # Quicksort
    print("\n6. Quicksort:")
    unsorted = [3, 6, 8, 10, 1, 2, 1]
    print(f"   quicksort({unsorted}) = {quicksort(unsorted)}")

    # Tower of Hanoi
    print("\n7. Tower of Hanoi (3 disks):")
    moves = hanoi(3, 'A', 'C', 'B')
    for move in moves:
        print(f"   {move}")

    # Tree operations
    print("\n8. Binary Tree:")
    #       5
    #      / \
    #     3   8
    #    / \   \
    #   1   4   9
    tree = TreeNode(5,
                    TreeNode(3,
                            TreeNode(1),
                            TreeNode(4)),
                    TreeNode(8,
                            None,
                            TreeNode(9)))

    print(f"   tree_height() = {tree_height(tree)}")
    print(f"   tree_sum() = {tree_sum(tree)}")
    print(f"   inorder_traversal() = {inorder_traversal(tree)}")
    print(f"   preorder_traversal() = {preorder_traversal(tree)}")
    print(f"   postorder_traversal() = {postorder_traversal(tree)}")

    # Mutual recursion
    print("\n9. Mutual Recursion:")
    print(f"   is_even(10) = {is_even(10)}")
    print(f"   is_odd(10) = {is_odd(10)}")
    print(f"   is_even(7) = {is_even(7)}")
    print(f"   is_odd(7) = {is_odd(7)}")

    # GCD
    print("\n10. Greatest Common Divisor:")
    print(f"   gcd(48, 18) = {gcd(48, 18)}")
    print(f"   gcd(100, 35) = {gcd(100, 35)}")


if __name__ == "__main__":
    main()
