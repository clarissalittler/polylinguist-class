# Trees and Tree Algorithms: Practice Exercises

These exercises progress from basic tree operations to advanced algorithms.

---

## Part 1: Basic Tree Operations

### Exercise 1.1: Build a Binary Tree

**Task:** Manually construct this binary tree:

```
       1
      / \
     2   3
    / \
   4   5
```

Write code in Python and Java to create this tree structure.

---

### Exercise 1.2: Tree Properties

**Given this tree:**
```
       10
      /  \
     5   15
    / \    \
   3   7   20
```

**Calculate:**
1. Height of the tree
2. Depth of node 7
3. Number of total nodes
4. Number of leaf nodes
5. Number of internal nodes

---

### Exercise 1.3: Implement Tree Node

**Task:** Implement a TreeNode class in your preferred language with:
- Constructor
- Method to check if node is a leaf
- Method to check if node has both children
- Method to get the sibling of a node (if it has one)

---

## Part 2: Tree Traversal

### Exercise 2.1: Manual Traversal

**Given this tree:**
```
       1
      / \
     2   3
    / \   \
   4   5   6
```

**Write the output for each traversal:**
1. Inorder
2. Preorder
3. Postorder
4. Level-order

---

### Exercise 2.2: Implement All Traversals

**Task:** Implement these four traversal methods:

```python
def inorder_traversal(root):
    # TODO: Implement
    pass

def preorder_traversal(root):
    # TODO: Implement
    pass

def postorder_traversal(root):
    # TODO: Implement
    pass

def level_order_traversal(root):
    # TODO: Implement
    pass
```

Test with the tree from Exercise 2.1.

---

### Exercise 2.3: Iterative Traversals

**Task:** Implement inorder traversal **iteratively** using a stack (no recursion).

**Hint:**
```python
def inorder_iterative(root):
    stack = []
    current = root
    result = []

    while current or stack:
        # TODO: Implement
        pass

    return result
```

---

### Exercise 2.4: Level-Order by Levels

**Task:** Modify level-order traversal to return nodes grouped by level:

```python
def level_order_by_level(root):
    """
    Returns: [[1], [2, 3], [4, 5, 6]]
    for tree:  1
              / \
             2   3
            / \   \
           4   5   6
    """
    # TODO: Implement
    pass
```

---

## Part 3: Binary Search Trees

### Exercise 3.1: BST Validation

**Which of these are valid BSTs?**

**Tree A:**
```
       5
      / \
     3   7
    / \
   2   4
```

**Tree B:**
```
       5
      / \
     3   7
    / \
   1   6
```

**Tree C:**
```
       10
      /  \
     5   15
    / \  / \
   3  7 12 20
```

---

### Exercise 3.2: Implement BST Class

**Task:** Implement a complete BST class with these operations:

```python
class BST:
    def __init__(self):
        self.root = None

    def insert(self, value):
        """Insert a value into the BST."""
        # TODO: Implement
        pass

    def search(self, value):
        """Return True if value exists in BST."""
        # TODO: Implement
        pass

    def find_min(self):
        """Return the minimum value in BST."""
        # TODO: Implement
        pass

    def find_max(self):
        """Return the maximum value in BST."""
        # TODO: Implement
        pass

    def inorder(self):
        """Return inorder traversal (should be sorted!)."""
        # TODO: Implement
        pass

    def delete(self, value):
        """Delete a value from BST."""
        # TODO: Implement
        pass
```

**Test your implementation:**
```python
bst = BST()
for value in [5, 3, 7, 2, 4, 6, 8]:
    bst.insert(value)

print(bst.inorder())        # [2, 3, 4, 5, 6, 7, 8]
print(bst.search(4))        # True
print(bst.search(10))       # False
print(bst.find_min())       # 2
print(bst.find_max())       # 8

bst.delete(3)
print(bst.inorder())        # [2, 4, 5, 6, 7, 8]
```

---

### Exercise 3.3: BST from Sorted Array

**Task:** Given a sorted array, create a **balanced** BST.

```python
def sorted_array_to_bst(arr):
    """
    Convert sorted array to balanced BST.

    Example: [1, 2, 3, 4, 5, 6, 7]
    Should create:
           4
          / \
         2   6
        / \ / \
       1  3 5  7
    """
    # TODO: Implement
    pass
```

**Hint:** Use the middle element as the root to keep it balanced.

---

### Exercise 3.4: Kth Smallest Element

**Task:** Find the kth smallest element in a BST.

```python
def kth_smallest(root, k):
    """
    Find kth smallest element (1-indexed).

    Example: For BST [1, 2, 3, 4, 5]
    kth_smallest(root, 1) → 1
    kth_smallest(root, 3) → 3
    kth_smallest(root, 5) → 5
    """
    # TODO: Implement
    pass
```

**Hint:** Inorder traversal visits nodes in sorted order.

---

### Exercise 3.5: Range Sum in BST

**Task:** Find the sum of all node values in a given range [low, high].

```python
def range_sum_bst(root, low, high):
    """
    Sum all values where low <= value <= high.

    Example: BST [10, 5, 15, 3, 7, 13, 18]
             Range [7, 15]
             Sum = 7 + 10 + 13 + 15 = 45
    """
    # TODO: Implement
    pass
```

---

## Part 4: Binary Tree Algorithms

### Exercise 4.1: Maximum Depth

**Task:** Find the maximum depth (height) of a binary tree.

```python
def max_depth(root):
    """
    Return the maximum depth of the tree.

    Example:    1
               / \
              2   3
             /
            4
    Returns: 3
    """
    # TODO: Implement
    pass
```

---

### Exercise 4.2: Symmetric Tree

**Task:** Check if a binary tree is a mirror of itself (symmetric).

```python
def is_symmetric(root):
    """
    Check if tree is symmetric.

    Example (symmetric):
           1
          / \
         2   2
        / \ / \
       3  4 4  3

    Example (not symmetric):
           1
          / \
         2   2
          \   \
          3    3
    """
    # TODO: Implement
    pass
```

---

### Exercise 4.3: Invert Binary Tree

**Task:** Invert a binary tree (swap left and right children at every node).

```python
def invert_tree(root):
    """
    Invert the tree.

    Before:     4               After:      4
               / \                         / \
              2   7                       7   2
             / \ / \                     / \ / \
            1  3 6  9                   9  6 3  1
    """
    # TODO: Implement
    pass
```

---

### Exercise 4.4: Path Sum

**Task:** Check if there's a root-to-leaf path with a given sum.

```python
def has_path_sum(root, target_sum):
    """
    Return True if there exists a root-to-leaf path
    where the sum equals target_sum.

    Example:      5
                 / \
                4   8
               /   / \
              11  13  4
             /  \      \
            7    2      1

    has_path_sum(root, 22) → True (5→4→11→2)
    has_path_sum(root, 10) → False
    """
    # TODO: Implement
    pass
```

---

### Exercise 4.5: All Root-to-Leaf Paths

**Task:** Find all root-to-leaf paths.

```python
def binary_tree_paths(root):
    """
    Return all root-to-leaf paths.

    Example:   1
              / \
             2   3
              \
               5

    Returns: ["1->2->5", "1->3"]
    """
    # TODO: Implement
    pass
```

---

### Exercise 4.6: Lowest Common Ancestor

**Task:** Find the lowest common ancestor of two nodes in a binary tree.

```python
def lowest_common_ancestor(root, p, q):
    """
    Find LCA of nodes p and q.

    Example:       3
                  / \
                 5   1
                / \ / \
               6  2 0  8
                 / \
                7   4

    LCA(5, 1) = 3
    LCA(5, 4) = 5
    LCA(6, 2) = 5
    """
    # TODO: Implement
    pass
```

---

### Exercise 4.7: Diameter of Binary Tree

**Task:** Find the diameter (longest path between any two nodes).

```python
def diameter_of_binary_tree(root):
    """
    Find the longest path between any two nodes.
    The path may or may not pass through root.

    Example:     1
                / \
               2   3
              / \
             4   5

    Diameter = 3 (path: 4→2→1→3 or 5→2→1→3)
    """
    # TODO: Implement
    pass
```

---

### Exercise 4.8: Serialize and Deserialize

**Task:** Serialize a binary tree to a string and deserialize it back.

```python
def serialize(root):
    """
    Convert tree to string representation.

    Example:  1
             / \
            2   3
               / \
              4   5

    Returns: "1,2,null,null,3,4,null,null,5,null,null"
    """
    # TODO: Implement
    pass

def deserialize(data):
    """Convert string back to tree."""
    # TODO: Implement
    pass
```

---

## Part 5: Balanced Trees

### Exercise 5.1: Check if Balanced

**Task:** Determine if a binary tree is height-balanced (height difference ≤ 1 for all nodes).

```python
def is_balanced(root):
    """
    Check if tree is balanced.

    Balanced:       3           Not balanced:   1
                   / \                           \
                  9  20                           2
                    /  \                           \
                   15   7                           3

    """
    # TODO: Implement
    pass
```

---

### Exercise 5.2: Balance a BST

**Task:** Given a BST that may be unbalanced, balance it.

```python
def balance_bst(root):
    """
    Balance an unbalanced BST.

    Before:  1               After:    2
              \                       / \
               2                     1   3
                \                         \
                 3                         4
                  \
                   4
    """
    # TODO: Implement (Hint: inorder traversal + build balanced tree)
    pass
```

---

## Part 6: Heaps

### Exercise 6.1: Implement Max Heap

**Task:** Implement a max heap with these operations:

```python
class MaxHeap:
    def __init__(self):
        self.heap = []

    def insert(self, value):
        """Insert value and maintain heap property."""
        # TODO: Implement
        pass

    def extract_max(self):
        """Remove and return maximum value."""
        # TODO: Implement
        pass

    def peek(self):
        """Return maximum value without removing."""
        # TODO: Implement
        pass

    def size(self):
        """Return number of elements."""
        # TODO: Implement
        pass
```

**Test:**
```python
heap = MaxHeap()
for val in [3, 1, 6, 5, 2, 4]:
    heap.insert(val)

print(heap.peek())          # 6
print(heap.extract_max())   # 6
print(heap.extract_max())   # 5
print(heap.extract_max())   # 4
```

---

### Exercise 6.2: Heap Sort

**Task:** Implement heap sort using a heap.

```python
def heap_sort(arr):
    """
    Sort array using heap sort.
    Time: O(n log n), Space: O(n)

    Example: [3, 1, 6, 5, 2, 4] → [1, 2, 3, 4, 5, 6]
    """
    # TODO: Implement
    pass
```

---

### Exercise 6.3: Top K Frequent Elements

**Task:** Find the k most frequent elements in an array.

```python
def top_k_frequent(nums, k):
    """
    Find k most frequent elements.

    Example: nums = [1,1,1,2,2,3], k = 2
    Returns: [1, 2]

    Example: nums = [1], k = 1
    Returns: [1]
    """
    # TODO: Implement using heap
    pass
```

---

### Exercise 6.4: Kth Largest Element

**Task:** Find the kth largest element in an unsorted array.

```python
def find_kth_largest(nums, k):
    """
    Find kth largest element.

    Example: nums = [3,2,1,5,6,4], k = 2
    Returns: 5

    Example: nums = [3,2,3,1,2,4,5,5,6], k = 4
    Returns: 4
    """
    # TODO: Implement using min heap of size k
    pass
```

---

### Exercise 6.5: Merge K Sorted Lists

**Task:** Merge k sorted linked lists into one sorted list using a heap.

```python
class ListNode:
    def __init__(self, val=0, next=None):
        self.val = val
        self.next = next

def merge_k_lists(lists):
    """
    Merge k sorted linked lists.

    Example:
    Input: [[1,4,5], [1,3,4], [2,6]]
    Output: [1,1,2,3,4,4,5,6]
    """
    # TODO: Implement using min heap
    pass
```

---

## Part 7: Advanced Tree Problems

### Exercise 7.1: Binary Tree Maximum Path Sum

**Task:** Find the maximum path sum where a path can start and end at any node.

```python
def max_path_sum(root):
    """
    Find maximum path sum in binary tree.

    Example:      -10
                  / \
                 9  20
                   /  \
                  15   7

    Maximum path: 15→20→7 = 42
    """
    # TODO: Implement
    pass
```

---

### Exercise 7.2: Construct Tree from Traversals

**Task:** Construct a binary tree from inorder and preorder traversals.

```python
def build_tree(preorder, inorder):
    """
    Build tree from preorder and inorder traversals.

    Example:
    preorder = [3, 9, 20, 15, 7]
    inorder = [9, 3, 15, 20, 7]

    Returns tree:  3
                  / \
                 9  20
                   /  \
                  15   7
    """
    # TODO: Implement
    pass
```

---

### Exercise 7.3: Vertical Order Traversal

**Task:** Return the vertical order traversal of a binary tree.

```python
def vertical_order(root):
    """
    Return nodes in vertical order from left to right.

    Example:      3
                 / \
                9  20
                  /  \
                 15   7

    Returns: [[9], [3, 15], [20], [7]]
    """
    # TODO: Implement (Hint: use BFS with column tracking)
    pass
```

---

### Exercise 7.4: Binary Tree Right Side View

**Task:** Return the values you would see if you looked at the tree from the right side.

```python
def right_side_view(root):
    """
    Return right side view of tree.

    Example:      1
                 / \
                2   3
                 \   \
                  5   4

    Returns: [1, 3, 4]
    (You can see 1, then 3 on second level, then 4)
    """
    # TODO: Implement
    pass
```

---

### Exercise 7.5: Count Complete Tree Nodes

**Task:** Count nodes in a complete binary tree efficiently (better than O(n)).

```python
def count_nodes(root):
    """
    Count nodes in complete binary tree.
    Try to achieve better than O(n) complexity.

    Complete tree:    1
                     / \
                    2   3
                   / \  /
                  4  5 6

    Returns: 6
    """
    # TODO: Implement using tree height properties
    pass
```

---

## Part 8: Real-World Applications

### Exercise 8.1: File System

**Task:** Implement a simple file system using a tree structure.

```python
class FileNode:
    def __init__(self, name, is_dir=False):
        self.name = name
        self.is_dir = is_dir
        self.children = [] if is_dir else None
        self.size = 0

class FileSystem:
    def __init__(self):
        self.root = FileNode("/", is_dir=True)

    def create_directory(self, path):
        """Create a new directory at path."""
        # TODO: Implement
        pass

    def create_file(self, path, size):
        """Create a file at path with given size."""
        # TODO: Implement
        pass

    def get_size(self, path):
        """Get total size of directory (including subdirectories)."""
        # TODO: Implement
        pass

    def list_directory(self, path):
        """List all files and directories at path."""
        # TODO: Implement
        pass
```

---

### Exercise 8.2: Expression Evaluator

**Task:** Build and evaluate arithmetic expression trees.

```python
class ExprNode:
    def __init__(self, value):
        self.value = value
        self.left = None
        self.right = None

def build_expression_tree(postfix):
    """
    Build expression tree from postfix notation.

    Example: "3 4 + 2 *" → 3 4 + 2 *
    Creates tree:    *
                    / \
                   +   2
                  / \
                 3   4
    """
    # TODO: Implement
    pass

def evaluate_expression_tree(root):
    """Evaluate the expression tree."""
    # TODO: Implement
    pass

def tree_to_infix(root):
    """Convert tree to infix notation with parentheses."""
    # TODO: Implement (should return "(3 + 4) * 2")
    pass
```

---

### Exercise 8.3: Decision Tree

**Task:** Implement a simple decision tree for classification.

```python
class Question:
    def __init__(self, feature, value):
        self.feature = feature
        self.value = value

    def match(self, data):
        """Check if data matches this question."""
        return data[self.feature] >= self.value

class DecisionNode:
    def __init__(self, question, true_branch, false_branch):
        self.question = question
        self.true_branch = true_branch
        self.false_branch = false_branch

class Leaf:
    def __init__(self, prediction):
        self.prediction = prediction

def classify(node, data):
    """Classify data using the decision tree."""
    # TODO: Implement
    pass

# Example: Classify if a fruit is an apple
# Features: [color_red (0-1), diameter (cm)]
def build_fruit_tree():
    """
    Build a simple fruit classifier:
    - If diameter >= 7 cm:
        - If red >= 0.7: Apple
        - Else: Orange
    - Else:
        - If red >= 0.7: Cherry
        - Else: Grape
    """
    # TODO: Build and return tree
    pass
```

---

## Part 9: Testing and Debugging

### Exercise 9.1: Test Tree Equality

**Task:** Write a function to check if two trees are identical.

```python
def is_same_tree(p, q):
    """
    Check if two trees are structurally identical
    and have the same node values.
    """
    # TODO: Implement
    pass
```

---

### Exercise 9.2: Validate Your BST Implementation

**Task:** Write comprehensive tests for your BST class.

```python
def test_bst():
    """Test all BST operations."""
    bst = BST()

    # Test 1: Empty tree
    assert bst.search(5) == False
    assert bst.find_min() is None

    # Test 2: Insert and search
    bst.insert(5)
    assert bst.search(5) == True
    assert bst.search(3) == False

    # Test 3: Multiple inserts
    for val in [3, 7, 2, 4, 6, 8]:
        bst.insert(val)

    # Test 4: Inorder should be sorted
    assert bst.inorder() == [2, 3, 4, 5, 6, 7, 8]

    # Test 5: Min and max
    assert bst.find_min() == 2
    assert bst.find_max() == 8

    # Test 6: Delete leaf
    bst.delete(2)
    assert bst.search(2) == False
    assert bst.inorder() == [3, 4, 5, 6, 7, 8]

    # Test 7: Delete node with one child
    # Add more tests...

    print("All tests passed!")
```

---

### Exercise 9.3: Debug Incorrect Implementations

**Find and fix the bugs in these implementations:**

**Buggy implementation 1:**
```python
def height(node):
    if node is None:
        return 0
    left_height = height(node.left)
    right_height = height(node.right)
    return max(left_height, right_height)  # Bug here!
```

**Buggy implementation 2:**
```python
def is_valid_bst(node):
    if node is None:
        return True
    if node.left and node.left.value >= node.value:
        return False
    if node.right and node.right.value <= node.value:
        return False
    return is_valid_bst(node.left) and is_valid_bst(node.right)
    # Bug: This doesn't check the full BST property!
```

**Buggy implementation 3:**
```python
def insert_bst(root, value):
    if root is None:
        return TreeNode(value)
    if value < root.value:
        root.left = insert_bst(root.left, value)
    else:  # Bug here!
        root.right = insert_bst(root.right, value)
    return root
```

---

## Solutions to Selected Exercises

### Exercise 2.1 Solution

**Tree:**
```
       1
      / \
     2   3
    / \   \
   4   5   6
```

1. **Inorder (Left → Root → Right):** 4, 2, 5, 1, 3, 6
2. **Preorder (Root → Left → Right):** 1, 2, 4, 5, 3, 6
3. **Postorder (Left → Right → Root):** 4, 5, 2, 6, 3, 1
4. **Level-order:** 1, 2, 3, 4, 5, 6

---

### Exercise 3.1 Solution

- **Tree A: Valid BST** ✓
- **Tree B: Invalid** ✗ (6 is in left subtree but > 5)
- **Tree C: Valid BST** ✓

---

### Exercise 4.1 Solution

```python
def max_depth(root):
    if root is None:
        return 0
    left_depth = max_depth(root.left)
    right_depth = max_depth(root.right)
    return 1 + max(left_depth, right_depth)
```

---

### Exercise 4.2 Solution

```python
def is_symmetric(root):
    def is_mirror(left, right):
        if left is None and right is None:
            return True
        if left is None or right is None:
            return False
        return (left.value == right.value and
                is_mirror(left.left, right.right) and
                is_mirror(left.right, right.left))

    return is_mirror(root, root) if root else True
```

---

### Exercise 4.4 Solution

```python
def has_path_sum(root, target_sum):
    if root is None:
        return False

    # Check if leaf node
    if root.left is None and root.right is None:
        return root.value == target_sum

    # Check left and right subtrees
    remaining = target_sum - root.value
    return (has_path_sum(root.left, remaining) or
            has_path_sum(root.right, remaining))
```

---

### Exercise 5.1 Solution

```python
def is_balanced(root):
    def check_height(node):
        if node is None:
            return 0

        left_height = check_height(node.left)
        if left_height == -1:
            return -1

        right_height = check_height(node.right)
        if right_height == -1:
            return -1

        if abs(left_height - right_height) > 1:
            return -1

        return 1 + max(left_height, right_height)

    return check_height(root) != -1
```

---

### Exercise 9.3 Solutions

**Bug 1:** Missing `+ 1` in return statement
```python
return 1 + max(left_height, right_height)
```

**Bug 2:** Only checks immediate children, not entire subtree. Need to pass min/max bounds:
```python
def is_valid_bst(node, min_val=float('-inf'), max_val=float('inf')):
    if node is None:
        return True
    if node.value <= min_val or node.value >= max_val:
        return False
    return (is_valid_bst(node.left, min_val, node.value) and
            is_valid_bst(node.right, node.value, max_val))
```

**Bug 3:** `else` should be `elif value > root.value` to avoid inserting duplicates:
```python
elif value > root.value:
    root.right = insert_bst(root.right, value)
```

---

## Additional Practice

For more practice:
1. **LeetCode Tree Problems** - 100+ tree problems
2. **Visualize trees** - Use tools like [VisuAlgo](https://visualgo.net)
3. **Implement variants** - Try creating threaded binary trees, expression trees
4. **Real projects** - Build a file indexer, decision tree classifier
5. **Study advanced trees** - Tries, Segment Trees, Splay Trees

Trees are fundamental to many algorithms and data structures. Master them!
