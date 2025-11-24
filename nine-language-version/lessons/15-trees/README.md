# Lesson 15: Trees and Tree Algorithms

## Introduction

Trees are hierarchical data structures that consist of nodes connected by edges. Unlike linear data structures (arrays, linked lists), trees represent hierarchical relationships, making them ideal for representing file systems, organizational charts, family trees, and many other real-world structures.

## Why Trees Matter

Trees are fundamental to computer science:
- **File systems**: Directories and subdirectories form a tree
- **HTML/XML DOM**: Web pages are represented as trees
- **Databases**: B-trees and B+ trees power database indices
- **Compilers**: Abstract Syntax Trees (AST) represent code structure
- **AI**: Decision trees for machine learning
- **Routing**: Network routing tables use tree structures

---

## Part 1: Tree Terminology

### Basic Concepts

```
        A         ← Root (no parent)
       / \
      B   C       ← Children of A
     / \   \
    D   E   F     ← Leaves (no children)
```

**Key Terms:**
- **Root**: The topmost node (A)
- **Parent**: A node with children (A is parent of B and C)
- **Child**: A node with a parent (B and C are children of A)
- **Leaf**: A node with no children (D, E, F)
- **Siblings**: Nodes with the same parent (B and C)
- **Edge**: Connection between nodes
- **Path**: Sequence of nodes connected by edges
- **Height**: Longest path from root to a leaf (height of tree above = 2)
- **Depth**: Distance from root to a node (depth of D = 2)
- **Subtree**: A node and all its descendants

### Properties

- **Degree**: Number of children a node has
  - Binary tree: Each node has at most 2 children
  - Ternary tree: Each node has at most 3 children

- **Level**: All nodes at the same depth
  ```
  Level 0:     A
  Level 1:    B C
  Level 2:   D E F
  ```

---

## Part 2: Binary Trees

A **binary tree** is a tree where each node has **at most two children** (left and right).

### Binary Tree Node Structure

**Python:**
```python
class TreeNode:
    def __init__(self, value):
        self.value = value
        self.left = None
        self.right = None

# Create a simple binary tree
root = TreeNode(1)
root.left = TreeNode(2)
root.right = TreeNode(3)
root.left.left = TreeNode(4)
root.left.right = TreeNode(5)

#       1
#      / \
#     2   3
#    / \
#   4   5
```

**Java:**
```java
class TreeNode {
    int value;
    TreeNode left;
    TreeNode right;

    TreeNode(int value) {
        this.value = value;
        this.left = null;
        this.right = null;
    }
}

// Create a simple binary tree
TreeNode root = new TreeNode(1);
root.left = new TreeNode(2);
root.right = new TreeNode(3);
root.left.left = new TreeNode(4);
root.left.right = new TreeNode(5);
```

**C:**
```c
struct TreeNode {
    int value;
    struct TreeNode* left;
    struct TreeNode* right;
};

struct TreeNode* createNode(int value) {
    struct TreeNode* node = malloc(sizeof(struct TreeNode));
    node->value = value;
    node->left = NULL;
    node->right = NULL;
    return node;
}
```

### Types of Binary Trees

**1. Full Binary Tree**
- Every node has 0 or 2 children (no node has 1 child)

```
       1
      / \
     2   3
    / \
   4   5
```

**2. Complete Binary Tree**
- All levels are filled except possibly the last
- Last level is filled left to right

```
       1
      / \
     2   3
    / \  /
   4  5 6
```

**3. Perfect Binary Tree**
- All internal nodes have 2 children
- All leaves at the same level

```
       1
      / \
     2   3
    / \ / \
   4  5 6  7
```

**4. Balanced Binary Tree**
- Height difference between left and right subtrees ≤ 1 for all nodes
- Ensures O(log n) operations

---

## Part 3: Tree Traversal

Tree traversal visits all nodes in a specific order. There are two main categories: **Depth-First** and **Breadth-First**.

### Depth-First Traversal (DFS)

**1. Inorder Traversal** (Left → Root → Right)

```python
def inorder(node):
    if node is None:
        return
    inorder(node.left)      # Visit left subtree
    print(node.value)        # Visit root
    inorder(node.right)      # Visit right subtree
```

**Example:**
```
Tree:     1
         / \
        2   3
       / \
      4   5

Inorder output: 4, 2, 5, 1, 3
```

**Use case:** In BST, inorder traversal gives sorted order!

**2. Preorder Traversal** (Root → Left → Right)

```python
def preorder(node):
    if node is None:
        return
    print(node.value)        # Visit root
    preorder(node.left)      # Visit left subtree
    preorder(node.right)     # Visit right subtree
```

**Example:**
```
Tree:     1
         / \
        2   3
       / \
      4   5

Preorder output: 1, 2, 4, 5, 3
```

**Use case:** Used to create a copy of the tree, or get prefix notation

**3. Postorder Traversal** (Left → Right → Root)

```python
def postorder(node):
    if node is None:
        return
    postorder(node.left)     # Visit left subtree
    postorder(node.right)    # Visit right subtree
    print(node.value)        # Visit root
```

**Example:**
```
Tree:     1
         / \
        2   3
       / \
      4   5

Postorder output: 4, 5, 2, 3, 1
```

**Use case:** Used to delete the tree (delete children before parent), or get postfix notation

### Breadth-First Traversal (BFS)

**Level-Order Traversal** - Visit nodes level by level, left to right

```python
from collections import deque

def level_order(root):
    if root is None:
        return

    queue = deque([root])

    while queue:
        node = queue.popleft()
        print(node.value)

        if node.left:
            queue.append(node.left)
        if node.right:
            queue.append(node.right)
```

**Example:**
```
Tree:     1
         / \
        2   3
       / \   \
      4   5   6

Level-order output: 1, 2, 3, 4, 5, 6
```

**Java Implementation:**
```java
void levelOrder(TreeNode root) {
    if (root == null) return;

    Queue<TreeNode> queue = new LinkedList<>();
    queue.add(root);

    while (!queue.isEmpty()) {
        TreeNode node = queue.remove();
        System.out.print(node.value + " ");

        if (node.left != null) queue.add(node.left);
        if (node.right != null) queue.add(node.right);
    }
}
```

---

## Part 4: Binary Search Trees (BST)

A **Binary Search Tree** is a binary tree with an ordering property:
- **All nodes in the left subtree < root**
- **All nodes in the right subtree > root**
- This property holds for **every** subtree

### BST Example

```
        8
       / \
      3   10
     / \    \
    1   6    14
       / \   /
      4   7 13
```

- All values in left subtree of 8 are < 8
- All values in right subtree of 8 are > 8
- This property applies recursively

### BST Operations

**1. Search** - O(h) where h is height

```python
def search(root, target):
    """Search for a value in BST."""
    if root is None:
        return False

    if root.value == target:
        return True
    elif target < root.value:
        return search(root.left, target)   # Search left
    else:
        return search(root.right, target)  # Search right
```

**Iterative version:**
```python
def search_iterative(root, target):
    current = root
    while current is not None:
        if current.value == target:
            return True
        elif target < current.value:
            current = current.left
        else:
            current = current.right
    return False
```

**2. Insert** - O(h)

```python
def insert(root, value):
    """Insert a value into BST."""
    if root is None:
        return TreeNode(value)

    if value < root.value:
        root.left = insert(root.left, value)
    elif value > root.value:
        root.right = insert(root.right, value)
    # If value == root.value, we don't insert duplicates

    return root
```

**Example:**
```
Insert 5 into:     8
                  / \
                 3   10

Step 1: 5 < 8, go left
Step 2: 5 > 3, go right
Step 3: Right is null, insert here

Result:    8
          / \
         3   10
          \
           5
```

**3. Find Minimum** - O(h)

```python
def find_min(root):
    """Find the minimum value in BST (leftmost node)."""
    if root is None:
        return None

    current = root
    while current.left is not None:
        current = current.left
    return current.value
```

**4. Find Maximum** - O(h)

```python
def find_max(root):
    """Find the maximum value in BST (rightmost node)."""
    if root is None:
        return None

    current = root
    while current.right is not None:
        current = current.right
    return current.value
```

**5. Delete** - O(h) - **Most complex operation!**

Three cases to handle:

```python
def delete(root, value):
    """Delete a value from BST."""
    if root is None:
        return None

    # Find the node to delete
    if value < root.value:
        root.left = delete(root.left, value)
    elif value > root.value:
        root.right = delete(root.right, value)
    else:
        # Found the node to delete!

        # Case 1: Node has no children (leaf)
        if root.left is None and root.right is None:
            return None

        # Case 2: Node has one child
        if root.left is None:
            return root.right
        if root.right is None:
            return root.left

        # Case 3: Node has two children
        # Find the inorder successor (smallest in right subtree)
        min_right = find_min_node(root.right)
        root.value = min_right.value
        # Delete the inorder successor
        root.right = delete(root.right, min_right.value)

    return root

def find_min_node(root):
    """Helper to find the leftmost node."""
    current = root
    while current.left is not None:
        current = current.left
    return current
```

**Delete Example - Case 3:**
```
Delete 3 from:     8
                  / \
                 3   10
                / \    \
               1   6   14
                  / \  /
                 4  7 13

Step 1: Find node with value 3
Step 2: It has two children
Step 3: Find inorder successor (smallest in right subtree) = 4
Step 4: Replace 3 with 4
Step 5: Delete the original 4 node

Result:    8
          / \
         4   10
        / \    \
       1   6   14
          / \  /
         5  7 13
```

### BST Time Complexity

| Operation | Average Case | Worst Case (Skewed) |
|-----------|--------------|---------------------|
| Search    | O(log n)     | O(n)                |
| Insert    | O(log n)     | O(n)                |
| Delete    | O(log n)     | O(n)                |

**Worst case** occurs when tree becomes skewed (like a linked list):
```
1
 \
  2
   \
    3
     \
      4  ← Height = n-1
```

**Best case** occurs when tree is balanced:
```
      2
     / \
    1   3  ← Height = log n
```

### Java BST Implementation

```java
class BinarySearchTree {
    TreeNode root;

    public boolean search(int target) {
        return searchHelper(root, target);
    }

    private boolean searchHelper(TreeNode node, int target) {
        if (node == null) {
            return false;
        }
        if (node.value == target) {
            return true;
        }
        if (target < node.value) {
            return searchHelper(node.left, target);
        }
        return searchHelper(node.right, target);
    }

    public void insert(int value) {
        root = insertHelper(root, value);
    }

    private TreeNode insertHelper(TreeNode node, int value) {
        if (node == null) {
            return new TreeNode(value);
        }
        if (value < node.value) {
            node.left = insertHelper(node.left, value);
        } else if (value > node.value) {
            node.right = insertHelper(node.right, value);
        }
        return node;
    }

    public void inorder() {
        inorderHelper(root);
        System.out.println();
    }

    private void inorderHelper(TreeNode node) {
        if (node != null) {
            inorderHelper(node.left);
            System.out.print(node.value + " ");
            inorderHelper(node.right);
        }
    }
}
```

---

## Part 5: Balanced Binary Search Trees

To maintain O(log n) operations, we need **balanced** trees. When a tree is balanced, the height remains logarithmic.

### AVL Trees

**AVL Tree** = Self-balancing BST
- Named after inventors Adelson-Velsky and Landis
- **Balance factor** = height(left subtree) - height(right subtree)
- Balance factor must be -1, 0, or 1 for every node

**Rotations** restore balance:

**Right Rotation:**
```
    y                x
   / \              / \
  x   C    →       A   y
 / \                  / \
A   B                B   C
```

**Left Rotation:**
```
  x                  y
 / \                / \
A   y      →       x   C
   / \            / \
  B   C          A   B
```

**When to use:**
- When you need guaranteed O(log n) performance
- When insertions and deletions are frequent
- Example: Database indexing

### Red-Black Trees

**Red-Black Tree** = Another self-balancing BST
- Each node is colored red or black
- Root is always black
- Red nodes cannot have red children
- Every path from root to leaf has the same number of black nodes

**Properties ensure** height ≤ 2 log(n+1), maintaining O(log n) operations

**Used in:**
- C++ `std::map` and `std::set`
- Java `TreeMap` and `TreeSet`
- Linux kernel's scheduler

### Comparison

| Tree Type | Guarantee | Rotation Overhead | Use Case |
|-----------|-----------|-------------------|----------|
| BST       | None      | None              | Simple scenarios, sorted input unlikely |
| AVL       | Strict    | High (many rotations) | Read-heavy applications |
| Red-Black | Looser    | Low (fewer rotations) | Balanced read/write |

---

## Part 6: Heap Data Structure

A **heap** is a complete binary tree with a heap property.

### Types of Heaps

**Max Heap:** Parent ≥ all children
```
       100
       /  \
      19   36
     / \   /
    17  3 25
```

**Min Heap:** Parent ≤ all children
```
        3
       / \
      17  25
     / \  /
    19 36 100
```

### Heap Properties

1. **Complete binary tree**: All levels filled except possibly the last
2. **Heap property**: Parent-child relationship maintained
3. **Array representation**: Can be stored efficiently in an array

**Array representation:**
```
Heap:       10
           /  \
          9    8
         / \  /
        7  6 5

Array: [10, 9, 8, 7, 6, 5]
Index:  0  1  2  3  4  5

For node at index i:
- Left child: 2*i + 1
- Right child: 2*i + 2
- Parent: (i-1) // 2
```

### Heap Operations

**1. Insert (Heapify Up)** - O(log n)

```python
class MaxHeap:
    def __init__(self):
        self.heap = []

    def insert(self, value):
        # Add to end
        self.heap.append(value)
        # Bubble up
        self._heapify_up(len(self.heap) - 1)

    def _heapify_up(self, index):
        parent = (index - 1) // 2

        # If current > parent, swap
        if index > 0 and self.heap[index] > self.heap[parent]:
            self.heap[index], self.heap[parent] = \
                self.heap[parent], self.heap[index]
            self._heapify_up(parent)
```

**2. Extract Max (Heapify Down)** - O(log n)

```python
    def extract_max(self):
        if len(self.heap) == 0:
            return None

        if len(self.heap) == 1:
            return self.heap.pop()

        # Store max value
        max_value = self.heap[0]

        # Move last to root
        self.heap[0] = self.heap.pop()

        # Bubble down
        self._heapify_down(0)

        return max_value

    def _heapify_down(self, index):
        largest = index
        left = 2 * index + 1
        right = 2 * index + 2

        # Find largest among node and children
        if left < len(self.heap) and self.heap[left] > self.heap[largest]:
            largest = left

        if right < len(self.heap) and self.heap[right] > self.heap[largest]:
            largest = right

        # If largest is not the current node, swap
        if largest != index:
            self.heap[index], self.heap[largest] = \
                self.heap[largest], self.heap[index]
            self._heapify_down(largest)
```

**3. Peek** - O(1)

```python
    def peek(self):
        return self.heap[0] if self.heap else None
```

### Heap Applications

**1. Priority Queue**
```python
import heapq

# Min heap by default
pq = []
heapq.heappush(pq, (1, "Low priority"))
heapq.heappush(pq, (0, "High priority"))
heapq.heappush(pq, (2, "Lowest priority"))

print(heapq.heappop(pq))  # (0, "High priority")
```

**2. Heap Sort** - O(n log n)
```python
def heap_sort(arr):
    # Build max heap
    heap = []
    for num in arr:
        heapq.heappush(heap, -num)  # Negative for max heap

    # Extract elements
    sorted_arr = []
    while heap:
        sorted_arr.append(-heapq.heappop(heap))

    return sorted_arr
```

**3. Top K Elements**
```python
def find_k_largest(arr, k):
    """Find k largest elements using min heap."""
    heap = []
    for num in arr:
        heapq.heappush(heap, num)
        if len(heap) > k:
            heapq.heappop(heap)  # Remove smallest
    return heap

# Example
print(find_k_largest([3, 1, 5, 12, 2, 11], k=3))  # [5, 11, 12]
```

### Java PriorityQueue

```java
import java.util.PriorityQueue;

PriorityQueue<Integer> minHeap = new PriorityQueue<>();
minHeap.add(5);
minHeap.add(2);
minHeap.add(8);

System.out.println(minHeap.poll());  // 2 (smallest)

// Max heap
PriorityQueue<Integer> maxHeap = new PriorityQueue<>((a, b) -> b - a);
maxHeap.add(5);
maxHeap.add(2);
maxHeap.add(8);

System.out.println(maxHeap.poll());  // 8 (largest)
```

---

## Part 7: Common Tree Algorithms

### 1. Calculate Tree Height

```python
def height(node):
    """Height = longest path from node to a leaf."""
    if node is None:
        return -1  # or 0, depending on definition

    left_height = height(node.left)
    right_height = height(node.right)

    return 1 + max(left_height, right_height)
```

### 2. Count Nodes

```python
def count_nodes(node):
    """Count total nodes in tree."""
    if node is None:
        return 0
    return 1 + count_nodes(node.left) + count_nodes(node.right)
```

### 3. Check if Balanced

```python
def is_balanced(node):
    """Check if tree is height-balanced."""
    def check_height(node):
        if node is None:
            return 0

        left_height = check_height(node.left)
        if left_height == -1:
            return -1  # Left subtree not balanced

        right_height = check_height(node.right)
        if right_height == -1:
            return -1  # Right subtree not balanced

        if abs(left_height - right_height) > 1:
            return -1  # Current node not balanced

        return 1 + max(left_height, right_height)

    return check_height(node) != -1
```

### 4. Lowest Common Ancestor (LCA) in BST

```python
def lca_bst(root, p, q):
    """Find lowest common ancestor in BST."""
    if root is None:
        return None

    # If both p and q are smaller, LCA is in left subtree
    if p < root.value and q < root.value:
        return lca_bst(root.left, p, q)

    # If both p and q are larger, LCA is in right subtree
    if p > root.value and q > root.value:
        return lca_bst(root.right, p, q)

    # Otherwise, root is the LCA
    return root
```

### 5. Validate BST

```python
def is_valid_bst(root):
    """Check if tree is a valid BST."""
    def validate(node, min_val, max_val):
        if node is None:
            return True

        # Current node must be within range
        if node.value <= min_val or node.value >= max_val:
            return False

        # Check left and right subtrees
        return (validate(node.left, min_val, node.value) and
                validate(node.right, node.value, max_val))

    return validate(root, float('-inf'), float('inf'))
```

### 6. Mirror/Invert Tree

```python
def invert_tree(root):
    """Mirror the tree (swap left and right subtrees)."""
    if root is None:
        return None

    # Swap children
    root.left, root.right = root.right, root.left

    # Recursively invert subtrees
    invert_tree(root.left)
    invert_tree(root.right)

    return root
```

### 7. Path Sum

```python
def has_path_sum(root, target_sum):
    """Check if there's a root-to-leaf path with given sum."""
    if root is None:
        return False

    # Leaf node
    if root.left is None and root.right is None:
        return root.value == target_sum

    # Check left and right paths
    remaining = target_sum - root.value
    return (has_path_sum(root.left, remaining) or
            has_path_sum(root.right, remaining))
```

### 8. Serialize and Deserialize

```python
def serialize(root):
    """Convert tree to string."""
    if root is None:
        return "null"

    left = serialize(root.left)
    right = serialize(root.right)

    return f"{root.value},{left},{right}"

def deserialize(data):
    """Convert string back to tree."""
    def helper(values):
        val = next(values)
        if val == "null":
            return None

        node = TreeNode(int(val))
        node.left = helper(values)
        node.right = helper(values)
        return node

    values = iter(data.split(','))
    return helper(values)
```

---

## Part 8: Real-World Applications

### File System

```python
class FileNode:
    def __init__(self, name, is_directory=False):
        self.name = name
        self.is_directory = is_directory
        self.children = [] if is_directory else None
        self.size = 0

def calculate_size(node):
    """Calculate total size of directory."""
    if not node.is_directory:
        return node.size

    total = 0
    for child in node.children:
        total += calculate_size(child)
    return total

# Example usage
root = FileNode("/", is_directory=True)
home = FileNode("home", is_directory=True)
docs = FileNode("documents", is_directory=True)
file1 = FileNode("file1.txt")
file1.size = 100

root.children.append(home)
home.children.append(docs)
docs.children.append(file1)
```

### Expression Trees

```python
class ExprNode:
    def __init__(self, value):
        self.value = value
        self.left = None
        self.right = None

def evaluate(node):
    """Evaluate arithmetic expression tree."""
    if node.left is None and node.right is None:
        return int(node.value)  # Leaf node (operand)

    left_val = evaluate(node.left)
    right_val = evaluate(node.right)

    if node.value == '+':
        return left_val + right_val
    elif node.value == '-':
        return left_val - right_val
    elif node.value == '*':
        return left_val * right_val
    elif node.value == '/':
        return left_val / right_val

# Example: (3 + 2) * 5
#        *
#       / \
#      +   5
#     / \
#    3   2

root = ExprNode('*')
root.left = ExprNode('+')
root.right = ExprNode('5')
root.left.left = ExprNode('3')
root.left.right = ExprNode('2')

print(evaluate(root))  # 25
```

### Decision Trees (Simplified)

```python
class DecisionNode:
    def __init__(self, question, true_branch=None, false_branch=None):
        self.question = question
        self.true_branch = true_branch
        self.false_branch = false_branch

def classify(node, data):
    """Classify data using decision tree."""
    if node.true_branch is None and node.false_branch is None:
        return node.question  # Leaf node (classification)

    if evaluate_question(node.question, data):
        return classify(node.true_branch, data)
    else:
        return classify(node.false_branch, data)

# Example: Classify if someone should bring umbrella
root = DecisionNode("Is it raining?",
    true_branch=DecisionNode("Yes, bring umbrella"),
    false_branch=DecisionNode("Is it cloudy?",
        true_branch=DecisionNode("Maybe bring umbrella"),
        false_branch=DecisionNode("No umbrella needed")
    )
)
```

---

## Part 9: Time and Space Complexity Summary

### Operations

| Operation | BST (Balanced) | BST (Skewed) | Heap |
|-----------|----------------|--------------|------|
| Search    | O(log n)       | O(n)         | O(n) |
| Insert    | O(log n)       | O(n)         | O(log n) |
| Delete    | O(log n)       | O(n)         | O(log n) |
| Find Min/Max | O(log n)    | O(n)         | O(1) |
| Traversal | O(n)           | O(n)         | O(n) |

### Space Complexity

| Structure | Space |
|-----------|-------|
| Tree (array) | O(n) |
| Tree (linked) | O(n) |
| Recursion depth | O(h) where h = height |
| Heap | O(n) |

---

## Best Practices

1. **Choose the right tree structure:**
   - Need sorted traversal → BST
   - Need priority operations → Heap
   - Need guaranteed balance → AVL or Red-Black tree

2. **Handle edge cases:**
   ```python
   if root is None:
       return # or appropriate value
   ```

3. **Use iterative approaches when possible** to avoid stack overflow:
   ```python
   # Iterative > Recursive for deep trees
   def iterative_search(root, target):
       while root:
           if root.value == target:
               return True
           root = root.left if target < root.value else root.right
       return False
   ```

4. **Consider space-time tradeoffs:**
   - Recursion is elegant but uses O(h) space
   - Iteration is more complex but uses O(1) space

5. **Test with different tree shapes:**
   - Balanced tree
   - Skewed tree (worst case)
   - Empty tree
   - Single node

---

## Common Pitfalls

1. **Forgetting null checks:**
   ```python
   # Wrong
   def height(node):
       return 1 + max(height(node.left), height(node.right))

   # Right
   def height(node):
       if node is None:
           return 0
       return 1 + max(height(node.left), height(node.right))
   ```

2. **Confusing height and depth:**
   - Height: longest path from node DOWN to leaf
   - Depth: distance from root DOWN to node

3. **Not handling empty tree:**
   ```python
   # Always check
   if root is None:
       return appropriate_value
   ```

4. **Incorrect BST property:**
   ```python
   # Wrong: only compares with immediate parent
   if node.left.value < node.value:  # Incomplete!

   # Right: all left values must be less than all right values
   validate(node, min_bound, max_bound)
   ```

---

## Next Steps

- **Practice:** Implement all tree operations from scratch
- **LeetCode:** Trees are common interview topics
- **Advanced topics:**
  - Tries (prefix trees)
  - Segment trees
  - Fenwick trees (Binary Indexed Trees)
  - B-trees and B+ trees (database indexing)

Trees are fundamental! Master them, and you'll have a strong foundation for advanced data structures and algorithms.
