# Lab 24: Tree Building

**Quarter 3, Week 4**
**Duration:** 90 minutes
**Format:** Pair programming

## Overview

Trees are hierarchical data structures that appear everywhere in computing: file systems, HTML DOM, decision trees, and more. This lab builds binary search trees from scratch.

## Objectives

By the end of this lab, you will:
- [ ] Implement a binary search tree (BST)
- [ ] Perform insertions and searches
- [ ] Implement tree traversals
- [ ] Understand tree height and balance

## Setup

- Partner up
- Create folder: `lab24-trees/`
- Files: `bst.py`, `bst.hs`

---

## Part 1: Tree Basics (20 minutes)

### Activity 1.1: What Is a Tree?

```
        8          <- Root
       / \
      3   10       <- Internal nodes
     / \    \
    1   6   14     <- More internal nodes / leaves
       / \   /
      4   7 13     <- Leaves
```

**Terminology:**
- **Root**: Top node (8)
- **Leaf**: Node with no children (1, 4, 7, 13)
- **Internal node**: Node with children
- **Height**: Longest path from root to leaf
- **Depth**: Distance from root to a node

### Activity 1.2: Binary Search Tree Property

**BST Rule:** For every node:
- All values in left subtree are smaller
- All values in right subtree are larger

```
        8
       / \
      3   10
     / \    \
    1   6   14

Is 6 in the tree?
  8 → go left (6 < 8)
  3 → go right (6 > 3)
  6 → found!

Is 5 in the tree?
  8 → go left (5 < 8)
  3 → go right (5 > 3)
  6 → go left (5 < 6)
  Not found!
```

### Activity 1.3: Node Structure

**Python:**
```python
class TreeNode:
    def __init__(self, value):
        self.value = value
        self.left = None
        self.right = None

    def __repr__(self):
        return f"TreeNode({self.value})"
```

**Haskell:**
```haskell
data Tree a = Empty
            | Node a (Tree a) (Tree a)
    deriving (Show, Eq)
```

### ✅ Checkpoint 1

Verify:
- [ ] Understand BST property
- [ ] Can trace search by hand

---

## Part 2: Building a BST (25 minutes)

### Activity 2.1: Insert Operation

```python
class BST:
    def __init__(self):
        self.root = None

    def insert(self, value):
        """Insert a value into the BST."""
        if self.root is None:
            self.root = TreeNode(value)
        else:
            self._insert_recursive(self.root, value)

    def _insert_recursive(self, node, value):
        """Helper: recursively find correct position."""
        if value < node.value:
            if node.left is None:
                node.left = TreeNode(value)
            else:
                self._insert_recursive(node.left, value)
        else:  # value >= node.value
            if node.right is None:
                node.right = TreeNode(value)
            else:
                self._insert_recursive(node.right, value)

# Build tree
tree = BST()
for val in [8, 3, 10, 1, 6, 14, 4, 7, 13]:
    tree.insert(val)
```

### Activity 2.2: Search Operation

```python
def search(self, value):
    """Return True if value is in tree."""
    return self._search_recursive(self.root, value)

def _search_recursive(self, node, value):
    """Helper: recursively search."""
    if node is None:
        return False
    if value == node.value:
        return True
    elif value < node.value:
        return self._search_recursive(node.left, value)
    else:
        return self._search_recursive(node.right, value)

# Test
print(tree.search(6))   # True
print(tree.search(5))   # False
print(tree.search(14))  # True
```

### Activity 2.3: Haskell Implementation

```haskell
-- Insert into BST
insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x (Node v left right)
    | x < v     = Node v (insert x left) right
    | otherwise = Node v left (insert x right)

-- Build tree from list
fromList :: Ord a => [a] -> Tree a
fromList = foldr insert Empty

-- Search in BST
search :: Ord a => a -> Tree a -> Bool
search _ Empty = False
search x (Node v left right)
    | x == v    = True
    | x < v     = search x left
    | otherwise = search x right

main :: IO ()
main = do
    let tree = fromList [8, 3, 10, 1, 6, 14, 4, 7, 13]
    print tree
    print (search 6 tree)   -- True
    print (search 5 tree)   -- False
```

### Activity 2.4: Visualization

```python
def display(self):
    """Print tree structure."""
    lines = self._build_display(self.root, "", True)
    for line in lines:
        print(line)

def _build_display(self, node, prefix, is_left):
    """Build display lines recursively."""
    if node is None:
        return []

    lines = []
    connector = "└── " if is_left else "┌── "
    lines.append(prefix + connector + str(node.value))

    if node.left or node.right:
        extension = "    " if is_left else "│   "
        if node.right:
            lines = self._build_display(node.right, prefix + extension, False) + lines
        if node.left:
            lines = lines + self._build_display(node.left, prefix + extension, True)

    return lines

# Alternative simple display
def print_tree(self, node=None, level=0):
    """Simple sideways tree print."""
    if node is None:
        node = self.root
    if node.right:
        self.print_tree(node.right, level + 1)
    print('    ' * level + str(node.value))
    if node.left:
        self.print_tree(node.left, level + 1)
```

### ✅ Checkpoint 2

Verify:
- [ ] Can insert values
- [ ] Can search for values
- [ ] Tree displays correctly

---

## Part 3: Tree Traversals (20 minutes)

### Activity 3.1: Three Traversal Orders

```python
def inorder(self):
    """Left → Root → Right (sorted order for BST!)"""
    result = []
    self._inorder_recursive(self.root, result)
    return result

def _inorder_recursive(self, node, result):
    if node:
        self._inorder_recursive(node.left, result)
        result.append(node.value)
        self._inorder_recursive(node.right, result)

def preorder(self):
    """Root → Left → Right"""
    result = []
    self._preorder_recursive(self.root, result)
    return result

def _preorder_recursive(self, node, result):
    if node:
        result.append(node.value)
        self._preorder_recursive(node.left, result)
        self._preorder_recursive(node.right, result)

def postorder(self):
    """Left → Right → Root"""
    result = []
    self._postorder_recursive(self.root, result)
    return result

def _postorder_recursive(self, node, result):
    if node:
        self._postorder_recursive(node.left, result)
        self._postorder_recursive(node.right, result)
        result.append(node.value)

# Test
print(f"Inorder:   {tree.inorder()}")   # [1, 3, 4, 6, 7, 8, 10, 13, 14]
print(f"Preorder:  {tree.preorder()}")  # [8, 3, 1, 6, 4, 7, 10, 14, 13]
print(f"Postorder: {tree.postorder()}") # [1, 4, 7, 6, 3, 13, 14, 10, 8]
```

### Activity 3.2: Haskell Traversals

```haskell
-- Inorder: Left, Root, Right
inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node v left right) =
    inorder left ++ [v] ++ inorder right

-- Preorder: Root, Left, Right
preorder :: Tree a -> [a]
preorder Empty = []
preorder (Node v left right) =
    [v] ++ preorder left ++ preorder right

-- Postorder: Left, Right, Root
postorder :: Tree a -> [a]
postorder Empty = []
postorder (Node v left right) =
    postorder left ++ postorder right ++ [v]

main :: IO ()
main = do
    let tree = fromList [8, 3, 10, 1, 6, 14, 4, 7, 13]
    print (inorder tree)    -- [1,3,4,6,7,8,10,13,14]
    print (preorder tree)   -- [13,14,10,7,4,6,1,3,8]
    print (postorder tree)  -- [1,4,7,6,3,13,14,10,8]
```

### Activity 3.3: Level-Order (BFS)

```python
from collections import deque

def levelorder(self):
    """Breadth-first traversal (level by level)."""
    if not self.root:
        return []

    result = []
    queue = deque([self.root])

    while queue:
        node = queue.popleft()
        result.append(node.value)

        if node.left:
            queue.append(node.left)
        if node.right:
            queue.append(node.right)

    return result

print(f"Level-order: {tree.levelorder()}")  # [8, 3, 10, 1, 6, 14, 4, 7, 13]
```

### Activity 3.4: Practice - Traversal Tracing

Trace traversals for this tree:
```
        5
       / \
      3   7
     / \   \
    2   4   9
```

Fill in:
- Inorder: _______________
- Preorder: _______________
- Postorder: _______________
- Level-order: _______________

### ✅ Checkpoint 3

Verify:
- [ ] All four traversals work
- [ ] Can trace traversals by hand
- [ ] Understand why inorder gives sorted output

---

## Part 4: Tree Properties (15 minutes)

### Activity 4.1: Height and Size

```python
def height(self):
    """Return tree height (longest root-to-leaf path)."""
    return self._height_recursive(self.root)

def _height_recursive(self, node):
    if node is None:
        return 0
    return 1 + max(
        self._height_recursive(node.left),
        self._height_recursive(node.right)
    )

def size(self):
    """Return number of nodes."""
    return self._size_recursive(self.root)

def _size_recursive(self, node):
    if node is None:
        return 0
    return 1 + self._size_recursive(node.left) + self._size_recursive(node.right)

print(f"Height: {tree.height()}")  # 4
print(f"Size: {tree.size()}")      # 9
```

### Activity 4.2: Min and Max

```python
def find_min(self):
    """Find minimum value (leftmost node)."""
    if not self.root:
        return None
    node = self.root
    while node.left:
        node = node.left
    return node.value

def find_max(self):
    """Find maximum value (rightmost node)."""
    if not self.root:
        return None
    node = self.root
    while node.right:
        node = node.right
    return node.value

print(f"Min: {tree.find_min()}")  # 1
print(f"Max: {tree.find_max()}")  # 14
```

### Activity 4.3: Is It Balanced?

A balanced tree has height O(log n).

```python
def is_balanced(self):
    """Check if tree is height-balanced."""
    def check_height(node):
        if node is None:
            return 0, True

        left_h, left_bal = check_height(node.left)
        right_h, right_bal = check_height(node.right)

        balanced = left_bal and right_bal and abs(left_h - right_h) <= 1
        return max(left_h, right_h) + 1, balanced

    _, balanced = check_height(self.root)
    return balanced
```

### Activity 4.4: Haskell Properties

```haskell
-- Height
height :: Tree a -> Int
height Empty = 0
height (Node _ left right) = 1 + max (height left) (height right)

-- Size
size :: Tree a -> Int
size Empty = 0
size (Node _ left right) = 1 + size left + size right

-- Minimum (leftmost)
findMin :: Tree a -> Maybe a
findMin Empty = Nothing
findMin (Node v Empty _) = Just v
findMin (Node _ left _) = findMin left

-- Maximum (rightmost)
findMax :: Tree a -> Maybe a
findMax Empty = Nothing
findMax (Node v _ Empty) = Just v
findMax (Node _ _ right) = findMax right
```

### ✅ Checkpoint 4

Verify:
- [ ] Height and size work
- [ ] Min/max work
- [ ] Understand balance concept

---

## Part 5: Complete BST Class (10 minutes)

### Activity 5.1: Full Implementation

```python
class BST:
    def __init__(self):
        self.root = None

    def insert(self, value):
        if self.root is None:
            self.root = TreeNode(value)
        else:
            self._insert_recursive(self.root, value)

    def _insert_recursive(self, node, value):
        if value < node.value:
            if node.left is None:
                node.left = TreeNode(value)
            else:
                self._insert_recursive(node.left, value)
        else:
            if node.right is None:
                node.right = TreeNode(value)
            else:
                self._insert_recursive(node.right, value)

    def search(self, value):
        return self._search_recursive(self.root, value)

    def _search_recursive(self, node, value):
        if node is None:
            return False
        if value == node.value:
            return True
        elif value < node.value:
            return self._search_recursive(node.left, value)
        else:
            return self._search_recursive(node.right, value)

    def inorder(self):
        result = []
        self._inorder_recursive(self.root, result)
        return result

    def _inorder_recursive(self, node, result):
        if node:
            self._inorder_recursive(node.left, result)
            result.append(node.value)
            self._inorder_recursive(node.right, result)

    def height(self):
        return self._height_recursive(self.root)

    def _height_recursive(self, node):
        if node is None:
            return 0
        return 1 + max(
            self._height_recursive(node.left),
            self._height_recursive(node.right)
        )

    def __contains__(self, value):
        """Enable 'in' operator."""
        return self.search(value)

    def __len__(self):
        """Enable len()."""
        return self._size_recursive(self.root)

    def _size_recursive(self, node):
        if node is None:
            return 0
        return 1 + self._size_recursive(node.left) + self._size_recursive(node.right)
```

---

## Challenges

### Challenge 1: Delete Operation

Implement node deletion (this is tricky!):

```python
def delete(self, value):
    """Delete a value from the BST."""
    self.root = self._delete_recursive(self.root, value)

def _delete_recursive(self, node, value):
    # Three cases:
    # 1. Leaf node - just remove
    # 2. One child - replace with child
    # 3. Two children - replace with successor
    pass
```

### Challenge 2: Iterator

Make the BST iterable:

```python
def __iter__(self):
    """Iterate through values in sorted order."""
    yield from self._iter_inorder(self.root)

def _iter_inorder(self, node):
    if node:
        yield from self._iter_inorder(node.left)
        yield node.value
        yield from self._iter_inorder(node.right)

# Now you can: for val in tree: print(val)
```

### Challenge 3: Serialize/Deserialize

```python
def serialize(self):
    """Convert tree to string representation."""
    pass

@classmethod
def deserialize(cls, data):
    """Reconstruct tree from string."""
    pass
```

---

## Wrap-Up

**Key takeaways:**

1. **BST property** enables O(log n) search
2. **Traversals** visit nodes in different orders
3. **Inorder on BST** gives sorted sequence
4. **Balance matters** - unbalanced trees degrade to O(n)
5. **Recursive structure** - trees are naturally recursive

**Complexity:**

| Operation | Average | Worst (unbalanced) |
|-----------|---------|-------------------|
| Search    | O(log n)| O(n)              |
| Insert    | O(log n)| O(n)              |
| Delete    | O(log n)| O(n)              |

**Next lab:** Graph Exploration - when trees aren't enough!
