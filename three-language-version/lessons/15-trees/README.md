# Lesson 15: Trees and Tree Algorithms

## Overview

Hierarchical data structures with a root and children.

## Binary Search Tree (BST)

**Properties:**
- Left subtree < node < Right subtree
- Efficient search, insert, delete (O(log n) average)

**Python:**
```python
class TreeNode:
    def __init__(self, val):
        self.val = val
        self.left = None
        self.right = None

class BST:
    def __init__(self):
        self.root = None
    
    def insert(self, val):
        if not self.root:
            self.root = TreeNode(val)
        else:
            self._insert_recursive(self.root, val)
    
    def _insert_recursive(self, node, val):
        if val < node.val:
            if node.left is None:
                node.left = TreeNode(val)
            else:
                self._insert_recursive(node.left, val)
        else:
            if node.right is None:
                node.right = TreeNode(val)
            else:
                self._insert_recursive(node.right, val)
    
    def search(self, val):
        return self._search_recursive(self.root, val)
    
    def _search_recursive(self, node, val):
        if node is None:
            return False
        if node.val == val:
            return True
        elif val < node.val:
            return self._search_recursive(node.left, val)
        else:
            return self._search_recursive(node.right, val)
```

**Haskell:**
```haskell
data Tree a = Empty | Node a (Tree a) (Tree a)

insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x (Node y left right)
    | x < y     = Node y (insert x left) right
    | otherwise = Node y left (insert x right)

search :: Ord a => a -> Tree a -> Bool
search _ Empty = False
search x (Node y left right)
    | x == y    = True
    | x < y     = search x left
    | otherwise = search x right
```

## Tree Traversals

**Inorder (Left, Root, Right):**
```python
def inorder(node):
    if node:
        inorder(node.left)
        print(node.val)
        inorder(node.right)
```

**Preorder (Root, Left, Right):**
```python
def preorder(node):
    if node:
        print(node.val)
        preorder(node.left)
        preorder(node.right)
```

**Postorder (Left, Right, Root):**
```python
def postorder(node):
    if node:
        postorder(node.left)
        postorder(node.right)
        print(node.val)
```

**Level-order (BFS):**
```python
from collections import deque

def level_order(root):
    if not root:
        return
    queue = deque([root])
    while queue:
        node = queue.popleft()
        print(node.val)
        if node.left:
            queue.append(node.left)
        if node.right:
            queue.append(node.right)
```

## Common Tree Operations

- **Height**: Maximum depth from root to leaf
- **Size**: Total number of nodes
- **Balance**: Difference in heights of subtrees
- **LCA**: Lowest Common Ancestor

## Complexity

| Operation | Average | Worst |
|-----------|---------|-------|
| Search | O(log n) | O(n) |
| Insert | O(log n) | O(n) |
| Delete | O(log n) | O(n) |
| Traversal | O(n) | O(n) |

See EXERCISES.md for tree implementation practice.
