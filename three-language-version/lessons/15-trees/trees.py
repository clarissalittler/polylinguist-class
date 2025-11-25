"""
Lesson 15: Trees - Python Examples
Binary trees, BSTs, traversals, and common operations
"""

from typing import TypeVar, Generic, Optional, List, Callable
from collections import deque

T = TypeVar('T')

# =============================================================================
# BINARY TREE NODE
# =============================================================================

class TreeNode(Generic[T]):
    """Basic binary tree node"""

    def __init__(self, value: T):
        self.value: T = value
        self.left: Optional['TreeNode[T]'] = None
        self.right: Optional['TreeNode[T]'] = None

    def __repr__(self) -> str:
        return f"TreeNode({self.value})"


# =============================================================================
# TREE TRAVERSALS
# =============================================================================

def inorder(node: Optional[TreeNode[T]]) -> List[T]:
    """
    In-order traversal: Left, Node, Right
    For BST: returns sorted order
    """
    if node is None:
        return []
    return inorder(node.left) + [node.value] + inorder(node.right)


def preorder(node: Optional[TreeNode[T]]) -> List[T]:
    """
    Pre-order traversal: Node, Left, Right
    Useful for copying/serializing tree
    """
    if node is None:
        return []
    return [node.value] + preorder(node.left) + preorder(node.right)


def postorder(node: Optional[TreeNode[T]]) -> List[T]:
    """
    Post-order traversal: Left, Right, Node
    Useful for deletion, evaluation of expressions
    """
    if node is None:
        return []
    return postorder(node.left) + postorder(node.right) + [node.value]


def level_order(root: Optional[TreeNode[T]]) -> List[List[T]]:
    """
    Level-order (BFS) traversal
    Returns nodes level by level
    """
    if root is None:
        return []

    result = []
    queue = deque([root])

    while queue:
        level = []
        level_size = len(queue)

        for _ in range(level_size):
            node = queue.popleft()
            level.append(node.value)

            if node.left:
                queue.append(node.left)
            if node.right:
                queue.append(node.right)

        result.append(level)

    return result


# Build sample tree:
#       1
#      / \
#     2   3
#    / \   \
#   4   5   6

print("=== Tree Traversals ===")
root = TreeNode(1)
root.left = TreeNode(2)
root.right = TreeNode(3)
root.left.left = TreeNode(4)
root.left.right = TreeNode(5)
root.right.right = TreeNode(6)

print(f"In-order:    {inorder(root)}")
print(f"Pre-order:   {preorder(root)}")
print(f"Post-order:  {postorder(root)}")
print(f"Level-order: {level_order(root)}")


# =============================================================================
# BINARY SEARCH TREE
# =============================================================================

class BST(Generic[T]):
    """
    Binary Search Tree
    Left < Node < Right
    Operations: insert, search, delete - O(h) where h is height
    Balanced: h = O(log n), Worst: h = O(n)
    """

    def __init__(self):
        self.root: Optional[TreeNode[T]] = None

    def insert(self, value: T) -> None:
        """Insert value into BST"""
        self.root = self._insert(self.root, value)

    def _insert(self, node: Optional[TreeNode[T]], value: T) -> TreeNode[T]:
        if node is None:
            return TreeNode(value)
        if value < node.value:
            node.left = self._insert(node.left, value)
        else:
            node.right = self._insert(node.right, value)
        return node

    def search(self, value: T) -> bool:
        """Check if value exists in BST"""
        return self._search(self.root, value)

    def _search(self, node: Optional[TreeNode[T]], value: T) -> bool:
        if node is None:
            return False
        if value == node.value:
            return True
        elif value < node.value:
            return self._search(node.left, value)
        else:
            return self._search(node.right, value)

    def find_min(self) -> Optional[T]:
        """Find minimum value"""
        if self.root is None:
            return None
        node = self.root
        while node.left:
            node = node.left
        return node.value

    def find_max(self) -> Optional[T]:
        """Find maximum value"""
        if self.root is None:
            return None
        node = self.root
        while node.right:
            node = node.right
        return node.value

    def delete(self, value: T) -> None:
        """Delete value from BST"""
        self.root = self._delete(self.root, value)

    def _delete(self, node: Optional[TreeNode[T]], value: T) -> Optional[TreeNode[T]]:
        if node is None:
            return None

        if value < node.value:
            node.left = self._delete(node.left, value)
        elif value > node.value:
            node.right = self._delete(node.right, value)
        else:
            # Node to delete found
            # Case 1: Leaf node
            if node.left is None and node.right is None:
                return None
            # Case 2: One child
            if node.left is None:
                return node.right
            if node.right is None:
                return node.left
            # Case 3: Two children - find in-order successor
            successor = node.right
            while successor.left:
                successor = successor.left
            node.value = successor.value
            node.right = self._delete(node.right, successor.value)

        return node

    def inorder(self) -> List[T]:
        """Return sorted elements"""
        return inorder(self.root)


print("\n=== Binary Search Tree ===")
bst = BST[int]()
for val in [5, 3, 7, 2, 4, 6, 8]:
    bst.insert(val)

print(f"BST in-order (sorted): {bst.inorder()}")
print(f"Search 4: {bst.search(4)}")
print(f"Search 9: {bst.search(9)}")
print(f"Min: {bst.find_min()}, Max: {bst.find_max()}")

bst.delete(5)  # Delete root
print(f"After deleting 5: {bst.inorder()}")


# =============================================================================
# TREE PROPERTIES
# =============================================================================

def height(node: Optional[TreeNode]) -> int:
    """Height of tree (longest path from root to leaf)"""
    if node is None:
        return -1  # Empty tree has height -1
    return 1 + max(height(node.left), height(node.right))


def size(node: Optional[TreeNode]) -> int:
    """Number of nodes in tree"""
    if node is None:
        return 0
    return 1 + size(node.left) + size(node.right)


def is_balanced(node: Optional[TreeNode]) -> bool:
    """Check if tree is height-balanced"""
    if node is None:
        return True

    left_height = height(node.left)
    right_height = height(node.right)

    if abs(left_height - right_height) > 1:
        return False

    return is_balanced(node.left) and is_balanced(node.right)


def is_bst(node: Optional[TreeNode], min_val=float('-inf'), max_val=float('inf')) -> bool:
    """Check if tree is a valid BST"""
    if node is None:
        return True

    if not (min_val < node.value < max_val):
        return False

    return (is_bst(node.left, min_val, node.value) and
            is_bst(node.right, node.value, max_val))


print("\n=== Tree Properties ===")
print(f"Height: {height(root)}")
print(f"Size: {size(root)}")
print(f"Is balanced: {is_balanced(root)}")
print(f"Is BST: {is_bst(bst.root)}")


# =============================================================================
# COMMON TREE PROBLEMS
# =============================================================================

def lowest_common_ancestor(root: TreeNode, p: TreeNode, q: TreeNode) -> Optional[TreeNode]:
    """Find LCA of two nodes"""
    if root is None or root == p or root == q:
        return root

    left = lowest_common_ancestor(root.left, p, q)
    right = lowest_common_ancestor(root.right, p, q)

    if left and right:
        return root
    return left if left else right


def path_sum(node: Optional[TreeNode[int]], target: int) -> bool:
    """Check if there's a root-to-leaf path with given sum"""
    if node is None:
        return False

    # Leaf node
    if node.left is None and node.right is None:
        return node.value == target

    remaining = target - node.value
    return path_sum(node.left, remaining) or path_sum(node.right, remaining)


def invert_tree(node: Optional[TreeNode[T]]) -> Optional[TreeNode[T]]:
    """Mirror/invert the tree"""
    if node is None:
        return None

    node.left, node.right = invert_tree(node.right), invert_tree(node.left)
    return node


def max_depth(node: Optional[TreeNode]) -> int:
    """Maximum depth of tree"""
    if node is None:
        return 0
    return 1 + max(max_depth(node.left), max_depth(node.right))


print("\n=== Common Tree Problems ===")
print(f"Max depth: {max_depth(root)}")
print(f"Path sum 7 exists: {path_sum(root, 7)}")  # 1 -> 2 -> 4


# =============================================================================
# TREE SERIALIZATION
# =============================================================================

def serialize(root: Optional[TreeNode]) -> str:
    """Serialize tree to string"""
    if root is None:
        return "null"
    return f"{root.value},{serialize(root.left)},{serialize(root.right)}"


def deserialize(data: str) -> Optional[TreeNode]:
    """Deserialize string to tree"""
    values = iter(data.split(","))

    def build():
        val = next(values)
        if val == "null":
            return None
        node = TreeNode(int(val))
        node.left = build()
        node.right = build()
        return node

    return build()


print("\n=== Serialization ===")
serialized = serialize(root)
print(f"Serialized: {serialized}")
restored = deserialize(serialized)
print(f"Restored in-order: {inorder(restored)}")


# =============================================================================
# EXPRESSION TREE
# =============================================================================

class ExprNode:
    """Node for expression tree"""

    def __init__(self, value: str):
        self.value = value
        self.left: Optional['ExprNode'] = None
        self.right: Optional['ExprNode'] = None


def evaluate(node: ExprNode) -> float:
    """Evaluate expression tree"""
    if node.left is None and node.right is None:
        return float(node.value)

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
    else:
        raise ValueError(f"Unknown operator: {node.value}")


# Build expression tree for: (3 + 4) * 2
#       *
#      / \
#     +   2
#    / \
#   3   4

print("\n=== Expression Tree ===")
expr_root = ExprNode('*')
expr_root.left = ExprNode('+')
expr_root.right = ExprNode('2')
expr_root.left.left = ExprNode('3')
expr_root.left.right = ExprNode('4')

print(f"(3 + 4) * 2 = {evaluate(expr_root)}")


# =============================================================================
# TRIE (Prefix Tree)
# =============================================================================

class TrieNode:
    def __init__(self):
        self.children: dict = {}
        self.is_end: bool = False


class Trie:
    """Prefix tree for string operations"""

    def __init__(self):
        self.root = TrieNode()

    def insert(self, word: str) -> None:
        node = self.root
        for char in word:
            if char not in node.children:
                node.children[char] = TrieNode()
            node = node.children[char]
        node.is_end = True

    def search(self, word: str) -> bool:
        node = self._find(word)
        return node is not None and node.is_end

    def starts_with(self, prefix: str) -> bool:
        return self._find(prefix) is not None

    def _find(self, prefix: str) -> Optional[TrieNode]:
        node = self.root
        for char in prefix:
            if char not in node.children:
                return None
            node = node.children[char]
        return node


print("\n=== Trie (Prefix Tree) ===")
trie = Trie()
words = ["apple", "app", "application", "banana", "ball"]
for word in words:
    trie.insert(word)

print(f"Search 'app': {trie.search('app')}")
print(f"Search 'appl': {trie.search('appl')}")
print(f"Starts with 'app': {trie.starts_with('app')}")
print(f"Starts with 'ban': {trie.starts_with('ban')}")


# =============================================================================
# COMPARISON
# =============================================================================

print("\n=== Tree Complexity Summary ===")
print("""
Binary Search Tree (BST):
  - Search, Insert, Delete: O(h)
  - h = log(n) balanced, h = n worst case

AVL Tree / Red-Black Tree:
  - Guaranteed O(log n) operations
  - Self-balancing

Trie:
  - Search, Insert: O(m) where m = string length
  - Prefix queries: O(m)
  - Space: Can be high

Heap:
  - Insert: O(log n)
  - Extract min/max: O(log n)
  - Peek: O(1)

Use Cases:
  - BST: Sorted data, range queries
  - Trie: Autocomplete, spell check
  - Heap: Priority queues, top-K
""")
