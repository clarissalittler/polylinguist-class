/**
 * Lesson 15: Trees - C++ Examples
 * Binary trees, BSTs, traversals, and common operations
 */

#include <iostream>
#include <vector>
#include <queue>
#include <stack>
#include <memory>
#include <functional>
#include <optional>
#include <unordered_map>
#include <string>
#include <algorithm>

using namespace std;

// =============================================================================
// BINARY TREE NODE
// =============================================================================

template<typename T>
struct TreeNode {
    T value;
    shared_ptr<TreeNode<T>> left;
    shared_ptr<TreeNode<T>> right;

    TreeNode(T val) : value(val), left(nullptr), right(nullptr) {}
};

template<typename T>
using NodePtr = shared_ptr<TreeNode<T>>;

// =============================================================================
// TREE TRAVERSALS
// =============================================================================

// In-order: Left, Node, Right (gives sorted order for BST)
template<typename T>
void inorder(NodePtr<T> node, vector<T>& result) {
    if (!node) return;
    inorder(node->left, result);
    result.push_back(node->value);
    inorder(node->right, result);
}

// Pre-order: Node, Left, Right
template<typename T>
void preorder(NodePtr<T> node, vector<T>& result) {
    if (!node) return;
    result.push_back(node->value);
    preorder(node->left, result);
    preorder(node->right, result);
}

// Post-order: Left, Right, Node
template<typename T>
void postorder(NodePtr<T> node, vector<T>& result) {
    if (!node) return;
    postorder(node->left, result);
    postorder(node->right, result);
    result.push_back(node->value);
}

// Level-order (BFS)
template<typename T>
vector<vector<T>> levelOrder(NodePtr<T> root) {
    vector<vector<T>> result;
    if (!root) return result;

    queue<NodePtr<T>> q;
    q.push(root);

    while (!q.empty()) {
        vector<T> level;
        int levelSize = q.size();

        for (int i = 0; i < levelSize; i++) {
            auto node = q.front();
            q.pop();
            level.push_back(node->value);

            if (node->left) q.push(node->left);
            if (node->right) q.push(node->right);
        }
        result.push_back(level);
    }
    return result;
}

// Iterative in-order using stack
template<typename T>
vector<T> inorderIterative(NodePtr<T> root) {
    vector<T> result;
    stack<NodePtr<T>> stk;
    auto current = root;

    while (current || !stk.empty()) {
        while (current) {
            stk.push(current);
            current = current->left;
        }
        current = stk.top();
        stk.pop();
        result.push_back(current->value);
        current = current->right;
    }
    return result;
}

void traversal_demo() {
    cout << "=== Tree Traversals ===" << endl;

    // Build tree:
    //       1
    //      / \
    //     2   3
    //    / \   \
    //   4   5   6
    auto root = make_shared<TreeNode<int>>(1);
    root->left = make_shared<TreeNode<int>>(2);
    root->right = make_shared<TreeNode<int>>(3);
    root->left->left = make_shared<TreeNode<int>>(4);
    root->left->right = make_shared<TreeNode<int>>(5);
    root->right->right = make_shared<TreeNode<int>>(6);

    vector<int> result;

    inorder(root, result);
    cout << "In-order: ";
    for (int x : result) cout << x << " ";
    cout << endl;

    result.clear();
    preorder(root, result);
    cout << "Pre-order: ";
    for (int x : result) cout << x << " ";
    cout << endl;

    result.clear();
    postorder(root, result);
    cout << "Post-order: ";
    for (int x : result) cout << x << " ";
    cout << endl;

    auto levels = levelOrder(root);
    cout << "Level-order: ";
    for (auto& level : levels) {
        cout << "[";
        for (int x : level) cout << x << " ";
        cout << "] ";
    }
    cout << endl;
}

// =============================================================================
// BINARY SEARCH TREE
// =============================================================================

template<typename T>
class BST {
private:
    NodePtr<T> root;

    NodePtr<T> insert(NodePtr<T> node, T value) {
        if (!node) return make_shared<TreeNode<T>>(value);
        if (value < node->value)
            node->left = insert(node->left, value);
        else
            node->right = insert(node->right, value);
        return node;
    }

    bool search(NodePtr<T> node, T value) {
        if (!node) return false;
        if (value == node->value) return true;
        if (value < node->value)
            return search(node->left, value);
        return search(node->right, value);
    }

    NodePtr<T> findMin(NodePtr<T> node) {
        while (node->left) node = node->left;
        return node;
    }

    NodePtr<T> deleteNode(NodePtr<T> node, T value) {
        if (!node) return nullptr;

        if (value < node->value) {
            node->left = deleteNode(node->left, value);
        } else if (value > node->value) {
            node->right = deleteNode(node->right, value);
        } else {
            // Found node to delete
            if (!node->left) return node->right;
            if (!node->right) return node->left;

            // Two children: replace with in-order successor
            auto successor = findMin(node->right);
            node->value = successor->value;
            node->right = deleteNode(node->right, successor->value);
        }
        return node;
    }

public:
    BST() : root(nullptr) {}

    void insert(T value) {
        root = insert(root, value);
    }

    bool search(T value) {
        return search(root, value);
    }

    void remove(T value) {
        root = deleteNode(root, value);
    }

    optional<T> findMin() {
        if (!root) return nullopt;
        auto node = findMin(root);
        return node->value;
    }

    optional<T> findMax() {
        if (!root) return nullopt;
        auto node = root;
        while (node->right) node = node->right;
        return node->value;
    }

    vector<T> inorder() {
        vector<T> result;
        ::inorder(root, result);
        return result;
    }
};

void bst_demo() {
    cout << "\n=== Binary Search Tree ===" << endl;

    BST<int> bst;
    for (int val : {5, 3, 7, 2, 4, 6, 8}) {
        bst.insert(val);
    }

    auto result = bst.inorder();
    cout << "In-order (sorted): ";
    for (int x : result) cout << x << " ";
    cout << endl;

    cout << "Search 4: " << boolalpha << bst.search(4) << endl;
    cout << "Search 9: " << bst.search(9) << endl;

    if (auto m = bst.findMin()) cout << "Min: " << *m << endl;
    if (auto m = bst.findMax()) cout << "Max: " << *m << endl;

    bst.remove(5);
    result = bst.inorder();
    cout << "After removing 5: ";
    for (int x : result) cout << x << " ";
    cout << endl;
}

// =============================================================================
// TREE PROPERTIES
// =============================================================================

template<typename T>
int height(NodePtr<T> node) {
    if (!node) return -1;
    return 1 + max(height(node->left), height(node->right));
}

template<typename T>
int size(NodePtr<T> node) {
    if (!node) return 0;
    return 1 + size(node->left) + size(node->right);
}

template<typename T>
bool isBalanced(NodePtr<T> node) {
    if (!node) return true;
    int leftHeight = height(node->left);
    int rightHeight = height(node->right);
    return abs(leftHeight - rightHeight) <= 1 &&
           isBalanced(node->left) && isBalanced(node->right);
}

template<typename T>
bool isBST(NodePtr<T> node, T* minVal = nullptr, T* maxVal = nullptr) {
    if (!node) return true;
    if (minVal && node->value <= *minVal) return false;
    if (maxVal && node->value >= *maxVal) return false;
    return isBST(node->left, minVal, &node->value) &&
           isBST(node->right, &node->value, maxVal);
}

void properties_demo() {
    cout << "\n=== Tree Properties ===" << endl;

    auto root = make_shared<TreeNode<int>>(1);
    root->left = make_shared<TreeNode<int>>(2);
    root->right = make_shared<TreeNode<int>>(3);
    root->left->left = make_shared<TreeNode<int>>(4);
    root->left->right = make_shared<TreeNode<int>>(5);

    cout << "Height: " << height(root) << endl;
    cout << "Size: " << size(root) << endl;
    cout << "Is balanced: " << boolalpha << isBalanced(root) << endl;
}

// =============================================================================
// COMMON TREE PROBLEMS
// =============================================================================

template<typename T>
NodePtr<T> invertTree(NodePtr<T> node) {
    if (!node) return nullptr;
    swap(node->left, node->right);
    invertTree(node->left);
    invertTree(node->right);
    return node;
}

bool hasPathSum(NodePtr<int> node, int target) {
    if (!node) return false;
    if (!node->left && !node->right) return node->value == target;
    int remaining = target - node->value;
    return hasPathSum(node->left, remaining) || hasPathSum(node->right, remaining);
}

template<typename T>
int maxDepth(NodePtr<T> node) {
    if (!node) return 0;
    return 1 + max(maxDepth(node->left), maxDepth(node->right));
}

void problems_demo() {
    cout << "\n=== Common Tree Problems ===" << endl;

    auto root = make_shared<TreeNode<int>>(1);
    root->left = make_shared<TreeNode<int>>(2);
    root->right = make_shared<TreeNode<int>>(3);
    root->left->left = make_shared<TreeNode<int>>(4);

    cout << "Max depth: " << maxDepth(root) << endl;
    cout << "Has path sum 7 (1->2->4): " << boolalpha << hasPathSum(root, 7) << endl;
    cout << "Has path sum 5: " << hasPathSum(root, 5) << endl;
}

// =============================================================================
// TRIE (Prefix Tree)
// =============================================================================

class Trie {
private:
    struct TrieNode {
        unordered_map<char, unique_ptr<TrieNode>> children;
        bool isEnd = false;
    };

    unique_ptr<TrieNode> root;

public:
    Trie() : root(make_unique<TrieNode>()) {}

    void insert(const string& word) {
        TrieNode* node = root.get();
        for (char c : word) {
            if (node->children.find(c) == node->children.end()) {
                node->children[c] = make_unique<TrieNode>();
            }
            node = node->children[c].get();
        }
        node->isEnd = true;
    }

    bool search(const string& word) {
        TrieNode* node = find(word);
        return node && node->isEnd;
    }

    bool startsWith(const string& prefix) {
        return find(prefix) != nullptr;
    }

private:
    TrieNode* find(const string& prefix) {
        TrieNode* node = root.get();
        for (char c : prefix) {
            if (node->children.find(c) == node->children.end()) {
                return nullptr;
            }
            node = node->children[c].get();
        }
        return node;
    }
};

void trie_demo() {
    cout << "\n=== Trie (Prefix Tree) ===" << endl;

    Trie trie;
    for (const string& word : {"apple", "app", "application", "banana"}) {
        trie.insert(word);
    }

    cout << "Search 'app': " << boolalpha << trie.search("app") << endl;
    cout << "Search 'appl': " << trie.search("appl") << endl;
    cout << "Starts with 'app': " << trie.startsWith("app") << endl;
    cout << "Starts with 'ban': " << trie.startsWith("ban") << endl;
}

// =============================================================================
// HEAP (Priority Queue underlying structure)
// =============================================================================

void heap_demo() {
    cout << "\n=== Heap / Priority Queue ===" << endl;

    // Max heap using priority_queue
    priority_queue<int> maxHeap;
    for (int x : {3, 1, 4, 1, 5, 9, 2, 6}) {
        maxHeap.push(x);
    }

    cout << "Max heap extraction: ";
    while (!maxHeap.empty()) {
        cout << maxHeap.top() << " ";
        maxHeap.pop();
    }
    cout << endl;

    // Min heap
    priority_queue<int, vector<int>, greater<int>> minHeap;
    for (int x : {3, 1, 4, 1, 5, 9, 2, 6}) {
        minHeap.push(x);
    }

    cout << "Min heap extraction: ";
    while (!minHeap.empty()) {
        cout << minHeap.top() << " ";
        minHeap.pop();
    }
    cout << endl;
}

// =============================================================================
// MAIN
// =============================================================================

int main() {
    traversal_demo();
    bst_demo();
    properties_demo();
    problems_demo();
    trie_demo();
    heap_demo();

    cout << "\n=== Tree Complexity Summary ===" << endl;
    cout << R"(
BST (unbalanced):
  - Search, Insert, Delete: O(h), h can be O(n)

Balanced BST (AVL, Red-Black):
  - All operations: O(log n) guaranteed

Trie:
  - Insert, Search: O(m) where m = string length

Heap (std::priority_queue):
  - Insert: O(log n)
  - Extract top: O(log n)
  - Peek top: O(1)

std::set / std::map (Red-Black tree):
  - Insert, Search, Delete: O(log n)
)" << endl;

    return 0;
}
