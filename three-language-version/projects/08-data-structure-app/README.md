# Project 8: Data Structure Application

**Quarter 3, Weeks 5-7**
**Prerequisites:** Lessons 1-16 (through Graphs)

---

## The Big Idea

Build an application that **showcases a non-trivial data structure**. Trees, graphs, heaps, or custom structures—pick one and build something that makes its power visible.

Data structures aren't just abstract concepts. They're tools that make certain problems elegant to solve.

---

## Base Requirements

Your application must:

1. **Use a non-trivial data structure** (tree, graph, heap, trie, etc.)
2. **Implement core operations** (insert, delete, search, traverse)
3. **Solve a real problem** that benefits from the structure
4. **Visualize or demonstrate** the structure in action
5. **Handle edge cases** correctly

### Core Features
- Working implementation of your chosen data structure
- At least 4 core operations implemented
- Application that uses the structure meaningfully
- Way to see/understand the structure's state

---

## Data Structure Options

### Trees
- **Binary Search Tree**: Ordered data, fast lookup
- **AVL/Red-Black Tree**: Self-balancing BST
- **B-Tree**: Database-style multi-way tree
- **Heap**: Priority queue operations
- **Trie**: Prefix-based string storage
- **Segment Tree**: Range queries
- **Merkle Tree**: Data verification

### Graphs
- **Adjacency List/Matrix**: Graph representation
- **Directed Acyclic Graph**: Dependencies, scheduling
- **Weighted Graph**: Shortest paths, networks

### Other
- **Skip List**: Probabilistic alternative to balanced trees
- **Bloom Filter**: Probabilistic membership testing
- **Union-Find**: Disjoint set operations
- **LRU Cache**: Least-recently-used eviction

---

## Application Ideas

### Tree Applications
- **File system navigator**: Directory tree exploration
- **Expression evaluator**: Parse tree for math expressions
- **Decision tree**: Interactive Q&A that narrows down
- **Family tree**: Genealogy viewer
- **Organization chart**: Hierarchical structure viewer
- **Tournament bracket**: Sports/competition structure
- **Code outline**: AST-like representation of code

### Graph Applications
- **Social network**: Friends, connections, suggestions
- **Route planner**: Maps, shortest paths
- **Dependency resolver**: Package manager style
- **Recommendation engine**: Graph-based suggestions
- **Network visualizer**: Show connected systems
- **Workflow engine**: Task dependencies
- **Knowledge graph**: Concept relationships

### Heap Applications
- **Task scheduler**: Priority-based execution
- **Event system**: Time-ordered events
- **Top-K finder**: Streaming top results
- **Merge K lists**: Efficient multi-way merge

### Other Applications
- **Autocomplete**: Trie-based suggestions
- **Spell checker**: Edit distance with tries
- **Rate limiter**: Sliding window with appropriate structure
- **Undo system**: Command pattern with appropriate storage

---

## Example: Route Planner (Graph)

```
═══════════════════════════════════════════════════════════════
              CAMPUS ROUTE PLANNER
═══════════════════════════════════════════════════════════════

CAMPUS MAP (Graph: 12 nodes, 18 edges):

     Library ──5── Science ──3── Engineering
        │           │              │
        4           2              6
        │           │              │
    Student ──2── Quad ────4──── CS Building
     Center        │              │
        │          3              2
        5          │              │
        │       Dining ────3──── Parking
     Dorms ────6─────┘

COMMANDS: find [from] [to], show, add, remove, help

> find library cs_building

FINDING SHORTEST PATH: Library → CS Building
════════════════════════════════════════════

Using Dijkstra's Algorithm...

Step 1: Start at Library (distance: 0)
Step 2: Explore Student Center (distance: 4)
Step 3: Explore Science (distance: 5)
Step 4: Explore Quad (distance: 6)
Step 5: Explore CS Building (distance: 10)

SHORTEST PATH FOUND:
  Library → Science → Quad → CS Building
  Total distance: 10 units
  Path length: 4 nodes

ALTERNATIVE PATHS:
  • Library → Student → Quad → CS: 10 units
  • Library → Science → Engineering → CS: 14 units

VISUALIZATION:
  [Library]──5──>[Science]──2──>[Quad]──4──>[CS Building]

> show graph-stats

GRAPH STATISTICS
════════════════
  Nodes: 12
  Edges: 18
  Density: 0.27
  Average degree: 3.0
  Connected: Yes
  Diameter: 15 (Dorms to Engineering)

Most central node: Quad (betweenness: 0.42)
```

---

## Example: Decision Tree

```
═══════════════════════════════════════════════════════════════
              TECH SUPPORT DECISION TREE
═══════════════════════════════════════════════════════════════

CURRENT TREE:
                    [Is it plugged in?]
                    /                \
                  Yes                 No
                  /                    \
        [Is it turned on?]        [Plug it in]
        /              \               |
      Yes              No          [Fixed!]
      /                 \
[Check connections]  [Turn it on]
     |                   |
  [Restart?]         [Fixed!]
   /      \
 Yes       No
  |         |
[Restart] [Call IT]

TREE STATS:
  Depth: 4
  Nodes: 11
  Leaves (solutions): 5
  Average path: 2.8 questions

MODE: Interactive diagnosis

> start

Question 1: Is it plugged in?
[y]es / [n]o > y

Question 2: Is it turned on?
[y]es / [n]o > y

Question 3: Check connections - are all cables secure?
[y]es / [n]o > y

Question 4: Should we try restarting?
[y]es / [n]o > n

DIAGNOSIS COMPLETE
══════════════════
Solution: Call IT
Path taken: Plugged → On → Connected → No restart
Questions asked: 4

> add-node "Check for error messages" after "Check connections" yes
Node added successfully.

> visualize

[Updated tree visualization...]
```

---

## Technical Requirements

### Data Structure Implementation
- Implement the structure yourself (not library)
- Core operations must work correctly
- Handle edge cases (empty, single element, etc.)
- Appropriate time complexity

### Visualization
- Show the structure's state in some form
- Text-based is fine (ASCII art trees, adjacency lists)
- Should help understand what the structure looks like

### Application
- The structure should solve a real problem
- Not just "insert and display" but actual use case
- Interactive or batch processing

### Testing
- Verify operations work correctly
- Test edge cases
- Performance on reasonable inputs

---

## Creative Extensions

### Visualization (+5 each)
- **ASCII art**: Beautiful text representations
- **Step-by-step**: Show operations animating
- **Statistics**: Display structure properties
- **Comparison**: Show before/after operations

### Advanced Operations (+5 each)
- **Balancing**: Self-balancing trees
- **Persistence**: Save/load structures
- **Serialization**: Import/export formats
- **Batch operations**: Efficient bulk changes

### Analysis (+5 each)
- **Complexity verification**: Measure actual vs theoretical
- **Space tracking**: Memory usage
- **Operation counting**: Comparisons, swaps
- **Degradation testing**: Worst-case inputs

### Prolog Bonus (+10)
- Implement core structure logic in Prolog
- Use logical rules for tree/graph relationships
- Reflect on declarative vs imperative approaches

---

## Getting Started

### Step 1: Choose Your Structure
What data structure interests you? What problem does it solve well?

### Step 2: Implement Core Operations
Get basic insert, delete, search, and traversal working.

### Step 3: Build Visualization
Create a way to see/understand the structure.

### Step 4: Create Application
Build something useful around the structure.

### Step 5: Test Thoroughly
Edge cases, performance, correctness.

---

## Language Hints

### Python
```python
class BinarySearchTree:
    class Node:
        def __init__(self, key, value):
            self.key = key
            self.value = value
            self.left = None
            self.right = None

    def __init__(self):
        self.root = None

    def insert(self, key, value):
        if not self.root:
            self.root = self.Node(key, value)
        else:
            self._insert_recursive(self.root, key, value)

    def _insert_recursive(self, node, key, value):
        if key < node.key:
            if node.left is None:
                node.left = self.Node(key, value)
            else:
                self._insert_recursive(node.left, key, value)
        else:
            if node.right is None:
                node.right = self.Node(key, value)
            else:
                self._insert_recursive(node.right, key, value)

    def visualize(self, node=None, prefix="", is_left=True):
        """ASCII tree visualization"""
        if node is None:
            node = self.root
        if node is not None:
            print(prefix + ("├── " if is_left else "└── ") + str(node.key))
            if node.left or node.right:
                if node.left:
                    self.visualize(node.left, prefix + ("│   " if is_left else "    "), True)
                if node.right:
                    self.visualize(node.right, prefix + ("│   " if is_left else "    "), False)
```

### C++
```cpp
template<typename K, typename V>
class Graph {
    std::unordered_map<K, std::vector<std::pair<K, V>>> adjacency;

public:
    void addEdge(K from, K to, V weight) {
        adjacency[from].push_back({to, weight});
    }

    std::vector<K> shortestPath(K start, K end) {
        // Dijkstra's algorithm
        std::unordered_map<K, V> distances;
        std::unordered_map<K, K> previous;
        std::priority_queue<std::pair<V, K>,
                          std::vector<std::pair<V, K>>,
                          std::greater<>> pq;

        // ... implementation
    }

    void visualize() {
        for (const auto& [node, edges] : adjacency) {
            std::cout << node << " -> ";
            for (const auto& [neighbor, weight] : edges) {
                std::cout << neighbor << "(" << weight << ") ";
            }
            std::cout << "\n";
        }
    }
};
```

### Haskell
```haskell
data Tree a = Empty
            | Node a (Tree a) (Tree a)
            deriving (Show, Eq)

insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x (Node y left right)
    | x < y     = Node y (insert x left) right
    | otherwise = Node y left (insert x right)

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node x left right) = inOrder left ++ [x] ++ inOrder right

visualize :: Show a => Tree a -> String
visualize = go "" ""
  where
    go _ _ Empty = ""
    go prefix childPrefix (Node x left right) =
        prefix ++ show x ++ "\n" ++
        go (childPrefix ++ "├── ") (childPrefix ++ "│   ") left ++
        go (childPrefix ++ "└── ") (childPrefix ++ "    ") right
```

---

## Reflection Questions

Answer in your `REFLECTION.md`:

1. **What structure did you choose and why?** What makes it suited to your application?

2. **What operations did you implement?** What's the complexity of each?

3. **What was hardest to implement correctly?** Edge cases? Balancing? Visualization?

4. **How did implementing it yourself deepen understanding?** vs using a library?

5. **What would a production version need?** What's missing from yours?

6. **How did the structure's properties show up in practice?**

---

## Submission Checklist

- [ ] Data structure implemented from scratch
- [ ] Core operations (insert, delete, search, traverse) work
- [ ] Application uses structure meaningfully
- [ ] Visualization shows structure state
- [ ] Edge cases handled correctly
- [ ] Git history shows development
- [ ] REFLECTION.md completed
- [ ] Ready for showcase!

---

## Showcase Presentation

5-7 minutes:
1. **The structure** (1-2 min) - What is it? What's it good for?
2. **The application** (2-3 min) - Demo what you built, show the structure in action
3. **Inside the implementation** (1-2 min) - Key operations, interesting code
4. **What you learned** (1 min) - Insights about the structure

---

**Pick a structure. Build it. Use it. Understand it deeply!**
