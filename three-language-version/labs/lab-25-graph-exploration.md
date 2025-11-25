# Lab 25: Graph Exploration

**Quarter 3, Week 5**
**Duration:** 90 minutes
**Format:** Pair programming

## Overview

Graphs model relationships: social networks, maps, dependencies, and more. This lab builds graphs from scratch and implements traversal algorithms.

## Objectives

By the end of this lab, you will:
- [ ] Represent graphs using adjacency lists
- [ ] Implement BFS (Breadth-First Search)
- [ ] Implement DFS (Depth-First Search)
- [ ] Find paths between nodes

## Setup

- Partner up
- Create folder: `lab25-graphs/`
- Files: `graph.py`, `graph.hs`

---

## Part 1: Graph Basics (20 minutes)

### Activity 1.1: What is a Graph?

A graph consists of:
- **Vertices (nodes)**: The things
- **Edges**: Connections between things

```
Social Network:
    Alice --- Bob
      |        |
      +-- Charlie --+
            |       |
          Diana -- Eve

Adjacency List:
Alice: [Bob, Charlie]
Bob: [Alice, Charlie]
Charlie: [Alice, Bob, Diana, Eve]
Diana: [Charlie, Eve]
Eve: [Charlie, Diana]
```

### Activity 1.2: Graph Types

| Type | Description | Example |
|------|-------------|---------|
| **Undirected** | Edges go both ways | Friendships |
| **Directed** | Edges have direction | Twitter follows |
| **Weighted** | Edges have values | Road distances |
| **Unweighted** | All edges equal | Social connections |

### Activity 1.3: Graph Implementation

```python
class Graph:
    """Undirected graph using adjacency list."""

    def __init__(self):
        self._adj = {}  # vertex -> list of neighbors

    def add_vertex(self, v):
        """Add a vertex to the graph."""
        if v not in self._adj:
            self._adj[v] = []

    def add_edge(self, v1, v2):
        """Add an undirected edge between v1 and v2."""
        self.add_vertex(v1)
        self.add_vertex(v2)
        if v2 not in self._adj[v1]:
            self._adj[v1].append(v2)
        if v1 not in self._adj[v2]:
            self._adj[v2].append(v1)

    def neighbors(self, v):
        """Get all neighbors of vertex v."""
        return self._adj.get(v, [])

    def vertices(self):
        """Get all vertices."""
        return list(self._adj.keys())

    def has_edge(self, v1, v2):
        """Check if edge exists between v1 and v2."""
        return v2 in self._adj.get(v1, [])

    def __repr__(self):
        lines = []
        for v, neighbors in self._adj.items():
            lines.append(f"{v}: {neighbors}")
        return "\n".join(lines)

# Build the social network
g = Graph()
g.add_edge("Alice", "Bob")
g.add_edge("Alice", "Charlie")
g.add_edge("Bob", "Charlie")
g.add_edge("Charlie", "Diana")
g.add_edge("Charlie", "Eve")
g.add_edge("Diana", "Eve")

print(g)
print(f"\nAlice's friends: {g.neighbors('Alice')}")
```

### Activity 1.4: Haskell Graph

```haskell
import qualified Data.Map as Map
import qualified Data.Set as Set

type Graph a = Map.Map a (Set.Set a)

emptyGraph :: Graph a
emptyGraph = Map.empty

addVertex :: Ord a => a -> Graph a -> Graph a
addVertex v g = Map.insertWith Set.union v Set.empty g

addEdge :: Ord a => a -> a -> Graph a -> Graph a
addEdge v1 v2 g =
    let g' = addVertex v1 (addVertex v2 g)
    in Map.adjust (Set.insert v2) v1 $
       Map.adjust (Set.insert v1) v2 g'

neighbors :: Ord a => a -> Graph a -> [a]
neighbors v g = Set.toList $ Map.findWithDefault Set.empty v g

main :: IO ()
main = do
    let g = addEdge "Alice" "Bob" $
            addEdge "Alice" "Charlie" $
            addEdge "Bob" "Charlie" emptyGraph
    print g
    print $ neighbors "Alice" g  -- ["Bob","Charlie"]
```

### ✅ Checkpoint 1

Verify:
- [ ] Graph class implemented
- [ ] Can add vertices and edges
- [ ] Can query neighbors

---

## Part 2: Breadth-First Search (25 minutes)

### Activity 2.1: BFS Concept

BFS explores level by level, like ripples in a pond:

```
Start at Alice:

Level 0: Alice
Level 1: Bob, Charlie (Alice's neighbors)
Level 2: Diana, Eve (neighbors of level 1)

         Alice           <- Level 0
        /     \
      Bob    Charlie     <- Level 1
              /    \
           Diana   Eve   <- Level 2
```

**Uses a queue** to process nodes in order discovered.

### Activity 2.2: BFS Implementation

```python
from collections import deque

def bfs(graph, start):
    """
    Breadth-first search from start vertex.
    Returns list of vertices in BFS order.
    """
    visited = set()
    order = []
    queue = deque([start])
    visited.add(start)

    while queue:
        vertex = queue.popleft()
        order.append(vertex)

        for neighbor in graph.neighbors(vertex):
            if neighbor not in visited:
                visited.add(neighbor)
                queue.append(neighbor)

    return order

# Test
print("BFS from Alice:", bfs(g, "Alice"))
# ['Alice', 'Bob', 'Charlie', 'Diana', 'Eve']
```

### Activity 2.3: BFS with Levels

```python
def bfs_levels(graph, start):
    """
    BFS that tracks distance from start.
    Returns dict of vertex -> distance.
    """
    distances = {start: 0}
    queue = deque([start])

    while queue:
        vertex = queue.popleft()
        current_dist = distances[vertex]

        for neighbor in graph.neighbors(vertex):
            if neighbor not in distances:
                distances[neighbor] = current_dist + 1
                queue.append(neighbor)

    return distances

# Test
distances = bfs_levels(g, "Alice")
for vertex, dist in sorted(distances.items(), key=lambda x: x[1]):
    print(f"{vertex}: {dist} hops from Alice")
```

### Activity 2.4: Shortest Path

```python
def shortest_path(graph, start, end):
    """
    Find shortest path between start and end.
    Returns the path as a list of vertices.
    """
    if start == end:
        return [start]

    visited = {start}
    queue = deque([(start, [start])])  # (vertex, path)

    while queue:
        vertex, path = queue.popleft()

        for neighbor in graph.neighbors(vertex):
            if neighbor == end:
                return path + [neighbor]

            if neighbor not in visited:
                visited.add(neighbor)
                queue.append((neighbor, path + [neighbor]))

    return None  # No path exists

# Test
path = shortest_path(g, "Alice", "Eve")
print(f"Shortest path Alice -> Eve: {path}")
# ['Alice', 'Charlie', 'Eve']
```

### ✅ Checkpoint 2

Verify:
- [ ] BFS visits nodes level by level
- [ ] Can find distances from start
- [ ] Shortest path works

---

## Part 3: Depth-First Search (25 minutes)

### Activity 3.1: DFS Concept

DFS explores as deep as possible before backtracking:

```
Start at Alice, go deep:
Alice -> Bob -> Charlie -> Diana -> Eve
                      (backtrack)

         Alice
        /     \
      Bob    Charlie     <- backtrack here
              /    \
           Diana   Eve   <- go deep

Order: Alice, Bob, Charlie, Diana, Eve
(depends on neighbor order)
```

**Uses a stack** (or recursion) to track path.

### Activity 3.2: DFS Implementation (Recursive)

```python
def dfs_recursive(graph, start, visited=None):
    """
    Depth-first search using recursion.
    Returns list of vertices in DFS order.
    """
    if visited is None:
        visited = set()

    visited.add(start)
    order = [start]

    for neighbor in graph.neighbors(start):
        if neighbor not in visited:
            order.extend(dfs_recursive(graph, neighbor, visited))

    return order

# Test
print("DFS from Alice:", dfs_recursive(g, "Alice"))
```

### Activity 3.3: DFS Implementation (Iterative)

```python
def dfs_iterative(graph, start):
    """
    Depth-first search using explicit stack.
    """
    visited = set()
    order = []
    stack = [start]

    while stack:
        vertex = stack.pop()

        if vertex not in visited:
            visited.add(vertex)
            order.append(vertex)

            # Add neighbors in reverse order to match recursive behavior
            for neighbor in reversed(graph.neighbors(vertex)):
                if neighbor not in visited:
                    stack.append(neighbor)

    return order

# Test
print("DFS iterative:", dfs_iterative(g, "Alice"))
```

### Activity 3.4: Finding All Paths

```python
def find_all_paths(graph, start, end, path=None):
    """
    Find all paths from start to end.
    """
    if path is None:
        path = []

    path = path + [start]

    if start == end:
        return [path]

    paths = []
    for neighbor in graph.neighbors(start):
        if neighbor not in path:  # Avoid cycles
            new_paths = find_all_paths(graph, neighbor, end, path)
            paths.extend(new_paths)

    return paths

# Test
all_paths = find_all_paths(g, "Alice", "Eve")
print("All paths from Alice to Eve:")
for p in all_paths:
    print(f"  {' -> '.join(p)}")
```

### Activity 3.5: Haskell DFS

```haskell
import Data.Set (Set)
import qualified Data.Set as Set

dfs :: Ord a => Graph a -> a -> [a]
dfs graph start = dfs' Set.empty [start]
  where
    dfs' _ [] = []
    dfs' visited (v:vs)
        | v `Set.member` visited = dfs' visited vs
        | otherwise = v : dfs' (Set.insert v visited)
                              (neighbors v graph ++ vs)

main :: IO ()
main = do
    let g = addEdge "A" "B" $ addEdge "A" "C" $
            addEdge "B" "D" $ addEdge "C" "D" emptyGraph
    print $ dfs g "A"  -- ["A","B","D","C"] or similar
```

### ✅ Checkpoint 3

Verify:
- [ ] DFS explores deep before backtracking
- [ ] Both recursive and iterative work
- [ ] Can find all paths

---

## Part 4: Applications (15 minutes)

### Activity 4.1: Connected Components

```python
def connected_components(graph):
    """Find all connected components in the graph."""
    visited = set()
    components = []

    for vertex in graph.vertices():
        if vertex not in visited:
            # BFS/DFS from this vertex finds its component
            component = bfs(graph, vertex)
            components.append(component)
            visited.update(component)

    return components

# Test with disconnected graph
g2 = Graph()
g2.add_edge("A", "B")
g2.add_edge("B", "C")
g2.add_edge("X", "Y")  # Separate component

print("Components:", connected_components(g2))
# [['A', 'B', 'C'], ['X', 'Y']]
```

### Activity 4.2: Cycle Detection

```python
def has_cycle(graph):
    """
    Check if undirected graph has a cycle.
    """
    visited = set()

    def dfs(vertex, parent):
        visited.add(vertex)

        for neighbor in graph.neighbors(vertex):
            if neighbor not in visited:
                if dfs(neighbor, vertex):
                    return True
            elif neighbor != parent:
                # Found a back edge - cycle!
                return True

        return False

    # Check all components
    for vertex in graph.vertices():
        if vertex not in visited:
            if dfs(vertex, None):
                return True

    return False

# Test
print(f"Original graph has cycle: {has_cycle(g)}")  # True
```

### Activity 4.3: Word Ladder

Find shortest transformation from one word to another:

```python
def word_ladder(start, end, word_list):
    """
    Find shortest transformation sequence from start to end.
    Each step changes exactly one letter.
    """
    def neighbors(word):
        """Words that differ by one letter."""
        result = []
        for i in range(len(word)):
            for c in 'abcdefghijklmnopqrstuvwxyz':
                new_word = word[:i] + c + word[i+1:]
                if new_word != word and new_word in word_set:
                    result.append(new_word)
        return result

    word_set = set(word_list)
    if end not in word_set:
        return None

    # BFS
    queue = deque([(start, [start])])
    visited = {start}

    while queue:
        word, path = queue.popleft()

        for next_word in neighbors(word):
            if next_word == end:
                return path + [next_word]

            if next_word not in visited:
                visited.add(next_word)
                queue.append((next_word, path + [next_word]))

    return None

# Test
words = ["hot", "dot", "dog", "lot", "log", "cog"]
path = word_ladder("hit", "cog", words)
print(f"Word ladder: {path}")
# ['hit', 'hot', 'dot', 'dog', 'cog']
```

---

## Part 5: Directed Graphs (5 minutes)

### Activity 5.1: Directed Graph

```python
class DiGraph:
    """Directed graph."""

    def __init__(self):
        self._adj = {}

    def add_edge(self, source, target):
        """Add directed edge from source to target."""
        if source not in self._adj:
            self._adj[source] = []
        if target not in self._adj:
            self._adj[target] = []
        self._adj[source].append(target)

    def neighbors(self, v):
        """Get outgoing neighbors."""
        return self._adj.get(v, [])

# Example: course prerequisites
courses = DiGraph()
courses.add_edge("CS101", "CS201")
courses.add_edge("CS101", "CS202")
courses.add_edge("CS201", "CS301")
courses.add_edge("CS202", "CS301")
courses.add_edge("CS301", "CS401")
```

---

## Challenges

### Challenge 1: Topological Sort

Given a directed acyclic graph (DAG), produce an ordering where each node comes after all its dependencies.

### Challenge 2: Dijkstra's Algorithm

Implement shortest path for weighted graphs.

### Challenge 3: Social Network Analysis

Using a graph of friendships:
- Find the "most connected" person
- Find mutual friends between two people
- Suggest friends (friends of friends)

---

## Wrap-Up

**Key takeaways:**

1. **Graphs** model relationships between things
2. **BFS** explores level by level (shortest path)
3. **DFS** explores deep first (all paths, cycles)
4. **Adjacency list** is efficient for sparse graphs

**BFS vs DFS:**
| BFS | DFS |
|-----|-----|
| Uses queue | Uses stack |
| Level by level | Deep first |
| Shortest path | All paths |
| More memory | Less memory |

**Next lab:** Problem Solving Workshop - dynamic programming!
