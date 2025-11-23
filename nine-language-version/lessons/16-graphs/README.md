# Lesson 16: Graphs and Graph Algorithms

## Introduction

A **graph** is a data structure consisting of **nodes (vertices)** connected by **edges**. Graphs are used to model relationships, networks, and connections between entities.

Unlike trees (which have a hierarchical structure), graphs can have:
- Cycles
- Multiple paths between nodes
- Disconnected components
- Bidirectional or unidirectional relationships

## Why Graphs Matter

Graphs are everywhere in computer science and the real world:
- **Social networks**: Friends, followers, connections
- **Maps and navigation**: Cities connected by roads
- **Web pages**: Links between websites (Google PageRank)
- **Computer networks**: Routers and connections
- **Recommendation systems**: Products, movies, music
- **Dependency resolution**: Package managers, build systems
- **Circuit design**: Electronic components and connections

---

## Part 1: Graph Terminology

### Basic Concepts

```
     A ---- B
     |      |
     |      |
     C ---- D ---- E
```

**Key Terms:**
- **Vertex (Node)**: A point in the graph (A, B, C, D, E)
- **Edge**: A connection between two vertices
- **Adjacent**: Two vertices connected by an edge (A and B are adjacent)
- **Degree**: Number of edges connected to a vertex
  - Degree of A = 2 (connected to B and C)
  - Degree of D = 3 (connected to B, C, and E)
- **Path**: A sequence of vertices where each adjacent pair is connected
  - Path from A to E: A → B → D → E
- **Cycle**: A path that starts and ends at the same vertex
  - A → B → D → C → A

### Types of Graphs

**1. Undirected Graph**
- Edges have no direction
- If A connects to B, then B connects to A
- Example: Facebook friendships

```
     A ---- B
     |      |
     C ---- D
```

**2. Directed Graph (Digraph)**
- Edges have direction (arrows)
- A → B doesn't mean B → A
- Example: Twitter follows, web page links

```
     A --→ B
     ↑      ↓
     C ←-- D
```

**3. Weighted Graph**
- Edges have weights (costs, distances, times)
- Example: Road networks with distances

```
        5
     A --- B
     |     |
   2 |     | 3
     |     |
     C --- D
        7
```

**4. Cyclic vs Acyclic**
- **Cyclic**: Contains at least one cycle
- **Acyclic**: No cycles (DAG = Directed Acyclic Graph)

```
Cyclic:        Acyclic (DAG):
  A → B           A → B
  ↓   ↓           ↓   ↓
  C ← D           C   D
```

**5. Connected vs Disconnected**
- **Connected**: Path exists between every pair of vertices
- **Disconnected**: Some vertices are unreachable

```
Connected:     Disconnected:
  A --- B         A --- B    E
  |     |                    |
  C --- D         C --- D    F
```

**6. Complete Graph**
- Every vertex is connected to every other vertex
- n vertices → n(n-1)/2 edges

```
Complete graph with 4 vertices:
     A ---- B
     |\    /|
     | \  / |
     |  \/  |
     |  /\  |
     | /  \ |
     |/    \|
     C ---- D
```

---

## Part 2: Graph Representation

### 1. Adjacency Matrix

A 2D array where `matrix[i][j] = 1` if there's an edge from vertex i to vertex j.

**Example:**
```
Graph:    0 → 1
          ↓   ↓
          2 → 3

Adjacency Matrix:
     0  1  2  3
  0 [0, 1, 1, 0]
  1 [0, 0, 0, 1]
  2 [0, 0, 0, 1]
  3 [0, 0, 0, 0]
```

**Python Implementation:**
```python
class GraphMatrix:
    def __init__(self, num_vertices):
        self.V = num_vertices
        self.matrix = [[0] * num_vertices for _ in range(num_vertices)]

    def add_edge(self, u, v):
        """Add directed edge from u to v."""
        self.matrix[u][v] = 1

    def add_edge_undirected(self, u, v):
        """Add undirected edge between u and v."""
        self.matrix[u][v] = 1
        self.matrix[v][u] = 1

    def has_edge(self, u, v):
        """Check if edge exists from u to v."""
        return self.matrix[u][v] == 1

    def get_neighbors(self, u):
        """Get all neighbors of vertex u."""
        return [v for v in range(self.V) if self.matrix[u][v] == 1]

    def print_graph(self):
        for row in self.matrix:
            print(row)
```

**Pros:**
- Fast edge lookup: O(1)
- Simple implementation
- Good for dense graphs

**Cons:**
- Space: O(V²) - wastes space for sparse graphs
- Adding/removing vertices is expensive

### 2. Adjacency List

An array of lists. Each vertex has a list of its neighbors.

**Example:**
```
Graph:    0 → 1
          ↓   ↓
          2 → 3

Adjacency List:
0 → [1, 2]
1 → [3]
2 → [3]
3 → []
```

**Python Implementation:**
```python
from collections import defaultdict

class Graph:
    def __init__(self):
        self.graph = defaultdict(list)

    def add_edge(self, u, v):
        """Add directed edge from u to v."""
        self.graph[u].append(v)

    def add_edge_undirected(self, u, v):
        """Add undirected edge between u and v."""
        self.graph[u].append(v)
        self.graph[v].append(u)

    def get_neighbors(self, u):
        """Get all neighbors of vertex u."""
        return self.graph[u]

    def print_graph(self):
        for vertex in self.graph:
            print(f"{vertex} → {self.graph[vertex]}")
```

**Weighted Graph:**
```python
class WeightedGraph:
    def __init__(self):
        self.graph = defaultdict(list)

    def add_edge(self, u, v, weight):
        """Add weighted edge from u to v."""
        self.graph[u].append((v, weight))

    def print_graph(self):
        for vertex in self.graph:
            print(f"{vertex} → {self.graph[vertex]}")
```

**Java Implementation:**
```java
import java.util.*;

class Graph {
    private Map<Integer, List<Integer>> adjList;

    public Graph() {
        adjList = new HashMap<>();
    }

    public void addEdge(int u, int v) {
        adjList.putIfAbsent(u, new ArrayList<>());
        adjList.get(u).add(v);
    }

    public void addEdgeUndirected(int u, int v) {
        adjList.putIfAbsent(u, new ArrayList<>());
        adjList.putIfAbsent(v, new ArrayList<>());
        adjList.get(u).add(v);
        adjList.get(v).add(u);
    }

    public List<Integer> getNeighbors(int u) {
        return adjList.getOrDefault(u, new ArrayList<>());
    }
}
```

**Pros:**
- Space efficient: O(V + E)
- Fast neighbor iteration
- Easy to add/remove edges

**Cons:**
- Edge lookup: O(degree of vertex)
- More complex implementation

### When to Use Which?

| Scenario | Use |
|----------|-----|
| Dense graph (many edges) | Adjacency Matrix |
| Sparse graph (few edges) | Adjacency List |
| Need fast edge lookup | Adjacency Matrix |
| Need to iterate neighbors | Adjacency List |
| Graph changes frequently | Adjacency List |

---

## Part 3: Graph Traversal

### Depth-First Search (DFS)

**DFS** explores as far as possible along each branch before backtracking.

**Algorithm:**
1. Start at a vertex
2. Mark it as visited
3. Visit an unvisited neighbor
4. Recursively apply DFS to that neighbor
5. Backtrack when no unvisited neighbors

**Visualization:**
```
Graph:    1
         / \
        2   3
       /
      4

DFS from 1: 1 → 2 → 4 (backtrack to 2, backtrack to 1) → 3
Order: 1, 2, 4, 3
```

**Python Implementation (Recursive):**
```python
def dfs(graph, start, visited=None):
    if visited is None:
        visited = set()

    visited.add(start)
    print(start, end=' ')

    for neighbor in graph[start]:
        if neighbor not in visited:
            dfs(graph, neighbor, visited)

    return visited
```

**Python Implementation (Iterative with Stack):**
```python
def dfs_iterative(graph, start):
    visited = set()
    stack = [start]

    while stack:
        vertex = stack.pop()

        if vertex not in visited:
            visited.add(vertex)
            print(vertex, end=' ')

            # Add neighbors to stack (reverse order for same traversal as recursive)
            for neighbor in reversed(graph[vertex]):
                if neighbor not in visited:
                    stack.append(neighbor)

    return visited
```

**Java Implementation:**
```java
void dfs(int start) {
    Set<Integer> visited = new HashSet<>();
    dfsHelper(start, visited);
}

void dfsHelper(int vertex, Set<Integer> visited) {
    visited.add(vertex);
    System.out.print(vertex + " ");

    for (int neighbor : getNeighbors(vertex)) {
        if (!visited.contains(neighbor)) {
            dfsHelper(neighbor, visited);
        }
    }
}
```

**Time Complexity:** O(V + E)
**Space Complexity:** O(V) for visited set + O(V) for recursion stack

**Applications:**
- Cycle detection
- Topological sorting
- Finding connected components
- Solving mazes
- Puzzle solving

### Breadth-First Search (BFS)

**BFS** explores all neighbors at the current depth before moving to the next depth.

**Algorithm:**
1. Start at a vertex
2. Mark it as visited and enqueue it
3. While queue is not empty:
   - Dequeue a vertex
   - Visit all unvisited neighbors
   - Mark them visited and enqueue them

**Visualization:**
```
Graph:    1
         / \
        2   3
       / \
      4   5

BFS from 1:
Level 0: 1
Level 1: 2, 3
Level 2: 4, 5

Order: 1, 2, 3, 4, 5
```

**Python Implementation:**
```python
from collections import deque

def bfs(graph, start):
    visited = set([start])
    queue = deque([start])

    while queue:
        vertex = queue.popleft()
        print(vertex, end=' ')

        for neighbor in graph[vertex]:
            if neighbor not in visited:
                visited.add(neighbor)
                queue.append(neighbor)

    return visited
```

**Java Implementation:**
```java
void bfs(int start) {
    Set<Integer> visited = new HashSet<>();
    Queue<Integer> queue = new LinkedList<>();

    visited.add(start);
    queue.add(start);

    while (!queue.isEmpty()) {
        int vertex = queue.remove();
        System.out.print(vertex + " ");

        for (int neighbor : getNeighbors(vertex)) {
            if (!visited.contains(neighbor)) {
                visited.add(neighbor);
                queue.add(neighbor);
            }
        }
    }
}
```

**Time Complexity:** O(V + E)
**Space Complexity:** O(V) for visited set and queue

**Applications:**
- Shortest path in unweighted graphs
- Level-order traversal
- Finding connected components
- Social network analysis (friends within N hops)
- Web crawling

### DFS vs BFS Comparison

| Aspect | DFS | BFS |
|--------|-----|-----|
| Data Structure | Stack (recursion) | Queue |
| Memory | O(height) | O(width) |
| Shortest path | No | Yes (unweighted) |
| Completeness | No (infinite paths) | Yes |
| Use case | Topological sort, cycles | Shortest path, level order |

---

## Part 4: Shortest Path Algorithms

### Dijkstra's Algorithm

**Finds shortest path from source to all vertices in a weighted graph** (non-negative weights).

**Algorithm:**
1. Initialize distances: source = 0, others = ∞
2. Use a priority queue (min heap)
3. While queue is not empty:
   - Extract vertex with minimum distance
   - For each neighbor, try to relax the edge
   - Relaxation: if dist[u] + weight(u,v) < dist[v], update dist[v]

**Example:**
```
Graph:
      7        9
   A ---- B ------ C
   |      |        |
 2 |    3 |      2 |
   |      |        |
   D ---- E ------ F
      1        8

Shortest path from A to F: A → D → E → B → C → F = 15
```

**Python Implementation:**
```python
import heapq

def dijkstra(graph, start):
    """
    Find shortest paths from start to all vertices.
    graph: dict of dict, graph[u][v] = weight of edge u→v
    Returns: dict of shortest distances from start
    """
    distances = {vertex: float('inf') for vertex in graph}
    distances[start] = 0

    # Priority queue: (distance, vertex)
    pq = [(0, start)]
    visited = set()

    while pq:
        current_dist, current = heapq.heappop(pq)

        if current in visited:
            continue

        visited.add(current)

        for neighbor, weight in graph[current].items():
            distance = current_dist + weight

            if distance < distances[neighbor]:
                distances[neighbor] = distance
                heapq.heappush(pq, (distance, neighbor))

    return distances

# Example usage
graph = {
    'A': {'B': 7, 'D': 2},
    'B': {'A': 7, 'C': 9, 'E': 3},
    'C': {'B': 9, 'F': 2},
    'D': {'A': 2, 'E': 1},
    'E': {'B': 3, 'D': 1, 'F': 8},
    'F': {'C': 2, 'E': 8}
}

print(dijkstra(graph, 'A'))
# {'A': 0, 'B': 6, 'C': 15, 'D': 2, 'E': 3, 'F': 13}
```

**With Path Reconstruction:**
```python
def dijkstra_with_path(graph, start, end):
    """Return shortest distance and path."""
    distances = {vertex: float('inf') for vertex in graph}
    distances[start] = 0
    previous = {vertex: None for vertex in graph}

    pq = [(0, start)]
    visited = set()

    while pq:
        current_dist, current = heapq.heappop(pq)

        if current in visited:
            continue

        visited.add(current)

        if current == end:
            break

        for neighbor, weight in graph[current].items():
            distance = current_dist + weight

            if distance < distances[neighbor]:
                distances[neighbor] = distance
                previous[neighbor] = current
                heapq.heappush(pq, (distance, neighbor))

    # Reconstruct path
    path = []
    current = end
    while current is not None:
        path.append(current)
        current = previous[current]
    path.reverse()

    return distances[end], path
```

**Time Complexity:** O((V + E) log V) with binary heap
**Space Complexity:** O(V)

**Applications:**
- GPS navigation
- Network routing protocols (OSPF)
- Google Maps

### Bellman-Ford Algorithm

**Finds shortest paths even with negative edge weights** (can detect negative cycles).

**Algorithm:**
1. Initialize distances: source = 0, others = ∞
2. Relax all edges V-1 times
3. Check for negative cycles

**Python Implementation:**
```python
def bellman_ford(graph, start):
    """
    Find shortest paths, works with negative weights.
    graph: list of (u, v, weight) tuples
    Returns: (distances, has_negative_cycle)
    """
    vertices = set()
    for u, v, w in graph:
        vertices.add(u)
        vertices.add(v)

    distances = {v: float('inf') for v in vertices}
    distances[start] = 0

    # Relax all edges V-1 times
    for _ in range(len(vertices) - 1):
        for u, v, weight in graph:
            if distances[u] + weight < distances[v]:
                distances[v] = distances[u] + weight

    # Check for negative cycles
    for u, v, weight in graph:
        if distances[u] + weight < distances[v]:
            return distances, True  # Negative cycle detected

    return distances, False

# Example
edges = [
    ('A', 'B', 4),
    ('A', 'C', 2),
    ('B', 'C', -3),
    ('C', 'D', 2),
    ('D', 'B', 1)
]

distances, has_neg_cycle = bellman_ford(edges, 'A')
print(distances)
```

**Time Complexity:** O(VE)
**Space Complexity:** O(V)

**Applications:**
- Currency arbitrage detection
- When graph has negative edges

### Floyd-Warshall Algorithm

**Finds shortest paths between all pairs of vertices.**

**Python Implementation:**
```python
def floyd_warshall(graph):
    """
    Find shortest paths between all pairs.
    graph: adjacency matrix (use float('inf') for no edge)
    Returns: distance matrix
    """
    V = len(graph)
    dist = [row[:] for row in graph]  # Copy matrix

    # Try all intermediate vertices
    for k in range(V):
        for i in range(V):
            for j in range(V):
                dist[i][j] = min(dist[i][j], dist[i][k] + dist[k][j])

    return dist
```

**Time Complexity:** O(V³)
**Space Complexity:** O(V²)

---

## Part 5: Minimum Spanning Tree

A **Minimum Spanning Tree (MST)** is a subset of edges that connects all vertices with minimum total edge weight (no cycles).

### Kruskal's Algorithm

**Greedy algorithm using Union-Find:**
1. Sort all edges by weight
2. For each edge (u, v):
   - If u and v are in different sets, add edge to MST
   - Union the sets

**Python Implementation:**
```python
class UnionFind:
    def __init__(self, n):
        self.parent = list(range(n))
        self.rank = [0] * n

    def find(self, x):
        if self.parent[x] != x:
            self.parent[x] = self.find(self.parent[x])  # Path compression
        return self.parent[x]

    def union(self, x, y):
        px, py = self.find(x), self.find(y)
        if px == py:
            return False
        # Union by rank
        if self.rank[px] < self.rank[py]:
            self.parent[px] = py
        elif self.rank[px] > self.rank[py]:
            self.parent[py] = px
        else:
            self.parent[py] = px
            self.rank[px] += 1
        return True

def kruskal(vertices, edges):
    """
    Find MST using Kruskal's algorithm.
    vertices: number of vertices
    edges: list of (weight, u, v) tuples
    Returns: list of edges in MST
    """
    edges.sort()  # Sort by weight
    uf = UnionFind(vertices)
    mst = []

    for weight, u, v in edges:
        if uf.union(u, v):
            mst.append((u, v, weight))
            if len(mst) == vertices - 1:
                break

    return mst

# Example
edges = [
    (1, 0, 1),
    (2, 0, 2),
    (3, 1, 2),
    (4, 1, 3),
    (5, 2, 3)
]

mst = kruskal(4, edges)
print("MST edges:", mst)
```

**Time Complexity:** O(E log E) for sorting
**Space Complexity:** O(V)

### Prim's Algorithm

**Greedy algorithm using priority queue:**
1. Start with any vertex
2. Repeatedly add the minimum weight edge connecting visited to unvisited vertices

**Python Implementation:**
```python
import heapq

def prim(graph, start):
    """
    Find MST using Prim's algorithm.
    graph: dict of dict, graph[u][v] = weight
    Returns: list of edges in MST
    """
    mst = []
    visited = {start}
    edges = [(weight, start, to) for to, weight in graph[start].items()]
    heapq.heapify(edges)

    while edges:
        weight, frm, to = heapq.heappop(edges)

        if to in visited:
            continue

        visited.add(to)
        mst.append((frm, to, weight))

        for next_to, next_weight in graph[to].items():
            if next_to not in visited:
                heapq.heappush(edges, (next_weight, to, next_to))

    return mst
```

**Time Complexity:** O(E log V) with binary heap
**Space Complexity:** O(V)

**Applications:**
- Network design (minimize cable length)
- Clustering algorithms
- Approximation algorithms

---

## Part 6: Topological Sort

**Topological Sort** orders vertices in a DAG such that for every edge u → v, u comes before v.

**Example:**
```
Task dependencies:
A → B → D
A → C → D

Topological order: A, B, C, D (or A, C, B, D)
```

### DFS-Based Topological Sort

**Python Implementation:**
```python
def topological_sort(graph):
    """
    Return topological ordering of vertices.
    Only works on DAG (Directed Acyclic Graph).
    """
    visited = set()
    stack = []

    def dfs(vertex):
        visited.add(vertex)
        for neighbor in graph.get(vertex, []):
            if neighbor not in visited:
                dfs(neighbor)
        stack.append(vertex)  # Add to stack after visiting all descendants

    for vertex in graph:
        if vertex not in visited:
            dfs(vertex)

    return stack[::-1]  # Reverse stack

# Example
graph = {
    'A': ['B', 'C'],
    'B': ['D'],
    'C': ['D'],
    'D': []
}

print(topological_sort(graph))  # ['A', 'B', 'C', 'D'] or ['A', 'C', 'B', 'D']
```

### Kahn's Algorithm (BFS-Based)

**Python Implementation:**
```python
from collections import deque, defaultdict

def topological_sort_kahn(graph):
    """Topological sort using Kahn's algorithm (BFS)."""
    in_degree = defaultdict(int)
    adj_list = defaultdict(list)

    # Build in-degree map and adjacency list
    for u in graph:
        for v in graph[u]:
            adj_list[u].append(v)
            in_degree[v] += 1
        if u not in in_degree:
            in_degree[u] = 0

    # Start with vertices having in-degree 0
    queue = deque([v for v in in_degree if in_degree[v] == 0])
    result = []

    while queue:
        vertex = queue.popleft()
        result.append(vertex)

        for neighbor in adj_list[vertex]:
            in_degree[neighbor] -= 1
            if in_degree[neighbor] == 0:
                queue.append(neighbor)

    # Check for cycle
    if len(result) != len(in_degree):
        return None  # Cycle detected

    return result
```

**Time Complexity:** O(V + E)
**Space Complexity:** O(V)

**Applications:**
- Task scheduling
- Build systems (make, npm)
- Course prerequisites
- Compilation order

---

## Part 7: Cycle Detection

### Detect Cycle in Undirected Graph

**Using DFS:**
```python
def has_cycle_undirected(graph):
    """Detect cycle in undirected graph using DFS."""
    visited = set()

    def dfs(vertex, parent):
        visited.add(vertex)

        for neighbor in graph[vertex]:
            if neighbor not in visited:
                if dfs(neighbor, vertex):
                    return True
            elif neighbor != parent:
                return True  # Back edge to non-parent = cycle

        return False

    for vertex in graph:
        if vertex not in visited:
            if dfs(vertex, None):
                return True

    return False
```

### Detect Cycle in Directed Graph

**Using DFS with recursion stack:**
```python
def has_cycle_directed(graph):
    """Detect cycle in directed graph using DFS."""
    WHITE, GRAY, BLACK = 0, 1, 2
    color = {vertex: WHITE for vertex in graph}

    def dfs(vertex):
        color[vertex] = GRAY  # Currently processing

        for neighbor in graph.get(vertex, []):
            if color[neighbor] == GRAY:
                return True  # Back edge = cycle
            if color[neighbor] == WHITE:
                if dfs(neighbor):
                    return True

        color[vertex] = BLACK  # Done processing
        return False

    for vertex in graph:
        if color[vertex] == WHITE:
            if dfs(vertex):
                return True

    return False
```

---

## Part 8: Connected Components

### Find Connected Components (Undirected Graph)

**Using DFS:**
```python
def find_connected_components(graph):
    """Find all connected components."""
    visited = set()
    components = []

    def dfs(vertex, component):
        visited.add(vertex)
        component.append(vertex)
        for neighbor in graph.get(vertex, []):
            if neighbor not in visited:
                dfs(neighbor, component)

    for vertex in graph:
        if vertex not in visited:
            component = []
            dfs(vertex, component)
            components.append(component)

    return components

# Example
graph = {
    0: [1, 2],
    1: [0],
    2: [0],
    3: [4],
    4: [3],
    5: []
}

print(find_connected_components(graph))
# [[0, 1, 2], [3, 4], [5]]
```

### Strongly Connected Components (Directed Graph)

**Kosaraju's Algorithm:**
```python
def kosaraju(graph):
    """Find strongly connected components in directed graph."""
    # Step 1: Fill stack with vertices in order of finish time
    visited = set()
    stack = []

    def dfs1(vertex):
        visited.add(vertex)
        for neighbor in graph.get(vertex, []):
            if neighbor not in visited:
                dfs1(neighbor)
        stack.append(vertex)

    for vertex in graph:
        if vertex not in visited:
            dfs1(vertex)

    # Step 2: Reverse the graph
    reversed_graph = {v: [] for v in graph}
    for u in graph:
        for v in graph[u]:
            reversed_graph[v].append(u)

    # Step 3: DFS on reversed graph in stack order
    visited.clear()
    sccs = []

    def dfs2(vertex, scc):
        visited.add(vertex)
        scc.append(vertex)
        for neighbor in reversed_graph.get(vertex, []):
            if neighbor not in visited:
                dfs2(neighbor, scc)

    while stack:
        vertex = stack.pop()
        if vertex not in visited:
            scc = []
            dfs2(vertex, scc)
            sccs.append(scc)

    return sccs
```

---

## Part 9: Real-World Applications

### Social Network Analysis

**Find mutual friends:**
```python
def find_mutual_friends(graph, person1, person2):
    """Find friends common to both people."""
    friends1 = set(graph[person1])
    friends2 = set(graph[person2])
    return friends1 & friends2

def friends_within_n_hops(graph, start, n):
    """Find all friends within n hops using BFS."""
    if n == 0:
        return {start}

    visited = {start}
    queue = deque([(start, 0)])
    result = set()

    while queue:
        person, hops = queue.popleft()

        if hops == n:
            result.add(person)
            continue

        for friend in graph[person]:
            if friend not in visited:
                visited.add(friend)
                queue.append((friend, hops + 1))
                if hops + 1 <= n:
                    result.add(friend)

    return result
```

### Web Crawler

**BFS-based web crawler:**
```python
def web_crawler(start_url, max_pages):
    """Crawl web pages using BFS."""
    visited = set([start_url])
    queue = deque([start_url])
    crawled = []

    while queue and len(crawled) < max_pages:
        url = queue.popleft()
        crawled.append(url)
        print(f"Crawling: {url}")

        # Get links from page (simplified)
        links = get_links_from_page(url)

        for link in links:
            if link not in visited:
                visited.add(link)
                queue.append(link)

    return crawled
```

### Dependency Resolution

**Install packages in correct order:**
```python
def install_packages(dependencies):
    """
    Install packages respecting dependencies.
    dependencies: dict where dependencies[pkg] = list of required packages
    """
    # This is topological sort!
    return topological_sort_kahn(dependencies)

# Example
deps = {
    'app': ['server', 'database'],
    'server': ['utils'],
    'database': ['utils'],
    'utils': []
}

print(install_packages(deps))
# ['utils', 'server', 'database', 'app']
```

---

## Part 10: Advanced Topics (Brief Overview)

### Graph Coloring

**Assign colors to vertices such that no adjacent vertices have the same color.**

```python
def graph_coloring_greedy(graph):
    """Greedy graph coloring."""
    colors = {}

    for vertex in graph:
        # Find colors of neighbors
        neighbor_colors = {colors[n] for n in graph[vertex] if n in colors}

        # Assign smallest available color
        color = 0
        while color in neighbor_colors:
            color += 1

        colors[vertex] = color

    return colors
```

**Applications:** Register allocation, scheduling, map coloring

### Maximum Flow

**Ford-Fulkerson algorithm finds maximum flow in a flow network.**

**Applications:** Network bandwidth, bipartite matching, airline scheduling

### Traveling Salesman Problem (TSP)

**Find shortest route visiting all cities exactly once.**

**Note:** NP-hard problem, no known polynomial solution

**Applications:** Logistics, circuit board drilling, DNA sequencing

---

## Part 11: Time Complexity Summary

| Algorithm | Time Complexity | Space Complexity |
|-----------|-----------------|------------------|
| DFS | O(V + E) | O(V) |
| BFS | O(V + E) | O(V) |
| Dijkstra (binary heap) | O((V + E) log V) | O(V) |
| Bellman-Ford | O(VE) | O(V) |
| Floyd-Warshall | O(V³) | O(V²) |
| Kruskal's MST | O(E log E) | O(V) |
| Prim's MST | O(E log V) | O(V) |
| Topological Sort | O(V + E) | O(V) |

---

## Best Practices

1. **Choose the right representation:**
   - Sparse graph → Adjacency list
   - Dense graph → Adjacency matrix

2. **Handle disconnected graphs:**
   ```python
   for vertex in graph:
       if vertex not in visited:
           dfs(vertex)
   ```

3. **Always check for cycles** in DAG algorithms

4. **Use appropriate algorithm:**
   - Unweighted shortest path → BFS
   - Weighted positive edges → Dijkstra
   - Negative edges → Bellman-Ford
   - All pairs → Floyd-Warshall

5. **Consider edge cases:**
   - Empty graph
   - Single vertex
   - Disconnected components
   - Self-loops
   - Parallel edges

---

## Common Pitfalls

1. **Forgetting to mark vertices as visited** leads to infinite loops

2. **Using DFS for shortest path** - DFS doesn't guarantee shortest path!

3. **Applying Dijkstra with negative weights** - gives wrong results!

4. **Not handling disconnected components**

5. **Confusing directed and undirected graphs**

---

## Next Steps

- **Practice:** Implement all algorithms from scratch
- **LeetCode/HackerRank:** Graph problems
- **Advanced topics:**
  - A* search algorithm
  - Network flow algorithms
  - Graph isomorphism
  - PageRank algorithm
- **Real projects:**
  - Build a maze solver
  - Implement Google Maps-like routing
  - Create a social network analyzer

Graphs are everywhere in CS! Mastering them opens doors to solving complex real-world problems.
