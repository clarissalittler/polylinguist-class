# Graphs and Graph Algorithms: Practice Exercises

These exercises progress from basic graph operations to advanced algorithms.

---

## Part 1: Graph Representation

### Exercise 1.1: Build a Graph

**Task:** Represent this graph using both adjacency matrix and adjacency list:

```
    0 --- 1
    |     |
    |     |
    2 --- 3
```

Implement both representations in Python.

---

### Exercise 1.2: Weighted Graph

**Task:** Implement a weighted graph using an adjacency list where edges have weights:

```
       5
    A --- B
    |     |
  2 |     | 3
    |     |
    C --- D
       7
```

Your implementation should support:
- Adding weighted edges
- Getting the weight of an edge
- Getting all neighbors with their weights

---

### Exercise 1.3: Directed Graph

**Task:** Implement a directed graph and add these edges:

```
A → B → C
↑       ↓
D ← --- E
```

Implement methods to:
- Add directed edge
- Check if edge exists
- Get in-degree of a vertex
- Get out-degree of a vertex

---

### Exercise 1.4: Convert Representations

**Task:** Write functions to convert between adjacency matrix and adjacency list.

```python
def matrix_to_list(matrix):
    """Convert adjacency matrix to adjacency list."""
    # TODO: Implement
    pass

def list_to_matrix(adj_list, num_vertices):
    """Convert adjacency list to adjacency matrix."""
    # TODO: Implement
    pass
```

---

## Part 2: Graph Traversal

### Exercise 2.1: Manual DFS

**Given this graph:**
```
       1
      / \
     2   3
    /     \
   4       5
```

Starting from vertex 1, write the order of vertices visited using DFS.

---

### Exercise 2.2: Manual BFS

**Using the same graph from 2.1**, write the order of vertices visited using BFS starting from vertex 1.

Compare with your DFS answer.

---

### Exercise 2.3: Implement DFS

**Task:** Implement both recursive and iterative DFS.

```python
def dfs_recursive(graph, start, visited=None):
    """DFS using recursion."""
    # TODO: Implement
    pass

def dfs_iterative(graph, start):
    """DFS using stack."""
    # TODO: Implement
    pass
```

Test both implementations on this graph:
```
graph = {
    0: [1, 2],
    1: [0, 3, 4],
    2: [0, 5],
    3: [1],
    4: [1],
    5: [2]
}
```

---

### Exercise 2.4: Implement BFS

**Task:** Implement BFS to return vertices in the order they were visited.

```python
from collections import deque

def bfs(graph, start):
    """BFS returning list of vertices in visit order."""
    # TODO: Implement
    pass
```

---

### Exercise 2.5: Find Path

**Task:** Use BFS to find if a path exists between two vertices, and return that path.

```python
def find_path_bfs(graph, start, end):
    """
    Return path from start to end using BFS.
    If no path exists, return None.

    Example: find_path_bfs(graph, 0, 5) → [0, 2, 5]
    """
    # TODO: Implement
    pass
```

---

### Exercise 2.6: All Paths

**Task:** Find ALL paths from start to end using DFS.

```python
def find_all_paths(graph, start, end, path=[]):
    """
    Find all paths from start to end.

    Example graph: 0→1→3
                   0→2→3

    find_all_paths(graph, 0, 3) → [[0,1,3], [0,2,3]]
    """
    # TODO: Implement
    pass
```

---

## Part 3: Shortest Path Algorithms

### Exercise 3.1: Shortest Path in Unweighted Graph

**Task:** Find the shortest path length between two vertices in an unweighted graph using BFS.

```python
def shortest_path_unweighted(graph, start, end):
    """
    Return length of shortest path in unweighted graph.

    Example:
    0 -- 1 -- 3
    |         |
    2 ------- 4

    shortest_path_unweighted(graph, 0, 4) → 2 (path: 0→2→4)
    """
    # TODO: Implement
    pass
```

---

### Exercise 3.2: Implement Dijkstra's Algorithm

**Task:** Implement Dijkstra's algorithm from scratch.

```python
import heapq

def dijkstra(graph, start):
    """
    Find shortest paths from start to all vertices.
    graph: dict of dict, graph[u][v] = weight

    Returns: dict of shortest distances
    """
    # TODO: Implement
    pass
```

Test with this graph:
```python
graph = {
    'A': {'B': 1, 'C': 4},
    'B': {'A': 1, 'C': 2, 'D': 5},
    'C': {'A': 4, 'B': 2, 'D': 1},
    'D': {'B': 5, 'C': 1}
}

print(dijkstra(graph, 'A'))
# Expected: {'A': 0, 'B': 1, 'C': 3, 'D': 4}
```

---

### Exercise 3.3: Dijkstra with Path

**Task:** Modify Dijkstra to also return the actual path.

```python
def dijkstra_with_path(graph, start, end):
    """
    Return (distance, path) for shortest path from start to end.

    Example: dijkstra_with_path(graph, 'A', 'D')
             → (4, ['A', 'B', 'C', 'D'])
    """
    # TODO: Implement
    pass
```

---

### Exercise 3.4: Bellman-Ford Algorithm

**Task:** Implement Bellman-Ford algorithm that handles negative weights.

```python
def bellman_ford(edges, vertices, start):
    """
    Find shortest paths from start, handles negative weights.
    edges: list of (u, v, weight) tuples
    vertices: list of all vertices
    start: starting vertex

    Returns: (distances_dict, has_negative_cycle)
    """
    # TODO: Implement
    pass
```

Test with:
```python
edges = [
    ('A', 'B', 4),
    ('A', 'C', 2),
    ('B', 'C', -5),  # Negative weight!
    ('C', 'D', 2),
    ('D', 'B', 1)
]
vertices = ['A', 'B', 'C', 'D']
```

---

### Exercise 3.5: Floyd-Warshall

**Task:** Implement Floyd-Warshall for all-pairs shortest paths.

```python
def floyd_warshall(graph):
    """
    Find shortest paths between all pairs.
    graph: 2D list (adjacency matrix)
           Use float('inf') for no edge

    Returns: distance matrix
    """
    # TODO: Implement
    pass
```

---

## Part 4: Minimum Spanning Tree

### Exercise 4.1: Understand MST

**Given this weighted graph:**
```
       2        3
    A --- B --- C
    |     |     |
  6 |   1 |   4 |
    |     |     |
    D --- E --- F
       5        2
```

**Manually find the MST** and calculate its total weight.

---

### Exercise 4.2: Implement Kruskal's Algorithm

**Task:** Implement Kruskal's algorithm with Union-Find.

```python
class UnionFind:
    def __init__(self, n):
        """Initialize Union-Find with n elements."""
        # TODO: Implement
        pass

    def find(self, x):
        """Find root of x with path compression."""
        # TODO: Implement
        pass

    def union(self, x, y):
        """Union sets containing x and y. Return True if sets were different."""
        # TODO: Implement
        pass

def kruskal(num_vertices, edges):
    """
    Find MST using Kruskal's algorithm.
    edges: list of (weight, u, v) tuples

    Returns: list of edges in MST
    """
    # TODO: Implement
    pass
```

---

### Exercise 4.3: Implement Prim's Algorithm

**Task:** Implement Prim's algorithm.

```python
import heapq

def prim(graph, start):
    """
    Find MST using Prim's algorithm.
    graph: dict of dict, graph[u][v] = weight

    Returns: list of (u, v, weight) tuples in MST
    """
    # TODO: Implement
    pass
```

---

### Exercise 4.4: MST vs Shortest Path

**Question:** Why is MST different from shortest path tree?

**Task:** Given this graph:
```
       1
    A --- B
    |\    |
  5 | \2  | 1
    |  \  |
    C---D-'
       1
```

Find:
1. MST starting from A
2. Shortest path tree from A

Are they the same? Why or why not?

---

## Part 5: Topological Sort

### Exercise 5.1: Manual Topological Sort

**Given this DAG:**
```
    A → B → D
    ↓   ↓
    C → E
```

Write at least 2 valid topological orderings.

---

### Exercise 5.2: Implement DFS-Based Topological Sort

**Task:** Implement topological sort using DFS.

```python
def topological_sort_dfs(graph):
    """
    Return topological ordering using DFS.
    Only works on DAG.

    Returns: list of vertices in topological order
    """
    # TODO: Implement
    pass
```

---

### Exercise 5.3: Implement Kahn's Algorithm

**Task:** Implement topological sort using Kahn's algorithm (BFS-based).

```python
from collections import deque, defaultdict

def topological_sort_kahn(graph):
    """
    Return topological ordering using Kahn's algorithm.
    Detects cycles.

    Returns: list of vertices in topological order, or None if cycle exists
    """
    # TODO: Implement
    pass
```

---

### Exercise 5.4: Course Schedule

**Task:** Given courses and prerequisites, determine if you can finish all courses.

```python
def can_finish(num_courses, prerequisites):
    """
    Determine if you can finish all courses.

    num_courses: int
    prerequisites: list of [course, prerequisite] pairs

    Example: can_finish(2, [[1, 0]]) → True (take 0, then 1)
             can_finish(2, [[1, 0], [0, 1]]) → False (cycle!)

    Returns: True if possible, False otherwise
    """
    # TODO: Implement using topological sort
    pass
```

---

### Exercise 5.5: Course Schedule II

**Task:** Return the order in which courses should be taken.

```python
def find_order(num_courses, prerequisites):
    """
    Return ordering of courses to take.

    Example: find_order(4, [[1,0], [2,0], [3,1], [3,2]])
             → [0, 1, 2, 3] or [0, 2, 1, 3]

    Returns: valid course ordering, or [] if impossible
    """
    # TODO: Implement
    pass
```

---

## Part 6: Cycle Detection

### Exercise 6.1: Detect Cycle in Undirected Graph

**Task:** Implement cycle detection for undirected graphs using DFS.

```python
def has_cycle_undirected(graph):
    """
    Detect if undirected graph has a cycle.

    Example:
    0 -- 1 -- 2  (no cycle)

    0 -- 1
    |    |
    2 -- 3  (has cycle)

    Returns: True if cycle exists, False otherwise
    """
    # TODO: Implement
    pass
```

---

### Exercise 6.2: Detect Cycle in Directed Graph

**Task:** Implement cycle detection for directed graphs.

```python
def has_cycle_directed(graph):
    """
    Detect if directed graph has a cycle.

    Example:
    0 → 1 → 2  (no cycle)

    0 → 1
    ↑   ↓
    3 ← 2  (has cycle)

    Returns: True if cycle exists, False otherwise
    """
    # TODO: Implement using DFS with colors (WHITE, GRAY, BLACK)
    pass
```

---

### Exercise 6.3: Find Cycle Path

**Task:** If a cycle exists, return the vertices in that cycle.

```python
def find_cycle(graph):
    """
    Find and return a cycle if one exists.

    Example:
    0 → 1 → 2
        ↑   ↓
        4 ← 3

    Returns: [1, 2, 3, 4, 1] or None if no cycle
    """
    # TODO: Implement
    pass
```

---

## Part 7: Connected Components

### Exercise 7.1: Count Connected Components

**Task:** Count the number of connected components in an undirected graph.

```python
def count_components(n, edges):
    """
    Count connected components.

    n: number of vertices (0 to n-1)
    edges: list of [u, v] pairs

    Example: n=5, edges=[[0,1], [1,2], [3,4]]
             → 2 components: {0,1,2} and {3,4}

    Returns: number of connected components
    """
    # TODO: Implement using DFS or Union-Find
    pass
```

---

### Exercise 7.2: Find All Components

**Task:** Return all connected components as separate lists.

```python
def find_components(graph):
    """
    Find all connected components.

    Returns: list of lists, each inner list is a component

    Example: [[0, 1, 2], [3, 4], [5]]
    """
    # TODO: Implement
    pass
```

---

### Exercise 7.3: Island Counting

**Task:** Given a 2D grid of 1s (land) and 0s (water), count the number of islands.

```python
def num_islands(grid):
    """
    Count islands in 2D grid.

    Example:
    grid = [
      [1, 1, 0, 0],
      [1, 1, 0, 0],
      [0, 0, 1, 1],
      [0, 0, 0, 1]
    ]
    Returns: 2 islands

    Hint: Use DFS or BFS to mark visited cells
    """
    # TODO: Implement
    pass
```

---

### Exercise 7.4: Strongly Connected Components

**Task:** Find strongly connected components in a directed graph using Kosaraju's algorithm.

```python
def kosaraju_scc(graph):
    """
    Find strongly connected components in directed graph.

    Returns: list of SCCs, each SCC is a list of vertices
    """
    # TODO: Implement Kosaraju's algorithm
    pass
```

---

## Part 8: Advanced Problems

### Exercise 8.1: Clone Graph

**Task:** Create a deep copy of a graph.

```python
class Node:
    def __init__(self, val=0, neighbors=None):
        self.val = val
        self.neighbors = neighbors if neighbors else []

def clone_graph(node):
    """
    Create a deep copy of the graph.
    node: reference to a node in the graph

    Returns: reference to the cloned node
    """
    # TODO: Implement using DFS or BFS with a hash map
    pass
```

---

### Exercise 8.2: Word Ladder

**Task:** Transform one word into another, changing one letter at a time, where each intermediate word must exist in the dictionary.

```python
def ladder_length(begin_word, end_word, word_list):
    """
    Find shortest transformation sequence length.

    Example:
    begin_word = "hit"
    end_word = "cog"
    word_list = ["hot","dot","dog","lot","log","cog"]

    Shortest: hit → hot → dot → dog → cog (length = 5)

    Returns: length of shortest sequence, or 0 if impossible

    Hint: Build a graph where each word is a node, edges connect
          words that differ by one letter. Use BFS.
    """
    # TODO: Implement
    pass
```

---

### Exercise 8.3: Network Delay Time

**Task:** Find how long it takes for a signal to reach all nodes in a network.

```python
def network_delay_time(times, n, k):
    """
    times: list of [u, v, w] where signal goes from u to v in time w
    n: number of nodes (1 to n)
    k: starting node

    Returns: time for signal to reach all nodes, or -1 if impossible

    Hint: Use Dijkstra's algorithm
    """
    # TODO: Implement
    pass
```

---

### Exercise 8.4: Cheapest Flights

**Task:** Find the cheapest price from source to destination with at most k stops.

```python
def find_cheapest_price(n, flights, src, dst, k):
    """
    n: number of cities (0 to n-1)
    flights: list of [from, to, price]
    k: maximum number of stops

    Returns: cheapest price, or -1 if impossible

    Hint: Modified Bellman-Ford or BFS with price tracking
    """
    # TODO: Implement
    pass
```

---

### Exercise 8.5: Critical Connections

**Task:** Find all critical connections (bridges) in a network. A bridge is an edge whose removal disconnects the graph.

```python
def critical_connections(n, connections):
    """
    Find all bridges in the network.

    n: number of servers (0 to n-1)
    connections: list of [u, v] edges

    Returns: list of critical connections

    Hint: Use Tarjan's algorithm or DFS with discovery times
    """
    # TODO: Implement
    pass
```

---

## Part 9: Real-World Applications

### Exercise 9.1: Social Network - Mutual Friends

**Task:** Implement functions for a social network.

```python
def mutual_friends(graph, person1, person2):
    """Find common friends between two people."""
    # TODO: Implement
    pass

def suggest_friends(graph, person):
    """
    Suggest friends for a person.
    Suggest friends of friends who aren't already friends.
    """
    # TODO: Implement
    pass

def degrees_of_separation(graph, person1, person2):
    """
    Find degrees of separation (shortest path length).

    Example: If person1 knows someone who knows person2,
             degrees of separation = 2
    """
    # TODO: Implement using BFS
    pass
```

---

### Exercise 9.2: Package Dependencies

**Task:** Implement a package manager's dependency resolver.

```python
class PackageManager:
    def __init__(self):
        self.dependencies = {}  # package -> list of dependencies

    def add_package(self, package, deps):
        """Add a package with its dependencies."""
        # TODO: Implement
        pass

    def install_order(self, package):
        """
        Return order in which packages should be installed.
        Includes all transitive dependencies.

        Example:
        app depends on [server, database]
        server depends on [utils]
        database depends on [utils]

        install_order('app') → ['utils', 'server', 'database', 'app']
        """
        # TODO: Implement using topological sort
        pass

    def has_circular_dependency(self):
        """Check if circular dependencies exist."""
        # TODO: Implement using cycle detection
        pass
```

---

### Exercise 9.3: Map Navigation

**Task:** Implement a simplified GPS navigation system.

```python
class MapGraph:
    def __init__(self):
        self.graph = {}  # location -> {neighbor: distance}

    def add_road(self, loc1, loc2, distance):
        """Add a bidirectional road between locations."""
        # TODO: Implement
        pass

    def shortest_route(self, start, end):
        """
        Find shortest route from start to end.

        Returns: (distance, path)
        """
        # TODO: Implement using Dijkstra
        pass

    def fastest_route(self, start, end, speed_limits):
        """
        Find fastest route considering speed limits.
        speed_limits: dict of {(loc1, loc2): speed}

        Returns: (time, path)
        """
        # TODO: Implement (modify Dijkstra for time instead of distance)
        pass

    def alternative_routes(self, start, end, k):
        """Find k shortest paths from start to end."""
        # TODO: Implement (harder: Yen's k-shortest paths algorithm)
        pass
```

---

### Exercise 9.4: Flight Itinerary

**Task:** Reconstruct flight itinerary from tickets.

```python
def find_itinerary(tickets):
    """
    Reconstruct itinerary from tickets.
    tickets: list of [from, to] pairs

    Example:
    tickets = [["JFK","SFO"], ["JFK","ATL"], ["SFO","ATL"],
               ["ATL","JFK"], ["ATL","SFO"]]

    Returns: ["JFK","ATL","JFK","SFO","ATL","SFO"]
    (valid Eulerian path through graph)

    Constraint: Use all tickets exactly once
    If multiple valid paths, return lexicographically smallest

    Hint: This is finding an Eulerian path
    """
    # TODO: Implement using DFS
    pass
```

---

## Part 10: Graph Coloring

### Exercise 10.1: Graph Coloring

**Task:** Color a graph with minimum colors such that no adjacent vertices have the same color.

```python
def graph_coloring(graph):
    """
    Assign colors to vertices (greedy algorithm).

    Returns: dict mapping vertex to color (integer)
    """
    # TODO: Implement greedy coloring
    pass

def chromatic_number(graph):
    """Return minimum number of colors needed."""
    colors = graph_coloring(graph)
    return max(colors.values()) + 1 if colors else 0
```

---

### Exercise 10.2: Bipartite Check

**Task:** Check if a graph is bipartite (can be colored with 2 colors).

```python
def is_bipartite(graph):
    """
    Check if graph is bipartite.

    A graph is bipartite if vertices can be split into two sets
    such that all edges go between sets (no edges within a set).

    Example: Tree → always bipartite
             Odd-length cycle → not bipartite

    Returns: True if bipartite, False otherwise

    Hint: Try to color with 2 colors using BFS or DFS
    """
    # TODO: Implement
    pass
```

---

## Solutions to Selected Exercises

### Exercise 2.1 & 2.2 Solution

**Graph:**
```
       1
      / \
     2   3
    /     \
   4       5
```

**DFS (recursive, visiting left first):** 1, 2, 4, 3, 5

**BFS:** 1, 2, 3, 4, 5

---

### Exercise 2.5 Solution

```python
from collections import deque

def find_path_bfs(graph, start, end):
    if start == end:
        return [start]

    visited = {start}
    queue = deque([(start, [start])])

    while queue:
        vertex, path = queue.popleft()

        for neighbor in graph[vertex]:
            if neighbor == end:
                return path + [neighbor]

            if neighbor not in visited:
                visited.add(neighbor)
                queue.append((neighbor, path + [neighbor]))

    return None  # No path found
```

---

### Exercise 3.2 Solution

```python
import heapq

def dijkstra(graph, start):
    distances = {vertex: float('inf') for vertex in graph}
    distances[start] = 0

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
```

---

### Exercise 5.2 Solution

```python
def topological_sort_dfs(graph):
    visited = set()
    stack = []

    def dfs(vertex):
        visited.add(vertex)
        for neighbor in graph.get(vertex, []):
            if neighbor not in visited:
                dfs(neighbor)
        stack.append(vertex)

    for vertex in graph:
        if vertex not in visited:
            dfs(vertex)

    return stack[::-1]
```

---

### Exercise 6.1 Solution

```python
def has_cycle_undirected(graph):
    visited = set()

    def dfs(vertex, parent):
        visited.add(vertex)

        for neighbor in graph[vertex]:
            if neighbor not in visited:
                if dfs(neighbor, vertex):
                    return True
            elif neighbor != parent:
                return True  # Back edge found

        return False

    for vertex in graph:
        if vertex not in visited:
            if dfs(vertex, None):
                return True

    return False
```

---

### Exercise 7.1 Solution

```python
def count_components(n, edges):
    # Build adjacency list
    graph = {i: [] for i in range(n)}
    for u, v in edges:
        graph[u].append(v)
        graph[v].append(u)

    visited = set()
    count = 0

    def dfs(vertex):
        visited.add(vertex)
        for neighbor in graph[vertex]:
            if neighbor not in visited:
                dfs(neighbor)

    for vertex in range(n):
        if vertex not in visited:
            dfs(vertex)
            count += 1

    return count
```

---

### Exercise 10.2 Solution

```python
from collections import deque

def is_bipartite(graph):
    color = {}

    for start in graph:
        if start in color:
            continue

        queue = deque([start])
        color[start] = 0

        while queue:
            vertex = queue.popleft()

            for neighbor in graph[vertex]:
                if neighbor not in color:
                    color[neighbor] = 1 - color[vertex]
                    queue.append(neighbor)
                elif color[neighbor] == color[vertex]:
                    return False  # Same color as neighbor

    return True
```

---

## Additional Practice

For more practice:
1. **LeetCode Graph Problems** - 150+ graph problems
2. **Project Euler** - Graph theory problems
3. **Visualize graphs** - Use [VisuAlgo](https://visualgo.net/en/dfsbfs)
4. **Implement advanced algorithms:**
   - A* search
   - Tarjan's algorithm (strongly connected components)
   - Johnson's algorithm (all-pairs shortest path)
5. **Real projects:**
   - Build a maze generator and solver
   - Create a social network analyzer
   - Implement a route planner

Graphs are fundamental to solving complex interconnected problems. Master them!
