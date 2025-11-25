"""
Lesson 16: Graphs - Python Examples
Graph representations, traversals, and algorithms
"""

from typing import Dict, List, Set, Optional, Tuple
from collections import defaultdict, deque
import heapq

# =============================================================================
# GRAPH REPRESENTATIONS
# =============================================================================

# Adjacency List - Most common, efficient for sparse graphs
class Graph:
    """Graph using adjacency list representation"""

    def __init__(self, directed: bool = False):
        self.adj: Dict[str, List[str]] = defaultdict(list)
        self.directed = directed

    def add_edge(self, u: str, v: str) -> None:
        self.adj[u].append(v)
        if not self.directed:
            self.adj[v].append(u)
        # Ensure both nodes exist even if isolated
        if v not in self.adj:
            self.adj[v] = []

    def neighbors(self, node: str) -> List[str]:
        return self.adj[node]

    def nodes(self) -> List[str]:
        return list(self.adj.keys())

    def __repr__(self) -> str:
        return "\n".join(f"{k}: {v}" for k, v in self.adj.items())


print("=== Adjacency List Representation ===")
g = Graph()
edges = [("A", "B"), ("A", "C"), ("B", "D"), ("C", "D"), ("D", "E")]
for u, v in edges:
    g.add_edge(u, v)
print(g)


# Adjacency Matrix - Good for dense graphs, O(1) edge lookup
class GraphMatrix:
    """Graph using adjacency matrix"""

    def __init__(self, nodes: List[str]):
        self.nodes = nodes
        self.node_idx = {node: i for i, node in enumerate(nodes)}
        n = len(nodes)
        self.matrix = [[0] * n for _ in range(n)]

    def add_edge(self, u: str, v: str, weight: int = 1) -> None:
        i, j = self.node_idx[u], self.node_idx[v]
        self.matrix[i][j] = weight
        self.matrix[j][i] = weight  # Undirected

    def has_edge(self, u: str, v: str) -> bool:
        i, j = self.node_idx[u], self.node_idx[v]
        return self.matrix[i][j] != 0

    def print_matrix(self) -> None:
        print("   " + " ".join(self.nodes))
        for i, node in enumerate(self.nodes):
            print(f"{node}: {self.matrix[i]}")


print("\n=== Adjacency Matrix Representation ===")
gm = GraphMatrix(["A", "B", "C", "D"])
gm.add_edge("A", "B")
gm.add_edge("A", "C")
gm.add_edge("B", "D")
gm.print_matrix()


# =============================================================================
# DEPTH-FIRST SEARCH (DFS)
# =============================================================================

def dfs_recursive(graph: Graph, start: str, visited: Set[str] = None) -> List[str]:
    """DFS using recursion"""
    if visited is None:
        visited = set()

    visited.add(start)
    result = [start]

    for neighbor in graph.neighbors(start):
        if neighbor not in visited:
            result.extend(dfs_recursive(graph, neighbor, visited))

    return result


def dfs_iterative(graph: Graph, start: str) -> List[str]:
    """DFS using explicit stack"""
    visited = set()
    stack = [start]
    result = []

    while stack:
        node = stack.pop()
        if node not in visited:
            visited.add(node)
            result.append(node)
            # Add neighbors in reverse for consistent order
            for neighbor in reversed(graph.neighbors(node)):
                if neighbor not in visited:
                    stack.append(neighbor)

    return result


print("\n=== Depth-First Search (DFS) ===")
print(f"DFS recursive from A: {dfs_recursive(g, 'A')}")
print(f"DFS iterative from A: {dfs_iterative(g, 'A')}")


# =============================================================================
# BREADTH-FIRST SEARCH (BFS)
# =============================================================================

def bfs(graph: Graph, start: str) -> List[str]:
    """BFS using queue - explores level by level"""
    visited = {start}
    queue = deque([start])
    result = []

    while queue:
        node = queue.popleft()
        result.append(node)

        for neighbor in graph.neighbors(node):
            if neighbor not in visited:
                visited.add(neighbor)
                queue.append(neighbor)

    return result


def bfs_with_levels(graph: Graph, start: str) -> List[List[str]]:
    """BFS returning nodes by level/distance"""
    visited = {start}
    queue = deque([start])
    levels = []

    while queue:
        level = []
        for _ in range(len(queue)):
            node = queue.popleft()
            level.append(node)

            for neighbor in graph.neighbors(node):
                if neighbor not in visited:
                    visited.add(neighbor)
                    queue.append(neighbor)

        levels.append(level)

    return levels


print("\n=== Breadth-First Search (BFS) ===")
print(f"BFS from A: {bfs(g, 'A')}")
print(f"BFS levels from A: {bfs_with_levels(g, 'A')}")


# =============================================================================
# SHORTEST PATH (Unweighted)
# =============================================================================

def shortest_path_bfs(graph: Graph, start: str, end: str) -> Optional[List[str]]:
    """Find shortest path using BFS (unweighted)"""
    if start == end:
        return [start]

    visited = {start}
    queue = deque([(start, [start])])

    while queue:
        node, path = queue.popleft()

        for neighbor in graph.neighbors(node):
            if neighbor not in visited:
                new_path = path + [neighbor]
                if neighbor == end:
                    return new_path
                visited.add(neighbor)
                queue.append((neighbor, new_path))

    return None


print("\n=== Shortest Path (Unweighted) ===")
print(f"Shortest path A to E: {shortest_path_bfs(g, 'A', 'E')}")


# =============================================================================
# WEIGHTED GRAPH
# =============================================================================

class WeightedGraph:
    """Graph with weighted edges"""

    def __init__(self, directed: bool = False):
        self.adj: Dict[str, List[Tuple[str, int]]] = defaultdict(list)
        self.directed = directed

    def add_edge(self, u: str, v: str, weight: int) -> None:
        self.adj[u].append((v, weight))
        if not self.directed:
            self.adj[v].append((u, weight))

    def neighbors(self, node: str) -> List[Tuple[str, int]]:
        return self.adj[node]

    def nodes(self) -> List[str]:
        return list(self.adj.keys())


# =============================================================================
# DIJKSTRA'S ALGORITHM
# =============================================================================

def dijkstra(graph: WeightedGraph, start: str) -> Dict[str, int]:
    """
    Dijkstra's algorithm for shortest paths
    Returns distances from start to all reachable nodes
    O((V + E) log V) with binary heap
    """
    distances = {node: float('inf') for node in graph.nodes()}
    distances[start] = 0
    pq = [(0, start)]  # (distance, node)
    visited = set()

    while pq:
        dist, node = heapq.heappop(pq)

        if node in visited:
            continue
        visited.add(node)

        for neighbor, weight in graph.neighbors(node):
            new_dist = dist + weight
            if new_dist < distances[neighbor]:
                distances[neighbor] = new_dist
                heapq.heappush(pq, (new_dist, neighbor))

    return distances


def dijkstra_with_path(graph: WeightedGraph, start: str, end: str) -> Tuple[int, List[str]]:
    """Dijkstra's that also returns the path"""
    distances = {node: float('inf') for node in graph.nodes()}
    distances[start] = 0
    previous = {}
    pq = [(0, start)]
    visited = set()

    while pq:
        dist, node = heapq.heappop(pq)

        if node in visited:
            continue
        visited.add(node)

        if node == end:
            break

        for neighbor, weight in graph.neighbors(node):
            new_dist = dist + weight
            if new_dist < distances[neighbor]:
                distances[neighbor] = new_dist
                previous[neighbor] = node
                heapq.heappush(pq, (new_dist, neighbor))

    # Reconstruct path
    path = []
    current = end
    while current in previous:
        path.append(current)
        current = previous[current]
    path.append(start)
    path.reverse()

    return distances[end], path


print("\n=== Dijkstra's Algorithm ===")
wg = WeightedGraph()
wg.add_edge("A", "B", 4)
wg.add_edge("A", "C", 2)
wg.add_edge("B", "C", 1)
wg.add_edge("B", "D", 5)
wg.add_edge("C", "D", 8)
wg.add_edge("C", "E", 10)
wg.add_edge("D", "E", 2)

distances = dijkstra(wg, "A")
print(f"Distances from A: {distances}")

dist, path = dijkstra_with_path(wg, "A", "E")
print(f"Shortest path A to E: {path} (distance: {dist})")


# =============================================================================
# CYCLE DETECTION
# =============================================================================

def has_cycle_undirected(graph: Graph) -> bool:
    """Detect cycle in undirected graph using DFS"""
    visited = set()

    def dfs(node: str, parent: Optional[str]) -> bool:
        visited.add(node)
        for neighbor in graph.neighbors(node):
            if neighbor not in visited:
                if dfs(neighbor, node):
                    return True
            elif neighbor != parent:
                return True
        return False

    for node in graph.nodes():
        if node not in visited:
            if dfs(node, None):
                return True
    return False


def has_cycle_directed(graph: Graph) -> bool:
    """Detect cycle in directed graph using colors"""
    WHITE, GRAY, BLACK = 0, 1, 2
    colors = {node: WHITE for node in graph.nodes()}

    def dfs(node: str) -> bool:
        colors[node] = GRAY
        for neighbor in graph.neighbors(node):
            if colors[neighbor] == GRAY:
                return True  # Back edge = cycle
            if colors[neighbor] == WHITE and dfs(neighbor):
                return True
        colors[node] = BLACK
        return False

    for node in graph.nodes():
        if colors[node] == WHITE:
            if dfs(node):
                return True
    return False


print("\n=== Cycle Detection ===")
cycle_graph = Graph()
cycle_graph.add_edge("A", "B")
cycle_graph.add_edge("B", "C")
cycle_graph.add_edge("C", "A")  # Creates cycle
print(f"Graph with cycle: {has_cycle_undirected(cycle_graph)}")


# =============================================================================
# TOPOLOGICAL SORT (DAG)
# =============================================================================

def topological_sort(graph: Graph) -> List[str]:
    """
    Topological sort using DFS
    Only valid for DAGs (Directed Acyclic Graphs)
    """
    visited = set()
    result = []

    def dfs(node: str):
        visited.add(node)
        for neighbor in graph.neighbors(node):
            if neighbor not in visited:
                dfs(neighbor)
        result.append(node)

    for node in graph.nodes():
        if node not in visited:
            dfs(node)

    result.reverse()
    return result


def topological_sort_kahn(graph: Graph) -> List[str]:
    """Topological sort using Kahn's algorithm (BFS)"""
    # Calculate in-degrees
    in_degree = defaultdict(int)
    for node in graph.nodes():
        for neighbor in graph.neighbors(node):
            in_degree[neighbor] += 1

    # Start with nodes that have no incoming edges
    queue = deque([node for node in graph.nodes() if in_degree[node] == 0])
    result = []

    while queue:
        node = queue.popleft()
        result.append(node)
        for neighbor in graph.neighbors(node):
            in_degree[neighbor] -= 1
            if in_degree[neighbor] == 0:
                queue.append(neighbor)

    return result if len(result) == len(graph.nodes()) else []  # Empty if cycle


print("\n=== Topological Sort ===")
dag = Graph(directed=True)
dag.add_edge("A", "C")
dag.add_edge("B", "C")
dag.add_edge("B", "D")
dag.add_edge("C", "E")
dag.add_edge("D", "F")
dag.add_edge("E", "F")
print(f"Topological order: {topological_sort(dag)}")


# =============================================================================
# CONNECTED COMPONENTS
# =============================================================================

def connected_components(graph: Graph) -> List[List[str]]:
    """Find all connected components in undirected graph"""
    visited = set()
    components = []

    def dfs(node: str, component: List[str]):
        visited.add(node)
        component.append(node)
        for neighbor in graph.neighbors(node):
            if neighbor not in visited:
                dfs(neighbor, component)

    for node in graph.nodes():
        if node not in visited:
            component = []
            dfs(node, component)
            components.append(component)

    return components


print("\n=== Connected Components ===")
disconnected = Graph()
disconnected.add_edge("A", "B")
disconnected.add_edge("B", "C")
disconnected.add_edge("D", "E")  # Separate component
disconnected.add_edge("F", "F")  # Isolated (self-loop for node existence)
# Add isolated node G
disconnected.adj["G"] = []
print(f"Components: {connected_components(disconnected)}")


# =============================================================================
# MINIMUM SPANNING TREE (PRIM'S)
# =============================================================================

def prim_mst(graph: WeightedGraph) -> List[Tuple[str, str, int]]:
    """
    Prim's algorithm for Minimum Spanning Tree
    Returns list of edges in MST
    """
    if not graph.nodes():
        return []

    start = graph.nodes()[0]
    visited = {start}
    edges = []
    pq = [(weight, start, neighbor) for neighbor, weight in graph.neighbors(start)]
    heapq.heapify(pq)

    while pq and len(visited) < len(graph.nodes()):
        weight, u, v = heapq.heappop(pq)
        if v in visited:
            continue

        visited.add(v)
        edges.append((u, v, weight))

        for neighbor, w in graph.neighbors(v):
            if neighbor not in visited:
                heapq.heappush(pq, (w, v, neighbor))

    return edges


print("\n=== Minimum Spanning Tree (Prim's) ===")
mst_edges = prim_mst(wg)
total_weight = sum(e[2] for e in mst_edges)
print(f"MST edges: {mst_edges}")
print(f"Total weight: {total_weight}")


# =============================================================================
# COMPARISON
# =============================================================================

print("\n=== Graph Algorithm Summary ===")
print("""
Traversals:
  - DFS: O(V + E) - uses stack/recursion
  - BFS: O(V + E) - uses queue, finds shortest path (unweighted)

Shortest Path:
  - BFS: O(V + E) - unweighted graphs
  - Dijkstra: O((V + E) log V) - weighted graphs, non-negative weights
  - Bellman-Ford: O(V * E) - handles negative weights

Minimum Spanning Tree:
  - Prim's: O((V + E) log V) - good for dense graphs
  - Kruskal's: O(E log E) - good for sparse graphs

Topological Sort:
  - DFS: O(V + E) - for DAGs only
  - Kahn's (BFS): O(V + E) - also detects cycles

Connected Components:
  - DFS/BFS: O(V + E)

Representation:
  - Adjacency List: O(V + E) space, good for sparse
  - Adjacency Matrix: O(V²) space, O(1) edge lookup
""")
