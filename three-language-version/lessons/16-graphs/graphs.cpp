/**
 * Lesson 16: Graphs - C++ Examples
 * Graph representations, traversals, and algorithms
 */

#include <iostream>
#include <vector>
#include <queue>
#include <stack>
#include <unordered_map>
#include <unordered_set>
#include <set>
#include <algorithm>
#include <limits>
#include <functional>

using namespace std;

// =============================================================================
// ADJACENCY LIST GRAPH
// =============================================================================

class Graph {
public:
    unordered_map<string, vector<string>> adj;
    bool directed;

    Graph(bool dir = false) : directed(dir) {}

    void addEdge(const string& u, const string& v) {
        adj[u].push_back(v);
        if (!directed) {
            adj[v].push_back(u);
        }
        // Ensure both nodes exist
        if (adj.find(v) == adj.end()) {
            adj[v] = {};
        }
    }

    vector<string> neighbors(const string& node) const {
        auto it = adj.find(node);
        return it != adj.end() ? it->second : vector<string>{};
    }

    vector<string> nodes() const {
        vector<string> result;
        for (const auto& p : adj) {
            result.push_back(p.first);
        }
        return result;
    }

    void print() const {
        for (const auto& p : adj) {
            cout << p.first << ": ";
            for (const auto& n : p.second) {
                cout << n << " ";
            }
            cout << endl;
        }
    }
};

void graph_representation_demo() {
    cout << "=== Adjacency List Representation ===" << endl;

    Graph g;
    g.addEdge("A", "B");
    g.addEdge("A", "C");
    g.addEdge("B", "D");
    g.addEdge("C", "D");
    g.addEdge("D", "E");

    g.print();
}

// =============================================================================
// DEPTH-FIRST SEARCH (DFS)
// =============================================================================

void dfsRecursive(const Graph& g, const string& node,
                  unordered_set<string>& visited, vector<string>& result) {
    visited.insert(node);
    result.push_back(node);

    for (const auto& neighbor : g.neighbors(node)) {
        if (visited.find(neighbor) == visited.end()) {
            dfsRecursive(g, neighbor, visited, result);
        }
    }
}

vector<string> dfs(const Graph& g, const string& start) {
    unordered_set<string> visited;
    vector<string> result;
    dfsRecursive(g, start, visited, result);
    return result;
}

vector<string> dfsIterative(const Graph& g, const string& start) {
    unordered_set<string> visited;
    vector<string> result;
    stack<string> stk;
    stk.push(start);

    while (!stk.empty()) {
        string node = stk.top();
        stk.pop();

        if (visited.find(node) != visited.end()) continue;
        visited.insert(node);
        result.push_back(node);

        auto neighbors = g.neighbors(node);
        for (auto it = neighbors.rbegin(); it != neighbors.rend(); ++it) {
            if (visited.find(*it) == visited.end()) {
                stk.push(*it);
            }
        }
    }
    return result;
}

void dfs_demo() {
    cout << "\n=== Depth-First Search (DFS) ===" << endl;

    Graph g;
    g.addEdge("A", "B");
    g.addEdge("A", "C");
    g.addEdge("B", "D");
    g.addEdge("C", "D");
    g.addEdge("D", "E");

    cout << "DFS recursive from A: ";
    for (const auto& node : dfs(g, "A")) cout << node << " ";
    cout << endl;

    cout << "DFS iterative from A: ";
    for (const auto& node : dfsIterative(g, "A")) cout << node << " ";
    cout << endl;
}

// =============================================================================
// BREADTH-FIRST SEARCH (BFS)
// =============================================================================

vector<string> bfs(const Graph& g, const string& start) {
    unordered_set<string> visited;
    vector<string> result;
    queue<string> q;

    visited.insert(start);
    q.push(start);

    while (!q.empty()) {
        string node = q.front();
        q.pop();
        result.push_back(node);

        for (const auto& neighbor : g.neighbors(node)) {
            if (visited.find(neighbor) == visited.end()) {
                visited.insert(neighbor);
                q.push(neighbor);
            }
        }
    }
    return result;
}

vector<string> shortestPathBFS(const Graph& g, const string& start, const string& end) {
    if (start == end) return {start};

    unordered_set<string> visited;
    queue<pair<string, vector<string>>> q;

    visited.insert(start);
    q.push({start, {start}});

    while (!q.empty()) {
        auto [node, path] = q.front();
        q.pop();

        for (const auto& neighbor : g.neighbors(node)) {
            if (visited.find(neighbor) == visited.end()) {
                vector<string> newPath = path;
                newPath.push_back(neighbor);

                if (neighbor == end) {
                    return newPath;
                }

                visited.insert(neighbor);
                q.push({neighbor, newPath});
            }
        }
    }
    return {};  // No path found
}

void bfs_demo() {
    cout << "\n=== Breadth-First Search (BFS) ===" << endl;

    Graph g;
    g.addEdge("A", "B");
    g.addEdge("A", "C");
    g.addEdge("B", "D");
    g.addEdge("C", "D");
    g.addEdge("D", "E");

    cout << "BFS from A: ";
    for (const auto& node : bfs(g, "A")) cout << node << " ";
    cout << endl;

    cout << "Shortest path A to E: ";
    for (const auto& node : shortestPathBFS(g, "A", "E")) cout << node << " ";
    cout << endl;
}

// =============================================================================
// WEIGHTED GRAPH
// =============================================================================

class WeightedGraph {
public:
    unordered_map<string, vector<pair<string, int>>> adj;

    void addEdge(const string& u, const string& v, int weight) {
        adj[u].push_back({v, weight});
        adj[v].push_back({u, weight});  // Undirected
    }

    vector<pair<string, int>> neighbors(const string& node) const {
        auto it = adj.find(node);
        return it != adj.end() ? it->second : vector<pair<string, int>>{};
    }

    vector<string> nodes() const {
        vector<string> result;
        for (const auto& p : adj) {
            result.push_back(p.first);
        }
        return result;
    }
};

// =============================================================================
// DIJKSTRA'S ALGORITHM
// =============================================================================

unordered_map<string, int> dijkstra(const WeightedGraph& g, const string& start) {
    unordered_map<string, int> distances;
    for (const auto& node : g.nodes()) {
        distances[node] = numeric_limits<int>::max();
    }
    distances[start] = 0;

    // Min-heap: (distance, node)
    priority_queue<pair<int, string>, vector<pair<int, string>>, greater<>> pq;
    pq.push({0, start});

    unordered_set<string> visited;

    while (!pq.empty()) {
        auto [dist, node] = pq.top();
        pq.pop();

        if (visited.count(node)) continue;
        visited.insert(node);

        for (const auto& [neighbor, weight] : g.neighbors(node)) {
            int newDist = dist + weight;
            if (newDist < distances[neighbor]) {
                distances[neighbor] = newDist;
                pq.push({newDist, neighbor});
            }
        }
    }

    return distances;
}

pair<int, vector<string>> dijkstraWithPath(const WeightedGraph& g,
                                           const string& start, const string& end) {
    unordered_map<string, int> distances;
    unordered_map<string, string> previous;

    for (const auto& node : g.nodes()) {
        distances[node] = numeric_limits<int>::max();
    }
    distances[start] = 0;

    priority_queue<pair<int, string>, vector<pair<int, string>>, greater<>> pq;
    pq.push({0, start});
    unordered_set<string> visited;

    while (!pq.empty()) {
        auto [dist, node] = pq.top();
        pq.pop();

        if (visited.count(node)) continue;
        visited.insert(node);

        if (node == end) break;

        for (const auto& [neighbor, weight] : g.neighbors(node)) {
            int newDist = dist + weight;
            if (newDist < distances[neighbor]) {
                distances[neighbor] = newDist;
                previous[neighbor] = node;
                pq.push({newDist, neighbor});
            }
        }
    }

    // Reconstruct path
    vector<string> path;
    string current = end;
    while (previous.count(current)) {
        path.push_back(current);
        current = previous[current];
    }
    path.push_back(start);
    reverse(path.begin(), path.end());

    return {distances[end], path};
}

void dijkstra_demo() {
    cout << "\n=== Dijkstra's Algorithm ===" << endl;

    WeightedGraph wg;
    wg.addEdge("A", "B", 4);
    wg.addEdge("A", "C", 2);
    wg.addEdge("B", "C", 1);
    wg.addEdge("B", "D", 5);
    wg.addEdge("C", "D", 8);
    wg.addEdge("C", "E", 10);
    wg.addEdge("D", "E", 2);

    auto distances = dijkstra(wg, "A");
    cout << "Distances from A: ";
    for (const auto& [node, dist] : distances) {
        cout << node << "=" << dist << " ";
    }
    cout << endl;

    auto [dist, path] = dijkstraWithPath(wg, "A", "E");
    cout << "Shortest path A to E: ";
    for (const auto& node : path) cout << node << " ";
    cout << "(distance: " << dist << ")" << endl;
}

// =============================================================================
// CYCLE DETECTION
// =============================================================================

bool hasCycleUndirectedDFS(const Graph& g, const string& node,
                           const string& parent, unordered_set<string>& visited) {
    visited.insert(node);
    for (const auto& neighbor : g.neighbors(node)) {
        if (visited.find(neighbor) == visited.end()) {
            if (hasCycleUndirectedDFS(g, neighbor, node, visited)) {
                return true;
            }
        } else if (neighbor != parent) {
            return true;
        }
    }
    return false;
}

bool hasCycleUndirected(const Graph& g) {
    unordered_set<string> visited;
    for (const auto& node : g.nodes()) {
        if (visited.find(node) == visited.end()) {
            if (hasCycleUndirectedDFS(g, node, "", visited)) {
                return true;
            }
        }
    }
    return false;
}

void cycle_demo() {
    cout << "\n=== Cycle Detection ===" << endl;

    Graph cycleGraph;
    cycleGraph.addEdge("A", "B");
    cycleGraph.addEdge("B", "C");
    cycleGraph.addEdge("C", "A");  // Creates cycle

    cout << "Graph has cycle: " << boolalpha << hasCycleUndirected(cycleGraph) << endl;

    Graph acyclic;
    acyclic.addEdge("A", "B");
    acyclic.addEdge("B", "C");
    cout << "Acyclic graph has cycle: " << hasCycleUndirected(acyclic) << endl;
}

// =============================================================================
// TOPOLOGICAL SORT
// =============================================================================

void topSortDFS(const Graph& g, const string& node,
                unordered_set<string>& visited, vector<string>& result) {
    visited.insert(node);
    for (const auto& neighbor : g.neighbors(node)) {
        if (visited.find(neighbor) == visited.end()) {
            topSortDFS(g, neighbor, visited, result);
        }
    }
    result.push_back(node);
}

vector<string> topologicalSort(const Graph& g) {
    unordered_set<string> visited;
    vector<string> result;

    for (const auto& node : g.nodes()) {
        if (visited.find(node) == visited.end()) {
            topSortDFS(g, node, visited, result);
        }
    }

    reverse(result.begin(), result.end());
    return result;
}

void topsort_demo() {
    cout << "\n=== Topological Sort ===" << endl;

    Graph dag(true);  // Directed
    dag.addEdge("A", "C");
    dag.addEdge("B", "C");
    dag.addEdge("B", "D");
    dag.addEdge("C", "E");
    dag.addEdge("D", "F");
    dag.addEdge("E", "F");

    cout << "Topological order: ";
    for (const auto& node : topologicalSort(dag)) cout << node << " ";
    cout << endl;
}

// =============================================================================
// CONNECTED COMPONENTS
// =============================================================================

void componentDFS(const Graph& g, const string& node,
                  unordered_set<string>& visited, vector<string>& component) {
    visited.insert(node);
    component.push_back(node);
    for (const auto& neighbor : g.neighbors(node)) {
        if (visited.find(neighbor) == visited.end()) {
            componentDFS(g, neighbor, visited, component);
        }
    }
}

vector<vector<string>> connectedComponents(const Graph& g) {
    unordered_set<string> visited;
    vector<vector<string>> components;

    for (const auto& node : g.nodes()) {
        if (visited.find(node) == visited.end()) {
            vector<string> component;
            componentDFS(g, node, visited, component);
            components.push_back(component);
        }
    }

    return components;
}

void components_demo() {
    cout << "\n=== Connected Components ===" << endl;

    Graph disconnected;
    disconnected.addEdge("A", "B");
    disconnected.addEdge("B", "C");
    disconnected.addEdge("D", "E");
    disconnected.adj["F"] = {};  // Isolated node

    auto components = connectedComponents(disconnected);
    cout << "Components: ";
    for (const auto& comp : components) {
        cout << "[";
        for (const auto& node : comp) cout << node << " ";
        cout << "] ";
    }
    cout << endl;
}

// =============================================================================
// MINIMUM SPANNING TREE (Prim's)
// =============================================================================

vector<tuple<string, string, int>> primMST(const WeightedGraph& g) {
    if (g.nodes().empty()) return {};

    string start = g.nodes()[0];
    unordered_set<string> visited;
    visited.insert(start);

    vector<tuple<string, string, int>> mst;

    // Min-heap: (weight, from, to)
    priority_queue<tuple<int, string, string>,
                   vector<tuple<int, string, string>>,
                   greater<>> pq;

    for (const auto& [neighbor, weight] : g.neighbors(start)) {
        pq.push({weight, start, neighbor});
    }

    while (!pq.empty() && visited.size() < g.nodes().size()) {
        auto [weight, u, v] = pq.top();
        pq.pop();

        if (visited.count(v)) continue;

        visited.insert(v);
        mst.push_back({u, v, weight});

        for (const auto& [neighbor, w] : g.neighbors(v)) {
            if (!visited.count(neighbor)) {
                pq.push({w, v, neighbor});
            }
        }
    }

    return mst;
}

void mst_demo() {
    cout << "\n=== Minimum Spanning Tree (Prim's) ===" << endl;

    WeightedGraph wg;
    wg.addEdge("A", "B", 4);
    wg.addEdge("A", "C", 2);
    wg.addEdge("B", "C", 1);
    wg.addEdge("B", "D", 5);
    wg.addEdge("C", "D", 8);
    wg.addEdge("D", "E", 2);

    auto mst = primMST(wg);
    int totalWeight = 0;
    cout << "MST edges: ";
    for (const auto& [u, v, w] : mst) {
        cout << "(" << u << "-" << v << ":" << w << ") ";
        totalWeight += w;
    }
    cout << "\nTotal weight: " << totalWeight << endl;
}

// =============================================================================
// MAIN
// =============================================================================

int main() {
    graph_representation_demo();
    dfs_demo();
    bfs_demo();
    dijkstra_demo();
    cycle_demo();
    topsort_demo();
    components_demo();
    mst_demo();

    cout << "\n=== Graph Algorithm Summary ===" << endl;
    cout << R"(
Traversals:
  - DFS: O(V + E) - stack/recursion
  - BFS: O(V + E) - queue, shortest path (unweighted)

Shortest Path:
  - BFS: O(V + E) - unweighted
  - Dijkstra: O((V + E) log V) - weighted, non-negative

MST:
  - Prim's: O((V + E) log V)
  - Kruskal's: O(E log E)

Topological Sort: O(V + E) - DAGs only
Connected Components: O(V + E)
)" << endl;

    return 0;
}
