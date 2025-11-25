-- Lesson 16: Graphs - Haskell Examples
-- Graph representations, traversals, and algorithms

module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (foldl', sortBy)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe, mapMaybe)

-- =============================================================================
-- GRAPH REPRESENTATION
-- =============================================================================

-- Adjacency list representation
type Graph a = Map a [a]

-- Create empty graph
emptyGraph :: Graph a
emptyGraph = Map.empty

-- Add edge (undirected)
addEdge :: Ord a => a -> a -> Graph a -> Graph a
addEdge u v g = Map.insertWith (++) v [u] $ Map.insertWith (++) u [v] g

-- Add directed edge
addDirectedEdge :: Ord a => a -> a -> Graph a -> Graph a
addDirectedEdge u v = Map.insertWith (++) u [v]

-- Get neighbors
neighbors :: Ord a => a -> Graph a -> [a]
neighbors node g = fromMaybe [] $ Map.lookup node g

-- Get all nodes
nodes :: Graph a -> [a]
nodes = Map.keys

-- Build graph from edge list
fromEdges :: Ord a => [(a, a)] -> Graph a
fromEdges = foldr (uncurry addEdge) emptyGraph

-- Sample graph
sampleGraph :: Graph String
sampleGraph = fromEdges [("A","B"), ("A","C"), ("B","D"), ("C","D"), ("D","E")]

graphDemo :: IO ()
graphDemo = do
    putStrLn "=== Graph Representation ==="
    putStrLn $ "Graph: " ++ show sampleGraph
    putStrLn $ "Neighbors of A: " ++ show (neighbors "A" sampleGraph)

-- =============================================================================
-- DEPTH-FIRST SEARCH (DFS)
-- =============================================================================

dfs :: Ord a => Graph a -> a -> [a]
dfs graph start = go Set.empty [start]
  where
    go _ [] = []
    go visited (x:xs)
        | x `Set.member` visited = go visited xs
        | otherwise = x : go (Set.insert x visited) (neighbors x graph ++ xs)

-- DFS that returns nodes in discovery order
dfsPath :: Ord a => Graph a -> a -> [a]
dfsPath graph start = reverse $ go Set.empty [start] []
  where
    go _ [] acc = acc
    go visited (x:xs) acc
        | x `Set.member` visited = go visited xs acc
        | otherwise = go (Set.insert x visited)
                        (neighbors x graph ++ xs)
                        (x : acc)

dfsDemo :: IO ()
dfsDemo = do
    putStrLn "\n=== Depth-First Search ==="
    putStrLn $ "DFS from A: " ++ show (dfs sampleGraph "A")

-- =============================================================================
-- BREADTH-FIRST SEARCH (BFS)
-- =============================================================================

bfs :: Ord a => Graph a -> a -> [a]
bfs graph start = go Set.empty [start]
  where
    go _ [] = []
    go visited queue =
        let (current, rest) = (head queue, tail queue)
        in if current `Set.member` visited
           then go visited rest
           else current : go (Set.insert current visited)
                             (rest ++ neighbors current graph)

-- BFS with levels
bfsLevels :: Ord a => Graph a -> a -> [[a]]
bfsLevels graph start = go (Set.singleton start) [start]
  where
    go _ [] = []
    go visited level =
        let nextNodes = concatMap (`neighbors` graph) level
            unvisited = filter (`Set.notMember` visited) nextNodes
            newVisited = foldr Set.insert visited unvisited
        in level : if null unvisited then [] else go newVisited unvisited

bfsDemo :: IO ()
bfsDemo = do
    putStrLn "\n=== Breadth-First Search ==="
    putStrLn $ "BFS from A: " ++ show (bfs sampleGraph "A")
    putStrLn $ "BFS levels from A: " ++ show (bfsLevels sampleGraph "A")

-- =============================================================================
-- SHORTEST PATH (Unweighted)
-- =============================================================================

shortestPath :: Ord a => Graph a -> a -> a -> Maybe [a]
shortestPath graph start end
    | start == end = Just [start]
    | otherwise = go (Set.singleton start) [(start, [start])]
  where
    go _ [] = Nothing
    go visited ((node, path):queue)
        | node == end = Just path
        | otherwise =
            let nexts = filter (`Set.notMember` visited) $ neighbors node graph
                newPaths = [(n, path ++ [n]) | n <- nexts]
                newVisited = foldr Set.insert visited nexts
            in go newVisited (queue ++ newPaths)

pathDemo :: IO ()
pathDemo = do
    putStrLn "\n=== Shortest Path (Unweighted) ==="
    putStrLn $ "Shortest path A to E: " ++ show (shortestPath sampleGraph "A" "E")

-- =============================================================================
-- WEIGHTED GRAPH
-- =============================================================================

type WeightedGraph a = Map a [(a, Int)]

addWeightedEdge :: Ord a => a -> a -> Int -> WeightedGraph a -> WeightedGraph a
addWeightedEdge u v w g =
    Map.insertWith (++) v [(u, w)] $
    Map.insertWith (++) u [(v, w)] g

weightedNeighbors :: Ord a => a -> WeightedGraph a -> [(a, Int)]
weightedNeighbors node g = fromMaybe [] $ Map.lookup node g

sampleWeighted :: WeightedGraph String
sampleWeighted = foldr (\(u,v,w) -> addWeightedEdge u v w) Map.empty
    [("A","B",4), ("A","C",2), ("B","C",1), ("B","D",5),
     ("C","D",8), ("C","E",10), ("D","E",2)]

-- =============================================================================
-- DIJKSTRA'S ALGORITHM
-- =============================================================================

type Distance = Int
type Distances a = Map a Distance

dijkstra :: Ord a => WeightedGraph a -> a -> Distances a
dijkstra graph start = go initialDist (Set.singleton (0, start))
  where
    allNodes = Map.keys graph
    initialDist = Map.insert start 0 $ Map.fromList [(n, maxBound) | n <- allNodes]

    go distances pq
        | Set.null pq = distances
        | otherwise =
            let ((dist, node), pq') = Set.deleteFindMin pq
                neighs = weightedNeighbors node graph
                (newDist, newPQ) = foldr (relax dist node) (distances, pq') neighs
            in go newDist newPQ

    relax dist node (neighbor, weight) (dists, pq) =
        let newDist = dist + weight
            oldDist = fromMaybe maxBound $ Map.lookup neighbor dists
        in if newDist < oldDist
           then (Map.insert neighbor newDist dists, Set.insert (newDist, neighbor) pq)
           else (dists, pq)

dijkstraDemo :: IO ()
dijkstraDemo = do
    putStrLn "\n=== Dijkstra's Algorithm ==="
    let distances = dijkstra sampleWeighted "A"
    putStrLn $ "Distances from A: " ++ show (Map.toList distances)

-- =============================================================================
-- CYCLE DETECTION
-- =============================================================================

hasCycle :: Ord a => Graph a -> Bool
hasCycle graph = any (checkNode Set.empty Nothing) (nodes graph)
  where
    checkNode visited parent node
        | node `Set.member` visited = True
        | otherwise =
            let neighs = filter (/= fromMaybe node parent) $ neighbors node graph
                newVisited = Set.insert node visited
            in any (checkNode newVisited (Just node)) neighs

cycleDemo :: IO ()
cycleDemo = do
    putStrLn "\n=== Cycle Detection ==="
    let cycleGraph = fromEdges [("A","B"), ("B","C"), ("C","A")]
    let acyclic = fromEdges [("A","B"), ("B","C")]
    putStrLn $ "Cycle graph has cycle: " ++ show (hasCycle cycleGraph)
    putStrLn $ "Acyclic graph has cycle: " ++ show (hasCycle acyclic)

-- =============================================================================
-- TOPOLOGICAL SORT
-- =============================================================================

-- For directed graphs
type DirectedGraph a = Map a [a]

topologicalSort :: Ord a => DirectedGraph a -> [a]
topologicalSort graph = reverse $ snd $ foldl' visit (Set.empty, []) (Map.keys graph)
  where
    visit (visited, order) node
        | node `Set.member` visited = (visited, order)
        | otherwise =
            let neighs = fromMaybe [] $ Map.lookup node graph
                (visited', order') = foldl' visit (Set.insert node visited, order) neighs
            in (visited', node : order')

topSortDemo :: IO ()
topSortDemo = do
    putStrLn "\n=== Topological Sort ==="
    let dag = foldr (uncurry addDirectedEdge) emptyGraph
              [("A","C"), ("B","C"), ("B","D"), ("C","E"), ("D","F"), ("E","F")]
    putStrLn $ "Topological order: " ++ show (topologicalSort dag)

-- =============================================================================
-- CONNECTED COMPONENTS
-- =============================================================================

connectedComponents :: Ord a => Graph a -> [[a]]
connectedComponents graph = go (Set.fromList $ nodes graph) []
  where
    go remaining acc
        | Set.null remaining = acc
        | otherwise =
            let start = Set.findMin remaining
                component = dfs graph start
                newRemaining = foldr Set.delete remaining component
            in go newRemaining (component : acc)

componentsDemo :: IO ()
componentsDemo = do
    putStrLn "\n=== Connected Components ==="
    let disconnected = Map.fromList
            [("A", ["B"]), ("B", ["A", "C"]), ("C", ["B"]),
             ("D", ["E"]), ("E", ["D"]), ("F", [])]
    putStrLn $ "Components: " ++ show (connectedComponents disconnected)

-- =============================================================================
-- MINIMUM SPANNING TREE (Kruskal's with Union-Find simulation)
-- =============================================================================

type Edge a = (a, a, Int)

-- Simple Kruskal's (without optimized Union-Find)
kruskalMST :: Ord a => [Edge a] -> [a] -> [Edge a]
kruskalMST edges allNodes = go sortedEdges initialComponents []
  where
    sortedEdges = sortBy (comparing (\(_,_,w) -> w)) edges
    initialComponents = Map.fromList [(n, n) | n <- allNodes]

    findRoot comps node =
        let parent = comps Map.! node
        in if parent == node then node else findRoot comps parent

    go [] _ mst = mst
    go ((u, v, w):es) comps mst
        | rootU == rootV = go es comps mst  -- Same component, skip
        | otherwise = go es (Map.insert rootU rootV comps) ((u, v, w) : mst)
      where
        rootU = findRoot comps u
        rootV = findRoot comps v

mstDemo :: IO ()
mstDemo = do
    putStrLn "\n=== Minimum Spanning Tree (Kruskal's) ==="
    let edges = [("A","B",4), ("A","C",2), ("B","C",1), ("B","D",5),
                 ("C","D",8), ("D","E",2)]
    let allNodes = ["A", "B", "C", "D", "E"]
    let mst = kruskalMST edges allNodes
    let totalWeight = sum [w | (_,_,w) <- mst]
    putStrLn $ "MST edges: " ++ show mst
    putStrLn $ "Total weight: " ++ show totalWeight

-- =============================================================================
-- FUNCTIONAL GRAPH PATTERNS
-- =============================================================================

functionalPatterns :: IO ()
functionalPatterns = do
    putStrLn "\n=== Functional Graph Patterns ==="
    putStrLn ""
    putStrLn "1. Immutable graphs: Changes create new graphs"
    putStrLn "   let g' = addEdge \"X\" \"Y\" g"
    putStrLn ""
    putStrLn "2. Fold over graph:"
    let g = sampleGraph
    let nodeCount = Map.size g
    let edgeCount = sum (map length $ Map.elems g) `div` 2
    putStrLn $ "   Nodes: " ++ show nodeCount ++ ", Edges: " ++ show edgeCount
    putStrLn ""
    putStrLn "3. Graph transformations:"
    putStrLn "   - map over values: Map.map"
    putStrLn "   - filter edges: Map.map (filter predicate)"
    putStrLn ""
    putStrLn "4. Lazy traversal:"
    putStrLn "   - DFS/BFS return lazy lists"
    putStrLn "   - take 3 (dfs graph start) -- only compute 3 nodes"

-- =============================================================================
-- COMPLEXITY NOTES
-- =============================================================================

complexityNotes :: IO ()
complexityNotes = do
    putStrLn "\n=== Graph Algorithm Complexity ==="
    putStrLn ""
    putStrLn "Using Data.Map (balanced tree):"
    putStrLn "  - addEdge: O(log V)"
    putStrLn "  - neighbors lookup: O(log V)"
    putStrLn ""
    putStrLn "Traversals:"
    putStrLn "  - DFS/BFS: O((V + E) log V)"
    putStrLn "    (extra log V from Set operations)"
    putStrLn ""
    putStrLn "Dijkstra's:"
    putStrLn "  - O((V + E) log V) with Set as priority queue"
    putStrLn ""
    putStrLn "For better performance:"
    putStrLn "  - Use Data.IntMap for integer nodes"
    putStrLn "  - Use Data.Vector for dense graphs"
    putStrLn "  - Use fgl library for serious graph work"

-- =============================================================================
-- MAIN
-- =============================================================================

main :: IO ()
main = do
    graphDemo
    dfsDemo
    bfsDemo
    pathDemo
    dijkstraDemo
    cycleDemo
    topSortDemo
    componentsDemo
    mstDemo
    functionalPatterns
    complexityNotes
