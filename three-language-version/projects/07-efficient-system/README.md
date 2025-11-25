# Project 7: Efficient System

**Quarter 3, Weeks 1-4**
**Prerequisites:** Lessons 1-13 (through Sorting & Searching)

---

## The Big Idea

Build something where **efficiency matters**. Choose good algorithms, measure performance, and make it fast. This is where Big-O theory meets reality.

Sometimes "it works" isn't good enough. Sometimes you need it to work *fast*.

---

## Base Requirements

Your efficient system must:

1. **Solve a problem where performance matters** (lots of data or repeated operations)
2. **Use appropriate algorithms** (not naive O(n²) when O(n log n) exists)
3. **Measure performance** (timing, operation counts)
4. **Handle scale** (work well on large inputs)
5. **Document complexity** (analyze your algorithm choices)

### Core Features
- Process significant amounts of data (1000+ items typical)
- Demonstrate algorithmic thinking
- Include benchmarks comparing approaches
- Work correctly AND efficiently

---

## Project Ideas

### Search & Retrieval
- **Search engine**: Index documents, fast keyword search
- **Autocomplete**: Trie-based word completion
- **Spell checker**: Fast dictionary lookup with suggestions
- **Database query engine**: Simple SQL-like queries on data

### Data Processing
- **Log analyzer**: Process large log files efficiently
- **Duplicate finder**: Find duplicates in large datasets
- **Data deduplication**: Efficient storage without redundancy
- **Stream processor**: Handle continuous data efficiently

### Spatial/Geometric
- **Collision detection**: Efficient overlap checking
- **Nearest neighbor**: Find closest points
- **Path finder**: A*, Dijkstra on large graphs
- **Range queries**: Find all items in a region

### Compression & Encoding
- **Text compressor**: Huffman coding, RLE, etc.
- **File differ**: Efficient diff algorithm
- **Data encoder**: Efficient serialization

### Games & Simulations
- **Game AI**: Minimax with alpha-beta pruning
- **Physics engine**: Efficient collision handling
- **Procedural generator**: Fast terrain/dungeon generation

### Caching & Memory
- **Cache implementation**: LRU, LFU cache
- **Memory allocator**: Pool allocator, slab allocator
- **Memoization framework**: Automatic result caching

---

## Example: Search Engine

```
═══════════════════════════════════════════════════════════════
              MINI SEARCH ENGINE
═══════════════════════════════════════════════════════════════

INDEXING...
  Files scanned: 1,000
  Total words: 523,847
  Unique terms: 28,456
  Index size: 2.3 MB
  Index time: 1.24 seconds

INDEX STRUCTURE:
  Using inverted index with term frequency
  Sorted posting lists for fast intersection

> search "algorithm complexity"

SEARCH RESULTS (0.003 seconds)
══════════════════════════════

Found 23 documents containing both terms:

1. algorithms/analysis.txt (score: 0.92)
   "...time complexity of the algorithm determines..."

2. cs101/lecture5.txt (score: 0.87)
   "...analyzing algorithm complexity requires..."

3. papers/efficiency.pdf (score: 0.71)
   "...the complexity of this algorithm is O(n log n)..."

[Showing 3 of 23 results]

PERFORMANCE:
  Query parse: 0.0001s
  Index lookup: 0.0015s
  Ranking: 0.0008s
  Total: 0.003s

> benchmark

BENCHMARK: 1000 random queries
══════════════════════════════
  Average query time: 0.0042s
  Median query time: 0.0031s
  95th percentile: 0.0089s
  Throughput: 238 queries/second

Compared to naive linear search:
  Naive average: 0.89s
  Speedup: 212x
```

---

## Example: Autocomplete System

```
═══════════════════════════════════════════════════════════════
              AUTOCOMPLETE ENGINE
═══════════════════════════════════════════════════════════════

Loading dictionary...
  Words loaded: 178,691
  Trie nodes: 423,892
  Memory usage: 8.2 MB
  Build time: 0.34 seconds

TYPE TO SEARCH (tab for completion, ctrl-c to quit):

> prog_
COMPLETIONS (0.0002s):
  program (freq: 8,432)
  programming (freq: 5,211)
  progress (freq: 3,987)
  programmer (freq: 2,156)
  progressive (freq: 1,823)
[5 of 12 matches]

> program_
COMPLETIONS (0.0001s):
  programming (freq: 5,211)
  programmer (freq: 2,156)
  programmatic (freq: 421)
  programmable (freq: 398)
[4 matches]

> benchmark prefix_length=3

BENCHMARK: Autocomplete performance
═══════════════════════════════════
  Prefix length 1: 0.021s avg (many results)
  Prefix length 2: 0.003s avg
  Prefix length 3: 0.0004s avg
  Prefix length 4: 0.0001s avg

  Trie lookup: O(m) where m = prefix length
  Result retrieval: O(k) where k = results returned

vs Linear search (scanning all words):
  Any prefix: 0.089s avg
  Speedup: 89-890x depending on prefix length
```

---

## Technical Requirements

### Algorithm Selection
- Use appropriate data structures (hash tables, trees, heaps, etc.)
- Choose algorithms matching your problem's complexity needs
- Justify your choices in documentation

### Benchmarking
- Measure execution time for operations
- Test with varying input sizes
- Compare naive vs optimized approaches
- Report results clearly

### Complexity Analysis
- Document Big-O for key operations
- Explain space vs time tradeoffs
- Identify bottlenecks

### Scalability
- System should work on inputs 10-100x larger than demos
- Performance should degrade gracefully, not catastrophically

---

## Creative Extensions

### Optimization (+5 each)
- **Multiple algorithms**: Compare approaches on same problem
- **Parallel processing**: Multi-threaded for speedup
- **Cache optimization**: Memory-aware algorithms
- **Incremental updates**: Efficient modifications

### Analysis (+5 each)
- **Visualization**: Graph performance curves
- **Profiling**: Identify hotspots in code
- **Memory tracking**: Measure space usage
- **Comparison charts**: Clear speedup demonstrations

### Scale (+5 each)
- **Large dataset handling**: Work with 100K+ items
- **Streaming**: Process without loading all to memory
- **Persistence**: Save/load optimized structures
- **Real-world data**: Use actual datasets

### Rust Bonus (+10)
- Implement in Rust
- Leverage ownership for efficiency
- Compare with C++ approach

---

## Getting Started

### Step 1: Choose Your Problem
What problem benefits from efficiency? Where does scale matter?

### Step 2: Understand the Naive Approach
Implement the simple O(n²) or O(n) brute force first. Measure it.

### Step 3: Research Better Algorithms
What data structures help? What's the theoretical best complexity?

### Step 4: Implement Optimized Version
Build the efficient solution.

### Step 5: Benchmark and Compare
Prove your optimization works. Show the speedup.

### Step 6: Document Your Analysis
Explain why it's faster. What's the complexity?

---

## Language Hints

### Python
```python
import time
from collections import defaultdict

class InvertedIndex:
    def __init__(self):
        self.index = defaultdict(list)  # term -> [doc_ids]

    def add_document(self, doc_id, text):
        for word in text.lower().split():
            if doc_id not in self.index[word]:
                self.index[word].append(doc_id)

    def search(self, query):
        terms = query.lower().split()
        if not terms:
            return []

        # Start with first term's documents
        result = set(self.index[terms[0]])

        # Intersect with remaining terms
        for term in terms[1:]:
            result &= set(self.index[term])

        return list(result)

def benchmark(func, *args, iterations=100):
    start = time.perf_counter()
    for _ in range(iterations):
        func(*args)
    elapsed = time.perf_counter() - start
    return elapsed / iterations
```

### C++
```cpp
#include <chrono>
#include <unordered_map>
#include <vector>

class Trie {
    struct Node {
        std::unordered_map<char, std::unique_ptr<Node>> children;
        bool isWord = false;
        int frequency = 0;
    };
    std::unique_ptr<Node> root = std::make_unique<Node>();

public:
    void insert(const std::string& word, int freq = 1) {
        Node* current = root.get();
        for (char c : word) {
            if (!current->children[c]) {
                current->children[c] = std::make_unique<Node>();
            }
            current = current->children[c].get();
        }
        current->isWord = true;
        current->frequency = freq;
    }

    std::vector<std::string> autocomplete(const std::string& prefix);
};

template<typename Func>
double benchmark(Func f, int iterations = 100) {
    auto start = std::chrono::high_resolution_clock::now();
    for (int i = 0; i < iterations; i++) f();
    auto end = std::chrono::high_resolution_clock::now();
    return std::chrono::duration<double>(end - start).count() / iterations;
}
```

### Haskell
```haskell
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Time.Clock

-- Inverted index using Maps
type InvertedIndex = M.Map String (S.Set Int)

buildIndex :: [(Int, String)] -> InvertedIndex
buildIndex docs = foldr addDoc M.empty docs
  where
    addDoc (docId, text) idx =
        foldr (addWord docId) idx (words $ map toLower text)
    addWord docId word idx =
        M.insertWith S.union word (S.singleton docId) idx

search :: InvertedIndex -> String -> S.Set Int
search idx query =
    case map (flip M.lookup idx) (words $ map toLower query) of
        [] -> S.empty
        (Nothing:_) -> S.empty
        (Just first:rest) -> foldr intersectMaybe first rest
  where
    intersectMaybe Nothing s = S.empty
    intersectMaybe (Just s1) s2 = S.intersection s1 s2

benchmark :: IO a -> IO Double
benchmark action = do
    start <- getCurrentTime
    replicateM_ 100 action
    end <- getCurrentTime
    return $ realToFrac (diffUTCTime end start) / 100
```

---

## Reflection Questions

Answer in your `REFLECTION.md`:

1. **What problem did you solve and why efficiency matters for it?**

2. **What's the complexity of your naive vs optimized solution?** Show the Big-O analysis.

3. **What speedup did you achieve?** Share benchmark results.

4. **What data structures were key?** Why did you choose them?

5. **What would you need to scale to 1 million items?** What would change?

6. **Where did theory match reality?** Any surprises in actual performance?

---

## Submission Checklist

- [ ] System solves a performance-sensitive problem
- [ ] Uses appropriate algorithms (not naive)
- [ ] Benchmarks demonstrate improvement
- [ ] Complexity is documented (Big-O)
- [ ] Works on large inputs (1000+)
- [ ] Code is clean and well-organized
- [ ] Git history shows development
- [ ] REFLECTION.md completed
- [ ] Ready for showcase!

---

## Showcase Presentation

5-7 minutes:
1. **The problem** (1 min) - Why does efficiency matter here?
2. **Naive vs optimized** (2-3 min) - Show the difference, explain the algorithm
3. **Benchmarks** (2 min) - Prove it's faster, show the numbers
4. **What you learned** (1 min) - Insights about algorithms and performance

---

**Make it fast. Prove it's fast. Explain why it's fast!**
