-- Lesson 13: Sorting and Searching - Haskell Examples
-- Implementations and comparisons of fundamental algorithms

module Main where

import Data.List (sortBy, partition)
import Data.Ord (comparing)

-- =============================================================================
-- SEARCHING ALGORITHMS
-- =============================================================================

-- Linear Search - O(n)
linearSearch :: Eq a => a -> [a] -> Maybe Int
linearSearch target = go 0
  where
    go _ [] = Nothing
    go i (x:xs)
        | x == target = Just i
        | otherwise   = go (i + 1) xs

-- Binary Search - O(log n) for arrays, but O(n) for lists due to indexing!
-- Note: Use Data.Vector or Data.Array for true O(log n)
binarySearch :: Ord a => a -> [a] -> Maybe Int
binarySearch target arr = go 0 (length arr - 1)
  where
    go left right
        | left > right = Nothing
        | midVal == target = Just mid
        | midVal < target  = go (mid + 1) right
        | otherwise        = go left (mid - 1)
      where
        mid = (left + right) `div` 2
        midVal = arr !! mid  -- O(n) for lists!

-- More idiomatic Haskell: use elem for membership
elemSearch :: Eq a => a -> [a] -> Bool
elemSearch = elem

searchDemo :: IO ()
searchDemo = do
    putStrLn "=== Searching Algorithms ==="
    let numbers = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]
    putStrLn $ "Array: " ++ show numbers
    putStrLn $ "Linear search for 11: " ++ show (linearSearch 11 numbers)
    putStrLn $ "Binary search for 11: " ++ show (binarySearch 11 numbers)
    putStrLn $ "elem 11: " ++ show (11 `elem` numbers)

-- =============================================================================
-- BASIC SORTING ALGORITHMS - O(n²)
-- =============================================================================

-- Bubble Sort - O(n²)
bubbleSort :: Ord a => [a] -> [a]
bubbleSort [] = []
bubbleSort xs = bubbleSort (init bubbled) ++ [last bubbled]
  where
    bubbled = bubble xs
    bubble [x] = [x]
    bubble (x:y:rest)
        | x > y     = y : bubble (x:rest)
        | otherwise = x : bubble (y:rest)

-- Selection Sort - O(n²)
selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort xs = minVal : selectionSort rest
  where
    minVal = minimum xs
    rest = deleteFirst minVal xs

deleteFirst :: Eq a => a -> [a] -> [a]
deleteFirst _ [] = []
deleteFirst target (x:xs)
    | x == target = xs
    | otherwise   = x : deleteFirst target xs

-- Insertion Sort - O(n²)
insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr insert []
  where
    insert x [] = [x]
    insert x (y:ys)
        | x <= y    = x : y : ys
        | otherwise = y : insert x ys

basicSortDemo :: IO ()
basicSortDemo = do
    putStrLn "\n=== O(n²) Sorting Algorithms ==="
    let unsorted = [64, 34, 25, 12, 22, 11, 90]
    putStrLn $ "Original: " ++ show unsorted
    putStrLn $ "Bubble sort: " ++ show (bubbleSort unsorted)
    putStrLn $ "Selection sort: " ++ show (selectionSort unsorted)
    putStrLn $ "Insertion sort: " ++ show (insertionSort unsorted)

-- =============================================================================
-- EFFICIENT SORTING ALGORITHMS - O(n log n)
-- =============================================================================

-- Merge Sort - O(n log n)
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
  where
    (left, right) = splitAt (length xs `div` 2) xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

-- Quick Sort - O(n log n) average
-- Classic Haskell one-liner (not in-place)
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort smaller ++ [x] ++ quickSort larger
  where
    smaller = [y | y <- xs, y <= x]
    larger  = [y | y <- xs, y > x]

-- Quick Sort using partition
quickSortPartition :: Ord a => [a] -> [a]
quickSortPartition [] = []
quickSortPartition (x:xs) = quickSortPartition smaller ++ [x] ++ quickSortPartition larger
  where
    (smaller, larger) = partition (<= x) xs

-- Tree Sort - O(n log n)
-- Uses binary search tree
data Tree a = Empty | Node a (Tree a) (Tree a)

treeSort :: Ord a => [a] -> [a]
treeSort = flatten . foldr insert Empty
  where
    insert x Empty = Node x Empty Empty
    insert x (Node y left right)
        | x <= y    = Node y (insert x left) right
        | otherwise = Node y left (insert x right)

    flatten Empty = []
    flatten (Node x left right) = flatten left ++ [x] ++ flatten right

efficientSortDemo :: IO ()
efficientSortDemo = do
    putStrLn "\n=== O(n log n) Sorting Algorithms ==="
    let unsorted = [64, 34, 25, 12, 22, 11, 90]
    putStrLn $ "Original: " ++ show unsorted
    putStrLn $ "Merge sort: " ++ show (mergeSort unsorted)
    putStrLn $ "Quick sort: " ++ show (quickSort unsorted)
    putStrLn $ "Tree sort: " ++ show (treeSort unsorted)

-- =============================================================================
-- COUNTING SORT - O(n + k)
-- =============================================================================

countingSort :: [Int] -> [Int]
countingSort [] = []
countingSort xs = concatMap (\n -> replicate (count n) n) [0..maxVal]
  where
    maxVal = maximum xs
    count n = length $ filter (== n) xs

-- More efficient counting sort using arrays would use Data.Array

countingSortDemo :: IO ()
countingSortDemo = do
    putStrLn "\n=== Counting Sort O(n + k) ==="
    let arr = [4, 2, 2, 8, 3, 3, 1]
    putStrLn $ "Original: " ++ show arr
    putStrLn $ "Counting sort: " ++ show (countingSort arr)

-- =============================================================================
-- BUILT-IN SORTING
-- =============================================================================

builtInSortDemo :: IO ()
builtInSortDemo = do
    putStrLn "\n=== Haskell Built-in Sorting ==="

    -- Data.List.sort - Merge sort, O(n log n)
    let numbers = [5, 2, 8, 1, 9, 3, 7]
    putStrLn $ "sort: " ++ show (Data.List.sort numbers)

    -- sortBy with custom comparator
    let words' = ["banana", "apple", "cherry", "date"]
    putStrLn $ "sortBy length: " ++ show (sortBy (comparing length) words')

    -- sortOn for efficiency
    putStrLn $ "sortOn length: " ++ show (Data.List.sortOn length words')

    -- Descending order
    putStrLn $ "sort descending: " ++ show (Data.List.sortBy (flip compare) numbers)

-- =============================================================================
-- STABILITY
-- =============================================================================

stabilityDemo :: IO ()
stabilityDemo = do
    putStrLn "\n=== Sorting Stability ==="
    let students = [("Alice", 85), ("Bob", 90), ("Charlie", 85), ("Diana", 90)]
    putStrLn $ "Original: " ++ show students

    let sorted = sortBy (comparing snd) students
    putStrLn $ "Sorted by grade: " ++ show sorted
    putStrLn "Haskell's sort is stable - Alice before Charlie (both 85)"

-- =============================================================================
-- SEARCHING WITH HIGHER-ORDER FUNCTIONS
-- =============================================================================

higherOrderSearchDemo :: IO ()
higherOrderSearchDemo = do
    putStrLn "\n=== Higher-Order Search Functions ==="
    let numbers = [1, 3, 5, 7, 9, 11, 13, 15]

    -- find: first element matching predicate
    putStrLn $ "find (>5): " ++ show (find (> 5) numbers)

    -- filter: all elements matching predicate
    putStrLn $ "filter (>5): " ++ show (filter (> 5) numbers)

    -- takeWhile / dropWhile
    putStrLn $ "takeWhile (<10): " ++ show (takeWhile (< 10) numbers)
    putStrLn $ "dropWhile (<10): " ++ show (dropWhile (< 10) numbers)

    -- span: splits at first failure
    let (before, after) = span (< 10) numbers
    putStrLn $ "span (<10): " ++ show (before, after)

    -- break: splits at first success
    let (b, a) = break (> 10) numbers
    putStrLn $ "break (>10): " ++ show (b, a)
  where
    find :: (a -> Bool) -> [a] -> Maybe a
    find _ [] = Nothing
    find p (x:xs)
        | p x       = Just x
        | otherwise = find p xs

-- =============================================================================
-- COMPLEXITY NOTES FOR HASKELL
-- =============================================================================

complexityNotes :: IO ()
complexityNotes = do
    putStrLn "\n=== Haskell-Specific Notes ==="
    putStrLn ""
    putStrLn "List operations affect complexity:"
    putStrLn "  - (!!): O(n) indexing - lists are linked lists!"
    putStrLn "  - length: O(n)"
    putStrLn "  - (++): O(n) in left operand"
    putStrLn ""
    putStrLn "For better performance:"
    putStrLn "  - Use Data.Vector for O(1) indexing"
    putStrLn "  - Use Data.Sequence for O(log n) both ends"
    putStrLn "  - Use Data.Set for O(log n) membership"
    putStrLn ""
    putStrLn "Haskell's sort:"
    putStrLn "  - Uses merge sort"
    putStrLn "  - O(n log n) time"
    putStrLn "  - Stable"
    putStrLn "  - Lazy: can get first k elements in O(n + k log k)"

-- =============================================================================
-- COMPARISON
-- =============================================================================

comparisonTable :: IO ()
comparisonTable = do
    putStrLn "\n=== When to Use Each Algorithm ==="
    putStrLn ""
    putStrLn "elem / linear search:"
    putStrLn "  - Unsorted data"
    putStrLn "  - Small lists"
    putStrLn "  - One-time search"
    putStrLn ""
    putStrLn "Data.Set.member:"
    putStrLn "  - Repeated searches O(log n)"
    putStrLn "  - Build set once, query many times"
    putStrLn ""
    putStrLn "Insertion sort:"
    putStrLn "  - Small lists"
    putStrLn "  - Nearly sorted data"
    putStrLn ""
    putStrLn "Merge sort (built-in sort):"
    putStrLn "  - General purpose"
    putStrLn "  - Stability needed"
    putStrLn ""
    putStrLn "Quick sort:"
    putStrLn "  - Clean implementation"
    putStrLn "  - Educational"
    putStrLn "  - Note: not in-place in Haskell"

-- =============================================================================
-- MAIN
-- =============================================================================

main :: IO ()
main = do
    searchDemo
    basicSortDemo
    efficientSortDemo
    countingSortDemo
    builtInSortDemo
    stabilityDemo
    higherOrderSearchDemo
    complexityNotes
    comparisonTable
