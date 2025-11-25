-- Lesson 12: Algorithm Analysis - Haskell Examples
-- Understanding time and space complexity through practical examples

module Main where

import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.Clock
import Control.Monad (forM_)
import System.CPUTime

-- =============================================================================
-- TIMING UTILITY
-- =============================================================================

timeIt :: String -> IO a -> IO a
timeIt label action = do
    start <- getCPUTime
    result <- action
    end <- getCPUTime
    let diff = fromIntegral (end - start) / (10^12) :: Double
    putStrLn $ label ++ ": " ++ show diff ++ "s"
    return result

-- =============================================================================
-- O(1) - CONSTANT TIME
-- =============================================================================

-- Direct pattern matching is O(1)
getFirst :: [a] -> Maybe a
getFirst []    = Nothing
getFirst (x:_) = Just x

-- Accessing first element of a tuple is O(1)
getFst :: (a, b) -> a
getFst (x, _) = x

-- Simple arithmetic is O(1)
isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

constantTimeDemo :: IO ()
constantTimeDemo = do
    putStrLn "=== O(1) Constant Time ==="
    let small = [1..1000]
    let large = [1..1000000]
    putStrLn $ "getFirst small list: " ++ show (getFirst small)
    putStrLn $ "getFirst large list: " ++ show (getFirst large)
    putStrLn "Both are instant - that's O(1)!"

-- =============================================================================
-- O(log n) - LOGARITHMIC TIME
-- =============================================================================

-- Binary search on a sorted list
-- Note: This is still O(n) because list indexing is O(n) in Haskell!
-- For true O(log n), use Data.Array or Data.Vector
binarySearch :: Ord a => [a] -> a -> Int -> Int -> Maybe Int
binarySearch arr target left right
    | left > right = Nothing
    | midVal == target = Just mid
    | midVal < target  = binarySearch arr target (mid + 1) right
    | otherwise        = binarySearch arr target left (mid - 1)
  where
    mid = (left + right) `div` 2
    midVal = arr !! mid  -- Note: !! is O(n) for lists!

-- Using Map for O(log n) lookups
lookupInMap :: Ord k => k -> Map k v -> Maybe v
lookupInMap = Map.lookup

logarithmicTimeDemo :: IO ()
logarithmicTimeDemo = do
    putStrLn "\n=== O(log n) Logarithmic Time ==="
    let myMap = Map.fromList [(i, i*2) | i <- [1..100000]]
    putStrLn $ "Map.lookup 50000: " ++ show (Map.lookup 50000 myMap)
    putStrLn "Data.Map uses balanced trees - O(log n) lookup"

-- =============================================================================
-- O(n) - LINEAR TIME
-- =============================================================================

-- Linear search through a list
linearSearch :: Eq a => a -> [a] -> Maybe Int
linearSearch target = go 0
  where
    go _ [] = Nothing
    go i (x:xs)
        | x == target = Just i
        | otherwise   = go (i + 1) xs

-- Finding maximum element
findMax :: Ord a => [a] -> Maybe a
findMax [] = Nothing
findMax xs = Just (maximum xs)  -- O(n)

-- Sum all elements
sumAll :: Num a => [a] -> a
sumAll = sum  -- O(n)

-- Map over all elements
doubleAll :: [Int] -> [Int]
doubleAll = map (*2)  -- O(n)

-- Filter elements
filterPositive :: [Int] -> [Int]
filterPositive = filter (> 0)  -- O(n)

linearTimeDemo :: IO ()
linearTimeDemo = do
    putStrLn "\n=== O(n) Linear Time ==="
    let numbers = [1..100000]
    putStrLn $ "sum of 100,000 numbers: " ++ show (sumAll numbers)
    putStrLn $ "maximum: " ++ show (findMax numbers)

-- =============================================================================
-- O(n log n) - LINEARITHMIC TIME
-- =============================================================================

-- Merge sort implementation
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

-- Haskell's built-in sort is also O(n log n)
builtInSort :: Ord a => [a] -> [a]
builtInSort = sort

linearithmicTimeDemo :: IO ()
linearithmicTimeDemo = do
    putStrLn "\n=== O(n log n) Linearithmic Time ==="
    let unsorted = [5,2,8,1,9,3,7,4,6,0]
    putStrLn $ "mergeSort " ++ show unsorted ++ " = " ++ show (mergeSort unsorted)

-- =============================================================================
-- O(n²) - QUADRATIC TIME
-- =============================================================================

-- Bubble sort - O(n²)
bubbleSort :: Ord a => [a] -> [a]
bubbleSort [] = []
bubbleSort xs = bubbleSort (init bubbled) ++ [last bubbled]
  where
    bubbled = bubble xs
    bubble [x] = [x]
    bubble (x:y:rest)
        | x > y     = y : bubble (x:rest)
        | otherwise = x : bubble (y:rest)

-- Check for duplicates - O(n²) naive version
hasDuplicatesNaive :: Eq a => [a] -> Bool
hasDuplicatesNaive [] = False
hasDuplicatesNaive (x:xs) = x `elem` xs || hasDuplicatesNaive xs

-- Check for duplicates - O(n log n) using Set
hasDuplicatesEfficient :: Ord a => [a] -> Bool
hasDuplicatesEfficient xs = length xs /= Set.size (Set.fromList xs)

-- All pairs - O(n²)
allPairs :: [a] -> [(a, a)]
allPairs xs = [(x, y) | x <- xs, y <- xs]

quadraticTimeDemo :: IO ()
quadraticTimeDemo = do
    putStrLn "\n=== O(n²) Quadratic Time ==="
    let small = [5,2,8,1,9]
    putStrLn $ "bubbleSort " ++ show small ++ " = " ++ show (bubbleSort small)
    putStrLn $ "allPairs [1,2,3] = " ++ show (allPairs [1,2,3])

-- =============================================================================
-- O(2^n) - EXPONENTIAL TIME
-- =============================================================================

-- Naive Fibonacci - O(2^n)
fibNaive :: Int -> Integer
fibNaive 0 = 0
fibNaive 1 = 1
fibNaive n = fibNaive (n-1) + fibNaive (n-2)

-- Memoized Fibonacci using infinite list - O(n)
fibMemo :: Int -> Integer
fibMemo n = fibs !! n
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Generate all subsets - O(2^n)
subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

exponentialTimeDemo :: IO ()
exponentialTimeDemo = do
    putStrLn "\n=== O(2^n) Exponential Time ==="
    putStrLn $ "fibNaive 20 = " ++ show (fibNaive 20)
    putStrLn $ "fibMemo 20 = " ++ show (fibMemo 20)
    putStrLn $ "subsets [1,2,3] = " ++ show (subsets [1,2,3 :: Int])
    putStrLn $ "Number of subsets of n elements = 2^n"

-- =============================================================================
-- SPACE COMPLEXITY
-- =============================================================================

-- O(1) space with tail recursion
sumTailRec :: [Int] -> Int
sumTailRec = go 0
  where
    go acc [] = acc
    go acc (x:xs) = go (acc + x) xs

-- O(n) space - building a new list
doubleList :: [Int] -> [Int]
doubleList [] = []
doubleList (x:xs) = (x*2) : doubleList xs

-- Haskell's laziness can help with space!
-- This doesn't create the full list in memory:
lazySum :: Int
lazySum = sum [1..1000000]  -- Evaluated lazily

spaceComplexityDemo :: IO ()
spaceComplexityDemo = do
    putStrLn "\n=== Space Complexity ==="
    putStrLn "Haskell's laziness affects space complexity!"
    putStrLn $ "sum [1..1000000] = " ++ show lazySum
    putStrLn "Computed lazily - doesn't store full list"
    putStrLn ""
    putStrLn "Strict vs Lazy evaluation matters:"
    putStrLn "  - foldl can cause space leaks (builds thunks)"
    putStrLn "  - foldl' (strict) is usually better for sums"

-- =============================================================================
-- COMPLEXITY COMPARISON
-- =============================================================================

printComparison :: IO ()
printComparison = do
    putStrLn "\n=== Complexity Comparison ==="
    putStrLn ""
    putStrLn "| Complexity | n=10    | n=100   | n=1000   | n=10000    |"
    putStrLn "|------------|---------|---------|----------|------------|"
    putStrLn "| O(1)       | 1       | 1       | 1        | 1          |"
    putStrLn "| O(log n)   | 3       | 7       | 10       | 13         |"
    putStrLn "| O(n)       | 10      | 100     | 1000     | 10000      |"
    putStrLn "| O(n log n) | 33      | 664     | 9966     | 132877     |"
    putStrLn "| O(n²)      | 100     | 10000   | 1000000  | 100000000  |"
    putStrLn "| O(2^n)     | 1024    | 10^30   | 10^301   | ∞          |"

-- =============================================================================
-- HASKELL-SPECIFIC CONSIDERATIONS
-- =============================================================================

haskellTips :: IO ()
haskellTips = do
    putStrLn "\n=== Haskell-Specific Complexity Notes ==="
    putStrLn ""
    putStrLn "1. List operations:"
    putStrLn "   - head, tail: O(1)"
    putStrLn "   - length: O(n)"
    putStrLn "   - (!!): O(n) - lists aren't arrays!"
    putStrLn "   - (++): O(n) in left operand"
    putStrLn ""
    putStrLn "2. Use appropriate data structures:"
    putStrLn "   - Data.Map: O(log n) lookup"
    putStrLn "   - Data.Set: O(log n) membership"
    putStrLn "   - Data.IntMap: Faster for Int keys"
    putStrLn "   - Data.Vector: O(1) indexing"
    putStrLn ""
    putStrLn "3. Laziness considerations:"
    putStrLn "   - Lazy evaluation can reduce work"
    putStrLn "   - But can cause space leaks with accumulation"
    putStrLn "   - Use seq or BangPatterns for strictness"

-- =============================================================================
-- MAIN
-- =============================================================================

main :: IO ()
main = do
    constantTimeDemo
    logarithmicTimeDemo
    linearTimeDemo
    linearithmicTimeDemo
    quadraticTimeDemo
    exponentialTimeDemo
    spaceComplexityDemo
    printComparison
    haskellTips
