-- Lesson 6: Recursion - Haskell Examples
-- Recursion is the natural way to program in Haskell!

module Main where

import Data.List (intercalate)

-- =============================================================================
-- BASIC RECURSION: Factorial
-- =============================================================================

-- Pattern matching makes the base case explicit
factorial :: Integer -> Integer
factorial 0 = 1                    -- Base case
factorial n = n * factorial (n - 1)  -- Recursive case

-- =============================================================================
-- FIBONACCI SEQUENCE
-- =============================================================================

-- Naive recursive (elegant but slow)
fibonacci :: Int -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- Efficient version using infinite list
fibList :: [Integer]
fibList = 0 : 1 : zipWith (+) fibList (tail fibList)

fibFast :: Int -> Integer
fibFast n = fibList !! n

-- =============================================================================
-- LIST PROCESSING (Haskell's bread and butter!)
-- =============================================================================

-- Sum a list
sumList :: [Int] -> Int
sumList []     = 0              -- Base case: empty list
sumList (x:xs) = x + sumList xs  -- Recursive case: head + sum of tail

-- Length of a list
len :: [a] -> Int
len []     = 0
len (_:xs) = 1 + len xs

-- Check if element is in list
contains :: Eq a => a -> [a] -> Bool
contains _ []     = False
contains target (x:xs)
    | target == x = True
    | otherwise   = contains target xs

-- Reverse a list
reverseList :: [a] -> [a]
reverseList []     = []
reverseList (x:xs) = reverseList xs ++ [x]

-- More efficient tail-recursive reverse
reverseList' :: [a] -> [a]
reverseList' = go []
  where
    go acc []     = acc
    go acc (x:xs) = go (x:acc) xs

-- =============================================================================
-- PATTERN MATCHING WITH RECURSION
-- =============================================================================

-- Take first n elements
takeN :: Int -> [a] -> [a]
takeN 0 _      = []
takeN _ []     = []
takeN n (x:xs) = x : takeN (n-1) xs

-- Drop first n elements
dropN :: Int -> [a] -> [a]
dropN 0 xs     = xs
dropN _ []     = []
dropN n (_:xs) = dropN (n-1) xs

-- Map a function over a list
mapList :: (a -> b) -> [a] -> [b]
mapList _ []     = []
mapList f (x:xs) = f x : mapList f xs

-- Filter elements
filterList :: (a -> Bool) -> [a] -> [a]
filterList _ []     = []
filterList p (x:xs)
    | p x       = x : filterList p xs
    | otherwise = filterList p xs

-- =============================================================================
-- BINARY SEARCH
-- =============================================================================

binarySearch :: Ord a => a -> [a] -> Maybe Int
binarySearch target arr = go 0 (length arr - 1)
  where
    go low high
        | low > high       = Nothing
        | arr !! mid == target = Just mid
        | arr !! mid < target  = go (mid + 1) high
        | otherwise        = go low (mid - 1)
      where
        mid = (low + high) `div` 2

-- =============================================================================
-- TREE STRUCTURES (Natural fit for recursion)
-- =============================================================================

data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Show, Eq)

-- Insert into BST
insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x (Node y left right)
    | x < y     = Node y (insert x left) right
    | otherwise = Node y left (insert x right)

-- In-order traversal
inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node x left right) = inOrder left ++ [x] ++ inOrder right

-- Tree height
treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node _ left right) = 1 + max (treeHeight left) (treeHeight right)

-- =============================================================================
-- ACCUMULATORS AND TAIL RECURSION
-- =============================================================================

-- Tail recursive factorial
factorialTail :: Integer -> Integer
factorialTail n = go n 1
  where
    go 0 acc = acc
    go n acc = go (n - 1) (n * acc)

-- Tail recursive sum
sumTail :: [Int] -> Int
sumTail = go 0
  where
    go acc []     = acc
    go acc (x:xs) = go (acc + x) xs

-- =============================================================================
-- MUTUAL RECURSION
-- =============================================================================

isEven :: Int -> Bool
isEven 0 = True
isEven n = isOdd (n - 1)

isOdd :: Int -> Bool
isOdd 0 = False
isOdd n = isEven (n - 1)

-- =============================================================================
-- FLATTEN NESTED STRUCTURES
-- =============================================================================

data Nested a = Elem a | List [Nested a]
    deriving Show

flatten :: Nested a -> [a]
flatten (Elem x)  = [x]
flatten (List xs) = concatMap flatten xs

-- =============================================================================
-- MAIN
-- =============================================================================

main :: IO ()
main = do
    putStrLn "=== Factorial ==="
    mapM_ (\n -> putStrLn $ show n ++ "! = " ++ show (factorial n)) [0..5]

    putStrLn "\n=== Fibonacci ==="
    putStrLn $ "First 10 Fibonacci: " ++ show (take 10 fibList)

    putStrLn "\n=== List Processing ==="
    let numbers = [1, 2, 3, 4, 5]
    putStrLn $ "sumList " ++ show numbers ++ " = " ++ show (sumList numbers)
    putStrLn $ "len " ++ show numbers ++ " = " ++ show (len numbers)
    putStrLn $ "contains 3 " ++ show numbers ++ " = " ++ show (contains 3 numbers)
    putStrLn $ "contains 9 " ++ show numbers ++ " = " ++ show (contains 9 numbers)
    putStrLn $ "reverseList " ++ show numbers ++ " = " ++ show (reverseList numbers)

    putStrLn "\n=== Binary Search ==="
    let sorted = [1, 3, 5, 7, 9, 11, 13, 15]
    putStrLn $ "binarySearch 7 " ++ show sorted ++ " = " ++ show (binarySearch 7 sorted)
    putStrLn $ "binarySearch 10 " ++ show sorted ++ " = " ++ show (binarySearch 10 sorted)

    putStrLn "\n=== Tree Operations ==="
    let tree = foldr insert Empty [5, 3, 7, 1, 9, 4, 6]
    putStrLn $ "In-order traversal: " ++ show (inOrder tree)
    putStrLn $ "Tree height: " ++ show (treeHeight tree)

    putStrLn "\n=== Tail Recursion ==="
    putStrLn $ "factorialTail 5 = " ++ show (factorialTail 5)
    putStrLn $ "sumTail [1..100] = " ++ show (sumTail [1..100])

    putStrLn "\n=== Mutual Recursion ==="
    mapM_ (\n -> putStrLn $ "isEven " ++ show n ++ " = " ++ show (isEven n) ++
                           ", isOdd " ++ show n ++ " = " ++ show (isOdd n)) [0..4]
