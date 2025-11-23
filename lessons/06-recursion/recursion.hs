{-
  Lesson 6: Recursion in Haskell

  Haskell is a purely functional language where recursion is the PRIMARY
  way to express repetition (no loops!). Haskell has tail call optimization
  and lazy evaluation, making recursion very efficient.
-}

module Recursion where

-- ====================
-- 1. Simple Recursion
-- ====================

factorial :: Integer -> Integer
-- Calculate n! recursively
factorial n
    | n <= 1    = 1
    | otherwise = n * factorial (n - 1)

factorialTail :: Integer -> Integer
-- Tail-recursive factorial (optimized by GHC)
factorialTail n = go n 1
  where
    go 1 acc = acc
    go n acc = go (n - 1) (n * acc)

-- ====================
-- 2. Fibonacci
-- ====================

fibonacci :: Int -> Int
-- Fibonacci (inefficient - exponential time)
fibonacci n
    | n <= 1    = n
    | otherwise = fibonacci (n - 1) + fibonacci (n - 2)

-- Efficient Fibonacci using list comprehension and lazy evaluation
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibonacciEfficient :: Int -> Integer
fibonacciEfficient n = fibs !! n

-- ====================
-- 3. List Recursion
-- ====================

sumList :: Num a => [a] -> a
-- Sum all elements (pattern matching on list)
sumList []     = 0
sumList (x:xs) = x + sumList xs

listLength :: [a] -> Int
-- Calculate length
listLength []     = 0
listLength (_:xs) = 1 + listLength xs

reverseList :: [a] -> [a]
-- Reverse list
reverseList []     = []
reverseList (x:xs) = reverseList xs ++ [x]

-- More efficient reverse using accumulator
reverseListTail :: [a] -> [a]
reverseListTail lst = go lst []
  where
    go []     acc = acc
    go (x:xs) acc = go xs (x:acc)

maxElement :: Ord a => [a] -> a
-- Find maximum element
maxElement []  = error "Empty list"
maxElement [x] = x
maxElement (x:xs) = max x (maxElement xs)

-- ====================
-- 4. String Recursion
-- ====================

reverseString :: String -> String
-- String is [Char] in Haskell, so use list recursion
reverseString = reverseList

isPalindrome :: String -> Bool
-- Check if palindrome
isPalindrome s = clean s == reverse (clean s)
  where
    clean = map toLower . filter (/= ' ')
    toLower c
        | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
        | otherwise = c

-- ====================
-- 5. Binary Search
-- ====================

binarySearch :: Ord a => [a] -> a -> Int -> Int -> Int
-- Binary search on sorted list
binarySearch arr target low high
    | low > high = -1  -- Not found
    | arr !! mid == target = mid
    | arr !! mid > target = binarySearch arr target low (mid - 1)
    | otherwise = binarySearch arr target (mid + 1) high
  where
    mid = (low + high) `div` 2

-- ====================
-- 6. Quicksort
-- ====================

quicksort :: Ord a => [a] -> [a]
-- Elegant quicksort using list comprehensions
quicksort [] = []
quicksort (pivot:rest) =
    quicksort smaller ++ [pivot] ++ quicksort larger
  where
    smaller = [x | x <- rest, x < pivot]
    larger  = [x | x <- rest, x >= pivot]

-- ====================
-- 7. Tower of Hanoi
-- ====================

hanoi :: Int -> String -> String -> String -> [String]
-- Solve Tower of Hanoi
hanoi 1 source target _ = ["Move disk 1 from " ++ source ++ " to " ++ target]
hanoi n source target auxiliary =
    hanoi (n-1) source auxiliary target ++
    ["Move disk " ++ show n ++ " from " ++ source ++ " to " ++ target] ++
    hanoi (n-1) auxiliary target source

-- ====================
-- 8. Tree Traversal
-- ====================

data Tree a = Empty
            | Node a (Tree a) (Tree a)
            deriving (Show, Eq)

treeHeight :: Tree a -> Int
-- Calculate height
treeHeight Empty = 0
treeHeight (Node _ left right) =
    1 + max (treeHeight left) (treeHeight right)

treeSum :: Num a => Tree a -> a
-- Sum all values
treeSum Empty = 0
treeSum (Node value left right) =
    value + treeSum left + treeSum right

inorderTraversal :: Tree a -> [a]
-- Left, root, right
inorderTraversal Empty = []
inorderTraversal (Node value left right) =
    inorderTraversal left ++ [value] ++ inorderTraversal right

preorderTraversal :: Tree a -> [a]
-- Root, left, right
preorderTraversal Empty = []
preorderTraversal (Node value left right) =
    [value] ++ preorderTraversal left ++ preorderTraversal right

postorderTraversal :: Tree a -> [a]
-- Left, right, root
postorderTraversal Empty = []
postorderTraversal (Node value left right) =
    postorderTraversal left ++ postorderTraversal right ++ [value]

-- ====================
-- 9. Mutual Recursion
-- ====================

isEven :: Int -> Bool
-- Check if even using mutual recursion
isEven 0 = True
isEven n = isOdd (n - 1)

isOdd :: Int -> Bool
-- Check if odd
isOdd 0 = False
isOdd n = isEven (n - 1)

-- ====================
-- 10. Greatest Common Divisor
-- ====================

gcd' :: Integral a => a -> a -> a
-- Euclid's algorithm
gcd' a 0 = a
gcd' a b = gcd' b (a `mod` b)

-- ====================
-- 11. Additional Examples
-- ====================

-- Merge sort
mergeSort :: Ord a => [a] -> [a]
mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs  = merge (mergeSort left) (mergeSort right)
  where
    (left, right) = splitAt (length xs `div` 2) xs

    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys)
        | x <= y    = x : merge xs (y:ys)
        | otherwise = y : merge (x:xs) ys

-- Map using recursion
mapRecursive :: (a -> b) -> [a] -> [b]
mapRecursive _ []     = []
mapRecursive f (x:xs) = f x : mapRecursive f xs

-- Filter using recursion
filterRecursive :: (a -> Bool) -> [a] -> [a]
filterRecursive _ []     = []
filterRecursive p (x:xs)
    | p x       = x : filterRecursive p xs
    | otherwise = filterRecursive p xs

-- Fold (reduce) using recursion
foldRecursive :: (b -> a -> b) -> b -> [a] -> b
foldRecursive _ acc []     = acc
foldRecursive f acc (x:xs) = foldRecursive f (f acc x) xs

-- ====================
-- Tests and Examples
-- ====================

exampleTree :: Tree Int
-- Create example tree:
--       5
--      / \
--     3   8
--    / \   \
--   1   4   9
exampleTree = Node 5
    (Node 3
        (Node 1 Empty Empty)
        (Node 4 Empty Empty))
    (Node 8
        Empty
        (Node 9 Empty Empty))

main :: IO ()
main = do
    putStrLn "=== Recursion Examples in Haskell ===\n"

    -- Factorial
    putStrLn "1. Factorial:"
    putStrLn $ "   factorial 5 = " ++ show (factorial 5)
    putStrLn $ "   factorialTail 5 = " ++ show (factorialTail 5)

    -- Fibonacci
    putStrLn "\n2. Fibonacci:"
    putStrLn $ "   fibonacci 10 = " ++ show (fibonacci 10)
    putStrLn $ "   fibonacciEfficient 30 = " ++ show (fibonacciEfficient 30)

    -- List operations
    putStrLn "\n3. List Operations:"
    let numbers = [1, 2, 3, 4, 5]
    putStrLn $ "   sumList " ++ show numbers ++ " = " ++ show (sumList numbers)
    putStrLn $ "   listLength " ++ show numbers ++ " = " ++ show (listLength numbers)
    putStrLn $ "   reverseList " ++ show numbers ++ " = " ++ show (reverseList numbers)
    putStrLn $ "   maxElement " ++ show numbers ++ " = " ++ show (maxElement numbers)

    -- String operations
    putStrLn "\n4. String Operations:"
    putStrLn $ "   reverseString \"hello\" = " ++ reverseString "hello"
    putStrLn $ "   isPalindrome \"racecar\" = " ++ show (isPalindrome "racecar")
    putStrLn $ "   isPalindrome \"A man a plan a canal Panama\" = " ++
               show (isPalindrome "A man a plan a canal Panama")

    -- Binary search
    putStrLn "\n5. Binary Search:"
    let sortedArr = [1, 3, 5, 7, 9, 11, 13, 15]
    putStrLn $ "   binarySearch " ++ show sortedArr ++ " 7 = " ++
               show (binarySearch sortedArr 7 0 (length sortedArr - 1))
    putStrLn $ "   binarySearch " ++ show sortedArr ++ " 4 = " ++
               show (binarySearch sortedArr 4 0 (length sortedArr - 1))

    -- Quicksort
    putStrLn "\n6. Quicksort:"
    let unsorted = [3, 6, 8, 10, 1, 2, 1]
    putStrLn $ "   quicksort " ++ show unsorted ++ " = " ++ show (quicksort unsorted)

    -- Merge sort
    putStrLn "\n7. Merge Sort:"
    putStrLn $ "   mergeSort " ++ show unsorted ++ " = " ++ show (mergeSort unsorted)

    -- Tower of Hanoi
    putStrLn "\n8. Tower of Hanoi (3 disks):"
    let moves = hanoi 3 "A" "C" "B"
    mapM_ (putStrLn . ("   " ++)) moves

    -- Tree operations
    putStrLn "\n9. Binary Tree:"
    putStrLn $ "   treeHeight = " ++ show (treeHeight exampleTree)
    putStrLn $ "   treeSum = " ++ show (treeSum exampleTree)
    putStrLn $ "   inorderTraversal = " ++ show (inorderTraversal exampleTree)
    putStrLn $ "   preorderTraversal = " ++ show (preorderTraversal exampleTree)
    putStrLn $ "   postorderTraversal = " ++ show (postorderTraversal exampleTree)

    -- Mutual recursion
    putStrLn "\n10. Mutual Recursion:"
    putStrLn $ "   isEven 10 = " ++ show (isEven 10)
    putStrLn $ "   isOdd 10 = " ++ show (isOdd 10)
    putStrLn $ "   isEven 7 = " ++ show (isEven 7)
    putStrLn $ "   isOdd 7 = " ++ show (isOdd 7)

    -- GCD
    putStrLn "\n11. Greatest Common Divisor:"
    putStrLn $ "   gcd' 48 18 = " ++ show (gcd' 48 18)
    putStrLn $ "   gcd' 100 35 = " ++ show (gcd' 100 35)

    -- Higher-order recursion
    putStrLn "\n12. Higher-Order Recursive Functions:"
    putStrLn $ "   mapRecursive (*2) [1,2,3,4,5] = " ++
               show (mapRecursive (*2) [1,2,3,4,5])
    putStrLn $ "   filterRecursive even [1,2,3,4,5,6] = " ++
               show (filterRecursive even [1,2,3,4,5,6])
    putStrLn $ "   foldRecursive (+) 0 [1,2,3,4,5] = " ++
               show (foldRecursive (+) 0 [1,2,3,4,5])
