{- Lesson 4: Functions in Haskell
   Demonstrates pure functions, type signatures, currying, and composition
-}

module Main where

import Data.List (intercalate)

-- ============================================
-- 1. Basic Function Definition
-- ============================================

-- Type signature (optional but recommended)
greet :: String -> String
greet name = "Hello, " ++ name ++ "!"

-- Multiple parameters (curried automatically)
add :: Int -> Int -> Int
add x y = x + y

square :: Int -> Int
square x = x * x

-- ============================================
-- 2. Currying and Partial Application
-- ============================================

-- All Haskell functions are curried!
-- add :: Int -> Int -> Int
-- is really: add :: Int -> (Int -> Int)

multiply :: Int -> Int -> Int
multiply x y = x * y

-- Partial application (creating new functions)
double :: Int -> Int
double = multiply 2  -- Partially apply multiply

triple :: Int -> Int
triple = multiply 3

-- Using operator sections
addFive :: Int -> Int
addFive = (+ 5)  -- Operator section

timesTwo :: Int -> Int
timesTwo = (* 2)

-- ============================================
-- 3. Return Values
-- ============================================

-- Haskell functions return exactly one value
-- Use tuples for multiple logical returns
divide :: Double -> Double -> (Maybe Double, Maybe String)
divide _ 0 = (Nothing, Just "Cannot divide by zero")
divide x y = (Just (x / y), Nothing)

-- Using Maybe for optional values
safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

-- ============================================
-- 4. Purity
-- ============================================

-- ALL functions in Haskell are pure (except IO)
pureAdd :: Int -> Int -> Int
pureAdd x y = x + y

-- Pure function - always same output for same input
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Impure functions MUST be in IO monad
impurePrint :: String -> IO ()
impurePrint message = putStrLn message

-- No global state - this doesn't exist in Haskell!
-- total :: Int
-- total = 0  -- Can't be modified!

-- ============================================
-- 5. First-Class Functions
-- ============================================

-- Functions as parameters
applyOperation :: (Int -> Int -> Int) -> Int -> Int -> Int
applyOperation op x y = op x y

-- Functions as return values (closures)
makeMultiplier :: Int -> (Int -> Int)
makeMultiplier factor = \x -> x * factor

makeAdder :: Int -> (Int -> Int)
makeAdder n = (+ n)  -- Operator section

-- ============================================
-- 6. Anonymous Functions (Lambdas)
-- ============================================

-- Lambda syntax: \x -> expression
squareLambda :: Int -> Int
squareLambda = \x -> x * x

addLambda :: Int -> Int -> Int
addLambda = \x y -> x + y

-- ============================================
-- 7. Function Composition
-- ============================================

-- Composition operator: (.)
-- (f . g) x = f (g x)

increment :: Int -> Int
increment x = x + 1

-- Compose square and increment
squareThenIncrement :: Int -> Int
squareThenIncrement = increment . square

-- Point-free style (no explicit parameter)
doubleSquare :: Int -> Int
doubleSquare = (* 2) . square

-- Manual composition function (already built-in as .)
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \x -> f (g x)

-- ============================================
-- 8. Higher-Order Functions
-- ============================================

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes 0 _ x = x
applyNTimes n f x = applyNTimes (n - 1) f (f x)

-- Map, filter, fold are built-in and fundamental
-- map :: (a -> b) -> [a] -> [b]
-- filter :: (a -> Bool) -> [a] -> [a]
-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- foldr :: (a -> b -> b) -> b -> [a] -> b

-- Custom implementations for demonstration
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter pred (x:xs)
    | pred x    = x : myFilter pred xs
    | otherwise = myFilter pred xs

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

-- ============================================
-- 9. Pattern Matching in Function Definitions
-- ============================================

-- Pattern match on values
describeNumber :: Int -> String
describeNumber 0 = "Zero"
describeNumber 1 = "One"
describeNumber 2 = "Two"
describeNumber _ = "Many"

-- Pattern match on lists
listLength :: [a] -> Int
listLength [] = 0
listLength (_:xs) = 1 + listLength xs

listSum :: [Int] -> Int
listSum [] = 0
listSum (x:xs) = x + listSum xs

-- Pattern match on tuples
first :: (a, b) -> a
first (x, _) = x

second :: (a, b) -> b
second (_, y) = y

-- ============================================
-- 10. Guards
-- ============================================

absoluteValue :: Int -> Int
absoluteValue n
    | n < 0     = -n
    | otherwise = n

describePerson :: String -> Int -> String
describePerson name age
    | age < 13  = name ++ " is a child"
    | age < 18  = name ++ " is a teenager"
    | otherwise = name ++ " is an adult"

-- Guards with pattern matching
describeList :: [a] -> String
describeList xs
    | null xs   = "Empty list"
    | length xs == 1 = "Single element"
    | length xs == 2 = "Two elements"
    | otherwise = "Many elements"

-- ============================================
-- 11. Point-Free Style
-- ============================================

-- Without point-free
addOneVerbose :: Int -> Int
addOneVerbose x = x + 1

-- Point-free (no explicit parameter)
addOne :: Int -> Int
addOne = (+ 1)

-- More complex example
sumOfSquares :: [Int] -> Int
sumOfSquares = sum . map (^ 2)
-- Same as: sumOfSquares xs = sum (map (^ 2) xs)

-- ============================================
-- 12. Recursion Patterns
-- ============================================

-- Tail recursion (more efficient)
factorialTail :: Int -> Int
factorialTail n = factHelper n 1
  where
    factHelper 0 acc = acc
    factHelper n acc = factHelper (n - 1) (n * acc)

-- Fibonacci (naive recursion)
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- ============================================
-- Main Program (IO Monad)
-- ============================================

main :: IO ()
main = do
    putStrLn "=== Haskell Functions ==="
    putStrLn ""

    -- 1. Basic functions
    putStrLn "1. Basic Functions:"
    putStrLn $ "  greet \"Alice\": " ++ greet "Alice"
    putStrLn $ "  add 5 3: " ++ show (add 5 3)
    putStrLn $ "  square 7: " ++ show (square 7)

    -- 2. Currying and partial application
    putStrLn "\n2. Currying and Partial Application:"
    putStrLn $ "  double 5: " ++ show (double 5)
    putStrLn $ "  triple 5: " ++ show (triple 5)

    let addFivePartial = add 5  -- Partial application
    putStrLn $ "  (add 5) 10: " ++ show (addFivePartial 10)

    putStrLn $ "  addFive 10: " ++ show (addFive 10)
    putStrLn $ "  timesTwo 7: " ++ show (timesTwo 7)

    -- 3. Multiple returns (tuples)
    putStrLn "\n3. Optional Return Values (Maybe):"
    let (result1, error1) = divide 10 2
    putStrLn $ "  divide 10 2: result=" ++ show result1 ++ ", error=" ++ show error1

    let (result2, error2) = divide 10 0
    putStrLn $ "  divide 10 0: result=" ++ show result2 ++ ", error=" ++ show error2

    -- 4. Closures (functions returning functions)
    putStrLn "\n4. Closures (functions capturing values):"
    let timesTwo' = makeMultiplier 2
    let timesThree = makeMultiplier 3
    putStrLn $ "  makeMultiplier 2: timesTwo' 5 = " ++ show (timesTwo' 5)
    putStrLn $ "  makeMultiplier 3: timesThree 5 = " ++ show (timesThree 5)

    let addTen = makeAdder 10
    putStrLn $ "  makeAdder 10: addTen 5 = " ++ show (addTen 5)

    -- 5. Purity
    putStrLn "\n5. Purity (all functions are pure except IO):"
    putStrLn $ "  pureAdd 5 3: " ++ show (pureAdd 5 3)
    putStrLn $ "  factorial 5: " ++ show (factorial 5)
    putStr "  impurePrint \"Hello\": "
    impurePrint "Hello"

    -- 6. Function composition
    putStrLn "\n6. Function Composition:"
    putStrLn $ "  squareThenIncrement 5: " ++ show (squareThenIncrement 5)  -- 26
    let incrementThenSquare = square . increment
    putStrLn $ "  incrementThenSquare 5: " ++ show (incrementThenSquare 5)  -- 36
    putStrLn $ "  doubleSquare 5: " ++ show (doubleSquare 5)  -- 50

    -- 7. Higher-order functions
    putStrLn "\n7. Higher-Order Functions:"
    putStrLn $ "  applyTwice increment 5: " ++ show (applyTwice increment 5)
    putStrLn $ "  applyNTimes 4 timesTwo 2: " ++ show (applyNTimes 4 timesTwo 2)

    -- 8. Map, filter, fold
    putStrLn "\n8. Map, Filter, Fold:"
    let numbers = [1, 2, 3, 4, 5]
    putStrLn $ "  numbers: " ++ show numbers

    let squared = map (^ 2) numbers
    putStrLn $ "  map (^ 2) numbers: " ++ show squared

    let evens = filter even numbers
    putStrLn $ "  filter even numbers: " ++ show evens

    let total = foldl (+) 0 numbers
    putStrLn $ "  foldl (+) 0 numbers: " ++ show total

    let product' = foldl (*) 1 numbers
    putStrLn $ "  foldl (*) 1 numbers: " ++ show product'

    -- 9. Pattern matching
    putStrLn "\n9. Pattern Matching:"
    putStrLn $ "  describeNumber 0: " ++ describeNumber 0
    putStrLn $ "  describeNumber 1: " ++ describeNumber 1
    putStrLn $ "  describeNumber 5: " ++ describeNumber 5

    putStrLn $ "  listLength [1,2,3]: " ++ show (listLength [1,2,3])
    putStrLn $ "  first (\"Alice\", 30): " ++ first ("Alice", 30)

    -- 10. Guards
    putStrLn "\n10. Guards:"
    putStrLn $ "  absoluteValue (-5): " ++ show (absoluteValue (-5))
    putStrLn $ "  describePerson \"Alice\" 25: " ++ describePerson "Alice" 25

    -- 11. Point-free style
    putStrLn "\n11. Point-Free Style:"
    putStrLn $ "  sumOfSquares [1,2,3,4]: " ++ show (sumOfSquares [1,2,3,4])
    -- Point-free composition
    let processNumbers = sum . filter even . map (* 2)
    putStrLn $ "  sum . filter even . map (* 2) $ [1..5]: " ++ show (processNumbers [1..5])

    -- 12. Tail recursion
    putStrLn "\n12. Tail Recursion:"
    putStrLn $ "  factorialTail 5: " ++ show (factorialTail 5)
    putStrLn $ "  fibonacci 10: " ++ show (fibonacci 10)

    putStrLn "\n=== Summary ==="
    putStrLn "Haskell functions are:"
    putStrLn "  - Pure by default (side effects in IO monad)"
    putStrLn "  - Automatically curried (all take one argument)"
    putStrLn "  - First-class values (pass, return, compose)"
    putStrLn "  - Type-safe with powerful inference"
    putStrLn "  - Support pattern matching and guards"
    putStrLn "  - Composable with (.) operator"
    putStrLn "  - Can be written point-free"
