-- Lesson 4: Functions in Haskell
-- Demonstrates pure functions, type signatures, currying, and composition

module Main where

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
-- 2. Parameters and Currying
-- ============================================

-- All Haskell functions are curried!
-- add :: Int -> Int -> Int
-- is really: add :: Int -> (Int -> Int)

multiply :: Int -> Int -> Int
multiply x y = x * y

-- Partial application
double :: Int -> Int
double = multiply 2  -- Partially apply multiply

triple :: Int -> Int
triple = multiply 3

-- ============================================
-- 3. Return Values
-- ============================================

-- Haskell functions return exactly one value
-- Use tuples for multiple returns
divide :: Double -> Double -> (Maybe Double, Maybe String)
divide _ 0 = (Nothing, Just "Cannot divide by zero")
divide x y = (Just (x / y), Nothing)

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

-- ============================================
-- 5. First-Class Functions
-- ============================================

-- Functions as parameters
applyOperation :: (Int -> Int -> Int) -> Int -> Int -> Int
applyOperation op x y = op x y

-- Functions as return values
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

-- Manual composition function
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \x -> f (g x)

-- ============================================
-- 8. Higher-Order Functions
-- ============================================

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes 0 f x = x
applyNTimes n f x = applyNTimes (n - 1) f (f x)

-- Map, filter, fold are built-in
-- map :: (a -> b) -> [a] -> [b]
-- filter :: (a -> Bool) -> [a] -> [a]
-- foldl :: (b -> a -> b) -> b -> [a] -> b

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

-- ============================================
-- Main Program (IO Monad)
-- ============================================

main :: IO ()
main = do
    putStrLn "=== Haskell Functions ==="
    putStrLn ""

    -- 1. Basic functions
    putStrLn "1. Basic Functions:"
    putStrLn $ "  greet 'Alice': " ++ greet "Alice"
    putStrLn $ "  add 5 3: " ++ show (add 5 3)
    putStrLn $ "  square 7: " ++ show (square 7)

    -- 2. Currying and partial application
    putStrLn "\n2. Currying and Partial Application:"
    putStrLn $ "  double 5: " ++ show (double 5)
    putStrLn $ "  triple 5: " ++ show (triple 5)

    let addFive = add 5  -- Partial application
    putStrLn $ "  addFive 10: " ++ show (addFive 10)

    -- 3. Multiple returns (tuples)
    putStrLn "\n3. Multiple Return Values (tuples):"
    let (result1, error1) = divide 10 2
    putStrLn $ "  divide 10 2: result=" ++ show result1 ++ ", error=" ++ show error1

    let (result2, error2) = divide 10 0
    putStrLn $ "  divide 10 0: result=" ++ show result2 ++ ", error=" ++ show error2

    -- 4. Closures (functions returning functions)
    putStrLn "\n4. Closures (functions capturing values):"
    let timesTwo = makeMultiplier 2
    let timesThree = makeMultiplier 3
    putStrLn $ "  timesTwo 5: " ++ show (timesTwo 5)
    putStrLn $ "  timesThree 5: " ++ show (timesThree 5)

    let addTen = makeAdder 10
    putStrLn $ "  addTen 5: " ++ show (addTen 5)

    -- 5. Lambdas
    putStrLn "\n5. Anonymous Functions (Lambdas):"
    putStrLn $ "  (\\x -> x * 2) 5: " ++ show ((\x -> x * 2) 5)
    putStrLn $ "  squareLambda 7: " ++ show (squareLambda 7)

    -- 6. Function composition
    putStrLn "\n6. Function Composition:"
    putStrLn $ "  squareThenIncrement 5: " ++ show (squareThenIncrement 5)
    putStrLn $ "  doubleSquare 5: " ++ show (doubleSquare 5)
    putStrLn $ "  (increment . square) 5: " ++ show ((increment . square) 5)

    -- 7. First-class functions
    putStrLn "\n7. First-Class Functions:"
    putStrLn $ "  applyOperation add 5 3: " ++ show (applyOperation add 5 3)
    putStrLn $ "  applyOperation multiply 5 3: " ++ show (applyOperation multiply 5 3)

    -- 8. Higher-order functions
    putStrLn "\n8. Higher-Order Functions:"
    putStrLn $ "  applyTwice double 5: " ++ show (applyTwice double 5)
    putStrLn $ "  applyNTimes 3 double 5: " ++ show (applyNTimes 3 double 5)

    -- 9. Map, filter, fold
    putStrLn "\n9. Map, Filter, Fold:"
    let numbers = [1, 2, 3, 4, 5]
    putStrLn $ "  numbers: " ++ show numbers
    putStrLn $ "  map square numbers: " ++ show (map square numbers)
    putStrLn $ "  filter even numbers: " ++ show (filter even numbers)
    putStrLn $ "  foldl (+) 0 numbers: " ++ show (foldl (+) 0 numbers)

    -- List comprehensions (syntactic sugar)
    putStrLn $ "  [x * x | x <- numbers]: " ++ show [x * x | x <- numbers]
    putStrLn $ "  [x | x <- numbers, even x]: " ++ show [x | x <- numbers, even x]

    -- 10. Pattern matching
    putStrLn "\n10. Pattern Matching:"
    putStrLn $ "  describeNumber 0: " ++ describeNumber 0
    putStrLn $ "  describeNumber 2: " ++ describeNumber 2
    putStrLn $ "  describeNumber 5: " ++ describeNumber 5

    -- 11. Guards
    putStrLn "\n11. Guards:"
    putStrLn $ "  describePerson 'Alice' 10: " ++ describePerson "Alice" 10
    putStrLn $ "  describePerson 'Bob' 15: " ++ describePerson "Bob" 15
    putStrLn $ "  describePerson 'Charlie' 25: " ++ describePerson "Charlie" 25

    -- 12. Purity demonstration
    putStrLn "\n12. Purity:"
    putStrLn "  All functions are pure (except IO)!"
    putStrLn "  factorial 5 always returns the same value:"
    putStrLn $ "    " ++ show (factorial 5)
    putStrLn $ "    " ++ show (factorial 5)
    putStrLn "  No side effects possible in pure functions"
