{- Lesson 3: Control Flow in Haskell
   Demonstrates conditionals, guards, pattern matching, and recursion
-}

module Main where

import Data.List (intercalate)

main :: IO ()
main = do
    putStrLn "=== Haskell Control Flow ==="
    putStrLn ""

    -- 1. Basic conditionals (if is an expression)
    putStrLn "1. Conditionals (if expressions):"
    let age = 20
    let description = if age >= 18
                      then "Adult"
                      else if age >= 13
                           then "Teenager"
                           else "Child"
    putStrLn $ "  Age " ++ show age ++ ": " ++ description

    -- Ternary-style (one-line if)
    let status = if age >= 18 then "Adult" else "Minor"
    putStrLn $ "  Status (if expression): " ++ status

    -- 2. Guards (preferred Haskell style)
    putStrLn "\n2. Guards (idiomatic Haskell):"
    putStrLn $ "  Age " ++ show age ++ ": " ++ describeAge age

    -- Multiple examples
    putStrLn "  More examples:"
    mapM_ (\a -> putStrLn $ "    Age " ++ show a ++ ": " ++ describeAge a) [5, 15, 25]

    -- 3. No traditional loops - use recursion
    putStrLn "\n3. Recursion (no traditional loops):"
    putStrLn "  Count to 5:"
    putStr "   "
    printNumbers 0 5
    putStrLn ""

    putStrLn "  Iterate list:"
    let fruits = ["apple", "banana", "cherry"]
    mapM_ (\f -> putStrLn $ "    " ++ f) fruits

    putStrLn "  With index (using zip):"
    mapM_ (\(i, f) -> putStrLn $ "    " ++ show i ++ ": " ++ f) (zip [0..] fruits)

    -- 4. List comprehensions (alternative to loops)
    putStrLn "\n4. List Comprehensions:"
    putStrLn "  Squares of 0-9:"
    let squares = [x * x | x <- [0..9]]
    putStrLn $ "    " ++ show squares

    putStrLn "  Even numbers from 0-19:"
    let evens = [x | x <- [0..19], even x]
    putStrLn $ "    " ++ show evens

    -- 5. Boolean logic
    putStrLn "\n5. Boolean Logic:"
    let x = 5
    let y = 10
    putStrLn $ "  x=" ++ show x ++ ", y=" ++ show y
    putStrLn $ "  x > 3 && y < 20: " ++ show (x > 3 && y < 20)
    putStrLn $ "  x > 10 || y > 5: " ++ show (x > 10 || y > 5)
    putStrLn $ "  not (x == y): " ++ show (not (x == y))

    putStrLn "\n  Only Bool type (no truthiness):"
    putStrLn "    Haskell requires explicit boolean expressions"
    putStrLn "    Cannot use numbers or lists directly in if conditions"

    -- 6. FizzBuzz
    putStrLn "\n6. FizzBuzz (1-20):"
    putStr " "
    putStrLn $ " " ++ intercalate " " (map fizzbuzz [1..20])

    -- Alternative FizzBuzz with pattern matching
    putStrLn "  FizzBuzz with pattern matching on remainders:"
    putStr " "
    putStrLn $ " " ++ intercalate " " (map fizzbuzzPattern [1..20])

    -- 7. Pattern matching
    putStrLn "\n7. Pattern Matching:"
    putStrLn $ "  describe 0: " ++ describeNumber 0
    putStrLn $ "  describe 1: " ++ describeNumber 1
    putStrLn $ "  describe 5: " ++ describeNumber 5

    -- Pattern matching on lists
    putStrLn "\n  Pattern matching on lists:"
    putStrLn $ "  []: " ++ describeList []
    putStrLn $ "  [1]: " ++ describeList [1]
    putStrLn $ "  [1,2]: " ++ describeList [1,2]
    putStrLn $ "  [1,2,3]: " ++ describeList [1,2,3]

    -- 8. Case expressions
    putStrLn "\n8. Case Expressions:"
    let day = 3
    let dayType = case day of
          0 -> "Weekend"
          6 -> "Weekend"
          d | d >= 1 && d <= 5 -> "Weekday"
            | otherwise -> "Invalid day"
    putStrLn $ "  Day " ++ show day ++ ": " ++ dayType

    -- More case examples
    putStrLn "  Describing data structures:"
    let point = (3, 4)
    putStrLn $ "  " ++ show point ++ ": " ++ describePoint point

    -- 9. Higher-order functions (map, filter, fold)
    putStrLn "\n9. Higher-Order Functions (replacing loops):"
    let numbers = [1..10]

    putStrLn $ "  Original: " ++ show numbers

    -- Map: transform each element
    let doubled = map (*2) numbers
    putStrLn $ "  Doubled (map): " ++ show doubled

    -- Filter: select elements
    let evenOnly = filter even numbers
    putStrLn $ "  Evens (filter): " ++ show evenOnly

    -- Fold: reduce to single value
    let total = foldl (+) 0 numbers
    putStrLn $ "  Sum (fold): " ++ show total

    -- 10. Multiple conditions (Grade Calculator)
    putStrLn "\n10. Multiple Conditions (Grade Calculator):"
    let scores = [95, 85, 75, 65, 55]
    mapM_ (\s -> putStrLn $ "  Score " ++ show s ++ ": Grade " ++ [letterGrade s]) scores

    -- 11. Recursion examples
    putStrLn "\n11. Recursion Examples:"
    putStrLn $ "  Factorial 5: " ++ show (factorial 5)
    putStrLn $ "  Fibonacci 10: " ++ show (fibonacci 10)
    putStrLn $ "  Sum 1-10: " ++ show (sumToN 10)

    -- 12. Multiplication table
    putStrLn "\n12. Multiplication Table for 5:"
    multiplicationTable 5

    putStrLn "\n=== Key Takeaway ==="
    putStrLn "Haskell uses expressions, not statements."
    putStrLn "Recursion and higher-order functions replace loops."
    putStrLn "Pattern matching provides elegant control flow."

-- Guards example
describeAge :: Int -> String
describeAge age
    | age >= 18 = "Adult"
    | age >= 13 = "Teenager"
    | otherwise = "Child"

-- Recursion example (instead of loops)
printNumbers :: Int -> Int -> IO ()
printNumbers current limit
    | current < limit = do
        putStr $ " " ++ show current
        printNumbers (current + 1) limit
    | otherwise = return ()

-- FizzBuzz with guards
fizzbuzz :: Int -> String
fizzbuzz n
    | n `mod` 15 == 0 = "FizzBuzz"
    | n `mod` 3 == 0  = "Fizz"
    | n `mod` 5 == 0  = "Buzz"
    | otherwise       = show n

-- FizzBuzz with pattern matching
fizzbuzzPattern :: Int -> String
fizzbuzzPattern n = case (n `mod` 3, n `mod` 5) of
    (0, 0) -> "FizzBuzz"
    (0, _) -> "Fizz"
    (_, 0) -> "Buzz"
    _      -> show n

-- Pattern matching example
describeNumber :: Int -> String
describeNumber 0 = "Zero"
describeNumber 1 = "One"
describeNumber 2 = "Two"
describeNumber _ = "Many"

-- Pattern matching on lists
describeList :: [Int] -> String
describeList [] = "Empty list"
describeList [x] = "Single element: " ++ show x
describeList [x, y] = "Two elements: " ++ show x ++ " and " ++ show y
describeList (x:y:_) = "Starts with " ++ show x ++ " and " ++ show y

-- Pattern matching on tuples (case expression)
describePoint :: (Int, Int) -> String
describePoint point = case point of
    (0, 0) -> "Origin"
    (0, y) -> "Y-axis at " ++ show y
    (x, 0) -> "X-axis at " ++ show x
    (x, y) -> "Point at (" ++ show x ++ ", " ++ show y ++ ")"

-- Grade calculator with guards
letterGrade :: Int -> Char
letterGrade score
    | score >= 90 = 'A'
    | score >= 80 = 'B'
    | score >= 70 = 'C'
    | score >= 60 = 'D'
    | otherwise   = 'F'

-- Recursive factorial
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Recursive Fibonacci
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- Recursive sum
sumToN :: Int -> Int
sumToN n
    | n <= 0    = 0
    | otherwise = n + sumToN (n - 1)

-- Alternatively, using built-in functions
sumToN' :: Int -> Int
sumToN' n = sum [1..n]

-- Multiplication table using mapM_
multiplicationTable :: Int -> IO ()
multiplicationTable n = mapM_ printRow [1..5]
  where printRow i = putStrLn $ "  " ++ show n ++ " x " ++ show i ++ " = " ++ show (n * i)
