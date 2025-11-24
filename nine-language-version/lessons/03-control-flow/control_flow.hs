-- Lesson 3: Control Flow in Haskell
-- Demonstrates conditionals, guards, pattern matching, and recursion

module Main where

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
    putStrLn $ "  Status: " ++ status

    -- 2. Guards (preferred Haskell style)
    putStrLn "\n2. Guards (idiomatic Haskell):"
    putStrLn $ "  Age " ++ show age ++ ": " ++ describeAge age

    -- 3. No traditional loops - use recursion
    putStrLn "\n3. Recursion (no traditional loops):"
    putStrLn "  Count to 5:"
    putStr "   "
    printNumbers 0 5
    putStrLn ""

    putStrLn "  Iterate list:"
    let fruits = ["apple", "banana", "cherry"]
    mapM_ (\f -> putStrLn $ "    " ++ f) fruits

    -- 4. List comprehensions (alternative to loops)
    putStrLn "\n4. List Comprehensions:"
    putStrLn "  Squares of 1-5:"
    putStr "   "
    putStrLn $ unwords [show (x * x) | x <- [1..5]]

    putStrLn "  Even numbers from 1-10:"
    putStr "   "
    putStrLn $ unwords [show x | x <- [1..10], even x]

    -- 5. Boolean logic
    putStrLn "\n5. Boolean Logic:"
    let x = 5
    let y = 10
    putStrLn $ "  x=" ++ show x ++ ", y=" ++ show y
    putStrLn $ "  x > 3 && y < 20: " ++ show (x > 3 && y < 20)
    putStrLn $ "  x > 10 || y > 5: " ++ show (x > 10 || y > 5)
    putStrLn $ "  not (x == y): " ++ show (not (x == y))

    putStrLn "\n  Only Bool type (no truthiness):"
    putStrLn "    Must use explicit boolean expressions"

    -- 6. FizzBuzz
    putStrLn "\n6. FizzBuzz (1-20):"
    putStr " "
    mapM_ (\x -> putStr $ " " ++ fizzbuzz x) [1..20]
    putStrLn ""

    -- 7. Pattern matching
    putStrLn "\n7. Pattern Matching:"
    putStrLn $ "  describe 0: " ++ describeNumber 0
    putStrLn $ "  describe 1: " ++ describeNumber 1
    putStrLn $ "  describe 5: " ++ describeNumber 5

    -- Pattern matching on lists
    putStrLn "\n  Pattern matching on lists:"
    putStrLn $ "  []: " ++ describeList []
    putStrLn $ "  [1]: " ++ describeList [1]
    putStrLn $ "  [1,2,3]: " ++ describeList [1,2,3]

    -- 8. Case expressions
    putStrLn "\n8. Case Expressions:"
    let day = 3
    let dayType = case day of
          0 -> "Weekend"
          6 -> "Weekend"
          _ | day >= 1 && day <= 5 -> "Weekday"
            | otherwise -> "Invalid day"
    putStrLn $ "  Day " ++ show day ++ ": " ++ dayType

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

-- Pattern matching example
describeNumber :: Int -> String
describeNumber 0 = "Zero"
describeNumber 1 = "One"
describeNumber 2 = "Two"
describeNumber _ = "Many"

-- Pattern matching on lists
describeList :: [Int] -> String
describeList [] = "Empty list"
describeList [_] = "Single element"
describeList [_, _] = "Two elements"
describeList (x:y:_) = "Starts with " ++ show x ++ " and " ++ show y
