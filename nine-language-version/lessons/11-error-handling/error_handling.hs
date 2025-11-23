-- Lesson 11: Error Handling in Haskell - Use Maybe/Either, avoid exceptions

import Control.Exception (catch, SomeException)

-- Using Maybe for operations that might fail
safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

-- Using Either for errors with information
divide :: Double -> Double -> Either String Double
divide _ 0 = Left "Cannot divide by zero"
divide x y = Right (x / y)

-- Chaining operations
compute :: Double -> Double -> Either String Double
compute x y = do
    result1 <- divide x y
    result2 <- divide result1 2
    return result2

main :: IO ()
main = do
    putStrLn "=== Error Handling in Haskell ===\n"

    putStrLn "1. Maybe type:"
    print $ safeDivide 10 2
    print $ safeDivide 10 0

    putStrLn "\n2. Either type:"
    print $ divide 10 2
    print $ divide 10 0

    putStrLn "\n3. Error chaining:"
    print $ compute 10 2
    print $ compute 10 0

    putStrLn "\n=== Haskell Error Handling ==="
    putStrLn "- Maybe for nullable results"
    putStrLn "- Either for errors with info"
    putStrLn "- Avoid exceptions, use types"
