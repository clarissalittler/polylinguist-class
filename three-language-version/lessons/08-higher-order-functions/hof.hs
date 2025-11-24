{- Lesson 8: Higher-Order Functions in Haskell
   Demonstrates native functional programming with higher-order functions
-}

main :: IO ()
main = do
    putStrLn $ replicate 70 '='
    putStrLn "LESSON 8: HIGHER-ORDER FUNCTIONS IN HASKELL"
    putStrLn $ replicate 70 '='

    -- PART 1: First-Class Functions
    putStrLn "\n--- PART 1: FIRST-CLASS FUNCTIONS ---"
    let square = \x -> x * x
    let cube = \x -> x * x * x
    let operations = [square, cube, (*2)]
    putStrLn $ "Operations on 5: " ++ show (map (\f -> f 5) operations)

    -- PART 2: Map, Filter, Fold
    putStrLn "\n--- PART 2: MAP, FILTER, FOLD ---"
    let numbers = [1..5]
    putStrLn $ "Original: " ++ show numbers
    putStrLn $ "Map (double): " ++ show (map (*2) numbers)
    putStrLn $ "Filter (evens): " ++ show (filter even numbers)
    putStrLn $ "Fold (sum): " ++ show (foldl (+) 0 numbers)
    putStrLn $ "Fold (product): " ++ show (foldl (*) 1 numbers)

    -- Combined pipeline
    let result = foldl (+) 0 $ map (^2) $ filter even numbers
    putStrLn $ "Sum of squares of evens: " ++ show result

    -- PART 3: Higher-Order Functions
    putStrLn "\n--- PART 3: HIGHER-ORDER FUNCTIONS ---"

    let applyTwice f x = f (f x)
    let compose f g = \x -> f (g x)

    putStrLn $ "applyTwice (+1) 5 = " ++ show (applyTwice (+1) 5)

    let addOne = (+1)
    let timesTwo = (*2)
    let composed = compose timesTwo addOne
    putStrLn $ "compose timesTwo addOne 5 = " ++ show (composed 5)

    -- Function composition operator (.)
    let composedOp = timesTwo . addOne
    putStrLn $ "(timesTwo . addOne) 5 = " ++ show (composedOp 5)

    -- PART 4: Functions Returning Functions (Currying is native!)
    putStrLn "\n--- PART 4: CURRYING (NATIVE IN HASKELL) ---"

    let makeMultiplier n = \x -> x * n
    let timesThree = makeMultiplier 3
    let timesFive = makeMultiplier 5
    putStrLn $ "timesThree 10 = " ++ show (timesThree 10)
    putStrLn $ "timesFive 4 = " ++ show (timesFive 4)

    -- All functions are curried by default!
    let add x y = x + y
    let add10 = add 10  -- Partial application
    putStrLn $ "add10 5 = " ++ show (add10 5)

    -- PART 5: Practical Examples
    putStrLn "\n--- PART 5: PRACTICAL EXAMPLES ---"

    data Person = Person { name :: String, age :: Int }
    instance Show Person where
        show (Person n a) = n ++ " (" ++ show a ++ ")"

    let people = [Person "Alice" 30, Person "Bob" 25, Person "Charlie" 35]
    let sortedByAge = sortBy (\p1 p2 -> compare (age p1) (age p2)) people

    import Data.List (sortBy)
    putStrLn $ "People sorted by age: " ++ show (map name sortedByAge)

    -- PART 6: Point-Free Style
    putStrLn "\n--- PART 6: POINT-FREE STYLE ---"

    -- With explicit parameter
    let doubleAll1 xs = map (*2) xs
    -- Point-free (no explicit parameter)
    let doubleAll2 = map (*2)

    putStrLn $ "doubleAll [1,2,3]: " ++ show (doubleAll2 [1,2,3])

    -- Complex composition
    let processData = sum . map (^2) . filter even
    putStrLn $ "processData [1..10]: " ++ show (processData [1..10])

    putStrLn $ "\n" ++ replicate 70 '='
    putStrLn "Haskell: Native functional programming with:"
    putStrLn "  - Automatic currying"
    putStrLn "  - Built-in composition (.)"
    putStrLn "  - Lazy evaluation"
    putStrLn "  - Point-free style"
    putStrLn $ replicate 70 '='
