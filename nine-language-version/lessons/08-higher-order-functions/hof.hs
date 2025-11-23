-- Lesson 8: Higher-Order Functions in Haskell
--
-- Haskell is THE functional language - HOFs are everywhere!
-- - All functions are curried by default
-- - Function composition operator (.)
-- - Map, filter, fold built-in
-- - Lazy evaluation
-- - Type system helps catch errors

import Data.List (sort, sortBy)
import Data.Char (toUpper, toLower)

-- ====================
-- 1. Functions as First-Class Values
-- ====================

-- Functions are first-class - can be assigned, passed, returned
greet :: String -> String
greet name = "Hello, " ++ name ++ "!"

-- Store functions in a list
operations :: [Int -> Int]
operations = [(*2), (+1), (^2), (\x -> x - 1)]

applyAll :: [Int -> Int] -> Int -> [Int]
applyAll funcs x = map (\f -> f x) funcs

-- ====================
-- 2. Functions Taking Functions
-- ====================

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes 0 _ x = x
applyNTimes n f x = applyNTimes (n - 1) f (f x)

-- ====================
-- 3. Functions Returning Functions (Currying)
-- ====================

-- All Haskell functions are curried by default!
add :: Int -> Int -> Int
add x y = x + y

-- Partial application is automatic
addFive :: Int -> Int
addFive = add 5  -- Partially applied!

-- Explicit function returning function
makeMultiplier :: Int -> (Int -> Int)
makeMultiplier n = \x -> x * n

makeAdder :: Int -> (Int -> Int)
makeAdder n = (+n)  -- Can use operator sections

-- ====================
-- 4. Map - Transform Each Element
-- ====================

-- map is built-in!
demonstrateMap :: IO ()
demonstrateMap = do
    let numbers = [1, 2, 3, 4, 5]

    -- Map with lambda
    putStrLn $ "   Doubled: " ++ show (map (*2) numbers)

    -- Map with named function
    putStrLn $ "   Squared: " ++ show (map (^2) numbers)

    -- Map multiple times (composition!)
    let result = map (*2) (map (+1) numbers)
    putStrLn $ "   Add 1 then double: " ++ show result

    -- Better: use composition
    let result2 = map ((*2) . (+1)) numbers
    putStrLn $ "   Same with composition: " ++ show result2

-- ====================
-- 5. Filter - Select Elements
-- ====================

-- filter is built-in!
demonstrateFilter :: IO ()
demonstrateFilter = do
    let numbers = [1..10]

    putStrLn $ "   Evens: " ++ show (filter even numbers)
    putStrLn $ "   Odds: " ++ show (filter odd numbers)
    putStrLn $ "   Greater than 5: " ++ show (filter (>5) numbers)

    -- Complex predicate
    let bigEvens = filter (\x -> even x && x > 5) numbers
    putStrLn $ "   Big evens: " ++ show bigEvens

-- ====================
-- 6. Fold (Reduce)
-- ====================

-- foldl = fold left, foldr = fold right
demonstrateFold :: IO ()
demonstrateFold = do
    let numbers = [1, 2, 3, 4, 5]

    -- Sum
    putStrLn $ "   Sum: " ++ show (foldl (+) 0 numbers)

    -- Product
    putStrLn $ "   Product: " ++ show (foldl (*) 1 numbers)

    -- Maximum
    putStrLn $ "   Max: " ++ show (foldl1 max numbers)

    -- Build a list backwards (demonstrating fold)
    let reversed = foldl (flip (:)) [] numbers
    putStrLn $ "   Reversed: " ++ show reversed

    -- Fold right vs left
    let words = ["Hello", "world", "from", "Haskell"]
    putStrLn $ "   foldr: " ++ foldr (\w acc -> w ++ " " ++ acc) "" words
    putStrLn $ "   foldl: " ++ foldl (\acc w -> acc ++ " " ++ w) "" words

-- ====================
-- 7. Closures (Captured Variables)
-- ====================

-- Haskell is pure, so "mutable closures" don't exist the same way
-- But we can return functions that capture values

makeCounter :: Int -> (Int -> Int)
makeCounter start = \x -> start + x

-- Using currying for closure-like behavior
addWithBase :: Int -> Int -> Int
addWithBase base value = base + value

-- ====================
-- 8. Currying and Partial Application
-- ====================

-- All functions are curried by default!
demonstrateCurrying :: IO ()
demonstrateCurrying = do
    -- Multi-argument function
    let power :: Int -> Int -> Int
        power base exp = base ^ exp

    -- Partial application
    let square = power 2
        cube = power 3

    putStrLn $ "   square 5 = " ++ show (square 5)
    putStrLn $ "   cube 5 = " ++ show (cube 5)

    -- Operator sections
    let addTen = (+10)
        divideByTwo = (/2)
        doubleIt = (*2)

    putStrLn $ "   addTen 5 = " ++ show (addTen 5)
    putStrLn $ "   doubleIt 5 = " ++ show (doubleIt 5)

-- ====================
-- 9. Function Composition
-- ====================

-- (.) is the composition operator!
demonstrateComposition :: IO ()
demonstrateComposition = do
    let addOne = (+1)
        double = (*2)
        square = (^2)

    -- Right-to-left composition
    let f = double . addOne
    putStrLn $ "   (double . addOne) 5 = " ++ show (f 5)

    -- Chain multiple
    let g = square . double . addOne
    putStrLn $ "   (square . double . addOne) 5 = " ++ show (g 5)

    -- Point-free style (no explicit argument)
    let processNumber = (\x -> x - 10) . square . double . addOne
    putStrLn $ "   Complex pipeline on 5 = " ++ show (processNumber 5)

    -- Composition with map
    let result = map (square . double) [1, 2, 3, 4, 5]
    putStrLn $ "   map (square . double) [1..5] = " ++ show result

-- ====================
-- 10. Common Higher-Order Functions
-- ====================

demonstrateCommon :: IO ()
demonstrateCommon = do
    let numbers = [1, 2, 3, 4, 5]

    -- all - all elements satisfy predicate
    putStrLn $ "   all (>0) numbers: " ++ show (all (>0) numbers)

    -- any - any element satisfies predicate
    putStrLn $ "   any even numbers: " ++ show (any even numbers)

    -- take - first n elements
    putStrLn $ "   take 3: " ++ show (take 3 numbers)

    -- drop - skip first n elements
    putStrLn $ "   drop 3: " ++ show (drop 3 numbers)

    -- takeWhile - take while predicate true
    putStrLn $ "   takeWhile (<4): " ++ show (takeWhile (<4) numbers)

    -- dropWhile - drop while predicate true
    putStrLn $ "   dropWhile (<4): " ++ show (dropWhile (<4) numbers)

    -- zip - combine two lists
    let list1 = [1, 2, 3]
        list2 = ['a', 'b', 'c']
    putStrLn $ "   zip: " ++ show (zip list1 list2)

    -- zipWith - combine with function
    putStrLn $ "   zipWith (+): " ++ show (zipWith (+) [1,2,3] [4,5,6])

-- ====================
-- 11. List Comprehensions (Alternative to HOFs)
-- ====================

demonstrateComprehensions :: IO ()
demonstrateComprehensions = do
    -- List comprehension = syntactic sugar for map/filter
    let numbers = [1..10]

    -- Equivalent to map (*2) numbers
    putStrLn $ "   [x*2 | x <- numbers]: " ++ show [x*2 | x <- numbers]

    -- Equivalent to filter even numbers
    putStrLn $ "   [x | x <- numbers, even x]: " ++ show [x | x <- numbers, even x]

    -- Combined map and filter
    putStrLn $ "   [x^2 | x <- numbers, x > 5]: " ++ show [x^2 | x <- numbers, x > 5]

-- ====================
-- 12. Custom HOFs
-- ====================

customMap :: (a -> b) -> [a] -> [b]
customMap _ [] = []
customMap f (x:xs) = f x : customMap f xs

customFilter :: (a -> Bool) -> [a] -> [a]
customFilter _ [] = []
customFilter p (x:xs)
    | p x       = x : customFilter p xs
    | otherwise = customFilter p xs

customFoldl :: (b -> a -> b) -> b -> [a] -> b
customFoldl _ acc [] = acc
customFoldl f acc (x:xs) = customFoldl f (f acc x) xs

-- ====================
-- 13. Higher-Order Functions with Type Classes
-- ====================

-- We can write very generic HOFs
twice :: (a -> a) -> a -> a
twice f = f . f

-- Works with any type!
demonstrateTwice :: IO ()
demonstrateTwice = do
    putStrLn $ "   twice (+1) 5 = " ++ show (twice (+1) 5)
    putStrLn $ "   twice (*2) 3 = " ++ show (twice (*2) 3)
    putStrLn $ "   twice reverse \"abc\" = " ++ twice reverse "abc"

-- ====================
-- 14. Flip - Swap Arguments
-- ====================

-- flip is built-in: reverses argument order
demonstrateFlip :: IO ()
demonstrateFlip = do
    let divide = (/)
        divideBy2 = flip divide 2  -- Now 2 is first argument

    putStrLn $ "   10 / 2 = " ++ show (divide 10 2)
    putStrLn $ "   divideBy2 10 = " ++ show (divideBy2 10)

    -- Useful with folds
    let numbers = [1, 2, 3, 4, 5]
        reversed = foldl (flip (:)) [] numbers
    putStrLn $ "   foldl (flip (:)) [] = " ++ show reversed

-- ====================
-- 15. Real-World Example: Data Pipeline
-- ====================

data Person = Person { name :: String, age :: Int } deriving (Show)

processData :: [Person] -> [Person]
processData = take 10                           -- Top 10
            . sortBy (\a b -> compare (age b) (age a))  -- Sort by age desc
            . map (\p -> p { name = toLowerName (name p) })  -- Normalize
            . filter ((>= 18) . age)            -- Adults only
    where
        toLowerName = map toLower

-- Point-free style!
sumOfSquaresOfPositives :: [Int] -> Int
sumOfSquaresOfPositives = sum . map (^2) . filter (>0)

-- ====================
-- Main Demonstration
-- ====================

main :: IO ()
main = do
    putStrLn "=== Higher-Order Functions in Haskell ===\n"

    -- 1. First-class functions
    putStrLn "1. Functions as First-Class Values:"
    putStrLn $ "   applyAll operations 5 = " ++ show (applyAll operations 5)

    -- 2. Functions taking functions
    putStrLn "\n2. Functions Taking Functions:"
    putStrLn $ "   applyTwice (+1) 5 = " ++ show (applyTwice (+1) 5)
    putStrLn $ "   applyNTimes 3 (*2) 2 = " ++ show (applyNTimes 3 (*2) 2)

    -- 3. Currying
    putStrLn "\n3. Currying (Automatic in Haskell!):"
    let timesThree = makeMultiplier 3
    putStrLn $ "   makeMultiplier 3 7 = " ++ show (timesThree 7)
    putStrLn $ "   addFive 10 = " ++ show (addFive 10)

    -- 4. Map
    putStrLn "\n4. Map - Transform Each Element:"
    demonstrateMap

    -- 5. Filter
    putStrLn "\n5. Filter - Select Elements:"
    demonstrateFilter

    -- 6. Fold
    putStrLn "\n6. Fold - Combine to Single Value:"
    demonstrateFold

    -- 7. Currying and partial application
    putStrLn "\n7. Currying and Partial Application:"
    demonstrateCurrying

    -- 8. Function composition
    putStrLn "\n8. Function Composition:"
    demonstrateComposition

    -- 9. Common HOFs
    putStrLn "\n9. Common Higher-Order Functions:"
    demonstrateCommon

    -- 10. List comprehensions
    putStrLn "\n10. List Comprehensions:"
    demonstrateComprehensions

    -- 11. Custom implementations
    putStrLn "\n11. Custom HOF Implementations:"
    let numbers = [1, 2, 3, 4, 5]
    putStrLn $ "   customMap (*2): " ++ show (customMap (*2) numbers)
    putStrLn $ "   customFilter even: " ++ show (customFilter even numbers)
    putStrLn $ "   customFoldl (+) 0: " ++ show (customFoldl (+) 0 numbers)

    -- 12. Twice
    putStrLn "\n12. Generic twice Function:"
    demonstrateTwice

    -- 13. Flip
    putStrLn "\n13. Flip - Reverse Arguments:"
    demonstrateFlip

    -- 14. Real-world example
    putStrLn "\n14. Real-World Data Pipeline:"
    let people = [Person "alice" 25, Person "BOB" 17, Person "charlie" 30]
        processed = processData people
    putStrLn $ "   Processed: " ++ show processed

    putStrLn "\n15. Point-Free Style:"
    putStrLn $ "   sumOfSquaresOfPositives [-1,2,3,-4,5] = " ++
               show (sumOfSquaresOfPositives [-1,2,3,-4,5])

    putStrLn "\n16. Key Haskell Features:"
    putStrLn "   - All functions are curried by default"
    putStrLn "   - Composition operator (.) for elegant pipelines"
    putStrLn "   - Point-free style (no explicit arguments)"
    putStrLn "   - Lazy evaluation (infinite lists possible!)"
    putStrLn "   - Strong type system catches errors"
