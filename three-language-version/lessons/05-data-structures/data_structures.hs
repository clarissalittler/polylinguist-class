-- Lesson 5: Data Structures in Haskell
-- Demonstrates immutable lists, tuples, Maps, Sets

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))

main :: IO ()
main = do
    putStrLn $ replicate 60 '='
    putStrLn "Haskell Data Structures Demonstration"
    putStrLn $ replicate 60 '='
    putStrLn ""

    demonstrateLists
    demonstrateListComprehensions
    demonstrateTuples
    demonstrateMaps
    demonstrateSets
    demonstrateStructuralSharing
    demonstrateInfiniteLists
    demonstratePatternMatching
    wordFrequencyCounter
    shoppingCartDemo
    performanceNotes

    putStrLn ""
    putStrLn $ replicate 60 '='
    putStrLn "Key Takeaways:"
    putStrLn "- All data structures are IMMUTABLE"
    putStrLn "- 'Modifications' create new structures"
    putStrLn "- Structural sharing makes this efficient"
    putStrLn "- No aliasing bugs possible"
    putStrLn "- Thread-safe by default"
    putStrLn "- Lazy evaluation enables infinite structures"
    putStrLn $ replicate 60 '='

-- Demonstrate Haskell lists
demonstrateLists :: IO ()
demonstrateLists = do
    putStrLn "=== 1. Lists (Immutable, Linked) ==="
    putStrLn ""

    let numbers = [1, 2, 3, 4, 5]
    let fruits = ["apple", "banana", "cherry"]

    putStrLn $ "Numbers: " ++ show numbers
    putStrLn $ "Fruits: " ++ show fruits

    -- Accessing elements
    putStrLn ""
    putStrLn $ "head numbers: " ++ show (head numbers)
    putStrLn $ "tail numbers: " ++ show (tail numbers)
    putStrLn $ "numbers !! 0: " ++ show (numbers !! 0)  -- O(n)!
    putStrLn $ "last numbers: " ++ show (last numbers)

    -- "Modifying" creates new lists (IMMUTABLE)
    let withZero = 0 : numbers  -- Prepend (fast!)
    let withSix = numbers ++ [6]  -- Append (slow!)

    putStrLn ""
    putStrLn $ "Prepend 0: " ++ show withZero
    putStrLn $ "Append 6: " ++ show withSix
    putStrLn $ "Original unchanged: " ++ show numbers

    -- List operations
    putStrLn ""
    putStrLn "--- List Operations ---"
    putStrLn $ "length: " ++ show (length numbers)
    putStrLn $ "reverse: " ++ show (reverse numbers)
    putStrLn $ "take 3: " ++ show (take 3 numbers)
    putStrLn $ "drop 2: " ++ show (drop 2 numbers)
    putStrLn $ "sum: " ++ show (sum numbers)
    putStrLn $ "product: " ++ show (product numbers)
    putStrLn $ "maximum: " ++ show (maximum numbers)
    putStrLn $ "minimum: " ++ show (minimum numbers)

    -- Cons operator (:)
    putStrLn ""
    putStrLn "--- Cons Operator (:) ---"
    let list1 = 1 : []            -- [1]
    let list2 = 1 : 2 : []        -- [1, 2]
    let list3 = 1 : 2 : 3 : []    -- [1, 2, 3]
    putStrLn $ "1 : []: " ++ show list1
    putStrLn $ "1 : 2 : []: " ++ show list2
    putStrLn $ "1 : 2 : 3 : []: " ++ show list3

-- Demonstrate list comprehensions
demonstrateListComprehensions :: IO ()
demonstrateListComprehensions = do
    putStrLn ""
    putStrLn "=== 2. List Comprehensions ==="
    putStrLn ""

    let numbers = [1..5]

    -- Basic comprehension
    let squared = [x * x | x <- numbers]
    putStrLn $ "Squared: " ++ show squared

    -- With condition
    let evens = [x | x <- [1..10], even x]
    putStrLn $ "Evens 1-10: " ++ show evens

    -- Multiple generators
    let pairs = [(x, y) | x <- [1..3], y <- [1..3]]
    putStrLn $ "Pairs: " ++ show pairs

    -- Multiple conditions
    let pythagorean = [(a, b, c) | c <- [1..10],
                                     b <- [1..c],
                                     a <- [1..b],
                                     a^2 + b^2 == c^2]
    putStrLn $ "Pythagorean triples: " ++ show pythagorean

    -- FizzBuzz with comprehension
    let fizzbuzz n
            | n `mod` 15 == 0 = "FizzBuzz"
            | n `mod` 3 == 0  = "Fizz"
            | n `mod` 5 == 0  = "Buzz"
            | otherwise       = show n

    let fb = [fizzbuzz i | i <- [1..20]]
    putStrLn $ "FizzBuzz 1-20: " ++ show fb

-- Demonstrate tuples
demonstrateTuples :: IO ()
demonstrateTuples = do
    putStrLn ""
    putStrLn "=== 3. Tuples (Immutable, Fixed Size) ==="
    putStrLn ""

    let point = (3, 4) :: (Int, Int)
    let person = ("Alice", 30, "NYC") :: (String, Int, String)

    putStrLn $ "Point: " ++ show point
    putStrLn $ "Person: " ++ show person

    -- Accessing pairs with fst/snd
    putStrLn ""
    putStrLn $ "fst point: " ++ show (fst point)
    putStrLn $ "snd point: " ++ show (snd point)

    -- Pattern matching
    let (name, age, city) = person
    putStrLn ""
    putStrLn $ "Destructured: " ++ name ++ ", " ++ show age ++ ", " ++ city

    -- Tuples in functions
    let addPoints (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
    let p1 = (1, 2)
    let p2 = (3, 4)
    putStrLn ""
    putStrLn $ "addPoints " ++ show p1 ++ " " ++ show p2 ++ " = " ++ show (addPoints p1 p2)

    -- Different sizes are different types
    putStrLn ""
    putStrLn "Note: (Int, Int) and (Int, Int, Int) are different types"

-- Demonstrate Maps
demonstrateMaps :: IO ()
demonstrateMaps = do
    putStrLn ""
    putStrLn "=== 4. Maps (Immutable) ==="
    putStrLn ""

    let person = Map.fromList [("name", "Alice"), ("age", "30"), ("city", "NYC")]
    putStrLn $ "Person map: " ++ show person

    -- Lookup
    putStrLn ""
    putStrLn $ "Lookup 'name': " ++ show (Map.lookup "name" person)
    putStrLn $ "Lookup 'country': " ++ show (Map.lookup "country" person)

    -- "Modifying" creates new map
    let updated = Map.insert "age" "31" person
    let deleted = Map.delete "city" person

    putStrLn ""
    putStrLn $ "After insert age=31: " ++ show updated
    putStrLn $ "After delete city: " ++ show deleted
    putStrLn $ "Original unchanged: " ++ show person

    -- Map operations
    putStrLn ""
    putStrLn "--- Map Operations ---"
    putStrLn $ "member 'name': " ++ show (Map.member "name" person)
    putStrLn $ "member 'country': " ++ show (Map.member "country" person)
    putStrLn $ "size: " ++ show (Map.size person)
    putStrLn $ "keys: " ++ show (Map.keys person)
    putStrLn $ "values: " ++ show (Map.elems person)

-- Demonstrate Sets
demonstrateSets :: IO ()
demonstrateSets = do
    putStrLn ""
    putStrLn "=== 5. Sets (Immutable, Unique) ==="
    putStrLn ""

    let numbers = Set.fromList [1, 2, 3, 4, 5]
    putStrLn $ "Numbers: " ++ show numbers

    -- Adding elements (creates new set)
    let withSix = Set.insert 6 numbers
    let withThree = Set.insert 3 numbers  -- Already exists

    putStrLn ""
    putStrLn $ "After insert 6: " ++ show withSix
    putStrLn $ "After insert 3 (duplicate): " ++ show withThree
    putStrLn $ "Original unchanged: " ++ show numbers

    -- Membership testing
    putStrLn ""
    putStrLn $ "member 3: " ++ show (Set.member 3 numbers)
    putStrLn $ "member 10: " ++ show (Set.member 10 numbers)

    -- Set operations
    putStrLn ""
    putStrLn "--- Set Operations ---"
    let evens = Set.fromList [2, 4, 6, 8]
    let odds = Set.fromList [1, 3, 5, 7]

    putStrLn $ "Evens: " ++ show evens
    putStrLn $ "Odds: " ++ show odds
    putStrLn $ "Union: " ++ show (Set.union evens odds)
    putStrLn $ "Intersection: " ++ show (Set.intersection evens odds)
    putStrLn $ "Difference: " ++ show (Set.difference evens odds)

    -- Remove duplicates
    putStrLn ""
    let duplicates = [1, 2, 2, 3, 3, 3, 4, 4, 4, 4]
    let unique = Set.fromList duplicates
    putStrLn $ "Remove duplicates: " ++ show duplicates ++ " -> " ++ show unique

-- Demonstrate structural sharing
demonstrateStructuralSharing :: IO ()
demonstrateStructuralSharing = do
    putStrLn ""
    putStrLn "=== 6. Structural Sharing (Efficiency) ==="
    putStrLn ""

    putStrLn "Immutable structures share data internally"
    let list1 = [2, 3, 4, 5]
    let list2 = 1 : list1  -- [1, 2, 3, 4, 5]

    putStrLn $ "list1: " ++ show list1
    putStrLn $ "list2 = 1 : list1: " ++ show list2
    putStrLn "list2 shares nodes with list1 (only 1 new node created)"

    putStrLn ""
    putStrLn "Visualization:"
    putStrLn "  list2: [1] -> list1"
    putStrLn "                [2] -> [3] -> [4] -> [5] -> []"
    putStrLn ""
    putStrLn "This makes 'copying' very efficient!"

-- Demonstrate infinite lists
demonstrateInfiniteLists :: IO ()
demonstrateInfiniteLists = do
    putStrLn ""
    putStrLn "=== 7. Infinite Lists (Lazy Evaluation) ==="
    putStrLn ""

    let naturals = [1..]
    putStrLn $ "First 10 naturals: " ++ show (take 10 naturals)

    let evens = [2, 4..]
    putStrLn $ "First 10 evens: " ++ show (take 10 evens)

    -- Fibonacci numbers (infinite!)
    let fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
    putStrLn $ "First 15 Fibonacci: " ++ show (take 15 fibs)

    -- Primes using Sieve of Eratosthenes
    let sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
    let primes = sieve [2..]
    putStrLn $ "First 20 primes: " ++ show (take 20 primes)

    putStrLn ""
    putStrLn "Note: These lists are infinite, but lazy evaluation"
    putStrLn "only computes what we actually need!"

-- Demonstrate pattern matching on lists
demonstratePatternMatching :: IO ()
demonstratePatternMatching = do
    putStrLn ""
    putStrLn "=== 8. Pattern Matching on Lists ==="
    putStrLn ""

    putStrLn $ "describeList []: " ++ describeList []
    putStrLn $ "describeList [1]: " ++ describeList [1]
    putStrLn $ "describeList [1,2]: " ++ describeList [1,2]
    putStrLn $ "describeList [1,2,3]: " ++ describeList [1,2,3]

    putStrLn ""
    putStrLn $ "sumList [1,2,3,4,5]: " ++ show (sumList [1,2,3,4,5])
    putStrLn $ "lengthList [1,2,3,4]: " ++ show (lengthList [1,2,3,4])

-- Helper functions for pattern matching demo
describeList :: [a] -> String
describeList [] = "Empty"
describeList [_] = "Singleton"
describeList [_, _] = "Pair"
describeList _ = "Many elements"

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

lengthList :: [a] -> Int
lengthList [] = 0
lengthList (_:xs) = 1 + lengthList xs

-- Word frequency counter
wordFrequencyCounter :: IO ()
wordFrequencyCounter = do
    putStrLn ""
    putStrLn "=== 9. Word Frequency Counter ==="
    putStrLn ""

    let text = "the quick brown fox jumps over the lazy dog the fox"
    let ws = words text

    -- Count frequencies using Map
    let countWord m word = Map.insertWith (+) word 1 m
    let freq = foldl countWord Map.empty ws

    putStrLn $ "Text: " ++ text
    putStrLn ""
    putStrLn "Word frequencies:"

    -- Sort by frequency (descending)
    let freqList = Map.toList freq
    let sorted = sortBy (comparing (Down . snd)) freqList

    mapM_ (\(word, count) -> putStrLn $ "  " ++ word ++ ": " ++ show count) sorted

-- Shopping cart demo (immutable version)
shoppingCartDemo :: IO ()
shoppingCartDemo = do
    putStrLn ""
    putStrLn "=== 10. Shopping Cart (Immutable) ==="
    putStrLn ""

    -- Represent items as tuples
    type Item = (String, Double, Int)  -- (name, price, quantity)
    type Cart = [Item]

    let addItem :: Cart -> String -> Double -> Int -> Cart
        addItem cart name price qty =
            case findAndUpdate cart of
                Just updated -> updated
                Nothing -> cart ++ [(name, price, qty)]
            where
                findAndUpdate [] = Nothing
                findAndUpdate ((n, p, q):rest)
                    | n == name = Just ((n, p, q + qty) : rest)
                    | otherwise = ((n, p, q) :) <$> findAndUpdate rest

    let calculateTotal :: Cart -> Double
        calculateTotal cart = sum [price * fromIntegral qty | (_, price, qty) <- cart]

    let printCart :: Cart -> IO ()
        printCart cart = do
            putStrLn "Cart contents:"
            mapM_ printItem cart
            putStrLn $ "Total: $" ++ show (calculateTotal cart)
            where
                printItem (name, price, qty) =
                    let subtotal = price * fromIntegral qty
                    in putStrLn $ "  " ++ name ++ ": $" ++ show price ++
                                  " x " ++ show qty ++ " = $" ++ show subtotal

    -- Use the cart (immutable operations)
    let cart1 = []
    let cart2 = addItem cart1 "Apple" 0.50 5
    let cart3 = addItem cart2 "Banana" 0.30 3
    let cart4 = addItem cart3 "Orange" 0.75 4

    printCart cart4

    putStrLn ""
    putStrLn "Adding more apples..."
    let cart5 = addItem cart4 "Apple" 0.50 3

    printCart cart5

    putStrLn ""
    putStrLn "Note: Each operation creates a new cart."
    putStrLn "Original carts are unchanged (immutable)!"

-- Performance notes
performanceNotes :: IO ()
performanceNotes = do
    putStrLn ""
    putStrLn "=== 11. Performance Notes ==="
    putStrLn ""

    putStrLn "Time Complexities:"
    putStrLn ""
    putStrLn "Lists:"
    putStrLn "  - Prepend (cons :): O(1)"
    putStrLn "  - Append (++): O(n)"
    putStrLn "  - Access by index (!!): O(n)"
    putStrLn "  - head/tail: O(1)"
    putStrLn "  - length: O(n)"

    putStrLn ""
    putStrLn "Maps (balanced tree):"
    putStrLn "  - Lookup: O(log n)"
    putStrLn "  - Insert: O(log n)"
    putStrLn "  - Delete: O(log n)"

    putStrLn ""
    putStrLn "Sets (balanced tree):"
    putStrLn "  - Membership: O(log n)"
    putStrLn "  - Insert: O(log n)"
    putStrLn "  - Union/Intersection: O(n log n)"

    putStrLn ""
    putStrLn "Key Insight: Immutability + structural sharing"
    putStrLn "  - Most operations are efficient despite immutability"
    putStrLn "  - Lazy evaluation avoids unnecessary computation"
    putStrLn "  - No garbage from mutations"
