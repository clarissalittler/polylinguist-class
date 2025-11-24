-- Lesson 5: Data Structures in Haskell
-- Demonstrates immutable lists, tuples, Maps, Sets

import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = do
    putStrLn "=== Haskell Data Structures ===\n"

    -- 1. Lists (immutable, linked)
    putStrLn "1. Lists (Immutable, Linked):"
    let numbers = [1, 2, 3, 4, 5]
    putStrLn $ "  Original: " ++ show numbers

    -- "Modifying" creates new list
    let withZero = 0 : numbers  -- Prepend
    putStrLn $ "  Prepend 0: " ++ show withZero
    putStrLn $ "  Original unchanged: " ++ show numbers

    let withSix = numbers ++ [6]  -- Append (expensive O(n))
    putStrLn $ "  Append 6: " ++ show withSix

    -- List operations
    putStrLn "\n  List operations:"
    putStrLn $ "  head: " ++ show (head numbers)
    putStrLn $ "  tail: " ++ show (tail numbers)
    putStrLn $ "  take 3: " ++ show (take 3 numbers)
    putStrLn $ "  drop 2: " ++ show (drop 2 numbers)

    -- 2. List comprehensions
    putStrLn "\n2. List Comprehensions:"
    let squared = [x * x | x <- numbers]
    putStrLn $ "  Squared: " ++ show squared

    let evens = [x | x <- [1..10], even x]
    putStrLn $ "  Evens 1-10: " ++ show evens

    -- 3. Tuples (immutable, fixed size)
    putStrLn "\n3. Tuples (Immutable, Fixed Size):"
    let point = (3, 4)
    putStrLn $ "  Point: " ++ show point
    putStrLn $ "  fst: " ++ show (fst point)
    putStrLn $ "  snd: " ++ show (snd point)

    let person = ("Alice", 30, "NYC")
    putStrLn $ "  Person: " ++ show person

    -- Pattern matching on tuples
    let (name, age, city) = person
    putStrLn $ "  Destructured: " ++ name ++ ", " ++ show age ++ ", " ++ city

    -- 4. Maps (immutable)
    putStrLn "\n4. Maps (Immutable):"
    let personMap = Map.fromList [("name", "Alice"), ("age", "30"), ("city", "NYC")]
    putStrLn $ "  Person map: " ++ show personMap

    putStrLn $ "  Lookup 'name': " ++ show (Map.lookup "name" personMap)

    -- "Modifying" creates new map
    let updated = Map.insert "age" "31" personMap
    putStrLn $ "  After insert age=31: " ++ show updated
    putStrLn $ "  Original unchanged: " ++ show personMap

    let deleted = Map.delete "city" personMap
    putStrLn $ "  After delete city: " ++ show deleted

    -- 5. Sets (immutable, unique)
    putStrLn "\n5. Sets (Immutable, Unique):"
    let numbersSet = Set.fromList [1, 2, 3, 4, 5]
    putStrLn $ "  Set: " ++ show numbersSet

    let withSix' = Set.insert 6 numbersSet
    let withThree = Set.insert 3 numbersSet  -- Already exists
    putStrLn $ "  After insert 6: " ++ show withSix'
    putStrLn $ "  After insert 3 (duplicate): " ++ show withThree

    putStrLn $ "  Member 3: " ++ show (Set.member 3 numbersSet)
    putStrLn $ "  Member 10: " ++ show (Set.member 10 numbersSet)

    -- Set operations
    let evensSet = Set.fromList [2, 4, 6, 8]
    let oddsSet = Set.fromList [1, 3, 5, 7]
    putStrLn $ "  Union: " ++ show (Set.union evensSet oddsSet)
    putStrLn $ "  Intersection: " ++ show (Set.intersection evensSet oddsSet)
    putStrLn $ "  Difference: " ++ show (Set.difference evensSet oddsSet)

    -- 6. Structural sharing
    putStrLn "\n6. Structural Sharing (Efficiency):"
    putStrLn "  Immutable structures share data internally"
    let list1 = [2, 3, 4, 5]
    let list2 = 1 : list1  -- [1, 2, 3, 4, 5]
    putStrLn $ "  list1: " ++ show list1
    putStrLn $ "  list2 = 1 : list1: " ++ show list2
    putStrLn "  list2 shares nodes with list1 (only 1 new node created)"

    -- 7. Infinite lists (lazy evaluation)
    putStrLn "\n7. Infinite Lists (Lazy Evaluation):"
    let naturals = [1..]
    putStrLn $ "  First 10 naturals: " ++ show (take 10 naturals)

    let fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
    putStrLn $ "  First 10 Fibonacci: " ++ show (take 10 fibs)

    -- 8. Pattern matching on lists
    putStrLn "\n8. Pattern Matching on Lists:"
    putStrLn $ "  describeList []: " ++ describeList []
    putStrLn $ "  describeList [1]: " ++ describeList [1]
    putStrLn $ "  describeList [1,2,3]: " ++ describeList [1,2,3]

    -- 9. Common list functions
    putStrLn "\n9. Common List Functions:"
    putStrLn $ "  length: " ++ show (length numbers)
    putStrLn $ "  reverse: " ++ show (reverse numbers)
    putStrLn $ "  sum: " ++ show (sum numbers)
    putStrLn $ "  product: " ++ show (product numbers)
    putStrLn $ "  maximum: " ++ show (maximum numbers)
    putStrLn $ "  minimum: " ++ show (minimum numbers)

    -- 10. Key insights
    putStrLn "\n10. Key Insights:"
    putStrLn "  - All data structures are IMMUTABLE"
    putStrLn "  - 'Modifications' create new structures"
    putStrLn "  - Structural sharing makes this efficient"
    putStrLn "  - No aliasing bugs possible"
    putStrLn "  - Thread-safe by default"
    putStrLn "  - Lazy evaluation enables infinite structures"

-- Helper function
describeList :: [a] -> String
describeList [] = "Empty"
describeList [_] = "Singleton"
describeList [_, _] = "Pair"
describeList _ = "Many elements"
