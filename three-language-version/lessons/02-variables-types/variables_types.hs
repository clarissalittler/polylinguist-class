{- variables_types.hs -}

-- Type signatures (optional but recommended)
age :: Int
age = 25

price :: Double
price = 19.99

name :: String
name = "Alice"

isStudent :: Bool
isStudent = True

hasGraduated :: Bool
hasGraduated = False

-- Type inference - no signature needed
inferredInt = 42        -- Haskell infers: Num a => a
inferredString = "hi"   -- Inferred as String
inferredList = [1, 2, 3] -- Inferred as Num a => [a]

-- All "variables" are actually constants (immutable)
-- age = 30  -- ERROR: cannot redefine

-- Polymorphic function
identity :: a -> a      -- works for any type 'a'
identity x = x

-- Tuples (fixed size, mixed types)
person :: (String, Int)
person = ("Alice", 25)

personWithTitle :: (String, Int, String)
personWithTitle = ("Bob", 30, "Engineer")

-- Lists (variable size, same type)
numbers :: [Int]
numbers = [1, 2, 3, 4, 5]

names :: [String]
names = ["Alice", "Bob", "Charlie"]

-- Maybe type (for nullable values)
findUser :: Int -> Maybe String
findUser 1 = Just "Alice"
findUser 2 = Just "Bob"
findUser _ = Nothing

-- Type conversion examples
intToDouble :: Int -> Double
intToDouble n = fromIntegral n

doubleToInt :: Double -> Int
doubleToInt d = floor d

-- String conversion
stringToInt :: String -> Int
stringToInt s = read s

intToString :: Int -> String
intToString n = show n

-- Demonstration function
demonstrateTypes :: IO ()
demonstrateTypes = do
    putStrLn "=== Haskell Variables and Types Demo ===\n"

    putStrLn "--- Basic Types ---"
    putStrLn $ "Age: " ++ show age ++ " (Int)"
    putStrLn $ "Price: " ++ show price ++ " (Double)"
    putStrLn $ "Name: " ++ name ++ " (String)"
    putStrLn $ "Is student: " ++ show isStudent ++ " (Bool)"
    putStrLn $ "Has graduated: " ++ show hasGraduated ++ " (Bool)"

    putStrLn "\n--- Type Inference ---"
    putStrLn $ "Inferred int: " ++ show inferredInt
    putStrLn $ "Inferred string: " ++ inferredString
    putStrLn $ "Inferred list: " ++ show inferredList

    putStrLn "\n--- Tuples ---"
    putStrLn $ "Person: " ++ show person
    putStrLn $ "Person with title: " ++ show personWithTitle

    -- Pattern matching on tuples
    let (pName, pAge) = person
    putStrLn $ "Extracted name: " ++ pName ++ ", age: " ++ show pAge

    putStrLn "\n--- Lists ---"
    putStrLn $ "Numbers: " ++ show numbers
    putStrLn $ "Names: " ++ show names
    putStrLn $ "First number: " ++ show (head numbers)
    putStrLn $ "Last number: " ++ show (last numbers)
    putStrLn $ "List length: " ++ show (length numbers)

    putStrLn "\n--- Maybe Type (Safe Nullability) ---"
    putStrLn $ "Find user 1: " ++ show (findUser 1)
    putStrLn $ "Find user 2: " ++ show (findUser 2)
    putStrLn $ "Find user 99: " ++ show (findUser 99)

    -- Pattern matching on Maybe
    case findUser 1 of
        Just userName -> putStrLn $ "Found user: " ++ userName
        Nothing -> putStrLn "User not found"

    putStrLn "\n--- Type Conversion ---"
    let intNum = 42
    let doubleNum = intToDouble intNum
    putStrLn $ "Int " ++ show intNum ++ " to Double: " ++ show doubleNum

    let doubleVal = 3.99
    let intVal = doubleToInt doubleVal
    putStrLn $ "Double " ++ show doubleVal ++ " to Int (floor): " ++ show intVal

    let strNum = "123"
    let convertedNum = stringToInt strNum
    putStrLn $ "String \"" ++ strNum ++ "\" to Int: " ++ show convertedNum

    let numToStr = intToString 456
    putStrLn $ "Int 456 to String: \"" ++ numToStr ++ "\""

    putStrLn "\n--- Immutability ---"
    putStrLn "All values in Haskell are immutable by default."
    putStrLn "You cannot reassign variables - they are constants!"

    -- Shadowing example
    let x = 10
    putStrLn $ "x = " ++ show x
    let x = 20  -- This shadows the previous x
    putStrLn $ "x (shadowed) = " ++ show x
    -- The original x is not changed, just hidden in this scope

    putStrLn "\n--- Polymorphic Functions ---"
    putStrLn $ "identity 42 = " ++ show (identity 42)
    putStrLn $ "identity \"hello\" = " ++ identity "hello"
    putStrLn $ "identity True = " ++ show (identity True)

    putStrLn "\n--- Strong Type System ---"
    putStrLn "Haskell has a strong, static type system with excellent inference."
    putStrLn "Type errors are caught at compile time, not runtime."
    putStrLn "This prevents many bugs before the program ever runs!"

-- List operations demonstration
listOperations :: IO ()
listOperations = do
    putStrLn "\n=== List Operations ==="
    let nums = [1, 2, 3, 4, 5]

    putStrLn $ "Original list: " ++ show nums
    putStrLn $ "Doubled: " ++ show (map (*2) nums)
    putStrLn $ "Filtered (> 3): " ++ show (filter (>3) nums)
    putStrLn $ "Sum: " ++ show (sum nums)
    putStrLn $ "Product: " ++ show (product nums)
    putStrLn $ "Cons (0 : list): " ++ show (0 : nums)
    putStrLn $ "Append (++ [6,7]): " ++ show (nums ++ [6, 7])

-- Main entry point
main :: IO ()
main = do
    demonstrateTypes
    listOperations

    putStrLn "\n--- Type Checking in GHCi ---"
    putStrLn "Try these commands in GHCi:"
    putStrLn "  :type age"
    putStrLn "  :type person"
    putStrLn "  :type findUser"
    putStrLn "  :type identity"
