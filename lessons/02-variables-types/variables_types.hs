-- variables_types.hs

-- Type signatures (optional but recommended)
age :: Int
age = 25

price :: Double
price = 19.99

name :: String
name = "Alice"

isStudent :: Bool
isStudent = True

-- Type inference - no signature needed
inferredInt = 42        -- Haskell infers: Num a => a
inferredString = "hi"   -- Inferred as String

-- All "variables" are actually constants (immutable)
-- age = 30  -- ERROR: cannot redefine

-- Polymorphic function
identity :: a -> a      -- works for any type 'a'
identity x = x

-- Tuples (fixed size, mixed types)
person :: (String, Int)
person = ("Alice", 25)

-- Lists (variable size, same type)
numbers :: [Int]
numbers = [1, 2, 3, 4, 5]

main :: IO ()
main = do
    putStrLn $ "Age: " ++ show age
    putStrLn $ "Name: " ++ name
    putStrLn $ "Is student: " ++ show isStudent
    putStrLn $ "Person: " ++ show person
    putStrLn $ "Numbers: " ++ show numbers
