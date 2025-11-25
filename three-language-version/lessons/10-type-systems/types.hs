-- Lesson 10: Type Systems - Haskell Examples
-- Haskell has one of the most powerful type systems in mainstream use

module Main where

import Data.Maybe (fromMaybe)

-- =============================================================================
-- BASIC TYPE ANNOTATIONS
-- =============================================================================

-- Explicit type signatures (optional but recommended)
add :: Int -> Int -> Int
add x y = x + y

-- Polymorphic type (works on any type)
identity :: a -> a
identity x = x

-- Constrained polymorphism
maximum' :: Ord a => [a] -> a
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

-- =============================================================================
-- TYPE INFERENCE
-- =============================================================================

-- Haskell infers types, but we can be explicit
inferredExample = 42 + 3.14  -- Inferred as Double

-- Type inference with functions
double x = x * 2  -- Inferred: Num a => a -> a

-- =============================================================================
-- ALGEBRAIC DATA TYPES
-- =============================================================================

-- Sum type (like enum or variant)
data Color = Red | Green | Blue
    deriving (Show, Eq)

-- Product type (like struct)
data Point = Point Double Double
    deriving (Show, Eq)

-- Combination of sum and product
data Shape = Circle Point Double      -- center and radius
           | Rectangle Point Point    -- two corners
           | Triangle Point Point Point
    deriving (Show, Eq)

area :: Shape -> Double
area (Circle _ r) = pi * r * r
area (Rectangle (Point x1 y1) (Point x2 y2)) =
    abs (x2 - x1) * abs (y2 - y1)
area (Triangle (Point x1 y1) (Point x2 y2) (Point x3 y3)) =
    abs ((x2-x1)*(y3-y1) - (x3-x1)*(y2-y1)) / 2

-- =============================================================================
-- MAYBE TYPE (Optional values)
-- =============================================================================

safeDiv :: Double -> Double -> Maybe Double
safeDiv _ 0 = Nothing
safeDiv x y = Just (x / y)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- Using Maybe values
processResult :: Maybe Int -> Int
processResult Nothing = 0
processResult (Just n) = n * 2

-- =============================================================================
-- EITHER TYPE (Error handling)
-- =============================================================================

data DivError = DivByZero | NegativeNumber
    deriving (Show, Eq)

safeSqrt :: Double -> Either DivError Double
safeSqrt x
    | x < 0     = Left NegativeNumber
    | otherwise = Right (sqrt x)

safeDiv' :: Double -> Double -> Either DivError Double
safeDiv' _ 0 = Left DivByZero
safeDiv' x y = Right (x / y)

-- Chaining Either computations
compute :: Double -> Double -> Either DivError Double
compute x y = do
    result <- safeDiv' x y
    safeSqrt result

-- =============================================================================
-- PARAMETRIC POLYMORPHISM
-- =============================================================================

-- Generic list functions
len :: [a] -> Int
len [] = 0
len (_:xs) = 1 + len xs

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

-- =============================================================================
-- TYPE CLASSES (Ad-hoc polymorphism)
-- =============================================================================

-- Define a type class
class Printable a where
    toString :: a -> String

-- Instance for Int
instance Printable Int where
    toString = show

-- Instance for Bool
instance Printable Bool where
    toString True = "yes"
    toString False = "no"

-- Instance for our Color type
instance Printable Color where
    toString Red = "red"
    toString Green = "green"
    toString Blue = "blue"

-- Function using type class
printAll :: Printable a => [a] -> String
printAll xs = "[" ++ intercalate ", " (map toString xs) ++ "]"
  where intercalate _ [] = ""
        intercalate sep (x:xs) = x ++ concatMap (sep ++) xs

-- =============================================================================
-- COMMON TYPE CLASSES
-- =============================================================================

-- Eq: equality comparison
-- Ord: ordering
-- Show: convert to string
-- Read: parse from string
-- Num: numeric operations
-- Functor: things that can be mapped over
-- Monad: things that support >>= and return

-- Deriving instances automatically
data Person = Person
    { name :: String
    , age :: Int
    } deriving (Show, Eq, Ord)

-- =============================================================================
-- NEWTYPE (Wrapper types)
-- =============================================================================

-- Create distinct types from existing ones
newtype UserId = UserId Int deriving (Show, Eq)
newtype ProductId = ProductId Int deriving (Show, Eq)

-- Now you can't accidentally mix them!
getUser :: UserId -> String
getUser (UserId uid) = "User #" ++ show uid

-- getUser (ProductId 1)  -- ERROR: type mismatch!

-- =============================================================================
-- GENERIC CONTAINERS
-- =============================================================================

-- Binary tree parameterized by value type
data Tree a = Empty
            | Node a (Tree a) (Tree a)
            deriving (Show, Eq)

-- Functor instance for Tree
instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

-- Tree operations
insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x (Node y left right)
    | x < y     = Node y (insert x left) right
    | otherwise = Node y left (insert x right)

-- =============================================================================
-- HIGHER-KINDED TYPES
-- =============================================================================

-- Functions that work on any Functor
doubleAll :: Functor f => f Int -> f Int
doubleAll = fmap (*2)

-- Works on lists, Maybe, trees, etc.!
-- doubleAll [1,2,3] = [2,4,6]
-- doubleAll (Just 5) = Just 10
-- doubleAll (Node 1 Empty Empty) = Node 2 Empty Empty

-- =============================================================================
-- TYPE SYNONYMS VS NEWTYPES VS DATA
-- =============================================================================

-- Type synonym: just an alias, no type safety
type Name = String

-- Newtype: new type, same runtime representation
newtype Email = Email String deriving (Show, Eq)

-- Data: new type with potentially different representation
data UserInfo = UserInfo
    { userName :: Name
    , userEmail :: Email
    } deriving (Show)

-- =============================================================================
-- MAIN
-- =============================================================================

main :: IO ()
main = do
    putStrLn "=== Basic Types ==="
    putStrLn $ "add 3 4 = " ++ show (add 3 4)
    putStrLn $ "identity \"hello\" = " ++ identity "hello"

    putStrLn "\n=== Algebraic Data Types ==="
    let shapes = [Circle (Point 0 0) 5, Rectangle (Point 0 0) (Point 3 4)]
    mapM_ (\s -> putStrLn $ show s ++ " area = " ++ show (area s)) shapes

    putStrLn "\n=== Maybe ==="
    putStrLn $ "safeDiv 10 2 = " ++ show (safeDiv 10 2)
    putStrLn $ "safeDiv 10 0 = " ++ show (safeDiv 10 0)
    putStrLn $ "safeHead [1,2,3] = " ++ show (safeHead [1,2,3::Int])
    putStrLn $ "safeHead [] = " ++ show (safeHead ([] :: [Int]))

    putStrLn "\n=== Either ==="
    putStrLn $ "safeSqrt 4 = " ++ show (safeSqrt 4)
    putStrLn $ "safeSqrt (-4) = " ++ show (safeSqrt (-4))
    putStrLn $ "compute 8 2 = " ++ show (compute 8 2)

    putStrLn "\n=== Type Classes ==="
    putStrLn $ "toString (42::Int) = " ++ toString (42::Int)
    putStrLn $ "toString Red = " ++ toString Red

    putStrLn "\n=== Newtype ==="
    putStrLn $ getUser (UserId 42)

    putStrLn "\n=== Generic Containers ==="
    let tree = foldr insert Empty [5,3,7,1,9::Int]
    putStrLn $ "tree = " ++ show tree
    putStrLn $ "fmap (*2) tree = " ++ show (fmap (*2) tree)

    putStrLn "\n=== Higher-Kinded Types ==="
    putStrLn $ "doubleAll [1,2,3] = " ++ show (doubleAll [1,2,3])
    putStrLn $ "doubleAll (Just 5) = " ++ show (doubleAll (Just 5))
