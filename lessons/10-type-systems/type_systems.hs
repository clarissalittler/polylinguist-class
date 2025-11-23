-- Lesson 10: Type Systems in Haskell
--
-- Haskell has one of the most advanced type systems:
-- - Strong static typing
-- - Full type inference (Hindley-Milner)
-- - Algebraic data types
-- - Parametric polymorphism (generics)
-- - Type classes (constrained polymorphism)
-- - Higher-kinded types
-- - Phantom types
--
-- This demonstrates Haskell's type system features.

-- ====================
-- 1. Basic Types
-- ====================

-- Explicit type signatures
increment :: Int -> Int
increment x = x + 1

add :: Int -> Int -> Int
add x y = x + y

-- Type inference works without signatures
double x = x * 2  -- Inferred: Num a => a -> a

-- ====================
-- 2. Algebraic Data Types
-- ====================

-- Sum type (OR)
data Shape = Circle Double
           | Rectangle Double Double
           | Triangle Double Double Double
           deriving (Show, Eq)

area :: Shape -> Double
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h
area (Triangle a b c) =
    let s = (a + b + c) / 2
    in sqrt (s * (s - a) * (s - b) * (s - c))

-- Product type (AND)
data Point = Point { x :: Double, y :: Double }
           deriving (Show, Eq)

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) =
    sqrt ((x2 - x1)^2 + (y2 - y1)^2)

-- ====================
-- 3. Parametric Polymorphism (Generics)
-- ====================

-- Generic identity function
identity :: a -> a
identity x = x

-- Generic pair
data Pair a b = Pair a b deriving (Show, Eq)

first :: Pair a b -> a
first (Pair x _) = x

second :: Pair a b -> b
second (Pair _ y) = y

swap :: Pair a b -> Pair b a
swap (Pair x y) = Pair y x

-- ====================
-- 4. Maybe Type (Option)
-- ====================

-- Built-in: data Maybe a = Nothing | Just a

safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- Map over Maybe
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe _ Nothing = Nothing
mapMaybe f (Just x) = Just (f x)

-- ====================
-- 5. Either Type (Result)
-- ====================

-- Built-in: data Either a b = Left a | Right b

divide :: Double -> Double -> Either String Double
divide _ 0 = Left "Division by zero"
divide x y = Right (x / y)

parseInt :: String -> Either String Int
parseInt s =
    case reads s of
        [(n, "")] -> Right n
        _ -> Left ("Not a valid integer: " ++ s)

-- ====================
-- 6. List Types
-- ====================

-- Explicit type for list functions
sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

-- Polymorphic list functions
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter p (x:xs)
    | p x       = x : myFilter p xs
    | otherwise = myFilter p xs

-- ====================
-- 7. Type Classes
-- ====================

-- Define a type class
class Describable a where
    describe :: a -> String

-- Implement for Shape
instance Describable Shape where
    describe (Circle r) = "Circle with radius " ++ show r
    describe (Rectangle w h) = "Rectangle " ++ show w ++ "x" ++ show h
    describe (Triangle a b c) = "Triangle with sides " ++ show [a, b, c]

-- Implement for Point
instance Describable Point where
    describe (Point px py) = "Point at (" ++ show px ++ ", " ++ show py ++ ")"

-- Function using type class constraint
printDescription :: Describable a => a -> IO ()
printDescription x = putStrLn $ describe x

-- ====================
-- 8. Multiple Type Class Constraints
-- ====================

-- Requires both Show and Eq
printIfEqual :: (Show a, Eq a) => a -> a -> String
printIfEqual x y
    | x == y    = "Equal: " ++ show x
    | otherwise = "Not equal: " ++ show x ++ " /= " ++ show y

-- ====================
-- 9. Custom Type Classes with Methods
-- ====================

class Measurable a where
    measure :: a -> Double

instance Measurable Shape where
    measure = area

instance Measurable Point where
    measure (Point px py) = sqrt (px^2 + py^2)  -- Distance from origin

-- Compare two measurables
compareMeasure :: Measurable a => a -> a -> Ordering
compareMeasure x y = compare (measure x) (measure y)

-- ====================
-- 10. Functor Type Class
-- ====================

-- Functor is built-in, but we can define our own types that are functors
data Tree a = Empty | Node a (Tree a) (Tree a)
            deriving (Show, Eq)

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Node x left right) =
        Node (f x) (fmap f left) (fmap f right)

-- Now we can map over trees
incrementTree :: Tree Int -> Tree Int
incrementTree = fmap (+1)

-- ====================
-- 11. Applicative and Monad
-- ====================

-- Maybe is a Monad, allowing chaining of computations
safeDivideChain :: Double -> Double -> Double -> Maybe Double
safeDivideChain x y z = do
    result1 <- safeDivide x y
    result2 <- safeDivide result1 z
    return result2

-- Equivalent without do-notation
safeDivideChain' :: Double -> Double -> Double -> Maybe Double
safeDivideChain' x y z =
    safeDivide x y >>= \result1 ->
    safeDivide result1 z

-- ====================
-- 12. Type Synonyms
-- ====================

type Name = String
type Age = Int
type Person = (Name, Age)

createPerson :: Name -> Age -> Person
createPerson = (,)

personName :: Person -> Name
personName = fst

personAge :: Person -> Age
personAge = snd

-- ====================
-- 13. Newtype (Zero-Cost Wrapper)
-- ====================

-- Creates a distinct type with no runtime overhead
newtype UserId = UserId Int deriving (Show, Eq)
newtype ProductId = ProductId Int deriving (Show, Eq)

-- Can't accidentally mix up user IDs and product IDs
getUserName :: UserId -> String
getUserName (UserId id) = "User #" ++ show id

-- This would be a type error:
-- getUserName (ProductId 123)  -- ERROR!

-- ====================
-- 14. Phantom Types
-- ====================

-- Type parameter doesn't appear in data definition
data State s a = State a deriving Show

data Locked
data Unlocked

-- Can only unlock locked doors
unlock :: State Locked a -> State Unlocked a
unlock (State x) = State x

-- Can only lock unlocked doors
lock :: State Unlocked a -> State Locked a
lock (State x) = State x

-- Type system prevents incorrect transitions:
-- unlock (State 5 :: State Unlocked Int)  -- TYPE ERROR!

-- ====================
-- 15. Existential Types
-- ====================

-- Hide the concrete type
data ShowBox = forall a. Show a => ShowBox a

instance Show ShowBox where
    show (ShowBox x) = "ShowBox(" ++ show x ++ ")"

-- Can put any showable value in a ShowBox
boxes :: [ShowBox]
boxes = [ShowBox 42, ShowBox "hello", ShowBox (3.14 :: Double)]

-- ====================
-- 16. Kind System (Types of Types)
-- ====================

-- * is the kind of types that have values
-- Int :: *
-- Bool :: *

-- * -> * is the kind of type constructors
-- Maybe :: * -> *
-- [] :: * -> *

-- Higher-kinded types
class MyFunctor f where
    myFmap :: (a -> b) -> f a -> f b

-- f has kind * -> *
instance MyFunctor Maybe where
    myFmap _ Nothing = Nothing
    myFmap g (Just x) = Just (g x)

instance MyFunctor [] where
    myFmap = map

-- ====================
-- 17. GADTs (Generalized ADTs)
-- ====================

-- Allows more precise types
{-# LANGUAGE GADTs #-}

data Expr a where
    LitInt  :: Int -> Expr Int
    LitBool :: Bool -> Expr Bool
    Add     :: Expr Int -> Expr Int -> Expr Int
    If      :: Expr Bool -> Expr a -> Expr a -> Expr a

eval :: Expr a -> a
eval (LitInt n) = n
eval (LitBool b) = b
eval (Add e1 e2) = eval e1 + eval e2
eval (If cond t e) = if eval cond then eval t else eval e

-- This is type-safe! Can't add booleans or if with int condition

-- ====================
-- 18. Type Families
-- ====================

{-# LANGUAGE TypeFamilies #-}

class Collection c where
    type Elem c
    empty :: c
    insert :: Elem c -> c -> c
    toList :: c -> [Elem c]

instance Collection [a] where
    type Elem [a] = a
    empty = []
    insert = (:)
    toList = id

-- ====================
-- 19. Constraint Kinds
-- ====================

-- Constraints are first-class
type Showable a = (Show a, Eq a)

showIfEqual :: Showable a => a -> a -> String
showIfEqual = printIfEqual

-- ====================
-- Main Demonstration
-- ====================

main :: IO ()
main = do
    putStrLn "=== Type Systems in Haskell ===\n"

    -- 1. Basic types
    putStrLn "1. Basic Types:"
    putStrLn $ "   increment 5 = " ++ show (increment 5)
    putStrLn $ "   add 3 7 = " ++ show (add 3 7)

    -- 2. Algebraic data types
    putStrLn "\n2. Algebraic Data Types:"
    let shapes = [Circle 5, Rectangle 4 6, Triangle 3 4 5]
    mapM_ (\s -> putStrLn $ "   " ++ show s ++ ", area = " ++ show (area s)) shapes

    -- 3. Polymorphism
    putStrLn "\n3. Parametric Polymorphism:"
    putStrLn $ "   identity 42 = " ++ show (identity 42)
    putStrLn $ "   identity \"hello\" = " ++ show (identity "hello")
    let p = Pair 1 "one"
    putStrLn $ "   first " ++ show p ++ " = " ++ show (first p)
    putStrLn $ "   swap " ++ show p ++ " = " ++ show (swap p)

    -- 4. Maybe type
    putStrLn "\n4. Maybe Type (Option):"
    putStrLn $ "   safeDivide 10 2 = " ++ show (safeDivide 10 2)
    putStrLn $ "   safeDivide 10 0 = " ++ show (safeDivide 10 0)
    putStrLn $ "   safeHead [1,2,3] = " ++ show (safeHead [1,2,3])
    putStrLn $ "   safeHead [] = " ++ show (safeHead ([] :: [Int]))

    -- 5. Either type
    putStrLn "\n5. Either Type (Result):"
    putStrLn $ "   divide 10 2 = " ++ show (divide 10 2)
    putStrLn $ "   divide 10 0 = " ++ show (divide 10 0)
    putStrLn $ "   parseInt \"42\" = " ++ show (parseInt "42")
    putStrLn $ "   parseInt \"abc\" = " ++ show (parseInt "abc")

    -- 6. Type classes
    putStrLn "\n6. Type Classes:"
    printDescription (Circle 5)
    printDescription (Point 3 4)

    -- 7. Measurable type class
    putStrLn "\n7. Custom Type Classes:"
    putStrLn $ "   measure (Circle 5) = " ++ show (measure (Circle 5))
    putStrLn $ "   measure (Point 3 4) = " ++ show (measure (Point 3 4))

    -- 8. Functor
    putStrLn "\n8. Functor (Mapping):"
    let tree = Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty)
    putStrLn $ "   Original tree: " ++ show tree
    putStrLn $ "   Incremented tree: " ++ show (incrementTree tree)

    -- 9. Monad chaining
    putStrLn "\n9. Monad Chaining:"
    putStrLn $ "   (100 / 2) / 5 = " ++ show (safeDivideChain 100 2 5)
    putStrLn $ "   (100 / 0) / 5 = " ++ show (safeDivideChain 100 0 5)

    -- 10. Newtype
    putStrLn "\n10. Newtype (Distinct Types):"
    let userId = UserId 123
    putStrLn $ "   getUserName " ++ show userId ++ " = " ++ getUserName userId

    -- 11. Phantom types
    putStrLn "\n11. Phantom Types (State Tracking):"
    let locked = State "door" :: State Locked String
    let unlocked = unlock locked
    let relocked = lock unlocked
    putStrLn $ "   Locked: " ++ show locked
    putStrLn $ "   Unlocked: " ++ show unlocked
    putStrLn $ "   Re-locked: " ++ show relocked

    -- 12. Existential types
    putStrLn "\n12. Existential Types:"
    mapM_ (\b -> putStrLn $ "   " ++ show b) boxes

    -- 13. GADTs
    putStrLn "\n13. GADTs (Type-Safe Expressions):"
    let expr1 = Add (LitInt 2) (LitInt 3)
    putStrLn $ "   eval (2 + 3) = " ++ show (eval expr1)
    let expr2 = If (LitBool True) (LitInt 42) (LitInt 0)
    putStrLn $ "   eval (if true then 42 else 0) = " ++ show (eval expr2)

    putStrLn "\n=== Haskell Type System Features ==="
    putStrLn "- Strong static typing with full inference"
    putStrLn "- Algebraic data types (sum and product)"
    putStrLn "- Parametric polymorphism (generics)"
    putStrLn "- Type classes (constrained polymorphism)"
    putStrLn "- Higher-kinded types"
    putStrLn "- Phantom types for compile-time tracking"
    putStrLn "- GADTs for more precise typing"
    putStrLn "- Type families and associated types"
    putStrLn "- One of the most advanced type systems!"
