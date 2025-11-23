-- Lesson 9: Pattern Matching in Haskell
--
-- Haskell has the most powerful pattern matching of any language!
-- Pattern matching is a core language feature used everywhere.

-- ====================
-- 1. Function Clauses (Pattern Matching in Function Definitions)
-- ====================

describeNumber :: Int -> String
describeNumber 0 = "zero"
describeNumber 1 = "one"
describeNumber 2 = "two"
describeNumber n = "many: " ++ show n

-- ====================
-- 2. Tuple Patterns
-- ====================

describePoint :: (Int, Int) -> String
describePoint (0, 0) = "origin"
describePoint (0, y) = "on y-axis at y=" ++ show y
describePoint (x, 0) = "on x-axis at x=" ++ show x
describePoint (x, y) = "point at (" ++ show x ++ ", " ++ show y ++ ")"

-- ====================
-- 3. List Patterns
-- ====================

describeList :: [Int] -> String
describeList [] = "empty list"
describeList [x] = "single element: " ++ show x
describeList [x, y] = "two elements: " ++ show x ++ ", " ++ show y
describeList (x:xs) = "first: " ++ show x ++ ", rest: " ++ show xs

-- List operations with pattern matching
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

-- ====================
-- 4. Algebraic Data Types
-- ====================

data Shape = Circle Double
           | Rectangle Double Double
           | Triangle Double Double Double

area :: Shape -> Double
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h
area (Triangle a b c) = 
    let s = (a + b + c) / 2
    in sqrt (s * (s - a) * (s - b) * (s - c))

describeShape :: Shape -> String
describeShape (Circle r) = "Circle with radius " ++ show r
describeShape (Rectangle w h) = "Rectangle " ++ show w ++ "x" ++ show h
describeShape (Triangle a b c) = "Triangle with sides " ++ show [a, b, c]

-- ====================
-- 5. Maybe and Either Pattern Matching
-- ====================

describeMaybe :: Maybe Int -> String
describeMaybe Nothing = "nothing"
describeMaybe (Just x) = "just " ++ show x

describeEither :: Either String Int -> String
describeEither (Left err) = "error: " ++ err
describeEither (Right val) = "value: " ++ show val

-- ====================
-- 6. Guards (Conditional Patterns)
-- ====================

classify :: Int -> String
classify n
    | n < 0     = "negative"
    | n == 0    = "zero"
    | n < 10    = "small positive"
    | n < 100   = "medium positive"
    | otherwise = "large positive"

-- ====================
-- 7. As-Patterns (@)
-- ====================

describeAll :: [a] -> ([a], Int)
describeAll xs@(_:_) = (xs, length xs)  -- xs@ captures the whole list
describeAll [] = ([], 0)

-- ====================
-- 8. Nested Patterns
-- ====================

data Point = Point Int Int deriving (Show)
data ComplexShape = Circ Point Double
                  | Rect Point Double Double

describeComplex :: ComplexShape -> String
describeComplex (Circ (Point 0 0) r) = "Circle at origin, radius " ++ show r
describeComplex (Circ (Point x y) r) = 
    "Circle at (" ++ show x ++ "," ++ show y ++ "), radius " ++ show r
describeComplex (Rect (Point x y) w h) = 
    "Rectangle at (" ++ show x ++ "," ++ show y ++ "), " ++ show w ++ "x" ++ show h

-- ====================
-- 9. Case Expressions
-- ====================

factorial :: Int -> Int
factorial n = case n of
    0 -> 1
    n -> n * factorial (n - 1)

listSum :: [Int] -> Int
listSum xs = case xs of
    [] -> 0
    (y:ys) -> y + listSum ys

-- ====================
-- 10. Expression Evaluator
-- ====================

data Expr = Num Int
          | Add Expr Expr
          | Mul Expr Expr
          | Neg Expr
          deriving (Show)

eval :: Expr -> Int
eval (Num n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Neg e) = negate (eval e)

-- ====================
-- 11. Tree Patterns
-- ====================

data Tree a = Empty
            | Node a (Tree a) (Tree a)
            deriving (Show)

treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node _ left right) = 1 + max (treeHeight left) (treeHeight right)

treeContains :: Eq a => a -> Tree a -> Bool
treeContains _ Empty = False
treeContains x (Node val left right)
    | x == val  = True
    | otherwise = treeContains x left || treeContains x right

-- ====================
-- Main Demonstration
-- ====================

main :: IO ()
main = do
    putStrLn "=== Pattern Matching in Haskell ===\n"

    -- 1. Function clauses
    putStrLn "1. Function Clause Patterns:"
    mapM_ (\n -> putStrLn $ "   " ++ show n ++ " -> " ++ describeNumber n) [0, 1, 2, 5]

    -- 2. Tuple patterns
    putStrLn "\n2. Tuple Patterns:"
    let points = [(0, 0), (0, 5), (3, 0), (2, 3)]
    mapM_ (\p -> putStrLn $ "   " ++ show p ++ " -> " ++ describePoint p) points

    -- 3. List patterns
    putStrLn "\n3. List Patterns:"
    let lists = [[], [1], [1, 2], [1, 2, 3, 4]]
    mapM_ (\l -> putStrLn $ "   " ++ show l ++ " -> " ++ describeList l) lists

    -- 4. Algebraic data types
    putStrLn "\n4. Algebraic Data Types:"
    let shapes = [Circle 5, Rectangle 4 6, Triangle 3 4 5]
    mapM_ (\s -> putStrLn $ "   " ++ describeShape s ++ ", area = " ++ show (area s)) shapes

    -- 5. Maybe/Either
    putStrLn "\n5. Maybe and Either:"
    putStrLn $ "   " ++ describeMaybe Nothing
    putStrLn $ "   " ++ describeMaybe (Just 42)
    putStrLn $ "   " ++ describeEither (Left "failed")
    putStrLn $ "   " ++ describeEither (Right 100)

    -- 6. Guards
    putStrLn "\n6. Guards:"
    mapM_ (\n -> putStrLn $ "   " ++ show n ++ " -> " ++ classify n) [-5, 0, 3, 50, 500]

    -- 7. Expression evaluator
    putStrLn "\n7. Expression Evaluator:"
    let expr1 = Mul (Add (Num 2) (Num 3)) (Num 4)  -- (2 + 3) * 4
    putStrLn $ "   (2 + 3) * 4 = " ++ show (eval expr1)
    let expr2 = Neg (Add (Num 5) (Num 3))  -- -(5 + 3)
    putStrLn $ "   -(5 + 3) = " ++ show (eval expr2)

    -- 8. Tree operations
    putStrLn "\n8. Tree Operations:"
    let tree = Node 5 (Node 3 Empty Empty) (Node 7 Empty (Node 9 Empty Empty))
    putStrLn $ "   Tree height: " ++ show (treeHeight tree)
    putStrLn $ "   Contains 7? " ++ show (treeContains 7 tree)
    putStrLn $ "   Contains 4? " ++ show (treeContains 4 tree)
