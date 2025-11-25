-- Lesson 9: Pattern Matching - Haskell Examples
-- Pattern matching is fundamental to Haskell programming!

module Main where

-- =============================================================================
-- BASIC PATTERN MATCHING ON VALUES
-- =============================================================================

-- Match on literal values
describe :: Int -> String
describe 0 = "zero"
describe 1 = "one"
describe 2 = "two"
describe n = "some other number: " ++ show n

-- Match on boolean
boolToString :: Bool -> String
boolToString True  = "yes"
boolToString False = "no"

-- =============================================================================
-- PATTERN MATCHING ON LISTS
-- =============================================================================

-- Match list structure
describeList :: [a] -> String
describeList []        = "empty list"
describeList [_]       = "singleton list"
describeList [_, _]    = "two elements"
describeList (_:_:_:_) = "three or more elements"

-- Head and tail
headSafe :: [a] -> Maybe a
headSafe []    = Nothing
headSafe (x:_) = Just x

-- Sum using pattern matching
sumList :: Num a => [a] -> a
sumList []     = 0
sumList (x:xs) = x + sumList xs

-- Length using pattern matching
len :: [a] -> Int
len []     = 0
len (_:xs) = 1 + len xs

-- Take elements
takeN :: Int -> [a] -> [a]
takeN 0 _      = []          -- Take 0 from anything
takeN _ []     = []          -- Take from empty
takeN n (x:xs) = x : takeN (n-1) xs

-- Zip two lists
zipLists :: [a] -> [b] -> [(a, b)]
zipLists [] _          = []
zipLists _ []          = []
zipLists (x:xs) (y:ys) = (x, y) : zipLists xs ys

-- =============================================================================
-- PATTERN MATCHING ON TUPLES
-- =============================================================================

-- Destructure tuples
fst' :: (a, b) -> a
fst' (x, _) = x

snd' :: (a, b) -> b
snd' (_, y) = y

-- Work with pairs
addPair :: (Int, Int) -> Int
addPair (x, y) = x + y

-- Process 3-tuples
processTriple :: (String, Int, Bool) -> String
processTriple (name, age, active) =
    name ++ " is " ++ show age ++ " years old" ++
    if active then " (active)" else " (inactive)"

-- =============================================================================
-- PATTERN MATCHING ON ALGEBRAIC DATA TYPES
-- =============================================================================

-- Define a simple shape type
data Shape = Circle Double
           | Rectangle Double Double
           | Triangle Double Double Double
           deriving (Show, Eq)

-- Pattern match on shapes
area :: Shape -> Double
area (Circle r)        = pi * r * r
area (Rectangle w h)   = w * h
area (Triangle a b c)  = let s = (a + b + c) / 2
                         in sqrt (s * (s-a) * (s-b) * (s-c))

-- Describe a shape
describeShape :: Shape -> String
describeShape (Circle r)
    | r == 1    = "Unit circle"
    | otherwise = "Circle with radius " ++ show r
describeShape (Rectangle w h)
    | w == h    = "Square with side " ++ show w
    | otherwise = "Rectangle " ++ show w ++ " x " ++ show h
describeShape (Triangle _ _ _) = "Triangle"

-- =============================================================================
-- PATTERN MATCHING WITH MAYBE
-- =============================================================================

-- Safe division
safeDiv :: Double -> Double -> Maybe Double
safeDiv _ 0 = Nothing
safeDiv x y = Just (x / y)

-- Process Maybe values
processResult :: Maybe Int -> String
processResult Nothing  = "No result"
processResult (Just n) = "Result: " ++ show n

-- Chain Maybe operations
addMaybes :: Maybe Int -> Maybe Int -> Maybe Int
addMaybes (Just x) (Just y) = Just (x + y)
addMaybes _ _               = Nothing

-- =============================================================================
-- PATTERN MATCHING WITH EITHER
-- =============================================================================

data Error = DivByZero | NegativeInput | TooLarge
    deriving (Show, Eq)

safeSqrt :: Double -> Either Error Double
safeSqrt x
    | x < 0     = Left NegativeInput
    | x > 1e10  = Left TooLarge
    | otherwise = Right (sqrt x)

processEither :: Either Error Double -> String
processEither (Left DivByZero)     = "Error: Division by zero"
processEither (Left NegativeInput) = "Error: Negative input"
processEither (Left TooLarge)      = "Error: Input too large"
processEither (Right result)       = "Success: " ++ show result

-- =============================================================================
-- GUARDS WITH PATTERN MATCHING
-- =============================================================================

classify :: Int -> String
classify n
    | n < 0     = "negative"
    | n == 0    = "zero"
    | n < 10    = "small positive"
    | n < 100   = "medium positive"
    | otherwise = "large positive"

-- Guards with destructuring
describePoint :: (Double, Double) -> String
describePoint (x, y)
    | x == 0 && y == 0 = "origin"
    | x == 0           = "on y-axis"
    | y == 0           = "on x-axis"
    | x > 0 && y > 0   = "quadrant I"
    | x < 0 && y > 0   = "quadrant II"
    | x < 0 && y < 0   = "quadrant III"
    | otherwise        = "quadrant IV"

-- =============================================================================
-- AS-PATTERNS
-- =============================================================================

-- Keep reference to whole while destructuring
firstTwo :: [a] -> ([a], Maybe a, Maybe a)
firstTwo xs@(a:b:_) = (xs, Just a, Just b)
firstTwo xs@[a]     = (xs, Just a, Nothing)
firstTwo xs         = (xs, Nothing, Nothing)

-- Duplicate first element
dupFirst :: [a] -> [a]
dupFirst all@(x:_) = x : all
dupFirst []        = []

-- =============================================================================
-- PATTERN MATCHING IN WHERE/LET
-- =============================================================================

processPair :: (Int, Int) -> Int
processPair pair = x * y + x + y
  where
    (x, y) = pair  -- Pattern match in where clause

-- With let
quadratic :: Double -> Double -> Double -> Double -> [Double]
quadratic a b c x =
    let discriminant = b*b - 4*a*c
        root1 = (-b + sqrt discriminant) / (2*a)
        root2 = (-b - sqrt discriminant) / (2*a)
    in [root1, root2]

-- =============================================================================
-- RECURSIVE DATA STRUCTURE PATTERN MATCHING
-- =============================================================================

data Tree a = Empty
            | Node a (Tree a) (Tree a)
            deriving (Show, Eq)

-- Tree operations using pattern matching
treeSize :: Tree a -> Int
treeSize Empty        = 0
treeSize (Node _ l r) = 1 + treeSize l + treeSize r

treeHeight :: Tree a -> Int
treeHeight Empty        = 0
treeHeight (Node _ l r) = 1 + max (treeHeight l) (treeHeight r)

inOrder :: Tree a -> [a]
inOrder Empty        = []
inOrder (Node x l r) = inOrder l ++ [x] ++ inOrder r

-- =============================================================================
-- EXPRESSION EVALUATOR
-- =============================================================================

data Expr = Lit Double
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          deriving (Show, Eq)

eval :: Expr -> Maybe Double
eval (Lit n)     = Just n
eval (Add e1 e2) = (+) <$> eval e1 <*> eval e2
eval (Sub e1 e2) = (-) <$> eval e1 <*> eval e2
eval (Mul e1 e2) = (*) <$> eval e1 <*> eval e2
eval (Div e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    if v2 == 0 then Nothing else Just (v1 / v2)

-- =============================================================================
-- MAIN
-- =============================================================================

main :: IO ()
main = do
    putStrLn "=== Basic Pattern Matching ==="
    mapM_ (putStrLn . describe) [0, 1, 2, 42]

    putStrLn "\n=== List Pattern Matching ==="
    mapM_ (putStrLn . describeList) [[], [1], [1,2], [1,2,3,4,5::Int]]
    putStrLn $ "sumList [1..5] = " ++ show (sumList [1..5])

    putStrLn "\n=== Tuple Pattern Matching ==="
    putStrLn $ "fst' (1, 2) = " ++ show (fst' (1::Int, 2::Int))
    putStrLn $ "addPair (3, 4) = " ++ show (addPair (3, 4))
    putStrLn $ processTriple ("Alice", 30, True)

    putStrLn "\n=== Shape Pattern Matching ==="
    let shapes = [Circle 5, Rectangle 3 4, Rectangle 4 4, Triangle 3 4 5]
    mapM_ (\s -> putStrLn $ describeShape s ++ " - Area: " ++ show (area s)) shapes

    putStrLn "\n=== Maybe Pattern Matching ==="
    putStrLn $ processResult (Just 42)
    putStrLn $ processResult Nothing
    putStrLn $ "safeDiv 10 2 = " ++ show (safeDiv 10 2)
    putStrLn $ "safeDiv 10 0 = " ++ show (safeDiv 10 0)

    putStrLn "\n=== Either Pattern Matching ==="
    mapM_ (putStrLn . processEither . safeSqrt) [4, -4, 1e12]

    putStrLn "\n=== Guards ==="
    putStrLn $ describePoint (0, 0)
    putStrLn $ describePoint (3, 4)
    putStrLn $ describePoint (-1, 2)

    putStrLn "\n=== Tree Pattern Matching ==="
    let tree = Node 5 (Node 3 (Node 1 Empty Empty) (Node 4 Empty Empty))
                      (Node 7 (Node 6 Empty Empty) (Node 8 Empty Empty))
    putStrLn $ "Tree size: " ++ show (treeSize tree)
    putStrLn $ "Tree height: " ++ show (treeHeight tree)
    putStrLn $ "In-order: " ++ show (inOrder tree)

    putStrLn "\n=== Expression Evaluation ==="
    let expr = Mul (Add (Lit 3) (Lit 4)) (Lit 2)  -- (3 + 4) * 2
    putStrLn $ "(3 + 4) * 2 = " ++ show (eval expr)
