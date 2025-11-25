-- Lesson 14: Stacks, Queues, and Lists - Haskell Examples
-- Implementing and using fundamental linear data structures

module Main where

import Data.List (foldl')
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>), (<|), ViewL(..), ViewR(..))

-- =============================================================================
-- STACK - Last In, First Out (LIFO)
-- =============================================================================

-- In Haskell, a list IS a stack! Head operations are O(1)
-- Stack operations: push = (:), pop = tail, peek = head

type Stack a = [a]

-- Stack operations using list
push :: a -> Stack a -> Stack a
push x stack = x : stack

pop :: Stack a -> Stack a
pop [] = error "Stack is empty"
pop (_:xs) = xs

peek :: Stack a -> a
peek [] = error "Stack is empty"
peek (x:_) = x

isEmpty :: Stack a -> Bool
isEmpty = null

-- Safe versions using Maybe
safePop :: Stack a -> Maybe (a, Stack a)
safePop [] = Nothing
safePop (x:xs) = Just (x, xs)

safePeek :: Stack a -> Maybe a
safePeek [] = Nothing
safePeek (x:_) = Just x

stackDemo :: IO ()
stackDemo = do
    putStrLn "=== Stack (LIFO) ==="
    let stack = foldr push [] [5, 4, 3, 2, 1]  -- Push 1-5
    putStrLn $ "Stack: " ++ show stack
    putStrLn $ "Peek: " ++ show (peek stack)
    putStrLn $ "After pop: " ++ show (pop stack)
    putStrLn $ "After 2 pops: " ++ show (pop (pop stack))

-- Stack Application: Balanced Parentheses
isBalanced :: String -> Bool
isBalanced = go []
  where
    go stack [] = null stack
    go stack (c:cs)
        | c `elem` "([{" = go (c:stack) cs
        | c `elem` ")]}" = case stack of
            [] -> False
            (top:rest) -> matches top c && go rest cs
        | otherwise = go stack cs

    matches '(' ')' = True
    matches '[' ']' = True
    matches '{' '}' = True
    matches _ _     = False

balancedDemo :: IO ()
balancedDemo = do
    putStrLn "\n=== Balanced Parentheses ==="
    let tests = ["(())", "([{}])", "(()", "([)]", "{[()]}"]
    mapM_ (\s -> putStrLn $ "'" ++ s ++ "' is " ++ show (isBalanced s)) tests

-- Reverse using stack
reverseWithStack :: [a] -> [a]
reverseWithStack = foldl' (flip (:)) []

-- =============================================================================
-- QUEUE - First In, First Out (FIFO)
-- =============================================================================

-- Simple queue using two lists (amortized O(1) operations)
data Queue a = Queue [a] [a] deriving Show

emptyQueue :: Queue a
emptyQueue = Queue [] []

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue front back) = Queue front (x:back)

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue [] []) = Nothing
dequeue (Queue [] back) = dequeue (Queue (reverse back) [])
dequeue (Queue (x:xs) back) = Just (x, Queue xs back)

front :: Queue a -> Maybe a
front q = fst <$> dequeue q

queueIsEmpty :: Queue a -> Bool
queueIsEmpty (Queue [] []) = True
queueIsEmpty _ = False

queueDemo :: IO ()
queueDemo = do
    putStrLn "\n=== Queue (FIFO) ==="
    let q1 = foldr (flip enqueue) emptyQueue ["Alice", "Bob", "Charlie", "Diana"]
    putStrLn $ "Queue: " ++ show q1
    case dequeue q1 of
        Nothing -> putStrLn "Empty queue"
        Just (x, q2) -> do
            putStrLn $ "Dequeue: " ++ x
            case dequeue q2 of
                Nothing -> putStrLn "Empty queue"
                Just (y, q3) -> do
                    putStrLn $ "Dequeue: " ++ y
                    putStrLn $ "Remaining: " ++ show q3

-- =============================================================================
-- DATA.SEQUENCE - Efficient Double-Ended Queue
-- =============================================================================

sequenceDemo :: IO ()
sequenceDemo = do
    putStrLn "\n=== Data.Sequence (Efficient Deque) ==="
    let seq1 = Seq.fromList [1, 2, 3, 4, 5]
    putStrLn $ "Sequence: " ++ show seq1

    -- Add to front and back - O(1)
    let seq2 = 0 <| seq1 |> 6
    putStrLn $ "After 0 <| ... |> 6: " ++ show seq2

    -- View from left
    case Seq.viewl seq2 of
        EmptyL -> putStrLn "Empty"
        x :< xs -> putStrLn $ "Head: " ++ show x ++ ", Tail: " ++ show xs

    -- View from right
    case Seq.viewr seq2 of
        EmptyR -> putStrLn "Empty"
        xs :> x -> putStrLn $ "Init: " ++ show xs ++ ", Last: " ++ show x

    -- Index access - O(log n)
    putStrLn $ "Element at index 3: " ++ show (Seq.index seq2 3)

-- =============================================================================
-- LINKED LIST IMPLEMENTATION
-- =============================================================================

data LinkedList a = Nil | Cons a (LinkedList a) deriving (Eq)

instance Show a => Show (LinkedList a) where
    show Nil = "Nil"
    show (Cons x xs) = show x ++ " -> " ++ show xs

-- List operations
llPrepend :: a -> LinkedList a -> LinkedList a
llPrepend = Cons

llAppend :: a -> LinkedList a -> LinkedList a
llAppend x Nil = Cons x Nil
llAppend x (Cons y ys) = Cons y (llAppend x ys)

llHead :: LinkedList a -> Maybe a
llHead Nil = Nothing
llHead (Cons x _) = Just x

llTail :: LinkedList a -> LinkedList a
llTail Nil = Nil
llTail (Cons _ xs) = xs

llLength :: LinkedList a -> Int
llLength Nil = 0
llLength (Cons _ xs) = 1 + llLength xs

llContains :: Eq a => a -> LinkedList a -> Bool
llContains _ Nil = False
llContains x (Cons y ys)
    | x == y    = True
    | otherwise = llContains x ys

llRemove :: Eq a => a -> LinkedList a -> LinkedList a
llRemove _ Nil = Nil
llRemove x (Cons y ys)
    | x == y    = ys
    | otherwise = Cons y (llRemove x ys)

-- Convert from standard list
fromList :: [a] -> LinkedList a
fromList = foldr Cons Nil

-- Convert to standard list
toList :: LinkedList a -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs

linkedListDemo :: IO ()
linkedListDemo = do
    putStrLn "\n=== Linked List ==="
    let list = fromList [1, 2, 3, 4, 5]
    putStrLn $ "List: " ++ show list
    putStrLn $ "Head: " ++ show (llHead list)
    putStrLn $ "Length: " ++ show (llLength list)
    putStrLn $ "Contains 3: " ++ show (llContains 3 list)
    putStrLn $ "After remove 3: " ++ show (llRemove 3 list)

-- =============================================================================
-- DIFFERENCE LIST (Efficient append)
-- =============================================================================

-- DList wraps a function for O(1) append
newtype DList a = DList { unDList :: [a] -> [a] }

dlistEmpty :: DList a
dlistEmpty = DList id

dlistSingleton :: a -> DList a
dlistSingleton x = DList (x:)

dlistAppend :: DList a -> DList a -> DList a
dlistAppend (DList f) (DList g) = DList (f . g)

dlistToList :: DList a -> [a]
dlistToList (DList f) = f []

dlistDemo :: IO ()
dlistDemo = do
    putStrLn "\n=== Difference List ==="
    let dl1 = dlistSingleton 1 `dlistAppend` dlistSingleton 2
    let dl2 = dlistSingleton 3 `dlistAppend` dlistSingleton 4
    let dl3 = dl1 `dlistAppend` dl2
    putStrLn $ "DList to list: " ++ show (dlistToList dl3)
    putStrLn "Note: Appending DLists is O(1)!"

-- =============================================================================
-- ZIPPER (Efficient traversal with modification)
-- =============================================================================

data ListZipper a = LZ [a] a [a] deriving Show

fromListZ :: [a] -> Maybe (ListZipper a)
fromListZ [] = Nothing
fromListZ (x:xs) = Just (LZ [] x xs)

goLeft :: ListZipper a -> Maybe (ListZipper a)
goLeft (LZ [] _ _) = Nothing
goLeft (LZ (l:ls) c rs) = Just (LZ ls l (c:rs))

goRight :: ListZipper a -> Maybe (ListZipper a)
goRight (LZ _ _ []) = Nothing
goRight (LZ ls c (r:rs)) = Just (LZ (c:ls) r rs)

modify :: (a -> a) -> ListZipper a -> ListZipper a
modify f (LZ ls c rs) = LZ ls (f c) rs

current :: ListZipper a -> a
current (LZ _ c _) = c

zipperDemo :: IO ()
zipperDemo = do
    putStrLn "\n=== List Zipper ==="
    case fromListZ [1, 2, 3, 4, 5] of
        Nothing -> putStrLn "Empty list"
        Just z -> do
            putStrLn $ "Initial: " ++ show z
            case goRight z >>= goRight of
                Nothing -> putStrLn "Can't move"
                Just z' -> do
                    putStrLn $ "After 2 rights: " ++ show z'
                    putStrLn $ "Current: " ++ show (current z')
                    let z'' = modify (*10) z'
                    putStrLn $ "After modify (*10): " ++ show z''

-- =============================================================================
-- COMPARISON OF LIST OPERATIONS
-- =============================================================================

comparisonTable :: IO ()
comparisonTable = do
    putStrLn "\n=== Haskell List Complexity ==="
    putStrLn ""
    putStrLn "Standard List []:"
    putStrLn "  head, tail, cons (:)  : O(1)"
    putStrLn "  length                : O(n)"
    putStrLn "  index (!!)            : O(n)"
    putStrLn "  append (++)           : O(n) in left list"
    putStrLn "  reverse               : O(n)"
    putStrLn ""
    putStrLn "Data.Sequence:"
    putStrLn "  <|, |> (cons/snoc)    : O(1)"
    putStrLn "  viewl, viewr          : O(1)"
    putStrLn "  index                 : O(log n)"
    putStrLn "  append (<>)           : O(log(min(n,m)))"
    putStrLn ""
    putStrLn "Difference List:"
    putStrLn "  append                : O(1)"
    putStrLn "  toList                : O(n)"
    putStrLn "  Good for building then converting once"

-- =============================================================================
-- WHEN TO USE WHAT
-- =============================================================================

usageGuide :: IO ()
usageGuide = do
    putStrLn "\n=== When to Use Each Structure ==="
    putStrLn ""
    putStrLn "Standard List []:"
    putStrLn "  - Stack operations (push/pop from front)"
    putStrLn "  - Sequential processing"
    putStrLn "  - Pattern matching"
    putStrLn "  - Most common choice"
    putStrLn ""
    putStrLn "Data.Sequence:"
    putStrLn "  - Need efficient access at both ends"
    putStrLn "  - Random access by index"
    putStrLn "  - Queue operations"
    putStrLn ""
    putStrLn "Two-List Queue:"
    putStrLn "  - Simple FIFO queue"
    putStrLn "  - Amortized O(1) operations"
    putStrLn ""
    putStrLn "Difference List:"
    putStrLn "  - Building list by appending many times"
    putStrLn "  - Logger/writer patterns"
    putStrLn ""
    putStrLn "Zipper:"
    putStrLn "  - Navigate and modify in place"
    putStrLn "  - Cursor-like interface"
    putStrLn "  - Efficient local modifications"

-- =============================================================================
-- MAIN
-- =============================================================================

main :: IO ()
main = do
    stackDemo
    balancedDemo
    queueDemo
    sequenceDemo
    linkedListDemo
    dlistDemo
    zipperDemo
    comparisonTable
    usageGuide
