-- Lesson 15: Trees - Haskell Examples
-- Binary trees, BSTs, traversals, and common operations

module Main where

import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Map (Map)

-- =============================================================================
-- BINARY TREE
-- =============================================================================

data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Eq)

instance Show a => Show (Tree a) where
    show Empty = "Empty"
    show (Node x left right) =
        "Node " ++ show x ++ " (" ++ show left ++ ") (" ++ show right ++ ")"

-- Smart constructor
leaf :: a -> Tree a
leaf x = Node x Empty Empty

-- Sample tree:
--       1
--      / \
--     2   3
--    / \   \
--   4   5   6
sampleTree :: Tree Int
sampleTree = Node 1
    (Node 2 (leaf 4) (leaf 5))
    (Node 3 Empty (leaf 6))

-- =============================================================================
-- TREE TRAVERSALS
-- =============================================================================

-- In-order: Left, Node, Right
inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node x left right) = inorder left ++ [x] ++ inorder right

-- Pre-order: Node, Left, Right
preorder :: Tree a -> [a]
preorder Empty = []
preorder (Node x left right) = [x] ++ preorder left ++ preorder right

-- Post-order: Left, Right, Node
postorder :: Tree a -> [a]
postorder Empty = []
postorder (Node x left right) = postorder left ++ postorder right ++ [x]

-- Level-order (BFS)
levelOrder :: Tree a -> [[a]]
levelOrder tree = go [tree]
  where
    go [] = []
    go level =
        let values = [x | Node x _ _ <- level]
            children = concat [[l, r] | Node _ l r <- level, let ls = [l | l /= Empty], let rs = [r | r /= Empty], (l, r) <- [(l, r)]]
            nextLevel = [child | Node _ l r <- level, child <- [l, r], child /= Empty]
        in if null values then [] else values : go nextLevel

-- Alternative level-order implementation
levels :: Tree a -> [[a]]
levels Empty = []
levels tree = map (map nodeValue . filter (not . isEmpty)) $ takeWhile (not . null) $ iterate nextLevel [tree]
  where
    isEmpty Empty = True
    isEmpty _ = False
    nodeValue (Node x _ _) = x
    nodeValue Empty = error "Empty node"
    nextLevel nodes = concatMap children nodes
    children Empty = []
    children (Node _ l r) = [l, r]

traversalDemo :: IO ()
traversalDemo = do
    putStrLn "=== Tree Traversals ==="
    putStrLn $ "Sample tree: " ++ show sampleTree
    putStrLn $ "In-order:    " ++ show (inorder sampleTree)
    putStrLn $ "Pre-order:   " ++ show (preorder sampleTree)
    putStrLn $ "Post-order:  " ++ show (postorder sampleTree)
    putStrLn $ "Level-order: " ++ show (levels sampleTree)

-- =============================================================================
-- BINARY SEARCH TREE
-- =============================================================================

-- Insert into BST
bstInsert :: Ord a => a -> Tree a -> Tree a
bstInsert x Empty = leaf x
bstInsert x (Node y left right)
    | x < y     = Node y (bstInsert x left) right
    | otherwise = Node y left (bstInsert x right)

-- Build BST from list
bstFromList :: Ord a => [a] -> Tree a
bstFromList = foldr bstInsert Empty

-- Search in BST
bstSearch :: Ord a => a -> Tree a -> Bool
bstSearch _ Empty = False
bstSearch x (Node y left right)
    | x == y    = True
    | x < y     = bstSearch x left
    | otherwise = bstSearch x right

-- Find minimum (leftmost)
bstMin :: Tree a -> Maybe a
bstMin Empty = Nothing
bstMin (Node x Empty _) = Just x
bstMin (Node _ left _) = bstMin left

-- Find maximum (rightmost)
bstMax :: Tree a -> Maybe a
bstMax Empty = Nothing
bstMax (Node x _ Empty) = Just x
bstMax (Node _ _ right) = bstMax right

-- Delete from BST
bstDelete :: Ord a => a -> Tree a -> Tree a
bstDelete _ Empty = Empty
bstDelete x (Node y left right)
    | x < y     = Node y (bstDelete x left) right
    | x > y     = Node y left (bstDelete x right)
    | otherwise = case (left, right) of
        (Empty, _) -> right
        (_, Empty) -> left
        _ -> case bstMin right of
            Just successor -> Node successor left (bstDelete successor right)
            Nothing -> left  -- Should not happen

bstDemo :: IO ()
bstDemo = do
    putStrLn "\n=== Binary Search Tree ==="
    let bst = bstFromList [5, 3, 7, 2, 4, 6, 8]
    putStrLn $ "BST in-order (sorted): " ++ show (inorder bst)
    putStrLn $ "Search 4: " ++ show (bstSearch 4 bst)
    putStrLn $ "Search 9: " ++ show (bstSearch 9 bst)
    putStrLn $ "Min: " ++ show (bstMin bst)
    putStrLn $ "Max: " ++ show (bstMax bst)
    let bst' = bstDelete 5 bst
    putStrLn $ "After deleting 5: " ++ show (inorder bst')

-- =============================================================================
-- TREE PROPERTIES
-- =============================================================================

-- Height of tree
treeHeight :: Tree a -> Int
treeHeight Empty = -1
treeHeight (Node _ left right) = 1 + max (treeHeight left) (treeHeight right)

-- Size (number of nodes)
treeSize :: Tree a -> Int
treeSize Empty = 0
treeSize (Node _ left right) = 1 + treeSize left + treeSize right

-- Check if balanced (height difference <= 1)
isBalanced :: Tree a -> Bool
isBalanced Empty = True
isBalanced (Node _ left right) =
    abs (treeHeight left - treeHeight right) <= 1 &&
    isBalanced left && isBalanced right

-- Check if valid BST
isBST :: Ord a => Tree a -> Bool
isBST tree = go tree Nothing Nothing
  where
    go Empty _ _ = True
    go (Node x left right) minVal maxVal =
        maybe True (< x) minVal &&
        maybe True (> x) maxVal &&
        go left minVal (Just x) &&
        go right (Just x) maxVal

propertiesDemo :: IO ()
propertiesDemo = do
    putStrLn "\n=== Tree Properties ==="
    putStrLn $ "Height: " ++ show (treeHeight sampleTree)
    putStrLn $ "Size: " ++ show (treeSize sampleTree)
    putStrLn $ "Is balanced: " ++ show (isBalanced sampleTree)
    let bst = bstFromList [5, 3, 7, 2, 4, 6, 8]
    putStrLn $ "Is BST: " ++ show (isBST bst)

-- =============================================================================
-- COMMON TREE OPERATIONS
-- =============================================================================

-- Invert/mirror tree
invertTree :: Tree a -> Tree a
invertTree Empty = Empty
invertTree (Node x left right) = Node x (invertTree right) (invertTree left)

-- Path sum (root to leaf with given sum)
hasPathSum :: Tree Int -> Int -> Bool
hasPathSum Empty _ = False
hasPathSum (Node x Empty Empty) target = x == target
hasPathSum (Node x left right) target =
    hasPathSum left (target - x) || hasPathSum right (target - x)

-- Max depth
maxDepth :: Tree a -> Int
maxDepth Empty = 0
maxDepth (Node _ left right) = 1 + max (maxDepth left) (maxDepth right)

-- All paths from root to leaves
allPaths :: Tree a -> [[a]]
allPaths Empty = []
allPaths (Node x Empty Empty) = [[x]]
allPaths (Node x left right) = map (x:) (allPaths left ++ allPaths right)

-- Fold over tree (in-order)
treeFold :: (b -> a -> b) -> b -> Tree a -> b
treeFold _ acc Empty = acc
treeFold f acc (Node x left right) =
    treeFold f (f (treeFold f acc left) x) right

problemsDemo :: IO ()
problemsDemo = do
    putStrLn "\n=== Common Tree Problems ==="
    putStrLn $ "Max depth: " ++ show (maxDepth sampleTree)
    putStrLn $ "Has path sum 7 (1->2->4): " ++ show (hasPathSum sampleTree 7)
    putStrLn $ "All root-to-leaf paths: " ++ show (allPaths sampleTree)
    putStrLn $ "Sum all nodes: " ++ show (treeFold (+) 0 sampleTree)

-- =============================================================================
-- ROSE TREE (N-ary tree)
-- =============================================================================

data Rose a = Rose a [Rose a] deriving (Show, Eq)

-- Fold over rose tree
roseFold :: (a -> [b] -> b) -> Rose a -> b
roseFold f (Rose x children) = f x (map (roseFold f) children)

-- Sum all values
roseSum :: Rose Int -> Int
roseSum = roseFold (\x sums -> x + sum sums)

-- Max depth
roseDepth :: Rose a -> Int
roseDepth = roseFold (\_ depths -> 1 + maximum (0 : depths))

roseDemo :: IO ()
roseDemo = do
    putStrLn "\n=== Rose Tree (N-ary) ==="
    let rose = Rose 1 [Rose 2 [Rose 4 [], Rose 5 []], Rose 3 [Rose 6 []]]
    putStrLn $ "Rose tree: " ++ show rose
    putStrLn $ "Sum: " ++ show (roseSum rose)
    putStrLn $ "Depth: " ++ show (roseDepth rose)

-- =============================================================================
-- TRIE (Prefix Tree)
-- =============================================================================

data Trie = TrieNode Bool (Map Char Trie) deriving Show

emptyTrie :: Trie
emptyTrie = TrieNode False Map.empty

trieInsert :: String -> Trie -> Trie
trieInsert [] (TrieNode _ children) = TrieNode True children
trieInsert (c:cs) (TrieNode isEnd children) =
    TrieNode isEnd (Map.alter (Just . trieInsert cs . maybe emptyTrie id) c children)

trieSearch :: String -> Trie -> Bool
trieSearch [] (TrieNode isEnd _) = isEnd
trieSearch (c:cs) (TrieNode _ children) =
    case Map.lookup c children of
        Nothing -> False
        Just child -> trieSearch cs child

trieStartsWith :: String -> Trie -> Bool
trieStartsWith [] _ = True
trieStartsWith (c:cs) (TrieNode _ children) =
    case Map.lookup c children of
        Nothing -> False
        Just child -> trieStartsWith cs child

trieDemo :: IO ()
trieDemo = do
    putStrLn "\n=== Trie (Prefix Tree) ==="
    let trie = foldr trieInsert emptyTrie ["apple", "app", "application", "banana"]
    putStrLn $ "Search 'app': " ++ show (trieSearch "app" trie)
    putStrLn $ "Search 'appl': " ++ show (trieSearch "appl" trie)
    putStrLn $ "Starts with 'app': " ++ show (trieStartsWith "app" trie)
    putStrLn $ "Starts with 'ban': " ++ show (trieStartsWith "ban" trie)

-- =============================================================================
-- ZIPPER FOR TREES (Navigate and modify)
-- =============================================================================

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving Show
type Breadcrumbs a = [Crumb a]
type TreeZipper a = (Tree a, Breadcrumbs a)

goLeft :: TreeZipper a -> Maybe (TreeZipper a)
goLeft (Node x left right, bs) = Just (left, LeftCrumb x right : bs)
goLeft _ = Nothing

goRight :: TreeZipper a -> Maybe (TreeZipper a)
goRight (Node x left right, bs) = Just (right, RightCrumb x left : bs)
goRight _ = Nothing

goUp :: TreeZipper a -> Maybe (TreeZipper a)
goUp (_, []) = Nothing
goUp (t, LeftCrumb x right : bs) = Just (Node x t right, bs)
goUp (t, RightCrumb x left : bs) = Just (Node x left t, bs)

modifyZipper :: (a -> a) -> TreeZipper a -> TreeZipper a
modifyZipper f (Node x l r, bs) = (Node (f x) l r, bs)
modifyZipper _ z = z

zipperDemo :: IO ()
zipperDemo = do
    putStrLn "\n=== Tree Zipper ==="
    let zipper = (sampleTree, [])
    putStrLn "Starting at root"
    case goLeft zipper >>= goLeft of
        Nothing -> putStrLn "Can't navigate"
        Just z -> do
            putStrLn $ "After going left twice, at node: " ++ show (fst z)
            let z' = modifyZipper (*10) z
            putStrLn $ "After modifying: " ++ show (fst z')

-- =============================================================================
-- COMPLEXITY SUMMARY
-- =============================================================================

complexityNotes :: IO ()
complexityNotes = do
    putStrLn "\n=== Tree Complexity in Haskell ==="
    putStrLn ""
    putStrLn "Binary Search Tree:"
    putStrLn "  - Insert, Search, Delete: O(h)"
    putStrLn "  - h = O(log n) balanced, O(n) worst case"
    putStrLn ""
    putStrLn "Data.Set / Data.Map (balanced trees):"
    putStrLn "  - Insert, Lookup, Delete: O(log n)"
    putStrLn "  - Use these instead of rolling your own!"
    putStrLn ""
    putStrLn "Trie:"
    putStrLn "  - Insert, Search: O(m) where m = string length"
    putStrLn ""
    putStrLn "Tree traversals:"
    putStrLn "  - All traversals: O(n)"
    putStrLn ""
    putStrLn "Haskell-specific notes:"
    putStrLn "  - Trees are immutable - 'modifications' create new trees"
    putStrLn "  - Shared structure makes this efficient"
    putStrLn "  - Zippers allow 'focused' modifications in O(1)"

-- =============================================================================
-- MAIN
-- =============================================================================

main :: IO ()
main = do
    traversalDemo
    bstDemo
    propertiesDemo
    problemsDemo
    roseDemo
    trieDemo
    zipperDemo
    complexityNotes
