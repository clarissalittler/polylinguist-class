# Lesson 9: Pattern Matching - Solution Guide

This guide provides example solutions for the Pattern Matching exercises.

## General Notes

- **Pattern matching**: Declarative way to check values against patterns
- **Destructuring**: Extract parts of data structures
- **Guards**: Add conditions to patterns
- **Exhaustiveness**: Compiler checks all cases are covered
- **Language support**: Haskell, Rust, OCaml have full support; Python 3.10+ has match; JavaScript has limited destructuring

---

## Exercise 1: Days of the Week

**Task:** Convert day number to day name

### Haskell Solution

```haskell
dayOfWeek :: Int -> String
dayOfWeek 1 = "Monday"
dayOfWeek 2 = "Tuesday"
dayOfWeek 3 = "Wednesday"
dayOfWeek 4 = "Thursday"
dayOfWeek 5 = "Friday"
dayOfWeek 6 = "Saturday"
dayOfWeek 7 = "Sunday"
dayOfWeek _ = "Invalid day"

main :: IO ()
main = do
    print $ dayOfWeek 1  -- "Monday"
    print $ dayOfWeek 7  -- "Sunday"
    print $ dayOfWeek 0  -- "Invalid day"
```

### Python 3.10+ Solution

```python
def day_of_week(day):
    match day:
        case 1:
            return "Monday"
        case 2:
            return "Tuesday"
        case 3:
            return "Wednesday"
        case 4:
            return "Thursday"
        case 5:
            return "Friday"
        case 6:
            return "Saturday"
        case 7:
            return "Sunday"
        case _:
            return "Invalid day"

# Test
print(day_of_week(1))  # "Monday"
print(day_of_week(7))  # "Sunday"
print(day_of_week(0))  # "Invalid day"

# Pre-3.10 Python (using dictionary)
def day_of_week_old(day):
    days = {
        1: "Monday", 2: "Tuesday", 3: "Wednesday",
        4: "Thursday", 5: "Friday", 6: "Saturday", 7: "Sunday"
    }
    return days.get(day, "Invalid day")
```

### Rust Solution

```rust
fn day_of_week(day: i32) -> &'static str {
    match day {
        1 => "Monday",
        2 => "Tuesday",
        3 => "Wednesday",
        4 => "Thursday",
        5 => "Friday",
        6 => "Saturday",
        7 => "Sunday",
        _ => "Invalid day",
    }
}

fn main() {
    println!("{}", day_of_week(1));  // "Monday"
    println!("{}", day_of_week(7));  // "Sunday"
    println!("{}", day_of_week(0));  // "Invalid day"
}
```

**Key Insights:**
- Pattern matching on literals
- Wildcard pattern `_` for default case
- More declarative than if-else chains
- Compiler can check exhaustiveness

---

## Exercise 3: Sign Function

**Task:** Determine sign of number using guards

### Haskell Solution

```haskell
sign :: (Ord a, Num a) => a -> a
sign n
    | n < 0     = -1
    | n == 0    = 0
    | otherwise = 1

-- Or with pattern matching and guards combined
sign' :: (Ord a, Num a) => a -> a
sign' 0 = 0
sign' n | n < 0     = -1
        | otherwise = 1

main :: IO ()
main = do
    print $ sign (-5)  -- -1
    print $ sign 0     -- 0
    print $ sign 10    -- 1
```

### Python 3.10+ Solution

```python
def sign(n):
    match n:
        case 0:
            return 0
        case n if n < 0:
            return -1
        case _:
            return 1

# Test
print(sign(-5))  # -1
print(sign(0))   # 0
print(sign(10))  # 1

# Traditional approach
def sign_old(n):
    if n < 0:
        return -1
    elif n == 0:
        return 0
    else:
        return 1
```

### Rust Solution

```rust
fn sign(n: i32) -> i32 {
    match n {
        0 => 0,
        n if n < 0 => -1,
        _ => 1,
    }
}

// Or using match on comparison
fn sign_alt(n: i32) -> i32 {
    match n.cmp(&0) {
        std::cmp::Ordering::Less => -1,
        std::cmp::Ordering::Equal => 0,
        std::cmp::Ordering::Greater => 1,
    }
}
```

**Key Insights:**
- Guards add conditions to patterns
- Pattern matching on specific values
- Combines structural matching with boolean logic

---

## Exercise 6: Point Quadrant

**Task:** Determine quadrant of a point using tuple patterns

### Haskell Solution

```haskell
type Point = (Double, Double)

quadrant :: Point -> String
quadrant (0, _) = "On axis"
quadrant (_, 0) = "On axis"
quadrant (x, y)
    | x > 0 && y > 0 = "Quadrant I"
    | x < 0 && y > 0 = "Quadrant II"
    | x < 0 && y < 0 = "Quadrant III"
    | otherwise      = "Quadrant IV"

main :: IO ()
main = do
    putStrLn $ quadrant (3, 4)    -- "Quadrant I"
    putStrLn $ quadrant (-2, 5)   -- "Quadrant II"
    putStrLn $ quadrant (-3, -4)  -- "Quadrant III"
    putStrLn $ quadrant (1, -2)   -- "Quadrant IV"
    putStrLn $ quadrant (0, 5)    -- "On axis"
```

### Python 3.10+ Solution

```python
def quadrant(point):
    match point:
        case (0, _) | (_, 0):
            return "On axis"
        case (x, y) if x > 0 and y > 0:
            return "Quadrant I"
        case (x, y) if x < 0 and y > 0:
            return "Quadrant II"
        case (x, y) if x < 0 and y < 0:
            return "Quadrant III"
        case (x, y):
            return "Quadrant IV"

# Test
print(quadrant((3, 4)))     # "Quadrant I"
print(quadrant((-2, 5)))    # "Quadrant II"
print(quadrant((0, 5)))     # "On axis"
```

### Rust Solution

```rust
fn quadrant(point: (f64, f64)) -> &'static str {
    match point {
        (0.0, _) | (_, 0.0) => "On axis",
        (x, y) if x > 0.0 && y > 0.0 => "Quadrant I",
        (x, y) if x < 0.0 && y > 0.0 => "Quadrant II",
        (x, y) if x < 0.0 && y < 0.0 => "Quadrant III",
        _ => "Quadrant IV",
    }
}
```

**Key Insights:**
- Tuple destructuring extracts components
- Wildcard `_` matches any value
- OR patterns `|` match multiple patterns
- Guards refine patterns with conditions

---

## Exercise 11: List Length Categories

**Task:** Categorize lists by length

### Haskell Solution

```haskell
listCategory :: [a] -> String
listCategory [] = "empty"
listCategory [_] = "singleton"
listCategory [_, _] = "pair"
listCategory [_, _, _] = "triple"
listCategory xs = "list of " ++ show (length xs) ++ " elements"

main :: IO ()
main = do
    putStrLn $ listCategory ([] :: [Int])      -- "empty"
    putStrLn $ listCategory [1]                -- "singleton"
    putStrLn $ listCategory [1, 2]             -- "pair"
    putStrLn $ listCategory [1, 2, 3]          -- "triple"
    putStrLn $ listCategory [1, 2, 3, 4, 5]    -- "list of 5 elements"
```

### Python 3.10+ Solution

```python
def list_category(lst):
    match lst:
        case []:
            return "empty"
        case [_]:
            return "singleton"
        case [_, _]:
            return "pair"
        case [_, _, _]:
            return "triple"
        case _:
            return f"list of {len(lst)} elements"

# Test
print(list_category([]))           # "empty"
print(list_category([1]))          # "singleton"
print(list_category([1, 2]))       # "pair"
print(list_category([1, 2, 3]))    # "triple"
print(list_category([1, 2, 3, 4, 5]))  # "list of 5 elements"
```

### Rust Solution

```rust
fn list_category<T>(list: &[T]) -> String {
    match list {
        [] => "empty".to_string(),
        [_] => "singleton".to_string(),
        [_, _] => "pair".to_string(),
        [_, _, _] => "triple".to_string(),
        _ => format!("list of {} elements", list.len()),
    }
}
```

**Key Insights:**
- List patterns match structure
- Underscore ignores values
- Pattern length determines match
- Beautiful for recursive list processing

---

## Exercise 12: Safe List Operations

**Task:** Safe head/tail using Option types

### Haskell Solution

```haskell
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (_:xs) = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit [_] = Just []
safeInit (x:xs) = case safeInit xs of
    Nothing -> Nothing
    Just ys -> Just (x:ys)

main :: IO ()
main = do
    print $ safeHead [1, 2, 3]      -- Just 1
    print $ safeHead ([] :: [Int])  -- Nothing
    print $ safeTail [1, 2, 3]      -- Just [2,3]
    print $ safeLast [1, 2, 3]      -- Just 3
    print $ safeInit [1, 2, 3]      -- Just [1,2]
```

### Python 3.10+ Solution

```python
from typing import Optional, List, TypeVar

T = TypeVar('T')

def safe_head(lst: List[T]) -> Optional[T]:
    match lst:
        case []:
            return None
        case [x, *_]:
            return x

def safe_tail(lst: List[T]) -> Optional[List[T]]:
    match lst:
        case []:
            return None
        case [_, *xs]:
            return xs

def safe_last(lst: List[T]) -> Optional[T]:
    match lst:
        case []:
            return None
        case [x]:
            return x
        case [_, *xs]:
            return safe_last(xs)

def safe_init(lst: List[T]) -> Optional[List[T]]:
    match lst:
        case []:
            return None
        case [_]:
            return []
        case [x, *xs]:
            rest = safe_init(xs)
            return [x] + rest if rest is not None else None

# Test
print(safe_head([1, 2, 3]))    # 1
print(safe_head([]))           # None
print(safe_tail([1, 2, 3]))    # [2, 3]
print(safe_last([1, 2, 3]))    # 3
print(safe_init([1, 2, 3]))    # [1, 2]
```

### Rust Solution

```rust
fn safe_head<T: Clone>(list: &[T]) -> Option<T> {
    match list {
        [] => None,
        [x, ..] => Some(x.clone()),
    }
}

fn safe_tail<T: Clone>(list: &[T]) -> Option<Vec<T>> {
    match list {
        [] => None,
        [_, xs @ ..] => Some(xs.to_vec()),
    }
}

fn safe_last<T: Clone>(list: &[T]) -> Option<T> {
    match list {
        [] => None,
        [x] => Some(x.clone()),
        [_, xs @ ..] => safe_last(xs),
    }
}
```

**Key Insights:**
- Option/Maybe types handle absence safely
- Pattern matching on list structure
- Cons pattern `(x:xs)` splits head and tail
- No null pointer exceptions!

---

## Exercise 13: List Sum with Patterns

**Task:** Recursive sum using pattern matching

### Haskell Solution

```haskell
listSum :: Num a => [a] -> a
listSum [] = 0
listSum [x] = x
listSum [x, y] = x + y  -- Optimization
listSum (x:xs) = x + listSum xs

-- Alternative with where clause
listSum' :: Num a => [a] -> a
listSum' [] = 0
listSum' (x:xs) = x + listSum' xs

main :: IO ()
main = do
    print $ listSum [1, 2, 3, 4, 5]  -- 15
    print $ listSum []               -- 0
    print $ listSum [10]             -- 10
```

### Python 3.10+ Solution

```python
def list_sum(lst):
    match lst:
        case []:
            return 0
        case [x]:
            return x
        case [x, y]:
            return x + y
        case [x, *xs]:
            return x + list_sum(xs)

# Test
print(list_sum([1, 2, 3, 4, 5]))  # 15
print(list_sum([]))               # 0
print(list_sum([10]))             # 10
```

### Rust Solution

```rust
fn list_sum(list: &[i32]) -> i32 {
    match list {
        [] => 0,
        [x] => *x,
        [x, y] => x + y,
        [x, xs @ ..] => x + list_sum(xs),
    }
}

fn main() {
    println!("{}", list_sum(&[1, 2, 3, 4, 5]));  // 15
    println!("{}", list_sum(&[]));                // 0
}
```

**Key Insights:**
- Base case: empty list
- Recursive case: head + sum of tail
- Pattern matching makes recursion natural
- Multiple base cases for optimization

---

## Exercise 16: Binary Tree Operations

**Task:** Pattern matching on tree structures

### Haskell Solution

```haskell
data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Show)

-- Count nodes
size :: Tree a -> Int
size Empty = 0
size (Node _ left right) = 1 + size left + size right

-- Maximum depth
depth :: Tree a -> Int
depth Empty = 0
depth (Node _ left right) = 1 + max (depth left) (depth right)

-- Sum all values
treeSum :: Num a => Tree a -> a
treeSum Empty = 0
treeSum (Node val left right) = val + treeSum left + treeSum right

-- Check if value exists
contains :: Eq a => a -> Tree a -> Bool
contains _ Empty = False
contains x (Node val left right) =
    x == val || contains x left || contains x right

-- Example tree:
--       5
--      / \
--     3   8
--    / \
--   1   4
exampleTree :: Tree Int
exampleTree = Node 5
    (Node 3
        (Node 1 Empty Empty)
        (Node 4 Empty Empty))
    (Node 8 Empty Empty)

main :: IO ()
main = do
    print $ size exampleTree      -- 5
    print $ depth exampleTree     -- 3
    print $ treeSum exampleTree   -- 21
    print $ contains 4 exampleTree  -- True
    print $ contains 10 exampleTree -- False
```

### Python 3.10+ Solution

```python
from dataclasses import dataclass
from typing import Optional, Generic, TypeVar

T = TypeVar('T')

@dataclass
class Empty:
    pass

@dataclass
class Node(Generic[T]):
    value: T
    left: 'Tree[T]'
    right: 'Tree[T]'

Tree = Empty | Node[T]

def size(tree: Tree) -> int:
    match tree:
        case Empty():
            return 0
        case Node(_, left, right):
            return 1 + size(left) + size(right)

def depth(tree: Tree) -> int:
    match tree:
        case Empty():
            return 0
        case Node(_, left, right):
            return 1 + max(depth(left), depth(right))

def tree_sum(tree: Tree[int]) -> int:
    match tree:
        case Empty():
            return 0
        case Node(val, left, right):
            return val + tree_sum(left) + tree_sum(right)

# Example tree
example_tree = Node(5,
    Node(3,
        Node(1, Empty(), Empty()),
        Node(4, Empty(), Empty())),
    Node(8, Empty(), Empty()))

print(size(example_tree))      # 5
print(depth(example_tree))     # 3
print(tree_sum(example_tree))  # 21
```

### Rust Solution

```rust
#[derive(Debug, Clone)]
enum Tree<T> {
    Empty,
    Node {
        value: T,
        left: Box<Tree<T>>,
        right: Box<Tree<T>>,
    },
}

impl<T> Tree<T> {
    fn size(&self) -> usize {
        match self {
            Tree::Empty => 0,
            Tree::Node { left, right, .. } => 1 + left.size() + right.size(),
        }
    }

    fn depth(&self) -> usize {
        match self {
            Tree::Empty => 0,
            Tree::Node { left, right, .. } => {
                1 + left.depth().max(right.depth())
            }
        }
    }
}

impl Tree<i32> {
    fn sum(&self) -> i32 {
        match self {
            Tree::Empty => 0,
            Tree::Node { value, left, right } => {
                value + left.sum() + right.sum()
            }
        }
    }
}
```

**Key Insights:**
- Algebraic data types perfect for trees
- Pattern matching on variants
- Recursive structure matches recursive algorithms
- Type-safe tree traversal

---

## Summary

Pattern matching is a powerful feature for working with structured data:

**Core Concepts:**
- **Literal patterns**: Match specific values
- **Variable patterns**: Bind to names
- **Wildcard**: `_` matches anything
- **Destructuring**: Extract components
- **Guards**: Add conditional logic
- **OR patterns**: Match multiple cases

**Benefits:**
- **Clarity**: Intent is clear
- **Safety**: Compiler checks exhaustiveness
- **Conciseness**: Less boilerplate than if-else
- **Correctness**: Harder to miss cases

**Common Patterns:**
- Lists: `[]`, `[x]`, `(x:xs)`
- Tuples: `(x, y)`, `(_, y, z)`
- Options: `Nothing`/`None`, `Just x`/`Some(x)`
- ADTs: Custom data type variants

**Language Support:**
- **Full**: Haskell, Rust, OCaml, F#, Elixir
- **Good**: Python 3.10+, Scala, Swift
- **Limited**: JavaScript (destructuring only)
- **None**: C, Java (pre-14)

Pattern matching makes code more declarative and maintainable. Master it to write clearer, safer programs!
