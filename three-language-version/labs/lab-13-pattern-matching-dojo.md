# Lab 13: Pattern Matching Dojo

**Quarter 2, Week 2**
**Duration:** 90 minutes
**Format:** Individual practice with partner check-ins

## Overview

Pattern matching is a powerful technique for working with structured data. Instead of asking "what is this?" with conditionals, you describe "what shape does this have?" and let the language match it. Haskell excels at this!

## Objectives

By the end of this lab, you will:
- [ ] Use pattern matching in Haskell fluently
- [ ] Convert conditionals to pattern matching
- [ ] Destructure data structures
- [ ] Appreciate pattern matching as a design tool

## Setup

- Create folder: `lab13-patterns/`
- Files: `matching.hs`, `matching.py`
- Have GHCi ready for experimentation

---

## Part 1: Basic Pattern Matching (20 minutes)

### Activity 1.1: Matching Literals

**Haskell - Pattern matching replaces if/else:**

```haskell
-- Instead of:
describeBad :: Int -> String
describeBad n = if n == 0 then "zero"
                else if n == 1 then "one"
                else "many"

-- Use pattern matching:
describe :: Int -> String
describe 0 = "zero"
describe 1 = "one"
describe _ = "many"  -- _ matches anything

main :: IO ()
main = do
    putStrLn (describe 0)  -- "zero"
    putStrLn (describe 1)  -- "one"
    putStrLn (describe 5)  -- "many"
```

**Python 3.10+ has match statements:**

```python
def describe(n):
    match n:
        case 0:
            return "zero"
        case 1:
            return "one"
        case _:
            return "many"

print(describe(0))  # "zero"
print(describe(5))  # "many"
```

### Activity 1.2: Guards - Conditional Patterns

**Haskell guards add conditions:**

```haskell
grade :: Int -> String
grade score
    | score >= 90 = "A"
    | score >= 80 = "B"
    | score >= 70 = "C"
    | score >= 60 = "D"
    | otherwise   = "F"

main :: IO ()
main = do
    putStrLn (grade 95)  -- "A"
    putStrLn (grade 72)  -- "C"
    putStrLn (grade 45)  -- "F"
```

**Python:**

```python
def grade(score):
    match score:
        case s if s >= 90:
            return "A"
        case s if s >= 80:
            return "B"
        case s if s >= 70:
            return "C"
        case s if s >= 60:
            return "D"
        case _:
            return "F"
```

### Activity 1.3: Practice - FizzBuzz with Patterns

**Your turn:** Implement FizzBuzz using pattern matching:

```haskell
-- Rules:
-- divisible by 3 and 5 -> "FizzBuzz"
-- divisible by 3 -> "Fizz"
-- divisible by 5 -> "Buzz"
-- otherwise -> show the number

fizzBuzz :: Int -> String
fizzBuzz n
    | n `mod` 15 == 0 = "FizzBuzz"
    | n `mod` 3 == 0  = "Fizz"
    | n `mod` 5 == 0  = "Buzz"
    | otherwise       = show n

main :: IO ()
main = mapM_ (putStrLn . fizzBuzz) [1..20]
```

### ✅ Checkpoint 1

Verify:
- [ ] Basic patterns work in Haskell
- [ ] Guards understood
- [ ] FizzBuzz implemented

---

## Part 2: Destructuring (25 minutes)

### Activity 2.1: Tuple Destructuring

**Haskell - Extract values from tuples:**

```haskell
-- Destructure in function definition
addPair :: (Int, Int) -> Int
addPair (x, y) = x + y

-- Get first and second
getFirst :: (a, b) -> a
getFirst (x, _) = x

getSecond :: (a, b) -> b
getSecond (_, y) = y

-- Triple
describePerson :: (String, Int, String) -> String
describePerson (name, age, city) =
    name ++ " is " ++ show age ++ " from " ++ city

main :: IO ()
main = do
    print (addPair (3, 5))                      -- 8
    print (getFirst ("hello", 42))              -- "hello"
    print (describePerson ("Alice", 30, "NYC")) -- "Alice is 30 from NYC"
```

**Python:**

```python
def add_pair(pair):
    x, y = pair
    return x + y

# Or directly in parameter
def add_pair_v2(pair):
    match pair:
        case (x, y):
            return x + y
```

### Activity 2.2: List Destructuring

**Haskell - Head and tail:**

```haskell
-- Match list patterns
sumList :: [Int] -> Int
sumList []     = 0                    -- Empty list
sumList (x:xs) = x + sumList xs       -- x is head, xs is tail

-- Get first element
headSafe :: [a] -> Maybe a
headSafe []    = Nothing
headSafe (x:_) = Just x

-- Get first two elements
firstTwo :: [a] -> (Maybe a, Maybe a)
firstTwo []       = (Nothing, Nothing)
firstTwo [x]      = (Just x, Nothing)
firstTwo (x:y:_)  = (Just x, Just y)

main :: IO ()
main = do
    print (sumList [1, 2, 3, 4, 5])  -- 15
    print (headSafe ([] :: [Int]))   -- Nothing
    print (headSafe [1, 2, 3])       -- Just 1
    print (firstTwo [1, 2, 3, 4])    -- (Just 1, Just 2)
```

**Python:**

```python
def sum_list(lst):
    match lst:
        case []:
            return 0
        case [x, *rest]:
            return x + sum_list(rest)

def first_two(lst):
    match lst:
        case []:
            return (None, None)
        case [x]:
            return (x, None)
        case [x, y, *_]:
            return (x, y)
```

### Activity 2.3: Practice - List Functions

**Your turn:** Implement these using pattern matching:

```haskell
-- Length of a list
myLength :: [a] -> Int
myLength [] = ???
myLength (_:xs) = ???

-- Last element of a list
myLast :: [a] -> Maybe a
myLast [] = ???
myLast [x] = ???
myLast (_:xs) = ???

-- Take first n elements
myTake :: Int -> [a] -> [a]
myTake 0 _ = ???
myTake _ [] = ???
myTake n (x:xs) = ???
```

### ✅ Checkpoint 2

Verify:
- [ ] Can destructure tuples
- [ ] Can destructure lists
- [ ] myLength and myLast work

---

## Part 3: Custom Data Types (25 minutes)

### Activity 3.1: Defining and Matching

**Haskell - Algebraic Data Types:**

```haskell
-- Define a shape type
data Shape = Circle Double
           | Rectangle Double Double
           | Triangle Double Double Double
    deriving Show

-- Calculate area using pattern matching
area :: Shape -> Double
area (Circle r)        = pi * r * r
area (Rectangle w h)   = w * h
area (Triangle a b c)  = let s = (a + b + c) / 2
                         in sqrt (s * (s-a) * (s-b) * (s-c))

-- Describe the shape
describe :: Shape -> String
describe (Circle r)      = "A circle with radius " ++ show r
describe (Rectangle w h) = "A " ++ show w ++ " by " ++ show h ++ " rectangle"
describe (Triangle _ _ _) = "A triangle"

main :: IO ()
main = do
    let shapes = [Circle 5, Rectangle 4 3, Triangle 3 4 5]
    mapM_ (\s -> putStrLn $ describe s ++ ", area = " ++ show (area s)) shapes
```

**Python:**

```python
from dataclasses import dataclass
import math

@dataclass
class Circle:
    radius: float

@dataclass
class Rectangle:
    width: float
    height: float

@dataclass
class Triangle:
    a: float
    b: float
    c: float

def area(shape):
    match shape:
        case Circle(r):
            return math.pi * r * r
        case Rectangle(w, h):
            return w * h
        case Triangle(a, b, c):
            s = (a + b + c) / 2
            return math.sqrt(s * (s-a) * (s-b) * (s-c))
```

### Activity 3.2: Maybe Type (Optional Values)

**Haskell's Maybe - No null!**

```haskell
-- Maybe is defined as:
-- data Maybe a = Nothing | Just a

safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

-- Pattern match on Maybe
showResult :: Maybe Double -> String
showResult Nothing  = "Error: division by zero"
showResult (Just x) = "Result: " ++ show x

-- Chain operations
doubleIfPositive :: Int -> Maybe Int
doubleIfPositive n
    | n > 0     = Just (n * 2)
    | otherwise = Nothing

main :: IO ()
main = do
    putStrLn $ showResult (safeDivide 10 2)  -- "Result: 5.0"
    putStrLn $ showResult (safeDivide 10 0)  -- "Error: division by zero"
```

### Activity 3.3: Either Type (Error Handling)

```haskell
-- Either has two cases: Left (error) or Right (success)
-- data Either a b = Left a | Right b

divide :: Double -> Double -> Either String Double
divide _ 0 = Left "Cannot divide by zero"
divide x y = Right (x / y)

sqrt' :: Double -> Either String Double
sqrt' x
    | x < 0     = Left "Cannot take sqrt of negative"
    | otherwise = Right (sqrt x)

-- Chain with case
calculate :: Double -> Double -> Either String Double
calculate x y = case divide x y of
    Left err -> Left err
    Right result -> sqrt' result

main :: IO ()
main = do
    print (calculate 16 4)   -- Right 2.0
    print (calculate 16 0)   -- Left "Cannot divide by zero"
    print (calculate (-4) 2) -- Left "Cannot take sqrt of negative"
```

### Activity 3.4: Your Turn - Expression Evaluator

Create a simple calculator using pattern matching:

```haskell
-- Define expression types
data Expr = Num Double
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
    deriving Show

-- Evaluate expressions
eval :: Expr -> Maybe Double
eval (Num n) = Just n
eval (Add a b) = -- TODO: evaluate both, add results
eval (Sub a b) = -- TODO
eval (Mul a b) = -- TODO
eval (Div a b) = -- TODO: handle division by zero!

-- Example: (5 + 3) * 2
example :: Expr
example = Mul (Add (Num 5) (Num 3)) (Num 2)
-- Should evaluate to 16
```

### ✅ Checkpoint 3

Verify:
- [ ] Shape area calculation works
- [ ] Understand Maybe type
- [ ] Expression evaluator started

---

## Part 4: Converting Conditionals (15 minutes)

### Activity 4.1: Refactoring Exercise

Convert this imperative code to pattern matching:

**Before (Python):**
```python
def process_command(cmd):
    if cmd == "quit":
        return "Goodbye!"
    elif cmd == "help":
        return "Available commands: quit, help, status"
    elif cmd == "status":
        return "All systems normal"
    elif cmd.startswith("echo "):
        return cmd[5:]
    else:
        return f"Unknown command: {cmd}"
```

**After (Haskell):**
```haskell
import Data.List (isPrefixOf, drop)

processCommand :: String -> String
processCommand "quit"   = "Goodbye!"
processCommand "help"   = "Available commands: quit, help, status"
processCommand "status" = "All systems normal"
processCommand cmd
    | "echo " `isPrefixOf` cmd = drop 5 cmd
    | otherwise                = "Unknown command: " ++ cmd
```

### Activity 4.2: State Machine

Model a simple traffic light:

```haskell
data Light = Red | Yellow | Green
    deriving (Show, Eq)

-- Next state
next :: Light -> Light
next Red    = Green
next Green  = Yellow
next Yellow = Red

-- Can a car go?
canGo :: Light -> Bool
canGo Green = True
canGo _     = False

-- Duration in seconds
duration :: Light -> Int
duration Red    = 30
duration Yellow = 5
duration Green  = 25

main :: IO ()
main = do
    let light = Green
    putStrLn $ "Light is " ++ show light
    putStrLn $ "Can go: " ++ show (canGo light)
    putStrLn $ "Duration: " ++ show (duration light) ++ "s"
    putStrLn $ "Next: " ++ show (next light)
```

---

## Part 5: Advanced Patterns (5 minutes)

### As-Patterns (@)

```haskell
-- Keep reference to whole while destructuring
firstAndAll :: [a] -> Maybe (a, [a])
firstAndAll []         = Nothing
firstAndAll all@(x:_)  = Just (x, all)  -- all@ binds the whole list

-- Useful for logging/debugging
process :: [Int] -> [Int]
process all@(x:xs)
    | sum all > 100 = all  -- Keep original if sum > 100
    | otherwise     = x * 2 : process xs
process [] = []
```

### View Patterns (Advanced)

```haskell
{-# LANGUAGE ViewPatterns #-}

import Data.Char (isDigit)

-- Match based on a transformation
classify :: String -> String
classify (all isDigit -> True)  = "All digits"
classify (length -> 0)          = "Empty"
classify _                      = "Mixed content"
```

---

## Challenges

### Challenge 1: JSON-like Parser

Define a data type for JSON and write functions to work with it:

```haskell
data JSON = JNull
          | JBool Bool
          | JNum Double
          | JStr String
          | JArr [JSON]
          | JObj [(String, JSON)]
    deriving Show

-- Pretty print JSON
prettyPrint :: JSON -> String
prettyPrint JNull = "null"
prettyPrint (JBool b) = if b then "true" else "false"
-- ... complete the implementation
```

### Challenge 2: Game Inventory

```haskell
data Item = Weapon String Int      -- name, damage
          | Armor String Int       -- name, defense
          | Potion String Int      -- name, healing
          | Key String             -- name
    deriving Show

-- Calculate total damage of weapons
totalDamage :: [Item] -> Int
-- Calculate total defense of armor
totalDefense :: [Item] -> Int
-- Find all potions
findPotions :: [Item] -> [Item]
```

---

## Wrap-Up

**Key takeaways:**

1. **Pattern matching** is declarative - describe what to match
2. **Destructuring** extracts data elegantly
3. **Algebraic Data Types** + patterns = powerful modeling
4. **Guards** add conditions to patterns
5. **Maybe/Either** handle errors without null

**Mental shift:**
- From: "Check if x, then do y"
- To: "When x looks like this, do y"

**Next lab:** Type Design - designing with types first!
