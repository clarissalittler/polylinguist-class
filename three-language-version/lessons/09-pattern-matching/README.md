# Lesson 9: Pattern Matching

## Overview

Pattern matching is a powerful control flow mechanism that allows you to:
- Check values against multiple patterns
- Destructure complex data structures
- Extract values from nested structures
- Write declarative, concise code

This lesson explores pattern matching across:
- **Haskell**: Native, comprehensive pattern matching
- **Python 3.10+**: Structural pattern matching (`match`/`case`)
- **C++**: std::variant with std::visit, or traditional approaches

## Learning Objectives

- Understand pattern matching vs traditional conditionals
- Match against literals, types, and structures
- Destructure data with patterns
- Use guards and conditions
- Compare pattern matching across paradigms

## What is Pattern Matching?

**Pattern matching** tests a value against patterns and executes code based on matches.

**Haskell (Native):**
```haskell
describe :: Int -> String
describe 0 = "zero"
describe 1 = "one"
describe 2 = "two"
describe n = "many: " ++ show n
```

**Python 3.10+ (match/case):**
```python
def describe(n):
    match n:
        case 0:
            return "zero"
        case 1:
            return "one"
        case 2:
            return "two"
        case _:
            return f"many: {n}"
```

**C++ (std::variant):**
```cpp
#include <variant>
std::string describe(const std::variant<int, std::string>& v) {
    return std::visit([](auto&& arg) -> std::string {
        using T = std::decay_t<decltype(arg)>;
        if constexpr (std::is_same_v<T, int>) {
            if (arg == 0) return "zero";
            else if (arg == 1) return "one";
            else return "many: " + std::to_string(arg);
        } else {
            return "string: " + arg;
        }
    }, v);
}
```

## Destructuring Data Structures

**Haskell (Lists):**
```haskell
head' :: [a] -> Maybe a
head' [] = Nothing
head' (x:xs) = Just x

tail' :: [a] -> Maybe [a]
tail' [] = Nothing
tail' (x:xs) = Just xs

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs
```

**Python (match/case):**
```python
def process_list(lst):
    match lst:
        case []:
            return "empty"
        case [x]:
            return f"single: {x}"
        case [x, y]:
            return f"pair: {x}, {y}"
        case [x, *rest]:
            return f"first: {x}, rest: {rest}"
```

**C++ (pattern-like with structured bindings):**
```cpp
auto process_vector(const std::vector<int>& vec) {
    if (vec.empty()) return "empty";
    if (vec.size() == 1) return "single: " + std::to_string(vec[0]);
    if (vec.size() == 2) {
        auto [a, b] = std::make_pair(vec[0], vec[1]);
        return "pair: " + std::to_string(a) + ", " + std::to_string(b);
    }
    return "multiple";
}
```

## Real-World Example: Expression Evaluator

**Haskell:**
```haskell
data Expr = Num Int
          | Add Expr Expr
          | Mul Expr Expr
          | Sub Expr Expr

eval :: Expr -> Int
eval (Num n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Sub e1 e2) = eval e1 - eval e2

-- Example: (2 + 3) * 4
result = eval (Mul (Add (Num 2) (Num 3)) (Num 4))  -- 20
```

**Python:**
```python
from dataclasses import dataclass

@dataclass
class Num:
    value: int

@dataclass
class Add:
    left: 'Expr'
    right: 'Expr'

@dataclass
class Mul:
    left: 'Expr'
    right: 'Expr'

Expr = Num | Add | Mul

def eval_expr(expr):
    match expr:
        case Num(n):
            return n
        case Add(left, right):
            return eval_expr(left) + eval_expr(right)
        case Mul(left, right):
            return eval_expr(left) * eval_expr(right)
```

## Guards and Conditions

**Haskell:**
```haskell
classify :: Int -> String
classify n
    | n < 0     = "negative"
    | n == 0    = "zero"
    | n < 10    = "small positive"
    | n < 100   = "medium positive"
    | otherwise = "large positive"
```

**Python:**
```python
def classify(n):
    match n:
        case x if x < 0:
            return "negative"
        case 0:
            return "zero"
        case x if x < 10:
            return "small positive"
        case x if x < 100:
            return "medium positive"
        case _:
            return "large positive"
```

## Language Comparison

| Feature | Haskell | Python 3.10+ | C++ |
|---------|---------|--------------|-----|
| **Native Support** | Yes | Yes (match/case) | Limited (variant/visit) |
| **Exhaustiveness** | Compile-time | Runtime | Manual |
| **Destructuring** | Full | Good | Limited |
| **Guards** | Built-in | With `if` | Manual |
| **Performance** | Optimized | Runtime overhead | Can optimize |

## Key Takeaways

1. **Pattern matching is more than switch/case** - it includes destructuring
2. **Haskell** has the most comprehensive pattern matching
3. **Python 3.10+** added structural pattern matching
4. **C++** requires std::variant or traditional approaches
5. Pattern matching makes code more declarative and safer

## Exercises

See `EXERCISES.md` for hands-on practice.
