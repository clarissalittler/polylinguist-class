# Lesson 10: Type Systems

## Overview

Type systems classify values and expressions, preventing certain programming errors at compile-time or runtime.

## Learning Objectives

- Understand static vs dynamic typing
- Explore strong vs weak typing
- Learn type inference and type annotations
- Compare Python (dynamic), C++ (static), Haskell (static with inference)
- Use algebraic data types (Haskell)

## Static vs Dynamic Typing

**Static (C++, Haskell):**
- Types checked at compile-time
- Errors caught before runtime
- Better performance (no runtime type checks)

**Dynamic (Python):**
- Types checked at runtime
- More flexible, less verbose
- Runtime type errors possible

**C++:**
```cpp
int x = 5;          // Type known at compile-time
// x = "hello";     // Compile error!
```

**Python:**
```python
x = 5               # Type determined at runtime
x = "hello"         # OK - dynamic typing
```

**Haskell:**
```haskell
x :: Int
x = 5               -- Type known at compile-time
-- x = "hello"     -- Compile error!
```

## Type Inference

**Haskell (Strong Inference):**
```haskell
add x y = x + y
-- Inferred: add :: Num a => a -> a -> a
```

**C++ (Auto keyword):**
```cpp
auto x = 5;         // Inferred as int
auto y = 5.0;       // Inferred as double
```

**Python (Type Hints):**
```python
def add(x: int, y: int) -> int:
    return x + y    # Hints, not enforced at runtime
```

## Algebraic Data Types (Haskell)

**Sum Types (OR):**
```haskell
data Maybe a = Nothing | Just a

data Either a b = Left a | Right b
```

**Product Types (AND):**
```haskell
data Person = Person String Int  -- Name AND Age
```

## Generic Programming

**Python (Duck Typing):**
```python
def first(items):
    return items[0]  # Works with any sequence
```

**C++ (Templates):**
```cpp
template<typename T>
T first(const std::vector<T>& items) {
    return items[0];
}
```

**Haskell (Parametric Polymorphism):**
```haskell
first :: [a] -> a
first (x:_) = x
```

## Key Takeaways

1. **Static typing** catches errors at compile-time
2. **Dynamic typing** offers flexibility
3. **Type inference** combines safety with conciseness
4. **Haskell** has the most advanced type system
5. **Python** added optional type hints
6. **C++** has powerful but complex type system

See EXERCISES.md for practice.
