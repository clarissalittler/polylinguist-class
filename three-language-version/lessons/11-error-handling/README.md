# Lesson 11: Error Handling

## Overview

Error handling manages exceptional conditions and failures gracefully.

## Learning Objectives

- Understand exceptions vs return codes
- Learn try/catch/finally patterns
- Explore Maybe/Option types
- Use Either/Result for errors
- Compare imperative vs functional error handling

## Exception Handling

**Python (Exceptions):**
```python
try:
    result = risky_operation()
except ValueError as e:
    print(f"Value error: {e}")
except Exception as e:
    print(f"Unexpected error: {e}")
finally:
    cleanup()
```

**C++ (Exceptions):**
```cpp
try {
    result = risky_operation();
} catch (const std::invalid_argument& e) {
    std::cerr << "Invalid argument: " << e.what() << std::endl;
} catch (const std::exception& e) {
    std::cerr << "Error: " << e.what() << std::endl;
}
```

**Haskell (Maybe/Either):**
```haskell
-- Maybe for operations that might fail
safe_div :: Double -> Double -> Maybe Double
safe_div _ 0 = Nothing
safe_div x y = Just (x / y)

-- Either for errors with information
safe_div' :: Double -> Double -> Either String Double
safe_div' _ 0 = Left "Division by zero"
safe_div' x y = Right (x / y)
```

## Functional Error Handling

**Option/Maybe Pattern:**
```haskell
lookup_user :: Int -> Maybe User
process_user :: User -> Maybe Result

-- Chaining with >>= (bind)
get_result :: Int -> Maybe Result
get_result id = lookup_user id >>= process_user
```

**Result/Either Pattern:**
```haskell
parse_int :: String -> Either String Int
validate :: Int -> Either String Int
process :: Int -> Either String Result

-- Chaining
full_process :: String -> Either String Result
full_process s = parse_int s >>= validate >>= process
```

## Error Handling Comparison

| Approach | Python | C++ | Haskell |
|----------|--------|-----|---------|
| **Exceptions** | Yes | Yes | Not idiomatic |
| **Return codes** | Possible | Possible | Not needed |
| **Maybe/Option** | Not built-in | std::optional | Built-in |
| **Either/Result** | Not built-in | Not built-in | Built-in |
| **Type safety** | Runtime | Compile-time | Compile-time |

## Best Practices

1. **Use exceptions** for truly exceptional conditions
2. **Use Maybe/Option** for expected failures
3. **Use Either/Result** for errors with context
4. **Always clean up resources** (RAII in C++, context managers in Python)
5. **Don't ignore errors** - handle or propagate them

See EXERCISES.md for practice with error handling patterns.
