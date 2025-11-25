# Lab 15: Error Handling

**Quarter 2, Week 4**
**Duration:** 90 minutes
**Format:** Pair programming

## Overview

Errors happen. Good programmers anticipate them and handle them gracefully. This lab explores different philosophies of error handling across our languages.

## Objectives

By the end of this lab, you will:
- [ ] Use try/except in Python
- [ ] Handle errors with Maybe/Either in Haskell
- [ ] Compare exception vs. return-value error handling
- [ ] Write robust code that fails gracefully

## Setup

- Partner up
- Create folder: `lab15-errors/`
- Files: `errors.py`, `errors.cpp`, `errors.hs`

---

## Part 1: Python Exceptions (25 minutes)

### Activity 1.1: Basic Try/Except

```python
def divide(a, b):
    """Divide a by b."""
    return a / b

# Without error handling
print(divide(10, 2))   # 5.0
# print(divide(10, 0)) # ZeroDivisionError - crashes!

# With error handling
def safe_divide(a, b):
    try:
        return a / b
    except ZeroDivisionError:
        print("Cannot divide by zero!")
        return None

print(safe_divide(10, 2))  # 5.0
print(safe_divide(10, 0))  # Cannot divide by zero! then None
```

### Activity 1.2: Multiple Exception Types

```python
def process_data(data, index):
    """Get item at index and convert to int."""
    try:
        item = data[index]
        return int(item)
    except IndexError:
        print(f"Index {index} is out of range")
        return None
    except ValueError:
        print(f"Cannot convert '{data[index]}' to int")
        return None
    except TypeError:
        print("Data is not subscriptable")
        return None

# Test cases
data = ["42", "hello", "100"]
print(process_data(data, 0))   # 42
print(process_data(data, 1))   # Cannot convert 'hello'... None
print(process_data(data, 10))  # Index 10 out of range... None
print(process_data(None, 0))   # Data is not subscriptable... None
```

### Activity 1.3: Finally and Else

```python
def read_file(filename):
    """Read file with proper cleanup."""
    file = None
    try:
        file = open(filename, 'r')
        content = file.read()
    except FileNotFoundError:
        print(f"File '{filename}' not found")
        content = None
    except PermissionError:
        print(f"Permission denied for '{filename}'")
        content = None
    else:
        # Runs only if no exception
        print(f"Successfully read {len(content)} characters")
    finally:
        # Always runs (cleanup)
        if file:
            file.close()
            print("File closed")
    return content

# Better: use context manager
def read_file_better(filename):
    try:
        with open(filename, 'r') as file:
            return file.read()
    except FileNotFoundError:
        return None
```

### Activity 1.4: Raising Exceptions

```python
def validate_age(age):
    """Validate that age is reasonable."""
    if not isinstance(age, (int, float)):
        raise TypeError("Age must be a number")
    if age < 0:
        raise ValueError("Age cannot be negative")
    if age > 150:
        raise ValueError("Age seems unrealistic")
    return int(age)

# Custom exception
class ValidationError(Exception):
    """Custom exception for validation errors."""
    pass

def validate_email(email):
    if not isinstance(email, str):
        raise ValidationError("Email must be a string")
    if "@" not in email:
        raise ValidationError("Email must contain @")
    if "." not in email.split("@")[1]:
        raise ValidationError("Email domain must contain .")
    return email.lower()

# Usage
try:
    email = validate_email("user@example.com")
    print(f"Valid email: {email}")
except ValidationError as e:
    print(f"Invalid: {e}")
```

### ✅ Checkpoint 1

Verify:
- [ ] Can catch specific exceptions
- [ ] Understand try/except/else/finally
- [ ] Can raise and catch custom exceptions

---

## Part 2: Haskell's Approach - Maybe and Either (25 minutes)

### Activity 2.1: Maybe - Optional Values

```haskell
-- Maybe is defined as:
-- data Maybe a = Nothing | Just a

-- Safe division
safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

-- Safe list head
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

-- Safe list index
safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _     = Nothing
safeIndex (x:_) 0  = Just x
safeIndex (_:xs) n
    | n < 0     = Nothing
    | otherwise = safeIndex xs (n - 1)

main :: IO ()
main = do
    print (safeDivide 10 2)    -- Just 5.0
    print (safeDivide 10 0)    -- Nothing

    print (safeHead [1,2,3])   -- Just 1
    print (safeHead ([] :: [Int]))  -- Nothing

    print (safeIndex [1,2,3] 1)   -- Just 2
    print (safeIndex [1,2,3] 10)  -- Nothing
```

### Activity 2.2: Working with Maybe

```haskell
-- Pattern matching on Maybe
showResult :: Maybe Int -> String
showResult Nothing  = "No result"
showResult (Just x) = "Result: " ++ show x

-- Using maybe function
-- maybe default f m = case m of Nothing -> default; Just x -> f x
processResult :: Maybe Int -> Int
processResult m = maybe 0 (*2) m  -- Double if Just, 0 if Nothing

-- Chaining Maybe with >>=
-- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
safeSqrt :: Double -> Maybe Double
safeSqrt x
    | x < 0     = Nothing
    | otherwise = Just (sqrt x)

-- Divide, then take square root
divideAndSqrt :: Double -> Double -> Maybe Double
divideAndSqrt x y = safeDivide x y >>= safeSqrt

main :: IO ()
main = do
    print (divideAndSqrt 16 4)   -- Just 2.0
    print (divideAndSqrt 16 0)   -- Nothing (div by zero)
    print (divideAndSqrt (-16) 4) -- Nothing (sqrt of negative)
```

### Activity 2.3: Either - Errors with Messages

```haskell
-- Either has two cases:
-- data Either a b = Left a | Right b
-- By convention: Left = error, Right = success

type Error = String

safeDivideE :: Double -> Double -> Either Error Double
safeDivideE _ 0 = Left "Division by zero"
safeDivideE x y = Right (x / y)

safeSqrtE :: Double -> Either Error Double
safeSqrtE x
    | x < 0     = Left "Cannot sqrt negative number"
    | otherwise = Right (sqrt x)

-- Chain with >>=
calculate :: Double -> Double -> Either Error Double
calculate x y = safeDivideE x y >>= safeSqrtE

-- Pattern match on result
showResultE :: Either Error Double -> String
showResultE (Left err)  = "Error: " ++ err
showResultE (Right val) = "Success: " ++ show val

main :: IO ()
main = do
    putStrLn $ showResultE (calculate 16 4)   -- Success: 2.0
    putStrLn $ showResultE (calculate 16 0)   -- Error: Division by zero
    putStrLn $ showResultE (calculate (-4) 2) -- Error: Cannot sqrt negative
```

### Activity 2.4: Practice

Implement these safe functions:

```haskell
-- Safe string to int conversion
safeRead :: String -> Maybe Int
safeRead s = ???  -- Hint: use reads function

-- Safe list lookup
lookupSafe :: Eq a => a -> [(a, b)] -> Maybe b
lookupSafe key [] = ???
lookupSafe key ((k,v):rest) = ???

-- Parse a point "x,y" returning Either
parsePoint :: String -> Either String (Int, Int)
parsePoint s = ???  -- Return Left with error message or Right with point
```

### ✅ Checkpoint 2

Verify:
- [ ] Can use Maybe for optional values
- [ ] Can chain Maybe operations
- [ ] Understand Either for error messages

---

## Part 3: Comparison and Best Practices (20 minutes)

### Activity 3.1: Same Problem, Different Styles

**Problem:** Parse a list of numbers from strings, handling errors.

**Python - Exceptions:**
```python
def parse_numbers(strings):
    """Parse list of strings to numbers, skipping invalid ones."""
    results = []
    errors = []

    for s in strings:
        try:
            results.append(int(s))
        except ValueError:
            errors.append(f"Could not parse: {s}")

    return results, errors

numbers, errors = parse_numbers(["1", "2", "abc", "4", "xyz"])
print(f"Numbers: {numbers}")  # [1, 2, 4]
print(f"Errors: {errors}")    # ["Could not parse: abc", ...]
```

**Haskell - Maybe/Either:**
```haskell
import Text.Read (readMaybe)

parseNumbers :: [String] -> ([Int], [String])
parseNumbers = foldr process ([], [])
  where
    process s (nums, errs) = case readMaybe s of
        Just n  -> (n : nums, errs)
        Nothing -> (nums, ("Could not parse: " ++ s) : errs)

main :: IO ()
main = do
    let (numbers, errors) = parseNumbers ["1", "2", "abc", "4", "xyz"]
    print numbers  -- [1,2,4]
    print errors   -- ["Could not parse: abc", ...]
```

### Activity 3.2: When to Use What

| Approach | Best For | Drawbacks |
|----------|----------|-----------|
| **Exceptions** | Unexpected errors, deep call stacks | Can be caught anywhere, invisible in types |
| **Maybe** | Optional values, simple absence | No error message |
| **Either** | Recoverable errors with context | More verbose |
| **Return codes** | C-style, simple cases | Easy to ignore |

### Activity 3.3: Error Handling Guidelines

**Do:**
```python
# Catch specific exceptions
try:
    value = int(user_input)
except ValueError:
    print("Please enter a number")

# Fail fast on programmer errors
def process(data):
    assert data is not None, "data cannot be None"

# Document what exceptions a function raises
def parse_json(text):
    """Parse JSON string.

    Raises:
        ValueError: If text is not valid JSON.
    """
```

**Don't:**
```python
# Don't catch everything blindly
try:
    do_something()
except:  # BAD - catches everything, even Ctrl+C!
    pass

# Don't use exceptions for flow control
try:
    return some_dict[key]
except KeyError:
    return default
# Better: return some_dict.get(key, default)

# Don't ignore errors
try:
    important_operation()
except SomeError:
    pass  # BAD - silently swallowing errors
```

### Activity 3.4: Refactoring Exercise

This code has poor error handling. Fix it:

```python
# BAD CODE
def process_user_data(data):
    name = data["name"]
    age = int(data["age"])
    email = data["email"]

    if "@" not in email:
        return None

    return {
        "name": name.strip().title(),
        "age": age,
        "email": email.lower()
    }

# What could go wrong?
# - data might not be a dict
# - "name", "age", "email" keys might be missing
# - age might not be convertible to int
# - name might not be a string
# - email might be None
```

**Your turn:** Rewrite with proper error handling.

### ✅ Checkpoint 3

Verify:
- [ ] Can compare exception vs. return-value approaches
- [ ] Know when to use each
- [ ] Refactored the bad code

---

## Part 4: Real-World Scenarios (15 minutes)

### Activity 4.1: API Request Handler

```python
import json
from dataclasses import dataclass

@dataclass
class User:
    id: int
    name: str
    email: str

class APIError(Exception):
    """Base class for API errors."""
    pass

class NotFoundError(APIError):
    """Resource not found."""
    pass

class ValidationError(APIError):
    """Invalid input data."""
    pass

def parse_user_request(json_data):
    """Parse user data from JSON request."""
    try:
        data = json.loads(json_data)
    except json.JSONDecodeError as e:
        raise ValidationError(f"Invalid JSON: {e}")

    if not isinstance(data, dict):
        raise ValidationError("Expected JSON object")

    required_fields = ["id", "name", "email"]
    for field in required_fields:
        if field not in data:
            raise ValidationError(f"Missing field: {field}")

    try:
        user_id = int(data["id"])
    except (ValueError, TypeError):
        raise ValidationError("ID must be an integer")

    return User(
        id=user_id,
        name=str(data["name"]),
        email=str(data["email"])
    )

def handle_request(json_data):
    """Handle API request with proper error responses."""
    try:
        user = parse_user_request(json_data)
        return {"status": "success", "user": user}
    except ValidationError as e:
        return {"status": "error", "message": str(e), "code": 400}
    except APIError as e:
        return {"status": "error", "message": str(e), "code": 500}
```

### Activity 4.2: File Processor

```python
from pathlib import Path
from typing import List, Tuple

def process_data_file(filepath: str) -> Tuple[List[int], List[str]]:
    """
    Read a file of numbers, one per line.
    Returns (valid_numbers, error_messages).
    """
    path = Path(filepath)
    numbers = []
    errors = []

    if not path.exists():
        errors.append(f"File not found: {filepath}")
        return numbers, errors

    if not path.is_file():
        errors.append(f"Not a file: {filepath}")
        return numbers, errors

    try:
        with open(path, 'r') as f:
            for line_num, line in enumerate(f, 1):
                line = line.strip()
                if not line:
                    continue  # Skip empty lines

                try:
                    numbers.append(int(line))
                except ValueError:
                    errors.append(f"Line {line_num}: Invalid number '{line}'")

    except PermissionError:
        errors.append(f"Permission denied: {filepath}")
    except IOError as e:
        errors.append(f"IO error reading file: {e}")

    return numbers, errors

# Usage
numbers, errors = process_data_file("data.txt")
if errors:
    print("Warnings:", errors)
print(f"Processed {len(numbers)} numbers")
```

---

## Challenges

### Challenge 1: Result Type in Python

Implement a Result type similar to Haskell's Either:

```python
from dataclasses import dataclass
from typing import TypeVar, Generic, Callable

T = TypeVar('T')
E = TypeVar('E')

@dataclass
class Result(Generic[T, E]):
    # Implement Ok and Err variants
    # Implement map, flat_map (bind), unwrap methods
    pass
```

### Challenge 2: Validation Pipeline

Create a validation pipeline that collects all errors:

```python
def validate_user(data):
    """Return list of all validation errors, or empty list if valid."""
    errors = []
    # Check all fields, collect all errors
    # Don't stop at first error
    return errors
```

---

## Wrap-Up

**Key takeaways:**

1. **Exceptions** for unexpected errors (Python, C++)
2. **Maybe/Optional** for values that might not exist
3. **Either/Result** for errors with context
4. **Be specific** - catch specific exceptions
5. **Fail fast** - don't hide errors
6. **Document** - what errors can a function raise?

**Error handling philosophy:**

- Python: "Easier to ask forgiveness than permission" (EAFP)
- Haskell: "Make illegal states unrepresentable"
- General: "Handle errors at the appropriate level"

**Next lab:** Project Showcase - show how robust your code is!
