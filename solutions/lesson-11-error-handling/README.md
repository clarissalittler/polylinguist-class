# Lesson 11: Error Handling & Debugging - Solution Guide

This guide provides example solutions for the Error Handling exercises.

## General Notes

- **Error handling**: Managing unexpected conditions gracefully
- **Exceptions vs Returns**: Two main approaches
- **Fail fast vs Recover**: When to stop, when to continue
- **User-friendly**: Provide helpful error messages
- **Defensive programming**: Validate inputs, check preconditions

---

## Exercise 1: Safe Division

**Task:** Implement division with comprehensive error handling

### Python Solution

```python
from typing import Union

def safe_divide(a: float, b: float) -> Union[float, str]:
    """Safe division with error handling"""
    # Check types
    if not isinstance(a, (int, float)) or not isinstance(b, (int, float)):
        return "Error: Both arguments must be numbers"

    # Check for division by zero
    if b == 0:
        return "Error: Division by zero"

    # Perform division
    try:
        result = a / b

        # Check for overflow/infinity
        if result == float('inf') or result == float('-inf'):
            return "Error: Result is infinity (overflow)"

        return result
    except OverflowError:
        return "Error: Numeric overflow"

# Test
print(safe_divide(10, 2))       # 5.0
print(safe_divide(10, 0))       # Error: Division by zero
print(safe_divide("10", 2))     # Error: Both arguments must be numbers
print(safe_divide(1e308, 1e-308))  # Check overflow

# Using exceptions
class DivisionError(Exception):
    pass

def safe_divide_exception(a: float, b: float) -> float:
    """Raises exceptions for errors"""
    if not isinstance(a, (int, float)) or not isinstance(b, (int, float)):
        raise TypeError("Both arguments must be numbers")

    if b == 0:
        raise DivisionError("Division by zero")

    return a / b

# Test with try/except
try:
    result = safe_divide_exception(10, 0)
except DivisionError as e:
    print(f"Caught error: {e}")
```

### Rust Solution

```rust
#[derive(Debug)]
enum DivisionError {
    DivisionByZero,
    Overflow,
}

fn safe_divide(a: f64, b: f64) -> Result<f64, DivisionError> {
    if b == 0.0 {
        return Err(DivisionError::DivisionByZero);
    }

    let result = a / b;

    if result.is_infinite() {
        return Err(DivisionError::Overflow);
    }

    Ok(result)
}

fn main() {
    match safe_divide(10.0, 2.0) {
        Ok(result) => println!("Result: {}", result),
        Err(e) => println!("Error: {:?}", e),
    }

    match safe_divide(10.0, 0.0) {
        Ok(result) => println!("Result: {}", result),
        Err(e) => println!("Error: {:?}", e),  // DivisionByZero
    }
}
```

### Haskell Solution

```haskell
data DivisionError = DivisionByZero | Overflow
    deriving (Show)

safeDivide :: Double -> Double -> Either DivisionError Double
safeDivide _ 0 = Left DivisionByZero
safeDivide a b
    | isInfinite result = Left Overflow
    | otherwise = Right result
  where
    result = a / b

main :: IO ()
main = do
    print $ safeDivide 10 2    -- Right 5.0
    print $ safeDivide 10 0    -- Left DivisionByZero
```

**Key Insights:**
- Check for edge cases before operating
- Return Result/Either for functional error handling
- Use exceptions for exceptional cases
- Provide specific error messages

---

## Exercise 2: Safe List Access

**Task:** Safely access list elements

### Python Solution

```python
from typing import Optional, List, TypeVar

T = TypeVar('T')

def safe_get(lst: List[T], index: int) -> Optional[T]:
    """Safely get list element"""
    # Handle negative indices (Python supports them)
    if index < 0:
        index = len(lst) + index

    # Check bounds
    if index < 0 or index >= len(lst):
        return None

    return lst[index]

# Test
numbers = [1, 2, 3, 4, 5]
print(safe_get(numbers, 0))     # 1
print(safe_get(numbers, -1))    # 5 (last element)
print(safe_get(numbers, 10))    # None (out of bounds)
print(safe_get([], 0))          # None (empty list)
```

### Rust Solution

```rust
fn safe_get<T: Clone>(list: &[T], index: isize) -> Option<T> {
    // Handle negative indices
    let actual_index = if index < 0 {
        let len = list.len() as isize;
        if -index > len {
            return None;
        }
        (len + index) as usize
    } else {
        index as usize
    };

    // Bounds check
    list.get(actual_index).cloned()
}

fn main() {
    let numbers = vec![1, 2, 3, 4, 5];

    println!("{:?}", safe_get(&numbers, 0));    // Some(1)
    println!("{:?}", safe_get(&numbers, -1));   // Some(5)
    println!("{:?}", safe_get(&numbers, 10));   // None
}
```

### Haskell Solution

```haskell
safeGet :: [a] -> Int -> Maybe a
safeGet [] _ = Nothing
safeGet (x:_) 0 = Just x
safeGet (_:xs) n
    | n < 0     = Nothing
    | otherwise = safeGet xs (n - 1)

-- Using built-in indexing with bounds check
safeGet' :: [a] -> Int -> Maybe a
safeGet' xs n
    | n < 0 || n >= length xs = Nothing
    | otherwise = Just (xs !! n)

main :: IO ()
main = do
    let numbers = [1, 2, 3, 4, 5]
    print $ safeGet numbers 0    -- Just 1
    print $ safeGet numbers 10   -- Nothing
```

**Key Insights:**
- Return Option/Maybe for operations that can fail
- Handle negative indices explicitly
- Check bounds before accessing
- Empty collections always return None

---

## Exercise 4: File Reader

**Task:** Read file with comprehensive error handling

### Python Solution

```python
from typing import Optional
import os

def safe_read_file(filename: str) -> Optional[str]:
    """Safely read file with error handling"""
    try:
        with open(filename, 'r', encoding='utf-8') as file:
            content = file.read()
            return content
    except FileNotFoundError:
        print(f"Error: File '{filename}' not found")
        return None
    except PermissionError:
        print(f"Error: Permission denied for '{filename}'")
        return None
    except UnicodeDecodeError:
        print(f"Error: Unable to decode '{filename}' as UTF-8")
        return None
    except Exception as e:
        print(f"Unexpected error: {e}")
        return None
    # 'with' statement ensures file is closed

# Test
content = safe_read_file("example.txt")
if content:
    print(f"File content: {content}")
else:
    print("Failed to read file")
```

### Rust Solution

```rust
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
enum FileError {
    NotFound,
    PermissionDenied,
    EncodingError,
    Other(String),
}

fn safe_read_file(filename: &str) -> Result<String, FileError> {
    let mut file = File::open(filename).map_err(|e| match e.kind() {
        io::ErrorKind::NotFound => FileError::NotFound,
        io::ErrorKind::PermissionDenied => FileError::PermissionDenied,
        _ => FileError::Other(e.to_string()),
    })?;

    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .map_err(|_| FileError::EncodingError)?;

    Ok(contents)
}

fn main() {
    match safe_read_file("example.txt") {
        Ok(content) => println!("File content: {}", content),
        Err(e) => println!("Error reading file: {:?}", e),
    }
}
```

### JavaScript Solution

```javascript
const fs = require('fs').promises;

async function safeReadFile(filename) {
    try {
        const content = await fs.readFile(filename, 'utf8');
        return { success: true, content };
    } catch (error) {
        if (error.code === 'ENOENT') {
            return { success: false, error: 'File not found' };
        } else if (error.code === 'EACCES') {
            return { success: false, error: 'Permission denied' };
        } else {
            return { success: false, error: error.message };
        }
    }
}

// Usage
safeReadFile('example.txt')
    .then(result => {
        if (result.success) {
            console.log('File content:', result.content);
        } else {
            console.log('Error:', result.error);
        }
    });
```

**Key Insights:**
- Multiple exception types for different errors
- Use `with`/RAII to ensure resource cleanup
- Specific error messages help debugging
- Always close files properly

---

## Exercise 9: Result Type

**Task:** Implement Result<T, E> type

### Python Solution

```python
from typing import TypeVar, Generic, Callable

T = TypeVar('T')
E = TypeVar('E')
U = TypeVar('U')

class Result(Generic[T, E]):
    """Result type for error handling"""

    def __init__(self, is_ok: bool, value=None, error=None):
        self._is_ok = is_ok
        self._value = value
        self._error = error

    @staticmethod
    def ok(value: T) -> 'Result[T, E]':
        return Result(True, value=value)

    @staticmethod
    def err(error: E) -> 'Result[T, E]':
        return Result(False, error=error)

    def is_ok(self) -> bool:
        return self._is_ok

    def is_err(self) -> bool:
        return not self._is_ok

    def unwrap(self) -> T:
        """Get value or raise exception"""
        if self._is_ok:
            return self._value
        raise Exception(f"Called unwrap on error: {self._error}")

    def unwrap_or(self, default: T) -> T:
        """Get value or return default"""
        return self._value if self._is_ok else default

    def map(self, f: Callable[[T], U]) -> 'Result[U, E]':
        """Transform success value"""
        if self._is_ok:
            return Result.ok(f(self._value))
        return Result.err(self._error)

    def flat_map(self, f: Callable[[T], 'Result[U, E]']) -> 'Result[U, E]':
        """Chain operations that return Result"""
        if self._is_ok:
            return f(self._value)
        return Result.err(self._error)

# Test
def divide(a: float, b: float) -> Result[float, str]:
    if b == 0:
        return Result.err("Division by zero")
    return Result.ok(a / b)

result = divide(10, 2)
print(result.unwrap())  # 5.0

error_result = divide(10, 0)
print(error_result.unwrap_or(0))  # 0

# Chaining
result_chain = (divide(10, 2)
    .map(lambda x: x * 2)
    .map(lambda x: x + 1))
print(result_chain.unwrap())  # 11.0
```

### Rust (Built-in)

```rust
fn divide(a: f64, b: f64) -> Result<f64, String> {
    if b == 0.0 {
        return Err("Division by zero".to_string());
    }
    Ok(a / b)
}

fn main() {
    let result = divide(10.0, 2.0);
    println!("{}", result.unwrap());  // 5.0

    let error_result = divide(10.0, 0.0);
    println!("{}", error_result.unwrap_or(0.0));  // 0.0

    // Chaining
    let chain = divide(10.0, 2.0)
        .map(|x| x * 2.0)
        .map(|x| x + 1.0);
    println!("{}", chain.unwrap());  // 11.0
}
```

**Key Insights:**
- Result type makes errors explicit
- Forces caller to handle errors
- Chainable with map/flat_map
- Better than exceptions for expected errors

---

## Exercise 11: Retry Logic

**Task:** Implement retry with exponential backoff

### Python Solution

```python
import time
from typing import Callable, TypeVar

T = TypeVar('T')

def retry_with_backoff(
    func: Callable[[], T],
    max_attempts: int = 3,
    initial_delay: float = 1.0,
    backoff_factor: float = 2.0
) -> T:
    """Retry function with exponential backoff"""
    delay = initial_delay

    for attempt in range(max_attempts):
        try:
            return func()
        except Exception as e:
            if attempt == max_attempts - 1:
                # Last attempt, re-raise
                raise

            print(f"Attempt {attempt + 1} failed: {e}")
            print(f"Retrying in {delay} seconds...")
            time.sleep(delay)
            delay *= backoff_factor

# Test
attempt_count = 0

def flaky_operation():
    global attempt_count
    attempt_count += 1
    if attempt_count < 3:
        raise Exception("Temporary failure")
    return "Success!"

result = retry_with_backoff(flaky_operation, max_attempts=5)
print(f"Result: {result}")
```

### JavaScript Solution

```javascript
async function retryWithBackoff(
    func,
    maxAttempts = 3,
    initialDelay = 1000,
    backoffFactor = 2
) {
    let delay = initialDelay;

    for (let attempt = 0; attempt < maxAttempts; attempt++) {
        try {
            return await func();
        } catch (error) {
            if (attempt === maxAttempts - 1) {
                throw error;  // Last attempt
            }

            console.log(`Attempt ${attempt + 1} failed: ${error.message}`);
            console.log(`Retrying in ${delay}ms...`);

            await new Promise(resolve => setTimeout(resolve, delay));
            delay *= backoffFactor;
        }
    }
}

// Test
let attemptCount = 0;
async function flakyOperation() {
    attemptCount++;
    if (attemptCount < 3) {
        throw new Error('Temporary failure');
    }
    return 'Success!';
}

retryWithBackoff(flakyOperation, 5)
    .then(result => console.log('Result:', result))
    .catch(error => console.error('Failed:', error));
```

**Key Insights:**
- Exponential backoff reduces load during failures
- Limit max attempts to avoid infinite loops
- Re-throw on final failure
- Useful for network operations

---

## Exercise 12: Batch Processing

**Task:** Process list, continue on errors

### Python Solution

```python
from typing import List, Tuple, Callable, TypeVar

T = TypeVar('T')
U = TypeVar('U')

def batch_process(
    items: List[T],
    processor: Callable[[T], U]
) -> Tuple[List[U], List[Tuple[T, Exception]]]:
    """Process all items, collecting successes and failures"""
    successes = []
    failures = []

    for item in items:
        try:
            result = processor(item)
            successes.append(result)
        except Exception as e:
            failures.append((item, e))

    return successes, failures

# Test
def risky_operation(x: int) -> int:
    if x == 0:
        raise ValueError("Cannot process zero")
    return 100 / x

items = [1, 2, 0, 4, 5]
successes, failures = batch_process(items, risky_operation)

print(f"Successes: {successes}")  # [100.0, 50.0, 25.0, 20.0]
print(f"Failures: {len(failures)}")  # 1
for item, error in failures:
    print(f"  Failed on {item}: {error}")
```

### Rust Solution

```rust
fn batch_process<T, U, F>(
    items: Vec<T>,
    processor: F,
) -> (Vec<U>, Vec<(T, String)>)
where
    T: Clone,
    F: Fn(&T) -> Result<U, String>,
{
    let mut successes = Vec::new();
    let mut failures = Vec::new();

    for item in items {
        match processor(&item) {
            Ok(result) => successes.push(result),
            Err(e) => failures.push((item, e)),
        }
    }

    (successes, failures)
}

fn main() {
    let items = vec![1, 2, 0, 4, 5];

    let (successes, failures) = batch_process(items, |&x| {
        if x == 0 {
            Err("Cannot process zero".to_string())
        } else {
            Ok(100 / x)
        }
    });

    println!("Successes: {:?}", successes);
    println!("Failures: {} items", failures.len());
}
```

**Key Insights:**
- Don't let one failure stop everything
- Collect both successes and failures
- Return structured results
- Useful for batch jobs and migrations

---

## Summary

Error handling is critical for robust software:

**Core Concepts:**
- **Exceptions**: For exceptional, unexpected errors
- **Return values**: For expected errors (Result/Either)
- **Option types**: For absence of value
- **Defensive programming**: Validate early
- **Error recovery**: Retry, fallback, degrade

**Error Handling Strategies:**
- **Fail fast**: Stop on first error
- **Fail safe**: Continue with defaults
- **Fail gracefully**: Degrade functionality
- **Fail loudly**: Log and alert

**Best Practices:**
- Use specific exception types
- Provide helpful error messages
- Include context in errors
- Clean up resources (finally/defer)
- Log errors appropriately
- Don't swallow exceptions silently

**Language Approaches:**
- **Python**: Exceptions with try/except
- **Rust**: Result<T, E> for explicit errors
- **Haskell**: Either/Maybe for pure errors
- **Go**: Multiple return values (value, error)
- **Java**: Checked and unchecked exceptions

**Patterns:**
- **Retry**: Try again on transient failures
- **Circuit breaker**: Stop trying after failures
- **Fallback**: Use alternative on failure
- **Timeout**: Don't wait forever
- **Graceful degradation**: Reduce functionality

Understanding error handling makes your software more reliable and maintainable!
