# Lesson 11: Error Handling & Debugging

## Introduction

Errors are inevitable in programming. The difference between a novice and an expert isn't that experts write bug-free code—it's that experts know how to **handle errors gracefully** and **debug efficiently**.

This lesson covers two critical practical skills:
1. **Error Handling**: How to catch, handle, and propagate errors
2. **Debugging**: How to find and fix bugs in your code

## Why Error Handling Matters

1. **Robustness**: Programs should handle errors gracefully, not crash
2. **User Experience**: Good error messages help users fix problems
3. **Maintainability**: Clear error handling makes code easier to debug
4. **Correctness**: Explicit error handling forces you to think about edge cases
5. **Recovery**: Programs can recover from errors and continue running

## Types of Errors

### 1. Syntax Errors
Mistakes in the code structure that prevent compilation/parsing.

```python
# Python
if x > 5  # Missing colon - SyntaxError
    print("big")
```

**Fix**: Read the error message, find the line, fix the syntax.

### 2. Runtime Errors
Errors that occur during execution.

```python
# Python
x = 10 / 0  # ZeroDivisionError at runtime
```

**Fix**: Handle with try/except, validate inputs, use defensive programming.

### 3. Logic Errors
Code runs but produces wrong results.

```python
# Python
def calculate_average(numbers):
    return sum(numbers) / (len(numbers) + 1)  # Bug: should not add 1
```

**Fix**: Use debugging techniques, tests, and careful reasoning.

### 4. Type Errors
Using values of the wrong type.

```python
# Python
"hello" + 5  # TypeError: can't concatenate str and int
```

**Fix**: Use type checking, validation, or static typing.

## Error Handling by Language

### Python: try/except/finally

```python
try:
    result = 10 / 0
except ZeroDivisionError:
    print("Cannot divide by zero!")
except Exception as e:
    print(f"Error: {e}")
else:
    print("Success!")
finally:
    print("This always runs")
```

**Key features:**
- `try`: Code that might raise an exception
- `except`: Handle specific exceptions
- `else`: Runs if no exception occurred
- `finally`: Always runs (cleanup code)
- `raise`: Throw an exception

### JavaScript: try/catch/finally

```javascript
try {
    const result = riskyOperation();
} catch (error) {
    console.error("Error:", error.message);
} finally {
    console.log("Cleanup");
}

// Throwing errors
throw new Error("Something went wrong");
throw new TypeError("Expected a number");
```

**Async error handling:**
```javascript
// Promises
fetchData()
    .then(data => processData(data))
    .catch(error => console.error(error));

// Async/await
async function loadData() {
    try {
        const data = await fetchData();
        return processData(data);
    } catch (error) {
        console.error("Failed to load:", error);
    }
}
```

### Java: try/catch/finally + Checked Exceptions

```java
try {
    FileReader file = new FileReader("data.txt");
    // use file
} catch (FileNotFoundException e) {
    System.err.println("File not found: " + e.getMessage());
} catch (IOException e) {
    System.err.println("IO error: " + e.getMessage());
} finally {
    // cleanup
}

// Try-with-resources (Java 7+)
try (FileReader file = new FileReader("data.txt")) {
    // use file - automatically closed
} catch (IOException e) {
    System.err.println("Error: " + e.getMessage());
}
```

**Checked vs Unchecked:**
- **Checked**: Must be declared or caught (IOException, SQLException)
- **Unchecked**: Runtime exceptions (NullPointerException, IllegalArgumentException)

### C: Error Codes and errno

C has no built-in exception handling. Use return codes:

```c
int divide(int a, int b, int* result) {
    if (b == 0) {
        return -1;  // Error code
    }
    *result = a / b;
    return 0;  // Success
}

// Usage
int result;
if (divide(10, 2, &result) != 0) {
    fprintf(stderr, "Error: division failed\n");
    return 1;
}
printf("Result: %d\n", result);

// errno for system calls
FILE* file = fopen("data.txt", "r");
if (file == NULL) {
    perror("Error opening file");
    // or: fprintf(stderr, "Error: %s\n", strerror(errno));
}
```

**Common patterns:**
- Return 0 for success, non-zero for errors
- Use output parameters for results
- Check `errno` after system calls
- Use `perror()` or `strerror()` for error messages

### Ruby: begin/rescue/ensure

```ruby
begin
    result = 10 / 0
rescue ZeroDivisionError => e
    puts "Cannot divide by zero!"
rescue StandardError => e
    puts "Error: #{e.message}"
else
    puts "Success!"
ensure
    puts "This always runs"
end

# Raising exceptions
raise ArgumentError, "Invalid argument"
raise "Something went wrong"

# Inline rescue
result = risky_operation rescue default_value
```

### Haskell: Maybe, Either, and Exceptions

Haskell prefers type-based error handling:

```haskell
-- Maybe for operations that might fail
safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

-- Either for errors with information
divide :: Double -> Double -> Either String Double
divide _ 0 = Left "Division by zero"
divide x y = Right (x / y)

-- Using in do-notation
compute :: Double -> Double -> Either String Double
compute x y = do
    result1 <- divide x y
    result2 <- divide result1 2
    return result2

-- IO exceptions (use sparingly)
import Control.Exception

main :: IO ()
main = catch (readFile "data.txt" >>= print)
             (\e -> putStrLn $ "Error: " ++ show (e :: IOException))
```

**Philosophy**: Make errors explicit in types, avoid exceptions.

### Racket: Exceptions and Contracts

```racket
;; Basic exception handling
(with-handlers ([exn:fail? (lambda (e)
                            (displayln "Error occurred"))])
  (/ 10 0))

;; Raising exceptions
(raise (exn:fail "Something went wrong" (current-continuation-marks)))
(error 'function-name "error message")

;; Contracts (runtime checking)
(define/contract (divide x y)
  (-> number? (and/c number? (not/c zero?)) number?)
  (/ x y))
```

### Rust: Result<T, E> and Option<T>

Rust has no exceptions. Use Result and Option:

```rust
// Result for operations that can fail
fn divide(x: f64, y: f64) -> Result<f64, String> {
    if y == 0.0 {
        Err("Division by zero".to_string())
    } else {
        Ok(x / y)
    }
}

// Using Result
match divide(10.0, 2.0) {
    Ok(result) => println!("Result: {}", result),
    Err(e) => eprintln!("Error: {}", e),
}

// ? operator for propagating errors
fn compute(x: f64, y: f64) -> Result<f64, String> {
    let result1 = divide(x, y)?;  // Returns early if Err
    let result2 = divide(result1, 2.0)?;
    Ok(result2)
}

// Option for nullable values
fn find_user(id: u32) -> Option<User> {
    // Returns Some(user) or None
}

// panic! for unrecoverable errors
if critical_invariant_violated {
    panic!("This should never happen!");
}
```

**Philosophy**: Errors are values, handled explicitly.

### Prolog: Catch/Throw

```prolog
% Catching exceptions
safe_divide(X, Y, Result) :-
    catch(
        Result is X / Y,
        error(evaluation_error(zero_divisor), _),
        (write('Error: Division by zero'), fail)
    ).

% Throwing exceptions
divide(_, 0, _) :-
    throw(error(zero_divisor, context(divide/3, 'Cannot divide by zero'))).
divide(X, Y, Result) :-
    Result is X / Y.

% Using catch/3
compute(X) :-
    catch(
        risky_operation(X),
        Exception,
        handle_error(Exception)
    ).
```

## Common Error Handling Patterns

### 1. Validation Pattern

Check inputs before processing:

```python
def divide(x, y):
    if not isinstance(x, (int, float)):
        raise TypeError("x must be a number")
    if not isinstance(y, (int, float)):
        raise TypeError("y must be a number")
    if y == 0:
        raise ValueError("Cannot divide by zero")
    return x / y
```

### 2. Guard Clauses (Early Return)

```python
def process_user(user):
    if user is None:
        return None
    if not user.is_active:
        return None
    if not user.has_permission:
        return None

    # Main logic here
    return user.process()
```

### 3. Try-Parse Pattern

```python
def safe_parse_int(s):
    try:
        return int(s)
    except ValueError:
        return None
```

```rust
// Rust
fn safe_parse_int(s: &str) -> Option<i32> {
    s.parse().ok()
}
```

### 4. Retry Pattern

```python
def retry(func, max_attempts=3, delay=1):
    for attempt in range(max_attempts):
        try:
            return func()
        except Exception as e:
            if attempt == max_attempts - 1:
                raise
            time.sleep(delay)
```

### 5. Fallback Pattern

```python
def get_config(key, default):
    try:
        return read_config(key)
    except ConfigError:
        return default
```

### 6. Resource Management (RAII)

```python
# Python: context manager
with open('file.txt', 'r') as f:
    content = f.read()
# File automatically closed

# Custom context manager
class DatabaseConnection:
    def __enter__(self):
        self.conn = connect_to_db()
        return self.conn

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.conn.close()
        return False  # Don't suppress exceptions

with DatabaseConnection() as conn:
    conn.execute("SELECT * FROM users")
```

```rust
// Rust: RAII with Drop
{
    let file = File::open("data.txt")?;
    // use file
}  // file automatically closed here (Drop trait)
```

### 7. Result Chaining

```rust
// Rust
fn process_file(path: &str) -> Result<Data, Error> {
    let contents = read_file(path)?;
    let parsed = parse_contents(&contents)?;
    let validated = validate_data(parsed)?;
    Ok(validated)
}
```

```haskell
-- Haskell
processFile :: FilePath -> Either Error Data
processFile path = do
    contents <- readFile path
    parsed <- parseContents contents
    validated <- validateData parsed
    return validated
```

## Custom Exceptions

### Python

```python
class InvalidUserError(Exception):
    """Raised when user validation fails."""

    def __init__(self, username, reason):
        self.username = username
        self.reason = reason
        super().__init__(f"Invalid user '{username}': {reason}")

# Usage
raise InvalidUserError("bob", "insufficient permissions")
```

### Java

```java
public class InvalidUserException extends Exception {
    private final String username;

    public InvalidUserException(String username, String message) {
        super(message);
        this.username = username;
    }

    public String getUsername() {
        return username;
    }
}

// Usage
throw new InvalidUserException("bob", "Insufficient permissions");
```

### Rust

```rust
#[derive(Debug)]
enum UserError {
    NotFound(String),
    InvalidPermissions(String),
    DatabaseError(String),
}

impl std::fmt::Display for UserError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            UserError::NotFound(name) => write!(f, "User not found: {}", name),
            UserError::InvalidPermissions(msg) => write!(f, "Invalid permissions: {}", msg),
            UserError::DatabaseError(msg) => write!(f, "Database error: {}", msg),
        }
    }
}

impl std::error::Error for UserError {}

// Usage
fn find_user(name: &str) -> Result<User, UserError> {
    Err(UserError::NotFound(name.to_string()))
}
```

## Debugging Techniques

### 1. Print Debugging

The simplest technique:

```python
def complex_function(x, y):
    print(f"DEBUG: x={x}, y={y}")  # Add print statements
    result = x * 2 + y
    print(f"DEBUG: result={result}")
    return result
```

**Pros**: Simple, works everywhere
**Cons**: Clutters code, easy to forget to remove

### 2. Logging

Better than print statements:

```python
import logging

logging.basicConfig(level=logging.DEBUG)
logger = logging.getLogger(__name__)

def process_data(data):
    logger.debug(f"Processing data: {data}")
    try:
        result = compute(data)
        logger.info(f"Successfully computed: {result}")
        return result
    except Exception as e:
        logger.error(f"Failed to process: {e}")
        raise
```

**Levels**: DEBUG, INFO, WARNING, ERROR, CRITICAL

### 3. Assertions

Check invariants:

```python
def calculate_average(numbers):
    assert len(numbers) > 0, "Cannot calculate average of empty list"
    assert all(isinstance(n, (int, float)) for n in numbers), "All items must be numbers"
    return sum(numbers) / len(numbers)
```

**Use assertions for**:
- Checking invariants
- Verifying preconditions
- Documenting assumptions

**Don't use assertions for**:
- Input validation (use exceptions)
- Side effects (assertions can be disabled)

### 4. Debugger

Use an interactive debugger:

**Python (pdb)**:
```python
import pdb

def buggy_function(x):
    pdb.set_trace()  # Debugger breaks here
    result = x * 2
    return result
```

**Common debugger commands**:
- `n` (next): Execute next line
- `s` (step): Step into function
- `c` (continue): Continue execution
- `p variable`: Print variable
- `l` (list): Show code
- `q` (quit): Exit debugger

### 5. Stack Traces

Read stack traces bottom-to-top (usually):

```
Traceback (most recent call last):
  File "main.py", line 10, in <module>
    result = calculate()          ← Called from here
  File "main.py", line 6, in calculate
    return divide(10, 0)          ← Which called this
  File "main.py", line 2, in divide
    return x / y                  ← Error happened here
ZeroDivisionError: division by zero
```

**Reading a stack trace**:
1. Find the error type (ZeroDivisionError)
2. Read the error message
3. Look at the bottom frame (where error occurred)
4. Trace back through the call stack

### 6. Binary Search Debugging

For complex bugs:
1. Find a point where code works
2. Find a point where code fails
3. Test the midpoint
4. Repeat until you narrow down the problem

### 7. Rubber Duck Debugging

Explain your code line-by-line to someone (or something):
1. Start at the beginning
2. Explain what each line does
3. Often you'll spot the bug while explaining!

### 8. Git Bisect

Find which commit introduced a bug:
```bash
git bisect start
git bisect bad          # Current version is broken
git bisect good v1.0    # Version 1.0 worked
# Git checks out midpoint
# Test the code
git bisect good  # or git bisect bad
# Repeat until bug is found
```

## Error Messages: Best Practices

### Bad Error Messages

```python
raise Exception("Error")  # What error?
raise ValueError("Invalid")  # Invalid what?
print("Something went wrong")  # No context!
```

### Good Error Messages

```python
raise ValueError(f"Invalid age: {age}. Must be between 0 and 120.")
raise FileNotFoundError(f"Configuration file not found: {config_path}")
raise TypeError(f"Expected list of integers, got {type(data).__name__}")
```

**Good error messages include**:
1. **What went wrong**: "File not found"
2. **Why it's wrong**: "Path does not exist"
3. **What was attempted**: f"Tried to open '{path}'"
4. **How to fix it**: "Check the file path and try again"

## Defensive Programming

Write code that prevents errors:

### 1. Input Validation

```python
def withdraw(account, amount):
    if amount < 0:
        raise ValueError("Amount cannot be negative")
    if amount > account.balance:
        raise ValueError("Insufficient funds")
    account.balance -= amount
```

### 2. Default Values

```python
def greet(name=None):
    name = name or "Guest"  # Provide default
    print(f"Hello, {name}!")
```

### 3. Type Hints (Python)

```python
def calculate_total(items: list[Item]) -> float:
    return sum(item.price for item in items)
```

### 4. Null Checks

```python
if user is not None and user.is_active():
    process_user(user)
```

### 5. Bounds Checking

```python
def get_item(items, index):
    if 0 <= index < len(items):
        return items[index]
    raise IndexError(f"Index {index} out of range [0, {len(items)})")
```

## Common Debugging Scenarios

### Scenario 1: Off-by-One Error

```python
# Bug
for i in range(len(items)):
    if items[i] == items[i+1]:  # IndexError on last iteration!
        ...

# Fix
for i in range(len(items) - 1):  # Stop one before the end
    if items[i] == items[i+1]:
        ...
```

### Scenario 2: Mutable Default Arguments

```python
# Bug
def add_item(item, items=[]):  # [] created once!
    items.append(item)
    return items

# Fix
def add_item(item, items=None):
    if items is None:
        items = []
    items.append(item)
    return items
```

### Scenario 3: Variable Shadowing

```python
# Bug
total = 100
def calculate():
    total = 0  # Creates local variable, doesn't modify global
    for item in items:
        total += item.price
    return total

# Fix
def calculate():
    local_total = 0  # Use different name
    for item in items:
        local_total += item.price
    return local_total
```

### Scenario 4: Integer Division

```python
# Bug (Python 2 style)
average = sum / count  # Integer division in some languages

# Fix
average = sum / float(count)  # Or use / in Python 3
```

## Testing to Prevent Errors

```python
def test_divide():
    # Test normal case
    assert divide(10, 2) == 5

    # Test edge cases
    assert divide(0, 5) == 0
    assert divide(1, 3) == 0.333...

    # Test error cases
    with pytest.raises(ZeroDivisionError):
        divide(10, 0)

    with pytest.raises(TypeError):
        divide("10", 2)
```

## Summary: Error Handling Strategies

| Language | Primary Strategy | Philosophy |
|----------|-----------------|------------|
| Python | Exceptions (try/except) | EAFP: Easier to Ask Forgiveness than Permission |
| JavaScript | Exceptions + Promises | Try/catch + .catch() |
| Java | Checked exceptions | Force handling of errors |
| C | Return codes | Explicit error checking |
| Ruby | Exceptions (rescue) | Similar to Python |
| Haskell | Maybe/Either types | Errors in types, avoid exceptions |
| Racket | Exceptions + Contracts | Runtime checking |
| Rust | Result/Option types | Errors are values, no exceptions |
| Prolog | Catch/throw + failure | Logical failure + exceptions |

## Key Takeaways

1. **Errors will happen**: Plan for them, don't ignore them
2. **Fail fast**: Detect errors early, near their source
3. **Be specific**: Use specific error types and clear messages
4. **Make errors visible**: Don't silently swallow errors
5. **Use types**: When possible, use types to prevent errors
6. **Test error cases**: Test both success and failure paths
7. **Debug systematically**: Use tools and techniques, not guesswork
8. **Learn from errors**: Each bug is a lesson

## Further Reading

- **Books**:
  - "The Pragmatic Programmer" - debugging tips
  - "Code Complete" - error handling strategies
  - "Debug It!" by Paul Butcher

- **Online**:
  - Python logging documentation
  - Rust error handling book
  - JavaScript promise error handling guide

## Next Steps

Practice is essential:
1. Intentionally introduce bugs to practice debugging
2. Write error-prone code, then handle errors properly
3. Use a debugger on real problems
4. Read and understand stack traces
5. Write tests for error cases

With good error handling and debugging skills, you can write robust, maintainable code that users can trust!
