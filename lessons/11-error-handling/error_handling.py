#!/usr/bin/env python3
"""
Lesson 11: Error Handling & Debugging in Python

Python uses exceptions for error handling:
- try/except/else/finally blocks
- Multiple exception handlers
- Custom exceptions
- Context managers for resource management
- Assertions for invariants
- Logging for debugging

This demonstrates Python's error handling capabilities.
"""

import logging
import sys
from typing import Optional, List
from contextlib import contextmanager

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(levelname)s: %(message)s'
)
logger = logging.getLogger(__name__)

print("=== Error Handling in Python ===\n")

# ====================
# 1. Basic try/except
# ====================

print("1. Basic try/except:")

def divide(x: float, y: float) -> float:
    """Divide two numbers with error handling."""
    try:
        result = x / y
        return result
    except ZeroDivisionError:
        print(f"   Error: Cannot divide {x} by zero!")
        return float('inf')
    except TypeError as e:
        print(f"   Error: Invalid types - {e}")
        return float('nan')

print(f"   divide(10, 2) = {divide(10, 2)}")
print(f"   divide(10, 0) = {divide(10, 0)}")
print(f"   divide('10', 2) = {divide('10', 2)}")

# ====================
# 2. Multiple except clauses
# ====================

print("\n2. Multiple Exception Handlers:")

def safe_list_access(lst: List, index: int) -> Optional[any]:
    """Safely access list element."""
    try:
        return lst[index]
    except IndexError:
        print(f"   Error: Index {index} out of range")
        return None
    except TypeError:
        print(f"   Error: Invalid list or index type")
        return None

numbers = [1, 2, 3, 4, 5]
print(f"   numbers[2] = {safe_list_access(numbers, 2)}")
print(f"   numbers[10] = {safe_list_access(numbers, 10)}")

# ====================
# 3. try/except/else/finally
# ====================

print("\n3. try/except/else/finally:")

def process_file(filename: str) -> bool:
    """Process a file with complete error handling."""
    file = None
    try:
        print(f"   Opening {filename}...")
        file = open(filename, 'r')
        content = file.read()
        print(f"   Read {len(content)} characters")
    except FileNotFoundError:
        print(f"   Error: File '{filename}' not found")
        return False
    except PermissionError:
        print(f"   Error: No permission to read '{filename}'")
        return False
    else:
        print(f"   Successfully processed '{filename}'")
        return True
    finally:
        if file:
            file.close()
            print(f"   Closed file")
        print(f"   Cleanup complete")

# Create a test file
with open('/tmp/test.txt', 'w') as f:
    f.write("Hello, World!")

process_file('/tmp/test.txt')
process_file('/nonexistent/file.txt')

# ====================
# 4. Raising exceptions
# ====================

print("\n4. Raising Exceptions:")

def validate_age(age: int) -> None:
    """Validate age with custom error messages."""
    if not isinstance(age, int):
        raise TypeError(f"Age must be an integer, got {type(age).__name__}")
    if age < 0:
        raise ValueError(f"Age cannot be negative, got {age}")
    if age > 150:
        raise ValueError(f"Age {age} seems unrealistic")
    print(f"   Valid age: {age}")

try:
    validate_age(25)
    validate_age(-5)
except ValueError as e:
    print(f"   Caught: {e}")

# ====================
# 5. Custom exceptions
# ====================

print("\n5. Custom Exceptions:")

class ValidationError(Exception):
    """Base class for validation errors."""
    pass

class InvalidEmailError(ValidationError):
    """Raised when email validation fails."""
    def __init__(self, email: str, reason: str):
        self.email = email
        self.reason = reason
        super().__init__(f"Invalid email '{email}': {reason}")

class InvalidPasswordError(ValidationError):
    """Raised when password validation fails."""
    def __init__(self, reason: str):
        self.reason = reason
        super().__init__(f"Invalid password: {reason}")

def validate_email(email: str) -> None:
    """Validate email address."""
    if '@' not in email:
        raise InvalidEmailError(email, "missing @ symbol")
    if '.' not in email.split('@')[1]:
        raise InvalidEmailError(email, "missing domain extension")
    print(f"   Valid email: {email}")

def validate_password(password: str) -> None:
    """Validate password strength."""
    if len(password) < 8:
        raise InvalidPasswordError("must be at least 8 characters")
    if not any(c.isdigit() for c in password):
        raise InvalidPasswordError("must contain at least one digit")
    print(f"   Valid password: {'*' * len(password)}")

try:
    validate_email("user@example.com")
    validate_email("invalid-email")
except InvalidEmailError as e:
    print(f"   Caught: {e}")

try:
    validate_password("SecurePass123")
    validate_password("short")
except InvalidPasswordError as e:
    print(f"   Caught: {e}")

# ====================
# 6. Context managers (with statement)
# ====================

print("\n6. Context Managers (Resource Management):")

@contextmanager
def timer(name: str):
    """Context manager for timing operations."""
    import time
    start = time.time()
    print(f"   Starting {name}...")
    try:
        yield
    finally:
        elapsed = time.time() - start
        print(f"   {name} took {elapsed:.4f} seconds")

with timer("example operation"):
    sum(range(1000000))

# Custom resource manager
class DatabaseConnection:
    """Simulated database connection."""

    def __init__(self, db_name: str):
        self.db_name = db_name
        self.connected = False

    def __enter__(self):
        print(f"   Connecting to {self.db_name}...")
        self.connected = True
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        print(f"   Closing connection to {self.db_name}")
        self.connected = False
        # Return False to propagate exceptions
        return False

    def execute(self, query: str):
        if not self.connected:
            raise RuntimeError("Not connected to database")
        print(f"   Executing: {query}")

with DatabaseConnection("users_db") as db:
    db.execute("SELECT * FROM users")

# ====================
# 7. Assertions
# ====================

print("\n7. Assertions:")

def calculate_average(numbers: List[float]) -> float:
    """Calculate average with assertions."""
    assert len(numbers) > 0, "Cannot calculate average of empty list"
    assert all(isinstance(n, (int, float)) for n in numbers), "All items must be numbers"

    avg = sum(numbers) / len(numbers)
    assert avg >= min(numbers) and avg <= max(numbers), "Average should be between min and max"

    print(f"   Average of {numbers} = {avg}")
    return avg

calculate_average([1, 2, 3, 4, 5])

try:
    calculate_average([])
except AssertionError as e:
    print(f"   Assertion failed: {e}")

# ====================
# 8. Logging
# ====================

print("\n8. Logging:")

def complex_operation(x: int) -> int:
    """Operation with logging at different levels."""
    logger.debug(f"Starting operation with x={x}")

    try:
        if x < 0:
            logger.warning(f"Negative input: {x}")
            x = abs(x)

        result = x * 2 + 10
        logger.info(f"Successfully computed result: {result}")
        return result

    except Exception as e:
        logger.error(f"Operation failed: {e}")
        raise

complex_operation(5)
complex_operation(-3)

# ====================
# 9. Exception chaining
# ====================

print("\n9. Exception Chaining:")

def parse_config(data: str) -> dict:
    """Parse configuration with chained exceptions."""
    try:
        import json
        return json.loads(data)
    except json.JSONDecodeError as e:
        raise ValueError("Invalid configuration format") from e

try:
    parse_config("{invalid json}")
except ValueError as e:
    print(f"   Error: {e}")
    print(f"   Caused by: {e.__cause__}")

# ====================
# 10. Error recovery
# ====================

print("\n10. Error Recovery:")

def robust_operation(items: List[any]) -> List[any]:
    """Process items with error recovery."""
    results = []

    for i, item in enumerate(items):
        try:
            # Try to process
            result = int(item) * 2
            results.append(result)
            logger.debug(f"Processed item {i}: {item} -> {result}")
        except (ValueError, TypeError) as e:
            logger.warning(f"Skipping item {i} ({item}): {e}")
            # Continue with next item
            continue

    print(f"   Successfully processed {len(results)}/{len(items)} items")
    return results

items = ['1', '2', 'invalid', '4', None, '6']
results = robust_operation(items)
print(f"   Results: {results}")

# ====================
# 11. Retry pattern
# ====================

print("\n11. Retry Pattern:")

def retry(func, max_attempts: int = 3, exceptions=(Exception,)):
    """Retry a function on failure."""
    for attempt in range(1, max_attempts + 1):
        try:
            print(f"   Attempt {attempt}/{max_attempts}")
            return func()
        except exceptions as e:
            if attempt == max_attempts:
                print(f"   All attempts failed!")
                raise
            print(f"   Failed: {e}. Retrying...")

# Simulated unreliable operation
attempt_count = 0
def unreliable_operation():
    global attempt_count
    attempt_count += 1
    if attempt_count < 3:
        raise ConnectionError("Network timeout")
    return "Success!"

attempt_count = 0
try:
    result = retry(unreliable_operation)
    print(f"   Result: {result}")
except ConnectionError as e:
    print(f"   Final error: {e}")

# ====================
# 12. Suppressing exceptions
# ====================

print("\n12. Suppressing Exceptions (contextlib):")

from contextlib import suppress

# Instead of try/except pass
with suppress(FileNotFoundError):
    print("   Trying to remove file...")
    import os
    os.remove('/nonexistent/file.txt')
    print("   This won't print if file doesn't exist")

print("   Continued execution")

# ====================
# Summary
# ====================

print("\n=== Python Error Handling Features ===")
print("- try/except/else/finally for exception handling")
print("- Multiple exception handlers")
print("- Custom exception classes")
print("- Context managers (with statement)")
print("- Assertions for invariants")
print("- Logging at multiple levels")
print("- Exception chaining (from clause)")
print("- Error recovery and retry patterns")
print("- EAFP philosophy: Easier to Ask Forgiveness than Permission")
