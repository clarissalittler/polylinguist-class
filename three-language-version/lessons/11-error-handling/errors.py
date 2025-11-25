"""
Lesson 11: Error Handling - Python Examples
Python uses exceptions for error handling
"""

# =============================================================================
# BASIC EXCEPTION HANDLING
# =============================================================================

def divide(a, b):
    """Division that can raise an exception"""
    return a / b

print("=== Basic Try/Except ===")
try:
    result = divide(10, 2)
    print(f"10 / 2 = {result}")
except ZeroDivisionError:
    print("Cannot divide by zero!")

try:
    result = divide(10, 0)
    print(f"10 / 0 = {result}")
except ZeroDivisionError:
    print("Cannot divide by zero!")


# =============================================================================
# CATCHING MULTIPLE EXCEPTIONS
# =============================================================================

def risky_operation(value):
    """Might raise different exceptions"""
    if value < 0:
        raise ValueError("Negative value not allowed")
    if value == 0:
        raise ZeroDivisionError("Cannot use zero")
    return 100 / value

print("\n=== Multiple Exception Types ===")
for val in [5, 0, -3, "abc"]:
    try:
        result = risky_operation(val)
        print(f"risky_operation({val}) = {result}")
    except ValueError as e:
        print(f"ValueError: {e}")
    except ZeroDivisionError as e:
        print(f"ZeroDivisionError: {e}")
    except TypeError as e:
        print(f"TypeError: {e}")


# =============================================================================
# EXCEPTION HIERARCHY
# =============================================================================

print("\n=== Exception Hierarchy ===")
try:
    # All these inherit from Exception
    raise FileNotFoundError("file.txt not found")
except IOError as e:
    # FileNotFoundError is a subclass of IOError
    print(f"Caught IOError: {e}")


# =============================================================================
# FINALLY AND CLEANUP
# =============================================================================

def process_file(filename):
    """Demonstrate finally for cleanup"""
    file = None
    try:
        print(f"Opening {filename}...")
        file = open(filename, 'r')
        return file.read()
    except FileNotFoundError:
        print(f"File not found: {filename}")
        return None
    finally:
        # This ALWAYS runs
        if file:
            print("Closing file...")
            file.close()
        print("Cleanup complete")

print("\n=== Finally Block ===")
process_file("nonexistent.txt")


# =============================================================================
# CONTEXT MANAGERS (with statement)
# =============================================================================

print("\n=== Context Managers ===")
try:
    with open("test.txt", "w") as f:
        f.write("Hello, World!")
    print("File written successfully")

    with open("test.txt", "r") as f:
        content = f.read()
        print(f"File contains: {content}")
except IOError as e:
    print(f"IO Error: {e}")


# =============================================================================
# CUSTOM EXCEPTIONS
# =============================================================================

class ValidationError(Exception):
    """Custom exception for validation failures"""
    pass

class InsufficientFundsError(Exception):
    """Custom exception for banking operations"""
    def __init__(self, balance, amount):
        self.balance = balance
        self.amount = amount
        super().__init__(f"Insufficient funds: balance={balance}, requested={amount}")

class BankAccount:
    def __init__(self, balance):
        self.balance = balance

    def withdraw(self, amount):
        if amount < 0:
            raise ValidationError("Cannot withdraw negative amount")
        if amount > self.balance:
            raise InsufficientFundsError(self.balance, amount)
        self.balance -= amount
        return amount

print("\n=== Custom Exceptions ===")
account = BankAccount(100)
try:
    account.withdraw(150)
except InsufficientFundsError as e:
    print(f"Error: {e}")
    print(f"Balance: {e.balance}, Attempted: {e.amount}")


# =============================================================================
# RE-RAISING EXCEPTIONS
# =============================================================================

def process_data(data):
    """Process data, log errors, then re-raise"""
    try:
        return int(data) * 2
    except ValueError:
        print(f"Error processing '{data}' - logging and re-raising")
        raise  # Re-raise the same exception

print("\n=== Re-raising Exceptions ===")
try:
    process_data("not a number")
except ValueError as e:
    print(f"Caught re-raised exception: {e}")


# =============================================================================
# EXCEPTION CHAINING
# =============================================================================

def load_config(filename):
    """Load config, raise custom error if file issues"""
    try:
        with open(filename) as f:
            return f.read()
    except FileNotFoundError as e:
        raise RuntimeError(f"Configuration failed") from e

print("\n=== Exception Chaining ===")
try:
    load_config("config.txt")
except RuntimeError as e:
    print(f"Error: {e}")
    print(f"Original cause: {e.__cause__}")


# =============================================================================
# RESULT-BASED ERROR HANDLING (Alternative to exceptions)
# =============================================================================

from typing import Union, Tuple
from dataclasses import dataclass

@dataclass
class Ok:
    value: any

@dataclass
class Error:
    message: str

Result = Union[Ok, Error]

def safe_divide(a: float, b: float) -> Result:
    """Return Result instead of raising exception"""
    if b == 0:
        return Error("Division by zero")
    return Ok(a / b)

def safe_sqrt(x: float) -> Result:
    """Return Result for square root"""
    if x < 0:
        return Error("Cannot take square root of negative number")
    return Ok(x ** 0.5)

print("\n=== Result-Based Error Handling ===")
result = safe_divide(10, 2)
if isinstance(result, Ok):
    print(f"Success: {result.value}")
else:
    print(f"Error: {result.message}")

result = safe_divide(10, 0)
if isinstance(result, Ok):
    print(f"Success: {result.value}")
else:
    print(f"Error: {result.message}")


# =============================================================================
# ASSERTIONS
# =============================================================================

def calculate_average(numbers):
    """Calculate average with precondition check"""
    assert len(numbers) > 0, "Cannot calculate average of empty list"
    assert all(isinstance(n, (int, float)) for n in numbers), "All elements must be numbers"
    return sum(numbers) / len(numbers)

print("\n=== Assertions ===")
print(f"average([1,2,3,4,5]) = {calculate_average([1,2,3,4,5])}")
try:
    calculate_average([])
except AssertionError as e:
    print(f"AssertionError: {e}")


# =============================================================================
# ELSE CLAUSE
# =============================================================================

print("\n=== Try/Except/Else ===")
for val in [10, 0]:
    try:
        result = 100 / val
    except ZeroDivisionError:
        print(f"Cannot divide by {val}")
    else:
        # Only runs if NO exception occurred
        print(f"100 / {val} = {result}")
