# Module: Testing and Debugging

## Learning Objectives

By the end of this module, you will be able to:

1. Write effective unit tests for your code
2. Practice test-driven development (TDD)
3. Debug code systematically using various techniques
4. Use debugging tools effectively
5. Recognize and fix common bug patterns
6. Write testable code
7. Apply testing and debugging across multiple languages

## Why Testing and Debugging Matter

**The Reality:**
- Professional developers spend **more time debugging than writing new code**
- Bugs found in production are **10-100x more expensive** to fix than bugs caught early
- **Untested code is legacy code** the moment you write it

**Benefits of Testing:**
- ✅ Catch bugs early (cheaper to fix)
- ✅ Confidence when refactoring
- ✅ Documentation of expected behavior
- ✅ Faster development (less time debugging)
- ✅ Better code design (testable code is often better code)

---

## Part 1: Testing Fundamentals

### Types of Testing

**1. Unit Testing**
- Test individual functions/methods in isolation
- Fast, automated, run frequently
- **Most common type you'll write**

**2. Integration Testing**
- Test how components work together
- Database connections, API calls, etc.

**3. System/End-to-End Testing**
- Test entire application flow
- Simulates real user behavior

**4. Manual Testing**
- Human testing the application
- Still important for UX, exploratory testing

### Anatomy of a Good Test

```python
def test_addition():
    # 1. ARRANGE: Set up test data
    a = 2
    b = 3

    # 2. ACT: Call the function
    result = add(a, b)

    # 3. ASSERT: Verify the result
    assert result == 5
```

**Good tests are:**
- **Fast** - Run quickly so you run them often
- **Independent** - Don't depend on other tests
- **Repeatable** - Same result every time
- **Self-checking** - Pass or fail automatically
- **Timely** - Written at the right time (ideally before or with code)

---

## Part 2: Unit Testing in Python

### Using pytest

**Installation:**
```bash
pip install pytest
```

### Basic Test Example

```python
# calculator.py
def add(a, b):
    return a + b

def subtract(a, b):
    return a - b

def divide(a, b):
    if b == 0:
        raise ValueError("Cannot divide by zero")
    return a / b
```

```python
# test_calculator.py
import pytest
from calculator import add, subtract, divide

def test_add():
    assert add(2, 3) == 5
    assert add(-1, 1) == 0
    assert add(0, 0) == 0

def test_subtract():
    assert subtract(5, 3) == 2
    assert subtract(3, 5) == -2
    assert subtract(0, 0) == 0

def test_divide():
    assert divide(6, 2) == 3
    assert divide(5, 2) == 2.5

def test_divide_by_zero():
    with pytest.raises(ValueError):
        divide(5, 0)
```

**Running tests:**
```bash
pytest test_calculator.py
pytest test_calculator.py -v  # Verbose output
pytest test_calculator.py::test_add  # Run specific test
```

### Testing Edge Cases

```python
# string_utils.py
def reverse_string(s):
    return s[::-1]

def is_palindrome(s):
    s = s.lower().replace(" ", "")
    return s == s[::-1]
```

```python
# test_string_utils.py
from string_utils import reverse_string, is_palindrome

def test_reverse_string():
    # Normal case
    assert reverse_string("hello") == "olleh"

    # Edge cases
    assert reverse_string("") == ""  # Empty string
    assert reverse_string("a") == "a"  # Single character
    assert reverse_string("12321") == "12321"  # Palindrome

def test_is_palindrome():
    # Palindromes
    assert is_palindrome("racecar") == True
    assert is_palindrome("A man a plan a canal Panama") == True

    # Not palindromes
    assert is_palindrome("hello") == False

    # Edge cases
    assert is_palindrome("") == True  # Empty is palindrome
    assert is_palindrome("a") == True  # Single char is palindrome
```

### Parametrized Tests

```python
import pytest

@pytest.mark.parametrize("a,b,expected", [
    (2, 3, 5),
    (0, 0, 0),
    (-1, 1, 0),
    (100, 200, 300),
    (-5, -3, -8),
])
def test_add_parametrized(a, b, expected):
    assert add(a, b) == expected
```

### Fixtures (Setup and Teardown)

```python
import pytest

@pytest.fixture
def sample_list():
    """Fixture that provides a sample list for tests"""
    return [1, 2, 3, 4, 5]

def test_list_length(sample_list):
    assert len(sample_list) == 5

def test_list_sum(sample_list):
    assert sum(sample_list) == 15

@pytest.fixture
def temp_file(tmp_path):
    """Fixture that creates a temporary file"""
    file_path = tmp_path / "test.txt"
    file_path.write_text("Hello, World!")
    return file_path

def test_file_reading(temp_file):
    content = temp_file.read_text()
    assert content == "Hello, World!"
```

---

## Part 3: Unit Testing in Java

### Using JUnit 5

```java
// Calculator.java
public class Calculator {
    public int add(int a, int b) {
        return a + b;
    }

    public int subtract(int a, int b) {
        return a - b;
    }

    public double divide(int a, int b) {
        if (b == 0) {
            throw new IllegalArgumentException("Cannot divide by zero");
        }
        return (double) a / b;
    }
}
```

```java
// CalculatorTest.java
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import static org.junit.jupiter.api.Assertions.*;

class CalculatorTest {
    private Calculator calculator;

    @BeforeEach
    void setUp() {
        calculator = new Calculator();
    }

    @Test
    void testAdd() {
        assertEquals(5, calculator.add(2, 3));
        assertEquals(0, calculator.add(-1, 1));
        assertEquals(0, calculator.add(0, 0));
    }

    @Test
    void testSubtract() {
        assertEquals(2, calculator.subtract(5, 3));
        assertEquals(-2, calculator.subtract(3, 5));
    }

    @Test
    void testDivide() {
        assertEquals(3.0, calculator.divide(6, 2), 0.001);
        assertEquals(2.5, calculator.divide(5, 2), 0.001);
    }

    @Test
    void testDivideByZero() {
        assertThrows(IllegalArgumentException.class, () -> {
            calculator.divide(5, 0);
        });
    }
}
```

---

## Part 4: Test-Driven Development (TDD)

### The TDD Cycle: Red-Green-Refactor

```
1. RED: Write a failing test
   ↓
2. GREEN: Write minimal code to pass
   ↓
3. REFACTOR: Improve the code
   ↓
   Repeat
```

### TDD Example: FizzBuzz

**Step 1: Write failing test**
```python
# test_fizzbuzz.py
from fizzbuzz import fizzbuzz

def test_fizzbuzz_1():
    assert fizzbuzz(1) == "1"
```

**Step 2: Write minimal code to pass**
```python
# fizzbuzz.py
def fizzbuzz(n):
    return "1"  # Hardcoded to pass the test
```

**Step 3: Add more tests, refactor**
```python
# test_fizzbuzz.py
def test_fizzbuzz_returns_number_as_string():
    assert fizzbuzz(1) == "1"
    assert fizzbuzz(2) == "2"

def test_fizzbuzz_returns_fizz_for_multiples_of_3():
    assert fizzbuzz(3) == "Fizz"
    assert fizzbuzz(6) == "Fizz"

def test_fizzbuzz_returns_buzz_for_multiples_of_5():
    assert fizzbuzz(5) == "Buzz"
    assert fizzbuzz(10) == "Buzz"

def test_fizzbuzz_returns_fizzbuzz_for_multiples_of_15():
    assert fizzbuzz(15) == "FizzBuzz"
    assert fizzbuzz(30) == "FizzBuzz"
```

**Final implementation:**
```python
# fizzbuzz.py
def fizzbuzz(n):
    if n % 15 == 0:
        return "FizzBuzz"
    if n % 3 == 0:
        return "Fizz"
    if n % 5 == 0:
        return "Buzz"
    return str(n)
```

### Benefits of TDD

✅ **Forces you to think about requirements first**
✅ **Creates tests automatically** (you don't forget)
✅ **Prevents over-engineering** (write only what's needed)
✅ **Provides instant feedback**
✅ **Builds confidence** in your code

---

## Part 5: Debugging Strategies

### 1. The Scientific Method

```
1. OBSERVE: What's the bug? What's happening?
2. HYPOTHESIZE: Why might this be happening?
3. PREDICT: If my hypothesis is true, what would I see?
4. TEST: Check if prediction matches reality
5. CONCLUDE: Was hypothesis correct?
6. REPEAT: If not, form new hypothesis
```

### 2. Reproduce the Bug

**Before fixing, ensure you can:**
- Trigger the bug consistently
- Describe the exact steps
- Know what should happen vs. what does happen

### 3. Isolate the Problem

**Binary search debugging:**
```python
# Comment out half the code
# Does bug still occur?
#   YES → Bug is in remaining code
#   NO  → Bug is in commented code
# Repeat with the problematic half
```

### 4. Check Your Assumptions

**Common faulty assumptions:**
- "This function always returns a list" (it might return None!)
- "The user will enter a number" (they might enter text!)
- "The file exists" (it might not!)

```python
# Bad: Assumes file exists
with open('data.txt') as f:
    content = f.read()

# Good: Check assumption
import os
if os.path.exists('data.txt'):
    with open('data.txt') as f:
        content = f.read()
else:
    print("File not found!")
```

---

## Part 6: Debugging Techniques

### Print Debugging

**Simple but effective:**

```python
def calculate_total(items):
    print(f"DEBUG: items = {items}")  # What are we getting?

    total = 0
    for item in items:
        print(f"DEBUG: Processing item = {item}")  # Each iteration
        total += item['price'] * item['quantity']
        print(f"DEBUG: total so far = {total}")

    print(f"DEBUG: Final total = {total}")
    return total
```

**Pro tips:**
- Use a prefix like "DEBUG:" to find them later
- Print variable types: `print(f"x = {x}, type = {type(x)}")`
- Print before and after suspicious operations
- Remove or comment out when done

### Logging (Better than Print)

```python
import logging

logging.basicConfig(level=logging.DEBUG)
logger = logging.getLogger(__name__)

def process_data(data):
    logger.debug(f"Processing {len(data)} items")

    for item in data:
        logger.info(f"Processing item: {item}")
        try:
            result = complex_operation(item)
            logger.debug(f"Result: {result}")
        except Exception as e:
            logger.error(f"Error processing {item}: {e}")

    logger.info("Processing complete")
```

**Advantages over print:**
- Can turn on/off by level (DEBUG, INFO, WARNING, ERROR)
- Can redirect to files
- Includes timestamps
- Can keep in production code

### Using a Debugger

**Python: pdb**

```python
import pdb

def buggy_function(x, y):
    pdb.set_trace()  # Execution pauses here
    result = x / y
    return result

# When set_trace() hits:
# (Pdb) print(x)  # Examine variables
# (Pdb) print(y)
# (Pdb) n  # Next line
# (Pdb) s  # Step into function
# (Pdb) c  # Continue execution
# (Pdb) q  # Quit debugger
```

**Common debugger commands:**
- `n` (next) - Execute next line
- `s` (step) - Step into function call
- `c` (continue) - Continue until next breakpoint
- `p variable` - Print variable value
- `pp variable` - Pretty-print variable
- `l` (list) - Show current code
- `q` (quit) - Exit debugger

### Rubber Duck Debugging

**Method:** Explain your code line-by-line to a rubber duck (or friend)

**Why it works:**
- Forces you to articulate your assumptions
- Often spot the bug while explaining
- Slows you down to think clearly

**Example:**
```
"So this function takes a list... wait, what if the list is empty?
Oh! That's why it's crashing. I need to check for empty list first!"
```

### Reading Error Messages

**Anatomy of an error:**

```python
Traceback (most recent call last):
  File "script.py", line 15, in <module>
    result = divide(10, 0)
  File "script.py", line 8, in divide
    return a / b
ZeroDivisionError: division by zero
```

**How to read:**
1. **Type of error:** `ZeroDivisionError`
2. **Error message:** "division by zero"
3. **Stack trace:** Shows function call chain
4. **Line numbers:** Where error occurred

**Read from bottom up:**
- Bottom = where error actually happened
- Top = where you called the problematic code

---

## Part 7: Common Bug Patterns

### 1. Off-by-One Errors

```python
# Bug: Misses last element
for i in range(len(arr) - 1):  # Oops! Should be len(arr)
    print(arr[i])

# Bug: Index out of bounds
for i in range(len(arr) + 1):  # Oops! One too many
    print(arr[i])

# Correct
for i in range(len(arr)):
    print(arr[i])

# Even better
for item in arr:
    print(item)
```

### 2. Mutable Default Arguments

```python
# Bug: List is shared across all calls!
def add_item(item, lst=[]):
    lst.append(item)
    return lst

print(add_item(1))  # [1]
print(add_item(2))  # [1, 2] - Oops!

# Fix: Use None as default
def add_item(item, lst=None):
    if lst is None:
        lst = []
    lst.append(item)
    return lst
```

### 3. Integer Division

```python
# Python 2 vs Python 3
result = 5 / 2

# Python 2: result = 2 (integer division)
# Python 3: result = 2.5 (float division)

# Explicit integer division (both versions)
result = 5 // 2  # Always 2

# Explicit float division
result = 5 / 2  # Python 3: 2.5
result = 5.0 / 2  # Python 2: 2.5
```

### 4. Comparing Floats

```python
# Bug: Float comparison
if 0.1 + 0.2 == 0.3:  # False! (floating point precision)
    print("Equal")

# Fix: Use tolerance
import math
if math.isclose(0.1 + 0.2, 0.3):
    print("Equal")

# Or manually
epsilon = 1e-9
if abs((0.1 + 0.2) - 0.3) < epsilon:
    print("Equal")
```

### 5. Variable Scope

```python
# Bug: Variable not defined
def increment():
    count += 1  # Error: count not defined in function scope

count = 0
increment()

# Fix 1: Use global (not recommended)
def increment():
    global count
    count += 1

# Fix 2: Pass and return (better)
def increment(count):
    return count + 1

count = increment(count)
```

### 6. Modifying List While Iterating

```python
# Bug: Skips elements
numbers = [1, 2, 3, 4, 5]
for num in numbers:
    if num % 2 == 0:
        numbers.remove(num)  # Modifying while iterating!

# Fix 1: Iterate over copy
for num in numbers[:]:
    if num % 2 == 0:
        numbers.remove(num)

# Fix 2: List comprehension (best)
numbers = [num for num in numbers if num % 2 != 0]
```

---

## Part 8: Writing Testable Code

### Principles of Testable Code

**1. Single Responsibility**
```python
# Bad: Does too much
def process_user_data(user_id):
    # Get from database
    # Validate
    # Transform
    # Save to file
    # Send email
    pass

# Good: Separate concerns
def get_user(user_id):
    pass

def validate_user(user):
    pass

def save_user(user):
    pass
```

**2. Dependency Injection**
```python
# Bad: Hard to test (depends on real database)
def get_user_name(user_id):
    db = Database()  # Hard-coded dependency
    user = db.get_user(user_id)
    return user.name

# Good: Inject dependency (easy to test with mock)
def get_user_name(user_id, db):
    user = db.get_user(user_id)
    return user.name

# Test with mock database
def test_get_user_name():
    mock_db = MockDatabase()
    name = get_user_name(1, mock_db)
    assert name == "Alice"
```

**3. Pure Functions**
```python
# Bad: Depends on external state
total = 0
def add_to_total(x):
    global total
    total += x

# Good: Pure function (easier to test)
def add(a, b):
    return a + b

total = add(total, x)
```

**4. Avoid Side Effects**
```python
# Bad: Side effect (modifies input)
def process(items):
    items.append("processed")
    return items

# Good: Return new list
def process(items):
    return items + ["processed"]
```

---

## Part 9: Test Coverage

### What is Test Coverage?

**Coverage** = Percentage of code executed by tests

```bash
# Python: Install coverage tool
pip install pytest-cov

# Run tests with coverage
pytest --cov=myproject --cov-report=html

# View coverage report
open htmlcov/index.html
```

### Coverage Goals

- **80%+ coverage** is generally good
- **100% coverage doesn't mean no bugs!**
- Focus on **critical paths** and **complex logic**
- Don't test trivial getters/setters

### Example Coverage Report

```
Name                Stmts   Miss  Cover
---------------------------------------
calculator.py          10      2    80%
string_utils.py        15      0   100%
---------------------------------------
TOTAL                  25      2    92%
```

---

## Part 10: Best Practices

### Testing Best Practices

1. **Test one thing at a time**
   ```python
   # Bad
   def test_everything():
       assert add(2, 3) == 5
       assert subtract(5, 3) == 2
       assert multiply(2, 3) == 6

   # Good
   def test_add():
       assert add(2, 3) == 5

   def test_subtract():
       assert subtract(5, 3) == 2
   ```

2. **Use descriptive test names**
   ```python
   # Bad
   def test_1():
       pass

   # Good
   def test_add_returns_sum_of_two_positive_numbers():
       pass

   def test_add_handles_negative_numbers():
       pass
   ```

3. **Test edge cases**
   - Empty inputs
   - Null/None values
   - Negative numbers
   - Very large numbers
   - Boundary values

4. **Keep tests independent**
   - Each test should run in isolation
   - Don't rely on test execution order
   - Clean up after yourself

5. **Make tests fast**
   - Unit tests should be milliseconds
   - Use mocks for slow operations (database, network)
   - Run full test suite frequently

### Debugging Best Practices

1. **Start with the error message**
   - Read it carefully
   - Google the exact error message
   - Check stack trace

2. **Reproduce reliably**
   - Know exact steps to trigger bug
   - Automate reproduction if possible

3. **Use version control**
   - Commit working code
   - Easy to revert if you break something
   - Compare with working version

4. **Take breaks**
   - Step away if stuck
   - Fresh perspective helps
   - Explain to someone else

5. **Keep a debugging log**
   - What you tried
   - What you learned
   - Patterns you notice

---

## Practical Example: Complete TDD Workflow

### Requirement: Bank Account Class

```python
# test_bank_account.py
import pytest
from bank_account import BankAccount

class TestBankAccount:
    def test_initial_balance_is_zero(self):
        account = BankAccount()
        assert account.balance == 0

    def test_deposit_increases_balance(self):
        account = BankAccount()
        account.deposit(100)
        assert account.balance == 100

    def test_multiple_deposits(self):
        account = BankAccount()
        account.deposit(100)
        account.deposit(50)
        assert account.balance == 150

    def test_withdraw_decreases_balance(self):
        account = BankAccount()
        account.deposit(100)
        account.withdraw(30)
        assert account.balance == 70

    def test_withdraw_more_than_balance_raises_error(self):
        account = BankAccount()
        account.deposit(50)
        with pytest.raises(ValueError):
            account.withdraw(100)

    def test_deposit_negative_amount_raises_error(self):
        account = BankAccount()
        with pytest.raises(ValueError):
            account.deposit(-50)

    def test_withdraw_negative_amount_raises_error(self):
        account = BankAccount()
        with pytest.raises(ValueError):
            account.withdraw(-50)

    def test_get_transaction_history(self):
        account = BankAccount()
        account.deposit(100)
        account.withdraw(30)
        history = account.get_history()
        assert len(history) == 2
        assert history[0] == ('deposit', 100)
        assert history[1] == ('withdraw', 30)
```

```python
# bank_account.py
class BankAccount:
    def __init__(self):
        self.balance = 0
        self.history = []

    def deposit(self, amount):
        if amount <= 0:
            raise ValueError("Deposit amount must be positive")
        self.balance += amount
        self.history.append(('deposit', amount))

    def withdraw(self, amount):
        if amount <= 0:
            raise ValueError("Withdrawal amount must be positive")
        if amount > self.balance:
            raise ValueError("Insufficient funds")
        self.balance -= amount
        self.history.append(('withdraw', amount))

    def get_history(self):
        return self.history.copy()
```

---

## Summary

### Testing Checklist

- ✅ Write tests for new code
- ✅ Test edge cases and error conditions
- ✅ Run tests before committing
- ✅ Aim for 80%+ coverage on critical code
- ✅ Keep tests fast and independent

### Debugging Checklist

- ✅ Read error messages carefully
- ✅ Reproduce the bug reliably
- ✅ Form and test hypotheses systematically
- ✅ Use appropriate debugging tools
- ✅ Take breaks when stuck

### Key Insights

1. **Testing saves time** - Catches bugs early
2. **TDD improves design** - Forces you to think about interfaces
3. **Debugging is a skill** - Gets better with practice
4. **Read error messages** - They tell you what's wrong
5. **Testable code is better code** - Looser coupling, clearer responsibilities

---

## Resources

**Python Testing:**
- pytest documentation: https://docs.pytest.org/
- unittest (built-in): https://docs.python.org/3/library/unittest.html

**Java Testing:**
- JUnit 5: https://junit.org/junit5/
- Mockito: https://site.mockito.org/

**General:**
- "Test Driven Development: By Example" by Kent Beck
- "The Art of Debugging" by Norman Matloff

**Practice:**
- Write tests for all code you write
- Debug deliberately and systematically
- Learn from every bug you encounter

**Remember:** Good developers test their code. Great developers debug systematically!
