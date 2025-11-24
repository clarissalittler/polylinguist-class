# Module: Testing and Debugging

## Learning Objectives

By the end of this module, you will be able to:

1. Write effective unit tests in Python, C++, and Haskell
2. Practice test-driven development (TDD)
3. Debug code systematically using various techniques
4. Use debugging tools effectively across languages
5. Recognize and fix common bug patterns
6. Write testable code
7. Apply testing and debugging across paradigms (imperative, OOP, functional)

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

**3. Property-Based Testing** (especially Haskell)
- Test properties that should always hold
- Framework generates test cases automatically

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

## Part 3: Unit Testing in C++

### Using Google Test

**Installation (with CMake):**

```cmake
# CMakeLists.txt
cmake_minimum_required(VERSION 3.14)
project(MyProject)

include(FetchContent)
FetchContent_Declare(
  googletest
  URL https://github.com/google/googletest/archive/release-1.12.1.zip
)
FetchContent_MakeAvailable(googletest)

enable_testing()

add_executable(calculator_test test_calculator.cpp)
target_link_libraries(calculator_test gtest_main)

include(GoogleTest)
gtest_discover_tests(calculator_test)
```

### Basic Test Example

```cpp
// calculator.h
#ifndef CALCULATOR_H
#define CALCULATOR_H

#include <stdexcept>

class Calculator {
public:
    int add(int a, int b) {
        return a + b;
    }

    int subtract(int a, int b) {
        return a - b;
    }

    double divide(int a, int b) {
        if (b == 0) {
            throw std::invalid_argument("Cannot divide by zero");
        }
        return static_cast<double>(a) / b;
    }
};

#endif
```

```cpp
// test_calculator.cpp
#include <gtest/gtest.h>
#include "calculator.h"

TEST(CalculatorTest, Add) {
    Calculator calc;
    EXPECT_EQ(calc.add(2, 3), 5);
    EXPECT_EQ(calc.add(-1, 1), 0);
    EXPECT_EQ(calc.add(0, 0), 0);
}

TEST(CalculatorTest, Subtract) {
    Calculator calc;
    EXPECT_EQ(calc.subtract(5, 3), 2);
    EXPECT_EQ(calc.subtract(3, 5), -2);
}

TEST(CalculatorTest, Divide) {
    Calculator calc;
    EXPECT_DOUBLE_EQ(calc.divide(6, 2), 3.0);
    EXPECT_DOUBLE_EQ(calc.divide(5, 2), 2.5);
}

TEST(CalculatorTest, DivideByZero) {
    Calculator calc;
    EXPECT_THROW(calc.divide(5, 0), std::invalid_argument);
}
```

**Running tests:**
```bash
mkdir build && cd build
cmake ..
make
ctest
# Or run directly:
./calculator_test
```

### Using Catch2 (Alternative)

```cpp
// test_calculator_catch.cpp
#define CATCH_CONFIG_MAIN
#include <catch2/catch.hpp>
#include "calculator.h"

TEST_CASE("Calculator addition", "[calculator]") {
    Calculator calc;

    REQUIRE(calc.add(2, 3) == 5);
    REQUIRE(calc.add(-1, 1) == 0);
    REQUIRE(calc.add(0, 0) == 0);
}

TEST_CASE("Calculator division by zero", "[calculator]") {
    Calculator calc;

    REQUIRE_THROWS_AS(calc.divide(5, 0), std::invalid_argument);
}

TEST_CASE("Parametrized addition", "[calculator]") {
    Calculator calc;

    auto [a, b, expected] = GENERATE(table<int, int, int>({
        {2, 3, 5},
        {0, 0, 0},
        {-1, 1, 0}
    }));

    REQUIRE(calc.add(a, b) == expected);
}
```

---

## Part 4: Unit Testing in Haskell

### Using HUnit

**Installation:**
```bash
stack install HUnit
# or add to .cabal file
build-depends: base, HUnit
```

### Basic Test Example

```haskell
-- Calculator.hs
module Calculator where

add :: Int -> Int -> Int
add a b = a + b

subtract' :: Int -> Int -> Int
subtract' a b = a - b

divide :: Int -> Int -> Maybe Double
divide _ 0 = Nothing
divide a b = Just (fromIntegral a / fromIntegral b)
```

```haskell
-- TestCalculator.hs
import Test.HUnit
import Calculator

testAdd :: Test
testAdd = TestCase $ do
    assertEqual "add 2 3" 5 (add 2 3)
    assertEqual "add -1 1" 0 (add (-1) 1)
    assertEqual "add 0 0" 0 (add 0 0)

testSubtract :: Test
testSubtract = TestCase $ do
    assertEqual "subtract 5 3" 2 (subtract' 5 3)
    assertEqual "subtract 3 5" (-2) (subtract' 3 5)

testDivide :: Test
testDivide = TestCase $ do
    assertEqual "divide 6 2" (Just 3.0) (divide 6 2)
    assertEqual "divide 5 2" (Just 2.5) (divide 5 2)

testDivideByZero :: Test
testDivideByZero = TestCase $
    assertEqual "divide by zero" Nothing (divide 5 0)

tests :: Test
tests = TestList [
    TestLabel "Addition" testAdd,
    TestLabel "Subtraction" testSubtract,
    TestLabel "Division" testDivide,
    TestLabel "Division by zero" testDivideByZero
    ]

main :: IO ()
main = do
    runTestTT tests
    return ()
```

**Running tests:**
```bash
ghc -o test TestCalculator.hs
./test

# Or with stack:
stack test
```

### Using QuickCheck (Property-Based Testing)

```haskell
-- TestCalculatorProps.hs
import Test.QuickCheck
import Calculator

-- Property: addition is commutative
prop_addCommutative :: Int -> Int -> Bool
prop_addCommutative a b = add a b == add b a

-- Property: addition with zero is identity
prop_addIdentity :: Int -> Bool
prop_addIdentity a = add a 0 == a

-- Property: subtraction reverses addition
prop_subtractReverse :: Int -> Int -> Bool
prop_subtractReverse a b = subtract' (add a b) b == a

-- Property: division of a*b by b gives a (for b /= 0)
prop_divideReverse :: Int -> NonZero Int -> Bool
prop_divideReverse a (NonZero b) =
    case divide (a * b) b of
        Just result -> abs (result - fromIntegral a) < 0.001
        Nothing -> False

main :: IO ()
main = do
    putStrLn "Testing addition properties:"
    quickCheck prop_addCommutative
    quickCheck prop_addIdentity

    putStrLn "\nTesting subtraction properties:"
    quickCheck prop_subtractReverse

    putStrLn "\nTesting division properties:"
    quickCheck prop_divideReverse
```

**Running property tests:**
```bash
ghc -o proptest TestCalculatorProps.hs
./proptest
```

**Output:**
```
Testing addition properties:
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.

Testing subtraction properties:
+++ OK, passed 100 tests.

Testing division properties:
+++ OK, passed 100 tests.
```

---

## Part 5: Test-Driven Development (TDD)

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

### TDD Example: FizzBuzz (Python)

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

### TDD Example: FizzBuzz (Haskell with HUnit)

```haskell
-- FizzBuzz.hs
module FizzBuzz where

fizzbuzz :: Int -> String
fizzbuzz n
    | n `mod` 15 == 0 = "FizzBuzz"
    | n `mod` 3 == 0  = "Fizz"
    | n `mod` 5 == 0  = "Buzz"
    | otherwise       = show n
```

```haskell
-- TestFizzBuzz.hs
import Test.HUnit
import FizzBuzz

tests :: Test
tests = TestList [
    "fizzbuzz 1" ~: fizzbuzz 1 ~?= "1",
    "fizzbuzz 2" ~: fizzbuzz 2 ~?= "2",
    "fizzbuzz 3" ~: fizzbuzz 3 ~?= "Fizz",
    "fizzbuzz 5" ~: fizzbuzz 5 ~?= "Buzz",
    "fizzbuzz 15" ~: fizzbuzz 15 ~?= "FizzBuzz",
    "fizzbuzz 30" ~: fizzbuzz 30 ~?= "FizzBuzz"
    ]

main :: IO ()
main = do
    runTestTT tests
    return ()
```

---

## Part 6: Debugging Strategies

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

---

## Part 7: Debugging Techniques

### Print Debugging

**Python:**
```python
def calculate_total(items):
    print(f"DEBUG: items = {items}")

    total = 0
    for item in items:
        print(f"DEBUG: Processing item = {item}")
        total += item['price'] * item['quantity']
        print(f"DEBUG: total so far = {total}")

    print(f"DEBUG: Final total = {total}")
    return total
```

**C++:**
```cpp
#include <iostream>

double calculateTotal(const std::vector<Item>& items) {
    std::cout << "DEBUG: Processing " << items.size() << " items" << std::endl;

    double total = 0;
    for (const auto& item : items) {
        std::cout << "DEBUG: item price=" << item.price
                  << ", quantity=" << item.quantity << std::endl;
        total += item.price * item.quantity;
        std::cout << "DEBUG: total=" << total << std::endl;
    }

    return total;
}
```

**Haskell:**
```haskell
import Debug.Trace

calculateTotal :: [(String, Double, Int)] -> Double
calculateTotal items = trace ("DEBUG: items = " ++ show items) $
    sum [price * fromIntegral qty | (name, price, qty) <- items]

-- Or more controlled:
calculateTotal' :: [(String, Double, Int)] -> Double
calculateTotal' items =
    let result = sum [trace ("Processing: " ++ name) (price * fromIntegral qty)
                     | (name, price, qty) <- items]
    in trace ("Final total: " ++ show result) result
```

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
# (Pdb) c  # Continue execution
```

**C++: gdb**

```bash
# Compile with debug symbols
g++ -g program.cpp -o program

# Run with gdb
gdb ./program

# GDB commands:
# (gdb) break main          # Set breakpoint
# (gdb) run                 # Start program
# (gdb) next                # Next line
# (gdb) step                # Step into function
# (gdb) print variable      # Print variable
# (gdb) continue            # Continue execution
# (gdb) backtrace           # Show call stack
```

**Haskell: GHCi debugger**

```haskell
-- Load in GHCi
ghci MyModule.hs

-- Set breakpoint
:break myFunction

-- Run code
:trace myFunction args

-- Debug commands:
-- :step         -- Step into
-- :steplocal    -- Step over
-- :continue     -- Continue
-- :print var    -- Print variable
-- :back         -- Go back in history
```

---

## Part 8: Common Bug Patterns

### 1. Off-by-One Errors

```python
# Bug: Misses last element
for i in range(len(arr) - 1):  # Should be len(arr)
    print(arr[i])

# Correct
for item in arr:
    print(item)
```

```cpp
// Bug: Index out of bounds
for (int i = 0; i <= vec.size(); i++) {  // Should be <
    std::cout << vec[i];
}

// Correct
for (const auto& item : vec) {
    std::cout << item;
}
```

```haskell
-- Bug: Pattern matching error
head' :: [a] -> a
head' (x:xs) = x
-- Missing case for empty list!

-- Correct
head' :: [a] -> Maybe a
head' [] = Nothing
head' (x:xs) = Just x
```

### 2. Null/Nil/Nothing Errors

**Python:**
```python
# Bug: Assumes function returns list
result = get_users()
for user in result:  # Crashes if result is None!
    print(user)

# Fix
result = get_users()
if result is not None:
    for user in result:
        print(user)
```

**C++:**
```cpp
// Bug: Dereferencing null pointer
int* ptr = nullptr;
std::cout << *ptr << std::endl;  // Crash!

// Fix: Check before dereferencing
if (ptr != nullptr) {
    std::cout << *ptr << std::endl;
}

// Better: Use smart pointers
std::unique_ptr<int> ptr = std::make_unique<int>(42);
```

**Haskell:**
```haskell
-- Bug: Pattern matching on Maybe without handling Nothing
getValue :: Maybe Int -> Int
getValue (Just x) = x
-- Missing Nothing case!

-- Correct
getValue :: Maybe Int -> Int
getValue (Just x) = x
getValue Nothing = 0  -- Or use default

-- Better: Use maybe or fromMaybe
getValue' :: Maybe Int -> Int
getValue' = fromMaybe 0
```

### 3. Type Errors

**Python:**
```python
# Bug: String concatenation with int
age = 25
message = "Age: " + age  # TypeError!

# Fix
message = "Age: " + str(age)
# Or use f-string
message = f"Age: {age}"
```

**C++:**
```cpp
// Bug: Integer division
double result = 5 / 2;  // result = 2.0, not 2.5!

// Fix
double result = 5.0 / 2.0;
// Or cast
double result = static_cast<double>(5) / 2;
```

**Haskell:**
```haskell
-- Bug: Type mismatch (caught at compile time!)
add :: Int -> Int -> Int
add x y = x + y

result = add 5 2.5  -- Compile error!

-- Haskell's strong typing prevents this at compile time
```

---

## Part 9: Writing Testable Code

### Principles of Testable Code

**1. Pure Functions (especially important in Haskell)**

```python
# Bad: Depends on global state
total = 0
def add_to_total(x):
    global total
    total += x

# Good: Pure function
def add(a, b):
    return a + b
```

```haskell
-- Pure functions are natural in Haskell
add :: Int -> Int -> Int
add x y = x + y

-- Impure function (marked with IO)
addAndPrint :: Int -> Int -> IO Int
addAndPrint x y = do
    let result = x + y
    putStrLn $ "Result: " ++ show result
    return result
```

**2. Dependency Injection**

```cpp
// Bad: Hard to test (depends on real database)
class UserService {
    Database db;  // Hard-coded dependency
public:
    User getUser(int id) {
        return db.query(id);
    }
};

// Good: Inject dependency
class UserService {
    Database* db;
public:
    UserService(Database* database) : db(database) {}

    User getUser(int id) {
        return db->query(id);
    }
};

// Can now test with mock database
```

**3. Single Responsibility**

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

---

## Part 10: Test Coverage

### What is Test Coverage?

**Coverage** = Percentage of code executed by tests

**Python:**
```bash
pip install pytest-cov

# Run tests with coverage
pytest --cov=myproject --cov-report=html

# View coverage report
open htmlcov/index.html
```

**C++:**
```bash
# Compile with coverage flags
g++ -fprofile-arcs -ftest-coverage test.cpp -o test

# Run tests
./test

# Generate coverage report
gcov test.cpp
lcov --capture --directory . --output-file coverage.info
genhtml coverage.info --output-directory coverage_html
```

**Haskell:**
```bash
# With stack
stack test --coverage

# With cabal
cabal test --enable-coverage
```

### Coverage Goals

- **80%+ coverage** is generally good
- **100% coverage doesn't mean no bugs!**
- Focus on **critical paths** and **complex logic**

---

## Practical Example: Complete TDD Workflow

### Python: Bank Account

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
```

```python
# bank_account.py
class BankAccount:
    def __init__(self):
        self.balance = 0

    def deposit(self, amount):
        if amount <= 0:
            raise ValueError("Deposit amount must be positive")
        self.balance += amount

    def withdraw(self, amount):
        if amount <= 0:
            raise ValueError("Withdrawal amount must be positive")
        if amount > self.balance:
            raise ValueError("Insufficient funds")
        self.balance -= amount
```

### Haskell: Bank Account

```haskell
-- BankAccount.hs
module BankAccount where

data Account = Account { balance :: Double } deriving (Eq, Show)

newAccount :: Account
newAccount = Account 0

deposit :: Double -> Account -> Either String Account
deposit amount acc
    | amount <= 0 = Left "Deposit amount must be positive"
    | otherwise = Right $ acc { balance = balance acc + amount }

withdraw :: Double -> Account -> Either String Account
withdraw amount acc
    | amount <= 0 = Left "Withdrawal amount must be positive"
    | amount > balance acc = Left "Insufficient funds"
    | otherwise = Right $ acc { balance = balance acc - amount }
```

```haskell
-- TestBankAccount.hs
import Test.HUnit
import BankAccount

testNewAccount :: Test
testNewAccount = TestCase $
    assertEqual "new account has zero balance" 0 (balance newAccount)

testDeposit :: Test
testDeposit = TestCase $ do
    let Right acc = deposit 100 newAccount
    assertEqual "deposit increases balance" 100 (balance acc)

testWithdraw :: Test
testWithdraw = TestCase $ do
    let Right acc1 = deposit 100 newAccount
        Right acc2 = withdraw 30 acc1
    assertEqual "withdraw decreases balance" 70 (balance acc2)

testInsufficientFunds :: Test
testInsufficientFunds = TestCase $ do
    let Right acc = deposit 50 newAccount
        result = withdraw 100 acc
    case result of
        Left _ -> return ()
        Right _ -> assertFailure "Expected error for insufficient funds"

tests :: Test
tests = TestList [
    TestLabel "New account" testNewAccount,
    TestLabel "Deposit" testDeposit,
    TestLabel "Withdraw" testWithdraw,
    TestLabel "Insufficient funds" testInsufficientFunds
    ]

main :: IO ()
main = do
    runTestTT tests
    return ()
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
6. **Functional programming helps** - Pure functions are easier to test

---

## Resources

**Python Testing:**
- pytest documentation: https://docs.pytest.org/
- unittest (built-in): https://docs.python.org/3/library/unittest.html

**C++ Testing:**
- Google Test: https://google.github.io/googletest/
- Catch2: https://github.com/catchorg/Catch2

**Haskell Testing:**
- HUnit: https://hackage.haskell.org/package/HUnit
- QuickCheck: https://hackage.haskell.org/package/QuickCheck
- Hspec: https://hspec.github.io/

**General:**
- "Test Driven Development: By Example" by Kent Beck
- "The Art of Debugging" by Norman Matloff

**Remember:** Good developers test their code. Great developers debug systematically!
