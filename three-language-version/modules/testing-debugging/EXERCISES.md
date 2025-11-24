# Testing & Debugging: Practice Exercises

These exercises help you develop testing and debugging skills in Python, C++, and Haskell.

---

## Part 1: Writing Basic Tests

### Exercise 1.1: Test a Simple Function (Python)

**Given this function:**

```python
def multiply(a, b):
    return a * b
```

**Task:** Write at least 5 test cases using pytest. Consider:
- Normal cases (positive numbers)
- Edge cases (zero, negative numbers)
- Different data types (integers, floats)

<details>
<summary>Solution</summary>

```python
def test_multiply_positive_numbers():
    assert multiply(2, 3) == 6

def test_multiply_negative_numbers():
    assert multiply(-2, -3) == 6
    assert multiply(-2, 3) == -6

def test_multiply_with_zero():
    assert multiply(0, 5) == 0
    assert multiply(5, 0) == 0

def test_multiply_floats():
    assert multiply(2.5, 4) == 10.0

def test_multiply_large_numbers():
    assert multiply(1000, 1000) == 1000000
```
</details>

---

### Exercise 1.2: Test String Functions (C++)

**Given this function:**

```cpp
std::string reverseString(const std::string& s) {
    return std::string(s.rbegin(), s.rend());
}
```

**Task:** Write Google Test cases covering:
1. Normal strings
2. Empty string
3. Single character
4. Strings with spaces

<details>
<summary>Solution</summary>

```cpp
#include <gtest/gtest.h>

TEST(ReverseStringTest, NormalString) {
    EXPECT_EQ(reverseString("hello"), "olleh");
}

TEST(ReverseStringTest, EmptyString) {
    EXPECT_EQ(reverseString(""), "");
}

TEST(ReverseStringTest, SingleCharacter) {
    EXPECT_EQ(reverseString("a"), "a");
}

TEST(ReverseStringTest, WithSpaces) {
    EXPECT_EQ(reverseString("hello world"), "dlrow olleh");
}

TEST(ReverseStringTest, Palindrome) {
    std::string palindrome = "racecar";
    EXPECT_EQ(reverseString(palindrome), palindrome);
}
```
</details>

---

### Exercise 1.3: Test Pure Functions (Haskell)

**Given this function:**

```haskell
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

**Task:** Write HUnit tests for:
1. Base case (0)
2. Small numbers (1, 2, 3)
3. Larger numbers (5, 10)

<details>
<summary>Solution</summary>

```haskell
import Test.HUnit

testFactorialZero :: Test
testFactorialZero = TestCase $ assertEqual "factorial 0" 1 (factorial 0)

testFactorialOne :: Test
testFactorialOne = TestCase $ assertEqual "factorial 1" 1 (factorial 1)

testFactorialSmall :: Test
testFactorialSmall = TestList [
    "factorial 2" ~: 2 ~?= factorial 2,
    "factorial 3" ~: 6 ~?= factorial 3,
    "factorial 4" ~: 24 ~?= factorial 4
    ]

testFactorialLarge :: Test
testFactorialLarge = TestList [
    "factorial 5" ~: 120 ~?= factorial 5,
    "factorial 10" ~: 3628800 ~?= factorial 10
    ]

tests :: Test
tests = TestList [
    TestLabel "Zero" testFactorialZero,
    TestLabel "One" testFactorialOne,
    TestLabel "Small" testFactorialSmall,
    TestLabel "Large" testFactorialLarge
    ]
```
</details>

---

## Part 2: Test-Driven Development (TDD)

### Exercise 2.1: TDD - Password Validator (Python)

**Use TDD to create a password validator with these requirements:**

1. Password must be at least 8 characters long
2. Must contain at least one uppercase letter
3. Must contain at least one lowercase letter
4. Must contain at least one digit
5. Must contain at least one special character (!@#$%^&*)

**TDD Steps:**
1. Write test for length → implement → pass
2. Write test for uppercase → implement → pass
3. Continue for all requirements

<details>
<summary>Solution</summary>

```python
# test_password_validator.py
import pytest
from password_validator import PasswordValidator

class TestPasswordValidator:
    def test_password_minimum_length(self):
        validator = PasswordValidator()
        assert validator.is_valid("Short1!") == False
        assert validator.is_valid("LongEnough1!") == True

    def test_password_requires_uppercase(self):
        validator = PasswordValidator()
        assert validator.is_valid("lowercase1!") == False
        assert validator.is_valid("Uppercase1!") == True

    def test_password_requires_lowercase(self):
        validator = PasswordValidator()
        assert validator.is_valid("UPPERCASE1!") == False
        assert validator.is_valid("Lowercase1!") == True

    def test_password_requires_digit(self):
        validator = PasswordValidator()
        assert validator.is_valid("NoDigits!Aa") == False
        assert validator.is_valid("HasDigit1!Aa") == True

    def test_password_requires_special_char(self):
        validator = PasswordValidator()
        assert validator.is_valid("NoSpecial1Aa") == False
        assert validator.is_valid("HasSpecial1!Aa") == True

# password_validator.py
import re

class PasswordValidator:
    def is_valid(self, password):
        if len(password) < 8:
            return False
        if not re.search(r'[A-Z]', password):
            return False
        if not re.search(r'[a-z]', password):
            return False
        if not re.search(r'\d', password):
            return False
        if not re.search(r'[!@#$%^&*]', password):
            return False
        return True
```
</details>

---

### Exercise 2.2: TDD - Stack Data Structure (C++)

**Use TDD to implement a Stack with:**
1. Push element
2. Pop element
3. Peek at top
4. Check if empty
5. Get size

<details>
<summary>Solution</summary>

```cpp
// test_stack.cpp
#include <gtest/gtest.h>
#include "stack.h"

TEST(StackTest, NewStackIsEmpty) {
    Stack<int> stack;
    EXPECT_TRUE(stack.isEmpty());
    EXPECT_EQ(stack.size(), 0);
}

TEST(StackTest, PushIncreasesSize) {
    Stack<int> stack;
    stack.push(1);
    EXPECT_FALSE(stack.isEmpty());
    EXPECT_EQ(stack.size(), 1);
}

TEST(StackTest, PushAndPeek) {
    Stack<int> stack;
    stack.push(42);
    EXPECT_EQ(stack.peek(), 42);
}

TEST(StackTest, PushAndPop) {
    Stack<int> stack;
    stack.push(1);
    stack.push(2);
    EXPECT_EQ(stack.pop(), 2);
    EXPECT_EQ(stack.pop(), 1);
}

TEST(StackTest, PopEmptyThrows) {
    Stack<int> stack;
    EXPECT_THROW(stack.pop(), std::underflow_error);
}

TEST(StackTest, PeekEmptyThrows) {
    Stack<int> stack;
    EXPECT_THROW(stack.peek(), std::underflow_error);
}

// stack.h
#include <vector>
#include <stdexcept>

template <typename T>
class Stack {
private:
    std::vector<T> data;

public:
    void push(const T& value) {
        data.push_back(value);
    }

    T pop() {
        if (isEmpty()) {
            throw std::underflow_error("Stack is empty");
        }
        T value = data.back();
        data.pop_back();
        return value;
    }

    T peek() const {
        if (isEmpty()) {
            throw std::underflow_error("Stack is empty");
        }
        return data.back();
    }

    bool isEmpty() const {
        return data.empty();
    }

    size_t size() const {
        return data.size();
    }
};
```
</details>

---

### Exercise 2.3: TDD - List Operations (Haskell)

**Use TDD to implement:**
1. `safeHead :: [a] -> Maybe a` - Get first element safely
2. `safeTail :: [a] -> Maybe [a]` - Get tail safely
3. `safeIndex :: [a] -> Int -> Maybe a` - Get element at index

<details>
<summary>Solution</summary>

```haskell
-- TestListOps.hs
import Test.HUnit
import ListOps

testSafeHeadEmpty :: Test
testSafeHeadEmpty = TestCase $
    assertEqual "safeHead []" Nothing (safeHead ([] :: [Int]))

testSafeHeadNonEmpty :: Test
testSafeHeadNonEmpty = TestCase $
    assertEqual "safeHead [1,2,3]" (Just 1) (safeHead [1,2,3])

testSafeTailEmpty :: Test
testSafeTailEmpty = TestCase $
    assertEqual "safeTail []" Nothing (safeTail ([] :: [Int]))

testSafeTailSingle :: Test
testSafeTailSingle = TestCase $
    assertEqual "safeTail [1]" (Just []) (safeTail [1])

testSafeTailMultiple :: Test
testSafeTailMultiple = TestCase $
    assertEqual "safeTail [1,2,3]" (Just [2,3]) (safeTail [1,2,3])

testSafeIndexOutOfBounds :: Test
testSafeIndexOutOfBounds = TestList [
    "negative index" ~: Nothing ~?= safeIndex [1,2,3] (-1),
    "too large index" ~: Nothing ~?= safeIndex [1,2,3] 5
    ]

testSafeIndexValid :: Test
testSafeIndexValid = TestList [
    "index 0" ~: Just 1 ~?= safeIndex [1,2,3] 0,
    "index 1" ~: Just 2 ~?= safeIndex [1,2,3] 1,
    "index 2" ~: Just 3 ~?= safeIndex [1,2,3] 2
    ]

tests :: Test
tests = TestList [
    TestLabel "safeHead empty" testSafeHeadEmpty,
    TestLabel "safeHead non-empty" testSafeHeadNonEmpty,
    TestLabel "safeTail empty" testSafeTailEmpty,
    TestLabel "safeTail single" testSafeTailSingle,
    TestLabel "safeTail multiple" testSafeTailMultiple,
    TestLabel "safeIndex out of bounds" testSafeIndexOutOfBounds,
    TestLabel "safeIndex valid" testSafeIndexValid
    ]

-- ListOps.hs
module ListOps where

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _ = Nothing
safeIndex (x:xs) 0 = Just x
safeIndex (x:xs) n
    | n < 0 = Nothing
    | otherwise = safeIndex xs (n - 1)
```
</details>

---

## Part 3: Debugging Exercises

### Exercise 3.1: Find and Fix - Off by One (Python)

**This function should return the sum of numbers from 1 to n:**

```python
def sum_to_n(n):
    total = 0
    for i in range(n):
        total += i
    return total

print(sum_to_n(5))  # Expected: 15, but returns 10
```

**Tasks:**
1. Identify the bug
2. Explain why it happens
3. Fix the code
4. Write tests to prevent this bug

<details>
<summary>Solution</summary>

**Bug:** `range(n)` generates 0 to n-1, not 1 to n.

**Fix:**
```python
def sum_to_n(n):
    total = 0
    for i in range(1, n + 1):  # Changed from range(n)
        total += i
    return total

# Or use the formula:
def sum_to_n(n):
    return n * (n + 1) // 2

# Tests
def test_sum_to_n():
    assert sum_to_n(1) == 1
    assert sum_to_n(5) == 15
    assert sum_to_n(10) == 55
    assert sum_to_n(0) == 0
```
</details>

---

### Exercise 3.2: Find and Fix - Segmentation Fault (C++)

**This code crashes:**

```cpp
#include <iostream>
#include <vector>

int main() {
    std::vector<int> vec = {1, 2, 3, 4, 5};

    for (int i = 0; i <= vec.size(); i++) {
        std::cout << vec[i] << std::endl;
    }

    return 0;
}
```

**Tasks:**
1. Identify the bug
2. Fix the code
3. Explain why it crashed

<details>
<summary>Solution</summary>

**Bug:** Loop condition uses `<=` instead of `<`, accessing one past the end.

**Fix:**
```cpp
#include <iostream>
#include <vector>

int main() {
    std::vector<int> vec = {1, 2, 3, 4, 5};

    // Fix 1: Correct loop condition
    for (int i = 0; i < vec.size(); i++) {
        std::cout << vec[i] << std::endl;
    }

    // Better: Range-based for loop
    for (const auto& elem : vec) {
        std::cout << elem << std::endl;
    }

    return 0;
}
```

**Explanation:** Using `<=` makes the loop iterate from 0 to 5 (inclusive), but valid indices are 0 to 4. Accessing `vec[5]` is undefined behavior.
</details>

---

### Exercise 3.3: Find and Fix - Pattern Matching (Haskell)

**This function crashes on some inputs:**

```haskell
last' :: [a] -> a
last' [x] = x
last' (x:xs) = last' xs

main = do
    print $ last' [1,2,3]  -- Works: 3
    print $ last' []        -- Crashes!
```

**Tasks:**
1. Identify the bug
2. Fix it to handle all cases
3. Add type safety

<details>
<summary>Solution</summary>

**Bug:** Missing pattern for empty list.

**Fix:**
```haskell
-- Option 1: Return Maybe
last' :: [a] -> Maybe a
last' [] = Nothing
last' [x] = Just x
last' (x:xs) = last' xs

-- Option 2: Provide default
last'WithDefault :: a -> [a] -> a
last'WithDefault def [] = def
last'WithDefault def [x] = x
last'WithDefault def (x:xs) = last'WithDefault def xs

-- Option 3: Use error (not recommended)
last'Unsafe :: [a] -> a
last'Unsafe [] = error "Empty list"
last'Unsafe [x] = x
last'Unsafe (x:xs) = last'Unsafe xs

-- Tests
import Test.HUnit

tests :: Test
tests = TestList [
    "last' empty" ~: Nothing ~?= last' ([] :: [Int]),
    "last' singleton" ~: Just 1 ~?= last' [1],
    "last' multiple" ~: Just 3 ~?= last' [1,2,3]
    ]
```
</details>

---

## Part 4: Property-Based Testing (Haskell)

### Exercise 4.1: QuickCheck Properties

**Write QuickCheck properties for reverse function:**

<details>
<summary>Solution</summary>

```haskell
import Test.QuickCheck

-- Property: Reversing twice gives original
prop_reverseTwice :: [Int] -> Bool
prop_reverseTwice xs = reverse (reverse xs) == xs

-- Property: Length is preserved
prop_reverseLength :: [Int] -> Bool
prop_reverseLength xs = length (reverse xs) == length xs

-- Property: First element becomes last
prop_reverseFirstLast :: NonEmptyList Int -> Bool
prop_reverseFirstLast (NonEmpty xs) =
    head xs == last (reverse xs)

-- Property: Reversing empty list gives empty list
prop_reverseEmpty :: Bool
prop_reverseEmpty = reverse ([] :: [Int]) == []

-- Property: Reverse of single element is itself
prop_reverseSingleton :: Int -> Bool
prop_reverseSingleton x = reverse [x] == [x]

main :: IO ()
main = do
    putStrLn "Testing reverse properties:"
    quickCheck prop_reverseTwice
    quickCheck prop_reverseLength
    quickCheck prop_reverseFirstLast
    quickCheck prop_reverseEmpty
    quickCheck prop_reverseSingleton
```
</details>

---

### Exercise 4.2: QuickCheck for Sorting

**Write properties for a sort function:**

<details>
<summary>Solution</summary>

```haskell
import Test.QuickCheck
import Data.List (sort)

-- Property: Output is sorted
prop_sorted :: [Int] -> Bool
prop_sorted xs =
    let sorted = sort xs
    in isSorted sorted
  where
    isSorted [] = True
    isSorted [_] = True
    isSorted (x:y:xs) = x <= y && isSorted (y:xs)

-- Property: Length is preserved
prop_sortLength :: [Int] -> Bool
prop_sortLength xs = length (sort xs) == length xs

-- Property: All elements are present
prop_sortElements :: [Int] -> Bool
prop_sortElements xs =
    all (`elem` sorted) xs && all (`elem` xs) sorted
  where
    sorted = sort xs

-- Property: Sorting is idempotent
prop_sortIdempotent :: [Int] -> Bool
prop_sortIdempotent xs = sort (sort xs) == sort xs

-- Property: Minimum element is first (if non-empty)
prop_sortMinimum :: NonEmptyList Int -> Bool
prop_sortMinimum (NonEmpty xs) =
    head (sort xs) == minimum xs

main :: IO ()
main = do
    putStrLn "Testing sort properties:"
    quickCheck prop_sorted
    quickCheck prop_sortLength
    quickCheck prop_sortElements
    quickCheck prop_sortIdempotent
    quickCheck prop_sortMinimum
```
</details>

---

## Part 5: Integration Testing

### Exercise 5.1: Test a Calculator Class (C++)

**Test multiple operations together:**

<details>
<summary>Solution</summary>

```cpp
#include <gtest/gtest.h>
#include "calculator.h"

class CalculatorIntegrationTest : public ::testing::Test {
protected:
    Calculator calc;

    void SetUp() override {
        calc = Calculator();
    }
};

TEST_F(CalculatorIntegrationTest, ComplexExpression) {
    // Test: (5 + 3) * 2 - 4 = 12
    int result = calc.add(5, 3);
    result = calc.multiply(result, 2);
    result = calc.subtract(result, 4);
    EXPECT_EQ(result, 12);
}

TEST_F(CalculatorIntegrationTest, ChainedOperations) {
    Calculator calc;
    calc.setMemory(10);
    calc.add(5);
    calc.multiply(2);
    calc.subtract(3);
    EXPECT_EQ(calc.getResult(), 27);  // (10 + 5) * 2 - 3
}

TEST_F(CalculatorIntegrationTest, ErrorRecovery) {
    try {
        calc.divide(10, 0);
        FAIL() << "Expected exception";
    } catch (std::invalid_argument&) {
        // Should be able to continue after error
        EXPECT_EQ(calc.add(5, 5), 10);
    }
}
```
</details>

---

## Part 6: Debugging with Tools

### Exercise 6.1: Use pdb (Python)

**Debug this buggy code using pdb:**

```python
def calculate_average(numbers):
    total = 0
    for num in numbers:
        total += num
    return total / len(numbers)

# This crashes!
result = calculate_average([])
```

**Task:** Use pdb to step through and fix the bug.

<details>
<summary>Solution</summary>

```python
import pdb

def calculate_average(numbers):
    pdb.set_trace()  # Debugger will stop here

    if not numbers:  # Fix: Check for empty list
        return 0

    total = 0
    for num in numbers:
        total += num
    return total / len(numbers)

# Test
def test_calculate_average():
    assert calculate_average([1, 2, 3]) == 2.0
    assert calculate_average([]) == 0
    assert calculate_average([5]) == 5.0
```

**pdb commands used:**
- `n` (next) - Execute next line
- `p numbers` - Print numbers
- `p total` - Print total
- `c` (continue) - Continue execution
</details>

---

### Exercise 6.2: Use gdb (C++)

**Debug this segmentation fault:**

```cpp
#include <iostream>

int main() {
    int* ptr = nullptr;
    *ptr = 42;  // Crash!
    std::cout << *ptr << std::endl;
    return 0;
}
```

**Task:** Compile with debug symbols and use gdb to find the bug.

<details>
<summary>Solution</summary>

```bash
# Compile with debug symbols
g++ -g program.cpp -o program

# Run with gdb
gdb ./program

# GDB session:
(gdb) run                    # Program crashes
(gdb) backtrace             # See where it crashed
(gdb) print ptr             # ptr is 0x0 (null)
(gdb) quit
```

**Fix:**
```cpp
#include <iostream>
#include <memory>

int main() {
    // Fix: Initialize pointer properly
    int value = 42;
    int* ptr = &value;
    std::cout << *ptr << std::endl;

    // Better: Use smart pointers
    auto smartPtr = std::make_unique<int>(42);
    std::cout << *smartPtr << std::endl;

    return 0;
}
```
</details>

---

## Part 7: Real-World Projects

### Exercise 7.1: TDD Bank Account System (Your Choice of Language)

**Build a complete bank account system using TDD:**

**Requirements:**
1. Create account with initial balance
2. Deposit money
3. Withdraw money (can't overdraft)
4. Get transaction history
5. Transfer money between accounts
6. Calculate interest

**Process:**
1. Write ONE test
2. Make it pass
3. Refactor
4. Repeat

<details>
<summary>Python Solution (Starter)</summary>

```python
# test_bank.py
import pytest
from bank import Account

class TestAccount:
    def test_new_account_has_initial_balance(self):
        account = Account(initial_balance=100)
        assert account.balance == 100

    def test_deposit_increases_balance(self):
        account = Account(initial_balance=100)
        account.deposit(50)
        assert account.balance == 150

    def test_withdraw_decreases_balance(self):
        account = Account(initial_balance=100)
        account.withdraw(30)
        assert account.balance == 70

    def test_cannot_overdraft(self):
        account = Account(initial_balance=100)
        with pytest.raises(ValueError):
            account.withdraw(150)

    def test_transaction_history(self):
        account = Account(initial_balance=100)
        account.deposit(50)
        account.withdraw(30)
        history = account.get_history()
        assert len(history) == 2
        assert history[0][0] == "deposit"
        assert history[1][0] == "withdraw"

# Implement bank.py to pass these tests!
```
</details>

---

### Exercise 7.2: Debug a Recursive Function (Haskell)

**This Fibonacci function has a bug:**

```haskell
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

main = do
    print $ fib 5      -- Works: 5
    print $ fib (-1)   -- Infinite recursion!
```

**Tasks:**
1. Identify all bugs
2. Add proper error handling
3. Write tests
4. Optimize for large inputs

<details>
<summary>Solution</summary>

```haskell
-- Fix 1: Handle negative input
fib :: Int -> Maybe Int
fib n
    | n < 0 = Nothing
    | n == 0 = Just 0
    | n == 1 = Just 1
    | otherwise = do
        a <- fib (n - 1)
        b <- fib (n - 2)
        return (a + b)

-- Fix 2: Optimized version with memoization
fib' :: Int -> Integer
fib' n
    | n < 0 = error "Negative input"
    | otherwise = fibs !! n
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Tests
import Test.HUnit

testFibNegative :: Test
testFibNegative = TestCase $
    assertEqual "fib negative" Nothing (fib (-1))

testFibBase :: Test
testFibBase = TestList [
    "fib 0" ~: Just 0 ~?= fib 0,
    "fib 1" ~: Just 1 ~?= fib 1
    ]

testFibSmall :: Test
testFibSmall = TestList [
    "fib 2" ~: Just 1 ~?= fib 2,
    "fib 3" ~: Just 2 ~?= fib 3,
    "fib 5" ~: Just 5 ~?= fib 5
    ]

testFibOptimized :: Test
testFibOptimized = TestCase $ do
    assertEqual "fib' 10" 55 (fib' 10)
    assertEqual "fib' 20" 6765 (fib' 20)
```
</details>

---

## Part 8: Test Coverage

### Exercise 8.1: Achieve 100% Coverage (Python)

**Given this function, write tests for 100% coverage:**

```python
def classify_number(n):
    if n < 0:
        return "negative"
    elif n == 0:
        return "zero"
    elif n < 10:
        return "single digit"
    elif n < 100:
        return "double digit"
    else:
        return "large number"
```

**Tasks:**
1. Write tests covering ALL branches
2. Run coverage analysis
3. Verify 100% coverage

<details>
<summary>Solution</summary>

```python
# test_classify.py
from classify import classify_number

def test_negative():
    assert classify_number(-5) == "negative"
    assert classify_number(-1) == "negative"

def test_zero():
    assert classify_number(0) == "zero"

def test_single_digit():
    assert classify_number(1) == "single digit"
    assert classify_number(5) == "single digit"
    assert classify_number(9) == "single digit"

def test_double_digit():
    assert classify_number(10) == "double digit"
    assert classify_number(50) == "double digit"
    assert classify_number(99) == "double digit"

def test_large():
    assert classify_number(100) == "large number"
    assert classify_number(1000) == "large number"

# Run coverage:
# pytest --cov=classify --cov-report=html
# open htmlcov/index.html
```
</details>

---

## Part 9: Cross-Language Challenge

### Exercise 9.1: Implement and Test in All Three Languages

**Implement a `Stack` data structure with tests in Python, C++, and Haskell.**

**Requirements:**
- Push
- Pop
- Peek
- isEmpty
- Size

**Compare:**
- How testing differs across languages
- What's easier/harder in each language
- How paradigms affect testing

---

## Part 10: Advanced Debugging

### Exercise 10.1: Debug Complex Logic (All Languages)

**This binary search has subtle bugs. Find and fix them:**

<details>
<summary>Python Version</summary>

```python
def binary_search(arr, target):
    left = 0
    right = len(arr)  # Bug: Should be len(arr) - 1

    while left <= right:
        mid = (left + right) / 2  # Bug: Should use //
        if arr[mid] == target:
            return mid
        elif arr[mid] < target:
            left = mid + 1
        else:
            right = mid - 1

    return -1

# Fix:
def binary_search_fixed(arr, target):
    left = 0
    right = len(arr) - 1

    while left <= right:
        mid = (left + right) // 2
        if arr[mid] == target:
            return mid
        elif arr[mid] < target:
            left = mid + 1
        else:
            right = mid - 1

    return -1

# Tests
def test_binary_search():
    arr = [1, 2, 3, 4, 5, 6, 7, 8, 9]
    assert binary_search_fixed(arr, 5) == 4
    assert binary_search_fixed(arr, 1) == 0
    assert binary_search_fixed(arr, 9) == 8
    assert binary_search_fixed(arr, 10) == -1
```
</details>

---

## Summary Checklist

After completing these exercises, you should be able to:
- ✅ Write unit tests in Python, C++, and Haskell
- ✅ Practice TDD workflow
- ✅ Use property-based testing (QuickCheck)
- ✅ Debug code systematically
- ✅ Use debugging tools (pdb, gdb, ghci)
- ✅ Write testable code across paradigms
- ✅ Achieve good test coverage
- ✅ Recognize common bug patterns
- ✅ Handle errors gracefully in different type systems

**Remember:** Testing and debugging are skills that improve with practice. Write tests for all your code and debug systematically!
