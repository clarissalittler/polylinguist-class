# Lab 10: Test-First Development

**Quarter 1, Week 10**
**Duration:** 90 minutes
**Format:** Pair programming (one writes tests, one writes code)

## Overview

"Test-First" (also called Test-Driven Development, TDD) means writing tests before you write the code. It sounds backwards, but it's incredibly powerful for writing correct, well-designed code.

## Objectives

By the end of this lab, you will:
- [ ] Write unit tests in Python (pytest)
- [ ] Practice the TDD cycle: Red → Green → Refactor
- [ ] Understand why testing matters
- [ ] Add tests to existing code

## Setup

- Partner up
- Create folder: `lab10-testing/`
- Install pytest: `pip install pytest`
- Decide who is "Test Writer" and who is "Code Writer" (you'll switch!)

---

## Part 1: Why Test? (10 minutes)

### Activity 1.1: The Bug Hunt

Look at this code:

```python
def calculate_discount(price, discount_percent):
    """Calculate discounted price."""
    discount = price * discount_percent
    return price - discount

# Test manually
print(calculate_discount(100, 0.20))  # 80.0 ✓
print(calculate_discount(50, 0.10))   # 45.0 ✓
print(calculate_discount(0, 0.50))    # 0.0 ✓
```

Looks good, right? But what about:
```python
print(calculate_discount(100, -0.10))  # 110.0 - Is this right??
print(calculate_discount(-50, 0.20))   # -40.0 - Negative price??
print(calculate_discount(100, 1.50))   # -50.0 - 150% discount??
```

**Discussion:** What edge cases weren't considered? How would tests have helped?

### Activity 1.2: Benefits of Testing

| Benefit | Explanation |
|---------|-------------|
| **Catch bugs early** | Before they reach users |
| **Document behavior** | Tests show how code should work |
| **Enable refactoring** | Change code confidently |
| **Design better** | Testable code is often better designed |
| **Save time** | Less manual testing, fewer bug hunts |

### Activity 1.3: First Test

Create `test_discount.py`:

```python
import pytest

def calculate_discount(price, discount_percent):
    """Calculate discounted price."""
    discount = price * discount_percent
    return price - discount

def test_basic_discount():
    """Test a simple 20% discount."""
    assert calculate_discount(100, 0.20) == 80.0

def test_no_discount():
    """Test 0% discount returns original price."""
    assert calculate_discount(50, 0) == 50

def test_full_discount():
    """Test 100% discount returns 0."""
    assert calculate_discount(100, 1.0) == 0
```

Run tests:
```bash
pytest test_discount.py -v
```

### ✅ Checkpoint 1

Verify:
- [ ] pytest is installed
- [ ] First tests pass
- [ ] Understand what `assert` does

---

## Part 2: The TDD Cycle (25 minutes)

### Activity 2.1: Red → Green → Refactor

The TDD mantra:

1. **🔴 Red**: Write a failing test
2. **🟢 Green**: Write just enough code to pass
3. **🔄 Refactor**: Clean up without breaking tests

Let's build a `StringCalculator` using TDD!

### Activity 2.2: Round 1 - Empty String

**🔴 Red: Write failing test**

Create `test_string_calc.py`:
```python
from string_calc import add

def test_empty_string_returns_zero():
    """Empty string should return 0."""
    assert add("") == 0
```

Run it:
```bash
pytest test_string_calc.py -v
```

It fails! (No module `string_calc`). Good!

**🟢 Green: Make it pass**

Create `string_calc.py`:
```python
def add(numbers):
    if numbers == "":
        return 0
```

Run tests again - passes!

**🔄 Refactor**: Nothing to clean up yet.

### Activity 2.3: Round 2 - Single Number

**🔴 Red:**
```python
def test_single_number():
    """Single number returns itself."""
    assert add("1") == 1
    assert add("5") == 5
```

Run - fails!

**🟢 Green:**
```python
def add(numbers):
    if numbers == "":
        return 0
    return int(numbers)
```

Run - passes!

### Activity 2.4: Round 3 - Two Numbers

**🔴 Red:**
```python
def test_two_numbers():
    """Two comma-separated numbers are added."""
    assert add("1,2") == 3
    assert add("10,20") == 30
```

Run - fails!

**🟢 Green:**
```python
def add(numbers):
    if numbers == "":
        return 0
    if "," in numbers:
        parts = numbers.split(",")
        return int(parts[0]) + int(parts[1])
    return int(numbers)
```

### Activity 2.5: Round 4 - Multiple Numbers

**🔴 Red:**
```python
def test_multiple_numbers():
    """Multiple numbers are all added."""
    assert add("1,2,3") == 6
    assert add("1,2,3,4,5") == 15
```

**🟢 Green:**
```python
def add(numbers):
    if numbers == "":
        return 0
    parts = numbers.split(",")
    return sum(int(p) for p in parts)
```

**🔄 Refactor**: Our code got cleaner! The sum handles 1, 2, or many numbers.

### Activity 2.6: Round 5 - Newlines as Delimiters

**🔴 Red:**
```python
def test_newlines_as_delimiter():
    """Newlines work as delimiters too."""
    assert add("1\n2,3") == 6
    assert add("1\n2\n3") == 6
```

**🟢 Green:**
```python
def add(numbers):
    if numbers == "":
        return 0
    # Replace newlines with commas
    numbers = numbers.replace("\n", ",")
    parts = numbers.split(",")
    return sum(int(p) for p in parts)
```

### ✅ Checkpoint 2

Verify with partner:
- [ ] All tests pass
- [ ] Experienced the Red → Green → Refactor cycle
- [ ] Code Writer and Test Writer switch roles!

---

## Part 3: Testing Edge Cases (20 minutes)

### Activity 3.1: Negative Numbers

What should happen with negative numbers?

```python
def test_negative_numbers():
    """Negative numbers should raise an error."""
    with pytest.raises(ValueError):
        add("-1,2,3")

    with pytest.raises(ValueError):
        add("1,-2")
```

**Implement:**
```python
def add(numbers):
    if numbers == "":
        return 0
    numbers = numbers.replace("\n", ",")
    parts = numbers.split(",")
    nums = [int(p) for p in parts]

    # Check for negatives
    negatives = [n for n in nums if n < 0]
    if negatives:
        raise ValueError(f"Negatives not allowed: {negatives}")

    return sum(nums)
```

### Activity 3.2: Invalid Input

```python
def test_invalid_input():
    """Non-numeric input should raise an error."""
    with pytest.raises(ValueError):
        add("a,b,c")

    with pytest.raises(ValueError):
        add("1,two,3")
```

**Your turn:** Modify `add()` to handle this gracefully.

### Activity 3.3: Boundary Testing

Test edge cases:

```python
def test_large_numbers():
    """Large numbers should work."""
    assert add("1000,2000") == 3000

def test_zero():
    """Zero should work normally."""
    assert add("0,1,2") == 3
    assert add("0") == 0

def test_whitespace():
    """Whitespace around numbers should be handled."""
    assert add(" 1 , 2 , 3 ") == 6
```

### Activity 3.4: Complete Test Suite

Here's what a good test suite looks like:

```python
import pytest
from string_calc import add

class TestStringCalculator:
    """Test suite for StringCalculator."""

    # Basic functionality
    def test_empty_string_returns_zero(self):
        assert add("") == 0

    def test_single_number(self):
        assert add("1") == 1
        assert add("42") == 42

    def test_two_numbers(self):
        assert add("1,2") == 3

    def test_multiple_numbers(self):
        assert add("1,2,3,4,5") == 15

    def test_newlines_as_delimiter(self):
        assert add("1\n2,3") == 6

    # Edge cases
    def test_negative_numbers_raise_error(self):
        with pytest.raises(ValueError) as exc_info:
            add("-1,2,3")
        assert "Negatives not allowed" in str(exc_info.value)

    def test_multiple_negatives_listed(self):
        with pytest.raises(ValueError) as exc_info:
            add("-1,-2,3")
        assert "-1" in str(exc_info.value)
        assert "-2" in str(exc_info.value)

    # Robustness
    def test_whitespace_handled(self):
        assert add(" 1 , 2 ") == 3

    def test_large_numbers(self):
        assert add("1000000,2000000") == 3000000
```

### ✅ Checkpoint 3

Verify:
- [ ] Tests cover happy path and edge cases
- [ ] Error handling tested with `pytest.raises`

---

## Part 4: Testing Existing Code (15 minutes)

### Activity 4.1: Add Tests to Project Code

Let's add tests to functions from previous labs.

**Code to test** (`math_functions.py`):
```python
def factorial(n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)

def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)

def is_prime(n):
    if n < 2:
        return False
    for i in range(2, int(n ** 0.5) + 1):
        if n % i == 0:
            return False
    return True

def gcd(a, b):
    while b:
        a, b = b, a % b
    return a
```

### Activity 4.2: Write the Tests

Create `test_math_functions.py`:

```python
import pytest
from math_functions import factorial, fibonacci, is_prime, gcd

class TestFactorial:
    def test_factorial_zero(self):
        assert factorial(0) == 1

    def test_factorial_one(self):
        assert factorial(1) == 1

    def test_factorial_five(self):
        assert factorial(5) == 120

    def test_factorial_ten(self):
        assert factorial(10) == 3628800


class TestFibonacci:
    def test_fib_zero(self):
        assert fibonacci(0) == 0

    def test_fib_one(self):
        assert fibonacci(1) == 1

    def test_fib_ten(self):
        assert fibonacci(10) == 55

    def test_fib_sequence(self):
        expected = [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
        for i, exp in enumerate(expected):
            assert fibonacci(i) == exp


class TestIsPrime:
    def test_zero_not_prime(self):
        assert is_prime(0) == False

    def test_one_not_prime(self):
        assert is_prime(1) == False

    def test_two_is_prime(self):
        assert is_prime(2) == True

    def test_small_primes(self):
        primes = [2, 3, 5, 7, 11, 13, 17, 19, 23]
        for p in primes:
            assert is_prime(p) == True

    def test_composites(self):
        composites = [4, 6, 8, 9, 10, 12, 14, 15]
        for c in composites:
            assert is_prime(c) == False


class TestGCD:
    def test_gcd_same_number(self):
        assert gcd(5, 5) == 5

    def test_gcd_one_is_factor(self):
        assert gcd(12, 4) == 4

    def test_gcd_coprime(self):
        assert gcd(7, 11) == 1

    def test_gcd_common(self):
        assert gcd(48, 18) == 6
```

### Activity 4.3: Run and Interpret

```bash
pytest test_math_functions.py -v
```

**Questions to consider:**
- Are there edge cases we missed?
- What happens with negative input?
- What happens with very large input?

### ✅ Checkpoint 4

Verify:
- [ ] All math function tests pass
- [ ] Can identify missing test cases

---

## Part 5: Testing in Other Languages (10 minutes)

### Activity 5.1: C++ with Catch2 (Preview)

```cpp
// test_math.cpp (using Catch2 header-only)
#define CATCH_CONFIG_MAIN
#include "catch.hpp"

int factorial(int n) {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}

TEST_CASE("Factorial computes correctly", "[factorial]") {
    REQUIRE(factorial(0) == 1);
    REQUIRE(factorial(1) == 1);
    REQUIRE(factorial(5) == 120);
}

TEST_CASE("Factorial of 10", "[factorial]") {
    REQUIRE(factorial(10) == 3628800);
}
```

### Activity 5.2: Haskell with HUnit (Preview)

```haskell
-- test/Spec.hs
import Test.HUnit

factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n - 1)

testFactorialZero :: Test
testFactorialZero = TestCase (assertEqual "factorial 0" 1 (factorial 0))

testFactorialFive :: Test
testFactorialFive = TestCase (assertEqual "factorial 5" 120 (factorial 5))

tests :: Test
tests = TestList [testFactorialZero, testFactorialFive]

main :: IO ()
main = runTestTT tests >> return ()
```

### Activity 5.3: Property-Based Testing (Advanced)

Instead of testing specific values, test properties:

```python
# Using hypothesis library
from hypothesis import given, strategies as st

@given(st.integers(min_value=0, max_value=12))
def test_factorial_positive(n):
    result = factorial(n)
    assert result >= 1
    assert result >= n

@given(st.integers(min_value=1), st.integers(min_value=1))
def test_gcd_divides_both(a, b):
    result = gcd(a, b)
    assert a % result == 0
    assert b % result == 0
```

---

## Challenges

### Challenge 1: 100% Coverage

Achieve 100% test coverage for a function:

```bash
pip install pytest-cov
pytest --cov=string_calc --cov-report=term-missing test_string_calc.py
```

### Challenge 2: Test a Previous Project

Go back to Project 1 or 2 and add at least 5 meaningful tests.

### Challenge 3: TDD a New Feature

Use TDD to implement these features for StringCalculator:

1. Custom delimiter: `"//;\n1;2"` uses `;` as delimiter
2. Ignore numbers > 1000: `add("2,1001")` returns 2
3. Multiple custom delimiters: `"//[*][%]\n1*2%3"` returns 6

---

## Wrap-Up

**Key takeaways:**

1. **Write tests first** - Forces you to think about requirements
2. **Red → Green → Refactor** - The TDD cycle
3. **Test edge cases** - Empty, negative, large, invalid inputs
4. **Tests are documentation** - Show how code should work
5. **Tests enable refactoring** - Change confidently

**Testing mindset:**
- "How could this break?"
- "What are the boundary conditions?"
- "What if the input is unexpected?"

**Next week:** Quarter 1 Showcase - show off your projects with all those tests!

---

## Quick Reference: pytest

```python
# Basic assertion
assert result == expected

# Approximate equality (for floats)
assert result == pytest.approx(3.14159, rel=1e-3)

# Exception testing
with pytest.raises(ValueError):
    function_that_raises()

# Parametrized tests (test multiple inputs)
@pytest.mark.parametrize("input,expected", [
    ("", 0),
    ("1", 1),
    ("1,2", 3),
])
def test_add(input, expected):
    assert add(input) == expected

# Run specific test
# pytest test_file.py::test_name -v

# Run tests matching pattern
# pytest -k "factorial" -v

# Show print output
# pytest -s
```
