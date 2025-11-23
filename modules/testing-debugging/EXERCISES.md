# Testing & Debugging: Practice Exercises

These exercises help you develop testing and debugging skills across multiple programming languages.

---

## Part 1: Writing Basic Tests

### Exercise 1.1: Test a Simple Function (Python)

**Given this function:**

```python
def add(a, b):
    return a + b
```

**Task:** Write at least 5 test cases using pytest. Consider:
- Normal cases (positive numbers)
- Edge cases (zero, negative numbers)
- Different data types (integers, floats)

**Template:**
```python
def test_add_positive_numbers():
    assert add(2, 3) == 5

# Write 4 more tests here
```

---

### Exercise 1.2: Test a Function with Edge Cases

**Given this function:**

```python
def find_max(numbers):
    """Returns the maximum number in a list."""
    max_num = numbers[0]
    for num in numbers:
        if num > max_num:
            max_num = num
    return max_num
```

**Task:** Write comprehensive tests covering:
1. Normal case (list with multiple numbers)
2. Single element list
3. List with all identical numbers
4. List with negative numbers
5. What happens with an empty list? (Should this raise an error?)

---

### Exercise 1.3: Test String Manipulation (Java)

**Given this method:**

```java
public static String reverseString(String s) {
    StringBuilder reversed = new StringBuilder();
    for (int i = s.length() - 1; i >= 0; i--) {
        reversed.append(s.charAt(i));
    }
    return reversed.toString();
}
```

**Task:** Write JUnit tests for:
1. Normal strings
2. Empty string
3. Single character
4. Strings with spaces
5. Strings with special characters

---

### Exercise 1.4: Test Mathematical Operations

**Task:** Write a function `divide(a, b)` that divides two numbers, along with comprehensive tests.

**Requirements:**
- Function should handle division by zero appropriately
- Should work with integers and floats
- Write at least 7 test cases

Consider:
- Normal division
- Division by zero (should raise exception or return special value?)
- Division resulting in a fraction
- Negative numbers
- Zero divided by a number

---

## Part 2: Test-Driven Development (TDD)

### Exercise 2.1: TDD - Password Validator

**Use TDD to create a password validator with these requirements:**

1. Password must be at least 8 characters long
2. Must contain at least one uppercase letter
3. Must contain at least one lowercase letter
4. Must contain at least one digit
5. Must contain at least one special character (!@#$%^&*)

**TDD Steps:**
```
1. Write test for requirement 1 (length check) → RED
2. Implement just enough code to pass → GREEN
3. Refactor if needed
4. Write test for requirement 2 (uppercase) → RED
5. Implement just enough code to pass → GREEN
6. Continue for all requirements...
```

**Example starting test:**
```python
def test_password_length_valid():
    validator = PasswordValidator()
    assert validator.is_valid("12345678") == False  # No uppercase/lowercase/special

def test_password_length_too_short():
    validator = PasswordValidator()
    assert validator.is_valid("Pass1!") == False
```

---

### Exercise 2.2: TDD - Shopping Cart

**Use TDD to build a shopping cart with these features:**

1. Add items to cart (with price and quantity)
2. Remove items from cart
3. Calculate total price
4. Apply a discount code (10% off)
5. Cannot have negative quantities

**Write tests first, then implement!**

Example tests to start:
```python
def test_empty_cart_total_is_zero():
    cart = ShoppingCart()
    assert cart.total() == 0

def test_add_single_item():
    cart = ShoppingCart()
    cart.add_item("Apple", price=1.00, quantity=3)
    assert cart.total() == 3.00
```

---

### Exercise 2.3: TDD - Email Validator

**Requirements:**
1. Must contain exactly one @ symbol
2. Must have at least one character before @
3. Must have at least one character after @
4. Domain part must contain at least one dot
5. No spaces allowed

**Use TDD to implement this step by step.**

---

## Part 3: Debugging Exercises

### Exercise 3.1: Find and Fix the Bug - Off by One

**This function should return the sum of numbers from 1 to n:**

```python
def sum_to_n(n):
    total = 0
    for i in range(n):
        total += i
    return total

# Test
print(sum_to_n(5))  # Expected: 15, but returns 10
```

**Tasks:**
1. Identify the bug
2. Explain why it happens
3. Fix the code
4. Write tests to prevent this bug

---

### Exercise 3.2: Find and Fix - Logic Error

**This function should check if a year is a leap year:**

```python
def is_leap_year(year):
    if year % 4 == 0:
        return True
    else:
        return False

# Tests reveal bugs:
print(is_leap_year(2000))  # Should be True, returns True ✓
print(is_leap_year(1900))  # Should be False, returns True ✗
print(is_leap_year(2020))  # Should be True, returns True ✓
```

**Tasks:**
1. Research the actual rules for leap years
2. Identify what's wrong with the implementation
3. Fix the function
4. Write comprehensive tests

---

### Exercise 3.3: Find and Fix - Index Error

**This function should find the first negative number in a list:**

```python
def find_first_negative(numbers):
    for i in range(len(numbers)):
        if numbers[i] < 0:
            return i
    return -1

# This crashes!
result = find_first_negative([])
```

**Tasks:**
1. Reproduce the error
2. Explain why it happens
3. Fix the code
4. Write tests including edge cases

---

### Exercise 3.4: Debug with Print Statements

**This function should reverse words in a sentence:**

```python
def reverse_words(sentence):
    words = sentence.split()
    reversed_words = []
    for word in words:
        reversed_word = ""
        for char in word:
            reversed_word = char + reversed_word
        reversed_words.append(word)  # Bug here!
    return " ".join(reversed_words)

print(reverse_words("Hello World"))  # Expected: "olleH dlroW", Got: "Hello World"
```

**Tasks:**
1. Add print statements to trace execution
2. Identify which variable has the wrong value
3. Fix the bug
4. Remove debug prints and add proper tests

---

### Exercise 3.5: Debug - Mutable Default Argument

**This function has a subtle bug:**

```python
def add_student(name, grades=[]):
    grades.append(85)
    return {"name": name, "grades": grades}

student1 = add_student("Alice")
print(student1)  # {'name': 'Alice', 'grades': [85]}

student2 = add_student("Bob")
print(student2)  # Expected: {'name': 'Bob', 'grades': [85]}
                # Actual: {'name': 'Bob', 'grades': [85, 85]} !!!
```

**Tasks:**
1. Explain why this happens
2. Research "mutable default arguments" in Python
3. Fix the function
4. Write tests that would catch this bug

---

## Part 4: Integration Testing

### Exercise 4.1: Test a Multi-Function System

**Given this file management system:**

```python
class FileManager:
    def __init__(self):
        self.files = {}

    def create_file(self, filename, content):
        self.files[filename] = content

    def read_file(self, filename):
        if filename not in self.files:
            raise FileNotFoundError(f"{filename} not found")
        return self.files[filename]

    def delete_file(self, filename):
        if filename not in self.files:
            raise FileNotFoundError(f"{filename} not found")
        del self.files[filename]

    def list_files(self):
        return list(self.files.keys())
```

**Task:** Write integration tests that test multiple operations together:

1. Create → Read → Verify content
2. Create → Delete → Verify not found
3. Create multiple → List → Verify all present
4. Create → Delete → Create with same name → Verify new content
5. Delete nonexistent → Verify error handling

---

### Exercise 4.2: Test User Authentication System

**System with multiple components:**

```python
class User:
    def __init__(self, username, password_hash):
        self.username = username
        self.password_hash = password_hash

class UserDatabase:
    def __init__(self):
        self.users = {}

    def add_user(self, username, password_hash):
        if username in self.users:
            return False
        self.users[username] = User(username, password_hash)
        return True

    def get_user(self, username):
        return self.users.get(username)

class AuthenticationService:
    def __init__(self, user_db):
        self.user_db = user_db

    def register(self, username, password):
        # Simple hash (in reality, use proper hashing!)
        password_hash = hash(password)
        return self.user_db.add_user(username, password_hash)

    def login(self, username, password):
        user = self.user_db.get_user(username)
        if user and user.password_hash == hash(password):
            return True
        return False
```

**Task:** Write integration tests for:
1. Register new user → Login successfully
2. Register duplicate user → Should fail
3. Login with wrong password → Should fail
4. Login before registration → Should fail

---

## Part 5: Advanced Testing Scenarios

### Exercise 5.1: Testing with Exceptions

**Create a calculator that handles errors properly:**

```python
class Calculator:
    def divide(self, a, b):
        # TODO: Implement with proper error handling
        pass

    def sqrt(self, x):
        # TODO: Implement with proper error handling
        pass
```

**Task:**
1. Implement the methods
2. Write tests using `pytest.raises()`:

```python
import pytest

def test_divide_by_zero_raises_error():
    calc = Calculator()
    with pytest.raises(ZeroDivisionError):
        calc.divide(10, 0)

def test_sqrt_negative_raises_error():
    calc = Calculator()
    with pytest.raises(ValueError):
        calc.sqrt(-4)
```

---

### Exercise 5.2: Testing with Fixtures

**Create a test suite using pytest fixtures:**

```python
@pytest.fixture
def sample_shopping_cart():
    cart = ShoppingCart()
    cart.add_item("Apple", 1.00, 3)
    cart.add_item("Banana", 0.50, 5)
    return cart

def test_total_with_items(sample_shopping_cart):
    # Use the fixture here
    pass

def test_remove_item(sample_shopping_cart):
    # Use the fixture here
    pass
```

**Task:** Implement tests using fixtures for:
1. Database connection
2. Pre-populated shopping cart
3. Temporary file for file I/O tests

---

### Exercise 5.3: Parametrized Testing

**Use pytest's parametrize to test multiple cases:**

```python
@pytest.mark.parametrize("input,expected", [
    (0, "FizzBuzz"),
    (3, "Fizz"),
    (5, "Buzz"),
    (15, "FizzBuzz"),
    (1, "1"),
])
def test_fizzbuzz(input, expected):
    assert fizzbuzz(input) == expected
```

**Task:** Write parametrized tests for:
1. Temperature converter (Celsius ↔ Fahrenheit)
2. Grade calculator (score → letter grade)
3. String validator (valid/invalid emails)

---

## Part 6: Real-World Debugging Scenarios

### Exercise 6.1: Debug a Sorting Algorithm

**This bubble sort has bugs:**

```python
def bubble_sort(arr):
    n = len(arr)
    for i in range(n):
        for j in range(n - i):
            if arr[j] > arr[j + 1]:
                arr[j], arr[j + 1] = arr[j + 1], arr[j]
    return arr

# Crashes on some inputs!
print(bubble_sort([5, 2, 8, 1, 9]))
```

**Tasks:**
1. Find the bug causing the crash
2. Write tests that expose the bug
3. Fix the implementation
4. Verify with comprehensive tests

---

### Exercise 6.2: Debug a Recursive Function

**This factorial function has issues:**

```python
def factorial(n):
    if n == 1:
        return 1
    return n * factorial(n - 1)

print(factorial(5))   # Works: 120
print(factorial(0))   # Crashes!
print(factorial(-3))  # Infinite recursion!
```

**Tasks:**
1. Identify all bugs
2. Fix the base case
3. Add input validation
4. Write comprehensive tests

---

### Exercise 6.3: Debug State Management

**This counter has unexpected behavior:**

```python
class Counter:
    count = 0  # Class variable!

    def __init__(self):
        self.count = 0

    def increment(self):
        self.count += 1

    def get_count(self):
        return self.count

# Bug manifests here:
counter1 = Counter()
counter2 = Counter()

counter1.increment()
print(counter1.get_count())  # 1
print(counter2.get_count())  # 0 (correct)

# But after this...
Counter.count = 5
counter3 = Counter()
print(counter3.get_count())  # Unexpected behavior!
```

**Tasks:**
1. Understand the difference between class and instance variables
2. Identify the design flaw
3. Rewrite the class correctly
4. Write tests that verify independence of instances

---

## Part 7: Code Coverage

### Exercise 7.1: Achieve 100% Coverage

**Given this function:**

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

**Task:**
1. Install coverage: `pip install coverage pytest-cov`
2. Write tests to cover ALL branches
3. Run: `pytest --cov=your_module --cov-report=html`
4. Verify 100% coverage

---

### Exercise 7.2: Find Untested Code Paths

**Given this function:**

```python
def process_order(order):
    if order["status"] == "pending":
        if order["priority"] == "high":
            return "Process immediately"
        elif order["priority"] == "normal":
            return "Process in queue"
        else:
            return "Process when available"
    elif order["status"] == "shipped":
        return "Already shipped"
    elif order["status"] == "cancelled":
        return "Order cancelled"
    return "Unknown status"
```

**Task:**
1. Write initial tests
2. Run coverage analysis
3. Identify untested branches
4. Add tests to cover all paths

---

## Part 8: Refactoring for Testability

### Exercise 8.1: Make Code Testable

**This code is hard to test:**

```python
import random
import datetime

def generate_user_id():
    timestamp = datetime.datetime.now().timestamp()
    random_num = random.randint(1000, 9999)
    return f"USER-{int(timestamp)}-{random_num}"
```

**Problem:** How do you test this? The output is different every time!

**Task:**
1. Refactor to accept dependencies:
```python
def generate_user_id(timestamp_func=None, random_func=None):
    if timestamp_func is None:
        timestamp_func = lambda: datetime.datetime.now().timestamp()
    if random_func is None:
        random_func = lambda: random.randint(1000, 9999)

    timestamp = timestamp_func()
    random_num = random_func()
    return f"USER-{int(timestamp)}-{random_num}"
```

2. Write tests using mock functions:
```python
def test_generate_user_id():
    timestamp_func = lambda: 1234567890.0
    random_func = lambda: 5555
    result = generate_user_id(timestamp_func, random_func)
    assert result == "USER-1234567890-5555"
```

---

### Exercise 8.2: Extract Testable Functions

**Monolithic function:**

```python
def process_and_save_data():
    # Read from database
    data = database.query("SELECT * FROM users")

    # Process data
    processed = []
    for row in data:
        processed.append({
            "name": row["name"].upper(),
            "age": row["age"],
            "status": "active" if row["age"] < 65 else "senior"
        })

    # Save to file
    with open("output.txt", "w") as f:
        for item in processed:
            f.write(f"{item['name']},{item['age']},{item['status']}\n")
```

**Task:** Refactor into three testable functions:
1. `fetch_data()` - can be mocked
2. `process_user_data(data)` - pure function, easy to test
3. `save_to_file(data, filename)` - can be mocked

---

## Part 9: Test Organization and Best Practices

### Exercise 9.1: Organize Test Suite

**Create a well-organized test structure for a library management system:**

```
tests/
├── unit/
│   ├── test_book.py
│   ├── test_member.py
│   └── test_loan.py
├── integration/
│   ├── test_checkout_workflow.py
│   └── test_return_workflow.py
└── fixtures/
    └── conftest.py
```

**Task:** Implement tests in this structure with:
- Unit tests for individual classes
- Integration tests for workflows
- Shared fixtures in conftest.py

---

### Exercise 9.2: Write Good Test Names

**Rewrite these poorly named tests:**

❌ Bad:
```python
def test1():
    assert add(2, 2) == 4

def test2():
    assert add(0, 0) == 0

def test_add():
    assert add(-1, 1) == 0
```

✅ Good:
```python
def test_add_positive_numbers_returns_correct_sum():
    assert add(2, 2) == 4

def test_add_zeros_returns_zero():
    assert add(0, 0) == 0

def test_add_negative_and_positive_returns_correct_sum():
    assert add(-1, 1) == 0
```

**Task:** Rewrite these test names to be descriptive:
1. `test_stack()` - for testing stack.push()
2. `test2()` - for testing empty list edge case
3. `test_login()` - for testing incorrect password
4. `test_something()` - for testing divide by zero

---

## Part 10: Project - Build a Tested Application

### Exercise 10.1: TDD Bank Account System

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

Start with:
```python
def test_new_account_has_zero_balance():
    account = BankAccount()
    assert account.balance == 0
```

---

### Exercise 10.2: Debug a Complete Application

**Given this buggy TODO list application:**

```python
class TodoList:
    def __init__(self):
        self.todos = []

    def add_todo(self, task, priority="normal"):
        todo = {
            "task": task,
            "priority": priority,
            "completed": False
        }
        self.todos.append(todo)

    def complete_todo(self, index):
        self.todos[index]["completed"] = True

    def get_incomplete_todos(self):
        return [t for t in self.todos if not t["completed"]]

    def get_high_priority_todos(self):
        return [t for t in self.todos if t["priority"] == "high"]

    def remove_todo(self, index):
        del self.todos[index]
```

**Known Issues:**
1. Index out of range errors
2. No validation for priority values
3. Race conditions with concurrent access
4. No way to update a todo

**Task:**
1. Write tests that expose all bugs
2. Fix each bug
3. Add new features with tests
4. Achieve >90% code coverage

---

## Part 11: Language-Specific Testing

### Exercise 11.1: Java JUnit Testing

**Test this Java class:**

```java
public class StringUtils {
    public static boolean isPalindrome(String s) {
        String cleaned = s.toLowerCase().replaceAll("[^a-z0-9]", "");
        int left = 0;
        int right = cleaned.length() - 1;

        while (left < right) {
            if (cleaned.charAt(left) != cleaned.charAt(right)) {
                return false;
            }
            left++;
            right--;
        }
        return true;
    }
}
```

**Task:** Write JUnit tests for:
- Simple palindromes
- Palindromes with spaces
- Palindromes with punctuation
- Non-palindromes
- Edge cases

---

### Exercise 11.2: Python Doctests

**Add doctest examples:**

```python
def fibonacci(n):
    """
    Calculate the nth Fibonacci number.

    >>> fibonacci(0)
    0
    >>> fibonacci(1)
    1
    >>> fibonacci(10)
    55

    # Add more examples here
    """
    if n <= 1:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)
```

**Task:**
1. Add at least 5 more doctest examples
2. Run with: `python -m doctest your_file.py -v`
3. Ensure all pass

---

## Solutions to Selected Exercises

### Exercise 1.1 Solution

```python
def test_add_positive_numbers():
    assert add(2, 3) == 5

def test_add_negative_numbers():
    assert add(-2, -3) == -5

def test_add_mixed_signs():
    assert add(-5, 10) == 5

def test_add_with_zero():
    assert add(0, 5) == 5
    assert add(5, 0) == 5

def test_add_floats():
    assert add(1.5, 2.3) == pytest.approx(3.8)
```

### Exercise 3.1 Solution

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
```

**Tests:**
```python
def test_sum_to_n_base_case():
    assert sum_to_n(1) == 1

def test_sum_to_n_small():
    assert sum_to_n(5) == 15  # 1+2+3+4+5

def test_sum_to_n_zero():
    assert sum_to_n(0) == 0
```

### Exercise 3.2 Solution

**Bug:** Doesn't account for century years correctly.

**Leap year rules:**
- Divisible by 4: leap year
- EXCEPT if divisible by 100: not a leap year
- EXCEPT if divisible by 400: leap year

**Fix:**
```python
def is_leap_year(year):
    if year % 400 == 0:
        return True
    if year % 100 == 0:
        return False
    if year % 4 == 0:
        return True
    return False
```

**Tests:**
```python
def test_typical_leap_year():
    assert is_leap_year(2020) == True

def test_typical_non_leap_year():
    assert is_leap_year(2021) == False

def test_century_non_leap_year():
    assert is_leap_year(1900) == False

def test_century_leap_year():
    assert is_leap_year(2000) == True
```

---

## Additional Practice

For more practice:
1. Find open-source projects and write tests for untested code
2. Practice TDD with coding challenge websites (but write tests first!)
3. Debug real code from past projects
4. Use coverage tools to find untested code paths
5. Pair program with focus on testing

Remember: **Good tests are as important as good code!**
