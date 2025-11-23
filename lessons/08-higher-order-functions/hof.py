#!/usr/bin/env python3
"""
Lesson 8: Higher-Order Functions in Python

Python has excellent support for functional programming with:
- First-class functions
- Lambda expressions
- Built-in HOFs (map, filter, reduce)
- List comprehensions (alternative syntax)
- Decorators (HOFs for metaprogramming)
"""

from functools import reduce, partial
from typing import Callable, List, TypeVar

T = TypeVar('T')
U = TypeVar('U')

# ====================
# 1. Functions as First-Class Values
# ====================

def demonstrate_first_class():
    """Functions can be assigned, passed, and returned"""

    def greet(name):
        return f"Hello, {name}!"

    # Assign to variable
    say_hello = greet
    print(f"   {say_hello('Alice')}")

    # Store in data structure
    operations = {
        'greet': greet,
        'shout': lambda name: f"HEY {name.upper()}!",
    }
    print(f"   {operations['greet']('Bob')}")
    print(f"   {operations['shout']('Bob')}")

# ====================
# 2. Functions Taking Functions (HOFs)
# ====================

def apply_twice(func: Callable, x):
    """Apply a function twice"""
    return func(func(x))

def apply_n_times(func: Callable, n: int, x):
    """Apply a function n times"""
    result = x
    for _ in range(n):
        result = func(result)
    return result

# ====================
# 3. Functions Returning Functions
# ====================

def make_multiplier(n: int) -> Callable:
    """Factory function returning a multiplier"""
    def multiplier(x):
        return x * n
    return multiplier

def make_adder(n: int) -> Callable:
    """Factory function returning an adder"""
    return lambda x: x + n

# ====================
# 4. Map - Transform Each Element
# ====================

def demonstrate_map():
    """Map applies a function to each element"""
    numbers = [1, 2, 3, 4, 5]

    # Using map with lambda
    doubled = list(map(lambda x: x * 2, numbers))
    print(f"   Doubled: {doubled}")

    # Using map with named function
    squared = list(map(lambda x: x ** 2, numbers))
    print(f"   Squared: {squared}")

    # List comprehension alternative (more Pythonic)
    doubled_comp = [x * 2 for x in numbers]
    print(f"   Doubled (comprehension): {doubled_comp}")

    # Map with multiple lists
    list1 = [1, 2, 3]
    list2 = [10, 20, 30]
    sums = list(map(lambda x, y: x + y, list1, list2))
    print(f"   Element-wise sum: {sums}")

# ====================
# 5. Filter - Select Elements
# ====================

def demonstrate_filter():
    """Filter keeps elements matching a predicate"""
    numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

    # Using filter
    evens = list(filter(lambda x: x % 2 == 0, numbers))
    print(f"   Evens: {evens}")

    # List comprehension alternative (more Pythonic)
    odds = [x for x in numbers if x % 2 != 0]
    print(f"   Odds: {odds}")

    # Complex predicate
    big_evens = list(filter(lambda x: x % 2 == 0 and x > 5, numbers))
    print(f"   Big evens: {big_evens}")

# ====================
# 6. Reduce - Combine to Single Value
# ====================

def demonstrate_reduce():
    """Reduce combines all elements using a binary function"""
    numbers = [1, 2, 3, 4, 5]

    # Sum all numbers
    total = reduce(lambda acc, x: acc + x, numbers, 0)
    print(f"   Sum: {total}")

    # Product of all numbers
    product = reduce(lambda acc, x: acc * x, numbers, 1)
    print(f"   Product: {product}")

    # Find maximum
    maximum = reduce(lambda a, b: a if a > b else b, numbers)
    print(f"   Max: {maximum}")

    # Build a string
    words = ['Hello', 'world', 'from', 'Python']
    sentence = reduce(lambda acc, word: acc + ' ' + word, words)
    print(f"   Sentence: {sentence.strip()}")

# ====================
# 7. Closures
# ====================

def make_counter():
    """Closure captures and modifies enclosed variable"""
    count = 0

    def increment():
        nonlocal count  # Needed to modify enclosed variable
        count += 1
        return count

    return increment

def make_bank_account(initial_balance: float):
    """Closure for encapsulation - private state"""
    balance = initial_balance

    def deposit(amount):
        nonlocal balance
        if amount > 0:
            balance += amount
        return balance

    def withdraw(amount):
        nonlocal balance
        if 0 < amount <= balance:
            balance -= amount
            return balance
        return balance

    def get_balance():
        return balance

    return deposit, withdraw, get_balance

# ====================
# 8. Partial Application
# ====================

def demonstrate_partial():
    """Partial application fixes some arguments"""

    def power(base, exponent):
        return base ** exponent

    # Create specialized functions
    square = partial(power, exponent=2)
    cube = partial(power, exponent=3)

    print(f"   square(5) = {square(5)}")
    print(f"   cube(5) = {cube(5)}")

    # Partial with multiple arguments
    def greet(greeting, name):
        return f"{greeting}, {name}!"

    say_hello = partial(greet, "Hello")
    say_goodbye = partial(greet, "Goodbye")

    print(f"   {say_hello('Alice')}")
    print(f"   {say_goodbye('Bob')}")

# ====================
# 9. Function Composition
# ====================

def compose(*functions):
    """Compose functions right-to-left: compose(f, g, h)(x) = f(g(h(x)))"""
    def inner(arg):
        result = arg
        for func in reversed(functions):
            result = func(result)
        return result
    return inner

def pipe(*functions):
    """Compose functions left-to-right: pipe(f, g, h)(x) = h(g(f(x)))"""
    def inner(arg):
        result = arg
        for func in functions:
            result = func(result)
        return result
    return inner

# ====================
# 10. Common HOFs
# ====================

def demonstrate_common_hofs():
    """Other useful higher-order functions"""
    numbers = [1, 2, 3, 4, 5]

    # all - all elements satisfy predicate?
    all_positive = all(x > 0 for x in numbers)
    print(f"   All positive? {all_positive}")

    # any - any element satisfies predicate?
    has_even = any(x % 2 == 0 for x in numbers)
    print(f"   Has even? {has_even}")

    # sorted with key function
    words = ['apple', 'pie', 'zoo', 'a']
    by_length = sorted(words, key=len)
    print(f"   Sorted by length: {by_length}")

    # max/min with key function
    longest = max(words, key=len)
    print(f"   Longest word: {longest}")

    # zip - combine multiple sequences
    list1 = [1, 2, 3]
    list2 = ['a', 'b', 'c']
    zipped = list(zip(list1, list2))
    print(f"   Zipped: {zipped}")

# ====================
# 11. Decorators (HOFs for Metaprogramming)
# ====================

def timer(func):
    """Decorator to time function execution"""
    import time

    def wrapper(*args, **kwargs):
        start = time.time()
        result = func(*args, **kwargs)
        end = time.time()
        print(f"   {func.__name__} took {end - start:.4f} seconds")
        return result
    return wrapper

def memoize(func):
    """Decorator to cache function results"""
    cache = {}

    def wrapper(*args):
        if args not in cache:
            cache[args] = func(*args)
        return cache[args]
    return wrapper

@memoize
def fibonacci(n):
    """Fibonacci with memoization"""
    if n <= 1:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)

# ====================
# 12. Custom HOFs
# ====================

def custom_map(func: Callable, iterable):
    """Our own implementation of map"""
    result = []
    for item in iterable:
        result.append(func(item))
    return result

def custom_filter(predicate: Callable, iterable):
    """Our own implementation of filter"""
    result = []
    for item in iterable:
        if predicate(item):
            result.append(item)
    return result

def custom_reduce(func: Callable, iterable, initial=None):
    """Our own implementation of reduce"""
    it = iter(iterable)
    if initial is None:
        value = next(it)
    else:
        value = initial

    for item in it:
        value = func(value, item)
    return value

# ====================
# 13. Real-World Example: Data Pipeline
# ====================

def process_data(data: List[dict]) -> List[dict]:
    """Example data processing pipeline"""

    # Filter valid entries
    valid = filter(lambda x: x.get('age', 0) > 0, data)

    # Transform data
    normalized = map(lambda x: {
        'name': x['name'].strip().title(),
        'age': x['age'],
        'category': 'adult' if x['age'] >= 18 else 'minor'
    }, valid)

    # Sort by age
    sorted_data = sorted(normalized, key=lambda x: x['age'])

    return list(sorted_data)

# ====================
# Main Demonstration
# ====================

def main():
    print("=== Higher-Order Functions in Python ===\n")

    # 1. First-class functions
    print("1. Functions as First-Class Values:")
    demonstrate_first_class()

    # 2. Functions taking functions
    print("\n2. Functions Taking Functions:")
    print(f"   apply_twice(lambda x: x + 1, 5) = {apply_twice(lambda x: x + 1, 5)}")
    print(f"   apply_n_times(lambda x: x * 2, 3, 2) = {apply_n_times(lambda x: x * 2, 3, 2)}")

    # 3. Functions returning functions
    print("\n3. Functions Returning Functions:")
    times_three = make_multiplier(3)
    add_ten = make_adder(10)
    print(f"   times_three(7) = {times_three(7)}")
    print(f"   add_ten(5) = {add_ten(5)}")

    # 4. Map
    print("\n4. Map - Transform Each Element:")
    demonstrate_map()

    # 5. Filter
    print("\n5. Filter - Select Elements:")
    demonstrate_filter()

    # 6. Reduce
    print("\n6. Reduce - Combine to Single Value:")
    demonstrate_reduce()

    # 7. Closures
    print("\n7. Closures:")
    counter = make_counter()
    print(f"   counter() = {counter()}")
    print(f"   counter() = {counter()}")
    print(f"   counter() = {counter()}")

    deposit, withdraw, get_balance = make_bank_account(1000)
    print(f"   Initial balance: ${get_balance()}")
    deposit(500)
    print(f"   After deposit: ${get_balance()}")
    withdraw(200)
    print(f"   After withdrawal: ${get_balance()}")

    # 8. Partial application
    print("\n8. Partial Application:")
    demonstrate_partial()

    # 9. Function composition
    print("\n9. Function Composition:")
    add_one = lambda x: x + 1
    double = lambda x: x * 2
    square = lambda x: x ** 2

    # Right-to-left composition
    f = compose(double, add_one)
    print(f"   compose(double, add_one)(5) = {f(5)}")  # (5 + 1) * 2 = 12

    # Left-to-right pipeline
    g = pipe(add_one, double, square)
    print(f"   pipe(add_one, double, square)(5) = {g(5)}")  # ((5 + 1) * 2) ** 2 = 144

    # 10. Common HOFs
    print("\n10. Common Higher-Order Functions:")
    demonstrate_common_hofs()

    # 11. Decorators
    print("\n11. Decorators:")
    print(f"   fibonacci(10) = {fibonacci(10)}")
    print(f"   fibonacci(10) again (cached) = {fibonacci(10)}")

    # 12. Custom implementations
    print("\n12. Custom HOF Implementations:")
    numbers = [1, 2, 3, 4, 5]
    print(f"   custom_map(lambda x: x*2, {numbers}) = {custom_map(lambda x: x*2, numbers)}")
    print(f"   custom_filter(lambda x: x%2==0, {numbers}) = {custom_filter(lambda x: x%2==0, numbers)}")
    print(f"   custom_reduce(lambda a,b: a+b, {numbers}, 0) = {custom_reduce(lambda a, b: a+b, numbers, 0)}")

    # 13. Real-world example
    print("\n13. Real-World Data Pipeline:")
    data = [
        {'name': ' alice ', 'age': 25},
        {'name': 'BOB', 'age': 17},
        {'name': 'charlie ', 'age': -1},  # Invalid
        {'name': 'DIANA', 'age': 30},
    ]
    processed = process_data(data)
    for person in processed:
        print(f"   {person}")

    # 14. Chaining example
    print("\n14. Method Chaining Example:")
    result = (
        [1, -2, 3, -4, 5, 6, 7, 8, 9, 10]
    )
    result = list(filter(lambda x: x > 0, result))
    result = list(map(lambda x: x ** 2, result))
    result = sum(result)
    print(f"   Sum of squares of positive numbers: {result}")


if __name__ == "__main__":
    main()
