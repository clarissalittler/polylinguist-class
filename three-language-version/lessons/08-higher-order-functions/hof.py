"""
Lesson 8: Higher-Order Functions in Python
Demonstrates functions as first-class citizens and functional programming patterns
"""

from functools import reduce, partial
from typing import Callable, List, TypeVar, Any
import time

T = TypeVar('T')
U = TypeVar('U')

print("=" * 70)
print("LESSON 8: HIGHER-ORDER FUNCTIONS IN PYTHON")
print("=" * 70)

# ============================================================================
# PART 1: FUNCTIONS AS FIRST-CLASS CITIZENS
# ============================================================================

print("\n--- PART 1: FIRST-CLASS FUNCTIONS ---")

# Assign functions to variables
square = lambda x: x ** 2
cube = lambda x: x ** 3

# Store in data structures
operations = [square, cube, lambda x: x * 2]

# Pass as arguments and use
numbers = [1, 2, 3, 4, 5]
print(f"Original: {numbers}")
print(f"Squared: {list(map(square, numbers))}")
print(f"All operations on 5: {[op(5) for op in operations]}")

# ============================================================================
# PART 2: MAP, FILTER, REDUCE
# ============================================================================

print("\n--- PART 2: MAP, FILTER, REDUCE ---")

# Map - transform each element
doubled = list(map(lambda x: x * 2, numbers))
print(f"Map (double): {doubled}")

# Filter - select elements
evens = list(filter(lambda x: x % 2 == 0, numbers))
print(f"Filter (evens): {evens}")

# Reduce - combine elements
sum_all = reduce(lambda acc, x: acc + x, numbers, 0)
product = reduce(lambda acc, x: acc * x, numbers, 1)
print(f"Reduce (sum): {sum_all}")
print(f"Reduce (product): {product}")

# Combined pipeline
result = reduce(
    lambda acc, x: acc + x,
    map(lambda x: x ** 2,
        filter(lambda x: x % 2 == 0, numbers)),
    0
)
print(f"Sum of squares of evens: {result}")

# ============================================================================
# PART 3: HIGHER-ORDER FUNCTION EXAMPLES
# ============================================================================

print("\n--- PART 3: HIGHER-ORDER FUNCTIONS ---")

def apply_twice(func: Callable, x: Any) -> Any:
    """Apply function twice"""
    return func(func(x))

def compose(f: Callable, g: Callable) -> Callable:
    """Compose two functions: f(g(x))"""
    return lambda x: f(g(x))

def pipe(*functions):
    """Compose functions left to right"""
    def inner(x):
        result = x
        for f in functions:
            result = f(result)
        return result
    return inner

print(f"apply_twice(lambda x: x + 1, 5) = {apply_twice(lambda x: x + 1, 5)}")

add_one = lambda x: x + 1
times_two = lambda x: x * 2
composed = compose(times_two, add_one)
print(f"compose(times_two, add_one)(5) = {composed(5)}")  # (5+1)*2 = 12

piped = pipe(add_one, times_two, lambda x: x - 3)
print(f"pipe(add_one, times_two, minus_three)(5) = {piped(5)}")  # ((5+1)*2)-3 = 9

# ============================================================================
# PART 4: FUNCTIONS RETURNING FUNCTIONS
# ============================================================================

print("\n--- PART 4: FUNCTIONS RETURNING FUNCTIONS ---")

def make_multiplier(n: int) -> Callable[[int], int]:
    """Returns a function that multiplies by n"""
    return lambda x: x * n

def make_power(exponent: int) -> Callable[[int], int]:
    """Returns a function that raises to exponent"""
    return lambda x: x ** exponent

times_three = make_multiplier(3)
times_five = make_multiplier(5)
print(f"times_three(10) = {times_three(10)}")
print(f"times_five(4) = {times_five(4)}")

square_func = make_power(2)
cube_func = make_power(3)
print(f"square_func(5) = {square_func(5)}")
print(f"cube_func(3) = {cube_func(3)}")

# ============================================================================
# PART 5: CLOSURES
# ============================================================================

print("\n--- PART 5: CLOSURES ---")

def make_counter(start: int = 0):
    """Returns a counter function with enclosed state"""
    count = start

    def increment():
        nonlocal count
        count += 1
        return count

    return increment

counter1 = make_counter(0)
counter2 = make_counter(10)

print(f"counter1: {counter1()}, {counter1()}, {counter1()}")
print(f"counter2: {counter2()}, {counter2()}")
print(f"counter1: {counter1()}")  # Independent state

# ============================================================================
# PART 6: PARTIAL APPLICATION
# ============================================================================

print("\n--- PART 6: PARTIAL APPLICATION ---")

def power(base: int, exponent: int) -> int:
    return base ** exponent

# Manual currying
def curried_power(base):
    return lambda exponent: base ** exponent

# Using functools.partial
square_partial = partial(power, exponent=2)
cube_partial = partial(power, exponent=3)

print(f"square_partial(5) = {square_partial(5)}")
print(f"cube_partial(3) = {cube_partial(3)}")

curried_square = curried_power(2)
print(f"curried_square(5) = {curried_square(5)}")

# ============================================================================
# PART 7: PRACTICAL EXAMPLES
# ============================================================================

print("\n--- PART 7: PRACTICAL EXAMPLES ---")

# Custom sorting
people = [
    {"name": "Alice", "age": 30},
    {"name": "Bob", "age": 25},
    {"name": "Charlie", "age": 35}
]

sorted_by_age = sorted(people, key=lambda p: p["age"])
sorted_by_name = sorted(people, key=lambda p: p["name"])
print(f"Sorted by age: {[p['name'] for p in sorted_by_age]}")
print(f"Sorted by name: {[p['name'] for p in sorted_by_name]}")

# Data pipeline
students = [
    {"name": "Alice", "grades": [85, 90, 88]},
    {"name": "Bob", "grades": [78, 85, 80]},
    {"name": "Charlie", "grades": [92, 95, 90]}
]

high_performers = list(
    map(lambda s: s["name"],
        filter(lambda s: sum(s["grades"]) / len(s["grades"]) >= 85,
               students))
)
print(f"High performers (avg >= 85): {high_performers}")

# ============================================================================
# PART 8: DECORATORS (HIGHER-ORDER FUNCTIONS FOR FUNCTIONS)
# ============================================================================

print("\n--- PART 8: DECORATORS ---")

def timer(func):
    """Decorator to time function execution"""
    def wrapper(*args, **kwargs):
        start = time.time()
        result = func(*args, **kwargs)
        end = time.time()
        print(f"{func.__name__} took {end - start:.4f} seconds")
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

@timer
def slow_sum(n):
    return sum(range(n))

@memoize
def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n - 1) + fibonacci(n - 2)

print(f"slow_sum(1000000): {slow_sum(1000000)}")
print(f"fibonacci(30) = {fibonacci(30)}")
print(f"fibonacci(30) again (cached) = {fibonacci(30)}")

# ============================================================================
# PART 9: FUNCTION COMPOSITION
# ============================================================================

print("\n--- PART 9: ADVANCED COMPOSITION ---")

def compose_multiple(*functions):
    """Compose multiple functions right to left"""
    def inner(arg):
        result = arg
        for f in reversed(functions):
            result = f(result)
        return result
    return inner

# Process data through multiple transformations
process = compose_multiple(
    lambda x: x * 2,      # Last: double
    lambda x: x + 10,     # Second: add 10
    lambda x: x ** 2      # First: square
)

print(f"process(5) = {process(5)}")  # ((5^2) + 10) * 2 = 70

# ============================================================================
# PART 10: REAL-WORLD EXAMPLE
# ============================================================================

print("\n--- PART 10: REAL-WORLD DATA PROCESSING ---")

transactions = [
    {"id": 1, "amount": 50.00, "category": "food"},
    {"id": 2, "amount": 30.00, "category": "transport"},
    {"id": 3, "amount": 20.00, "category": "food"},
    {"id": 4, "amount": 100.00, "category": "entertainment"},
    {"id": 5, "amount": 15.00, "category": "food"},
]

# Total food spending
food_total = reduce(
    lambda acc, t: acc + t["amount"],
    filter(lambda t: t["category"] == "food", transactions),
    0
)

# Average transaction amount
avg_amount = sum(t["amount"] for t in transactions) / len(transactions)

# Transform: add 10% tax
with_tax = list(map(
    lambda t: {**t, "amount": t["amount"] * 1.1},
    transactions
))

print(f"Total food spending: ${food_total:.2f}")
print(f"Average transaction: ${avg_amount:.2f}")
print(f"First transaction with tax: ${with_tax[0]['amount']:.2f}")

print("\n" + "=" * 70)
print("Higher-order functions enable:")
print("  - Code reuse through abstraction")
print("  - Declarative data transformations")
print("  - Function composition for complex logic")
print("  - Elegant solutions to complex problems")
print("=" * 70)
