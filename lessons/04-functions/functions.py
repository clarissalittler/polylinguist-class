#!/usr/bin/env python3
"""
Lesson 4: Functions in Python
Demonstrates function definition, parameters, scope, closures, and purity
"""

# ============================================
# 1. Basic Function Definition
# ============================================

def greet(name):
    """Simple function with one parameter"""
    return f"Hello, {name}!"

def add(x, y):
    """Function with multiple parameters"""
    return x + y

def square(x):
    """Pure function - same input, same output"""
    return x * x

# ============================================
# 2. Parameters and Arguments
# ============================================

def describe_person(name, age, city="Unknown"):
    """Function with default parameter"""
    return f"{name} is {age} years old and lives in {city}"

def print_info(*args, **kwargs):
    """Variadic function with *args and **kwargs"""
    print("Positional arguments:", args)
    print("Keyword arguments:", kwargs)

# ============================================
# 3. Return Values
# ============================================

def divide(x, y):
    """Function with multiple return values"""
    if y == 0:
        return None, "Cannot divide by zero"
    return x / y, None

def no_return():
    """Function with no explicit return (returns None)"""
    print("This function doesn't return a value")

# ============================================
# 4. Scope and Closures
# ============================================

global_var = "I'm global"

def scope_demo():
    """Demonstrates variable scope"""
    local_var = "I'm local"
    print(f"Inside function: {local_var}")
    print(f"Can access global: {global_var}")

def make_multiplier(factor):
    """Closure: inner function captures 'factor'"""
    def multiply(x):
        return x * factor
    return multiply

def make_counter():
    """Closure for maintaining state"""
    count = 0

    def increment():
        nonlocal count  # Modify enclosing scope variable
        count += 1
        return count

    return increment

# ============================================
# 5. Pure vs Impure Functions
# ============================================

# Pure function
def pure_add(x, y):
    """Pure: same inputs, same output, no side effects"""
    return x + y

# Impure function (side effect)
def impure_print(message):
    """Impure: has side effect (I/O)"""
    print(message)

# Impure function (depends on external state)
total = 0
def impure_add_to_total(x):
    """Impure: modifies global state"""
    global total
    total += x
    return total

# Impure function (non-deterministic)
import random
def impure_random():
    """Impure: different output for same input"""
    return random.randint(1, 100)

# ============================================
# 6. First-Class Functions
# ============================================

def apply_operation(operation, x, y):
    """Higher-order function: takes function as parameter"""
    return operation(x, y)

def compose(f, g):
    """Function composition: returns new function"""
    return lambda x: f(g(x))

# ============================================
# 7. Anonymous Functions (Lambdas)
# ============================================

# Lambda examples
double = lambda x: x * 2
is_even = lambda x: x % 2 == 0
add_lambda = lambda x, y: x + y

# ============================================
# 8. Higher-Order Functions
# ============================================

def apply_twice(f, x):
    """Apply function twice"""
    return f(f(x))

def apply_n_times(f, x, n):
    """Apply function n times"""
    result = x
    for _ in range(n):
        result = f(result)
    return result

# ============================================
# Main Program
# ============================================

def main():
    print("=== Python Functions ===\n")

    # 1. Basic functions
    print("1. Basic Functions:")
    print(f"  greet('Alice'): {greet('Alice')}")
    print(f"  add(5, 3): {add(5, 3)}")
    print(f"  square(7): {square(7)}")

    # 2. Parameters
    print("\n2. Parameters and Arguments:")
    print(f"  describe_person('Alice', 30): {describe_person('Alice', 30)}")
    print(f"  describe_person('Bob', 25, 'NYC'): {describe_person('Bob', 25, 'NYC')}")
    print("\n  Variadic function:")
    print_info(1, 2, 3, name="Alice", age=30)

    # 3. Multiple returns
    print("\n3. Multiple Return Values:")
    result, error = divide(10, 2)
    print(f"  divide(10, 2): {result}, error: {error}")
    result, error = divide(10, 0)
    print(f"  divide(10, 0): {result}, error: {error}")

    # 4. Closures
    print("\n4. Closures:")
    times_two = make_multiplier(2)
    times_three = make_multiplier(3)
    print(f"  times_two(5): {times_two(5)}")
    print(f"  times_three(5): {times_three(5)}")

    print("\n  Counter (stateful closure):")
    counter1 = make_counter()
    counter2 = make_counter()
    print(f"  counter1(): {counter1()}")  # 1
    print(f"  counter1(): {counter1()}")  # 2
    print(f"  counter2(): {counter2()}")  # 1
    print(f"  counter1(): {counter1()}")  # 3

    # 5. Pure vs Impure
    print("\n5. Pure vs Impure:")
    print(f"  pure_add(5, 3): {pure_add(5, 3)}")
    print("  impure_print('Hello'): ", end="")
    impure_print("Hello")
    print(f"  impure_add_to_total(5): {impure_add_to_total(5)}")
    print(f"  impure_add_to_total(5): {impure_add_to_total(5)}")  # Different result!

    # 6. First-class functions
    print("\n6. First-Class Functions:")
    print(f"  apply_operation(add, 5, 3): {apply_operation(add, 5, 3)}")
    print(f"  apply_operation(lambda x, y: x * y, 5, 3): {apply_operation(lambda x, y: x * y, 5, 3)}")

    # 7. Function composition
    print("\n7. Function Composition:")
    increment = lambda x: x + 1
    square_then_increment = compose(increment, square)
    print(f"  square_then_increment(5): {square_then_increment(5)}")  # (5^2) + 1 = 26

    # 8. Lambdas
    print("\n8. Anonymous Functions (Lambdas):")
    print(f"  double(5): {double(5)}")
    print(f"  is_even(4): {is_even(4)}")
    print(f"  is_even(5): {is_even(5)}")

    # 9. Higher-order functions with map/filter
    print("\n9. Higher-Order Functions (map/filter/reduce):")
    numbers = [1, 2, 3, 4, 5]
    print(f"  numbers: {numbers}")
    print(f"  map(square, numbers): {list(map(square, numbers))}")
    print(f"  filter(is_even, numbers): {list(filter(is_even, numbers))}")

    from functools import reduce
    print(f"  reduce(add, numbers): {reduce(add, numbers)}")

    # 10. Apply n times
    print("\n10. Apply Function Multiple Times:")
    print(f"  apply_twice(double, 5): {apply_twice(double, 5)}")  # 5 * 2 * 2 = 20
    print(f"  apply_n_times(double, 5, 3): {apply_n_times(double, 5, 3)}")  # 5 * 2^3 = 40

if __name__ == "__main__":
    main()
