#!/usr/bin/env python3
"""
Lesson 3: Control Flow in Python
Demonstrates conditionals, loops, and boolean logic
"""

def main():
    print("=== Python Control Flow ===\n")

    # 1. Basic conditionals
    print("1. Basic Conditionals:")
    age = 20
    if age >= 18:
        print(f"  Age {age}: Adult")
    elif age >= 13:
        print(f"  Age {age}: Teenager")
    else:
        print(f"  Age {age}: Child")

    # Ternary expression
    status = "Adult" if age >= 18 else "Minor"
    print(f"  Status (ternary): {status}")

    # 2. For loops
    print("\n2. For Loops:")
    print("  Count to 5:")
    for i in range(5):
        print(f"    {i}", end=" ")
    print()

    print("  Iterate list:")
    fruits = ["apple", "banana", "cherry"]
    for fruit in fruits:
        print(f"    {fruit}")

    print("  Enumerate (with index):")
    for i, fruit in enumerate(fruits):
        print(f"    {i}: {fruit}")

    # 3. While loop
    print("\n3. While Loop:")
    count = 0
    while count < 3:
        print(f"  Count: {count}")
        count += 1

    # 4. Boolean logic and truthiness
    print("\n4. Boolean Logic:")
    x, y = 5, 10
    print(f"  x={x}, y={y}")
    print(f"  x > 3 and y < 20: {x > 3 and y < 20}")
    print(f"  x > 10 or y > 5: {x > 10 or y > 5}")
    print(f"  not (x == y): {not (x == y)}")

    print("\n  Truthiness (Python's flexible approach):")
    print("  Falsy values: False, None, 0, '', [], {}, ()")
    values = [True, False, 0, 1, "", "hello", [], [1, 2], None]
    for val in values:
        truthy = "truthy" if val else "falsy"
        print(f"    {repr(val):10} : {truthy}")

    # 5. FizzBuzz
    print("\n5. FizzBuzz (1-20):")
    fizzbuzz_output = []
    for i in range(1, 21):
        if i % 15 == 0:
            fizzbuzz_output.append("FizzBuzz")
        elif i % 3 == 0:
            fizzbuzz_output.append("Fizz")
        elif i % 5 == 0:
            fizzbuzz_output.append("Buzz")
        else:
            fizzbuzz_output.append(str(i))
    print("  " + " ".join(fizzbuzz_output))

    # 6. List comprehensions (functional style)
    print("\n6. List Comprehensions:")
    squares = [x * x for x in range(10)]
    print(f"  Squares 0-9: {squares}")

    evens = [x for x in range(20) if x % 2 == 0]
    print(f"  Even numbers 0-19: {evens}")

    # 7. Pattern matching (Python 3.10+)
    print("\n7. Pattern Matching (Python 3.10+):")

    def describe_point(point):
        match point:
            case (0, 0):
                return "Origin"
            case (0, y):
                return f"Y-axis at {y}"
            case (x, 0):
                return f"X-axis at {x}"
            case (x, y):
                return f"Point at ({x}, {y})"

    try:
        print(f"  (0, 0): {describe_point((0, 0))}")
        print(f"  (0, 5): {describe_point((0, 5))}")
        print(f"  (3, 4): {describe_point((3, 4))}")
    except SyntaxError:
        print("  (Pattern matching requires Python 3.10+)")

    # 8. Break and continue
    print("\n8. Break and Continue:")
    print("  Finding first even number:")
    for i in range(10):
        if i % 2 == 0:
            print(f"    Found: {i}")
            break

    print("  Skipping odd numbers:")
    for i in range(10):
        if i % 2 != 0:
            continue
        print(f"    {i}", end=" ")
    print()

    # 9. Multiple conditions
    print("\n9. Multiple Conditions (Grade Calculator):")

    def letter_grade(score):
        if score >= 90:
            return "A"
        elif score >= 80:
            return "B"
        elif score >= 70:
            return "C"
        elif score >= 60:
            return "D"
        else:
            return "F"

    for score in [95, 85, 75, 65, 55]:
        print(f"  Score {score}: Grade {letter_grade(score)}")

    # 10. Nested loops
    print("\n10. Nested Loops (Multiplication Table for 5):")
    n = 5
    for i in range(1, 6):
        print(f"  {n} x {i} = {n * i}")

if __name__ == "__main__":
    main()
