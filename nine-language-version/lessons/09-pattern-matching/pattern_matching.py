#!/usr/bin/env python3
"""
Lesson 9: Pattern Matching in Python

Python 3.10+ introduced structural pattern matching with the match statement.
This provides powerful pattern matching capabilities similar to functional languages.

Note: This requires Python 3.10 or later!
"""

import sys

# Check Python version
if sys.version_info < (3, 10):
    print("This code requires Python 3.10 or later for pattern matching!")
    print(f"Current version: {sys.version}")
    sys.exit(1)

from dataclasses import dataclass
from typing import Union

# ====================
# 1. Basic Literal Matching
# ====================

def describe_number(n: int) -> str:
    """Match against literal values"""
    match n:
        case 0:
            return "zero"
        case 1:
            return "one"
        case 2:
            return "two"
        case _:  # Wildcard pattern
            return f"many: {n}"

# ====================
# 2. Tuple/Sequence Patterns
# ====================

def describe_point(point: tuple) -> str:
    """Match and destructure tuples"""
    match point:
        case (0, 0):
            return "origin"
        case (0, y):
            return f"on y-axis at y={y}"
        case (x, 0):
            return f"on x-axis at x={x}"
        case (x, y):
            return f"point at ({x}, {y})"

def describe_list(lst: list) -> str:
    """Match list patterns"""
    match lst:
        case []:
            return "empty list"
        case [x]:
            return f"single element: {x}"
        case [x, y]:
            return f"two elements: {x}, {y}"
        case [first, *rest]:  # Head and tail
            return f"first: {first}, rest: {rest}"

# ====================
# 3. Dictionary/Mapping Patterns
# ====================

def process_command(cmd: dict) -> str:
    """Match dictionary structures"""
    match cmd:
        case {"action": "quit"}:
            return "Quitting..."
        case {"action": "move", "direction": dir}:
            return f"Moving {dir}"
        case {"action": "attack", "target": target, "damage": dmg}:
            return f"Attacking {target} for {dmg} damage"
        case {"action": action}:
            return f"Unknown action: {action}"
        case _:
            return "Invalid command"

# ====================
# 4. Class Patterns
# ====================

@dataclass
class Point:
    x: float
    y: float

@dataclass
class Circle:
    center: Point
    radius: float

@dataclass
class Rectangle:
    top_left: Point
    width: float
    height: float

def describe_shape(shape) -> str:
    """Match against class instances"""
    match shape:
        case Circle(center=Point(0, 0), radius=r):
            return f"Circle at origin with radius {r}"
        case Circle(center=Point(x, y), radius=r):
            return f"Circle at ({x}, {y}) with radius {r}"
        case Rectangle(top_left=Point(x, y), width=w, height=h):
            return f"Rectangle at ({x}, {y}), {w}x{h}"
        case _:
            return "Unknown shape"

# ====================
# 5. Guards (Conditional Patterns)
# ====================

def classify_number(n: int) -> str:
    """Use guards for conditional matching"""
    match n:
        case n if n < 0:
            return "negative"
        case 0:
            return "zero"
        case n if n < 10:
            return "small positive"
        case n if n < 100:
            return "medium positive"
        case _:
            return "large positive"

# ====================
# 6. Or Patterns
# ====================

def is_weekend(day: str) -> bool:
    """Match multiple patterns with |"""
    match day.lower():
        case "saturday" | "sunday":
            return True
        case _:
            return False

# ====================
# 7. As Patterns (Capture)
# ====================

def process_data(data) -> str:
    """Capture matched values with 'as'"""
    match data:
        case [x, y] as pair:
            return f"Pair {pair}: sum = {x + y}"
        case [x, y, z] as triple:
            return f"Triple {triple}: sum = {x + y + z}"
        case other:
            return f"Other: {other}"

# ====================
# 8. Nested Patterns
# ====================

def analyze_nested(data) -> str:
    """Match nested structures"""
    match data:
        case {"user": {"name": name, "age": age}, "active": True}:
            return f"Active user: {name} ({age})"
        case {"user": {"name": name}, "active": False}:
            return f"Inactive user: {name}"
        case {"error": message}:
            return f"Error: {message}"
        case _:
            return "Unknown data"

# ====================
# 9. Type Patterns with Union
# ====================

Value = Union[int, float, str, list]

def describe_value(val: Value) -> str:
    """Match based on type"""
    match val:
        case int(n):
            return f"Integer: {n}"
        case float(f):
            return f"Float: {f:.2f}"
        case str(s):
            return f"String: '{s}'"
        case list(items):
            return f"List with {len(items)} items"
        case _:
            return "Unknown type"

# ====================
# 10. Practical Example: Expression Evaluator
# ====================

@dataclass
class Num:
    value: int

@dataclass
class Add:
    left: 'Expr'
    right: 'Expr'

@dataclass
class Mul:
    left: 'Expr'
    right: 'Expr'

@dataclass
class Neg:
    expr: 'Expr'

Expr = Union[Num, Add, Mul, Neg]

def eval_expr(expr: Expr) -> int:
    """Evaluate arithmetic expressions using pattern matching"""
    match expr:
        case Num(value=n):
            return n
        case Add(left=l, right=r):
            return eval_expr(l) + eval_expr(r)
        case Mul(left=l, right=r):
            return eval_expr(l) * eval_expr(r)
        case Neg(expr=e):
            return -eval_expr(e)
        case _:
            raise ValueError(f"Unknown expression: {expr}")

# ====================
# 11. State Machine
# ====================

def traffic_light_next(current: str, action: str) -> str:
    """State machine using pattern matching"""
    match (current, action):
        case ("red", "timer"):
            return "green"
        case ("green", "timer"):
            return "yellow"
        case ("yellow", "timer"):
            return "red"
        case (state, "emergency"):
            return "red"
        case (state, _):
            return state  # No change for unknown actions

# ====================
# 12. JSON-like Data Processing
# ====================

def process_user(user_data: dict) -> str:
    """Process user data with various formats"""
    match user_data:
        case {"type": "admin", "name": name, "permissions": perms}:
            return f"Admin {name} with {len(perms)} permissions"
        case {"type": "user", "name": name, "age": age} if age >= 18:
            return f"Adult user: {name}"
        case {"type": "user", "name": name, "age": age}:
            return f"Minor user: {name}"
        case {"type": "guest", "session": session}:
            return f"Guest session: {session}"
        case _:
            return "Unknown user type"

# ====================
# Main Demonstration
# ====================

def main():
    print("=== Pattern Matching in Python 3.10+ ===\n")

    # 1. Basic literal matching
    print("1. Basic Literal Matching:")
    for n in [0, 1, 2, 5]:
        print(f"   describe_number({n}) = {describe_number(n)}")

    # 2. Tuple patterns
    print("\n2. Tuple Patterns:")
    points = [(0, 0), (0, 5), (3, 0), (2, 3)]
    for p in points:
        print(f"   {p} -> {describe_point(p)}")

    # 3. List patterns
    print("\n3. List Patterns:")
    lists = [[], [1], [1, 2], [1, 2, 3, 4]]
    for lst in lists:
        print(f"   {lst} -> {describe_list(lst)}")

    # 4. Dictionary patterns
    print("\n4. Dictionary Patterns:")
    commands = [
        {"action": "quit"},
        {"action": "move", "direction": "north"},
        {"action": "attack", "target": "orc", "damage": 10},
    ]
    for cmd in commands:
        print(f"   {cmd}")
        print(f"   -> {process_command(cmd)}")

    # 5. Class patterns
    print("\n5. Class Patterns:")
    shapes = [
        Circle(Point(0, 0), 5),
        Circle(Point(10, 20), 3),
        Rectangle(Point(0, 0), 100, 50),
    ]
    for shape in shapes:
        print(f"   {describe_shape(shape)}")

    # 6. Guards
    print("\n6. Guards (Conditional Patterns):")
    numbers = [-5, 0, 3, 50, 500]
    for n in numbers:
        print(f"   {n} -> {classify_number(n)}")

    # 7. Or patterns
    print("\n7. Or Patterns:")
    days = ["Monday", "Saturday", "Sunday", "Wednesday"]
    for day in days:
        print(f"   {day} is weekend? {is_weekend(day)}")

    # 8. As patterns
    print("\n8. As Patterns:")
    data_items = [[1, 2], [3, 4, 5], "other"]
    for item in data_items:
        print(f"   {process_data(item)}")

    # 9. Nested patterns
    print("\n9. Nested Patterns:")
    nested_data = [
        {"user": {"name": "Alice", "age": 30}, "active": True},
        {"user": {"name": "Bob"}, "active": False},
        {"error": "Not found"},
    ]
    for data in nested_data:
        print(f"   {analyze_nested(data)}")

    # 10. Type patterns
    print("\n10. Type Patterns:")
    values = [42, 3.14, "hello", [1, 2, 3]]
    for val in values:
        print(f"   {describe_value(val)}")

    # 11. Expression evaluator
    print("\n11. Expression Evaluator:")
    # (2 + 3) * 4 = 20
    expr = Mul(Add(Num(2), Num(3)), Num(4))
    print(f"   (2 + 3) * 4 = {eval_expr(expr)}")

    # -(5 + 3) = -8
    expr2 = Neg(Add(Num(5), Num(3)))
    print(f"   -(5 + 3) = {eval_expr(expr2)}")

    # 12. State machine
    print("\n12. State Machine (Traffic Light):")
    state = "red"
    for action in ["timer", "timer", "timer", "emergency"]:
        state = traffic_light_next(state, action)
        print(f"   After '{action}': {state}")

    # 13. User data processing
    print("\n13. JSON-like Data Processing:")
    users = [
        {"type": "admin", "name": "Alice", "permissions": ["read", "write"]},
        {"type": "user", "name": "Bob", "age": 25},
        {"type": "user", "name": "Charlie", "age": 16},
        {"type": "guest", "session": "abc123"},
    ]
    for user in users:
        print(f"   {process_user(user)}")

if __name__ == "__main__":
    main()
