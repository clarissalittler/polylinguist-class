#!/usr/bin/env python3
"""
Lesson 10: Type Systems in Python

Python is dynamically typed, but Python 3.5+ added optional type hints:
- Dynamic typing at runtime
- Optional static type hints
- Gradual typing (mix typed and untyped code)
- Generic types
- Protocol types (structural subtyping)
- Type checking with mypy/pyright
- Runtime type checking with pydantic

This demonstrates Python's type system features.
"""

from typing import (
    List, Dict, Tuple, Optional, Union, Any, TypeVar, Generic,
    Callable, Protocol, cast, overload, Literal, Final
)
from dataclasses import dataclass
from enum import Enum
import sys

# ====================
# 1. Basic Type Hints
# ====================

def increment(x: int) -> int:
    """Simple function with type hints."""
    return x + 1

def add(x: int, y: int) -> int:
    """Function with multiple parameters."""
    return x + y

# Without type hints (still valid Python)
def multiply(x, y):
    return x * y

# ====================
# 2. Collection Types
# ====================

def sum_list(numbers: List[int]) -> int:
    """List of specific type."""
    return sum(numbers)

def create_mapping() -> Dict[str, int]:
    """Dictionary with string keys and int values."""
    return {"one": 1, "two": 2, "three": 3}

def get_coordinates() -> Tuple[float, float]:
    """Tuple with fixed types."""
    return (3.14, 2.71)

# Modern Python 3.9+ syntax (no imports needed)
def modern_list(items: list[int]) -> list[int]:
    """Can use list directly instead of List."""
    return [x * 2 for x in items]

# ====================
# 3. Optional and Union Types
# ====================

def safe_divide(x: float, y: float) -> Optional[float]:
    """Returns None for division by zero."""
    if y == 0:
        return None
    return x / y

def parse_value(s: str) -> Union[int, float, str]:
    """Can return multiple types."""
    try:
        return int(s)
    except ValueError:
        try:
            return float(s)
        except ValueError:
            return s

# Modern Python 3.10+ syntax
def safe_get(items: list[int], index: int) -> int | None:
    """Use | for union types."""
    if 0 <= index < len(items):
        return items[index]
    return None

# ====================
# 4. Type Aliases
# ====================

# Simple alias
UserId = int
ProductId = int
Point = Tuple[float, float]

# Complex alias
UserData = Dict[str, Union[str, int, List[str]]]

def get_user(user_id: UserId) -> str:
    """Using type alias for clarity."""
    return f"User #{user_id}"

# ====================
# 5. Generic Types (TypeVar)
# ====================

T = TypeVar('T')

def identity(x: T) -> T:
    """Generic identity function."""
    return x

def first(items: List[T]) -> Optional[T]:
    """Get first element of any list."""
    return items[0] if items else None

def swap(pair: Tuple[T, T]) -> Tuple[T, T]:
    """Swap tuple elements."""
    return (pair[1], pair[0])

# Multiple type variables
U = TypeVar('U')

def pair(x: T, y: U) -> Tuple[T, U]:
    """Create a pair of any two types."""
    return (x, y)

# ====================
# 6. Generic Classes
# ====================

class Box(Generic[T]):
    """A box that can hold any type."""

    def __init__(self, value: T):
        self.value = value

    def get_value(self) -> T:
        return self.value

    def map(self, func: Callable[[T], U]) -> 'Box[U]':
        """Transform the value."""
        return Box(func(self.value))

# ====================
# 7. Dataclasses with Types
# ====================

@dataclass
class Person:
    """Person with typed fields."""
    name: str
    age: int
    email: Optional[str] = None

    def is_adult(self) -> bool:
        return self.age >= 18

@dataclass
class Point2D:
    """2D point."""
    x: float
    y: float

    def distance(self, other: 'Point2D') -> float:
        return ((self.x - other.x)**2 + (self.y - other.y)**2)**0.5

# ====================
# 8. Enums (Algebraic Data Types - Limited)
# ====================

class Color(Enum):
    """Enumeration of colors."""
    RED = "red"
    GREEN = "green"
    BLUE = "blue"

class TrafficLight(Enum):
    """Traffic light states."""
    RED = 1
    YELLOW = 2
    GREEN = 3

    def next(self) -> 'TrafficLight':
        """Get next state."""
        if self == TrafficLight.RED:
            return TrafficLight.GREEN
        elif self == TrafficLight.GREEN:
            return TrafficLight.YELLOW
        else:
            return TrafficLight.RED

# ====================
# 9. Protocols (Structural Subtyping)
# ====================

class Drawable(Protocol):
    """Protocol: any object with a draw method."""

    def draw(self) -> str:
        ...

class Circle:
    """Circle class (doesn't explicitly inherit Drawable)."""

    def __init__(self, radius: float):
        self.radius = radius

    def draw(self) -> str:
        return f"Drawing circle with radius {self.radius}"

class Rectangle:
    """Rectangle class."""

    def __init__(self, width: float, height: float):
        self.width = width
        self.height = height

    def draw(self) -> str:
        return f"Drawing rectangle {self.width}x{self.height}"

def render(shape: Drawable) -> None:
    """Accepts any object with a draw method."""
    print(f"   {shape.draw()}")

# ====================
# 10. Callable Types
# ====================

def apply_twice(func: Callable[[int], int], x: int) -> int:
    """Apply a function twice."""
    return func(func(x))

def make_multiplier(n: int) -> Callable[[int], int]:
    """Return a function that multiplies by n."""
    def multiplier(x: int) -> int:
        return x * n
    return multiplier

# ====================
# 11. Literal Types
# ====================

def process_mode(mode: Literal["read", "write", "append"]) -> str:
    """Only accepts specific string values."""
    return f"Processing in {mode} mode"

# ====================
# 12. Final Types
# ====================

# Constants
MAX_SIZE: Final = 100
API_KEY: Final[str] = "secret"

class BaseClass:
    """Class with final attribute."""

    def __init__(self):
        self.regular: int = 0
        self.constant: Final[int] = 42  # Should not be reassigned

# ====================
# 13. Overloading
# ====================

@overload
def process(x: int) -> str:
    ...

@overload
def process(x: str) -> int:
    ...

def process(x: Union[int, str]) -> Union[str, int]:
    """Different behavior based on input type."""
    if isinstance(x, int):
        return str(x)
    else:
        return len(x)

# ====================
# 14. Type Narrowing
# ====================

def describe_value(value: Union[int, str, List[int]]) -> str:
    """Type narrows based on isinstance checks."""
    if isinstance(value, int):
        # Here, type checker knows value is int
        return f"Integer: {value}"
    elif isinstance(value, str):
        # Here, type checker knows value is str
        return f"String: '{value}'"
    else:
        # Here, type checker knows value is List[int]
        return f"List of {len(value)} integers"

# ====================
# 15. Type Guards
# ====================

def is_string_list(val: List[Any]) -> bool:
    """Check if list contains only strings."""
    return all(isinstance(x, str) for x in val)

# ====================
# 16. NewType (Distinct Types)
# ====================

from typing import NewType

UserId = NewType('UserId', int)
ProductId = NewType('ProductId', int)

def get_user_name(user_id: UserId) -> str:
    """Only accepts UserId, not plain int."""
    return f"User #{user_id}"

# Usage:
# user_id = UserId(123)  # OK
# get_user_name(user_id)  # OK
# get_user_name(123)  # Type checker error!
# get_user_name(ProductId(123))  # Type checker error!

# ====================
# 17. Runtime Type Checking
# ====================

def validate_types(x: int, y: str) -> None:
    """Runtime type checking."""
    if not isinstance(x, int):
        raise TypeError(f"Expected int, got {type(x)}")
    if not isinstance(y, str):
        raise TypeError(f"Expected str, got {type(y)}")

# ====================
# 18. Complex Nested Types
# ====================

ApiResponse = Dict[str, Union[str, int, List[Dict[str, Any]]]]

def parse_api_response(response: ApiResponse) -> str:
    """Handle complex nested types."""
    return f"Parsed response with {len(response)} keys"

# ====================
# 19. Variance Annotations
# ====================

# Covariant type variable (for immutable containers)
T_co = TypeVar('T_co', covariant=True)

class ImmutableBox(Generic[T_co]):
    """Immutable box is covariant."""

    def __init__(self, value: T_co):
        self._value = value

    def get(self) -> T_co:
        return self._value

# ====================
# Main Demonstration
# ====================

def main() -> None:
    print("=== Type Systems in Python ===\n")

    # 1. Basic types
    print("1. Basic Type Hints:")
    print(f"   increment(5) = {increment(5)}")
    print(f"   add(3, 7) = {add(3, 7)}")
    print(f"   multiply(4, 5) = {multiply(4, 5)} (no type hints)")

    # 2. Collection types
    print("\n2. Collection Types:")
    print(f"   sum_list([1,2,3,4,5]) = {sum_list([1, 2, 3, 4, 5])}")
    print(f"   create_mapping() = {create_mapping()}")
    print(f"   get_coordinates() = {get_coordinates()}")

    # 3. Optional and Union
    print("\n3. Optional and Union Types:")
    print(f"   safe_divide(10, 2) = {safe_divide(10, 2)}")
    print(f"   safe_divide(10, 0) = {safe_divide(10, 0)}")
    print(f"   parse_value('42') = {parse_value('42')} (type: {type(parse_value('42')).__name__})")
    print(f"   parse_value('3.14') = {parse_value('3.14')} (type: {type(parse_value('3.14')).__name__})")

    # 4. Generics
    print("\n4. Generic Types:")
    print(f"   identity(42) = {identity(42)}")
    print(f"   identity('hello') = {identity('hello')}")
    print(f"   first([1,2,3]) = {first([1, 2, 3])}")
    print(f"   swap((1, 2)) = {swap((1, 2))}")

    # 5. Generic classes
    print("\n5. Generic Classes:")
    int_box = Box(42)
    str_box = Box("hello")
    print(f"   Box(42).get_value() = {int_box.get_value()}")
    print(f"   Box('hello').get_value() = {str_box.get_value()}")
    doubled = int_box.map(lambda x: x * 2)
    print(f"   Box(42).map(lambda x: x * 2) = {doubled.get_value()}")

    # 6. Dataclasses
    print("\n6. Dataclasses with Types:")
    person = Person("Alice", 30, "alice@example.com")
    print(f"   {person}")
    print(f"   is_adult() = {person.is_adult()}")

    p1 = Point2D(0, 0)
    p2 = Point2D(3, 4)
    print(f"   distance({p1}, {p2}) = {p1.distance(p2)}")

    # 7. Enums
    print("\n7. Enums:")
    light = TrafficLight.RED
    print(f"   Current: {light.name}")
    print(f"   Next: {light.next().name}")

    # 8. Protocols
    print("\n8. Protocols (Structural Subtyping):")
    circle = Circle(5)
    rectangle = Rectangle(4, 6)
    render(circle)
    render(rectangle)

    # 9. Callable types
    print("\n9. Callable Types:")
    print(f"   apply_twice(lambda x: x + 1, 5) = {apply_twice(lambda x: x + 1, 5)}")
    times_three = make_multiplier(3)
    print(f"   make_multiplier(3)(7) = {times_three(7)}")

    # 10. Literal types
    print("\n10. Literal Types:")
    print(f"   {process_mode('read')}")
    print(f"   {process_mode('write')}")

    # 11. Overloading
    print("\n11. Function Overloading:")
    print(f"   process(42) = '{process(42)}' (int -> str)")
    print(f"   process('hello') = {process('hello')} (str -> int)")

    # 12. Type narrowing
    print("\n12. Type Narrowing:")
    print(f"   {describe_value(42)}")
    print(f"   {describe_value('hello')}")
    print(f"   {describe_value([1, 2, 3])}")

    # 13. NewType
    print("\n13. NewType (Distinct Types):")
    user_id = UserId(123)
    print(f"   {get_user_name(user_id)}")

    print("\n=== Python Type System Features ===")
    print("- Dynamic typing at runtime")
    print("- Optional static type hints (3.5+)")
    print("- Gradual typing (mix typed/untyped)")
    print("- Generic types with TypeVar")
    print("- Protocols for structural subtyping")
    print("- Literal types for specific values")
    print("- NewType for distinct types")
    print("- Type checking with mypy/pyright/pytype")
    print("- Runtime: types are not enforced (use tools)")

if __name__ == "__main__":
    main()
