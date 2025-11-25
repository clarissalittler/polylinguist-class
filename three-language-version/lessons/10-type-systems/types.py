"""
Lesson 10: Type Systems - Python Examples
Python has dynamic typing with optional type hints
"""

from typing import List, Dict, Optional, Union, Callable, TypeVar, Generic
from dataclasses import dataclass
from abc import ABC, abstractmethod

# =============================================================================
# DYNAMIC TYPING IN ACTION
# =============================================================================

def dynamic_example():
    """Python variables can change types"""
    x = 42          # x is an int
    print(f"x = {x}, type = {type(x)}")

    x = "hello"     # Now x is a string!
    print(f"x = {x}, type = {type(x)}")

    x = [1, 2, 3]   # Now x is a list!
    print(f"x = {x}, type = {type(x)}")

print("=== Dynamic Typing ===")
dynamic_example()

# =============================================================================
# TYPE HINTS (PEP 484)
# =============================================================================

def greet(name: str) -> str:
    """Type hints document expected types"""
    return f"Hello, {name}!"

def add_numbers(a: int, b: int) -> int:
    return a + b

def process_items(items: List[str]) -> Dict[str, int]:
    """Return word counts"""
    return {item: len(item) for item in items}

print("\n=== Type Hints ===")
print(greet("Alice"))
print(add_numbers(3, 4))
print(process_items(["hello", "world"]))

# =============================================================================
# OPTIONAL AND UNION TYPES
# =============================================================================

def find_user(user_id: int) -> Optional[str]:
    """Return user name or None if not found"""
    users = {1: "Alice", 2: "Bob"}
    return users.get(user_id)

def parse_value(value: str) -> Union[int, float, str]:
    """Parse a string into int, float, or leave as string"""
    try:
        return int(value)
    except ValueError:
        try:
            return float(value)
        except ValueError:
            return value

print("\n=== Optional and Union ===")
print(f"find_user(1) = {find_user(1)}")
print(f"find_user(99) = {find_user(99)}")
print(f"parse_value('42') = {parse_value('42')} ({type(parse_value('42')).__name__})")
print(f"parse_value('3.14') = {parse_value('3.14')} ({type(parse_value('3.14')).__name__})")

# =============================================================================
# GENERIC TYPES
# =============================================================================

T = TypeVar('T')

def first(items: List[T]) -> Optional[T]:
    """Get first element of any list type"""
    return items[0] if items else None

def identity(x: T) -> T:
    """Return the input unchanged"""
    return x

print("\n=== Generic Types ===")
print(f"first([1, 2, 3]) = {first([1, 2, 3])}")
print(f"first(['a', 'b']) = {first(['a', 'b'])}")
print(f"first([]) = {first([])}")

# =============================================================================
# GENERIC CLASSES
# =============================================================================

@dataclass
class Box(Generic[T]):
    """A generic container"""
    value: T

    def map(self, f: Callable[[T], T]) -> 'Box[T]':
        return Box(f(self.value))

print("\n=== Generic Classes ===")
int_box: Box[int] = Box(42)
str_box: Box[str] = Box("hello")
print(f"int_box = {int_box}")
print(f"str_box = {str_box}")
print(f"int_box.map(lambda x: x * 2) = {int_box.map(lambda x: x * 2)}")

# =============================================================================
# TYPE ALIASES
# =============================================================================

UserId = int
UserName = str
UserDatabase = Dict[UserId, UserName]

def get_user_name(db: UserDatabase, uid: UserId) -> Optional[UserName]:
    return db.get(uid)

print("\n=== Type Aliases ===")
db: UserDatabase = {1: "Alice", 2: "Bob"}
print(f"get_user_name(db, 1) = {get_user_name(db, 1)}")

# =============================================================================
# CALLABLE TYPES
# =============================================================================

def apply_twice(f: Callable[[int], int], x: int) -> int:
    """Apply function f twice to x"""
    return f(f(x))

def double(n: int) -> int:
    return n * 2

print("\n=== Callable Types ===")
print(f"apply_twice(double, 3) = {apply_twice(double, 3)}")
print(f"apply_twice(lambda x: x + 1, 5) = {apply_twice(lambda x: x + 1, 5)}")

# =============================================================================
# ABSTRACT BASE CLASSES (Interface-like)
# =============================================================================

class Shape(ABC):
    @abstractmethod
    def area(self) -> float:
        pass

    @abstractmethod
    def perimeter(self) -> float:
        pass

class Circle(Shape):
    def __init__(self, radius: float):
        self.radius = radius

    def area(self) -> float:
        return 3.14159 * self.radius ** 2

    def perimeter(self) -> float:
        return 2 * 3.14159 * self.radius

class Rectangle(Shape):
    def __init__(self, width: float, height: float):
        self.width = width
        self.height = height

    def area(self) -> float:
        return self.width * self.height

    def perimeter(self) -> float:
        return 2 * (self.width + self.height)

def describe_shape(shape: Shape) -> str:
    return f"Area: {shape.area():.2f}, Perimeter: {shape.perimeter():.2f}"

print("\n=== Abstract Base Classes ===")
shapes: List[Shape] = [Circle(5), Rectangle(3, 4)]
for s in shapes:
    print(f"{type(s).__name__}: {describe_shape(s)}")

# =============================================================================
# DATACLASSES WITH TYPES
# =============================================================================

@dataclass
class Point:
    x: float
    y: float

    def distance_from_origin(self) -> float:
        return (self.x ** 2 + self.y ** 2) ** 0.5

@dataclass
class Person:
    name: str
    age: int
    email: Optional[str] = None

print("\n=== Dataclasses ===")
p = Point(3, 4)
print(f"Point(3, 4).distance_from_origin() = {p.distance_from_origin()}")
person = Person("Alice", 30)
print(f"Person: {person}")

# =============================================================================
# DUCK TYPING VS STRUCTURAL TYPING
# =============================================================================

from typing import Protocol

class Drawable(Protocol):
    """Structural subtyping - any class with draw() method works"""
    def draw(self) -> str: ...

class Circle2:
    def draw(self) -> str:
        return "Drawing a circle"

class Square:
    def draw(self) -> str:
        return "Drawing a square"

def render(shape: Drawable) -> None:
    print(shape.draw())

print("\n=== Protocols (Structural Typing) ===")
render(Circle2())
render(Square())

# =============================================================================
# RUNTIME TYPE CHECKING
# =============================================================================

def strict_add(a, b):
    """Manually check types at runtime"""
    if not isinstance(a, (int, float)):
        raise TypeError(f"Expected number, got {type(a)}")
    if not isinstance(b, (int, float)):
        raise TypeError(f"Expected number, got {type(b)}")
    return a + b

print("\n=== Runtime Type Checking ===")
print(f"strict_add(1, 2) = {strict_add(1, 2)}")
try:
    strict_add("hello", 2)
except TypeError as e:
    print(f"Error: {e}")
