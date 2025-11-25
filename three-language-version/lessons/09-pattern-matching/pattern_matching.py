"""
Lesson 9: Pattern Matching - Python Examples
Python uses structural pattern matching (match/case) since 3.10
"""

# =============================================================================
# BASIC PATTERN MATCHING (Python 3.10+)
# =============================================================================

def describe_type(value):
    """Match on different types"""
    match value:
        case int():
            return "It's an integer"
        case float():
            return "It's a float"
        case str():
            return "It's a string"
        case list():
            return "It's a list"
        case _:
            return "It's something else"

print("=== Type Matching ===")
print(describe_type(42))
print(describe_type(3.14))
print(describe_type("hello"))
print(describe_type([1, 2, 3]))


# =============================================================================
# MATCHING LITERAL VALUES
# =============================================================================

def http_status(code):
    """Match HTTP status codes"""
    match code:
        case 200:
            return "OK"
        case 201:
            return "Created"
        case 400:
            return "Bad Request"
        case 404:
            return "Not Found"
        case 500:
            return "Internal Server Error"
        case _:
            return f"Unknown status: {code}"

print("\n=== Literal Matching ===")
for code in [200, 404, 418]:
    print(f"HTTP {code}: {http_status(code)}")


# =============================================================================
# DESTRUCTURING SEQUENCES
# =============================================================================

def process_point(point):
    """Match and destructure point coordinates"""
    match point:
        case (0, 0):
            return "Origin"
        case (0, y):
            return f"On Y-axis at y={y}"
        case (x, 0):
            return f"On X-axis at x={x}"
        case (x, y):
            return f"Point at ({x}, {y})"
        case _:
            return "Not a valid point"

print("\n=== Sequence Destructuring ===")
points = [(0, 0), (0, 5), (3, 0), (3, 4)]
for p in points:
    print(f"{p} -> {process_point(p)}")


# =============================================================================
# MATCHING LISTS WITH PATTERNS
# =============================================================================

def analyze_list(lst):
    """Pattern match on list structure"""
    match lst:
        case []:
            return "Empty list"
        case [x]:
            return f"Single element: {x}"
        case [x, y]:
            return f"Two elements: {x} and {y}"
        case [x, y, *rest]:
            return f"Starts with {x}, {y}, then {len(rest)} more"
        case _:
            return "Not a list"

print("\n=== List Pattern Matching ===")
lists = [[], [1], [1, 2], [1, 2, 3, 4, 5]]
for lst in lists:
    print(f"{lst} -> {analyze_list(lst)}")


# =============================================================================
# MATCHING WITH GUARDS
# =============================================================================

def categorize_age(age):
    """Match with conditional guards"""
    match age:
        case n if n < 0:
            return "Invalid age"
        case n if n < 13:
            return "Child"
        case n if n < 20:
            return "Teenager"
        case n if n < 65:
            return "Adult"
        case _:
            return "Senior"

print("\n=== Guards ===")
ages = [-1, 5, 15, 30, 70]
for age in ages:
    print(f"Age {age}: {categorize_age(age)}")


# =============================================================================
# MATCHING DICTIONARIES
# =============================================================================

def process_event(event):
    """Match dictionary structure"""
    match event:
        case {"type": "click", "x": x, "y": y}:
            return f"Click at ({x}, {y})"
        case {"type": "keypress", "key": k}:
            return f"Key pressed: {k}"
        case {"type": "scroll", "direction": d}:
            return f"Scroll {d}"
        case {"type": t}:
            return f"Unknown event type: {t}"
        case _:
            return "Invalid event"

print("\n=== Dictionary Matching ===")
events = [
    {"type": "click", "x": 100, "y": 200},
    {"type": "keypress", "key": "Enter"},
    {"type": "scroll", "direction": "down"},
    {"type": "hover"},
]
for event in events:
    print(f"{event} -> {process_event(event)}")


# =============================================================================
# MATCHING OBJECTS (Classes)
# =============================================================================

class Point:
    def __init__(self, x, y):
        self.x = x
        self.y = y

class Circle:
    def __init__(self, center, radius):
        self.center = center
        self.radius = radius

class Rectangle:
    def __init__(self, corner, width, height):
        self.corner = corner
        self.width = width
        self.height = height

def describe_shape(shape):
    """Match on object types and attributes"""
    match shape:
        case Circle(center=Point(x=0, y=0), radius=r):
            return f"Circle centered at origin with radius {r}"
        case Circle(radius=r):
            return f"Circle with radius {r}"
        case Rectangle(width=w, height=h) if w == h:
            return f"Square with side {w}"
        case Rectangle(width=w, height=h):
            return f"Rectangle {w}x{h}"
        case _:
            return "Unknown shape"

print("\n=== Object Matching ===")
shapes = [
    Circle(Point(0, 0), 5),
    Circle(Point(1, 1), 3),
    Rectangle(Point(0, 0), 4, 4),
    Rectangle(Point(0, 0), 3, 5),
]
for shape in shapes:
    print(f"{type(shape).__name__} -> {describe_shape(shape)}")


# =============================================================================
# ALTERNATIVE: Traditional Approach (Before Python 3.10)
# =============================================================================

def process_point_traditional(point):
    """Traditional approach without match/case"""
    if not isinstance(point, tuple) or len(point) != 2:
        return "Not a valid point"

    x, y = point  # Manual destructuring

    if x == 0 and y == 0:
        return "Origin"
    elif x == 0:
        return f"On Y-axis at y={y}"
    elif y == 0:
        return f"On X-axis at x={x}"
    else:
        return f"Point at ({x}, {y})"

print("\n=== Traditional Approach (pre-3.10) ===")
print("Same functionality, but more verbose")
print(process_point_traditional((0, 0)))
print(process_point_traditional((3, 4)))


# =============================================================================
# RECURSIVE PATTERN MATCHING
# =============================================================================

def evaluate(expr):
    """Evaluate simple expressions using pattern matching"""
    match expr:
        case int(n):
            return n
        case float(n):
            return n
        case ('+', left, right):
            return evaluate(left) + evaluate(right)
        case ('-', left, right):
            return evaluate(left) - evaluate(right)
        case ('*', left, right):
            return evaluate(left) * evaluate(right)
        case ('/', left, right):
            return evaluate(left) / evaluate(right)
        case _:
            raise ValueError(f"Unknown expression: {expr}")

print("\n=== Expression Evaluation ===")
# (3 + 4) * 2
expr = ('*', ('+', 3, 4), 2)
print(f"{expr} = {evaluate(expr)}")

# 10 / (5 - 3)
expr = ('/', 10, ('-', 5, 3))
print(f"{expr} = {evaluate(expr)}")
