# Lesson 10: Type Systems - Solution Guide

This guide provides example solutions for the Type Systems exercises.

## General Notes

- **Type systems**: Rules for assigning types to program constructs
- **Static vs Dynamic**: Compile-time vs runtime type checking
- **Strong vs Weak**: Strictness of type enforcement
- **Type inference**: Compiler deduces types automatically
- **Generics**: Code that works with multiple types

---

## Exercise 1: Type Conversions

**Task:** Implement safe type conversions

### Python Solution

```python
from typing import Optional

def string_to_int(s: str) -> Optional[int]:
    """Safe string to int conversion"""
    try:
        return int(s)
    except ValueError:
        return None

def int_to_string(n: int) -> str:
    """Always succeeds"""
    return str(n)

def float_to_int(f: float) -> int:
    """Round float to int"""
    return round(f)

# Test
print(string_to_int("123"))     # 123
print(string_to_int("hello"))   # None
print(int_to_string(42))        # "42"
print(float_to_int(3.14))       # 3
print(float_to_int(3.7))        # 4
```

### Rust Solution

```rust
fn string_to_int(s: &str) -> Option<i32> {
    s.parse::<i32>().ok()
}

fn int_to_string(n: i32) -> String {
    n.to_string()
}

fn float_to_int(f: f64) -> i32 {
    f.round() as i32
}

fn main() {
    println!("{:?}", string_to_int("123"));     // Some(123)
    println!("{:?}", string_to_int("hello"));   // None
    println!("{}", int_to_string(42));          // "42"
    println!("{}", float_to_int(3.14));         // 3
}
```

### Haskell Solution

```haskell
import Text.Read (readMaybe)

stringToInt :: String -> Maybe Int
stringToInt = readMaybe

intToString :: Int -> String
intToString = show

floatToInt :: Double -> Int
floatToInt = round

main :: IO ()
main = do
    print $ stringToInt "123"       -- Just 123
    print $ stringToInt "hello"     -- Nothing
    print $ intToString 42          -- "42"
    print $ floatToInt 3.14         -- 3
```

**Key Insights:**
- Use Option/Maybe for conversions that can fail
- Type-safe conversions prevent runtime errors
- Explicit error handling is better than exceptions

---

## Exercise 3: Strong vs Weak Typing

**Task:** Demonstrate type coercion differences

### JavaScript (Weak Typing)

```javascript
// Implicit coercions
console.log("5" + 3);           // "53" (string concatenation)
console.log("5" - 3);           // 2 (numeric subtraction)
console.log("5" * "3");         // 15 (both converted to numbers)
console.log(true + 1);          // 2 (true → 1)
console.log("hello" - 1);       // NaN (can't convert "hello" to number)
console.log([] + []);           // "" (empty string)
console.log([] + {});           // "[object Object]"
console.log({} + []);           // 0 (!! weird coercion)

// Loose equality with coercion
console.log(5 == "5");          // true
console.log(null == undefined); // true
console.log(0 == false);        // true

// Strict equality (no coercion)
console.log(5 === "5");         // false
console.log(0 === false);       // false
```

### Python (Strong Typing)

```python
# Type errors (no implicit coercion)
try:
    print("5" + 3)  # TypeError: can only concatenate str to str
except TypeError as e:
    print(f"Error: {e}")

try:
    print("5" - 3)  # TypeError: unsupported operand type(s)
except TypeError as e:
    print(f"Error: {e}")

# Must explicitly convert
print("5" + str(3))     # "53"
print(int("5") + 3)     # 8

# Boolean operations
print(True + 1)         # 2 (special case: bool is subclass of int)
print(1 == True)        # True (another special case)

# But strings and numbers strictly separated
print("5" == 5)         # False
```

### Rust (Very Strong Typing)

```rust
fn main() {
    // These won't even compile:
    // let x = "5" + 3;          // Error: cannot add integer to &str
    // let y = "5" - 3;          // Error: cannot subtract
    // let z: i32 = true;        // Error: expected i32, found bool

    // Must explicitly convert
    let s = "5".to_string() + &3.to_string();  // "53"
    let n = "5".parse::<i32>().unwrap() + 3;   // 8

    // No coercion at all
    // if 1 { }  // Error: expected bool, found integer
    if true { println!("Correct!"); }
}
```

**Key Insights:**
- Weak typing: implicit conversions, convenient but error-prone
- Strong typing: explicit conversions, verbose but safe
- JavaScript's coercion rules are notoriously confusing
- Rust prevents type errors at compile time

---

## Exercise 8: Tagged Unions (Sum Types)

**Task:** Create Shape ADT with area/perimeter

### Haskell Solution

```haskell
data Shape
    = Circle Double
    | Rectangle Double Double
    | Triangle Double Double Double
    deriving (Show)

area :: Shape -> Double
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h
area (Triangle a b c) =
    let s = (a + b + c) / 2  -- semi-perimeter
    in sqrt (s * (s - a) * (s - b) * (s - c))  -- Heron's formula

perimeter :: Shape -> Double
perimeter (Circle r) = 2 * pi * r
perimeter (Rectangle w h) = 2 * (w + h)
perimeter (Triangle a b c) = a + b + c

isSquare :: Shape -> Bool
isSquare (Rectangle w h) = w == h
isSquare _ = False

main :: IO ()
main = do
    let circle = Circle 5
    let rect = Rectangle 4 6
    let square = Rectangle 5 5
    let triangle = Triangle 3 4 5

    putStrLn $ "Circle area: " ++ show (area circle)
    putStrLn $ "Rectangle area: " ++ show (area rect)
    putStrLn $ "Is square? " ++ show (isSquare square)
    putStrLn $ "Triangle perimeter: " ++ show (perimeter triangle)
```

### Rust Solution

```rust
enum Shape {
    Circle { radius: f64 },
    Rectangle { width: f64, height: f64 },
    Triangle { a: f64, b: f64, c: f64 },
}

impl Shape {
    fn area(&self) -> f64 {
        match self {
            Shape::Circle { radius } => std::f64::consts::PI * radius * radius,
            Shape::Rectangle { width, height } => width * height,
            Shape::Triangle { a, b, c } => {
                let s = (a + b + c) / 2.0;
                (s * (s - a) * (s - b) * (s - c)).sqrt()
            }
        }
    }

    fn perimeter(&self) -> f64 {
        match self {
            Shape::Circle { radius } => 2.0 * std::f64::consts::PI * radius,
            Shape::Rectangle { width, height } => 2.0 * (width + height),
            Shape::Triangle { a, b, c } => a + b + c,
        }
    }

    fn is_square(&self) -> bool {
        match self {
            Shape::Rectangle { width, height } => width == height,
            _ => false,
        }
    }
}

fn main() {
    let circle = Shape::Circle { radius: 5.0 };
    let square = Shape::Rectangle { width: 5.0, height: 5.0 };

    println!("Circle area: {}", circle.area());
    println!("Is square? {}", square.is_square());
}
```

### Python 3.10+ Solution

```python
from dataclasses import dataclass
from typing import Union
import math

@dataclass
class Circle:
    radius: float

@dataclass
class Rectangle:
    width: float
    height: float

@dataclass
class Triangle:
    a: float
    b: float
    c: float

Shape = Circle | Rectangle | Triangle

def area(shape: Shape) -> float:
    match shape:
        case Circle(r):
            return math.pi * r * r
        case Rectangle(w, h):
            return w * h
        case Triangle(a, b, c):
            s = (a + b + c) / 2
            return math.sqrt(s * (s - a) * (s - b) * (s - c))

def perimeter(shape: Shape) -> float:
    match shape:
        case Circle(r):
            return 2 * math.pi * r
        case Rectangle(w, h):
            return 2 * (w + h)
        case Triangle(a, b, c):
            return a + b + c

def is_square(shape: Shape) -> bool:
    match shape:
        case Rectangle(w, h):
            return w == h
        case _:
            return False

# Test
circle = Circle(5)
square = Rectangle(5, 5)
triangle = Triangle(3, 4, 5)

print(f"Circle area: {area(circle):.2f}")
print(f"Square perimeter: {perimeter(square)}")
print(f"Is square? {is_square(square)}")
```

**Key Insights:**
- Sum types represent "one of" multiple possibilities
- Pattern matching makes working with them natural
- Type-safe: can't forget to handle a case
- Better than inheritance for this problem

---

## Exercise 11: Generic Box

**Task:** Implement generic Box container

### Haskell Solution

```haskell
-- Box is just a wrapper
data Box a = Box a deriving (Show)

-- Construct
new :: a -> Box a
new = Box

-- Extract
get :: Box a -> a
get (Box x) = x

-- Map (Functor)
boxMap :: (a -> b) -> Box a -> Box b
boxMap f (Box x) = Box (f x)

-- Flat map (Monad)
boxFlatMap :: (a -> Box b) -> Box a -> Box b
boxFlatMap f (Box x) = f x

main :: IO ()
main = do
    let box1 = new 5                    -- Box 5
    print $ get box1                    -- 5

    let box2 = boxMap (*2) box1         -- Box 10
    print box2

    let box3 = boxFlatMap (\x -> new (x + 1)) box2  -- Box 11
    print box3
```

### Rust Solution

```rust
struct Box<T> {
    value: T,
}

impl<T> Box<T> {
    fn new(value: T) -> Self {
        Box { value }
    }

    fn get(self) -> T {
        self.value
    }

    fn map<U, F>(self, f: F) -> Box<U>
    where
        F: FnOnce(T) -> U,
    {
        Box::new(f(self.value))
    }

    fn flat_map<U, F>(self, f: F) -> Box<U>
    where
        F: FnOnce(T) -> Box<U>,
    {
        f(self.value)
    }
}

fn main() {
    let box1 = Box::new(5);
    println!("{}", box1.get());  // 5

    let box2 = Box::new(5).map(|x| x * 2);
    println!("{}", box2.get());  // 10

    let box3 = Box::new(5).flat_map(|x| Box::new(x + 1));
    println!("{}", box3.get());  // 6
}
```

### Python Solution

```python
from typing import TypeVar, Generic, Callable

T = TypeVar('T')
U = TypeVar('U')

class Box(Generic[T]):
    def __init__(self, value: T):
        self.value = value

    def get(self) -> T:
        return self.value

    def map(self, f: Callable[[T], U]) -> 'Box[U]':
        return Box(f(self.value))

    def flat_map(self, f: Callable[[T], 'Box[U]']) -> 'Box[U]':
        return f(self.value)

# Test
box1 = Box(5)
print(box1.get())  # 5

box2 = box1.map(lambda x: x * 2)
print(box2.get())  # 10

box3 = box1.flat_map(lambda x: Box(x + 1))
print(box3.get())  # 6
```

**Key Insights:**
- Generics allow code reuse across types
- `map` transforms wrapped value
- `flat_map` prevents nested wrapping
- Foundation of functional programming patterns

---

## Exercise 13: Bounded Generics

**Task:** Implement functions with type constraints

### Rust Solution

```rust
use std::fmt::Display;
use std::cmp::Ord;

// T must implement Ord (ordering)
fn max_value<T: Ord>(a: T, b: T) -> T {
    if a > b { a } else { b }
}

// T must implement Display
fn describe<T: Display>(value: T) -> String {
    format!("The value is: {}", value)
}

// T must implement Ord
fn sort<T: Ord + Clone>(mut list: Vec<T>) -> Vec<T> {
    list.sort();
    list
}

fn main() {
    println!("{}", max_value(5, 10));           // 10
    println!("{}", max_value("abc", "xyz"));    // "xyz"

    println!("{}", describe(42));               // "The value is: 42"
    println!("{}", describe("hello"));          // "The value is: hello"

    let numbers = vec![5, 2, 8, 1, 9];
    println!("{:?}", sort(numbers));            // [1, 2, 5, 8, 9]
}
```

### Haskell Solution

```haskell
-- Type constraints with =>
maxValue :: Ord a => a -> a -> a
maxValue a b = if a > b then a else b

describe :: Show a => a -> String
describe x = "The value is: " ++ show x

sortList :: Ord a => [a] -> [a]
sortList = Data.List.sort

main :: IO ()
main = do
    print $ maxValue 5 10           -- 10
    print $ maxValue "abc" "xyz"    -- "xyz"

    putStrLn $ describe 42          -- "The value is: 42"
    putStrLn $ describe "hello"     -- "The value is: hello"

    print $ sortList [5, 2, 8, 1, 9]  -- [1,2,5,8,9]
```

### Java Solution

```java
import java.util.*;

class BoundedGenerics {
    // T must extend Comparable
    public static <T extends Comparable<T>> T maxValue(T a, T b) {
        return a.compareTo(b) > 0 ? a : b;
    }

    // T can be any type (toString always available)
    public static <T> String describe(T value) {
        return "The value is: " + value.toString();
    }

    // T must extend Comparable
    public static <T extends Comparable<T>> List<T> sort(List<T> list) {
        List<T> copy = new ArrayList<>(list);
        Collections.sort(copy);
        return copy;
    }

    public static void main(String[] args) {
        System.out.println(maxValue(5, 10));            // 10
        System.out.println(maxValue("abc", "xyz"));     // xyz

        System.out.println(describe(42));               // The value is: 42

        List<Integer> numbers = Arrays.asList(5, 2, 8, 1, 9);
        System.out.println(sort(numbers));              // [1, 2, 5, 8, 9]
    }
}
```

**Key Insights:**
- Bounds restrict which types can be used
- `T: Ord` means T must be orderable
- `T: Display` means T must be printable
- Enables generic algorithms with guarantees

---

## Summary

Type systems are fundamental to programming languages:

**Key Concepts:**
- **Static vs Dynamic**: When types are checked
- **Strong vs Weak**: How strictly types are enforced
- **Type inference**: Compiler deduces types
- **Generics**: Code that works with multiple types
- **Constraints**: Restricting what types can do

**Type System Features:**
- **Primitive types**: int, float, bool, char
- **Composite types**: tuples, records, arrays
- **Sum types**: OR relationships (Either, Shape)
- **Product types**: AND relationships (tuples, structs)
- **Generic types**: Parameterized by other types
- **Option/Maybe**: Safe handling of absence
- **Result/Either**: Safe error handling

**Benefits:**
- **Safety**: Catch errors at compile time
- **Documentation**: Types explain code
- **Refactoring**: Changes checked automatically
- **Performance**: Compiler optimizations
- **Tooling**: Better IDE support

**Tradeoffs:**
- **Flexibility**: Dynamic is more flexible
- **Verbosity**: Static types require annotations
- **Learning curve**: Advanced features are complex
- **Iteration speed**: Compilation takes time

**Language Comparison:**
- **Haskell**: Strong, static, inferred, very expressive
- **Rust**: Strong, static, explicit, ownership types
- **TypeScript**: Gradual typing for JavaScript
- **Python**: Strong, dynamic, optional type hints
- **JavaScript**: Weak, dynamic, no static checking

Understanding type systems helps you choose the right language and write safer code!
