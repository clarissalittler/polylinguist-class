# Lesson 2: Variables and Types - Solution Guide

This guide provides example solutions for the Variables and Types exercises.

## General Notes

- Type systems are fundamental to understanding programming languages
- Static vs dynamic typing represents a key paradigm difference
- Mutability affects how we reason about program state
- Different languages make different trade-offs between safety and flexibility

---

## Exercise 1: Variable Declaration

**Task:** Declare variables of different types

### Python Solution (Dynamic Typing)

```python
# No type annotations required
name = "Alice"
age = 25
height = 5.6
is_student = True

# Print types
print(type(name))       # <class 'str'>
print(type(age))        # <class 'int'>
print(type(height))     # <class 'float'>
print(type(is_student)) # <class 'bool'>
```

### JavaScript Solution (Dynamic Typing)

```javascript
// No type annotations required
let name = "Alice";
let age = 25;
let height = 5.6;
let isStudent = true;

// Print types
console.log(typeof name);      // string
console.log(typeof age);       // number
console.log(typeof height);    // number
console.log(typeof isStudent); // boolean
```

### Java Solution (Static Typing, Explicit)

```java
public class Variables {
    public static void main(String[] args) {
        // Must declare types explicitly
        String name = "Alice";
        int age = 25;
        double height = 5.6;
        boolean isStudent = true;

        System.out.println(name);
        System.out.println(age);
        System.out.println(height);
        System.out.println(isStudent);
    }
}
```

### Haskell Solution (Static Typing, Inferred)

```haskell
-- Types are inferred but can be annotated
name :: String
name = "Alice"

age :: Int
age = 25

height :: Double
height = 5.6

isStudent :: Bool
isStudent = True

main :: IO ()
main = do
    print name
    print age
    print height
    print isStudent
```

### Rust Solution (Static Typing, Inferred)

```rust
fn main() {
    // Types are inferred
    let name = "Alice";
    let age = 25;
    let height = 5.6;
    let is_student = true;

    // Or explicit:
    let name: &str = "Alice";
    let age: i32 = 25;
    let height: f64 = 5.6;
    let is_student: bool = true;

    println!("{}", name);
    println!("{}", age);
    println!("{}", height);
    println!("{}", is_student);
}
```

**Key Differences:**
- **Python/JavaScript**: No type declarations, fully dynamic
- **Java**: Explicit type annotations required
- **Haskell/Rust**: Types inferred but can be annotated for clarity
- **JavaScript**: `number` for both integers and floats
- **Rust/C**: Distinct integer types (`i32`, `i64`, etc.)

---

## Exercise 2: Type Errors

**Task:** Cause type errors in different languages

### Python (Runtime Type Error)

```python
x = "5"
y = 10
# print(x + y)  # TypeError: can only concatenate str (not "int") to str

# Fix 1: Convert string to int
print(int(x) + y)  # 15

# Fix 2: Convert int to string
print(x + str(y))  # "510"
```

**Behavior:** Python raises `TypeError` at runtime

### JavaScript (Type Coercion)

```javascript
let x = "5";
let y = 10;
console.log(x + y);  // "510" - string concatenation!

// JavaScript coerces y to string
// This can be surprising!

// To add as numbers:
console.log(Number(x) + y);  // 15
console.log(+x + y);         // 15 (unary + converts to number)
```

**Behavior:** JavaScript performs implicit type coercion (often unexpected!)

### C (Compile-Time Type Error)

```c
#include <stdio.h>

int main() {
    int x = 5;
    char* y = "10";

    // printf("%d\n", x + y);  // Compile error!
    // warning: incompatible pointer to integer conversion

    // Must explicitly convert
    int y_int = atoi(y);
    printf("%d\n", x + y_int);  // 15

    return 0;
}
```

**Behavior:** C catches type error at compile time

**Answers to Questions:**
1. **Which allow?** JavaScript allows (coerces); Python/C don't
2. **Type coercion:** Automatic conversion between types (can be helpful or dangerous)
3. **JavaScript behavior:** Convenient but can hide bugs (`"5" + 1` → `"51"`, `"5" - 1` → `4`!)

---

## Exercise 3: Type Inference

**Task:** Explore type inference in statically-typed languages

### Haskell Type Inference

```haskell
-- In GHCi:
-- > :type 42
-- 42 :: Num a => a

-- > :type "hello"
-- "hello" :: [Char]

-- > :type [1, 2, 3]
-- [1, 2, 3] :: Num a => [a]

-- Inferred types:
x = 42           -- Num a => a (polymorphic numeric)
y = "hello"      -- String (which is [Char])
z = [1, 2, 3]    -- Num a => [a]

-- Type errors caught at compile time:
-- badList = [1, "two", 3]  -- Error: Couldn't match expected type 'Integer' with actual type '[Char]'
```

### Rust Type Inference

```rust
fn main() {
    let x = 42;          // i32 inferred
    let y = 3.14;        // f64 inferred
    let z = vec![1, 2, 3]; // Vec<i32> inferred

    // See inferred type by intentionally causing error:
    // let _: () = x;
    // Error: expected (), found i32

    // Type errors caught at compile time:
    // let mixed = vec![1, "two", 3];  // Error: mismatched types
}
```

**Learning Points:**
- **Haskell:** Very powerful type inference, often don't need annotations
- **Rust:** Good type inference for local variables
- **Both:** Types checked at compile time, catching errors early
- **Polymorphism:** Haskell's `Num a => a` means "any numeric type"

---

## Exercise 4: Mutability Challenge

**Task:** Compare mutability across languages

### Python (Mutable by Default)

```python
x = 10
print(x)  # 10

x = 20    # Reassignment allowed
print(x)  # 20

# But note: strings, numbers, tuples are immutable values
# Only the binding changes
```

### Haskell (Immutable)

```haskell
main :: IO ()
main = do
    let x = 10
    print x  -- 10

    -- let x = 20  -- This creates NEW binding, doesn't change original
    -- Can't "reassign" in same scope!

    let y = 10
    let z = y + 10  -- Create new value instead of modifying
    print z  -- 20
```

**Why immutability?**
- Easier to reason about code
- Enables parallel computation safely
- Prevents accidental changes
- Functional programming core principle

### Rust (Explicit Mutability)

```rust
fn main() {
    let x = 10;
    println!("{}", x);  // 10

    // x = 20;  // Error: cannot assign twice to immutable variable

    // Must explicitly declare mutable:
    let mut y = 10;
    println!("{}", y);  // 10

    y = 20;  // OK now!
    println!("{}", y);  // 20
}
```

**Benefits of Rust's approach:**
- **Default immutability**: Encourages safer code
- **Explicit `mut`**: Makes mutability visible
- **Compiler enforced**: Catches errors at compile time
- **Best of both worlds**: Immutability where possible, mutation when needed

**Answers:**
- **Haskell immutability:** Ensures referential transparency, enables optimizations
- **Rust's `mut`:** Makes mutability explicit and intentional
- **When useful:** Concurrent programming, caching, pure functions

---

## Exercise 5: Type Conversion

**Task:** Convert between types

### Python Solutions

```python
# String to int
x = int("42")
print(x)  # 42

# Int to string
y = str(123)
print(y)  # "123"

# Float to int (truncates)
z = int(3.99)
print(z)  # 3

# Invalid conversion raises exception
try:
    bad = int("hello")
except ValueError as e:
    print(f"Error: {e}")  # invalid literal for int()
```

### JavaScript Solutions

```javascript
// String to number
let x = Number("42");
let y = parseInt("42");
let z = parseFloat("3.14");

console.log(x);  // 42
console.log(y);  // 42
console.log(z);  // 3.14

// Number to string
let a = String(123);
let b = (123).toString();

console.log(a);  // "123"
console.log(b);  // "123"

// Invalid conversion returns NaN (Not a Number)
let bad = Number("hello");
console.log(bad);  // NaN
console.log(isNaN(bad));  // true
```

### Haskell Solutions

```haskell
-- String to Int (using read)
x :: Int
x = read "42"  -- 42

-- Int to String (using show)
y :: String
y = show 123  -- "123"

-- Invalid conversion raises exception at runtime
-- bad = read "hello" :: Int  -- Exception: no parse

-- Safe conversion with Maybe:
import Text.Read (readMaybe)

safeRead :: String -> Maybe Int
safeRead s = readMaybe s

main :: IO ()
main = do
    print (safeRead "42")     -- Just 42
    print (safeRead "hello")  -- Nothing
```

**Key Differences:**
- **Python:** Exceptions for invalid conversions
- **JavaScript:** Returns `NaN` for invalid numbers
- **Haskell:** Can use `Maybe` for safe parsing

---

## Exercise 6: Constants vs Variables

**Task:** Understand constants

### Python (Convention Only)

```python
# Constant by convention (all caps)
PI = 3.14159
MAX_SIZE = 100

# But nothing prevents this (bad practice!):
PI = 3  # No error, just bad style

# Python has no true constants at language level
# Rely on convention and discipline
```

### JavaScript (const prevents reassignment)

```javascript
// const prevents reassignment
const PI = 3.14159;
// PI = 3;  // TypeError: Assignment to constant variable

// But const doesn't prevent mutation:
const arr = [1, 2, 3];
arr.push(4);  // Allowed!
console.log(arr);  // [1, 2, 3, 4]

// Can't reassign the array:
// arr = [5, 6, 7];  // Error!

// To prevent mutation, use Object.freeze:
const frozen = Object.freeze([1, 2, 3]);
// frozen.push(4);  // Error in strict mode, silently fails otherwise
```

### Rust (const vs let)

```rust
fn main() {
    // Immutable by default
    let x = 10;
    // x = 20;  // Error

    // True compile-time constant
    const PI: f64 = 3.14159;
    // Must have type annotation
    // Must be compile-time evaluable
    // UPPERCASE naming convention

    // Mutable variable
    let mut y = 10;
    y = 20;  // OK
}
```

**Answers:**
- **JavaScript `const`:** Prevents reassignment, not mutation
- **Rust:** Immutable `let` vs compile-time `const`
- **Why useful:** Prevent accidental changes, document intent, enable optimizations

---

## Exercise 7: Dynamic vs Static Typing

**Task:** Compare type systems

### Python (Dynamic) - Duck Typing

```python
def process(x):
    """Works with any type that supports multiplication"""
    return x * 2

print(process(5))        # 10
print(process("hi"))     # "hihi"
print(process([1, 2]))   # [1, 2, 1, 2]

# Runtime error only when called with wrong type:
# print(process(None))  # TypeError: unsupported operand type(s)
```

### Java (Static) - Type Checking

```java
public class TypeDemo {
    public static int process(int x) {
        return x * 2;
    }

    public static void main(String[] args) {
        System.out.println(process(5));  // 10
        // System.out.println(process("hi"));  // Compile error!
    }
}
```

### Haskell (Static with Polymorphism)

```haskell
-- Type-safe polymorphism
process :: Num a => a -> a
process x = x * 2

main :: IO ()
main = do
    print (process 5)       -- 10
    print (process 5.5)     -- 11.0
    -- print (process "hi") -- Compile error: No instance for (Num [Char])
```

**Comparison:**

| Aspect | Dynamic (Python) | Static (Java) | Static + Inference (Haskell) |
|--------|------------------|---------------|------------------------------|
| Type errors | Runtime | Compile time | Compile time |
| Flexibility | Very flexible | Less flexible | Flexible with constraints |
| IDE support | Limited | Excellent | Excellent |
| Refactoring | Risky | Safe | Very safe |
| Boilerplate | Minimal | More verbose | Minimal (inferred) |

**Answers:**
- **Static advantages:** Catch errors early, better tooling, documentation
- **Dynamic advantages:** Faster prototyping, more flexible
- **Catches errors earlier:** Static typing
- **More flexible:** Dynamic typing

---

## Exercise 8: Scope Exploration

**Task:** Understand variable scope

### Python Solution

```python
x = "global"

def outer():
    x = "outer"  # Shadows global x

    def inner():
        x = "inner"  # Shadows outer x
        print(x)  # Prints: "inner"

    inner()
    print(x)  # Prints: "outer"

outer()
print(x)  # Prints: "global"

# Output:
# inner
# outer
# global
```

**Without inner `x`:**
```python
x = "global"

def outer():
    x = "outer"

    def inner():
        # No x declared here, uses outer's x
        print(x)  # Prints: "outer"

    inner()
    print(x)  # Prints: "outer"

outer()
print(x)  # Prints: "global"
```

**Modifying outer variable:**
```python
x = "global"

def outer():
    x = "outer"

    def inner():
        nonlocal x  # Modify outer's x
        x = "modified"
        print(x)  # Prints: "modified"

    inner()
    print(x)  # Prints: "modified"

outer()
print(x)  # Prints: "global" (unchanged)
```

### JavaScript Solution

```javascript
let x = "global";

function outer() {
    let x = "outer";  // Shadows global x

    function inner() {
        let x = "inner";  // Shadows outer x
        console.log(x);  // Prints: "inner"
    }

    inner();
    console.log(x);  // Prints: "outer"
}

outer();
console.log(x);  // Prints: "global"

// Output:
// inner
// outer
// global
```

**Key Concepts:**
- **Shadowing:** Inner scope variable hides outer scope variable with same name
- **Lexical scoping:** Variables visible in scope where defined and nested scopes
- **`nonlocal` (Python)** / **no `let` (JavaScript):** Access outer scope variable

---

## Exercise 9: Type Safety

**Task:** Explore how type systems prevent bugs

### Python (Runtime Checking Only)

```python
def divide(a, b):
    return a / b

# All accepted at parse time:
print(divide(10, 2))  # 5.0 - OK

try:
    print(divide("10", "2"))  # TypeError at runtime
except TypeError as e:
    print(f"Error: {e}")

try:
    print(divide(10, 0))  # ZeroDivisionError at runtime
except ZeroDivisionError as e:
    print(f"Error: {e}")
```

### Haskell (Compile-Time Type Checking)

```haskell
divide :: Float -> Float -> Float
divide a b = a / b

main :: IO ()
main = do
    print (divide 10 2)  -- 5.0 - OK
    -- print (divide "10" "2")  -- Compile error: type mismatch
    -- print (divide 10 0)  -- Runtime error: divide by zero (not caught by types)
```

### Rust (Type Safety + Error Handling)

```rust
fn divide(a: f64, b: f64) -> Option<f64> {
    if b == 0.0 {
        None  // Return None instead of error
    } else {
        Some(a / b)  // Return Some with result
    }
}

fn main() {
    match divide(10.0, 2.0) {
        Some(result) => println!("Result: {}", result),  // 5.0
        None => println!("Cannot divide by zero"),
    }

    // divide("10", "2");  // Compile error: type mismatch

    match divide(10.0, 0.0) {
        Some(result) => println!("Result: {}", result),
        None => println!("Cannot divide by zero"),  // This branch taken
    }
}
```

**Answers:**
1. **Catches earliest:** Rust (types at compile time + error handling); Haskell (types at compile time); Python (runtime only)
2. **Rust's division by zero:** Returns `Option<f64>` - `None` for zero, `Some(result)` otherwise
3. **Trade-offs:**
   - **Python:** Fast to write, errors found late
   - **Haskell:** Type safe, but can't express "non-zero number" in type system easily
   - **Rust:** Explicit error handling, verbose but safe

---

## Exercise 10: Build a Calculator

**Task:** Create a calculator with type awareness

### Python Solution (Dynamic)

```python
def calculator(a, b, op):
    """Simple calculator

    Args:
        a: First number
        b: Second number
        op: Operation ('+', '-', '*', '/')

    Returns:
        Result or None if error
    """
    if not isinstance(a, (int, float)) or not isinstance(b, (int, float)):
        print("Error: Arguments must be numbers")
        return None

    if op == '+':
        return a + b
    elif op == '-':
        return a - b
    elif op == '*':
        return a * b
    elif op == '/':
        if b == 0:
            print("Error: Division by zero")
            return None
        return a / b
    else:
        print(f"Error: Unknown operation '{op}'")
        return None

# Tests
print(calculator(10, 5, '+'))   # 15
print(calculator(10, 5, '-'))   # 5
print(calculator(10, 5, '*'))   # 50
print(calculator(10, 5, '/'))   # 2.0
print(calculator(10, 0, '/'))   # Error: Division by zero, None
print(calculator("10", 5, '+')) # Error: Arguments must be numbers, None
```

### Rust Solution (Static + Safe)

```rust
enum Operation {
    Add,
    Subtract,
    Multiply,
    Divide,
}

fn calculator(a: f64, b: f64, op: Operation) -> Result<f64, String> {
    match op {
        Operation::Add => Ok(a + b),
        Operation::Subtract => Ok(a - b),
        Operation::Multiply => Ok(a * b),
        Operation::Divide => {
            if b == 0.0 {
                Err("Division by zero".to_string())
            } else {
                Ok(a / b)
            }
        }
    }
}

fn main() {
    println!("{:?}", calculator(10.0, 5.0, Operation::Add));      // Ok(15.0)
    println!("{:?}", calculator(10.0, 5.0, Operation::Subtract)); // Ok(5.0)
    println!("{:?}", calculator(10.0, 5.0, Operation::Multiply)); // Ok(50.0)
    println!("{:?}", calculator(10.0, 5.0, Operation::Divide));   // Ok(2.0)
    println!("{:?}", calculator(10.0, 0.0, Operation::Divide));   // Err("Division by zero")

    // calculator("10", 5.0, Operation::Add);  // Compile error!
}
```

**Key Differences:**
- **Python:** Must check types at runtime
- **Rust:** Types guaranteed at compile time, uses `Result` for error handling
- **Python:** Uses `None` for errors (or could raise exceptions)
- **Rust:** Uses `Result<T, E>` - type-safe error handling

---

## Exercise 11: Type Annotations (Python)

**Task:** Use Python type hints

### Python with Type Hints

```python
def greet(name: str) -> str:
    """Greet a person by name"""
    return f"Hello, {name}!"

def add(x: int, y: int) -> int:
    """Add two integers"""
    return x + y

def divide(a: float, b: float) -> float | None:
    """Divide a by b, return None if b is zero"""
    if b == 0:
        return None
    return a / b

# These work at runtime (hints not enforced):
print(greet("Alice"))  # Hello, Alice!
print(greet(123))      # Hello, 123! (converts to string in f-string)

print(add(5, 3))       # 8
print(add("5", "3"))   # 53 (string concatenation)

# But mypy will warn about type mismatches
```

**Running mypy:**
```bash
$ mypy script.py
script.py:15: error: Argument 1 to "greet" has incompatible type "int"; expected "str"
script.py:18: error: Argument 1 to "add" has incompatible type "str"; expected "int"
```

**Answers:**
- **Change runtime behavior?** No, purely for static analysis
- **Enforced?** Not by Python runtime, but by tools like `mypy`
- **Benefit:** Documentation, IDE support, catch bugs before running

---

## Exercise 12: Null/None/Nothing

**Task:** Handle "no value" in different languages

### Python (None)

```python
def find_user(user_id):
    """Simulate database lookup"""
    users = {1: "Alice", 2: "Bob"}
    return users.get(user_id)  # Returns None if not found

user = find_user(1)
if user is not None:
    print(f"Found: {user}")
else:
    print("User not found")

user = find_user(999)
if user is not None:
    print(f"Found: {user}")
else:
    print("User not found")  # This branch
```

### JavaScript (null vs undefined)

```javascript
// undefined: variable declared but not assigned
let x;
console.log(x);  // undefined

// null: explicitly no value
let y = null;
console.log(y);  // null

// Function returns undefined if no return statement
function noReturn() {
    let x = 5;
}
console.log(noReturn());  // undefined

// Check for null or undefined
function findUser(userId) {
    const users = {1: "Alice", 2: "Bob"};
    return users[userId];  // undefined if not found
}

const user = findUser(1);
if (user !== null && user !== undefined) {
    console.log(`Found: ${user}`);
} else {
    console.log("User not found");
}
```

### Haskell (Maybe)

```haskell
-- Maybe type: either Just value or Nothing
findUser :: Int -> Maybe String
findUser userId =
    case userId of
        1 -> Just "Alice"
        2 -> Just "Bob"
        _ -> Nothing

main :: IO ()
main = do
    case findUser 1 of
        Just user -> putStrLn $ "Found: " ++ user
        Nothing -> putStrLn "User not found"

    case findUser 999 of
        Just user -> putStrLn $ "Found: " ++ user
        Nothing -> putStrLn "User not found"  -- This branch
```

### Rust (Option)

```rust
fn find_user(user_id: i32) -> Option<String> {
    match user_id {
        1 => Some("Alice".to_string()),
        2 => Some("Bob".to_string()),
        _ => None,
    }
}

fn main() {
    match find_user(1) {
        Some(user) => println!("Found: {}", user),
        None => println!("User not found"),
    }

    match find_user(999) {
        Some(user) => println!("Found: {}", user),
        None => println!("User not found"),  // This branch
    }

    // Can also use if let:
    if let Some(user) = find_user(1) {
        println!("Found: {}", user);
    }
}
```

**Why is null the "billion-dollar mistake"?**
- **Null pointer exceptions:** Accessing null causes crashes
- **Implicit:** Can't tell from type if value might be null
- **Maybe/Option solution:** Type system tracks possibility of "no value"
- **Forces handling:** Must explicitly handle None/Nothing case

---

## Challenge Solutions

### Challenge 1: Type Detective

```python
# Python
def type_detective(value):
    return type(value).__name__

print(type_detective(42))        # int
print(type_detective("hello"))   # str
print(type_detective([1, 2, 3])) # list
print(type_detective(3.14))      # float
```

```javascript
// JavaScript
function typeDetective(value) {
    return typeof value;
}

console.log(typeDetective(42));        // number
console.log(typeDetective("hello"));   // string
console.log(typeDetective([1, 2, 3])); // object (!)
console.log(typeDetective(true));      // boolean
```

### Challenge 2: Temperature Converter

```python
# Python
def convert_temperature(temp, unit):
    """Convert between Celsius and Fahrenheit

    Args:
        temp: Temperature value
        unit: 'C' or 'F'

    Returns:
        (converted_temp, converted_unit) or None if invalid
    """
    if not isinstance(temp, (int, float)):
        return None

    if unit.upper() == 'C':
        # Celsius to Fahrenheit
        return (temp * 9/5 + 32, 'F')
    elif unit.upper() == 'F':
        # Fahrenheit to Celsius
        return ((temp - 32) * 5/9, 'C')
    else:
        return None

print(convert_temperature(0, 'C'))   # (32.0, 'F')
print(convert_temperature(32, 'F'))  # (0.0, 'C')
print(convert_temperature(100, 'C')) # (212.0, 'F')
```

### Challenge 3: Variable Swap

```python
# Python - tuple unpacking
a = 5
b = 10
print(f"Before: a={a}, b={b}")

a, b = b, a
print(f"After: a={a}, b={b}")
```

```javascript
// JavaScript - destructuring
let a = 5;
let b = 10;
console.log(`Before: a=${a}, b=${b}`);

[a, b] = [b, a];
console.log(`After: a=${a}, b=${b}`);
```

```c
// C - XOR trick (for integers only)
int a = 5;
int b = 10;
printf("Before: a=%d, b=%d\n", a, b);

a = a ^ b;
b = a ^ b;
a = a ^ b;
printf("After: a=%d, b=%d\n", a, b);
```

---

## Common Mistakes

1. **Confusing `==` and `is` in Python:** `==` compares values, `is` compares identity
2. **JavaScript's `==` vs `===`:** Always use `===` to avoid type coercion
3. **Forgetting `mut` in Rust:** Variables immutable by default
4. **Type annotation syntax:** Different in each language
5. **Null checking:** Must check for null/None/Nothing before using value

---

## Key Takeaways

1. **Type systems** catch different classes of errors at different times
2. **Static typing** provides early error detection and better tooling
3. **Dynamic typing** provides flexibility and rapid prototyping
4. **Immutability** makes code easier to reason about
5. **Explicit mutability** (Rust) combines safety with practicality
6. **Option/Maybe** types eliminate null pointer errors

Remember: No type system is "best" - each makes different trade-offs for different use cases!
