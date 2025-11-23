# Lesson 2: Variables and Types

## Learning Objectives

- Understand what variables are and how to use them
- Learn about different data types across languages
- Explore static vs dynamic typing
- Understand type inference
- Compare type systems across paradigms

## Concept: Variables

A **variable** is a named storage location that holds a value. Think of it as a labeled box where you can put data.

## Key Concepts

### Static vs Dynamic Typing

**Statically Typed** (C++, Haskell):
- Types are checked at compile-time
- Variables must be declared with their type (or type can be inferred)
- Type errors caught before running
- Generally faster execution
- More verbose but safer

**Dynamically Typed** (Python):
- Types are checked at runtime
- Variables can hold values of any type
- Type determined by the value currently stored
- More flexible but can have runtime errors
- Less verbose

### Type Inference

Some statically typed languages (Haskell, and modern C++ with `auto`) can **infer** types automatically:
- Get the safety of static typing
- Without the verbosity of type annotations
- Best of both worlds!

## Examples

### Python (Dynamic, Strong)

```python
# variables_types.py

# Numbers
age = 25                    # int
price = 19.99              # float
complex_num = 3 + 4j       # complex

# Strings
name = "Alice"             # str
greeting = 'Hello'         # single or double quotes

# Booleans
is_student = True          # bool
has_graduated = False

# None (null/nil)
nothing = None

# Python is dynamically typed - variables can change type
x = 42          # x is an int
x = "now text"  # x is now a str (allowed!)

# Type checking
print(type(age))        # <class 'int'>
print(type(name))       # <class 'str'>

# Type conversion
num_str = "123"
num = int(num_str)      # Convert string to int
print(num + 1)          # 124
```

**Key points:**
- No type declarations needed
- Variables created on first assignment
- `type()` function shows current type
- Can reassign to different type

---

### C++ (Static, Strong)

```cpp
// variables_types.cpp
#include <iostream>
#include <string>
#include <typeinfo>

int main() {
    // Must declare type before use (or use auto)

    // Integers (various sizes)
    char small = 127;              // 8-bit integer
    short medium = 32000;          // 16-bit integer
    int age = 25;                  // 32-bit integer (usually)
    long big = 2147483647L;        // 64-bit integer
    long long huge = 9223372036854775807LL;

    // Floating point
    float price = 19.99f;          // 32-bit float
    double precise = 3.14159265359; // 64-bit float

    // Character
    char letter = 'A';             // single character

    // Boolean
    bool is_student = true;

    // Strings (C++ string class)
    std::string name = "Alice";

    // Type inference with auto (C++11+)
    auto inferred_int = 42;        // int
    auto inferred_double = 3.14;   // double
    auto inferred_string = "text"; // const char*
    auto inferred_str = std::string("text"); // std::string

    // Cannot change type!
    // age = "text";  // ERROR: incompatible types

    std::cout << "Age: " << age << std::endl;
    std::cout << "Price: " << price << std::endl;
    std::cout << "Name: " << name << std::endl;
    std::cout << "Is student: " << std::boolalpha << is_student << std::endl;

    return 0;
}
```

**Key points:**
- Must declare types explicitly (or use `auto`)
- Multiple integer types of different sizes
- `std::string` for text (better than C-style char arrays)
- Very strict type checking
- `auto` keyword for type inference (C++11+)

---

### Haskell (Static, Strong, Inferred)

```haskell
-- variables_types.hs

-- Type signatures (optional but recommended)
age :: Int
age = 25

price :: Double
price = 19.99

name :: String
name = "Alice"

isStudent :: Bool
isStudent = True

-- Type inference - no signature needed
inferredInt = 42        -- Haskell infers: Num a => a
inferredString = "hi"   -- Inferred as String

-- All "variables" are actually constants (immutable)
-- age = 30  -- ERROR: cannot redefine

-- Type checking in GHCi
-- :type age          -- age :: Int
-- :type price        -- price :: Double

-- Polymorphic types
identity :: a -> a      -- works for any type 'a'
identity x = x

-- Tuples (fixed size, mixed types)
person :: (String, Int)
person = ("Alice", 25)

-- Lists (variable size, same type)
numbers :: [Int]
numbers = [1, 2, 3, 4, 5]

main :: IO ()
main = do
    print age
    print name
    print isStudent
```

**Key points:**
- Strong type inference
- Everything is immutable by default
- Type signatures optional but helpful
- Polymorphic types with type variables

---

## Type System Comparison

| Feature            | Python              | C++                 | Haskell             |
|--------------------|---------------------|---------------------|---------------------|
| **Typing**         | Dynamic, Strong     | Static, Strong      | Static, Strong      |
| **Type Inference** | N/A (dynamic)       | Limited (`auto`)    | Excellent           |
| **Immutability**   | No (mutable)        | No (use `const`)    | Yes (default)       |
| **Type Annotation**| Optional (hints)    | Required or `auto`  | Optional            |
| **Type Safety**    | Runtime checks      | Compile-time checks | Compile-time checks |
| **Learning Curve** | Easy                | Medium              | Steep               |
| **Performance**    | Slower              | Fastest             | Fast                |

## Detailed Language Comparisons

### Python - The Flexible Scripter

**Strengths:**
- Quick to write and prototype
- No need to think about types upfront
- Very readable and concise
- Great for rapid development

**Weaknesses:**
- Type errors only caught at runtime
- Can be slower for computation-heavy tasks
- Harder to refactor large codebases safely

**When to use:**
- Scripting and automation
- Data analysis and machine learning
- Web development with frameworks like Django/Flask
- Rapid prototyping

---

### C++ - The Performance Powerhouse

**Strengths:**
- Maximum performance and control
- Direct memory management
- Excellent for system programming
- Large ecosystem and libraries

**Weaknesses:**
- More verbose than dynamic languages
- Steeper learning curve
- Manual memory management (though modern C++ helps)
- Longer compile times

**When to use:**
- Game engines and graphics
- Operating systems and drivers
- High-performance computing
- Embedded systems

---

### Haskell - The Pure Mathematician

**Strengths:**
- Excellent type inference and type safety
- Immutability prevents many bugs
- Lazy evaluation
- Mathematical elegance

**Weaknesses:**
- Steepest learning curve
- Different paradigm (pure functional)
- Smaller ecosystem than mainstream languages
- Can be harder to reason about performance

**When to use:**
- Financial systems requiring correctness
- Compilers and language tools
- Concurrent and parallel systems
- Learning functional programming concepts

---

## Common Data Types Across Languages

### Numeric Types

| Type         | Python     | C++              | Haskell        |
|--------------|------------|------------------|----------------|
| Integer      | `int`      | `int`, `long`    | `Int`, `Integer`|
| Decimal      | `float`    | `float`, `double`| `Float`, `Double`|
| Complex      | `complex`  | `std::complex`   | `Complex`      |

### Text Types

| Type         | Python     | C++              | Haskell        |
|--------------|------------|------------------|----------------|
| Character    | `str` (1)  | `char`           | `Char`         |
| String       | `str`      | `std::string`    | `String`       |

### Boolean Types

| Type         | Python     | C++              | Haskell        |
|--------------|------------|------------------|----------------|
| Boolean      | `bool`     | `bool`           | `Bool`         |
| Values       | `True`, `False` | `true`, `false` | `True`, `False`|

### Null/Nothing

| Type         | Python     | C++              | Haskell        |
|--------------|------------|------------------|----------------|
| Null/Nothing | `None`     | `nullptr`, `NULL`| `Nothing` (Maybe)|

---

## Type Conversion

### Python
```python
# Explicit conversion
x = int("42")        # String to int
y = str(123)         # Int to string
z = float("3.14")    # String to float

# Implicit conversion (coercion)
result = 5 + 2.0     # int + float = float
```

### C++
```cpp
// Explicit conversion
int x = std::stoi("42");          // String to int
std::string y = std::to_string(123); // Int to string
double z = std::stod("3.14");     // String to double

// C-style cast (avoid)
int a = (int)3.14;

// C++ style casts (preferred)
int b = static_cast<int>(3.14);
```

### Haskell
```haskell
-- Explicit conversion functions
x = read "42" :: Int        -- String to Int
y = show 123                -- Int to String

-- Numeric conversions
a = fromIntegral 5 :: Double  -- Int to Double
b = floor 3.14 :: Int         -- Double to Int
```

---

## Mutability and Constants

### Python
```python
# Everything is mutable by default
x = 10
x = 20  # Allowed

# Convention: UPPERCASE for constants (not enforced)
PI = 3.14159
PI = 3  # Bad practice but allowed!
```

### C++
```cpp
// Variables are mutable by default
int x = 10;
x = 20;  // Allowed

// const for constants
const double PI = 3.14159;
// PI = 3;  // Error: assignment of read-only variable

// constexpr for compile-time constants
constexpr int SIZE = 100;
```

### Haskell
```haskell
-- Everything is immutable by default!
x = 10
-- x = 20  -- Error: multiple declarations

-- To "change" values, create new ones
let x = 10 in
  let x = 20 in  -- This shadows the original x
    x  -- Returns 20
```

---

## Type Checking and Errors

### Runtime Type Checking (Python)
```python
x = "5"
y = 10
# This will fail at runtime:
# result = x + y  # TypeError: can only concatenate str to str

# Check types at runtime
if isinstance(x, int):
    result = x + y
```

### Compile-Time Type Checking (C++)
```cpp
std::string x = "5";
int y = 10;
// This will fail at compile time:
// int result = x + y;  // Error: invalid operands

// Must explicitly convert
int result = std::stoi(x) + y;
```

### Compile-Time Type Checking (Haskell)
```haskell
x = "5" :: String
y = 10 :: Int
-- This will fail at compile time:
-- result = x + y  -- Error: couldn't match type

-- Must explicitly convert
result = read x + y  -- read converts String to Int
```

---

## Exercises

See [EXERCISES.md](EXERCISES.md) for hands-on practice with:
- Variable declarations in all three languages
- Type errors and conversions
- Type inference exploration
- Mutability challenges
- Type safety comparisons

## Discussion Questions

1. **Static vs Dynamic:** What are the advantages of static typing? When might dynamic typing be preferable?

2. **Immutability:** Why might a language designer choose to make variables immutable by default (like Haskell)?

3. **Type Inference:** How does type inference provide both safety and convenience? What are its limitations?

4. **Performance:** How does the type system affect program performance?

5. **Development Speed:** Does static typing slow down initial development? Speed up maintenance?

## Looking Ahead

In this course, you're focusing on three core languages that represent different paradigms:
- **Python** for dynamic, imperative programming
- **C++** for performance and systems programming
- **Haskell** for pure functional programming

Later in the course, you'll also explore:
- **Racket** (functional with Lisp heritage)
- **C** (low-level systems programming)
- **Rust** (modern systems language with safety)
- **Prolog** (logic programming)

## Next Lesson

In Lesson 3, we'll explore **Control Flow** structures (if/else, loops) and see how different paradigms approach conditional execution and iteration.
