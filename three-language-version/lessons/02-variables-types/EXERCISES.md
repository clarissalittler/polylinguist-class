# Lesson 2: Variables and Types - Exercises

## Instructions

These exercises will help you understand variables, types, and mutability across Python, C++, and Haskell. Complete exercises in all three languages to appreciate the different approaches.

---

## Exercise 1: Variable Declaration (Warmup)

**Difficulty:** Easy

Declare variables of different types in all three languages.

**Python:**
```python
# Python - Implement this
name = "Alice"
age = 25
height = 5.6
is_student = True
```

**C++:**
```cpp
// C++ - Implement this
std::string name = "Alice";
int age = 25;
double height = 5.6;
bool is_student = true;
```

**Haskell:**
```haskell
-- Haskell - Implement this
name :: String
name = "Alice"

age :: Int
age = 25

height :: Double
height = 5.6

isStudent :: Bool
isStudent = True
```

**Tasks:**
1. Implement in all three languages
2. Print/display all variables
3. Use type checking features to verify types

**Questions:**
- Which languages require type annotations?
- Which languages infer types automatically?
- Which approach do you find clearer?

---

## Exercise 2: Type Errors

**Difficulty:** Easy

Explore what happens when you mix incompatible types.

**Python:**
```python
x = "5"
y = 10
# What happens with: print(x + y)
# Try it! Then fix it.
```

**C++:**
```cpp
std::string x = "5";
int y = 10;
// What happens with: std::cout << x + y;
// Does it compile? How do you fix it?
```

**Haskell:**
```haskell
x = "5" :: String
y = 10 :: Int
-- What happens with: result = x ++ show y
-- Try different combinations
```

**Questions:**
- Which language catches the error earliest?
- Which language gives the clearest error message?
- How do you convert between types in each language?

---

## Exercise 3: Type Inference Exploration

**Difficulty:** Medium

Experiment with type inference in C++ and Haskell.

**C++ (auto keyword):**
```cpp
auto x = 42;           // What type?
auto y = 3.14;         // What type?
auto z = "hello";      // What type?
auto w = std::string("hello");  // What type?

// Can you check types at compile time?
// Hint: Trigger an error to see the type
```

**Haskell (GHCi):**
```haskell
x = 42
y = 3.14
z = "hello"
list = [1, 2, 3]

-- Use :type (or :t) in GHCi to check types
-- What types are inferred?
```

**Tasks:**
1. Write the code
2. Use GHCi's `:type` command (Haskell)
3. Try to use variables in ways that cause type errors
4. Observe the error messages

**Questions:**
- When is type inference helpful?
- When do explicit types improve readability?
- Can C++'s `auto` infer function return types?

---

## Exercise 4: Mutability Challenge

**Difficulty:** Medium

Understand the different approaches to mutability.

**Python (Mutable by default):**
```python
x = 10
print(x)
x = 20
print(x)  # Works fine

# Can you make x immutable?
# (Hint: No built-in way, but convention uses CONSTANTS)
```

**C++ (const modifier):**
```cpp
int x = 10;
std::cout << x << std::endl;
x = 20;
std::cout << x << std::endl;  // Works

const int y = 10;
// y = 20;  // Will this compile?
```

**Haskell (Immutable by default):**
```haskell
x = 10
-- Can you "change" x to 20?
-- What happens if you try?

-- You can shadow in a new scope:
demo = let x = 10 in
       let x = 20 in
       x
```

**Questions:**
- Why doesn't Haskell allow reassignment?
- What's the benefit of C++'s explicit `const`?
- When might immutability prevent bugs?

---

## Exercise 5: Type Conversion

**Difficulty:** Medium

Convert between types in all three languages.

**Python:**
```python
# Implement conversions:
# 1. String to int
# 2. Int to string
# 3. Float to int (truncates)
# 4. String to float

# Handle errors:
try:
    bad = int("hello")
except ValueError as e:
    print(f"Error: {e}")
```

**C++:**
```cpp
// Implement conversions:
// 1. String to int (std::stoi)
// 2. Int to string (std::to_string)
// 3. Double to int (static_cast)
// 4. String to double (std::stod)

// Handle errors:
try {
    int bad = std::stoi("hello");
} catch (std::invalid_argument& e) {
    std::cout << "Error: " << e.what() << std::endl;
}
```

**Haskell:**
```haskell
-- Implement conversions:
-- 1. String to Int (read)
-- 2. Int to String (show)
-- 3. Double to Int (floor, ceiling, round)
-- 4. Int to Double (fromIntegral)

-- For safe parsing:
import Text.Read (readMaybe)
safeRead :: String -> Maybe Int
safeRead s = readMaybe s
```

**Tasks:**
1. Convert string → number → string in each language
2. Try invalid conversions ("hello" → number)
3. Compare error handling approaches

---

## Exercise 6: Numeric Type Sizes

**Difficulty:** Medium

Explore the different sizes of numeric types.

**Python:**
```python
import sys

# Python ints have unlimited size!
big = 10 ** 100
print(f"Big number: {big}")

# Floats are 64-bit
print(f"Float info: {sys.float_info}")
```

**C++:**
```cpp
#include <limits>

// Check sizes
std::cout << "int size: " << sizeof(int) << " bytes\n";
std::cout << "long size: " << sizeof(long) << " bytes\n";
std::cout << "double size: " << sizeof(double) << " bytes\n";

// Check ranges
std::cout << "int max: " << std::numeric_limits<int>::max() << "\n";
std::cout << "int min: " << std::numeric_limits<int>::min() << "\n";
```

**Haskell:**
```haskell
-- Int is fixed-size (machine dependent)
import Data.Int

maxInt = maxBound :: Int
minInt = minBound :: Int

-- Integer is unlimited size
bigNum = 10^100 :: Integer
```

**Questions:**
- Which language has unbounded integers?
- What happens with integer overflow in C++?
- Why might you choose Int over Integer in Haskell?

---

## Exercise 7: Dynamic vs Static Typing

**Difficulty:** Medium

Compare dynamic and static typing approaches.

**Python (Dynamic):**
```python
def process(x):
    return x * 2

print(process(5))        # 10
print(process("hi"))     # "hihi"
print(process([1, 2]))   # [1, 2, 1, 2]
# Dynamic typing allows flexibility!
```

**C++ (Static with templates):**
```cpp
template<typename T>
T process(T x) {
    return x + x;  // Works for any type with +
}

// Type-safe but generic
std::cout << process(5) << std::endl;       // 10
std::cout << process(std::string("hi")) << std::endl;  // "hihi"
```

**Haskell (Static with type classes):**
```haskell
-- Polymorphic but type-safe
process :: a -> [a]
process x = [x, x]

-- Or with Num constraint:
double :: Num a => a -> a
double x = x * 2
```

**Questions:**
- What are the advantages of each approach?
- Which catches errors earlier?
- Which is more flexible? Which is safer?

---

## Exercise 8: Boolean Logic

**Difficulty:** Easy

Work with boolean values and logic.

**Python:**
```python
# Implement:
a = True
b = False

# Boolean operations
and_result = a and b
or_result = a or b
not_result = not a

# Truthy and falsy values
if 0:  # Falsy
    print("Won't print")
if [1, 2]:  # Truthy
    print("Will print")
```

**C++:**
```cpp
// Implement:
bool a = true;
bool b = false;

// Boolean operations
bool and_result = a && b;
bool or_result = a || b;
bool not_result = !a;

// Print with boolalpha
std::cout << std::boolalpha << and_result << std::endl;
```

**Haskell:**
```haskell
-- Implement:
a = True
b = False

-- Boolean operations
andResult = a && b
orResult = a || b
notResult = not a
```

**Tasks:**
- Implement truth tables for AND, OR, NOT
- Compare boolean syntax across languages

---

## Exercise 9: Null/Nothing/None

**Difficulty:** Medium to Hard

Understand how each language handles "no value".

**Python:**
```python
def find_user(id):
    if id == 1:
        return {"name": "Alice"}
    else:
        return None

user = find_user(2)
if user is None:
    print("User not found")
else:
    print(user["name"])
```

**C++:**
```cpp
#include <optional>

std::optional<std::string> find_user(int id) {
    if (id == 1) {
        return "Alice";
    } else {
        return std::nullopt;
    }
}

auto user = find_user(2);
if (user.has_value()) {
    std::cout << user.value() << std::endl;
} else {
    std::cout << "User not found" << std::endl;
}
```

**Haskell:**
```haskell
findUser :: Int -> Maybe String
findUser 1 = Just "Alice"
findUser _ = Nothing

-- Pattern matching
case findUser 2 of
    Just name -> putStrLn name
    Nothing -> putStrLn "User not found"
```

**Questions:**
- How does each language prevent null pointer errors?
- What is the "billion-dollar mistake"?
- Which approach is safest?

---

## Exercise 10: Build a Temperature Converter

**Difficulty:** Medium

Create a temperature converter in all three languages.

**Requirements:**
- Convert between Celsius and Fahrenheit
- Handle invalid input
- Display results with 2 decimal places

**Python:**
```python
def celsius_to_fahrenheit(c):
    return c * 9/5 + 32

def fahrenheit_to_celsius(f):
    return (f - 32) * 5/9

# Test your functions
```

**C++:**
```cpp
double celsiusToFahrenheit(double c) {
    return c * 9.0/5.0 + 32.0;
}

double fahrenheitToCelsius(double f) {
    return (f - 32.0) * 5.0/9.0;
}

// Test your functions
```

**Haskell:**
```haskell
celsiusToFahrenheit :: Double -> Double
celsiusToFahrenheit c = c * 9/5 + 32

fahrenheitToCelsius :: Double -> Double
fahrenheitToCelsius f = (f - 32) * 5/9

-- Test your functions
```

---

## Exercise 11: Type Annotations (Python 3.5+)

**Difficulty:** Medium

Python supports optional type hints.

```python
def greet(name: str) -> str:
    return f"Hello, {name}!"

def add(x: int, y: int) -> int:
    return x + y

# These work at runtime, but type checkers warn:
greet(123)
add("5", "10")
```

**Tasks:**
1. Add type hints to functions
2. Install and run `mypy`: `pip install mypy`
3. Run `mypy your_file.py`
4. See what warnings you get

**Questions:**
- Do type hints change runtime behavior?
- Are they enforced by Python?
- What's their benefit?
- How do they compare to C++ types?

---

## Exercise 12: String Manipulation

**Difficulty:** Easy to Medium

Work with strings in all three languages.

**Python:**
```python
# Implement:
s = "Hello, World!"
# - Length
# - Uppercase
# - Lowercase
# - Substring
# - Contains check
# - Split into words
```

**C++:**
```cpp
// Implement:
std::string s = "Hello, World!";
// - Length (.length() or .size())
// - Uppercase (transform with toupper)
// - Lowercase (transform with tolower)
// - Substring (.substr())
// - Contains check (find())
// - Split into words (stringstream)
```

**Haskell:**
```haskell
-- Implement:
s = "Hello, World!"
-- - Length (length)
-- - Uppercase (map toUpper)
-- - Lowercase (map toLower)
-- - Substring (take, drop)
-- - Contains check (isInfixOf)
-- - Split into words (words function)
```

---

## Challenge Projects

### Challenge 1: Type Detective

Write a program that analyzes a value and reports detailed type information.

**Python:** Use `type()`, `isinstance()`, `dir()`
**C++:** Use `typeid()` and templates
**Haskell:** Use `:type` in GHCi or type-level programming

### Challenge 2: Safe Division

Create a division function that handles division by zero safely.

**Python:** Use try/except
**C++:** Use std::optional or exceptions
**Haskell:** Use Maybe type

### Challenge 3: Variable Swap

Swap two variables without using a temporary variable.

**Python:**
```python
a, b = b, a  # Tuple unpacking
```

**C++:**
```cpp
// Can you do it with XOR or std::swap?
```

**Haskell:**
```haskell
-- Variables are immutable, but you can return a tuple
swap (a, b) = (b, a)
```

---

## Reflection Questions

1. **Static vs Dynamic:** Which approach felt more natural? Which caught more errors?

2. **Mutability:** When did immutability help? When was it frustrating?

3. **Type Inference:** Is automatic type inference better than explicit types?

4. **Error Messages:** Which language gave the clearest error messages for type errors?

5. **Safety vs Flexibility:** What's the right balance for different projects?

6. **Learning Curve:** Which type system was easiest to learn? Hardest?

---

## Going Further

- **Python:** Explore the `typing` module and gradual typing
- **C++:** Learn about `auto`, `decltype`, and template metaprogramming
- **Haskell:** Study type classes, kinds, and dependent types
- **Compare:** Look at how Rust's type system combines the best ideas

---

## Testing Your Understanding

You should now be able to:
- [ ] Declare variables in all three languages
- [ ] Explain static vs dynamic typing
- [ ] Use type conversion functions
- [ ] Understand mutability differences
- [ ] Handle null/none/nothing values
- [ ] Work with basic data types (numbers, strings, booleans)

Remember: Understanding types deeply makes you a better programmer in ANY language!
