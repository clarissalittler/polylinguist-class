# Lesson 2: Variables and Types - Exercises

## Instructions

These exercises will help you understand variables, types, and mutability across different programming paradigms.

---

## Exercise 1: Variable Declaration (Warmup)

**Difficulty:** Easy

Declare variables of different types in at least 3 languages:

```python
# Python
name = "Alice"
age = 25
height = 5.6
is_student = True
```

**Tasks:**
1. Implement in Python
2. Implement in JavaScript
3. Implement in a statically-typed language (C, Java, Rust, or Haskell)

**Questions:**
- Which languages require type annotations?
- Which languages infer types automatically?

---

## Exercise 2: Type Errors

**Difficulty:** Easy

Try to cause type errors in different languages:

**Python:**
```python
x = "5"
y = 10
print(x + y)  # What happens?
```

**JavaScript:**
```javascript
let x = "5";
let y = 10;
console.log(x + y);  # What happens?
```

**C or Java:**
```c
int x = 5;
char* y = "10";
// Can you add these?
```

**Questions:**
- Which languages allow this? Which don't?
- What is "type coercion"?
- Is JavaScript's behavior helpful or confusing?

---

## Exercise 3: Type Inference

**Difficulty:** Medium

Experiment with type inference:

**Haskell:**
```haskell
-- What type is inferred?
x = 42
y = "hello"
z = [1, 2, 3]
```

**Rust:**
```rust
let x = 42;  // What type?
let y = 3.14;  // What type?
let z = vec![1, 2, 3];  // What type?
```

**Tasks:**
- Write the code
- Use `:type` in GHCi (Haskell) or compiler errors (Rust) to see inferred types
- Try to mix types incorrectly and see the error messages

---

## Exercise 4: Mutability Challenge

**Difficulty:** Medium

**Task 1: Python (Mutable)**
```python
x = 10
print(x)
x = 20
print(x)  # Works fine
```

**Task 2: Haskell (Immutable)**
```haskell
x = 10
-- Can you "change" x to 20? Try it!
```

**Task 3: Rust (Explicit)**
```rust
let x = 10;
// x = 20;  // Will this work?

let mut y = 10;
// y = 20;  // Will this work?
```

**Questions:**
- Why doesn't Haskell allow reassignment?
- What's the benefit of Rust's explicit `mut`?
- When might immutability be useful?

---

## Exercise 5: Type Conversion

**Difficulty:** Medium

Convert between types in multiple languages:

**Python:**
```python
# String to int
x = int("42")

# Int to string
y = str(123)

# Float to int (truncates)
z = int(3.99)
```

**JavaScript:**
```javascript
// String to number
let x = Number("42");
let y = parseInt("42");

// Number to string
let z = String(123);
let w = (123).toString();
```

**Tasks:**
1. Convert string → number → string in Python and JavaScript
2. Try in a statically-typed language (Rust or Haskell)
3. What happens with invalid conversions? ("hello" → number)

---

## Exercise 6: Constants vs Variables

**Difficulty:** Easy

Understand the difference between constants and variables:

**Python:**
```python
# Convention only (all caps = constant)
PI = 3.14159
# But you CAN change it!
PI = 3  # Bad practice, but allowed
```

**JavaScript:**
```javascript
// const prevents reassignment
const PI = 3.14159;
// PI = 3;  // Error!

// But watch out:
const arr = [1, 2, 3];
arr.push(4);  // Allowed! const doesn't prevent mutation
```

**Rust:**
```rust
// Immutable by default
let x = 10;
// x = 20;  // Error

// True constant (compile-time)
const PI: f64 = 3.14159;
```

**Questions:**
- What does `const` prevent in JavaScript?
- How does Rust's approach differ?
- Why are constants useful?

---

## Exercise 7: Dynamic vs Static Typing

**Difficulty:** Medium

Compare dynamic and static typing:

**Python (Dynamic):**
```python
def process(x):
    return x * 2

print(process(5))      # 10
print(process("hi"))   # "hihi"
print(process([1, 2])) # [1, 2, 1, 2]
```

**Java (Static):**
```java
public static int process(int x) {
    return x * 2;
}
// Can only call with int!
```

**Tasks:**
1. Implement both versions
2. Try calling the static version with wrong types
3. Try calling the dynamic version with unexpected types

**Questions:**
- What are the advantages of each approach?
- Which catches errors earlier?
- Which is more flexible?

---

## Exercise 8: Scope Exploration

**Difficulty:** Medium

Understand variable scope:

**Python:**
```python
x = "global"

def outer():
    x = "outer"

    def inner():
        x = "inner"
        print(x)  # What prints?

    inner()
    print(x)  # What prints?

outer()
print(x)  # What prints?
```

**JavaScript:**
```javascript
let x = "global";

function outer() {
    let x = "outer";

    function inner() {
        let x = "inner";
        console.log(x);  // What prints?
    }

    inner();
    console.log(x);  // What prints?
}

outer();
console.log(x);  // What prints?
```

**Tasks:**
1. Predict the output
2. Run the code
3. Try without declaring `x` in inner function

---

## Exercise 9: Type Safety

**Difficulty:** Medium to Hard

Explore how type systems prevent bugs:

**Python (No type safety):**
```python
def divide(a, b):
    return a / b

# All allowed:
divide(10, 2)      # OK
divide("10", "2")  # Runtime error!
divide(10, 0)      # Runtime error!
```

**Haskell (Type safe):**
```haskell
divide :: Float -> Float -> Float
divide a b = a / b

-- divide "10" "2"  -- Compile error!
-- Still: divide 10 0  -- Runtime error
```

**Rust (Type + safety):**
```rust
fn divide(a: f64, b: f64) -> Option<f64> {
    if b == 0.0 {
        None
    } else {
        Some(a / b)
    }
}

// divide("10", "2")  // Compile error!
// divide(10, 0) returns None, not error!
```

**Questions:**
- Which catches errors earliest?
- How does Rust handle division by zero?
- Trade-offs of each approach?

---

## Exercise 10: Build a Calculator

**Difficulty:** Medium

Create a simple calculator that:
1. Takes two numbers and an operator (+, -, *, /)
2. Performs the operation
3. Returns the result

**Implement in:**
- Python (dynamic typing)
- Java or Rust (static typing)

**Bonus:** Handle division by zero gracefully in each language.

---

## Exercise 11: Type Annotations (Python 3.5+)

**Difficulty:** Medium

Python supports optional type hints:

```python
def greet(name: str) -> str:
    return f"Hello, {name}!"

def add(x: int, y: int) -> int:
    return x + y

# These work, but type checkers warn:
greet(123)
add("5", "10")
```

**Tasks:**
1. Add type hints to functions
2. Run `mypy` (install: `pip install mypy`)
3. See what warnings you get

**Questions:**
- Do type hints change runtime behavior?
- Are they enforced?
- What's their benefit?

---

## Exercise 12: Null/None/Nothing

**Difficulty:** Medium to Hard

Different languages handle "no value" differently:

**Python:**
```python
x = None
if x is None:
    print("No value")
```

**JavaScript:**
```javascript
let x = null;
let y = undefined;
// What's the difference?
```

**Haskell:**
```haskell
-- No null! Use Maybe
x :: Maybe Int
x = Nothing  -- No value
y = Just 5   -- Has value
```

**Rust:**
```rust
let x: Option<i32> = None;
let y: Option<i32> = Some(5);
// No null pointers!
```

**Tasks:**
1. Explore null/None in dynamic languages
2. Explore Maybe/Option in static languages
3. Why is null called the "billion-dollar mistake"?

---

## Challenge Projects

### Challenge 1: Type Detective

Write a function that:
1. Takes a value
2. Returns its type as a string
3. Works in both Python and JavaScript

**Python:** Use `type(x).__name__`
**JavaScript:** Use `typeof x`

### Challenge 2: Temperature Converter

Create a temperature converter:
- Input: number and unit (C or F)
- Output: converted temperature
- Handle invalid input

Implement in a statically-typed and dynamically-typed language.

### Challenge 3: Variable Swap

Swap two variables without using a temporary variable.

**Hint:** Use tuple unpacking in Python:
```python
a, b = b, a
```

Can you do this in other languages?

---

## Reflection Questions

1. **Static vs Dynamic:** Which felt more natural? Which caught more errors?

2. **Mutability:** When did immutability help? When was it frustrating?

3. **Type Inference:** Is automatic type inference better than explicit types?

4. **Error Messages:** Which language gave the clearest error messages for type errors?

5. **Safety vs Flexibility:** What's the right balance for different projects?

---

## Going Further

- **Explore:** Research "gradual typing" (TypeScript, Python type hints)
- **Experiment:** Try creating custom types/classes in different languages
- **Compare:** Look up the type systems of other languages (Go, Swift, Kotlin)

Remember: Understanding types deeply makes you a better programmer in ANY language!
