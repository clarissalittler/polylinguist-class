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

**Statically Typed** (C, Java, Haskell, OCaml, Rust):
- Types are checked at compile-time
- Variables must be declared with their type (or type can be inferred)
- Type errors caught before running
- Generally faster execution
- More verbose but safer

**Dynamically Typed** (Python, JavaScript, Ruby):
- Types are checked at runtime
- Variables can hold values of any type
- Type determined by the value currently stored
- More flexible but can have runtime errors
- Less verbose

### Type Inference

Some statically typed languages (Haskell, OCaml, Rust) can **infer** types automatically:
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

### JavaScript (Dynamic, Weak)

```javascript
// variables_types.js

// Variable declarations (3 ways)
var oldStyle = "avoid var";     // function-scoped (old)
let mutable = "can change";     // block-scoped
const immutable = "cannot reassign";  // block-scoped, constant

// Numbers (only one number type!)
let age = 25;                   // integer value
let price = 19.99;              // floating-point value
// Both are actually 64-bit floats

// Strings
let name = "Alice";
let greeting = 'Hello';
let template = `Hello, ${name}!`;  // template literals

// Booleans
let isStudent = true;
let hasGraduated = false;

// Null and Undefined
let nothing = null;             // intentional absence
let notDefined;                 // undefined (not initialized)

// Type checking
console.log(typeof age);        // "number"
console.log(typeof name);       // "string"

// Weak typing - automatic conversions
console.log("5" + 3);           // "53" (string concatenation)
console.log("5" - 3);           // 2 (numeric subtraction)
console.log(true + 1);          // 2 (boolean to number)
```

**Key points:**
- Three ways to declare variables
- Only one numeric type
- Weak typing can cause surprises
- `typeof` operator for type checking

---

### C (Static, Strong)

```c
// variables_types.c
#include <stdio.h>
#include <stdbool.h>

int main() {
    // Must declare type before use

    // Integers (various sizes)
    char small = 127;              // 8-bit integer
    short medium = 32000;          // 16-bit integer
    int age = 25;                  // 32-bit integer (usually)
    long big = 2147483647L;        // 64-bit integer

    // Floating point
    float price = 19.99f;          // 32-bit float
    double precise = 3.14159265359; // 64-bit float

    // Character
    char letter = 'A';             // single character

    // Boolean (need stdbool.h)
    bool is_student = true;

    // Strings (character arrays)
    char name[] = "Alice";

    // Cannot change type!
    // age = "text";  // ERROR: incompatible types

    printf("Age: %d\n", age);
    printf("Price: %.2f\n", price);
    printf("Name: %s\n", name);

    return 0;
}
```

**Key points:**
- Must declare types explicitly
- Multiple integer types of different sizes
- No string type (use char arrays)
- Very strict type checking
- Direct memory control

---

### Java (Static, Strong)

```java
// VariablesTypes.java
public class VariablesTypes {
    public static void main(String[] args) {
        // Primitive types
        byte smallNum = 127;           // 8-bit
        short mediumNum = 32000;       // 16-bit
        int age = 25;                  // 32-bit
        long bigNum = 9223372036854775807L;  // 64-bit

        float price = 19.99f;          // 32-bit float
        double precise = 3.14159265359; // 64-bit float

        char letter = 'A';             // Unicode character
        boolean isStudent = true;

        // Reference types (objects)
        String name = "Alice";         // String is an object
        Integer boxedAge = 25;         // Wrapper for int

        // Type inference (Java 10+)
        var inferredType = "I'm a String";  // type inferred
        var number = 42;               // inferred as int

        // Cannot change type
        // age = "text";  // ERROR: incompatible types

        System.out.println("Age: " + age);
        System.out.println("Name: " + name);
    }
}
```

**Key points:**
- Primitive types (lowercase) and reference types
- Must declare types (or use `var` for inference)
- Wrapper classes for primitives
- String is an object, not primitive

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

### OCaml (Static, Strong, Inferred)

```ocaml
(* variables_types.ml *)

(* Type inference - no annotations needed *)
let age = 25                    (* inferred as int *)
let price = 19.99               (* inferred as float *)
let name = "Alice"              (* inferred as string *)
let is_student = true           (* inferred as bool *)

(* Optional type annotations *)
let (explicit_age : int) = 25
let (explicit_name : string) = "Alice"

(* Immutable by default *)
(* age = 30  (* ERROR: cannot reassign *) *)

(* Mutable references (if needed) *)
let counter = ref 0             (* int ref *)
let () = counter := !counter + 1  (* dereference and assign *)

(* Tuples *)
let person = ("Alice", 25)      (* string * int *)

(* Lists (same type) *)
let numbers = [1; 2; 3; 4; 5]   (* int list *)

(* Type checking in utop *)
(* age;;  (* - : int = 25 *) *)

(* Printing *)
let () = Printf.printf "Age: %d\n" age
let () = Printf.printf "Name: %s\n" name
let () = Printf.printf "Price: %.2f\n" price
```

**Key points:**
- Excellent type inference
- Immutable by default
- `;` separates list elements
- `ref` for mutable data when needed

---

### Rust (Static, Strong, Inferred)

```rust
// variables_types.rs

fn main() {
    // Immutable by default
    let age = 25;               // inferred as i32
    let price = 19.99;          // inferred as f64
    let name = "Alice";         // &str (string slice)
    let is_student = true;      // bool

    // Explicit type annotations
    let explicit_age: i32 = 25;
    let explicit_price: f64 = 19.99;

    // Mutable variables (must be declared)
    let mut counter = 0;
    counter = counter + 1;      // OK
    // age = 30;                // ERROR: cannot assign twice

    // Integer types (signed and unsigned)
    let signed: i32 = -42;      // i8, i16, i32, i64, i128
    let unsigned: u32 = 42;     // u8, u16, u32, u64, u128

    // Floating point
    let f32_num: f32 = 3.14;
    let f64_num: f64 = 3.14159265359;

    // Character (Unicode)
    let letter: char = 'A';
    let emoji: char = '😀';

    // Tuples
    let person: (&str, i32) = ("Alice", 25);

    // Arrays (fixed size)
    let numbers: [i32; 5] = [1, 2, 3, 4, 5];

    println!("Age: {}", age);
    println!("Name: {}", name);
    println!("Person: {:?}", person);
}
```

**Key points:**
- Immutable by default (use `mut` for mutable)
- Strong type inference
- Many integer types (different sizes, signed/unsigned)
- Memory safety without garbage collection

---

## Type System Comparison

| Language   | Typing          | Inference | Immutable Default | Type Annotation |
|------------|-----------------|-----------|-------------------|-----------------|
| Python     | Dynamic, Strong | N/A       | No                | Optional (hints)|
| JavaScript | Dynamic, Weak   | N/A       | No (use `const`)  | No              |
| C          | Static, Strong  | No        | No                | Required        |
| Java       | Static, Strong  | Limited   | No (`final`)      | Required/`var`  |
| Haskell    | Static, Strong  | Excellent | Yes               | Optional        |
| OCaml      | Static, Strong  | Excellent | Yes               | Optional        |
| Rust       | Static, Strong  | Excellent | Yes               | Optional        |

## Exercises

1. **Type Detective**: In each language, create variables of different types and use the language's way to check their types

2. **Type Errors**: Try to cause a type error in each language and observe the difference between compile-time and runtime errors

3. **Conversion Challenge**: Write a program that reads a string input and converts it to a number in 3 different languages

4. **Type Safety**: Compare what happens when you try to divide a number by zero in Python, C, and Haskell

## Discussion Questions

1. What are the advantages of static typing? Dynamic typing?
2. Why might a language designer choose to make variables immutable by default?
3. How does type inference provide both safety and convenience?
4. When would you prefer weak typing (JavaScript) vs strong typing (Python)?

## Next Lesson

In Lesson 3, we'll explore control flow structures (if/else, loops) and see how different paradigms approach conditional execution and iteration.
