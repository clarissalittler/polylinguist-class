# Rust Module
## Modern Systems Programming with Safety Guarantees

**Duration:** 1-2 weeks (Weeks 9-10)
**Prerequisites:** Lessons 1-10, C Module recommended
**Languages:** Rust

---

## Overview

Rust (2010, Mozilla) is a modern systems programming language that provides memory safety without garbage collection. It achieves this through an innovative **ownership system** that catches memory bugs at compile time.

### Why Learn Rust?

1. **Memory safety** without garbage collection
2. **Modern tooling**: Cargo (package manager), rustfmt, clippy
3. **Zero-cost abstractions**: High-level features compile to efficient code
4. **Growing ecosystem**: Web, CLI tools, embedded, WebAssembly
5. **Loved by developers**: #1 "most loved language" on Stack Overflow surveys

### What Makes Rust Different?

| Feature | C/C++ | Rust |
|---------|-------|------|
| Memory safety | Manual (unsafe) | Compiler enforced |
| Null pointers | Common bug source | No null (Option type) |
| Data races | Possible | Prevented at compile time |
| Memory management | Manual or RAII | Ownership system |
| Error handling | Return codes, exceptions | Result type |

---

## Installation

```bash
# Install rustup (Rust installer)
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# Verify
rustc --version
cargo --version
```

---

## Part 1: Basics (Day 1)

### Hello, World!

```rust
fn main() {
    println!("Hello, World!");
}
```

Compile and run:
```bash
rustc hello.rs
./hello

# Or use Cargo (recommended)
cargo new hello_project
cd hello_project
cargo run
```

### Variables and Mutability

```rust
fn main() {
    // Immutable by default!
    let x = 5;
    // x = 6;  // ERROR: cannot assign twice to immutable variable

    // Use mut for mutable
    let mut y = 5;
    y = 6;  // OK

    // Type annotations (usually inferred)
    let z: i32 = 42;

    // Constants (always immutable, must be typed)
    const MAX_POINTS: u32 = 100_000;

    // Shadowing (re-declare with let)
    let x = x + 1;  // New x shadows old x
    let x = "now I'm a string";  // Can even change type!
}
```

### Basic Types

```rust
fn main() {
    // Integers
    let i: i32 = 42;      // Signed 32-bit (default)
    let u: u64 = 100;     // Unsigned 64-bit
    let byte: u8 = 255;   // Byte

    // Floats
    let f: f64 = 3.14;    // 64-bit float (default)

    // Boolean
    let t: bool = true;

    // Character (Unicode!)
    let c: char = 'z';
    let emoji: char = '🦀';

    // Tuples
    let tup: (i32, f64, char) = (500, 6.4, 'y');
    let (x, y, z) = tup;  // Destructuring
    let first = tup.0;    // Index access

    // Arrays (fixed size)
    let arr: [i32; 5] = [1, 2, 3, 4, 5];
    let first = arr[0];
}
```

### Functions

```rust
fn main() {
    let result = add(5, 3);
    println!("5 + 3 = {}", result);
}

fn add(x: i32, y: i32) -> i32 {
    x + y  // No semicolon = expression (returns value)
}

// With explicit return
fn subtract(x: i32, y: i32) -> i32 {
    return x - y;  // Semicolon required with return
}
```

---

## Part 2: Ownership (Day 1-2)

### The Ownership Rules

1. Each value has exactly one **owner**
2. When the owner goes out of scope, the value is dropped
3. There can only be one owner at a time

```rust
fn main() {
    let s1 = String::from("hello");  // s1 owns the string
    let s2 = s1;                      // Ownership MOVES to s2
    // println!("{}", s1);            // ERROR: s1 no longer valid!
    println!("{}", s2);               // OK
}
```

### Move vs Copy

```rust
fn main() {
    // Copy types (stored on stack): integers, floats, bool, char
    let x = 5;
    let y = x;  // Copy, both valid
    println!("x = {}, y = {}", x, y);  // OK

    // Move types (stored on heap): String, Vec, etc.
    let s1 = String::from("hello");
    let s2 = s1;  // Move, s1 invalid
    // println!("{}", s1);  // ERROR
}
```

### Clone (Deep Copy)

```rust
fn main() {
    let s1 = String::from("hello");
    let s2 = s1.clone();  // Explicit deep copy
    println!("s1 = {}, s2 = {}", s1, s2);  // Both valid
}
```

### Ownership and Functions

```rust
fn main() {
    let s = String::from("hello");
    takes_ownership(s);       // s moves into function
    // println!("{}", s);     // ERROR: s no longer valid

    let x = 5;
    makes_copy(x);            // Copy passed
    println!("{}", x);        // OK: x still valid
}

fn takes_ownership(s: String) {
    println!("{}", s);
}  // s is dropped here

fn makes_copy(x: i32) {
    println!("{}", x);
}
```

---

## Part 3: Borrowing and References (Day 2-3)

### References (Borrowing)

Instead of moving ownership, you can **borrow** with references:

```rust
fn main() {
    let s1 = String::from("hello");
    let len = calculate_length(&s1);  // Borrow with &
    println!("Length of '{}' is {}", s1, len);  // s1 still valid!
}

fn calculate_length(s: &String) -> usize {
    s.len()
}  // s goes out of scope but doesn't drop the value (it doesn't own it)
```

### Mutable References

```rust
fn main() {
    let mut s = String::from("hello");
    change(&mut s);  // Mutable borrow
    println!("{}", s);  // "hello, world"
}

fn change(s: &mut String) {
    s.push_str(", world");
}
```

### Borrowing Rules

1. You can have **either** one mutable reference **or** any number of immutable references
2. References must always be valid (no dangling references)

```rust
fn main() {
    let mut s = String::from("hello");

    let r1 = &s;      // OK: immutable borrow
    let r2 = &s;      // OK: another immutable borrow
    println!("{} and {}", r1, r2);
    // r1 and r2 no longer used after this point

    let r3 = &mut s;  // OK: mutable borrow (r1, r2 out of scope)
    println!("{}", r3);
}

// This would NOT compile:
fn bad() {
    let mut s = String::from("hello");
    let r1 = &s;
    let r2 = &mut s;  // ERROR: can't borrow as mutable while immutable borrow exists
    println!("{}, {}", r1, r2);
}
```

---

## Part 4: Lifetimes (Day 3)

Lifetimes ensure references are valid:

```rust
// This won't compile - Rust doesn't know which lifetime to use
// fn longest(x: &str, y: &str) -> &str {

// Explicit lifetime annotation
fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
    if x.len() > y.len() {
        x
    } else {
        y
    }
}

fn main() {
    let s1 = String::from("long string");
    let s2 = String::from("short");

    let result = longest(&s1, &s2);
    println!("Longest: {}", result);
}
```

The `'a` means "the returned reference is valid as long as both inputs are valid."

---

## Part 5: Enums and Pattern Matching (Day 3-4)

### Enums

```rust
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

// Enums with data
enum Message {
    Quit,
    Move { x: i32, y: i32 },
    Write(String),
    ChangeColor(i32, i32, i32),
}

fn main() {
    let dir = Direction::Up;
    let msg = Message::Move { x: 10, y: 20 };
}
```

### Option: No More Null!

```rust
// Option is defined as:
// enum Option<T> {
//     Some(T),
//     None,
// }

fn main() {
    let some_number: Option<i32> = Some(5);
    let no_number: Option<i32> = None;

    // Must handle both cases!
    match some_number {
        Some(n) => println!("Got {}", n),
        None => println!("Got nothing"),
    }
}

fn divide(a: f64, b: f64) -> Option<f64> {
    if b == 0.0 {
        None
    } else {
        Some(a / b)
    }
}
```

### Pattern Matching

```rust
fn main() {
    let x = 5;

    match x {
        1 => println!("one"),
        2 | 3 => println!("two or three"),
        4..=10 => println!("four through ten"),
        _ => println!("something else"),  // _ is wildcard
    }

    // With enums
    let msg = Message::Move { x: 10, y: 20 };
    match msg {
        Message::Quit => println!("Quit"),
        Message::Move { x, y } => println!("Move to ({}, {})", x, y),
        Message::Write(text) => println!("Write: {}", text),
        Message::ChangeColor(r, g, b) => println!("Color: {},{},{}", r, g, b),
    }

    // if let for single pattern
    if let Some(n) = some_option {
        println!("Got {}", n);
    }
}
```

---

## Part 6: Error Handling (Day 4)

### Result Type

```rust
// Result is defined as:
// enum Result<T, E> {
//     Ok(T),
//     Err(E),
// }

use std::fs::File;
use std::io::Read;

fn read_file(path: &str) -> Result<String, std::io::Error> {
    let mut file = File::open(path)?;  // ? propagates error
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}

fn main() {
    match read_file("hello.txt") {
        Ok(contents) => println!("{}", contents),
        Err(e) => println!("Error: {}", e),
    }

    // Or use unwrap (panics on error) / expect (panics with message)
    let contents = read_file("hello.txt").unwrap();
    let contents = read_file("hello.txt").expect("Failed to read file");
}
```

---

## Part 7: Structs and Methods (Day 4-5)

### Structs

```rust
struct Point {
    x: f64,
    y: f64,
}

struct Rectangle {
    width: u32,
    height: u32,
}

// Methods
impl Rectangle {
    // Method (takes &self)
    fn area(&self) -> u32 {
        self.width * self.height
    }

    // Method with mutable self
    fn double(&mut self) {
        self.width *= 2;
        self.height *= 2;
    }

    // Associated function (no self, like static method)
    fn square(size: u32) -> Rectangle {
        Rectangle { width: size, height: size }
    }
}

fn main() {
    let rect = Rectangle { width: 30, height: 50 };
    println!("Area: {}", rect.area());

    let square = Rectangle::square(10);

    let mut rect2 = Rectangle { width: 5, height: 5 };
    rect2.double();
}
```

---

## Part 8: Collections (Day 5)

### Vector

```rust
fn main() {
    // Create
    let mut v: Vec<i32> = Vec::new();
    let v2 = vec![1, 2, 3, 4, 5];  // Macro

    // Add elements
    v.push(1);
    v.push(2);

    // Access
    let third = &v2[2];           // Panics if out of bounds
    let third = v2.get(2);        // Returns Option<&i32>

    // Iterate
    for i in &v2 {
        println!("{}", i);
    }

    for i in &mut v {
        *i += 10;  // Modify in place
    }
}
```

### String

```rust
fn main() {
    let mut s = String::new();
    let s2 = "hello".to_string();
    let s3 = String::from("hello");

    // Append
    s.push_str("hello");
    s.push(' ');
    s.push_str("world");

    // Concatenation
    let s4 = s2 + &s3;  // s2 is moved, s3 is borrowed

    // Format macro (doesn't take ownership)
    let s5 = format!("{} {}", "hello", "world");
}
```

### HashMap

```rust
use std::collections::HashMap;

fn main() {
    let mut scores = HashMap::new();

    scores.insert(String::from("Blue"), 10);
    scores.insert(String::from("Yellow"), 50);

    // Access
    let team = String::from("Blue");
    let score = scores.get(&team);  // Returns Option<&V>

    // Iterate
    for (key, value) in &scores {
        println!("{}: {}", key, value);
    }

    // Entry API
    scores.entry(String::from("Red")).or_insert(25);
}
```

---

## Comparing with Core Languages

| Feature | Python | C++ | Haskell | Rust |
|---------|--------|-----|---------|------|
| Memory | GC | Manual/RAII | GC | Ownership |
| Null | None | nullptr | Nothing | Option |
| Errors | Exceptions | Exceptions | Maybe/Either | Result |
| Mutability | Default | Default | None | Explicit |
| Generics | Duck typing | Templates | Parametric | Traits |

---

## Exercises

### Exercise RS1: Ownership
```rust
// Fix this code so it compiles
fn main() {
    let s = String::from("hello");
    let len = get_length(s);
    println!("Length of '{}' is {}", s, len);
}

fn get_length(s: String) -> usize {
    s.len()
}
```

### Exercise RS2: Borrowing
```rust
// Implement a function that modifies a string without taking ownership
fn append_world(s: /* what type? */) {
    // Add ", world!" to s
}
```

### Exercise RS3: Structs
```rust
// Implement a Point struct with methods:
// - new(x, y) - constructor
// - distance(&self, other: &Point) - distance between points
// - translate(&mut self, dx, dy) - move the point
```

### Exercise RS4: Error Handling
```rust
// Implement safe division that returns Result
fn safe_divide(a: f64, b: f64) -> Result<f64, String> {
    // Return Err if b is 0, Ok(result) otherwise
}
```

### Exercise RS5: Collections
```rust
// Implement a word frequency counter
fn word_count(text: &str) -> HashMap<String, u32> {
    // Count occurrences of each word
}
```

---

## Key Takeaways

1. **Ownership prevents memory bugs** at compile time
2. **Borrowing** allows multiple readers OR one writer
3. **No null** - use Option instead
4. **No exceptions** - use Result instead
5. **Pattern matching** is powerful and exhaustive
6. **The compiler is your friend** - its errors teach you

---

## Resources

- **The Rust Book**: https://doc.rust-lang.org/book/ (free, excellent)
- **Rust by Example**: https://doc.rust-lang.org/rust-by-example/
- **Rustlings**: https://github.com/rust-lang/rustlings (exercises)
- **Crates.io**: https://crates.io (package registry)

---

## Why Rust Matters

Rust proves that systems programming can be safe without sacrificing performance. Understanding Rust's ownership model teaches you to think carefully about memory—skills that transfer to every language you use.
