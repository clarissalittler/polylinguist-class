# Lab 28: Rust Programming

**Quarter 3, Week 8**
**Duration:** 90 minutes
**Format:** Guided exploration

## Overview

Rust offers memory safety without garbage collection through its ownership system. It's increasingly popular for systems programming, web assembly, and anywhere performance and safety matter.

## Objectives

By the end of this lab, you will:
- [ ] Understand ownership, borrowing, and lifetimes
- [ ] Write basic Rust programs
- [ ] Handle errors the Rust way
- [ ] Appreciate Rust's safety guarantees

## Setup

- Install Rust: https://rustup.rs/
- Verify: `rustc --version`
- Create folder: `lab28-rust/`
- Create file: `src/main.rs`

---

## Part 1: Hello, Rust! (15 minutes)

### Activity 1.1: First Program

```rust
// main.rs
fn main() {
    println!("Hello, Rust!");

    // Variables are immutable by default
    let x = 5;
    println!("x = {}", x);

    // Make mutable with 'mut'
    let mut y = 10;
    println!("y = {}", y);
    y = 20;
    println!("y is now {}", y);

    // Type annotations (usually inferred)
    let z: i32 = 42;
    let pi: f64 = 3.14159;
    let active: bool = true;
    let letter: char = 'R';

    println!("z={}, pi={}, active={}, letter={}", z, pi, active, letter);
}
```

Run with: `cargo run` or `rustc main.rs && ./main`

### Activity 1.2: Functions

```rust
fn main() {
    let result = add(3, 5);
    println!("3 + 5 = {}", result);

    let fact = factorial(5);
    println!("5! = {}", fact);

    greet("Alice");
}

// Function with return type
fn add(a: i32, b: i32) -> i32 {
    a + b  // No semicolon = return value
}

// Recursive function
fn factorial(n: u32) -> u32 {
    if n <= 1 {
        1
    } else {
        n * factorial(n - 1)
    }
}

// Function that doesn't return a value
fn greet(name: &str) {
    println!("Hello, {}!", name);
}
```

### Activity 1.3: Control Flow

```rust
fn main() {
    let number = 7;

    // if/else (expression, not statement!)
    let description = if number % 2 == 0 {
        "even"
    } else {
        "odd"
    };
    println!("{} is {}", number, description);

    // Loop
    let mut count = 0;
    loop {
        count += 1;
        if count == 5 {
            break;
        }
    }

    // While
    let mut n = 3;
    while n > 0 {
        println!("{}!", n);
        n -= 1;
    }
    println!("LIFTOFF!");

    // For loop
    for i in 1..=5 {
        println!("i = {}", i);
    }

    // For over array
    let arr = [10, 20, 30, 40, 50];
    for element in arr.iter() {
        println!("element: {}", element);
    }
}
```

### ✅ Checkpoint 1

Verify:
- [ ] Can compile and run Rust
- [ ] Understand let vs let mut
- [ ] Functions and control flow work

---

## Part 2: Ownership (The Key Concept) (25 minutes)

### Activity 2.1: What is Ownership?

Rust's core feature: **memory safety without garbage collection**

**Three rules of ownership:**
1. Each value has an owner
2. Only one owner at a time
3. Value is dropped when owner goes out of scope

```rust
fn main() {
    {
        let s = String::from("hello"); // s owns the string
        println!("{}", s);
    } // s goes out of scope, string is dropped (freed)

    // This won't compile - s is gone!
    // println!("{}", s);
}
```

### Activity 2.2: Move Semantics

```rust
fn main() {
    let s1 = String::from("hello");
    let s2 = s1;  // s1's value MOVES to s2

    // println!("{}", s1);  // ERROR! s1 no longer valid
    println!("{}", s2);  // OK

    // For Copy types (integers, etc.), copy happens instead
    let x = 5;
    let y = x;  // x is copied, both valid
    println!("x={}, y={}", x, y);  // OK
}
```

### Activity 2.3: Functions and Ownership

```rust
fn main() {
    let s = String::from("hello");

    takes_ownership(s);  // s moves into function

    // println!("{}", s);  // ERROR! s was moved

    let x = 5;
    makes_copy(x);  // x is copied
    println!("{}", x);  // OK, x is still valid

    // Getting values back
    let s1 = String::from("world");
    let s2 = takes_and_returns(s1);
    // s1 is gone, but s2 has the value
    println!("{}", s2);
}

fn takes_ownership(s: String) {
    println!("{}", s);
} // s is dropped here

fn makes_copy(x: i32) {
    println!("{}", x);
} // x goes out of scope, nothing special

fn takes_and_returns(s: String) -> String {
    s  // Return transfers ownership back
}
```

### Activity 2.4: Borrowing (References)

Instead of moving, you can **borrow**:

```rust
fn main() {
    let s = String::from("hello");

    // Pass a reference - borrow, don't move
    let len = calculate_length(&s);
    println!("'{}' has {} characters", s, len);  // s still valid!

    // Mutable borrowing
    let mut s2 = String::from("hello");
    change(&mut s2);
    println!("{}", s2);  // "hello, world"
}

fn calculate_length(s: &String) -> usize {
    s.len()
} // s goes out of scope, but doesn't own data, so nothing happens

fn change(s: &mut String) {
    s.push_str(", world");
}
```

### Activity 2.5: Borrowing Rules

```rust
fn main() {
    let mut s = String::from("hello");

    // Rule: Many immutable borrows OR one mutable borrow
    let r1 = &s;
    let r2 = &s;
    println!("{}, {}", r1, r2);  // OK

    // But NOT both:
    // let r3 = &mut s;  // ERROR! Can't borrow as mutable
                        // while immutable borrows exist

    // After r1, r2 are done being used, we can borrow mutably
    let r3 = &mut s;
    r3.push_str("!");
    println!("{}", r3);
}
```

### ✅ Checkpoint 2

Verify:
- [ ] Understand ownership and moves
- [ ] Can use references to borrow
- [ ] Know the difference between & and &mut

---

## Part 3: Common Patterns (20 minutes)

### Activity 3.1: Structs

```rust
// Define a struct
struct Person {
    name: String,
    age: u32,
}

// Methods using impl
impl Person {
    // Associated function (like static method)
    fn new(name: &str, age: u32) -> Person {
        Person {
            name: String::from(name),
            age,
        }
    }

    // Method (takes &self)
    fn greet(&self) {
        println!("Hello, I'm {} and I'm {} years old", self.name, self.age);
    }

    // Method that mutates (takes &mut self)
    fn have_birthday(&mut self) {
        self.age += 1;
        println!("Happy birthday! Now I'm {}", self.age);
    }
}

fn main() {
    let mut alice = Person::new("Alice", 30);
    alice.greet();
    alice.have_birthday();
}
```

### Activity 3.2: Enums and Pattern Matching

```rust
enum Message {
    Quit,
    Move { x: i32, y: i32 },
    Write(String),
    ChangeColor(i32, i32, i32),
}

fn process_message(msg: Message) {
    match msg {
        Message::Quit => {
            println!("Quitting!");
        }
        Message::Move { x, y } => {
            println!("Moving to ({}, {})", x, y);
        }
        Message::Write(text) => {
            println!("Writing: {}", text);
        }
        Message::ChangeColor(r, g, b) => {
            println!("Changing color to RGB({}, {}, {})", r, g, b);
        }
    }
}

fn main() {
    process_message(Message::Write(String::from("Hello")));
    process_message(Message::Move { x: 10, y: 20 });
    process_message(Message::Quit);
}
```

### Activity 3.3: Option and Result

```rust
// Option<T>: Some(T) or None
fn divide(a: f64, b: f64) -> Option<f64> {
    if b == 0.0 {
        None
    } else {
        Some(a / b)
    }
}

// Result<T, E>: Ok(T) or Err(E)
fn parse_number(s: &str) -> Result<i32, String> {
    match s.parse::<i32>() {
        Ok(n) => Ok(n),
        Err(_) => Err(format!("'{}' is not a valid number", s)),
    }
}

fn main() {
    // Using Option
    match divide(10.0, 2.0) {
        Some(result) => println!("10 / 2 = {}", result),
        None => println!("Cannot divide by zero"),
    }

    // Using if let for Option
    if let Some(x) = divide(10.0, 0.0) {
        println!("Result: {}", x);
    } else {
        println!("Division failed");
    }

    // Using Result with ? operator
    fn try_parse() -> Result<i32, String> {
        let n = parse_number("42")?;  // Propagates error if Err
        Ok(n * 2)
    }

    match try_parse() {
        Ok(n) => println!("Parsed and doubled: {}", n),
        Err(e) => println!("Error: {}", e),
    }
}
```

### ✅ Checkpoint 3

Verify:
- [ ] Can define and use structs
- [ ] Understand enums and pattern matching
- [ ] Know how Option and Result work

---

## Part 4: Collections and Iterators (20 minutes)

### Activity 4.1: Vectors

```rust
fn main() {
    // Create a vector
    let mut v: Vec<i32> = Vec::new();
    v.push(1);
    v.push(2);
    v.push(3);

    // Or use vec! macro
    let v2 = vec![1, 2, 3, 4, 5];

    // Access elements
    println!("First: {}", v2[0]);

    // Safe access with get
    match v2.get(10) {
        Some(value) => println!("Found: {}", value),
        None => println!("Index out of bounds"),
    }

    // Iterate
    for val in &v2 {
        println!("{}", val);
    }

    // Iterate and modify
    let mut v3 = vec![1, 2, 3];
    for val in &mut v3 {
        *val *= 2;  // Dereference and modify
    }
    println!("{:?}", v3);  // [2, 4, 6]
}
```

### Activity 4.2: HashMaps

```rust
use std::collections::HashMap;

fn main() {
    let mut scores = HashMap::new();

    scores.insert(String::from("Alice"), 100);
    scores.insert(String::from("Bob"), 85);

    // Access
    if let Some(score) = scores.get("Alice") {
        println!("Alice's score: {}", score);
    }

    // Update
    scores.insert(String::from("Alice"), 105);  // Overwrites

    // Insert only if key doesn't exist
    scores.entry(String::from("Charlie")).or_insert(90);

    // Iterate
    for (name, score) in &scores {
        println!("{}: {}", name, score);
    }
}
```

### Activity 4.3: Iterators (Functional Style)

```rust
fn main() {
    let v = vec![1, 2, 3, 4, 5];

    // Map
    let doubled: Vec<i32> = v.iter().map(|x| x * 2).collect();
    println!("{:?}", doubled);  // [2, 4, 6, 8, 10]

    // Filter
    let evens: Vec<&i32> = v.iter().filter(|x| *x % 2 == 0).collect();
    println!("{:?}", evens);  // [2, 4]

    // Fold (reduce)
    let sum: i32 = v.iter().fold(0, |acc, x| acc + x);
    println!("Sum: {}", sum);  // 15

    // Chaining
    let result: i32 = v.iter()
        .filter(|x| *x % 2 == 1)  // Odd numbers
        .map(|x| x * x)           // Square them
        .sum();                   // Sum
    println!("Sum of squares of odds: {}", result);  // 1 + 9 + 25 = 35

    // Find
    let first_even = v.iter().find(|x| *x % 2 == 0);
    println!("First even: {:?}", first_even);  // Some(2)
}
```

### ✅ Checkpoint 4

Verify:
- [ ] Can use vectors and hashmaps
- [ ] Understand iterator methods
- [ ] Can chain iterator operations

---

## Part 5: A Complete Example (10 minutes)

### Activity 5.1: Word Counter

```rust
use std::collections::HashMap;
use std::io::{self, BufRead};

fn main() {
    println!("Enter text (Ctrl+D to finish):");

    let mut word_counts: HashMap<String, u32> = HashMap::new();

    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let line = line.expect("Failed to read line");
        for word in line.split_whitespace() {
            let word = word.to_lowercase();
            let count = word_counts.entry(word).or_insert(0);
            *count += 1;
        }
    }

    // Sort by count (descending)
    let mut counts: Vec<(&String, &u32)> = word_counts.iter().collect();
    counts.sort_by(|a, b| b.1.cmp(a.1));

    println!("\nWord counts:");
    for (word, count) in counts.iter().take(10) {
        println!("{}: {}", word, count);
    }
}
```

---

## Challenges

### Challenge 1: Linked List

Implement a singly-linked list using Rust's ownership:

```rust
struct Node<T> {
    value: T,
    next: Option<Box<Node<T>>>,
}

struct LinkedList<T> {
    head: Option<Box<Node<T>>>,
}
```

### Challenge 2: File Processing

Read a CSV file and compute statistics using Result for error handling.

### Challenge 3: Concurrent Counter

Use Rust's Arc and Mutex to create a thread-safe counter.

---

## Wrap-Up

**Key takeaways:**

1. **Ownership** - Each value has one owner
2. **Borrowing** - References allow temporary access
3. **No null** - Use Option<T> instead
4. **No exceptions** - Use Result<T, E> instead
5. **Memory safe** - Compiler prevents common bugs

**Rust vs Other Languages:**

| Feature | Rust | C++ | Python |
|---------|------|-----|--------|
| Memory safety | Compile-time | Manual | GC |
| Null | Option<T> | nullptr | None |
| Errors | Result<T,E> | Exceptions | Exceptions |
| Speed | Fast | Fast | Slower |

**When to use Rust:**
- Systems programming
- Performance-critical code
- Web Assembly
- Command-line tools
- Anywhere safety matters

**Next lab:** Capstone Planning - design your final project!
