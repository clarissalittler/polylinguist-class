# Rust Module Exercises

## Warmup Exercises

### Exercise RS.W1: Basic Syntax
```rust
// 1. Write a function that returns the larger of two i32 values
fn max(a: i32, b: i32) -> i32 { todo!() }

// 2. Write a function that returns true if n is even
fn is_even(n: i32) -> bool { todo!() }

// 3. Write a function that returns the factorial of n
fn factorial(n: u64) -> u64 { todo!() }
```

### Exercise RS.W2: Strings
```rust
// 1. Return a greeting string
fn greet(name: &str) -> String { todo!() }
// greet("Alice") -> "Hello, Alice!"

// 2. Count vowels in a string
fn count_vowels(s: &str) -> usize { todo!() }

// 3. Reverse a string
fn reverse(s: &str) -> String { todo!() }
```

### Exercise RS.W3: Vectors
```rust
// 1. Return the sum of a vector
fn sum(v: &Vec<i32>) -> i32 { todo!() }

// 2. Return a vector of squares
fn squares(n: i32) -> Vec<i32> { todo!() }
// squares(5) -> [1, 4, 9, 16, 25]

// 3. Filter to only even numbers
fn evens(v: &Vec<i32>) -> Vec<i32> { todo!() }
```

---

## Standard Exercises

### Exercise RS.S1: Ownership
Fix these programs so they compile:

```rust
// Program 1: Fix the ownership error
fn main() {
    let s = String::from("hello");
    let len = get_length(s);
    println!("Length of '{}' is {}", s, len);
}

fn get_length(s: String) -> usize {
    s.len()
}
```

```rust
// Program 2: Fix the borrowing error
fn main() {
    let mut s = String::from("hello");
    let r1 = &s;
    let r2 = &s;
    let r3 = &mut s;
    println!("{}, {}, {}", r1, r2, r3);
}
```

```rust
// Program 3: Fix the lifetime error
fn longest(x: &str, y: &str) -> &str {
    if x.len() > y.len() { x } else { y }
}
```

### Exercise RS.S2: Option and Result
```rust
// 1. Safe division that returns None on divide by zero
fn safe_div(a: f64, b: f64) -> Option<f64> { todo!() }

// 2. Parse a string to i32, returning Result
fn parse_int(s: &str) -> Result<i32, String> { todo!() }

// 3. Get element at index, returning Option
fn get_at<T: Clone>(v: &Vec<T>, index: usize) -> Option<T> { todo!() }
```

### Exercise RS.S3: Structs and Methods
```rust
struct Rectangle {
    width: f64,
    height: f64,
}

impl Rectangle {
    // Constructor
    fn new(width: f64, height: f64) -> Rectangle { todo!() }

    // Return the area
    fn area(&self) -> f64 { todo!() }

    // Return the perimeter
    fn perimeter(&self) -> f64 { todo!() }

    // Return true if this is a square
    fn is_square(&self) -> bool { todo!() }

    // Scale the rectangle by a factor
    fn scale(&mut self, factor: f64) { todo!() }

    // Create a square (associated function)
    fn square(size: f64) -> Rectangle { todo!() }
}
```

### Exercise RS.S4: Enums and Pattern Matching
```rust
enum Shape {
    Circle(f64),           // radius
    Rectangle(f64, f64),   // width, height
    Triangle(f64, f64, f64), // three sides
}

impl Shape {
    // Calculate area
    fn area(&self) -> f64 { todo!() }

    // Calculate perimeter
    fn perimeter(&self) -> f64 { todo!() }

    // Return a description string
    fn describe(&self) -> String { todo!() }
}
```

### Exercise RS.S5: Error Handling
```rust
use std::fs::File;
use std::io::{self, Read};

// Read file contents, propagating errors with ?
fn read_file(path: &str) -> Result<String, io::Error> {
    todo!()
}

// Count words in file, returning Result
fn count_words_in_file(path: &str) -> Result<usize, io::Error> {
    todo!()
}

// Parse config file in format "key=value" per line
// Return HashMap or error
fn parse_config(path: &str) -> Result<std::collections::HashMap<String, String>, io::Error> {
    todo!()
}
```

---

## Advanced Exercises

### Exercise RS.A1: Generic Functions
```rust
// 1. Find minimum in a slice (generic)
fn min<T: Ord>(slice: &[T]) -> Option<&T> { todo!() }

// 2. Find element satisfying predicate
fn find<T, F>(slice: &[T], predicate: F) -> Option<&T>
where
    F: Fn(&T) -> bool
{ todo!() }

// 3. Map a function over a vector
fn my_map<T, U, F>(v: Vec<T>, f: F) -> Vec<U>
where
    F: Fn(T) -> U
{ todo!() }
```

### Exercise RS.A2: Traits
```rust
// Define a trait for things that can be displayed as a summary
trait Summary {
    fn summarize(&self) -> String;

    // Default implementation
    fn summarize_short(&self) -> String {
        format!("(Read more...)")
    }
}

// Implement for these types:
struct NewsArticle {
    headline: String,
    author: String,
    content: String,
}

struct Tweet {
    username: String,
    content: String,
    likes: u32,
}

// Implement Summary for both types
```

### Exercise RS.A3: Iterators
```rust
// 1. Sum of squares using iterators
fn sum_of_squares(n: u32) -> u32 {
    // Use (1..=n).map(...).sum()
    todo!()
}

// 2. Find words longer than n characters
fn long_words(text: &str, n: usize) -> Vec<String> {
    // Use .split_whitespace().filter(...).map(...).collect()
    todo!()
}

// 3. Create an iterator that generates Fibonacci numbers
struct Fibonacci {
    curr: u64,
    next: u64,
}

impl Iterator for Fibonacci {
    type Item = u64;

    fn next(&mut self) -> Option<Self::Item> {
        todo!()
    }
}

fn fibonacci() -> Fibonacci {
    Fibonacci { curr: 0, next: 1 }
}

// Usage: fibonacci().take(10).collect::<Vec<_>>()
```

### Exercise RS.A4: Lifetimes
```rust
// 1. Return a reference to the longer string
fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
    todo!()
}

// 2. A struct that holds a reference
struct Excerpt<'a> {
    text: &'a str,
}

impl<'a> Excerpt<'a> {
    fn new(text: &'a str) -> Excerpt<'a> {
        todo!()
    }

    fn words(&self) -> Vec<&str> {
        todo!()
    }
}

// 3. Explain why this doesn't compile and fix it:
// fn invalid_reference() -> &str {
//     let s = String::from("hello");
//     &s
// }
```

### Exercise RS.A5: Smart Pointers
```rust
use std::rc::Rc;
use std::cell::RefCell;

// 1. Create a reference-counted string
fn shared_string() {
    let s = Rc::new(String::from("hello"));
    // Create two more references
    // Print the reference count
    todo!()
}

// 2. Interior mutability with RefCell
fn mutable_in_immutable() {
    let data = Rc::new(RefCell::new(5));
    // Modify the data through multiple Rc references
    todo!()
}

// 3. Implement a simple tree with shared ownership
#[derive(Debug)]
struct TreeNode {
    value: i32,
    children: Vec<Rc<TreeNode>>,
}
```

---

## Challenge Exercises

### Exercise RS.C1: Linked List
Implement a singly linked list:
```rust
type Link<T> = Option<Box<Node<T>>>;

struct Node<T> {
    data: T,
    next: Link<T>,
}

pub struct List<T> {
    head: Link<T>,
}

impl<T> List<T> {
    pub fn new() -> Self { todo!() }
    pub fn push(&mut self, data: T) { todo!() }
    pub fn pop(&mut self) -> Option<T> { todo!() }
    pub fn peek(&self) -> Option<&T> { todo!() }
}

// Implement Iterator for List
```

### Exercise RS.C2: Thread Safety
```rust
use std::thread;
use std::sync::{Arc, Mutex};

// 1. Spawn 10 threads that each increment a shared counter
fn parallel_counter() -> i32 {
    let counter = Arc::new(Mutex::new(0));
    let mut handles = vec![];

    for _ in 0..10 {
        // Clone Arc, spawn thread, increment counter
        todo!()
    }

    // Wait for all threads
    for handle in handles {
        handle.join().unwrap();
    }

    *counter.lock().unwrap()
}

// 2. Parallel sum of a vector
fn parallel_sum(v: Vec<i32>) -> i32 {
    // Split vector, sum in parallel
    todo!()
}
```

### Exercise RS.C3: Build a Command-Line Tool
Build a simple grep-like tool:
```rust
use std::env;
use std::fs;

// Usage: minigrep <pattern> <filename>
// Print all lines containing pattern

fn main() {
    let args: Vec<String> = env::args().collect();
    // Parse args, read file, search and print
    todo!()
}

fn search(pattern: &str, contents: &str) -> Vec<String> {
    todo!()
}
```

---

## Debugging Exercises

### Exercise RS.D1: Fix the Borrow Checker Errors
```rust
// Fix these so they compile:

// 1.
fn modify_and_print(v: &mut Vec<i32>) {
    for i in v.iter() {
        v.push(*i * 2);
    }
}

// 2.
fn string_slice() {
    let mut s = String::from("hello");
    let slice = &s[0..2];
    s.clear();
    println!("{}", slice);
}

// 3.
fn return_reference() -> &String {
    let s = String::from("hello");
    &s
}
```

---

## Reflection Questions

1. How does Rust's ownership system compare to C's manual memory management?
2. What bugs does the borrow checker prevent?
3. When would you choose Rust over C++ for a project?
4. How does `Option<T>` compare to nullable pointers in other languages?
5. What was most surprising about Rust's approach to safety?
