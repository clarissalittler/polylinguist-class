// Lesson 10: Type Systems in Rust
//
// Rust has a sophisticated type system featuring:
// - Strong static typing
// - Local type inference
// - Algebraic data types (enums and structs)
// - Generics (parametric polymorphism)
// - Traits (like type classes/interfaces)
// - Ownership types (unique to Rust!)
// - Lifetime parameters
// - Zero-cost abstractions
//
// This demonstrates Rust's type system features.

use std::fmt::Display;

// ====================
// 1. Basic Types
// ====================

fn increment(x: i32) -> i32 {
    x + 1
}

fn add(x: i32, y: i32) -> i32 {
    x + y
}

// Type inference for local variables
fn demonstrate_inference() {
    let x = 5;          // Inferred: i32
    let y = 3.14;       // Inferred: f64
    let s = "hello";    // Inferred: &str
    let v = vec![1, 2, 3];  // Inferred: Vec<i32>

    println!("   Inferred types: x={}, y={}, s={}, v={:?}", x, y, s, v);
}

// ====================
// 2. Algebraic Data Types
// ====================

// Enum (sum type)
#[derive(Debug)]
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
}

// Struct (product type)
#[derive(Debug, Clone, Copy)]
struct Point {
    x: f64,
    y: f64,
}

impl Point {
    fn new(x: f64, y: f64) -> Self {
        Point { x, y }
    }

    fn distance(&self, other: &Point) -> f64 {
        ((self.x - other.x).powi(2) + (self.y - other.y).powi(2)).sqrt()
    }
}

// ====================
// 3. Generic Types (Parametric Polymorphism)
// ====================

// Generic function
fn identity<T>(x: T) -> T {
    x
}

// Generic struct
#[derive(Debug)]
struct Pair<A, B> {
    first: A,
    second: B,
}

impl<A, B> Pair<A, B> {
    fn new(first: A, second: B) -> Self {
        Pair { first, second }
    }

    fn swap(self) -> Pair<B, A> {
        Pair {
            first: self.second,
            second: self.first,
        }
    }
}

// ====================
// 4. Option Type (Null Safety)
// ====================

fn safe_divide(x: f64, y: f64) -> Option<f64> {
    if y == 0.0 {
        None
    } else {
        Some(x / y)
    }
}

fn safe_head<T: Clone>(items: &[T]) -> Option<T> {
    items.first().cloned()
}

// ====================
// 5. Result Type (Error Handling)
// ====================

#[derive(Debug)]
struct DivisionError(String);

fn divide(x: f64, y: f64) -> Result<f64, DivisionError> {
    if y == 0.0 {
        Err(DivisionError("Division by zero".to_string()))
    } else {
        Ok(x / y)
    }
}

fn parse_int(s: &str) -> Result<i32, std::num::ParseIntError> {
    s.parse::<i32>()
}

// ====================
// 6. Traits (Type Classes)
// ====================

// Define a trait
trait Describable {
    fn describe(&self) -> String;
}

// Implement for Shape
impl Describable for Shape {
    fn describe(&self) -> String {
        match self {
            Shape::Circle { radius } => format!("Circle with radius {}", radius),
            Shape::Rectangle { width, height } => format!("Rectangle {}x{}", width, height),
            Shape::Triangle { a, b, c } => format!("Triangle with sides {}, {}, {}", a, b, c),
        }
    }
}

// Implement for Point
impl Describable for Point {
    fn describe(&self) -> String {
        format!("Point at ({}, {})", self.x, self.y)
    }
}

// Function using trait bound
fn print_description<T: Describable>(x: &T) {
    println!("   {}", x.describe());
}

// ====================
// 7. Multiple Trait Bounds
// ====================

// Requires both Debug and Display
fn print_if_equal<T: PartialEq + Display>(x: &T, y: &T) -> String {
    if x == y {
        format!("Equal: {}", x)
    } else {
        format!("Not equal: {} != {}", x, y)
    }
}

// ====================
// 8. Custom Trait with Methods
// ====================

trait Measurable {
    fn measure(&self) -> f64;
}

impl Measurable for Shape {
    fn measure(&self) -> f64 {
        self.area()
    }
}

impl Measurable for Point {
    fn measure(&self) -> f64 {
        (self.x * self.x + self.y * self.y).sqrt()  // Distance from origin
    }
}

fn compare_measure<T: Measurable>(x: &T, y: &T) -> std::cmp::Ordering {
    x.measure().partial_cmp(&y.measure()).unwrap()
}

// ====================
// 9. Generic Enums
// ====================

// Custom Result type
enum MyResult<T, E> {
    Ok(T),
    Err(E),
}

// Custom Option type
enum MyOption<T> {
    Some(T),
    None,
}

// ====================
// 10. Ownership Types (Unique to Rust!)
// ====================

// Takes ownership
fn take_ownership(s: String) {
    println!("   Took ownership of: {}", s);
    // s is dropped here
}

// Borrows immutably
fn borrow_immutable(s: &String) {
    println!("   Borrowed immutably: {}", s);
}

// Borrows mutably
fn borrow_mutable(s: &mut String) {
    s.push_str(" (modified)");
    println!("   Modified: {}", s);
}

// ====================
// 11. Lifetime Parameters
// ====================

// Explicit lifetime annotations
fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
    if x.len() > y.len() {
        x
    } else {
        y
    }
}

// Struct with lifetime
struct Reference<'a> {
    value: &'a str,
}

// ====================
// 12. Newtype Pattern
// ====================

#[derive(Debug, PartialEq, Eq)]
struct UserId(u32);

#[derive(Debug, PartialEq, Eq)]
struct ProductId(u32);

fn get_user_name(id: UserId) -> String {
    format!("User #{}", id.0)
}

// This would be a compile error:
// get_user_name(ProductId(123));  // ERROR: wrong type!

// ====================
// 13. Phantom Types
// ====================

use std::marker::PhantomData;

struct State<S, T> {
    value: T,
    _state: PhantomData<S>,
}

struct Locked;
struct Unlocked;

impl<T> State<Locked, T> {
    fn unlock(self) -> State<Unlocked, T> {
        State {
            value: self.value,
            _state: PhantomData,
        }
    }
}

impl<T> State<Unlocked, T> {
    fn lock(self) -> State<Locked, T> {
        State {
            value: self.value,
            _state: PhantomData,
        }
    }
}

// ====================
// 14. Associated Types
// ====================

trait Container {
    type Item;

    fn get(&self, index: usize) -> Option<&Self::Item>;
    fn len(&self) -> usize;
}

impl<T> Container for Vec<T> {
    type Item = T;

    fn get(&self, index: usize) -> Option<&Self::Item> {
        self.get(index)
    }

    fn len(&self) -> usize {
        self.len()
    }
}

// ====================
// 15. Trait Objects (Dynamic Dispatch)
// ====================

fn describe_all(items: &[&dyn Describable]) {
    for item in items {
        println!("   {}", item.describe());
    }
}

// ====================
// 16. Const Generics
// ====================

// Fixed-size array with length in type
fn sum_array<const N: usize>(arr: [i32; N]) -> i32 {
    arr.iter().sum()
}

// ====================
// 17. where Clauses for Complex Bounds
// ====================

fn complex_bounds<T, U>(x: T, y: U) -> String
where
    T: Display + PartialOrd,
    U: Display + PartialOrd,
{
    format!("{} and {}", x, y)
}

// ====================
// 18. Derivable Traits
// ====================

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct Person {
    name: String,
    age: u32,
}

// ====================
// 19. Type Aliases
// ====================

type Result<T> = std::result::Result<T, String>;
type Point2D = (f64, f64);
type UsersMap = std::collections::HashMap<UserId, String>;

// ====================
// Main Demonstration
// ====================

fn main() {
    println!("=== Type Systems in Rust ===\n");

    // 1. Basic types
    println!("1. Basic Types:");
    println!("   increment(5) = {}", increment(5));
    println!("   add(3, 7) = {}", add(3, 7));

    // 2. Type inference
    println!("\n2. Type Inference:");
    demonstrate_inference();

    // 3. Algebraic data types
    println!("\n3. Algebraic Data Types:");
    let shapes = vec![
        Shape::Circle { radius: 5.0 },
        Shape::Rectangle { width: 4.0, height: 6.0 },
        Shape::Triangle { a: 3.0, b: 4.0, c: 5.0 },
    ];
    for shape in &shapes {
        println!("   {:?}, area = {:.2}", shape, shape.area());
    }

    // 4. Generics
    println!("\n4. Parametric Polymorphism (Generics):");
    println!("   identity(42) = {}", identity(42));
    println!("   identity(\"hello\") = {}", identity("hello"));
    let pair = Pair::new(1, "one");
    println!("   Pair: {:?}", pair);
    println!("   Swapped: {:?}", pair.swap());

    // 5. Option type
    println!("\n5. Option Type (Null Safety):");
    println!("   safe_divide(10.0, 2.0) = {:?}", safe_divide(10.0, 2.0));
    println!("   safe_divide(10.0, 0.0) = {:?}", safe_divide(10.0, 0.0));
    println!("   safe_head([1,2,3]) = {:?}", safe_head(&[1, 2, 3]));
    println!("   safe_head([]) = {:?}", safe_head(&[] as &[i32]));

    // 6. Result type
    println!("\n6. Result Type (Error Handling):");
    println!("   divide(10.0, 2.0) = {:?}", divide(10.0, 2.0));
    println!("   divide(10.0, 0.0) = {:?}", divide(10.0, 0.0));
    println!("   parse_int(\"42\") = {:?}", parse_int("42"));
    println!("   parse_int(\"abc\") = {:?}", parse_int("abc"));

    // 7. Traits
    println!("\n7. Traits:");
    print_description(&Shape::Circle { radius: 5.0 });
    print_description(&Point::new(3.0, 4.0));

    // 8. Custom traits
    println!("\n8. Custom Traits:");
    let circle = Shape::Circle { radius: 5.0 };
    let point = Point::new(3.0, 4.0);
    println!("   measure(circle) = {:.2}", circle.measure());
    println!("   measure(point) = {:.2}", point.measure());

    // 9. Ownership
    println!("\n9. Ownership Types:");
    let s1 = String::from("owned");
    borrow_immutable(&s1);
    let mut s2 = String::from("mutable");
    borrow_mutable(&mut s2);
    take_ownership(s1);  // s1 moved, can't use anymore
    // println!("{}", s1);  // ERROR: s1 was moved

    // 10. Lifetimes
    println!("\n10. Lifetime Parameters:");
    let s1 = "short";
    let s2 = "longer string";
    println!("   longest(\"{}\", \"{}\") = {}", s1, s2, longest(s1, s2));

    // 11. Newtype
    println!("\n11. Newtype Pattern:");
    let user_id = UserId(123);
    println!("   get_user_name({:?}) = {}", user_id, get_user_name(user_id));

    // 12. Phantom types
    println!("\n12. Phantom Types (State Tracking):");
    let locked = State::<Locked, &str> { value: "door", _state: PhantomData };
    let unlocked = locked.unlock();
    let _relocked = unlocked.lock();
    println!("   State transitions tracked at compile time!");

    // 13. Const generics
    println!("\n13. Const Generics:");
    println!("   sum_array([1,2,3,4,5]) = {}", sum_array([1, 2, 3, 4, 5]));

    // 14. Derivable traits
    println!("\n14. Derivable Traits:");
    let p1 = Person { name: "Alice".to_string(), age: 30 };
    let p2 = Person { name: "Bob".to_string(), age: 25 };
    println!("   {:?}", p1);
    println!("   p1 == p2: {}", p1 == p2);
    println!("   p1 < p2: {}", p1 < p2);

    println!("\n=== Rust Type System Features ===");
    println!("- Strong static typing with inference");
    println!("- Algebraic data types (enums and structs)");
    println!("- Generics with trait bounds");
    println!("- Traits for polymorphism");
    println!("- Ownership and borrowing (unique!)");
    println!("- Lifetime parameters");
    println!("- Zero-cost abstractions");
    println!("- No null - use Option instead");
    println!("- No exceptions - use Result instead");
    println!("- Memory safety without garbage collection!");
}
