#!/usr/bin/env rust-script
/*
 * Lesson 4: Functions in Rust
 * Demonstrates function definition, closures, traits, and functional patterns
 */

// ============================================
// 1. Basic Function Definition
// ============================================

fn greet(name: &str) -> String {
    format!("Hello, {}!", name)
}

fn add(x: i32, y: i32) -> i32 {
    x + y  // Implicit return (no semicolon)
}

fn square(x: i32) -> i32 {
    x * x
}

fn multiply(x: i32, y: i32) -> i32 {
    return x * y;  // Explicit return (with semicolon)
}

// ============================================
// 2. Return Values
// ============================================

// Rust can only return one value
// Use tuples for multiple returns
fn divide(x: f64, y: f64) -> (Option<f64>, Option<String>) {
    if y == 0.0 {
        (None, Some("Cannot divide by zero".to_string()))
    } else {
        (Some(x / y), None)
    }
}

// Using Result type (idiomatic Rust)
fn divide_result(x: f64, y: f64) -> Result<f64, String> {
    if y == 0.0 {
        Err("Cannot divide by zero".to_string())
    } else {
        Ok(x / y)
    }
}

// ============================================
// 3. Generic Functions
// ============================================

fn max<T: PartialOrd>(a: T, b: T) -> T {
    if a > b { a } else { b }
}

// Generic function with multiple constraints
fn print_and_return<T: std::fmt::Display + Clone>(value: T) -> T {
    println!("Value: {}", value);
    value.clone()
}

// ============================================
// 4. Pure vs Impure Functions
// ============================================

// Pure function
fn pure_add(x: i32, y: i32) -> i32 {
    x + y
}

// Impure function (side effect - I/O)
fn impure_print(message: &str) {
    println!("{}", message);
}

// Impure function (depends on/modifies global state)
static mut TOTAL: i32 = 0;
unsafe fn impure_add_to_total(x: i32) -> i32 {
    TOTAL += x;
    TOTAL
}

// ============================================
// 5. Closures
// ============================================

// Function that returns a closure
fn make_multiplier(factor: i32) -> impl Fn(i32) -> i32 {
    move |x| x * factor  // 'move' captures ownership of factor
}

fn make_adder(n: i32) -> impl Fn(i32) -> i32 {
    move |x| x + n
}

// Mutable closure (stateful)
fn make_counter() -> impl FnMut() -> i32 {
    let mut count = 0;
    move || {
        count += 1;
        count
    }
}

// ============================================
// 6. First-Class Functions
// ============================================

fn apply_operation<F>(operation: F, x: i32, y: i32) -> i32
where
    F: Fn(i32, i32) -> i32,
{
    operation(x, y)
}

fn compose<F, G, A, B, C>(f: F, g: G) -> impl Fn(A) -> C
where
    F: Fn(B) -> C,
    G: Fn(A) -> B,
{
    move |x| f(g(x))
}

// ============================================
// 7. Higher-Order Functions
// ============================================

fn apply_twice<F>(f: F, x: i32) -> i32
where
    F: Fn(i32) -> i32,
{
    f(f(x))
}

fn apply_n_times<F>(f: F, mut x: i32, n: usize) -> i32
where
    F: Fn(i32) -> i32,
{
    for _ in 0..n {
        x = f(x);
    }
    x
}

// ============================================
// 8. Function Pointers
// ============================================

// Function pointer type (less flexible than closures)
fn apply_operation_ptr(operation: fn(i32, i32) -> i32, x: i32, y: i32) -> i32 {
    operation(x, y)
}

// ============================================
// Main Function
// ============================================

fn main() {
    println!("=== Rust Functions ===\n");

    // 1. Basic functions
    println!("1. Basic Functions:");
    println!("  greet('Alice'): {}", greet("Alice"));
    println!("  add(5, 3): {}", add(5, 3));
    println!("  square(7): {}", square(7));
    println!("  multiply(4, 5): {}", multiply(4, 5));

    // 2. Return values
    println!("\n2. Multiple Return Values:");
    let (result1, error1) = divide(10.0, 2.0);
    println!("  divide(10, 2): result={:?}, error={:?}", result1, error1);
    let (result2, error2) = divide(10.0, 0.0);
    println!("  divide(10, 0): result={:?}, error={:?}", result2, error2);

    println!("\n  Using Result type:");
    match divide_result(10.0, 2.0) {
        Ok(r) => println!("  divide_result(10, 2): {}", r),
        Err(e) => println!("  Error: {}", e),
    }
    match divide_result(10.0, 0.0) {
        Ok(r) => println!("  divide_result(10, 0): {}", r),
        Err(e) => println!("  Error: {}", e),
    }

    // 3. Generic functions
    println!("\n3. Generic Functions:");
    println!("  max(5, 3): {}", max(5, 3));
    println!("  max(5.5, 3.2): {}", max(5.5, 3.2));
    println!("  max('a', 'z'): {}", max('a', 'z'));

    // 4. Pure vs Impure
    println!("\n4. Pure vs Impure:");
    println!("  pure_add(5, 3): {}", pure_add(5, 3));
    print!("  impure_print('Hello'): ");
    impure_print("Hello");
    unsafe {
        TOTAL = 0;  // Reset
        println!("  impure_add_to_total(5): {}", impure_add_to_total(5));
        println!("  impure_add_to_total(5): {} (different!)", impure_add_to_total(5));
    }

    // 5. Closures
    println!("\n5. Closures:");
    let double = |x: i32| x * 2;
    let square_closure = |x: i32| x * x;

    println!("  double(5): {}", double(5));
    println!("  square_closure(7): {}", square_closure(7));

    let times_two = make_multiplier(2);
    let times_three = make_multiplier(3);
    println!("  times_two(5): {}", times_two(5));
    println!("  times_three(5): {}", times_three(5));

    let add_ten = make_adder(10);
    println!("  add_ten(5): {}", add_ten(5));

    // 6. Mutable closures (stateful)
    println!("\n  Counter (stateful closure):");
    let mut counter1 = make_counter();
    let mut counter2 = make_counter();
    println!("  counter1(): {}", counter1());  // 1
    println!("  counter1(): {}", counter1());  // 2
    println!("  counter2(): {}", counter2());  // 1
    println!("  counter1(): {}", counter1());  // 3

    // 7. First-class functions
    println!("\n6. First-Class Functions:");
    println!("  apply_operation(add, 5, 3): {}", apply_operation(add, 5, 3));
    println!("  apply_operation(|x,y| x*y, 5, 3): {}",
             apply_operation(|x, y| x * y, 5, 3));

    // 8. Function composition
    println!("\n7. Function Composition:");
    let increment = |x: i32| x + 1;
    let square_then_increment = compose(increment, square_closure);
    println!("  square_then_increment(5): {}", square_then_increment(5));

    // 9. Higher-order functions
    println!("\n8. Higher-Order Functions:");
    println!("  apply_twice(double, 5): {}", apply_twice(double, 5));
    println!("  apply_n_times(double, 5, 3): {}", apply_n_times(double, 5, 3));

    // 10. Iterator methods (functional style)
    println!("\n9. Iterator Methods (functional style):");
    let numbers: Vec<i32> = vec![1, 2, 3, 4, 5];
    println!("  numbers: {:?}", numbers);

    let squared: Vec<i32> = numbers.iter().map(|x| x * x).collect();
    println!("  map(square): {:?}", squared);

    let evens: Vec<i32> = numbers.iter().filter(|x| *x % 2 == 0).copied().collect();
    println!("  filter(is_even): {:?}", evens);

    let sum: i32 = numbers.iter().sum();
    println!("  sum(): {}", sum);

    let product: i32 = numbers.iter().product();
    println!("  product(): {}", product);

    // Chaining operations
    let result: i32 = numbers.iter()
        .filter(|x| *x % 2 == 0)
        .map(|x| x * x)
        .sum();
    println!("  filter(even).map(square).sum(): {}", result);

    // 11. Function pointers
    println!("\n10. Function Pointers:");
    println!("  apply_operation_ptr(add, 5, 3): {}", apply_operation_ptr(add, 5, 3));
    println!("  apply_operation_ptr(multiply, 5, 3): {}",
             apply_operation_ptr(multiply, 5, 3));

    // 12. Ownership and borrowing in closures
    println!("\n11. Ownership in Closures:");
    let owned_value = String::from("Hello");

    // Closure that borrows
    let borrow_closure = || println!("  Borrowed: {}", owned_value);
    borrow_closure();
    println!("  Still can use owned_value: {}", owned_value);

    // Closure that moves
    let move_closure = move || println!("  Moved: {}", owned_value);
    move_closure();
    // owned_value is no longer available here
    // println!("{}", owned_value);  // This would error!

    println!("\n12. Key Rust Features:");
    println!("  - Zero-cost abstractions: closures are as fast as direct code");
    println!("  - Ownership system: prevents data races at compile time");
    println!("  - Functional patterns with safety guarantees");
}
