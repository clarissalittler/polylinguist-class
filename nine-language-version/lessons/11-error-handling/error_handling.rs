// Lesson 11: Error Handling & Debugging in Rust
//
// Rust features:
// - Result<T, E> for recoverable errors
// - Option<T> for nullable values
// - panic! for unrecoverable errors
// - ? operator for error propagation
// - Custom error types
// - No exceptions!
//
// This demonstrates Rust's error handling.

use std::fs::File;
use std::io::{self, Read};
use std::num::ParseIntError;

// ====================
// 1. Result type
// ====================

fn divide(x: f64, y: f64) -> Result<f64, String> {
    if y == 0.0 {
        Err("Cannot divide by zero".to_string())
    } else {
        Ok(x / y)
    }
}

// ====================
// 2. Option type
// ====================

fn safe_get<T: Clone>(vec: &[T], index: usize) -> Option<T> {
    if index < vec.len() {
        Some(vec[index].clone())
    } else {
        None
    }
}

// ====================
// 3. ? operator (error propagation)
// ====================

fn compute(x: f64, y: f64) -> Result<f64, String> {
    let result1 = divide(x, y)?;  // Returns early if Err
    let result2 = divide(result1, 2.0)?;
    Ok(result2)
}

// ====================
// 4. Custom error types
// ====================

#[derive(Debug)]
enum ValidationError {
    InvalidEmail(String),
    InvalidPassword(String),
}

impl std::fmt::Display for ValidationError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ValidationError::InvalidEmail(msg) => write!(f, "Invalid email: {}", msg),
            ValidationError::InvalidPassword(msg) => write!(f, "Invalid password: {}", msg),
        }
    }
}

impl std::error::Error for ValidationError {}

fn validate_email(email: &str) -> Result<(), ValidationError> {
    if !email.contains('@') {
        return Err(ValidationError::InvalidEmail("missing @ symbol".to_string()));
    }
    if !email.split('@').nth(1).unwrap_or("").contains('.') {
        return Err(ValidationError::InvalidEmail("missing domain extension".to_string()));
    }
    Ok(())
}

// ====================
// 5. Pattern matching on Result
// ====================

fn handle_result() {
    match divide(10.0, 2.0) {
        Ok(result) => println!("   Success: {}", result),
        Err(e) => println!("   Error: {}", e),
    }

    match divide(10.0, 0.0) {
        Ok(result) => println!("   Success: {}", result),
        Err(e) => println!("   Error: {}", e),
    }
}

// ====================
// 6. unwrap and expect
// ====================

fn demonstrate_unwrap() {
    // unwrap: panics if Err
    let result = divide(10.0, 2.0).unwrap();
    println!("   unwrap result: {}", result);

    // expect: panic with custom message
    let result = divide(10.0, 5.0).expect("Division should work");
    println!("   expect result: {}", result);

    // This would panic:
    // divide(10.0, 0.0).unwrap();  // thread 'main' panicked
}

// ====================
// 7. unwrap_or and unwrap_or_else
// ====================

fn demonstrate_unwrap_or() {
    let result1 = divide(10.0, 2.0).unwrap_or(0.0);
    println!("   unwrap_or (success): {}", result1);

    let result2 = divide(10.0, 0.0).unwrap_or(0.0);
    println!("   unwrap_or (failure): {}", result2);

    let result3 = divide(10.0, 0.0).unwrap_or_else(|e| {
        println!("   Error occurred: {}", e);
        -1.0
    });
    println!("   unwrap_or_else: {}", result3);
}

// ====================
// 8. Option combinators
// ====================

fn demonstrate_option() {
    let numbers = vec![1, 2, 3, 4, 5];

    let item = safe_get(&numbers, 2);
    println!("   numbers[2] = {:?}", item);

    let item = safe_get(&numbers, 10);
    println!("   numbers[10] = {:?}", item);

    // map
    let doubled = safe_get(&numbers, 2).map(|x| x * 2);
    println!("   doubled = {:?}", doubled);

    // unwrap_or
    let value = safe_get(&numbers, 10).unwrap_or(0);
    println!("   with default = {}", value);
}

// ====================
// 9. Error recovery
// ====================

fn robust_parse(items: &[&str]) -> Vec<i32> {
    let mut results = Vec::new();
    let mut errors = 0;

    for item in items {
        match item.parse::<i32>() {
            Ok(num) => results.push(num),
            Err(_) => errors += 1,
        }
    }

    println!("   Parsed {}/{} items", results.len(), items.len());
    println!("   Results: {:?}", results);
    if errors > 0 {
        println!("   Errors: {}", errors);
    }

    results
}

// ====================
// 10. panic! for unrecoverable errors
// ====================

fn demonstrate_panic() {
    println!("   Before panic (this prints)");

    // Uncomment to see panic:
    // panic!("This is an unrecoverable error!");

    println!("   After panic (never reached if panic occurs)");
}

// ====================
// 11. File I/O with Result
// ====================

fn read_file_safe(filename: &str) -> Result<String, io::Error> {
    let mut file = File::open(filename)?;  // ? propagates error
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}

fn demonstrate_file_io() {
    match read_file_safe("/tmp/test.txt") {
        Ok(contents) => println!("   Read {} bytes", contents.len()),
        Err(e) => println!("   Error: {}", e),
    }

    match read_file_safe("/nonexistent.txt") {
        Ok(contents) => println!("   Read {} bytes", contents.len()),
        Err(e) => println!("   Error: {}", e),
    }
}

// ====================
// Main demonstration
// ====================

fn main() {
    println!("=== Error Handling in Rust ===\n");

    // 1. Basic Result
    println!("1. Result Type:");
    println!("   divide(10, 2) = {:?}", divide(10.0, 2.0));
    println!("   divide(10, 0) = {:?}", divide(10.0, 0.0));

    // 2. Option type
    println!("\n2. Option Type:");
    let numbers = vec![1, 2, 3, 4, 5];
    println!("   numbers[2] = {:?}", safe_get(&numbers, 2));
    println!("   numbers[10] = {:?}", safe_get(&numbers, 10));

    // 3. ? operator
    println!("\n3. ? Operator (Error Propagation):");
    println!("   compute(10, 2) = {:?}", compute(10.0, 2.0));
    println!("   compute(10, 0) = {:?}", compute(10.0, 0.0));

    // 4. Custom errors
    println!("\n4. Custom Error Types:");
    match validate_email("user@example.com") {
        Ok(()) => println!("   Valid email"),
        Err(e) => println!("   Error: {}", e),
    }
    match validate_email("invalid-email") {
        Ok(()) => println!("   Valid email"),
        Err(e) => println!("   Error: {}", e),
    }

    // 5. Pattern matching
    println!("\n5. Pattern Matching on Result:");
    handle_result();

    // 6. unwrap/expect
    println!("\n6. unwrap and expect:");
    demonstrate_unwrap();

    // 7. unwrap_or
    println!("\n7. unwrap_or and unwrap_or_else:");
    demonstrate_unwrap_or();

    // 8. Option combinators
    println!("\n8. Option Combinators:");
    demonstrate_option();

    // 9. Error recovery
    println!("\n9. Error Recovery:");
    robust_parse(&["1", "2", "bad", "4", "invalid", "6"]);

    // 10. panic
    println!("\n10. panic! (Unrecoverable Errors):");
    demonstrate_panic();

    // 11. File I/O
    println!("\n11. File I/O with Result:");
    // Create test file
    std::fs::write("/tmp/test.txt", "Hello, Rust!").ok();
    demonstrate_file_io();

    println!("\n=== Rust Error Handling Features ===");
    println!("- Result<T, E> for recoverable errors");
    println!("- Option<T> for nullable values");
    println!("- ? operator for error propagation");
    println!("- panic! for unrecoverable errors");
    println!("- No exceptions - errors are values");
    println!("- Pattern matching on errors");
    println!("- Rich combinator library");
    println!("- Compile-time error checking");
}
