#!/usr/bin/env rust-script
/*
 * Lesson 3: Control Flow in Rust
 * Demonstrates conditionals, loops, pattern matching, and expressions
 */

fn main() {
    println!("=== Rust Control Flow ===\n");

    // 1. Basic conditionals (if is an expression)
    println!("1. Conditionals (if expressions):");
    let age = 20;

    // If as statement
    if age >= 18 {
        println!("  Age {}: Adult", age);
    } else if age >= 13 {
        println!("  Age {}: Teenager", age);
    } else {
        println!("  Age {}: Child", age);
    }

    // If as expression
    let status = if age >= 18 { "Adult" } else { "Minor" };
    println!("  Status: {}", status);

    // 2. For loops
    println!("\n2. For Loops:");
    println!("  Count to 5:");
    print!("   ");
    for i in 0..5 {
        print!(" {}", i);
    }
    println!();

    println!("  Iterate array:");
    let fruits = vec!["apple", "banana", "cherry"];
    for fruit in &fruits {
        println!("    {}", fruit);
    }

    println!("  With enumerate:");
    for (i, fruit) in fruits.iter().enumerate() {
        println!("    {}: {}", i, fruit);
    }

    // 3. While loop
    println!("\n3. While Loop:");
    let mut count = 0;
    while count < 3 {
        println!("  Count: {}", count);
        count += 1;
    }

    // 4. Loop (infinite loop with break)
    println!("\n4. Loop (with break):");
    let mut i = 0;
    loop {
        println!("  Iteration: {}", i);
        i += 1;
        if i >= 3 {
            break;
        }
    }

    // Loop as expression (returns value)
    let result = loop {
        i += 1;
        if i >= 10 {
            break i * 2;  // Return value
        }
    };
    println!("  Loop returned: {}", result);

    // 5. Boolean logic
    println!("\n5. Boolean Logic:");
    let x = 5;
    let y = 10;
    println!("  x={}, y={}", x, y);
    println!("  x > 3 && y < 20: {}", x > 3 && y < 20);
    println!("  x > 10 || y > 5: {}", x > 10 || y > 5);
    println!("  !(x == y): {}", !(x == y));

    println!("\n  Only bool type (no truthiness):");
    println!("    Must use explicit boolean expressions");

    // 6. FizzBuzz
    println!("\n6. FizzBuzz (1-20):");
    print!(" ");
    for i in 1..=20 {
        print!(" {}", fizzbuzz(i));
    }
    println!();

    // 7. Match expressions (pattern matching)
    println!("\n7. Match Expressions (pattern matching):");
    println!("  Match on values:");
    for n in 0..=3 {
        println!("    {}: {}", n, describe_number(n));
    }

    println!("\n  Match with guards:");
    for age in &[10, 15, 20] {
        println!("    Age {}: {}", age, describe_age(*age));
    }

    // Match on tuples
    println!("\n  Match on tuples (FizzBuzz style):");
    for i in 1..=5 {
        let result = match (i % 3, i % 5) {
            (0, 0) => "FizzBuzz",
            (0, _) => "Fizz",
            (_, 0) => "Buzz",
            _ => "Number",
        };
        println!("    {}: {}", i, result);
    }

    // 8. If let (pattern matching for single case)
    println!("\n8. If let (single pattern match):");
    let some_value: Option<i32> = Some(42);
    if let Some(x) = some_value {
        println!("  Got value: {}", x);
    } else {
        println!("  Got None");
    }

    let none_value: Option<i32> = None;
    if let Some(x) = none_value {
        println!("  Got value: {}", x);
    } else {
        println!("  Got None");
    }

    // 9. While let (pattern matching loop)
    println!("\n9. While let (pattern matching loop):");
    let mut stack = vec![1, 2, 3];
    while let Some(top) = stack.pop() {
        println!("  Popped: {}", top);
    }

    // 10. Ranges
    println!("\n10. Ranges:");
    println!("  Inclusive range (1..=5):");
    print!("   ");
    for i in 1..=5 {
        print!(" {}", i);
    }
    println!();

    println!("  Exclusive range (1..5):");
    print!("   ");
    for i in 1..5 {
        print!(" {}", i);
    }
    println!();
}

// FizzBuzz using match
fn fizzbuzz(n: i32) -> String {
    match (n % 3, n % 5) {
        (0, 0) => "FizzBuzz".to_string(),
        (0, _) => "Fizz".to_string(),
        (_, 0) => "Buzz".to_string(),
        _ => n.to_string(),
    }
}

// Pattern matching example
fn describe_number(n: i32) -> &'static str {
    match n {
        0 => "Zero",
        1 => "One",
        2 => "Two",
        _ => "Many",
    }
}

// Match with guards
fn describe_age(age: i32) -> &'static str {
    match age {
        n if n >= 18 => "Adult",
        n if n >= 13 => "Teenager",
        _ => "Child",
    }
}
