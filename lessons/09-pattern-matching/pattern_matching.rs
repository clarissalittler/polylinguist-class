// Lesson 9: Pattern Matching in Rust
//
// Rust has excellent pattern matching! It's exhaustive by default,
// meaning the compiler ensures you handle all possible cases.

// ====================
// 1. Basic Match Expression
// ====================

fn describe_number(n: i32) -> &'static str {
    match n {
        0 => "zero",
        1 => "one",
        2 => "two",
        _ => "many",  // _ is wildcard
    }
}

// ====================
// 2. Tuple Patterns
// ====================

fn describe_point(point: (i32, i32)) -> String {
    match point {
        (0, 0) => "origin".to_string(),
        (0, y) => format!("on y-axis at y={}", y),
        (x, 0) => format!("on x-axis at x={}", x),
        (x, y) => format!("point at ({}, {})", x, y),
    }
}

// ====================
// 3. Enum Patterns
// ====================

enum Shape {
    Circle { radius: f64 },
    Rectangle { width: f64, height: f64 },
    Triangle { a: f64, b: f64, c: f64 },
}

fn area(shape: &Shape) -> f64 {
    use std::f64::consts::PI;
    match shape {
        Shape::Circle { radius } => PI * radius * radius,
        Shape::Rectangle { width, height } => width * height,
        Shape::Triangle { a, b, c } => {
            let s = (a + b + c) / 2.0;
            (s * (s - a) * (s - b) * (s - c)).sqrt()
        }
    }
}

// ====================
// 4. Option Pattern Matching
// ====================

fn describe_option(opt: Option<i32>) -> String {
    match opt {
        Some(x) => format!("just {}", x),
        None => "nothing".to_string(),
    }
}

// ====================
// 5. Result Pattern Matching
// ====================

fn describe_result(res: Result<i32, String>) -> String {
    match res {
        Ok(val) => format!("success: {}", val),
        Err(e) => format!("error: {}", e),
    }
}

// ====================
// 6. Guards
// ====================

fn classify(n: i32) -> &'static str {
    match n {
        n if n < 0 => "negative",
        0 => "zero",
        n if n < 10 => "small positive",
        n if n < 100 => "medium positive",
        _ => "large positive",
    }
}

// ====================
// 7. Or Patterns
// ====================

fn is_weekend(day: &str) -> bool {
    match day.to_lowercase().as_str() {
        "saturday" | "sunday" => true,
        _ => false,
    }
}

// ====================
// 8. Range Patterns
// ====================

fn grade_letter(score: u32) -> char {
    match score {
        90..=100 => 'A',
        80..=89 => 'B',
        70..=79 => 'C',
        60..=69 => 'D',
        _ => 'F',
    }
}

// ====================
// 9. Expression Evaluator
// ====================

enum Expr {
    Num(i32),
    Add(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Neg(Box<Expr>),
}

fn eval(expr: &Expr) -> i32 {
    match expr {
        Expr::Num(n) => *n,
        Expr::Add(left, right) => eval(left) + eval(right),
        Expr::Mul(left, right) => eval(left) * eval(right),
        Expr::Neg(e) => -eval(e),
    }
}

// ====================
// 10. Nested Patterns
// ====================

fn analyze_nested(data: (Option<i32>, Option<i32>)) -> String {
    match data {
        (Some(x), Some(y)) => format!("both: {} and {}", x, y),
        (Some(x), None) => format!("only first: {}", x),
        (None, Some(y)) => format!("only second: {}", y),
        (None, None) => "neither".to_string(),
    }
}

// ====================
// 11. Struct Patterns
// ====================

#[derive(Debug)]
struct User {
    name: String,
    age: u32,
    active: bool,
}

fn describe_user(user: &User) -> String {
    match user {
        User { active: true, age, name } if *age >= 18 => {
            format!("Active adult: {}", name)
        }
        User { active: true, name, .. } => {
            format!("Active minor: {}", name)
        }
        User { active: false, name, .. } => {
            format!("Inactive: {}", name)
        }
    }
}

// ====================
// Main Demonstration
// ====================

fn main() {
    println!("=== Pattern Matching in Rust ===\n");

    // 1. Basic matching
    println!("1. Basic Match:");
    for n in [0, 1, 2, 5] {
        println!("   {} -> {}", n, describe_number(n));
    }

    // 2. Tuple patterns
    println!("\n2. Tuple Patterns:");
    let points = vec![(0, 0), (0, 5), (3, 0), (2, 3)];
    for p in points {
        println!("   {:?} -> {}", p, describe_point(p));
    }

    // 3. Enum patterns
    println!("\n3. Enum Patterns (Shapes):");
    let shapes = vec![
        Shape::Circle { radius: 5.0 },
        Shape::Rectangle { width: 4.0, height: 6.0 },
        Shape::Triangle { a: 3.0, b: 4.0, c: 5.0 },
    ];
    for shape in &shapes {
        println!("   area = {:.2}", area(shape));
    }

    // 4. Option
    println!("\n4. Option Pattern:");
    println!("   {}", describe_option(Some(42)));
    println!("   {}", describe_option(None));

    // 5. Result
    println!("\n5. Result Pattern:");
    println!("   {}", describe_result(Ok(100)));
    println!("   {}", describe_result(Err("failed".to_string())));

    // 6. Guards
    println!("\n6. Guards:");
    for n in [-5, 0, 3, 50, 500] {
        println!("   {} -> {}", n, classify(n));
    }

    // 7. Or patterns
    println!("\n7. Or Patterns:");
    for day in ["Monday", "Saturday", "Sunday"] {
        println!("   {} is weekend? {}", day, is_weekend(day));
    }

    // 8. Range patterns
    println!("\n8. Range Patterns:");
    for score in [95, 85, 75, 65, 55] {
        println!("   {} -> {}", score, grade_letter(score));
    }

    // 9. Expression evaluator
    println!("\n9. Expression Evaluator:");
    let expr = Expr::Mul(
        Box::new(Expr::Add(Box::new(Expr::Num(2)), Box::new(Expr::Num(3)))),
        Box::new(Expr::Num(4)),
    );
    println!("   (2 + 3) * 4 = {}", eval(&expr));

    // 10. Nested patterns
    println!("\n10. Nested Patterns:");
    let cases = vec![
        (Some(1), Some(2)),
        (Some(3), None),
        (None, Some(4)),
        (None, None),
    ];
    for case in cases {
        println!("   {:?} -> {}", case, analyze_nested(case));
    }

    // 11. Struct patterns
    println!("\n11. Struct Patterns:");
    let users = vec![
        User { name: "Alice".to_string(), age: 25, active: true },
        User { name: "Bob".to_string(), age: 16, active: true },
        User { name: "Charlie".to_string(), age: 30, active: false },
    ];
    for user in &users {
        println!("   {}", describe_user(user));
    }
}
