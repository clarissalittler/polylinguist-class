// Lesson 8: Higher-Order Functions in Rust
//
// Rust has excellent functional programming support:
// - First-class functions
// - Closures with type inference
// - Iterator trait with map/filter/fold
// - Zero-cost abstractions
// - Ownership affects closure types

use std::collections::HashMap;

// ====================
// 1. Functions as First-Class Values
// ====================

fn demonstrate_first_class() {
    // Functions can be assigned
    let greet = |name: &str| format!("Hello, {}!", name);

    println!("   {}", greet("Alice"));

    // Store in vec
    let operations: Vec<fn(i32) -> i32> = vec![
        |x| x * 2,
        |x| x + 1,
        |x| x * x,
    ];

    let results: Vec<i32> = operations.iter().map(|f| f(5)).collect();
    println!("   Apply all operations to 5: {:?}", results);
}

// ====================
// 2. Functions Taking Functions
// ====================

fn apply_twice<F>(func: F, x: i32) -> i32
where
    F: Fn(i32) -> i32,
{
    func(func(x))
}

fn apply_n_times<F>(func: F, n: usize, x: i32) -> i32
where
    F: Fn(i32) -> i32,
{
    let mut result = x;
    for _ in 0..n {
        result = func(result);
    }
    result
}

// ====================
// 3. Functions Returning Functions
// ====================

fn make_multiplier(n: i32) -> impl Fn(i32) -> i32 {
    move |x| x * n
}

fn make_adder(n: i32) -> impl Fn(i32) -> i32 {
    move |x| x + n
}

// ====================
// 4. Closure Types: Fn, FnMut, FnOnce
// ====================

fn demonstrate_closure_types() {
    // Fn - immutable borrow
    let x = 10;
    let add_x = |y| x + y;  // Borrows x immutably
    println!("   Fn closure: {} + 5 = {}", x, add_x(5));

    // FnMut - mutable borrow
    let mut count = 0;
    let mut increment = || {
        count += 1;
        count
    };
    println!("   FnMut: {}", increment());
    println!("   FnMut: {}", increment());

    // FnOnce - takes ownership
    let s = String::from("hello");
    let consume = || {
        let _owned = s;  // Takes ownership
        // s cannot be used again
    };
    consume();  // Can only be called once
}

// ====================
// 5. Map - Transform Each Element
// ====================

fn demonstrate_map() {
    let numbers = vec![1, 2, 3, 4, 5];

    // Map with closure
    let doubled: Vec<i32> = numbers.iter().map(|x| x * 2).collect();
    println!("   Doubled: {:?}", doubled);

    // Map with method reference
    let strings = vec!["hello", "world"];
    let lengths: Vec<usize> = strings.iter().map(|s| s.len()).collect();
    println!("   Lengths: {:?}", lengths);

    // Chaining maps
    let result: Vec<i32> = numbers
        .iter()
        .map(|x| x + 1)
        .map(|x| x * 2)
        .collect();
    println!("   Chained: {:?}", result);

    // enumerate for index
    let with_index: Vec<String> = numbers
        .iter()
        .enumerate()
        .map(|(i, x)| format!("{}: {}", i, x))
        .collect();
    println!("   With index: {:?}", with_index);
}

// ====================
// 6. Filter - Select Elements
// ====================

fn demonstrate_filter() {
    let numbers: Vec<i32> = (1..=10).collect();

    // Filter evens
    let evens: Vec<i32> = numbers.iter().filter(|&&x| x % 2 == 0).copied().collect();
    println!("   Evens: {:?}", evens);

    // Filter with complex predicate
    let big_odds: Vec<i32> = numbers
        .iter()
        .filter(|&&x| x % 2 != 0 && x > 5)
        .copied()
        .collect();
    println!("   Big odds: {:?}", big_odds);

    // filter_map - combines filter and map
    let parsed: Vec<i32> = vec!["1", "two", "3", "four"]
        .iter()
        .filter_map(|s| s.parse::<i32>().ok())
        .collect();
    println!("   Parsed numbers: {:?}", parsed);
}

// ====================
// 7. Fold (Reduce)
// ====================

fn demonstrate_fold() {
    let numbers = vec![1, 2, 3, 4, 5];

    // Sum
    let sum = numbers.iter().fold(0, |acc, x| acc + x);
    println!("   Sum: {}", sum);

    // Product
    let product = numbers.iter().fold(1, |acc, x| acc * x);
    println!("   Product: {}", product);

    // Maximum
    let max = numbers.iter().fold(i32::MIN, |max, &x| max.max(x));
    println!("   Max: {}", max);

    // Build a HashMap
    let fruits = vec!["apple", "banana", "apple", "orange", "banana", "apple"];
    let counts = fruits.iter().fold(HashMap::new(), |mut acc, &fruit| {
        *acc.entry(fruit).or_insert(0) += 1;
        acc
    });
    println!("   Fruit counts: {:?}", counts);

    // Flatten nested vec
    let nested = vec![vec![1, 2], vec![3, 4], vec![5]];
    let flattened: Vec<i32> = nested
        .iter()
        .fold(vec![], |mut acc, v| {
            acc.extend(v);
            acc
        });
    println!("   Flattened: {:?}", flattened);
}

// ====================
// 8. Closures and Ownership
// ====================

fn make_counter() -> impl FnMut() -> i32 {
    let mut count = 0;
    move || {
        count += 1;
        count
    }
}

struct BankAccount {
    balance: i32,
}

impl BankAccount {
    fn new(initial: i32) -> Self {
        BankAccount { balance: initial }
    }

    fn deposit(&mut self, amount: i32) {
        if amount > 0 {
            self.balance += amount;
        }
    }

    fn withdraw(&mut self, amount: i32) -> bool {
        if amount > 0 && amount <= self.balance {
            self.balance -= amount;
            true
        } else {
            false
        }
    }

    fn balance(&self) -> i32 {
        self.balance
    }
}

// ====================
// 9. Function Composition
// ====================

fn compose<A, B, C, F, G>(f: F, g: G) -> impl Fn(A) -> C
where
    F: Fn(B) -> C,
    G: Fn(A) -> B,
{
    move |x| f(g(x))
}

fn demonstrate_composition() {
    let add_one = |x: i32| x + 1;
    let double = |x: i32| x * 2;

    let f = compose(double, add_one);
    println!("   compose(double, add_one)(5) = {}", f(5));

    // Manual pipeline
    let pipeline = |x: i32| {
        let a = add_one(x);
        let b = double(a);
        let c = |x: i32| x * x;
        c(b)
    };
    println!("   Pipeline(5) = {}", pipeline(5));
}

// ====================
// 10. Common Iterator Methods
// ====================

fn demonstrate_common() {
    let numbers = vec![1, 2, 3, 4, 5];

    // all - all elements satisfy predicate
    let all_positive = numbers.iter().all(|&x| x > 0);
    println!("   All positive? {}", all_positive);

    // any - any element satisfies predicate
    let has_even = numbers.iter().any(|&x| x % 2 == 0);
    println!("   Has even? {}", has_even);

    // find - first element matching predicate
    let first_even = numbers.iter().find(|&&x| x % 2 == 0);
    println!("   First even: {:?}", first_even);

    // position - index of first match
    let pos = numbers.iter().position(|&x| x == 3);
    println!("   Position of 3: {:?}", pos);

    // take - first n elements
    let first_three: Vec<i32> = numbers.iter().take(3).copied().collect();
    println!("   Take 3: {:?}", first_three);

    // skip - skip first n elements
    let skip_two: Vec<i32> = numbers.iter().skip(2).copied().collect();
    println!("   Skip 2: {:?}", skip_two);

    // zip - combine two iterators
    let list1 = vec![1, 2, 3];
    let list2 = vec!['a', 'b', 'c'];
    let zipped: Vec<(i32, char)> = list1.iter().zip(list2.iter()).map(|(&x, &y)| (x, y)).collect();
    println!("   Zipped: {:?}", zipped);

    // partition - split into two vecs
    let (evens, odds): (Vec<i32>, Vec<i32>) = numbers
        .iter()
        .copied()
        .partition(|&x| x % 2 == 0);
    println!("   Partition: evens={:?}, odds={:?}", evens, odds);
}

// ====================
// 11. Lazy Evaluation
// ====================

fn demonstrate_lazy() {
    // Iterators are lazy - nothing computed until collect()
    let numbers = 1..=1000000;  // Range, not Vec!

    // Only computes what's needed
    let result: Vec<i32> = numbers
        .filter(|&x| x % 2 == 0)
        .map(|x| x * 2)
        .take(5)
        .collect();

    println!("   First 5 doubled evens: {:?}", result);

    // Infinite iterator (lazy!)
    let fibonacci = std::iter::successors(Some((0, 1)), |&(a, b)| Some((b, a + b)))
        .map(|(a, _)| a);

    let first_10: Vec<i32> = fibonacci.take(10).collect();
    println!("   First 10 Fibonacci: {:?}", first_10);
}

// ====================
// 12. Custom Higher-Order Function
// ====================

fn custom_map<T, U, F>(vec: Vec<T>, f: F) -> Vec<U>
where
    F: Fn(T) -> U,
{
    let mut result = Vec::new();
    for item in vec {
        result.push(f(item));
    }
    result
}

fn custom_filter<T, F>(vec: Vec<T>, predicate: F) -> Vec<T>
where
    F: Fn(&T) -> bool,
{
    let mut result = Vec::new();
    for item in vec {
        if predicate(&item) {
            result.push(item);
        }
    }
    result
}

fn custom_fold<T, U, F>(vec: Vec<T>, init: U, f: F) -> U
where
    F: Fn(U, T) -> U,
{
    let mut acc = init;
    for item in vec {
        acc = f(acc, item);
    }
    acc
}

// ====================
// 13. Real-World Example: Data Pipeline
// ====================

#[derive(Debug, Clone)]
struct User {
    name: String,
    age: i32,
    active: bool,
}

fn process_users(users: Vec<User>) -> Vec<User> {
    users
        .into_iter()
        .filter(|u| u.active)
        .map(|mut u| {
            u.name = u.name.trim().to_lowercase();
            u
        })
        .collect::<Vec<_>>()
        .into_iter()
        .take(10)
        .collect()
}

// ====================
// Main Demonstration
// ====================

fn main() {
    println!("=== Higher-Order Functions in Rust ===\n");

    // 1. First-class functions
    println!("1. Functions as First-Class Values:");
    demonstrate_first_class();

    // 2. Functions taking functions
    println!("\n2. Functions Taking Functions:");
    println!("   apply_twice(|x| x + 1, 5) = {}", apply_twice(|x| x + 1, 5));
    println!("   apply_n_times(|x| x * 2, 3, 2) = {}", apply_n_times(|x| x * 2, 3, 2));

    // 3. Functions returning functions
    println!("\n3. Functions Returning Functions:");
    let times_three = make_multiplier(3);
    let add_ten = make_adder(10);
    println!("   times_three(7) = {}", times_three(7));
    println!("   add_ten(5) = {}", add_ten(5));

    // 4. Closure types
    println!("\n4. Closure Types (Fn, FnMut, FnOnce):");
    demonstrate_closure_types();

    // 5. Map
    println!("\n5. Map - Transform Each Element:");
    demonstrate_map();

    // 6. Filter
    println!("\n6. Filter - Select Elements:");
    demonstrate_filter();

    // 7. Fold
    println!("\n7. Fold - Combine to Single Value:");
    demonstrate_fold();

    // 8. Closures and ownership
    println!("\n8. Closures and Ownership:");
    let mut counter = make_counter();
    println!("   counter() = {}", counter());
    println!("   counter() = {}", counter());
    println!("   counter() = {}", counter());

    let mut account = BankAccount::new(1000);
    println!("   Initial balance: ${}", account.balance());
    account.deposit(500);
    println!("   After deposit: ${}", account.balance());
    account.withdraw(200);
    println!("   After withdrawal: ${}", account.balance());

    // 9. Function composition
    println!("\n9. Function Composition:");
    demonstrate_composition();

    // 10. Common iterator methods
    println!("\n10. Common Iterator Methods:");
    demonstrate_common();

    // 11. Lazy evaluation
    println!("\n11. Lazy Evaluation:");
    demonstrate_lazy();

    // 12. Custom implementations
    println!("\n12. Custom HOF Implementations:");
    let numbers = vec![1, 2, 3, 4, 5];
    let doubled = custom_map(numbers.clone(), |x| x * 2);
    println!("   custom_map: {:?}", doubled);
    let evens = custom_filter(numbers.clone(), |&x| x % 2 == 0);
    println!("   custom_filter: {:?}", evens);
    let sum = custom_fold(numbers.clone(), 0, |acc, x| acc + x);
    println!("   custom_fold: {}", sum);

    // 13. Real-world example
    println!("\n13. Real-World Data Pipeline:");
    let users = vec![
        User { name: " Alice ".to_string(), age: 25, active: true },
        User { name: "BOB".to_string(), age: 17, active: true },
        User { name: "Charlie ".to_string(), age: 30, active: false },
        User { name: "DIANA".to_string(), age: 28, active: true },
    ];
    let processed = process_users(users);
    println!("   Processed: {:?}", processed);

    // 14. Method chaining
    println!("\n14. Method Chaining:");
    let result: i32 = vec![1, -2, 3, -4, 5, 6, 7, 8, 9, 10]
        .into_iter()
        .filter(|&x| x > 0)
        .map(|x| x * x)
        .sum();
    println!("   Sum of squares of positive numbers: {}", result);

    println!("\n15. Rust's Unique Features:");
    println!("   - Zero-cost abstractions (compiled to efficient code)");
    println!("   - Ownership affects closure types (Fn/FnMut/FnOnce)");
    println!("   - Iterators are lazy by default");
    println!("   - Strong type inference for closures");
    println!("   - Move semantics for capturing environment");
}
