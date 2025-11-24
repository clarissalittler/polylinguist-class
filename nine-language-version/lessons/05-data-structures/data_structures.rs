#!/usr/bin/env rust-script
/*
 * Lesson 5: Data Structures in Rust
 * Demonstrates ownership, borrowing, and memory safety
 */

use std::collections::{HashMap, HashSet};

fn main() {
    println!("=== Rust Data Structures ===\n");

    // 1. Vectors (mutable with mut)
    println!("1. Vectors (Explicit Mutability):");
    let mut numbers = vec![1, 2, 3, 4, 5];
    println!("  Original: {:?}", numbers);

    numbers.push(6);
    println!("  After push(6): {:?}", numbers);

    numbers[0] = 10;
    println!("  After numbers[0] = 10: {:?}", numbers);

    // Immutable vector
    let numbers_immut = vec![1, 2, 3, 4, 5];
    // numbers_immut.push(6);  // ERROR: not mutable!
    println!("\n  Immutable vector: {:?}", numbers_immut);

    // 2. Ownership and moves
    println!("\n2. Ownership and Moves:");
    let vec1 = vec![1, 2, 3];
    let vec2 = vec1;  // MOVES ownership, vec1 no longer usable!
    // println!("{:?}", vec1);  // ERROR: value moved
    println!("  vec2 (moved from vec1): {:?}", vec2);

    // Cloning to keep both
    let vec3 = vec![4, 5, 6];
    let vec4 = vec3.clone();  // Explicit copy
    println!("  vec3: {:?}", vec3);  // Still usable
    println!("  vec4 (cloned): {:?}", vec4);

    // 3. Borrowing (references)
    println!("\n3. Borrowing (References):");
    let vec5 = vec![1, 2, 3];

    // Immutable borrow
    let borrowed = &vec5;
    println!("  Borrowed: {:?}", borrowed);
    println!("  Original: {:?}", vec5);  // Still usable

    // Mutable borrow
    let mut vec6 = vec![1, 2, 3];
    {
        let borrowed_mut = &mut vec6;
        borrowed_mut.push(4);
        println!("  Mutably borrowed and modified: {:?}", borrowed_mut);
    }  // Mutable borrow ends here
    println!("  After mutable borrow: {:?}", vec6);

    // 4. Iterator methods (functional style)
    println!("\n4. Iterator Methods:");
    let nums = vec![1, 2, 3, 4, 5];

    let squared: Vec<i32> = nums.iter().map(|x| x * x).collect();
    println!("  map(square): {:?}", squared);

    let evens: Vec<i32> = nums.iter().filter(|x| *x % 2 == 0).copied().collect();
    println!("  filter(even): {:?}", evens);

    let sum: i32 = nums.iter().sum();
    println!("  sum: {}", sum);

    // 5. HashMap (mutable)
    println!("\n5. HashMap (Explicit Mutability):");
    let mut person = HashMap::new();
    person.insert("name", "Alice");
    person.insert("age", "30");
    person.insert("city", "NYC");
    println!("  Person: {:?}", person);

    // Modify
    person.insert("age", "31");
    person.insert("email", "alice@example.com");
    println!("  After modifications: {:?}", person);

    // Access
    match person.get("name") {
        Some(name) => println!("  Name: {}", name),
        None => println!("  Name not found"),
    }

    println!("  Has 'name': {}", person.contains_key("name"));

    // 6. HashSet (unique values)
    println!("\n6. HashSet (Unique Values):");
    let mut set = HashSet::new();
    set.insert(1);
    set.insert(2);
    set.insert(3);
    set.insert(3);  // Duplicate, ignored
    println!("  Set: {:?}", set);

    println!("  Contains 2: {}", set.contains(&2));
    println!("  Length: {}", set.len());

    // Set operations
    let evens: HashSet<i32> = [2, 4, 6, 8].iter().cloned().collect();
    let odds: HashSet<i32> = [1, 3, 5, 7].iter().cloned().collect();
    let union: HashSet<i32> = evens.union(&odds).cloned().collect();
    println!("  Union: {:?}", union);

    // 7. Tuples (immutable)
    println!("\n7. Tuples (Immutable):");
    let point = (3, 4);
    println!("  Point: {:?}", point);
    println!("  x={}, y={}", point.0, point.1);

    // Destructuring
    let (x, y) = point;
    println!("  Destructured: x={}, y={}", x, y);

    // Mixed types
    let mixed = ("Alice", 30, true);
    println!("  Mixed tuple: {:?}", mixed);

    // 8. Arrays (fixed size)
    println!("\n8. Arrays (Fixed Size):");
    let arr: [i32; 5] = [1, 2, 3, 4, 5];
    println!("  Array: {:?}", arr);
    println!("  Length: {}", arr.len());

    // Arrays have fixed size at compile time
    // let arr2: [i32; 6] = arr;  // ERROR: different size

    // 9. Slices (views into arrays/vectors)
    println!("\n9. Slices (Views):");
    let vec7 = vec![1, 2, 3, 4, 5];
    let slice = &vec7[1..4];  // [2, 3, 4]
    println!("  Slice [1..4]: {:?}", slice);

    // 10. Ownership prevents bugs
    println!("\n10. Ownership Prevents Bugs:");
    println!("  - Can't use moved values");
    println!("  - Can't have multiple mutable references");
    println!("  - Can't have mutable + immutable refs simultaneously");
    println!("  - No null pointer errors");
    println!("  - No data races at compile time");

    // 11. Smart pointers
    println!("\n11. Smart Pointers:");
    use std::rc::Rc;

    // Rc for shared ownership
    let shared = Rc::new(vec![1, 2, 3]);
    let shared2 = Rc::clone(&shared);
    println!("  Shared count: {}", Rc::strong_count(&shared));
    println!("  Shared data: {:?}", shared);

    // 12. Performance notes
    println!("\n12. Performance Notes:");
    println!("  - Zero-cost abstractions");
    println!("  - No garbage collector");
    println!("  - Compile-time memory safety");
    println!("  - Explicit cloning makes copies visible");
}
