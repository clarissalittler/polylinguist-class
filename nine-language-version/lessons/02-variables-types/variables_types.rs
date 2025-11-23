// variables_types.rs

fn main() {
    // Immutable by default
    let age = 25;               // inferred as i32
    let price = 19.99;          // inferred as f64
    let name = "Alice";         // &str (string slice)
    let is_student = true;      // bool

    // Explicit type annotations
    let explicit_age: i32 = 25;
    let explicit_price: f64 = 19.99;

    // Mutable variables (must be declared)
    let mut counter = 0;
    counter = counter + 1;      // OK
    // age = 30;                // ERROR: cannot assign twice

    // Integer types (signed and unsigned)
    let signed: i32 = -42;      // i8, i16, i32, i64, i128
    let unsigned: u32 = 42;     // u8, u16, u32, u64, u128

    // Floating point
    let f32_num: f32 = 3.14;
    let f64_num: f64 = 3.14159265359;

    // Character (Unicode)
    let letter: char = 'A';
    let emoji: char = '😀';

    // Tuples
    let person: (&str, i32) = ("Alice", 25);

    // Arrays (fixed size)
    let numbers: [i32; 5] = [1, 2, 3, 4, 5];

    println!("Age: {}", age);
    println!("Name: {}", name);
    println!("Is student: {}", is_student);
    println!("Person: {:?}", person);
    println!("Numbers: {:?}", numbers);
    println!("Letter: {}, Emoji: {}", letter, emoji);
}
