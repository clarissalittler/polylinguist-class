// Lesson 7: Object-Oriented Programming in Rust
//
// Rust doesn't have classes or inheritance in the traditional sense.
// Instead, it uses:
// - Structs for data
// - Impl blocks for methods
// - Traits for shared behavior (like interfaces)
// - Composition over inheritance

use std::f64::consts::PI;

// ====================
// 1. Basic Struct with Methods
// ====================

struct Person {
    name: String,
    age: u32,
}

impl Person {
    // Associated function (like static method) - constructor
    fn new(name: String, age: u32) -> Self {
        Person { name, age }
    }

    // Method (takes &self)
    fn introduce(&self) -> String {
        format!("Hi, I'm {}, {} years old", self.name, self.age)
    }

    // Mutable method (takes &mut self)
    fn have_birthday(&mut self) {
        self.age += 1;
    }

    fn get_age(&self) -> u32 {
        self.age
    }
}

// ====================
// 2. Traits (like interfaces)
// ====================

trait Animal {
    fn speak(&self) -> String;
    fn sleep(&self) -> String {
        // Default implementation
        "Zzz...".to_string()
    }
}

struct Dog {
    name: String,
    breed: String,
}

impl Dog {
    fn new(name: String, breed: String) -> Self {
        Dog { name, breed }
    }

    fn fetch(&self) -> String {
        format!("{} is fetching the ball!", self.name)
    }
}

impl Animal for Dog {
    fn speak(&self) -> String {
        format!("{} says Woof!", self.name)
    }

    fn sleep(&self) -> String {
        format!("{} is sleeping... Zzz", self.name)
    }
}

struct Cat {
    name: String,
    indoor: bool,
}

impl Cat {
    fn new(name: String, indoor: bool) -> Self {
        Cat { name, indoor }
    }

    fn scratch(&self) -> String {
        format!("{} is scratching the furniture!", self.name)
    }
}

impl Animal for Cat {
    fn speak(&self) -> String {
        format!("{} says Meow!", self.name)
    }

    fn sleep(&self) -> String {
        format!("{} is sleeping... Zzz", self.name)
    }
}

// ====================
// 3. Trait Objects for Polymorphism
// ====================

fn make_animal_speak(animal: &dyn Animal) {
    println!("   {}", animal.speak());
}

// ====================
// 4. Abstract-like Behavior with Traits
// ====================

trait Shape {
    fn area(&self) -> f64;
    fn perimeter(&self) -> f64;

    fn describe(&self) -> String {
        format!(
            "{}: area={:.2}, perimeter={:.2}",
            self.name(),
            self.area(),
            self.perimeter()
        )
    }

    fn name(&self) -> &str;
}

struct Circle {
    radius: f64,
}

impl Circle {
    fn new(radius: f64) -> Self {
        Circle { radius }
    }
}

impl Shape for Circle {
    fn area(&self) -> f64 {
        PI * self.radius * self.radius
    }

    fn perimeter(&self) -> f64 {
        2.0 * PI * self.radius
    }

    fn name(&self) -> &str {
        "Circle"
    }
}

struct Rectangle {
    width: f64,
    height: f64,
}

impl Rectangle {
    fn new(width: f64, height: f64) -> Self {
        Rectangle { width, height }
    }
}

impl Shape for Rectangle {
    fn area(&self) -> f64 {
        self.width * self.height
    }

    fn perimeter(&self) -> f64 {
        2.0 * (self.width + self.height)
    }

    fn name(&self) -> &str {
        "Rectangle"
    }
}

struct Triangle {
    side_a: f64,
    side_b: f64,
    side_c: f64,
}

impl Triangle {
    fn new(side_a: f64, side_b: f64, side_c: f64) -> Self {
        Triangle {
            side_a,
            side_b,
            side_c,
        }
    }
}

impl Shape for Triangle {
    fn area(&self) -> f64 {
        // Heron's formula
        let s = self.perimeter() / 2.0;
        (s * (s - self.side_a) * (s - self.side_b) * (s - self.side_c)).sqrt()
    }

    fn perimeter(&self) -> f64 {
        self.side_a + self.side_b + self.side_c
    }

    fn name(&self) -> &str {
        "Triangle"
    }
}

// ====================
// 5. Encapsulation (pub vs private)
// ====================

mod bank {
    pub struct BankAccount {
        account_number: String,
        balance: f64,          // Private by default!
        transactions: Vec<String>,
    }

    impl BankAccount {
        pub fn new(account_number: String, initial_balance: f64) -> Self {
            BankAccount {
                account_number,
                balance: initial_balance,
                transactions: Vec::new(),
            }
        }

        pub fn deposit(&mut self, amount: f64) -> bool {
            if amount > 0.0 {
                self.balance += amount;
                self.transactions
                    .push(format!("Deposit: +${:.2}", amount));
                true
            } else {
                false
            }
        }

        pub fn withdraw(&mut self, amount: f64) -> bool {
            if amount > 0.0 && amount <= self.balance {
                self.balance -= amount;
                self.transactions
                    .push(format!("Withdrawal: -${:.2}", amount));
                true
            } else {
                false
            }
        }

        pub fn get_balance(&self) -> f64 {
            self.balance
        }

        pub fn get_transaction_history(&self) -> Vec<String> {
            self.transactions.clone()
        }
    }
}

use bank::BankAccount;

// ====================
// 6. Associated Functions (like static methods)
// ====================

struct Temperature {
    celsius: f64,
}

impl Temperature {
    // Associated function (constructor)
    fn new(celsius: f64) -> Self {
        Temperature { celsius }
    }

    // Factory methods
    fn from_fahrenheit(fahrenheit: f64) -> Self {
        let celsius = (fahrenheit - 32.0) * 5.0 / 9.0;
        Temperature { celsius }
    }

    fn from_kelvin(kelvin: f64) -> Self {
        let celsius = kelvin - 273.15;
        Temperature { celsius }
    }

    fn is_freezing(celsius: f64) -> bool {
        celsius <= 0.0
    }

    fn to_fahrenheit(&self) -> f64 {
        (self.celsius * 9.0 / 5.0) + 32.0
    }

    fn to_kelvin(&self) -> f64 {
        self.celsius + 273.15
    }
}

// ====================
// 7. Composition
// ====================

struct Engine {
    horsepower: u32,
    running: bool,
}

impl Engine {
    fn new(horsepower: u32) -> Self {
        Engine {
            horsepower,
            running: false,
        }
    }

    fn start(&mut self) -> String {
        self.running = true;
        format!("Engine starting... {}hp engine now running", self.horsepower)
    }

    fn stop(&mut self) -> String {
        self.running = false;
        "Engine stopped".to_string()
    }
}

struct Car {
    brand: String,
    model: String,
    engine: Engine, // Composition!
}

impl Car {
    fn new(brand: String, model: String, horsepower: u32) -> Self {
        Car {
            brand,
            model,
            engine: Engine::new(horsepower),
        }
    }

    fn start(&mut self) -> String {
        format!("{} {}: {}", self.brand, self.model, self.engine.start())
    }

    fn stop(&mut self) -> String {
        format!("{} {}: {}", self.brand, self.model, self.engine.stop())
    }
}

// ====================
// 8. Multiple Traits (like multiple interfaces)
// ====================

trait Flyable {
    fn fly(&self) -> String;
}

trait Swimmable {
    fn swim(&self) -> String;
}

struct Duck {
    name: String,
}

impl Duck {
    fn new(name: String) -> Self {
        Duck { name }
    }

    fn quack(&self) -> String {
        "Quack!".to_string()
    }
}

impl Animal for Duck {
    fn speak(&self) -> String {
        format!("{} says Quack!", self.name)
    }
}

impl Flyable for Duck {
    fn fly(&self) -> String {
        format!("{} is flying through the air", self.name)
    }
}

impl Swimmable for Duck {
    fn swim(&self) -> String {
        format!("{} is swimming in water", self.name)
    }
}

// ====================
// 9. Operator Overloading with Traits
// ====================

use std::ops::{Add, Sub};

#[derive(Debug, Clone, Copy)]
struct Point {
    x: f64,
    y: f64,
}

impl Point {
    fn new(x: f64, y: f64) -> Self {
        Point { x, y }
    }

    fn distance_from_origin(&self) -> f64 {
        (self.x * self.x + self.y * self.y).sqrt()
    }
}

impl Add for Point {
    type Output = Point;

    fn add(self, other: Point) -> Point {
        Point::new(self.x + other.x, self.y + other.y)
    }
}

impl Sub for Point {
    type Output = Point;

    fn sub(self, other: Point) -> Point {
        Point::new(self.x - other.x, self.y - other.y)
    }
}

// ====================
// 10. Generic Structs and Traits
// ====================

struct Container<T> {
    value: T,
}

impl<T> Container<T> {
    fn new(value: T) -> Self {
        Container { value }
    }

    fn get(&self) -> &T {
        &self.value
    }
}

// ====================
// 11. Design Pattern: Builder
// ====================
// (Rust doesn't use Singleton much due to ownership)

struct PersonBuilder {
    name: Option<String>,
    age: Option<u32>,
}

impl PersonBuilder {
    fn new() -> Self {
        PersonBuilder {
            name: None,
            age: None,
        }
    }

    fn name(mut self, name: String) -> Self {
        self.name = Some(name);
        self
    }

    fn age(mut self, age: u32) -> Self {
        self.age = Some(age);
        self
    }

    fn build(self) -> Result<Person, &'static str> {
        Ok(Person {
            name: self.name.ok_or("Name is required")?,
            age: self.age.ok_or("Age is required")?,
        })
    }
}

// ====================
// Tests and Examples
// ====================

fn main() {
    println!("=== Object-Oriented Programming in Rust ===\n");

    // 1. Basic struct
    println!("1. Basic Struct:");
    let mut alice = Person::new("Alice".to_string(), 30);
    println!("   {}", alice.introduce());
    alice.have_birthday();
    println!("   After birthday: age = {}", alice.get_age());

    // 2. Traits and Polymorphism
    println!("\n2. Traits and Polymorphism:");
    let buddy = Dog::new("Buddy".to_string(), "Golden Retriever".to_string());
    let whiskers = Cat::new("Whiskers".to_string(), true);
    let max = Dog::new("Max".to_string(), "German Shepherd".to_string());

    println!("   {}", buddy.speak());
    println!("   {}", whiskers.speak());
    println!("   {}", max.speak());

    println!("   {}", buddy.fetch());
    println!("   {}", whiskers.scratch());

    // Trait objects for runtime polymorphism
    println!("\n   Using trait objects:");
    let animals: Vec<&dyn Animal> = vec![&buddy, &whiskers, &max];
    for animal in animals {
        make_animal_speak(animal);
    }

    // 3. Shapes
    println!("\n3. Shapes (Trait-based polymorphism):");
    let circle = Circle::new(5.0);
    let rectangle = Rectangle::new(4.0, 6.0);
    let triangle = Triangle::new(3.0, 4.0, 5.0);

    let shapes: Vec<&dyn Shape> = vec![&circle, &rectangle, &triangle];
    for shape in shapes {
        println!("   {}", shape.describe());
    }

    // 4. Encapsulation (Bank Account)
    println!("\n4. Encapsulation (Bank Account):");
    let mut account = BankAccount::new("ACC001".to_string(), 1000.0);
    println!("   Initial balance: ${:.2}", account.get_balance());
    account.deposit(500.0);
    println!("   After deposit: ${:.2}", account.get_balance());
    account.withdraw(200.0);
    println!("   After withdrawal: ${:.2}", account.get_balance());
    println!("   Transactions: {:?}", account.get_transaction_history());

    // 5. Associated functions
    println!("\n5. Associated Functions:");
    let temp1 = Temperature::new(0.0);
    let temp2 = Temperature::from_fahrenheit(32.0);
    let temp3 = Temperature::from_kelvin(273.15);

    println!("   0°C = {:.1}°F", temp1.to_fahrenheit());
    println!("   32°F = {:.1}°C", temp2.celsius);
    println!("   273.15K = {:.1}°C", temp3.celsius);
    println!("   Is 0°C freezing? {}", Temperature::is_freezing(0.0));

    // 6. Composition
    println!("\n6. Composition:");
    let mut car = Car::new("Toyota".to_string(), "Camry".to_string(), 200);
    println!("   {}", car.start());
    println!("   {}", car.stop());

    // 7. Multiple traits
    println!("\n7. Multiple Traits:");
    let duck = Duck::new("Donald".to_string());
    println!("   {}", duck.quack());
    println!("   {}", duck.fly());
    println!("   {}", duck.swim());

    // 8. Operator overloading
    println!("\n8. Operator Overloading:");
    let p1 = Point::new(3.0, 4.0);
    let p2 = Point::new(1.0, 2.0);
    let p3 = p1 + p2;
    let p4 = p1 - p2;
    println!("   p1 = {:?}", p1);
    println!("   p2 = {:?}", p2);
    println!("   p1 + p2 = {:?}", p3);
    println!("   p1 - p2 = {:?}", p4);
    println!(
        "   p1 distance from origin: {:.2}",
        p1.distance_from_origin()
    );

    // 9. Generics
    println!("\n9. Generics:");
    let int_container = Container::new(42);
    let string_container = Container::new("Hello".to_string());
    println!("   Integer container: {}", int_container.get());
    println!("   String container: {}", string_container.get());

    // 10. Builder pattern
    println!("\n10. Builder Pattern:");
    let bob = PersonBuilder::new()
        .name("Bob".to_string())
        .age(25)
        .build()
        .unwrap();
    println!("   {}", bob.introduce());
}
