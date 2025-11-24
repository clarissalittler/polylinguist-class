# Language Comparison Guide

This guide provides side-by-side comparisons of how different languages approach common programming tasks.

## Basic Syntax Comparison

### Variable Declaration

```python
# Python - dynamic, implicit
name = "Alice"
age = 25
```

```javascript
// JavaScript - dynamic, explicit declaration
let name = "Alice";
const age = 25;  // immutable
```

```c
// C - static, explicit type
char name[] = "Alice";
int age = 25;
```

```java
// Java - static, explicit type
String name = "Alice";
int age = 25;
// Or with type inference (Java 10+)
var name = "Alice";
```

```haskell
-- Haskell - static, inferred, immutable
name = "Alice"
age = 25
-- Optional type signatures
name :: String
age :: Int
```

```rust
// Rust - static, inferred, immutable by default
let name = "Alice";
let age = 25;
// Mutable
let mut counter = 0;
```

---

## Function Definition

### Simple Function (Add Two Numbers)

```python
# Python
def add(x, y):
    return x + y

result = add(5, 3)  # 8
```

```javascript
// JavaScript - function declaration
function add(x, y) {
    return x + y;
}

// Arrow function (ES6)
const add = (x, y) => x + y;

let result = add(5, 3);  // 8
```

```c
// C
int add(int x, int y) {
    return x + y;
}

int result = add(5, 3);  // 8
```

```java
// Java
public static int add(int x, int y) {
    return x + y;
}

int result = add(5, 3);  // 8
```

```ruby
# Ruby
def add(x, y)
  x + y  # implicit return
end

result = add(5, 3)  # 8
```

```haskell
-- Haskell
add :: Int -> Int -> Int
add x y = x + y

result = add 5 3  -- 8
-- Or with operators
result = 5 `add` 3
```

```ocaml
(* OCaml *)
let add x y = x + y

let result = add 5 3  (* 8 *)
```

```racket
; Racket
(define (add x y)
  (+ x y))

(define result (add 5 3))  ; 8
```

```rust
// Rust
fn add(x: i32, y: i32) -> i32 {
    x + y  // no semicolon = return
}

let result = add(5, 3);  // 8
```

---

## Conditionals

```python
# Python
if age >= 18:
    print("Adult")
elif age >= 13:
    print("Teenager")
else:
    print("Child")
```

```javascript
// JavaScript
if (age >= 18) {
    console.log("Adult");
} else if (age >= 13) {
    console.log("Teenager");
} else {
    console.log("Child");
}

// Ternary
const status = age >= 18 ? "Adult" : "Minor";
```

```c
// C
if (age >= 18) {
    printf("Adult\n");
} else if (age >= 13) {
    printf("Teenager\n");
} else {
    printf("Child\n");
}
```

```haskell
-- Haskell - if expression (returns value)
status = if age >= 18
         then "Adult"
         else if age >= 13
              then "Teenager"
              else "Child"

-- Guards (preferred)
status
  | age >= 18 = "Adult"
  | age >= 13 = "Teenager"
  | otherwise = "Child"
```

```rust
// Rust - if is an expression
let status = if age >= 18 {
    "Adult"
} else if age >= 13 {
    "Teenager"
} else {
    "Child"
};
```

---

## Loops

### For Loop

```python
# Python - iterate over range
for i in range(5):
    print(i)  # 0, 1, 2, 3, 4

# Iterate over list
for name in ["Alice", "Bob", "Carol"]:
    print(name)
```

```javascript
// JavaScript - C-style for
for (let i = 0; i < 5; i++) {
    console.log(i);
}

// For-of (ES6)
for (const name of ["Alice", "Bob", "Carol"]) {
    console.log(name);
}
```

```c
// C - traditional for loop
for (int i = 0; i < 5; i++) {
    printf("%d\n", i);
}
```

```java
// Java - traditional
for (int i = 0; i < 5; i++) {
    System.out.println(i);
}

// Enhanced for (for-each)
for (String name : new String[]{"Alice", "Bob", "Carol"}) {
    System.out.println(name);
}
```

```ruby
# Ruby - block iteration
5.times do |i|
  puts i
end

# Iterate array
["Alice", "Bob", "Carol"].each do |name|
  puts name
end
```

```haskell
-- Haskell - no traditional loops, use recursion or higher-order functions
-- Map over list
mapM_ print [0..4]

-- List comprehension
[x * 2 | x <- [1..5]]  -- [2,4,6,8,10]
```

```rust
// Rust
for i in 0..5 {
    println!("{}", i);
}

for name in ["Alice", "Bob", "Carol"].iter() {
    println!("{}", name);
}
```

---

## Lists/Arrays

```python
# Python - dynamic lists
numbers = [1, 2, 3, 4, 5]
numbers.append(6)
first = numbers[0]
```

```javascript
// JavaScript - dynamic arrays
const numbers = [1, 2, 3, 4, 5];
numbers.push(6);
const first = numbers[0];
```

```c
// C - fixed-size arrays
int numbers[5] = {1, 2, 3, 4, 5};
int first = numbers[0];
// Cannot grow dynamically
```

```java
// Java - arrays (fixed) or ArrayList (dynamic)
int[] numbers = {1, 2, 3, 4, 5};
ArrayList<Integer> list = new ArrayList<>();
list.add(1);
```

```haskell
-- Haskell - immutable lists
numbers = [1, 2, 3, 4, 5]
newList = 0 : numbers  -- prepend: [0,1,2,3,4,5]
first = head numbers
rest = tail numbers
```

```ocaml
(* OCaml - immutable lists *)
let numbers = [1; 2; 3; 4; 5]
let newList = 0 :: numbers  (* prepend *)
let first = List.hd numbers
```

```rust
// Rust - vectors (dynamic) or arrays (fixed)
let mut numbers = vec![1, 2, 3, 4, 5];
numbers.push(6);
let first = numbers[0];
```

---

## Pattern Matching

```python
# Python - match (3.10+)
match status:
    case "active":
        print("Running")
    case "paused":
        print("Waiting")
    case _:
        print("Unknown")
```

```haskell
-- Haskell - pattern matching is core feature
describe [] = "Empty list"
describe [x] = "Single element: " ++ show x
describe (x:y:_) = "Starts with " ++ show x ++ " and " ++ show y

-- Case expression
case status of
    "active" -> "Running"
    "paused" -> "Waiting"
    _ -> "Unknown"
```

```ocaml
(* OCaml - pattern matching *)
match status with
| "active" -> "Running"
| "paused" -> "Waiting"
| _ -> "Unknown"

(* List patterns *)
match mylist with
| [] -> "Empty"
| [x] -> "Single"
| x :: xs -> "Multiple"
```

```rust
// Rust - powerful pattern matching
match status {
    "active" => println!("Running"),
    "paused" => println!("Waiting"),
    _ => println!("Unknown"),
}

// Match on enums
match result {
    Ok(value) => println!("Success: {}", value),
    Err(e) => println!("Error: {}", e),
}
```

```prolog
% Prolog - pattern matching is THE feature
describe([]) :- write('Empty list').
describe([_]) :- write('Single element').
describe([X, Y | _]) :-
    write('Starts with '), write(X),
    write(' and '), write(Y).
```

---

## Higher-Order Functions (Map/Filter/Reduce)

```python
# Python
numbers = [1, 2, 3, 4, 5]

# Map
doubled = list(map(lambda x: x * 2, numbers))
# Or list comprehension
doubled = [x * 2 for x in numbers]

# Filter
evens = list(filter(lambda x: x % 2 == 0, numbers))
evens = [x for x in numbers if x % 2 == 0]

# Reduce
from functools import reduce
sum = reduce(lambda acc, x: acc + x, numbers, 0)
```

```javascript
// JavaScript - array methods
const numbers = [1, 2, 3, 4, 5];

// Map
const doubled = numbers.map(x => x * 2);

// Filter
const evens = numbers.filter(x => x % 2 === 0);

// Reduce
const sum = numbers.reduce((acc, x) => acc + x, 0);
```

```haskell
-- Haskell - built-in higher-order functions
numbers = [1, 2, 3, 4, 5]

-- Map
doubled = map (*2) numbers
-- Or list comprehension
doubled = [x * 2 | x <- numbers]

-- Filter
evens = filter even numbers
evens = [x | x <- numbers, even x]

-- Fold (reduce)
sum = foldl (+) 0 numbers
```

```ocaml
(* OCaml *)
let numbers = [1; 2; 3; 4; 5]

(* Map *)
let doubled = List.map (fun x -> x * 2) numbers

(* Filter *)
let evens = List.filter (fun x -> x mod 2 = 0) numbers

(* Fold (reduce) *)
let sum = List.fold_left (+) 0 numbers
```

```ruby
# Ruby - enumerable methods
numbers = [1, 2, 3, 4, 5]

# Map
doubled = numbers.map { |x| x * 2 }
doubled = numbers.map(&:*2)  # with Symbol#to_proc

# Filter
evens = numbers.filter { |x| x.even? }

# Reduce
sum = numbers.reduce(0) { |acc, x| acc + x }
sum = numbers.reduce(:+)
```

```rust
// Rust - iterator methods
let numbers = vec![1, 2, 3, 4, 5];

// Map
let doubled: Vec<i32> = numbers.iter().map(|x| x * 2).collect();

// Filter
let evens: Vec<i32> = numbers.iter()
    .filter(|x| *x % 2 == 0)
    .copied()
    .collect();

// Fold (reduce)
let sum: i32 = numbers.iter().fold(0, |acc, x| acc + x);
```

---

## Object-Oriented Features

### Classes

```python
# Python
class Dog:
    def __init__(self, name, age):
        self.name = name
        self.age = age

    def bark(self):
        return f"{self.name} says woof!"

dog = Dog("Buddy", 3)
print(dog.bark())
```

```javascript
// JavaScript
class Dog {
    constructor(name, age) {
        this.name = name;
        this.age = age;
    }

    bark() {
        return `${this.name} says woof!`;
    }
}

const dog = new Dog("Buddy", 3);
console.log(dog.bark());
```

```ruby
# Ruby
class Dog
  attr_accessor :name, :age

  def initialize(name, age)
    @name = name
    @age = age
  end

  def bark
    "#{@name} says woof!"
  end
end

dog = Dog.new("Buddy", 3)
puts dog.bark
```

```java
// Java
public class Dog {
    private String name;
    private int age;

    public Dog(String name, int age) {
        this.name = name;
        this.age = age;
    }

    public String bark() {
        return name + " says woof!";
    }
}

Dog dog = new Dog("Buddy", 3);
System.out.println(dog.bark());
```

---

## Error Handling

```python
# Python - exceptions
try:
    result = 10 / 0
except ZeroDivisionError as e:
    print(f"Error: {e}")
finally:
    print("Cleanup")
```

```javascript
// JavaScript - try/catch
try {
    const result = riskyOperation();
} catch (error) {
    console.error(`Error: ${error}`);
} finally {
    console.log("Cleanup");
}
```

```haskell
-- Haskell - Maybe and Either types (no exceptions)
safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

-- Pattern match on result
case safeDivide 10 0 of
    Nothing -> putStrLn "Cannot divide by zero"
    Just result -> print result
```

```rust
// Rust - Result type (no exceptions)
fn safe_divide(x: f64, y: f64) -> Result<f64, String> {
    if y == 0.0 {
        Err("Cannot divide by zero".to_string())
    } else {
        Ok(x / y)
    }
}

// Pattern match on result
match safe_divide(10.0, 0.0) {
    Ok(result) => println!("Result: {}", result),
    Err(e) => println!("Error: {}", e),
}
```

---

## Key Paradigm Differences

| Feature | Imperative | OOP | Functional | Logic |
|---------|-----------|-----|------------|-------|
| Primary unit | Statement | Object | Function | Rule |
| State | Mutable | Encapsulated | Immutable | Logical facts |
| Control flow | Sequential | Method calls | Composition | Unification |
| Data | Variables | Objects | Values | Facts |
| Example langs | C, Python | Java, Ruby | Haskell, OCaml | Prolog |

## When to Use Each Language

- **Python**: General purpose, scripting, data science, rapid prototyping
- **JavaScript**: Web development, Node.js servers, full-stack
- **C**: Systems programming, embedded, performance-critical code
- **Java**: Enterprise applications, Android, large team projects
- **Ruby**: Web development (Rails), scripting, DSLs
- **Haskell**: Pure functional programming, type-driven development, math
- **OCaml**: Functional programming with pragmatism, compilers
- **Racket**: Language experimentation, education, DSLs
- **Prolog**: AI, logic puzzles, symbolic reasoning
- **Rust**: Systems programming with safety, performance-critical, concurrency

## Conclusion

Each language represents different design philosophies:
- **C**: Close to hardware, explicit control
- **Python**: Readability, simplicity, batteries included
- **Haskell**: Purity, type safety, mathematical elegance
- **Prolog**: Logic and reasoning
- **Rust**: Safety without garbage collection

Understanding these differences makes you a better programmer in ANY language!
