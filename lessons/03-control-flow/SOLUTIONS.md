# Lesson 3: Control Flow - Solutions

This guide provides solutions for the control flow exercises. Each solution includes:
- Multiple language implementations
- Different approaches (imperative, functional, recursive)
- Explanations of key concepts
- Common mistakes to avoid

## Exercise 1: Even or Odd

### Python Solution
```python
# Approach 1: if/else statement
def even_or_odd_if(n):
    if n % 2 == 0:
        return "Even"
    else:
        return "Odd"

# Approach 2: Ternary operator
def even_or_odd_ternary(n):
    return "Even" if n % 2 == 0 else "Odd"

# Tests
print(even_or_odd_if(4))      # Even
print(even_or_odd_ternary(7))  # Odd
```

### JavaScript Solution
```javascript
// Approach 1: if/else
function evenOrOdd(n) {
    if (n % 2 === 0) {
        return "Even";
    } else {
        return "Odd";
    }
}

// Approach 2: Ternary
const evenOrOddTernary = (n) => n % 2 === 0 ? "Even" : "Odd";

console.log(evenOrOdd(4));          // Even
console.log(evenOrOddTernary(7));   // Odd
```

### Haskell Solution (Pattern Matching)
```haskell
-- Using guards
evenOrOdd :: Int -> String
evenOrOdd n
    | n `mod` 2 == 0 = "Even"
    | otherwise      = "Odd"

-- Using pattern matching on remainder
evenOrOddPattern :: Int -> String
evenOrOddPattern n = case n `mod` 2 of
    0 -> "Even"
    _ -> "Odd"

main = do
    print $ evenOrOdd 4  -- "Even"
    print $ evenOrOdd 7  -- "Odd"
```

### Rust Solution (Pattern Matching)
```rust
fn even_or_odd(n: i32) -> &'static str {
    match n % 2 {
        0 => "Even",
        _ => "Odd",
    }
}

fn main() {
    println!("{}", even_or_odd(4));  // Even
    println!("{}", even_or_odd(7));  // Odd
}
```

**Key Concepts:**
- Modulo operator (`%`) returns remainder after division
- Even numbers have remainder 0 when divided by 2
- Ternary operator is concise for simple conditionals
- Pattern matching is elegant for discrete values

**Common Mistakes:**
- Using `n / 2 == 0` instead of `n % 2 == 0` (wrong operator!)
- Forgetting to handle negative numbers (they work fine with modulo)

---

## Exercise 2: Grade Calculator

### Python Solution
```python
def get_grade(score):
    """Convert numeric score to letter grade."""
    if score >= 90:
        return 'A'
    elif score >= 80:
        return 'B'
    elif score >= 70:
        return 'C'
    elif score >= 60:
        return 'D'
    else:
        return 'F'

# Tests
print(get_grade(95))  # A
print(get_grade(85))  # B
print(get_grade(72))  # C
print(get_grade(55))  # F
```

### JavaScript Solution
```javascript
function getGrade(score) {
    if (score >= 90) return 'A';
    if (score >= 80) return 'B';
    if (score >= 70) return 'C';
    if (score >= 60) return 'D';
    return 'F';
}

// Alternative: using a switch with ranges (less elegant)
function getGradeSwitch(score) {
    const tens = Math.floor(score / 10);
    switch (tens) {
        case 10:
        case 9: return 'A';
        case 8: return 'B';
        case 7: return 'C';
        case 6: return 'D';
        default: return 'F';
    }
}
```

### Haskell Solution (Guards)
```haskell
getGrade :: Int -> Char
getGrade score
    | score >= 90 = 'A'
    | score >= 80 = 'B'
    | score >= 70 = 'C'
    | score >= 60 = 'D'
    | otherwise   = 'F'

main = do
    print $ getGrade 95  -- 'A'
    print $ getGrade 72  -- 'C'
```

### Rust Solution (Match with Guards)
```rust
fn get_grade(score: i32) -> char {
    match score {
        90..=100 => 'A',
        80..=89  => 'B',
        70..=79  => 'C',
        60..=69  => 'D',
        _        => 'F',
    }
}

// Alternative with guards
fn get_grade_guards(score: i32) -> char {
    match score {
        s if s >= 90 => 'A',
        s if s >= 80 => 'B',
        s if s >= 70 => 'C',
        s if s >= 60 => 'D',
        _ => 'F',
    }
}
```

**Key Concepts:**
- Use if/elif for sequential checks
- Check highest value first (early return)
- Guards in Haskell are very readable
- Rust's range patterns (`90..=100`) are elegant

**Common Mistakes:**
- Checking in wrong order (low to high instead of high to low)
- Using `>=` and `<` inconsistently causing gaps or overlaps
- Not handling edge cases (exactly 90, 80, etc.)

---

## Exercise 3: Sum of Numbers

### Python Solutions
```python
# Approach 1: For loop (imperative)
def sum_n_for(n):
    total = 0
    for i in range(1, n + 1):
        total += i
    return total

# Approach 2: While loop
def sum_n_while(n):
    total = 0
    i = 1
    while i <= n:
        total += i
        i += 1
    return total

# Approach 3: Built-in sum (functional)
def sum_n_builtin(n):
    return sum(range(1, n + 1))

# Approach 4: List comprehension
def sum_n_comprehension(n):
    return sum([i for i in range(1, n + 1)])

# Approach 5: Mathematical formula (most efficient!)
def sum_n_formula(n):
    return n * (n + 1) // 2

# Tests
print(sum_n_for(5))       # 15
print(sum_n_while(10))    # 55
print(sum_n_formula(100)) # 5050
```

### Haskell Solutions
```haskell
-- Approach 1: Recursion
sumN :: Int -> Int
sumN 0 = 0
sumN n = n + sumN (n - 1)

-- Approach 2: Built-in sum with range
sumNRange :: Int -> Int
sumNRange n = sum [1..n]

-- Approach 3: Fold
sumNFold :: Int -> Int
sumNFold n = foldl (+) 0 [1..n]

-- Approach 4: Formula
sumNFormula :: Int -> Int
sumNFormula n = n * (n + 1) `div` 2

main = do
    print $ sumN 5          -- 15
    print $ sumNRange 10    -- 55
    print $ sumNFormula 100 -- 5050
```

### C Solution
```c
#include <stdio.h>

// For loop approach
int sum_n_for(int n) {
    int total = 0;
    for (int i = 1; i <= n; i++) {
        total += i;
    }
    return total;
}

// While loop approach
int sum_n_while(int n) {
    int total = 0;
    int i = 1;
    while (i <= n) {
        total += i;
        i++;
    }
    return total;
}

// Formula approach
int sum_n_formula(int n) {
    return n * (n + 1) / 2;
}

int main() {
    printf("%d\n", sum_n_for(5));       // 15
    printf("%d\n", sum_n_while(10));    // 55
    printf("%d\n", sum_n_formula(100)); // 5050
    return 0;
}
```

### Racket Solution (Recursive)
```racket
#lang racket

; Recursive approach
(define (sum-n n)
  (if (= n 0)
      0
      (+ n (sum-n (- n 1)))))

; Tail-recursive (more efficient)
(define (sum-n-tail n)
  (define (helper n acc)
    (if (= n 0)
        acc
        (helper (- n 1) (+ acc n))))
  (helper n 0))

; Using built-in
(define (sum-n-builtin n)
  (apply + (range 1 (+ n 1))))

(displayln (sum-n 5))          ; 15
(displayln (sum-n-tail 10))    ; 55
(displayln (sum-n-builtin 100)); 5050
```

**Key Concepts:**
- For loops are most common in imperative languages
- Recursion is natural in functional languages
- Mathematical formula is O(1) vs O(n) for loops
- Tail recursion prevents stack overflow

**Performance Comparison:**
- Formula: O(1) - instant
- Loop: O(n) - linear time
- Recursion: O(n) time, O(n) space (without tail call optimization)

**Common Mistakes:**
- `range(1, n)` instead of `range(1, n+1)` (off by one!)
- Integer overflow for large n (use long/int64)
- Stack overflow with deep recursion

---

## Exercise 4: Multiplication Table

### Python Solutions
```python
# Approach 1: For loop
def mult_table_for(n):
    for i in range(1, 11):
        print(f"{n} x {i} = {n * i}")

# Approach 2: List comprehension (functional)
def mult_table_comprehension(n):
    results = [f"{n} x {i} = {n * i}" for i in range(1, 11)]
    for line in results:
        print(line)

# Approach 3: Using map
def mult_table_map(n):
    lines = map(lambda i: f"{n} x {i} = {n * i}", range(1, 11))
    for line in lines:
        print(line)

mult_table_for(5)
```

### JavaScript Solution
```javascript
function multTable(n) {
    for (let i = 1; i <= 10; i++) {
        console.log(`${n} x ${i} = ${n * i}`);
    }
}

// Functional approach
function multTableFunctional(n) {
    [...Array(10).keys()].map(i => i + 1)
        .forEach(i => console.log(`${n} x ${i} = ${n * i}`));
}

multTable(5);
```

### Haskell Solution
```haskell
-- Using list comprehension
multTable :: Int -> IO ()
multTable n = mapM_ putStrLn [show n ++ " x " ++ show i ++ " = " ++ show (n * i) | i <- [1..10]]

-- Using map
multTableMap :: Int -> IO ()
multTableMap n = mapM_ putStrLn $ map formatLine [1..10]
  where formatLine i = show n ++ " x " ++ show i ++ " = " ++ show (n * i)

main = multTable 5
```

### Rust Solution
```rust
fn mult_table(n: i32) {
    for i in 1..=10 {
        println!("{} x {} = {}", n, i, n * i);
    }
}

// Functional approach
fn mult_table_functional(n: i32) {
    (1..=10).for_each(|i| {
        println!("{} x {} = {}", n, i, n * i);
    });
}

fn main() {
    mult_table(5);
}
```

**Key Concepts:**
- String formatting/interpolation varies by language
- Functional approaches use map/forEach
- Range syntax differs (1..11 vs 1..=10 vs [1..10])

---

## Exercise 5: FizzBuzz

### Python Solution (All Parts)
```python
# Part A: Classic FizzBuzz
def fizzbuzz():
    for i in range(1, 101):
        if i % 15 == 0:
            print("FizzBuzz")
        elif i % 3 == 0:
            print("Fizz")
        elif i % 5 == 0:
            print("Buzz")
        else:
            print(i)

# Alternative: build string
def fizzbuzz_string():
    for i in range(1, 101):
        output = ""
        if i % 3 == 0:
            output += "Fizz"
        if i % 5 == 0:
            output += "Buzz"
        print(output or i)

# Part C: Triple FizzBuzz (extensible)
def fizzbuzz_triple():
    for i in range(1, 101):
        output = ""
        if i % 3 == 0:
            output += "Fizz"
        if i % 5 == 0:
            output += "Buzz"
        if i % 7 == 0:
            output += "Boom"
        print(output or i)

# Most extensible approach
def fizzbuzz_extensible():
    rules = [(3, "Fizz"), (5, "Buzz"), (7, "Boom")]
    for i in range(1, 101):
        output = "".join(word for divisor, word in rules if i % divisor == 0)
        print(output or i)
```

### JavaScript Solution
```javascript
// Classic approach
function fizzBuzz() {
    for (let i = 1; i <= 100; i++) {
        if (i % 15 === 0) console.log("FizzBuzz");
        else if (i % 3 === 0) console.log("Fizz");
        else if (i % 5 === 0) console.log("Buzz");
        else console.log(i);
    }
}

// String building (better for multiple conditions)
function fizzBuzzString() {
    for (let i = 1; i <= 100; i++) {
        let output = "";
        if (i % 3 === 0) output += "Fizz";
        if (i % 5 === 0) output += "Buzz";
        if (i % 7 === 0) output += "Boom";
        console.log(output || i);
    }
}

// Functional approach
function fizzBuzzFunctional() {
    Array.from({length: 100}, (_, i) => i + 1).forEach(i => {
        const output = (i % 3 === 0 ? "Fizz" : "") +
                      (i % 5 === 0 ? "Buzz" : "") +
                      (i % 7 === 0 ? "Boom" : "");
        console.log(output || i);
    });
}
```

### Haskell Solution
```haskell
fizzBuzz :: Int -> String
fizzBuzz n
    | n `mod` 15 == 0 = "FizzBuzz"
    | n `mod` 3 == 0  = "Fizz"
    | n `mod` 5 == 0  = "Buzz"
    | otherwise       = show n

-- Better: build string
fizzBuzzString :: Int -> String
fizzBuzzString n =
    let fizz = if n `mod` 3 == 0 then "Fizz" else ""
        buzz = if n `mod` 5 == 0 then "Buzz" else ""
        boom = if n `mod` 7 == 0 then "Boom" else ""
        result = fizz ++ buzz ++ boom
    in if null result then show n else result

main = mapM_ (putStrLn . fizzBuzzString) [1..100]
```

**Key Concepts:**
- Check combined condition first (15 before 3 and 5)
- String building is more extensible
- `output or i` uses Python's truthiness (empty string is falsy)
- Functional approach separates logic from iteration

**Common Mistakes:**
- Checking 3 and 5 before 15 (prints "Fizz" for 15)
- Using else-if when multiple conditions should apply

**Scalability:** String building approach is best for many conditions

---

## Exercise 6: Collatz Sequence

### Python Solutions
```python
# Approach 1: While loop with printing
def collatz_steps_print(n):
    steps = 0
    print(f"Starting from {n}:")
    while n != 1:
        if n % 2 == 0:
            n = n // 2
        else:
            n = 3 * n + 1
        steps += 1
        print(n, end=" → " if n != 1 else "\n")
    print(f"Steps: {steps}")
    return steps

# Approach 2: Just count steps
def collatz_steps(n):
    steps = 0
    while n != 1:
        n = n // 2 if n % 2 == 0 else 3 * n + 1
        steps += 1
    return steps

# Challenge: Find max steps in range
def max_collatz_steps(limit):
    max_steps = 0
    max_n = 0
    for n in range(1, limit + 1):
        steps = collatz_steps(n)
        if steps > max_steps:
            max_steps = steps
            max_n = n
    return max_n, max_steps

# Tests
print(collatz_steps(10))   # 6
print(collatz_steps(19))   # 20
print(collatz_steps(27))   # 111
print(max_collatz_steps(100))  # (97, 118)
```

### Haskell Solution (Recursive)
```haskell
-- Generate sequence as list
collatzSequence :: Int -> [Int]
collatzSequence 1 = [1]
collatzSequence n
    | even n    = n : collatzSequence (n `div` 2)
    | otherwise = n : collatzSequence (3 * n + 1)

-- Count steps
collatzSteps :: Int -> Int
collatzSteps n = length (collatzSequence n) - 1

-- Find maximum in range
maxCollatzSteps :: Int -> (Int, Int)
maxCollatzSteps limit =
    let steps = [(n, collatzSteps n) | n <- [1..limit]]
    in maximumBy (comparing snd) steps

main = do
    print $ collatzSteps 10  -- 6
    print $ collatzSteps 27  -- 111
    print $ maxCollatzSteps 100  -- (97, 118)
```

### Racket Solution
```racket
#lang racket

(define (collatz-steps n)
  (if (= n 1)
      0
      (+ 1 (if (even? n)
               (collatz-steps (/ n 2))
               (collatz-steps (+ (* 3 n) 1))))))

(define (collatz-sequence n)
  (if (= n 1)
      '(1)
      (cons n (if (even? n)
                  (collatz-sequence (/ n 2))
                  (collatz-sequence (+ (* 3 n) 1))))))

(displayln (collatz-steps 10))  ; 6
(displayln (collatz-sequence 10))  ; (10 5 16 8 4 2 1)
```

**Key Concepts:**
- While loop is natural for this problem
- Recursion can cause stack overflow for large n
- Integer division (`//` or `div`) is important
- Memoization can optimize repeated calculations

**Warning:** Collatz conjecture is unproven - sequence might not always reach 1!

---

## Exercise 8: Prime Number Checker

### Python Solutions
```python
import math

# Approach 1: Simple loop
def is_prime_simple(n):
    if n < 2:
        return False
    if n == 2:
        return True
    if n % 2 == 0:
        return False

    for i in range(3, int(math.sqrt(n)) + 1, 2):
        if n % i == 0:
            return False
    return True

# Approach 2: All at once
def is_prime_oneliner(n):
    return n > 1 and all(n % i != 0 for i in range(2, int(math.sqrt(n)) + 1))

# Get all primes up to n
def get_primes(n):
    return [i for i in range(2, n + 1) if is_prime_simple(i)]

# Tests
print(is_prime_simple(2))   # True
print(is_prime_simple(7))   # True
print(is_prime_simple(9))   # False
print(is_prime_simple(17))  # True
print(get_primes(20))       # [2, 3, 5, 7, 11, 13, 17, 19]
```

### Haskell Solution
```haskell
import Data.List (find)

-- Using list comprehension
isPrime :: Int -> Bool
isPrime n
    | n < 2     = False
    | n == 2    = True
    | even n    = False
    | otherwise = null [x | x <- [3,5..sqrtN], n `mod` x == 0]
  where sqrtN = floor $ sqrt $ fromIntegral n

-- Using all
isPrimeAll :: Int -> Bool
isPrimeAll n
    | n < 2  = False
    | n == 2 = True
    | otherwise = all (\x -> n `mod` x /= 0) [2..sqrtN]
  where sqrtN = floor $ sqrt $ fromIntegral n

-- Get all primes up to n
primes :: Int -> [Int]
primes n = filter isPrime [2..n]

main = do
    print $ isPrime 17  -- True
    print $ isPrime 20  -- False
    print $ primes 20   -- [2,3,5,7,11,13,17,19]
```

### Rust Solution
```rust
fn is_prime(n: u32) -> bool {
    if n < 2 {
        return false;
    }
    if n == 2 {
        return true;
    }
    if n % 2 == 0 {
        return false;
    }

    let sqrt_n = (n as f64).sqrt() as u32;
    for i in (3..=sqrt_n).step_by(2) {
        if n % i == 0 {
            return false;
        }
    }
    true
}

fn get_primes(n: u32) -> Vec<u32> {
    (2..=n).filter(|&i| is_prime(i)).collect()
}

fn main() {
    println!("{}", is_prime(17));     // true
    println!("{:?}", get_primes(20)); // [2, 3, 5, 7, 11, 13, 17, 19]
}
```

**Key Concepts:**
- Only check up to sqrt(n) - if n has a divisor > sqrt(n), it must also have one < sqrt(n)
- Can skip even numbers after checking 2
- Early returns improve performance

**Optimization Tips:**
- Check 2 separately, then only odd numbers
- For many checks, use Sieve of Eratosthenes
- Trial division is fine for small numbers

---

## Summary of Key Patterns

### 1. **Choosing Loop Types**
- **For loop**: Known number of iterations
- **While loop**: Unknown iterations, condition-based
- **Recursion**: Natural for divide-and-conquer, functional style

### 2. **Conditional Patterns**
- **Simple if/else**: Two branches
- **if/elif/else chain**: Multiple mutually exclusive conditions
- **Nested ifs**: Multiple independent conditions
- **Guards**: Functional style, very readable
- **Pattern matching**: Best for discrete values

### 3. **Common Idioms**
- **Accumulator pattern**: Sum, product, count
- **Flag pattern**: Boolean to track state
- **Early return**: Exit function as soon as answer known
- **Build string**: Concatenate parts, output at end

### 4. **Performance Considerations**
- Mathematical formula > loop > recursion (for simple sums)
- Check most common case first
- Use appropriate data structures
- Consider memoization for repeated calculations

---

## Testing Your Solutions

For each solution, test:
1. **Normal cases**: Typical inputs
2. **Edge cases**: 0, 1, negative numbers
3. **Boundaries**: Minimum/maximum values
4. **Invalid input**: What should happen?

Example test cases for `is_prime`:
```python
assert is_prime(2) == True    # Smallest prime
assert is_prime(3) == True    # Small prime
assert is_prime(4) == False   # Even composite
assert is_prime(9) == False   # Odd composite (3*3)
assert is_prime(17) == True   # Larger prime
assert is_prime(1) == False   # Edge case
assert is_prime(0) == False   # Edge case
```

---

Remember: These are reference solutions. Your solution might be different and still correct! The goal is to understand the concepts, not to match these exactly.
