# Lesson 3: Control Flow - Exercises

## Instructions

Complete these exercises to practice control flow concepts across Python, C++, and Haskell. For each exercise, try implementing in at least 2-3 languages to appreciate the different approaches.

---

## Exercise 1: Even or Odd (Warmup)

**Difficulty:** Easy

Write a program that:
- Takes a number as input (or use a hard-coded value)
- Prints "Even" if the number is even
- Prints "Odd" if the number is odd

**Implement in all three languages:**

**Python:**
```python
def is_even_or_odd(n):
    if n % 2 == 0:
        return "Even"
    else:
        return "Odd"
```

**C++:**
```cpp
std::string isEvenOrOdd(int n) {
    if (n % 2 == 0) {
        return "Even";
    } else {
        return "Odd";
    }
}
```

**Haskell:**
```haskell
isEvenOrOdd :: Int -> String
isEvenOrOdd n
  | even n    = "Even"
  | otherwise = "Odd"
```

**Bonus:** Implement using:
- Ternary operator (Python, C++)
- Pattern matching on remainders

---

## Exercise 2: Grade Calculator

**Difficulty:** Easy

Write a program that converts a numeric score (0-100) to a letter grade:
- 90-100: A
- 80-89: B
- 70-79: C
- 60-69: D
- Below 60: F

**Implementation requirements:**

**Python:** Use if/elif/else
```python
def letter_grade(score):
    if score >= 90:
        return "A"
    elif score >= 80:
        return "B"
    # ... complete this
```

**C++:** Use if/else if/else
```cpp
char letterGrade(int score) {
    if (score >= 90) {
        return 'A';
    } else if (score >= 80) {
        return 'B';
    }
    // ... complete this
}
```

**Haskell:** Use guards
```haskell
letterGrade :: Int -> Char
letterGrade score
  | score >= 90 = 'A'
  | score >= 80 = 'B'
  -- ... complete this
```

**Test cases:**
- Score 95 → A
- Score 85 → B
- Score 72 → C
- Score 55 → F

**Discussion:** Which approach feels most natural? Which is most concise?

---

## Exercise 3: Sum of Numbers

**Difficulty:** Easy to Medium

Write a program that computes the sum of numbers from 1 to N (e.g., N=10: 1+2+3+...+10 = 55).

**Implementation requirements - try all approaches:**

**Python - For loop:**
```python
def sum_to_n_loop(n):
    total = 0
    for i in range(1, n + 1):
        total += i
    return total
```

**Python - While loop:**
```python
def sum_to_n_while(n):
    total = 0
    i = 1
    while i <= n:
        total += i
        i += 1
    return total
```

**C++ - For loop:**
```cpp
int sumToN(int n) {
    int total = 0;
    for (int i = 1; i <= n; i++) {
        total += i;
    }
    return total;
}
```

**Haskell - Recursion:**
```haskell
sumToN :: Int -> Int
sumToN n
  | n <= 0    = 0
  | otherwise = n + sumToN (n - 1)
```

**Haskell - List and sum:**
```haskell
sumToN :: Int -> Int
sumToN n = sum [1..n]
```

**Test cases:**
- N=5 → 15
- N=10 → 55
- N=100 → 5050

**Discussion:**
- Which implementation is clearest?
- Most efficient?
- Which best expresses the mathematical concept?

---

## Exercise 4: Multiplication Table

**Difficulty:** Medium

Write a program that prints a multiplication table for a given number N.

Example for N=5:
```
5 x 1 = 5
5 x 2 = 10
5 x 3 = 15
...
5 x 10 = 50
```

**Python:**
```python
def multiplication_table(n):
    for i in range(1, 11):
        print(f"{n} x {i} = {n * i}")
```

**C++:**
```cpp
void multiplicationTable(int n) {
    for (int i = 1; i <= 10; i++) {
        std::cout << n << " x " << i << " = " << (n * i) << std::endl;
    }
}
```

**Haskell:**
```haskell
multiplicationTable :: Int -> IO ()
multiplicationTable n = mapM_ printRow [1..10]
  where printRow i = putStrLn $ show n ++ " x " ++ show i ++ " = " ++ show (n * i)
```

**Bonus:** Create a full 10x10 multiplication table grid.

---

## Exercise 5: FizzBuzz Variations

**Difficulty:** Medium

### Part A: Classic FizzBuzz

Implement FizzBuzz for numbers 1-100:
- Multiples of 3: print "Fizz"
- Multiples of 5: print "Buzz"
- Multiples of both: print "FizzBuzz"
- Otherwise: print the number

**Implement in all three languages.**

### Part B: Custom FizzBuzz

Modify FizzBuzz to use different numbers:
- Multiples of 7: print "Boom"
- Multiples of 11: print "Bang"
- Multiples of both 7 and 11: print "BoomBang"

### Part C: Triple FizzBuzz

Add a third condition:
- Multiples of 3: "Fizz"
- Multiples of 5: "Buzz"
- Multiples of 7: "Boom"
- Combine for multiples (e.g., 15 → "FizzBuzz", 21 → "FizzBoom", 35 → "BuzzBoom", 105 → "FizzBuzzBoom")

**Challenge:** How does your implementation scale? Is it easy to add more conditions?

**Hint for scalable solution:** Consider building the output string incrementally or using a list of (divisor, word) pairs.

---

## Exercise 6: Collatz Sequence

**Difficulty:** Medium

The Collatz conjecture says that starting from any positive integer:
- If n is even: divide by 2
- If n is odd: multiply by 3 and add 1
- Repeat until n = 1

Write a program that:
1. Prints the Collatz sequence for a starting number
2. Counts how many steps it takes to reach 1

Example for N=10:
```
10 → 5 → 16 → 8 → 4 → 2 → 1
Steps: 6
```

**Python - While loop:**
```python
def collatz_sequence(n):
    steps = 0
    while n != 1:
        if n % 2 == 0:
            n = n // 2
        else:
            n = 3 * n + 1
        print(n, end=' ')
        steps += 1
    return steps
```

**C++ - While loop:**
```cpp
int collatzSequence(int n) {
    int steps = 0;
    while (n != 1) {
        if (n % 2 == 0) {
            n = n / 2;
        } else {
            n = 3 * n + 1;
        }
        std::cout << n << " ";
        steps++;
    }
    return steps;
}
```

**Haskell - Recursion:**
```haskell
collatzSequence :: Int -> [Int]
collatzSequence 1 = [1]
collatzSequence n
  | even n    = n : collatzSequence (n `div` 2)
  | otherwise = n : collatzSequence (3 * n + 1)

collatzSteps :: Int -> Int
collatzSteps n = length (collatzSequence n) - 1
```

**Test cases:**
- N=10 → 6 steps
- N=19 → 20 steps
- N=27 → 111 steps

**Challenge:** Find the number between 1 and 100 that takes the most steps.

---

## Exercise 7: Prime Number Checker

**Difficulty:** Medium

Write a program that determines if a number is prime.

A prime number is only divisible by 1 and itself.

**Algorithm:**
- Check if n < 2 (not prime)
- Check if n == 2 (prime)
- Check if n is divisible by any number from 2 to sqrt(n)

**Python:**
```python
def is_prime(n):
    if n < 2:
        return False
    if n == 2:
        return True
    if n % 2 == 0:
        return False
    # Check odd divisors up to sqrt(n)
    i = 3
    while i * i <= n:
        if n % i == 0:
            return False
        i += 2
    return True
```

**C++:**
```cpp
bool isPrime(int n) {
    if (n < 2) return false;
    if (n == 2) return true;
    if (n % 2 == 0) return false;

    for (int i = 3; i * i <= n; i += 2) {
        if (n % i == 0) return false;
    }
    return true;
}
```

**Haskell:**
```haskell
isPrime :: Int -> Bool
isPrime n
  | n < 2     = False
  | n == 2    = True
  | even n    = False
  | otherwise = not (any divides [3, 5..limit])
  where
    limit = floor (sqrt (fromIntegral n))
    divides d = n `mod` d == 0
```

**Test cases:**
- 2 → Prime
- 7 → Prime
- 9 → Not prime
- 17 → Prime
- 20 → Not prime

**Challenge:** Print all prime numbers from 1 to 100

---

## Exercise 8: Pattern Matching Practice

**Difficulty:** Medium

### Part A: Playing Cards (Haskell)

Write a function that describes a playing card using pattern matching:

```haskell
data Suit = Hearts | Diamonds | Clubs | Spades
data Rank = Ace | King | Queen | Jack | Number Int

describeCard :: Rank -> Suit -> String
describeCard Ace suit = "Ace of " ++ show suit
describeCard King suit = "King of " ++ show suit
-- ... complete this
```

### Part B: List Patterns (Haskell)

Write a function that describes a list based on its structure:

```haskell
describeList :: [Int] -> String
describeList [] = "Empty"
describeList [x] = "Singleton: " ++ show x
describeList [x, y] = "Pair: " ++ show x ++ " and " ++ show y
describeList (x:y:z:_) = "List starting with " ++ show x ++ ", " ++ show y ++ ", " ++ show z
```

### Part C: Switch Equivalents (Python/C++)

Implement the playing card example using if/else or switch in Python and C++.

**Discussion:** When is pattern matching more elegant than if/else?

---

## Exercise 9: Number Classification

**Difficulty:** Medium

Write a program that classifies a number as:
- "Negative" if < 0
- "Zero" if == 0
- "Small positive" if 1-10
- "Medium positive" if 11-100
- "Large positive" if > 100

**Implement in all three languages using appropriate control structures.**

**Extra challenge:** In Haskell, use both guards and case expressions.

---

## Exercise 10: Factorial

**Difficulty:** Medium

Calculate factorial (n! = 1 × 2 × 3 × ... × n).

**Python - Iterative:**
```python
def factorial_iterative(n):
    result = 1
    for i in range(1, n + 1):
        result *= i
    return result
```

**C++ - Iterative:**
```cpp
long long factorial(int n) {
    long long result = 1;
    for (int i = 1; i <= n; i++) {
        result *= i;
    }
    return result;
}
```

**Haskell - Recursive:**
```haskell
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

**Haskell - Using fold:**
```haskell
factorial :: Int -> Int
factorial n = product [1..n]
```

**Test cases:**
- 0! = 1
- 5! = 120
- 10! = 3,628,800

**Discussion:** Compare the iterative and recursive approaches.

---

## Exercise 11: Truth Table Generator

**Difficulty:** Hard

Write a program that generates truth tables for boolean operations.

### Part A: Basic AND Operation

Generate a truth table for AND:

```
p     | q     | p AND q
------|-------|--------
false | false | false
false | true  | false
true  | false | false
true  | true  | true
```

**Implementation tip:** Use nested loops to iterate all combinations of true/false.

### Part B: Multiple Operations

Generate truth tables for: AND, OR, NOT, XOR, IMPLIES

**Python:**
```python
def truth_table_and():
    print("p     | q     | p AND q")
    print("------|-------|--------")
    for p in [False, True]:
        for q in [False, True]:
            result = p and q
            print(f"{str(p):5} | {str(q):5} | {result}")
```

**Challenge:** Generate a truth table for: (p AND q) OR (NOT p AND r)

---

## Exercise 12: Pascal's Triangle

**Difficulty:** Hard

Generate Pascal's Triangle up to N rows.

Example for N=5:
```
    1
   1 1
  1 2 1
 1 3 3 1
1 4 6 4 1
```

**Hint:** Each element is the sum of the two elements above it.

**Python - Iterative:**
```python
def pascals_triangle(n):
    triangle = [[1]]
    for i in range(1, n):
        row = [1]
        for j in range(1, i):
            row.append(triangle[i-1][j-1] + triangle[i-1][j])
        row.append(1)
        triangle.append(row)
    return triangle
```

**Haskell - Functional:**
```haskell
-- Generate next row from current row
nextRow :: [Int] -> [Int]
nextRow row = zipWith (+) (0:row) (row ++ [0])

-- Generate n rows
pascalsTriangle :: Int -> [[Int]]
pascalsTriangle n = take n (iterate nextRow [1])
```

**Bonus:** Format the output as a proper triangle with spacing.

---

## Challenge Projects

### Challenge 1: Roman Numeral Converter

Convert integers to Roman numerals (1-3999).

Rules:
- I=1, V=5, X=10, L=50, C=100, D=500, M=1000
- Subtractive notation: IV=4, IX=9, XL=40, XC=90, CD=400, CM=900

Example: 1994 → MCMXCIV

**Implement using conditionals and loops.**

### Challenge 2: Leap Year Calculator

Determine if a year is a leap year:
- Divisible by 4: leap year
- Exception: divisible by 100: not a leap year
- Exception to exception: divisible by 400: leap year

Examples:
- 2000 → leap year (divisible by 400)
- 1900 → not a leap year (divisible by 100 but not 400)
- 2024 → leap year (divisible by 4)

**Implement in all three languages.**

### Challenge 3: Guess the Number Game

Create an interactive number guessing game:
1. Computer picks random number 1-100
2. User guesses
3. Computer responds "Too high", "Too low", or "Correct!"
4. Count guesses

**Bonus:** Limit to 7 attempts, use binary search strategy hint.

---

## Reflection Questions

After completing the exercises, consider:

1. **How did control flow differ between Python, C++, and Haskell?**

2. **When did pattern matching (Haskell) feel more natural than if/else?**

3. **Which language made recursion easiest? Why?**

4. **Did any language's approach to truthiness surprise you?**

5. **For the same problem, which language felt most natural? Which was most concise?**

6. **When would you prefer imperative loops over functional recursion?**

7. **How does Python's indentation compare to C++'s braces for readability?**

---

## Going Further

### Additional Challenges

- Implement Fibonacci sequence using different control structures
- Create a simple calculator with multiple operations
- Write a word counter that handles various text inputs
- Implement the Sieve of Eratosthenes for finding primes

### Study Topics

- Tail recursion optimization in Haskell
- Loop unrolling and optimization in C++
- Python's `itertools` for functional-style iteration
- Comparing performance of iterative vs recursive solutions

---

## Solutions

Solutions for all exercises will be provided separately. Try to complete exercises on your own first!

**Remember:** The goal is not just to make it work, but to understand how each language approaches control flow differently.
