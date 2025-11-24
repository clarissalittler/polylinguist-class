# Module 0: Computational Thinking

## Overview

**Computational thinking** is the mental process of approaching problems the way a computer scientist does. It's not about memorizing syntax or learning a specific language—it's about developing a **problem-solving mindset** that applies to any programming challenge.

This module should be studied **before or alongside** your first programming lessons. The skills you learn here will make you a better programmer regardless of which language or paradigm you use.

## Learning Objectives

By the end of this module, you will be able to:

1. Break complex problems into smaller, manageable parts
2. Recognize patterns across different problems and paradigms
3. Think abstractly to generalize solutions
4. Design step-by-step algorithms
5. Anticipate edge cases and errors
6. Approach debugging systematically
7. Communicate your problem-solving approach clearly
8. Understand how computational thinking applies across imperative, object-oriented, and functional paradigms

## What is Computational Thinking?

**Computational thinking** is a problem-solving process that includes:

1. **Decomposition** - Breaking problems into smaller pieces
2. **Pattern Recognition** - Finding similarities with problems you've solved before
3. **Abstraction** - Focusing on important information, ignoring irrelevant details
4. **Algorithm Design** - Creating step-by-step solutions

These skills apply **beyond programming**—to planning projects, organizing information, making decisions, and solving everyday problems.

---

## The Four Pillars of Computational Thinking

### 1. Decomposition: Breaking Down Problems

**Definition:** Divide a complex problem into smaller, more manageable sub-problems.

#### Example: Making a Sandwich

**Big problem:** "Make a sandwich"

**Decomposed:**
1. Get ingredients
   - Bread
   - Filling (cheese, ham, etc.)
   - Condiments
2. Prepare ingredients
   - Slice bread (if needed)
   - Slice vegetables (if needed)
3. Assemble sandwich
   - Place bread slice
   - Add fillings in order
   - Add top bread slice
4. Serve
   - Cut sandwich (optional)
   - Place on plate

**Why this helps:** Each small step is easier to think about and execute.

#### Example: Building a Contact Management System

**Big problem:** "Create a contact manager"

**Decomposed:**
1. Store contacts
   - Design contact data structure
   - Implement adding contacts
   - Implement removing contacts
2. Search contacts
   - Search by name
   - Search by phone
3. Display contacts
   - List all contacts
   - Show single contact details
4. Persistence
   - Save to file
   - Load from file

#### Programming Application: Imperative Decomposition

**Python example:**
```python
# Bad: One giant function doing everything
def do_everything():
    # 200 lines of code...
    pass

# Good: Decomposed into smaller functions
def get_user_input():
    """Get and return user input."""
    pass

def validate_input(data):
    """Check if input is valid."""
    pass

def process_data(data):
    """Process the validated data."""
    pass

def save_results(results):
    """Save results to file or database."""
    pass

def main():
    data = get_user_input()
    if validate_input(data):
        results = process_data(data)
        save_results(results)
```

**C++ example:**
```cpp
// header.h
#include <string>
#include <vector>

class DataProcessor {
private:
    std::string getUserInput();
    bool validateInput(const std::string& data);
    std::vector<int> processData(const std::string& data);
    void saveResults(const std::vector<int>& results);

public:
    void run();
};

// implementation.cpp
void DataProcessor::run() {
    std::string data = getUserInput();
    if (validateInput(data)) {
        auto results = processData(data);
        saveResults(results);
    }
}
```

**Haskell example:**
```haskell
-- Functional decomposition: pure functions composed together

getUserInput :: IO String
getUserInput = getLine

validateInput :: String -> Bool
validateInput data = length data > 0

processData :: String -> [Int]
processData data = -- processing logic
    map length (words data)

saveResults :: [Int] -> IO ()
saveResults results = writeFile "output.txt" (show results)

main :: IO ()
main = do
    data <- getUserInput
    when (validateInput data) $ do
        let results = processData data
        saveResults results
```

**Notice the differences:**
- **Python/C++:** Sequential steps, mutable state, procedures that "do things"
- **Haskell:** Separation of pure functions (processData) from I/O actions (getUserInput, saveResults), immutability

**Practice:** When faced with a problem, ask yourself:
- "What are the major steps?"
- "Can I break this step into smaller steps?"
- "What's the simplest version of this problem?"

---

### 2. Pattern Recognition: Finding Similarities

**Definition:** Identify similarities between problems to reuse solutions.

#### Example: Everyday Patterns

**Pattern:** "Getting to a destination"
- Going to work: wake up → get ready → travel → arrive
- Going shopping: plan → travel → shop → return
- Going on vacation: plan → pack → travel → arrive

**Solution:** Once you've solved "traveling to a destination" once, you can adapt it!

#### Example: Programming Patterns Across Paradigms

**Pattern:** "Processing a list of items"

**Imperative approach (Python):**
```python
# Count items
count = 0
for item in items:
    count += 1

# Find maximum
max_value = items[0]
for item in items:
    if item > max_value:
        max_value = item

# Sum items
total = 0
for item in items:
    total += item
```

**Recognition:** All three use the same pattern: **iterate through a list and accumulate a result**.

**Imperative generalized solution (Python):**
```python
def process_list(items, initial_value, operation):
    """Generic accumulator pattern."""
    result = initial_value
    for item in items:
        result = operation(result, item)
    return result

# Usage:
count = process_list(items, 0, lambda acc, _: acc + 1)
max_val = process_list(items[1:], items[0], lambda acc, x: max(acc, x))
total = process_list(items, 0, lambda acc, x: acc + x)
```

**Object-oriented approach (C++):**
```cpp
#include <vector>
#include <functional>

template<typename T, typename U>
U processList(const std::vector<T>& items,
              U initialValue,
              std::function<U(U, T)> operation) {
    U result = initialValue;
    for (const auto& item : items) {
        result = operation(result, item);
    }
    return result;
}

// Usage:
auto count = processList(items, 0,
    [](int acc, int _) { return acc + 1; });
auto maxVal = processList(items, items[0],
    [](int acc, int x) { return std::max(acc, x); });
auto total = processList(items, 0,
    [](int acc, int x) { return acc + x; });
```

**Functional approach (Haskell):**
```haskell
-- In Haskell, this pattern is called 'fold' or 'reduce'
-- It's built into the language!

-- Count items
count = length items  -- or: foldr (\_ acc -> acc + 1) 0 items

-- Find maximum
maxVal = maximum items  -- or: foldr max (head items) (tail items)

-- Sum items
total = sum items  -- or: foldr (+) 0 items

-- The pattern is so common, it's abstracted as 'foldr' and 'foldl':
-- foldr :: (a -> b -> b) -> b -> [a] -> b
processList :: (a -> b -> b) -> b -> [a] -> b
processList = foldr
```

**Key insight:** The functional paradigm recognizes this pattern so deeply that it's a fundamental operation (fold/reduce) with built-in functions for common cases.

#### Common Programming Patterns

1. **Loop/iteration pattern:** Do something repeatedly
   - Imperative: for/while loops
   - Functional: map, filter, fold, recursion

2. **Search pattern:** Find an item matching criteria
   - Imperative: loop with break
   - Functional: find, filter, takeWhile

3. **Transform pattern:** Convert items from one form to another
   - Imperative: loop with mutation
   - Functional: map, comprehensions

4. **Filter pattern:** Select items matching criteria
   - Imperative: loop with condition
   - Functional: filter, list comprehensions

5. **Accumulate pattern:** Keep a running total or result
   - Imperative: loop with accumulator variable
   - Functional: fold/reduce

**Practice:** When solving a problem, ask:
- "Have I solved something similar before?"
- "What patterns do I recognize?"
- "Can I adapt an existing solution?"
- "Does my language have built-in support for this pattern?"

---

### 3. Abstraction: Focusing on What Matters

**Definition:** Hide complexity and focus on the essential information.

#### Example: Driving a Car

**Abstraction:** You press the gas pedal, the car goes faster.

**Hidden details:** Engine combustion, transmission gears, fuel injection, etc.

**Why useful:** You don't need to understand engine mechanics to drive!

#### Abstraction Across Paradigms

Abstraction works differently in different programming paradigms:

**Procedural abstraction (Python):**
```python
# Using abstraction
result = sorted([3, 1, 4, 1, 5])
# You don't need to know HOW sorted() works
# You only need to know WHAT it does: sorts a list

# Creating abstraction with functions
def calculate_area(length, width):
    """Calculate rectangle area - implementation hidden."""
    return length * width

area1 = calculate_area(5, 3)
area2 = calculate_area(10, 2)
```

**Object-oriented abstraction (C++):**
```cpp
// Abstraction through classes and interfaces
class Rectangle {
private:
    double length;
    double width;

public:
    Rectangle(double l, double w) : length(l), width(w) {}

    // Public interface hides implementation
    double area() const {
        return length * width;
    }

    double perimeter() const {
        return 2 * (length + width);
    }
};

// Usage - implementation details hidden
Rectangle rect(5.0, 3.0);
std::cout << rect.area() << std::endl;  // Don't need to know how
```

**Functional abstraction (Haskell):**
```haskell
-- Abstraction through higher-order functions and type classes

-- Simple function abstraction
calculateArea :: Double -> Double -> Double
calculateArea length width = length * width

-- Higher-order abstraction: abstract the operation itself
applyOperation :: (a -> a -> a) -> a -> a -> a
applyOperation op x y = op x y

-- Type class abstraction: abstract over types
class Shape a where
    area :: a -> Double
    perimeter :: a -> Double

data Rectangle = Rectangle { rectLength :: Double, rectWidth :: Double }

instance Shape Rectangle where
    area (Rectangle l w) = l * w
    perimeter (Rectangle l w) = 2 * (l + w)

-- Usage
rect = Rectangle 5.0 3.0
result = area rect  -- Type class determines which 'area' function
```

#### Levels of Abstraction

**High-level (abstract):**
```python
send_email(to="user@example.com", subject="Hello", body="Hi there!")
```

**Medium-level (more detail):**
```python
smtp_connection = connect_to_smtp_server()
message = format_email_message(to, subject, body)
smtp_connection.send(message)
smtp_connection.close()
```

**Lower-level (implementation details):**
```python
# Opening sockets, formatting MIME, handling protocols...
socket = create_socket(host, port)
socket.send(b"HELO\r\n")
# ... many more low-level operations
```

**Practice:** When designing solutions, ask:
- "What's the essential information?"
- "What details can I hide?"
- "Can I describe this at a higher level?"
- "What should the user of this code know vs. what can be internal?"
- "Does my paradigm offer special abstraction mechanisms?" (classes, higher-order functions, etc.)

---

### 4. Algorithm Design: Step-by-Step Solutions

**Definition:** Create clear, ordered instructions to solve a problem.

#### Example: Making Tea

**Algorithm:**
1. Fill kettle with water
2. Turn on kettle
3. Wait for water to boil
4. Put tea bag in cup
5. Pour boiling water into cup
6. Wait 3-5 minutes
7. Remove tea bag
8. Add milk/sugar (optional)
9. Stir
10. Drink

**Key properties:**
- **Clear:** Each step is unambiguous
- **Ordered:** Steps must happen in sequence
- **Finite:** It ends
- **Effective:** It produces the desired result

#### Example: Finding the Largest Number

**Problem:** Find the largest number in a list.

**Algorithm (in plain English):**
1. Start with the first number as the "current largest"
2. For each remaining number:
   - If it's bigger than the "current largest"
   - Make it the new "current largest"
3. Return the "current largest"

**Imperative implementation (Python):**
```python
def find_max(numbers):
    """Imperative: use mutable state and loops."""
    if not numbers:
        return None

    max_so_far = numbers[0]

    for num in numbers[1:]:
        if num > max_so_far:
            max_so_far = num

    return max_so_far
```

**Object-oriented implementation (C++):**
```cpp
#include <vector>
#include <optional>

class NumberAnalyzer {
public:
    static std::optional<int> findMax(const std::vector<int>& numbers) {
        if (numbers.empty()) {
            return std::nullopt;
        }

        int maxSoFar = numbers[0];

        for (size_t i = 1; i < numbers.size(); ++i) {
            if (numbers[i] > maxSoFar) {
                maxSoFar = numbers[i];
            }
        }

        return maxSoFar;
    }
};
```

**Functional implementation (Haskell):**
```haskell
-- Recursive approach - no mutable state
findMax :: (Ord a) => [a] -> Maybe a
findMax [] = Nothing
findMax [x] = Just x
findMax (x:xs) = case findMax xs of
    Nothing -> Just x
    Just maxRest -> Just (max x maxRest)

-- Or using fold (common functional pattern)
findMax' :: (Ord a) => [a] -> Maybe a
findMax' [] = Nothing
findMax' (x:xs) = Just (foldl max x xs)

-- Or use the built-in
findMax'' :: (Ord a) => [a] -> Maybe a
findMax'' [] = Nothing
findMax'' xs = Just (maximum xs)
```

**Notice the algorithmic differences:**
- **Imperative (Python):** Explicit loop, mutable `max_so_far` variable
- **OOP (C++):** Similar to imperative, but wrapped in a class structure
- **Functional (Haskell):** Recursion or fold instead of loops, no mutation, pattern matching

All solve the same problem, but the **thinking process** differs!

#### Algorithm Design Process

1. **Understand the problem**
   - What are the inputs?
   - What should the output be?
   - What are the constraints?

2. **Design the solution**
   - Write steps in plain language first
   - Consider: imperative (step-by-step) or functional (transformation) approach?
   - Don't worry about syntax yet!
   - Think about edge cases

3. **Trace through examples**
   - Walk through your algorithm with sample data
   - Does it work?
   - Does it handle edge cases?

4. **Implement**
   - Choose the paradigm/language
   - Translate to code
   - Test with various inputs

5. **Refine**
   - Can it be simpler?
   - Can it be more efficient?
   - Is it readable?
   - Does it fit the paradigm well?

**Practice:** For any problem:
1. Write the algorithm in plain English first
2. Test it mentally with examples
3. Consider which paradigm fits naturally
4. Only then write code

---

## Computational Thinking Across Paradigms

Understanding how computational thinking applies differently across paradigms will make you a more versatile programmer.

### Decomposition Across Paradigms

**Problem:** Process a text file to count word frequencies.

**Imperative decomposition (Python):**
```python
# Think in terms of steps and state changes
def count_words(filename):
    # Step 1: Read file
    with open(filename) as f:
        text = f.read()

    # Step 2: Normalize text
    text = text.lower()

    # Step 3: Split into words
    words = text.split()

    # Step 4: Count frequencies (mutable dictionary)
    freq = {}
    for word in words:
        freq[word] = freq.get(word, 0) + 1

    return freq
```

**Object-oriented decomposition (C++):**
```cpp
// Think in terms of objects and their responsibilities
class TextAnalyzer {
private:
    std::string filename;
    std::string text;
    std::map<std::string, int> frequencies;

    void readFile() { /* read into text */ }
    void normalize() { /* convert text to lowercase */ }
    std::vector<std::string> tokenize() { /* split into words */ }
    void computeFrequencies(const std::vector<std::string>& words) {
        for (const auto& word : words) {
            frequencies[word]++;
        }
    }

public:
    TextAnalyzer(const std::string& fname) : filename(fname) {}

    std::map<std::string, int> analyze() {
        readFile();
        normalize();
        auto words = tokenize();
        computeFrequencies(words);
        return frequencies;
    }
};
```

**Functional decomposition (Haskell):**
```haskell
-- Think in terms of data transformations through pipelines
import Data.Char (toLower)
import Data.List (group, sort)
import qualified Data.Map as Map

countWords :: String -> IO (Map.Map String Int)
countWords filename = do
    text <- readFile filename              -- IO action
    return $ countWords' text              -- Pure function

-- Pure transformation pipeline
countWords' :: String -> Map.Map String Int
countWords' text =
    text
    |> map toLower                         -- normalize
    |> words                                -- tokenize
    |> sort                                 -- group requires sorting
    |> group                                -- group identical words
    |> map (\ws -> (head ws, length ws))   -- count each group
    |> Map.fromList                         -- convert to map

-- Helper operator for pipeline (if not in standard library)
(|>) :: a -> (a -> b) -> b
x |> f = f x
```

**Key differences:**
- **Imperative:** Steps that modify state sequentially
- **OOP:** Objects with responsibilities, encapsulated state
- **Functional:** Data flowing through transformations, minimal state

### Pattern Recognition Across Paradigms

**Pattern:** "Do something to each element in a collection"

**Python (imperative or functional style):**
```python
# Imperative
result = []
for x in numbers:
    result.append(x * 2)

# Functional (Python supports both!)
result = [x * 2 for x in numbers]  # list comprehension
result = list(map(lambda x: x * 2, numbers))  # map function
```

**C++ (imperative or STL algorithms):**
```cpp
// Imperative
std::vector<int> result;
for (int x : numbers) {
    result.push_back(x * 2);
}

// Using STL algorithms (more functional)
std::vector<int> result(numbers.size());
std::transform(numbers.begin(), numbers.end(),
               result.begin(),
               [](int x) { return x * 2; });
```

**Haskell (purely functional):**
```haskell
-- map is the standard way
result = map (*2) numbers

-- Or list comprehension
result = [x * 2 | x <- numbers]
```

**Recognition:** When you see "do X to each Y", think:
- Imperative languages → for loop
- Languages with functional features → map, comprehensions
- Functional languages → map is the natural choice

### Abstraction Mechanisms by Paradigm

Different paradigms offer different abstraction tools:

**Imperative (Python):**
- Functions
- Modules
- First-class functions and closures

**Object-Oriented (C++):**
- Functions
- Classes and objects
- Inheritance and polymorphism
- Templates (generic programming)
- Abstract classes and interfaces

**Functional (Haskell):**
- Functions (primary unit)
- Higher-order functions
- Type classes (like interfaces, but more powerful)
- Algebraic data types
- Monads (abstracting computation patterns)

**Example: Abstracting "optional values"**

**Python:**
```python
# Using None
def find_user(id):
    # Returns user or None
    pass

user = find_user(123)
if user is not None:
    print(user.name)
```

**C++:**
```cpp
// Using std::optional
std::optional<User> findUser(int id) {
    // Returns optional containing user or empty optional
}

auto user = findUser(123);
if (user.has_value()) {
    std::cout << user->name << std::endl;
}
```

**Haskell:**
```haskell
-- Using Maybe (a built-in algebraic data type)
findUser :: Int -> Maybe User
findUser id = -- returns Just user or Nothing

case findUser 123 of
    Just user -> putStrLn (userName user)
    Nothing   -> putStrLn "User not found"

-- Or using functional abstractions
findUser 123 >>= \user -> putStrLn (userName user)
```

### Algorithm Design: Imperative vs Functional

**Problem:** Reverse a list

**Imperative thinking (Python):**
"Swap elements from both ends toward the middle"
```python
def reverse_list(lst):
    """In-place reversal by swapping."""
    left = 0
    right = len(lst) - 1
    while left < right:
        lst[left], lst[right] = lst[right], lst[left]
        left += 1
        right -= 1
    return lst
```

**Functional thinking (Haskell):**
"Build a new list by taking elements in reverse order"
```haskell
-- Recursive definition
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

-- More efficient with accumulator
reverseList' :: [a] -> [a]
reverseList' = go []
  where
    go acc [] = acc
    go acc (x:xs) = go (x:acc) xs

-- Or just use the built-in
reverseList'' :: [a] -> [a]
reverseList'' = reverse
```

**Key difference:**
- **Imperative:** Think about *how* to manipulate state step-by-step
- **Functional:** Think about *what* the result is in terms of the input

### When to Use Which Paradigm

**Use imperative/OOP thinking when:**
- The problem naturally involves state changes over time
- You're working with I/O, systems programming, or mutable resources
- Performance requires in-place updates
- The domain models objects with behavior (games, simulations)

**Use functional thinking when:**
- The problem is about transforming data
- You want strong guarantees about correctness
- You're doing mathematical or symbolic computation
- You want to maximize code reusability through composition
- Concurrency/parallelism is important (immutability helps!)

**Best approach:** Learn to think in both ways! Many problems benefit from mixing paradigms:
- Python and C++ support both imperative and functional patterns
- Even Haskell uses monads to handle imperative-style I/O
- Modern programming often combines the best of both worlds

---

## Practical Problem-Solving Strategies

### Strategy 1: Start Simple

**Don't solve the full problem immediately!**

#### Example: Building a Calculator

**Don't start with:** "Build a calculator that handles +, -, ×, ÷, parentheses, decimals, and errors"

**Start with:**
1. Build a calculator that adds two numbers
2. Add subtraction
3. Add multiplication and division
4. Add support for more than two numbers
5. Add parentheses support
6. Add error handling
7. Add decimal support

**Why:** Each step is easier, and you can test as you go.

### Strategy 2: Work Backwards

Sometimes it's easier to think from the solution back to the start.

#### Example: Maze Solving

**Forward thinking:** "Where do I go from the start?"
**Backward thinking:** "What positions can reach the end?"

#### Example: Planning a Road Trip

**Forward:** "What should I do first?" (Overwhelming!)
**Backward:**
- "I need to be at the destination by 5pm"
- "So I need to leave by noon"
- "So I need to pack by 11am"
- "So I need to start packing at 9am"

### Strategy 3: Solve by Example

**Work through concrete examples before generalizing.**

#### Example: Reversing a String

**Example:** "hello" → "olleh"

**Trace it:**
- Take from end: 'o'
- Next: 'l'  → "ol"
- Next: 'l'  → "oll"
- Next: 'e'  → "olle"
- Next: 'h'  → "olleh"

**Pattern recognized:** "Take characters from end to start"

**Generalized (Python):**
```python
def reverse_string(s):
    result = ""
    for char in reversed(s):
        result += char
    return result

# Or even simpler:
def reverse_string(s):
    return s[::-1]
```

**Functional approach (Haskell):**
```haskell
-- The pattern naturally expresses the recursive structure
reverseString :: String -> String
reverseString "" = ""
reverseString (c:cs) = reverseString cs ++ [c]
```

### Strategy 4: Draw It Out

**Visual representations help understanding.**

- Lists → boxes in a row
- Trees → branching diagrams
- Algorithms → flowcharts
- State → state diagrams
- Function composition → data flow diagrams

#### Example: Understanding Recursion

Draw the call stack:
```
factorial(4)
  → 4 * factorial(3)
       → 3 * factorial(2)
            → 2 * factorial(1)
                 → 1
            → 2 * 1 = 2
       → 3 * 2 = 6
  → 4 * 6 = 24
```

---

## Thinking About Edge Cases

**Edge cases** are unusual or extreme inputs that might break your solution.

### Common Edge Cases

1. **Empty input**
   - Empty list: `[]`
   - Empty string: `""`
   - Zero: `0`
   - None/null

2. **Single item**
   - List with one element: `[5]`
   - String with one character: `"a"`

3. **Duplicates**
   - All same: `[3, 3, 3, 3]`
   - Some duplicates: `[1, 2, 2, 3]`

4. **Negative numbers**
   - What if input is negative when you expect positive?

5. **Very large numbers**
   - What if the number is too big to fit in memory?

6. **Special characters**
   - Spaces, punctuation, unicode

### Example: Finding Maximum with Edge Cases

**Python:**
```python
def find_max(numbers):
    # Edge case: empty list
    if not numbers:
        return None  # or raise ValueError("Empty list")

    # Edge case: single element (works with general case)
    if len(numbers) == 1:
        return numbers[0]

    # General case
    max_so_far = numbers[0]
    for num in numbers[1:]:
        if num > max_so_far:
            max_so_far = num

    return max_so_far
```

**Haskell (types help handle edge cases!):**
```haskell
-- Type signature makes edge case handling explicit
findMax :: (Ord a) => [a] -> Maybe a
findMax [] = Nothing           -- Edge case: empty list
findMax [x] = Just x           -- Edge case: single element
findMax (x:xs) =               -- General case
    case findMax xs of
        Nothing -> Just x
        Just maxRest -> Just (max x maxRest)
```

**Practice:** For every solution, ask:
- "What if the input is empty?"
- "What if there's only one item?"
- "What if all items are the same?"
- "What if the input is negative/zero/huge?"

---

## The Debugging Mindset

**Debugging is problem-solving!** When your code doesn't work:

### 1. Reproduce the Problem

- Can you make it fail consistently?
- What are the exact steps to trigger it?

### 2. Simplify

- Can you create a minimal example that shows the bug?
- Remove unrelated code

### 3. Form a Hypothesis

- "I think the bug is because..."
- "I expect X to happen, but Y happens instead"

### 4. Test Your Hypothesis

**Python debugging:**
```python
def mysterious_function(x):
    print(f"Input: {x}")  # What's coming in?
    result = x * 2 + 5
    print(f"Result: {result}")  # What's going out?
    return result
```

**C++ debugging:**
```cpp
int mysteriousFunction(int x) {
    std::cout << "Input: " << x << std::endl;
    int result = x * 2 + 5;
    std::cout << "Result: " << result << std::endl;
    return result;
}
```

**Haskell debugging:**
```haskell
import Debug.Trace

mysteriousFunction :: Int -> Int
mysteriousFunction x =
    let result = x * 2 + 5
    in trace ("Input: " ++ show x ++ ", Result: " ++ show result) result
```

### 5. Fix and Verify

- Make the fix
- Test that it works
- Test that you didn't break anything else

### Debugging Techniques

**Print debugging:** (shown above)

**Rubber duck debugging:**
- Explain your code line-by-line to a rubber duck (or friend)
- Often you'll spot the problem while explaining!

**Binary search debugging:**
- Comment out half the code
- Does the bug still occur?
- If yes, bug is in the remaining half
- If no, bug is in the commented half
- Repeat

**Type-driven debugging (especially useful in Haskell/C++):**
- Let the type system guide you
- If it doesn't compile, read the error message carefully
- Types often reveal logic errors before runtime

**Read error messages carefully:**
- What line number?
- What type of error?
- What does the message say?

---

## Summary

### The Four Pillars

1. **Decomposition:** Break big problems into smaller ones
2. **Pattern Recognition:** Find similarities with solved problems
3. **Abstraction:** Hide details, focus on essentials
4. **Algorithm Design:** Create step-by-step solutions

### Problem-Solving Process

1. **Understand** the problem
2. **Decompose** into smaller parts
3. **Look for patterns** you recognize
4. **Design** an algorithm in plain language
5. **Consider edge cases**
6. **Choose a paradigm** that fits naturally
7. **Implement** in code
8. **Test** thoroughly
9. **Debug** systematically
10. **Refine** and improve

### Key Mindsets

- **Start simple**, then build complexity
- **Work through examples** before generalizing
- **Draw diagrams** to visualize
- **Think about edge cases** early
- **Choose the right paradigm** for the problem
- **Don't be afraid to fail** - debugging is learning!

### Paradigm-Specific Tips

**Imperative (Python, C++):**
- Think in steps and state changes
- Use loops and mutable variables naturally
- Break into functions/methods

**Object-Oriented (C++, Python):**
- Think in objects with responsibilities
- Use classes to encapsulate state and behavior
- Apply design patterns

**Functional (Haskell, Python features):**
- Think in data transformations
- Use immutability and pure functions
- Compose small functions into larger ones
- Let types guide your design

---

## Next Steps

**Apply computational thinking to every problem:**

1. Before writing code, spend time **thinking**
2. Write algorithms in **plain English first**
3. **Draw diagrams** of your solution
4. Consider **edge cases** before implementing
5. **Break down** complex problems systematically
6. **Choose the right paradigm** for the problem
7. **Practice in all three languages** to see different perspectives

**Remember:** Programming languages and paradigms are tools. Computational thinking is the **skill** that makes you a great programmer in **any** language or paradigm!

As you work through the lessons in this curriculum, you'll see these concepts applied in Python, C++, and Haskell. Notice how:
- **Python** gives you flexibility to choose imperative or functional style
- **C++** adds object-oriented power with explicit control
- **Haskell** enforces functional thinking with powerful abstractions

Each language will strengthen different aspects of your computational thinking!

---

## Additional Resources

- **Book:** "How to Solve It" by George Pólya (problem-solving classic)
- **Video:** "Computational Thinking" - Google CS Education
- **Book:** "Structure and Interpretation of Computer Programs" (SICP) - paradigm comparison
- **Practice:** Project Euler, LeetCode, HackerRank (apply these concepts!)
- **Practice:** Solve the same problem in all three languages to see paradigm differences

**Most importantly:** Practice, practice, practice! Computational thinking improves with every problem you solve, and seeing solutions across paradigms deepens your understanding.
