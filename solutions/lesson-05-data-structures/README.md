# Lesson 5: Data Structures - Solution Guide

This guide provides example solutions for the Data Structures exercises.

## General Notes

- **Multiple correct solutions exist**: Your solution may differ and still be correct
- **Focus on understanding mutability vs immutability**: Pay attention to how different paradigms handle data
- **Language idioms matter**: Each language has preferred patterns
- **Try before looking**: Attempt each exercise before checking solutions

---

## Exercise 1: List Operations (Warmup)

**Task:** Perform basic list operations in mutable and immutable styles

### Python Solution (Mutable)

```python
# 1. Create list with numbers 1-10
numbers = list(range(1, 11))
print(numbers)  # [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

# 2. Add 11 to the end
numbers.append(11)
print(numbers)  # [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]

# 3. Insert 0 at the beginning
numbers.insert(0, 0)
print(numbers)  # [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]

# 4. Remove element at index 5
del numbers[5]
# Or: numbers.pop(5)
print(numbers)  # [0, 1, 2, 3, 4, 6, 7, 8, 9, 10, 11]

# 5. Print length and all elements
print(f"Length: {len(numbers)}")
print(f"Elements: {numbers}")
```

### Python Solution (Immutable Style)

```python
# 1. Create list
numbers = list(range(1, 11))

# 2. Add 11 (create new list)
numbers = numbers + [11]

# 3. Insert 0 (create new list)
numbers = [0] + numbers

# 4. Remove at index 5 (create new list)
numbers = numbers[:5] + numbers[6:]

# 5. Print
print(f"Length: {len(numbers)}")
print(f"Elements: {numbers}")
```

### Haskell Solution (Immutable)

```haskell
-- 1. Create list with numbers 1-10
numbers :: [Int]
numbers = [1..10]

-- 2. "Add" 11 to the end (creates new list)
numbers2 :: [Int]
numbers2 = numbers ++ [11]

-- 3. "Insert" 0 at beginning (creates new list)
numbers3 :: [Int]
numbers3 = 0 : numbers2

-- 4. "Remove" element at index 5 (creates new list)
removeAt :: Int -> [a] -> [a]
removeAt idx xs = take idx xs ++ drop (idx + 1) xs

numbers4 :: [Int]
numbers4 = removeAt 5 numbers3

main :: IO ()
main = do
    putStrLn $ "Length: " ++ show (length numbers4)
    putStrLn $ "Elements: " ++ show numbers4
```

**Key Differences:**
- **Mutable (Python)**: Operations modify the list in-place
- **Immutable (Haskell)**: Each operation creates a new list
- **Efficiency**: Mutable is more efficient for large lists
- **Safety**: Immutable prevents accidental modifications
- **Naturalness**: Depends on the problem and paradigm

---

## Exercise 2: FizzBuzz with Arrays

**Task:** Create FizzBuzz array for numbers 1-100

### Python Solution (List Comprehension)

```python
def fizzbuzz_value(n):
    if n % 15 == 0:
        return "FizzBuzz"
    elif n % 3 == 0:
        return "Fizz"
    elif n % 5 == 0:
        return "Buzz"
    else:
        return n

fizzbuzz = [fizzbuzz_value(i) for i in range(1, 101)]

# Print first 20
print(fizzbuzz[:20])
# [1, 2, 'Fizz', 4, 'Buzz', 'Fizz', 7, 8, 'Fizz', 'Buzz', 11, 'Fizz', 13, 14, 'FizzBuzz', ...]
```

**One-liner version:**

```python
fizzbuzz = [
    "FizzBuzz" if i % 15 == 0 else
    "Fizz" if i % 3 == 0 else
    "Buzz" if i % 5 == 0 else
    i
    for i in range(1, 101)
]
```

### JavaScript Solution (map)

```javascript
function fizzbuzzValue(n) {
    if (n % 15 === 0) return "FizzBuzz";
    if (n % 3 === 0) return "Fizz";
    if (n % 5 === 0) return "Buzz";
    return n;
}

const fizzbuzz = Array.from({length: 100}, (_, i) => i + 1).map(fizzbuzzValue);

// Or more concise
const fizzbuzz2 = [...Array(100).keys()].map(i => {
    const n = i + 1;
    if (n % 15 === 0) return "FizzBuzz";
    if (n % 3 === 0) return "Fizz";
    if (n % 5 === 0) return "Buzz";
    return n;
});

console.log(fizzbuzz.slice(0, 20));
```

### Haskell Solution (List Comprehension)

```haskell
fizzbuzz :: Int -> String
fizzbuzz n
    | n `mod` 15 == 0 = "FizzBuzz"
    | n `mod` 3 == 0  = "Fizz"
    | n `mod` 5 == 0  = "Buzz"
    | otherwise       = show n

fizzbuzzList :: [String]
fizzbuzzList = [fizzbuzz n | n <- [1..100]]

-- Or using map
fizzbuzzList' :: [String]
fizzbuzzList' = map fizzbuzz [1..100]

main :: IO ()
main = do
    print $ take 20 fizzbuzzList
```

**Key Insights:**
- List comprehensions provide concise syntax in Python and Haskell
- JavaScript's `map` is the functional equivalent
- All three approaches are declarative (describe what, not how)

---

## Exercise 3: Dictionary/Map Practice

**Task:** Create a phonebook with add, update, lookup, delete operations

### Python Solution

```python
# 1. Create phonebook
phonebook = {
    "Alice": "555-1234",
    "Bob": "555-2345",
    "Charlie": "555-3456",
    "Diana": "555-4567",
    "Eve": "555-5678"
}

# 2. Add new person
phonebook["Frank"] = "555-6789"

# 3. Update phone number
phonebook["Alice"] = "555-9999"

# 4. Look up number
if "Bob" in phonebook:
    print(f"Bob's number: {phonebook['Bob']}")

# Safe lookup with get
print(f"Bob's number: {phonebook.get('Bob', 'Not found')}")
print(f"Unknown: {phonebook.get('Unknown', 'Not found')}")

# 5. Delete person
del phonebook["Eve"]
# Or: phonebook.pop("Eve")

# 6. Print all names
print("All names:")
for name in phonebook:
    print(f"  {name}")

# Or just keys
print(phonebook.keys())

# Bonus: Safe operations
def get_number(phonebook, name):
    return phonebook.get(name, "Person not found")

def add_person(phonebook, name, number):
    if name in phonebook:
        print(f"{name} already exists, use update instead")
        return False
    phonebook[name] = number
    return True

def update_number(phonebook, name, number):
    if name not in phonebook:
        print(f"{name} not found")
        return False
    phonebook[name] = number
    return True

print(get_number(phonebook, "Alice"))    # 555-9999
print(get_number(phonebook, "Unknown"))  # Person not found
```

### JavaScript Solution

```javascript
// 1. Create phonebook
const phonebook = {
    "Alice": "555-1234",
    "Bob": "555-2345",
    "Charlie": "555-3456",
    "Diana": "555-4567",
    "Eve": "555-5678"
};

// Or using Map
const phonebookMap = new Map([
    ["Alice", "555-1234"],
    ["Bob", "555-2345"],
    ["Charlie", "555-3456"],
    ["Diana", "555-4567"],
    ["Eve", "555-5678"]
]);

// 2. Add new person
phonebook["Frank"] = "555-6789";
phonebookMap.set("Frank", "555-6789");

// 3. Update number
phonebook["Alice"] = "555-9999";
phonebookMap.set("Alice", "555-9999");

// 4. Look up number
console.log(phonebook["Bob"]);  // 555-2345
console.log(phonebookMap.get("Bob"));  // 555-2345

// Safe lookup
const getNumber = (pb, name) => pb[name] || "Not found";
console.log(getNumber(phonebook, "Unknown"));  // Not found

// 5. Delete person
delete phonebook["Eve"];
phonebookMap.delete("Eve");

// 6. Print all names
console.log("All names:");
Object.keys(phonebook).forEach(name => console.log(`  ${name}`));

// With Map
phonebookMap.forEach((number, name) => {
    console.log(`${name}: ${number}`);
});
```

### Java Solution

```java
import java.util.HashMap;
import java.util.Map;

public class Phonebook {
    public static void main(String[] args) {
        // 1. Create phonebook
        Map<String, String> phonebook = new HashMap<>();
        phonebook.put("Alice", "555-1234");
        phonebook.put("Bob", "555-2345");
        phonebook.put("Charlie", "555-3456");
        phonebook.put("Diana", "555-4567");
        phonebook.put("Eve", "555-5678");

        // 2. Add new person
        phonebook.put("Frank", "555-6789");

        // 3. Update number
        phonebook.put("Alice", "555-9999");

        // 4. Look up number
        String bobNumber = phonebook.get("Bob");
        System.out.println("Bob's number: " + bobNumber);

        // Safe lookup
        String unknownNumber = phonebook.getOrDefault("Unknown", "Not found");
        System.out.println("Unknown: " + unknownNumber);

        // 5. Delete person
        phonebook.remove("Eve");

        // 6. Print all names
        System.out.println("All names:");
        for (String name : phonebook.keySet()) {
            System.out.println("  " + name);
        }
    }
}
```

**Key Insights:**
- Dictionaries/maps store key-value pairs
- Python: `dict`, JavaScript: `Object` or `Map`, Java: `HashMap`
- Always handle missing keys gracefully
- `get()` with default is safer than direct access

---

## Exercise 4: Aliasing Bug Hunt

**Task:** Debug aliasing issue and explain the problem

### Python Problem

```python
original = [1, 2, 3]
copy = original  # This creates an ALIAS, not a copy!
copy.append(4)
print(original)  # [1, 2, 3, 4] - SURPRISE!
```

### Explanation

**Why `original` changed:**
- `copy = original` doesn't create a new list
- Both `copy` and `original` refer to the **same list object** in memory
- Modifying through either name affects the same underlying data
- This is called **aliasing**

### Fixed Version

```python
# Fix 1: Using .copy()
original = [1, 2, 3]
copy = original.copy()
copy.append(4)
print(original)  # [1, 2, 3] - Unchanged!
print(copy)      # [1, 2, 3, 4]

# Fix 2: Using slice notation
original = [1, 2, 3]
copy = original[:]
copy.append(4)
print(original)  # [1, 2, 3]

# Fix 3: Using list()
original = [1, 2, 3]
copy = list(original)
copy.append(4)
print(original)  # [1, 2, 3]

# Deep copy for nested structures
import copy as copy_module
nested = [[1, 2], [3, 4]]
shallow = nested.copy()  # Only copies outer list
deep = copy_module.deepcopy(nested)  # Copies everything

shallow[0].append(5)
print(nested)   # [[1, 2, 5], [3, 4]] - Modified!
print(shallow)  # [[1, 2, 5], [3, 4]] - Shares inner lists

deep[1].append(6)
print(nested)   # [[1, 2, 5], [3, 4]] - Unchanged
```

### When Aliasing is Useful

```python
# Useful: Multiple names for same data
students = ["Alice", "Bob", "Charlie"]
class_roster = students  # Intentional alias
# Changes to students reflect in class_roster automatically
```

### When Aliasing is Dangerous

```python
# Dangerous: Unintended sharing
def add_student(student_list, name):
    student_list.append(name)  # Modifies original!
    return student_list

original_students = ["Alice", "Bob"]
new_students = add_student(original_students, "Charlie")
# original_students is now ["Alice", "Bob", "Charlie"] - might be unexpected!
```

### Haskell Comparison

```haskell
-- Haskell: No aliasing possible with immutable data
original = [1, 2, 3]
copy = original
-- copy and original refer to the same value, BUT
-- since lists are immutable, there's no way to "change" them
-- Any "modification" creates a new list

newList = original ++ [4]  -- Creates new list
-- original is still [1, 2, 3]
-- newList is [1, 2, 3, 4]
```

### Rust Comparison

```rust
// Rust: Prevents aliasing issues at compile time
fn main() {
    let mut original = vec![1, 2, 3];
    let copy = original;  // Ownership moved!
    // original.push(4);  // ERROR: original no longer valid
    // copy.push(4);      // OK

    // To actually copy:
    let original = vec![1, 2, 3];
    let copy = original.clone();  // Explicit clone
    // Now both work independently
}
```

**Key Insights:**
- Aliasing creates multiple names for the same data
- Can cause unexpected behavior in mutable languages
- Always use `.copy()` or `[:]` when you want independent data
- Haskell avoids the problem with immutability
- Rust catches it at compile time with ownership

---

## Exercise 5: Immutable Update Pattern

**Task:** Update, remove, and insert in immutable list

### Haskell Solution

```haskell
numbers :: [Int]
numbers = [1, 2, 3, 4, 5]

-- 1. "Update" element at index 2 to be 10
updateAt :: Int -> a -> [a] -> [a]
updateAt idx newVal xs =
    take idx xs ++ [newVal] ++ drop (idx + 1) xs

numbers1 :: [Int]
numbers1 = updateAt 2 10 numbers  -- [1, 2, 10, 4, 5]

-- 2. "Remove" element at index 3
removeAt :: Int -> [a] -> [a]
removeAt idx xs = take idx xs ++ drop (idx + 1) xs

numbers2 :: [Int]
numbers2 = removeAt 3 numbers  -- [1, 2, 3, 5]

-- 3. "Insert" 0 at index 0
insertAt :: Int -> a -> [a] -> [a]
insertAt idx newVal xs =
    take idx xs ++ [newVal] ++ drop idx xs

numbers3 :: [Int]
numbers3 = insertAt 0 0 numbers  -- [0, 1, 2, 3, 4, 5]

main :: IO ()
main = do
    print numbers   -- [1, 2, 3, 4, 5]
    print numbers1  -- [1, 2, 10, 4, 5]
    print numbers2  -- [1, 2, 3, 5]
    print numbers3  -- [0, 1, 2, 3, 4, 5]
```

### JavaScript Bonus (Spread Operator)

```javascript
const numbers = [1, 2, 3, 4, 5];

// 1. Update element at index 2
const updateAt = (arr, idx, val) => [
    ...arr.slice(0, idx),
    val,
    ...arr.slice(idx + 1)
];

const numbers1 = updateAt(numbers, 2, 10);
console.log(numbers1);  // [1, 2, 10, 4, 5]

// 2. Remove element at index 3
const removeAt = (arr, idx) => [
    ...arr.slice(0, idx),
    ...arr.slice(idx + 1)
];

const numbers2 = removeAt(numbers, 3);
console.log(numbers2);  // [1, 2, 3, 5]

// 3. Insert 0 at index 0
const insertAt = (arr, idx, val) => [
    ...arr.slice(0, idx),
    val,
    ...arr.slice(idx)
];

const numbers3 = insertAt(numbers, 0, 0);
console.log(numbers3);  // [0, 1, 2, 3, 4, 5]

// Original unchanged
console.log(numbers);  // [1, 2, 3, 4, 5]
```

**Key Insights:**
- Immutable updates create new data structures
- Use `take` and `drop` in Haskell
- Use spread operator `...` in JavaScript
- Original data remains unchanged
- Slightly less efficient but safer

---

## Exercise 6: Set Operations

**Task:** Perform union, intersection, difference, symmetric difference

### Python Solution

```python
# Create sets
A = {1, 2, 3, 4, 5}
B = {4, 5, 6, 7, 8}

# 1. Union (A ∪ B)
union = A | B
# Or: union = A.union(B)
print(f"Union: {union}")  # {1, 2, 3, 4, 5, 6, 7, 8}

# 2. Intersection (A ∩ B)
intersection = A & B
# Or: intersection = A.intersection(B)
print(f"Intersection: {intersection}")  # {4, 5}

# 3. Difference (A - B)
difference = A - B
# Or: difference = A.difference(B)
print(f"Difference: {difference}")  # {1, 2, 3}

# 4. Symmetric difference (A △ B)
sym_diff = A ^ B
# Or: sym_diff = A.symmetric_difference(B)
print(f"Symmetric difference: {sym_diff}")  # {1, 2, 3, 6, 7, 8}
```

### Application: Unique Words

```python
def unique_words(text):
    """Find unique words in text"""
    # Split and convert to lowercase
    words = text.lower().split()
    return set(words)

text1 = "the quick brown fox jumps over the lazy dog"
text2 = "the lazy cat sleeps on the mat"

words1 = unique_words(text1)
words2 = unique_words(text2)

print(f"Unique to text1: {words1 - words2}")
print(f"Common words: {words1 & words2}")
print(f"All words: {words1 | words2}")
```

### Haskell Solution

```haskell
import Data.Set (Set)
import qualified Data.Set as Set

-- Create sets
a :: Set Int
a = Set.fromList [1, 2, 3, 4, 5]

b :: Set Int
b = Set.fromList [4, 5, 6, 7, 8]

-- 1. Union
unionSet :: Set Int
unionSet = Set.union a b  -- fromList [1,2,3,4,5,6,7,8]

-- 2. Intersection
intersectionSet :: Set Int
intersectionSet = Set.intersection a b  -- fromList [4,5]

-- 3. Difference
differenceSet :: Set Int
differenceSet = Set.difference a b  -- fromList [1,2,3]

-- 4. Symmetric difference
symDiff :: Set Int
symDiff = Set.union (Set.difference a b) (Set.difference b a)
-- fromList [1,2,3,6,7,8]

main :: IO ()
main = do
    print unionSet
    print intersectionSet
    print differenceSet
    print symDiff
```

### Java Solution

```java
import java.util.HashSet;
import java.util.Set;

public class SetOperations {
    public static void main(String[] args) {
        Set<Integer> a = new HashSet<>();
        a.add(1); a.add(2); a.add(3); a.add(4); a.add(5);

        Set<Integer> b = new HashSet<>();
        b.add(4); b.add(5); b.add(6); b.add(7); b.add(8);

        // 1. Union
        Set<Integer> union = new HashSet<>(a);
        union.addAll(b);
        System.out.println("Union: " + union);

        // 2. Intersection
        Set<Integer> intersection = new HashSet<>(a);
        intersection.retainAll(b);
        System.out.println("Intersection: " + intersection);

        // 3. Difference
        Set<Integer> difference = new HashSet<>(a);
        difference.removeAll(b);
        System.out.println("Difference: " + difference);

        // 4. Symmetric difference
        Set<Integer> symDiff = new HashSet<>(a);
        symDiff.addAll(b);
        Set<Integer> temp = new HashSet<>(a);
        temp.retainAll(b);
        symDiff.removeAll(temp);
        System.out.println("Symmetric difference: " + symDiff);
    }
}
```

**Key Insights:**
- Sets automatically handle uniqueness
- Python has concise operators: `|`, `&`, `-`, `^`
- Haskell requires qualified imports for Set
- Sets are perfect for membership testing and deduplication

---

## Exercise 7: Nested Data Structures

**Task:** Manage classroom data with nested dictionaries and lists

### Python Solution

```python
classroom = {
    "teacher": "Ms. Smith",
    "students": [
        {"name": "Alice", "grades": [90, 85, 88]},
        {"name": "Bob", "grades": [75, 80, 78]},
        {"name": "Charlie", "grades": [95, 92, 94]}
    ]
}

# 1. Add new student
def add_student(classroom, name, grades=None):
    if grades is None:
        grades = []
    new_student = {"name": name, "grades": grades}
    classroom["students"].append(new_student)

add_student(classroom, "Diana", [88, 90, 85])

# 2. Add grade for specific student
def add_grade(classroom, student_name, grade):
    for student in classroom["students"]:
        if student["name"] == student_name:
            student["grades"].append(grade)
            return True
    return False

add_grade(classroom, "Alice", 92)

# 3. Calculate average for each student
def calculate_averages(classroom):
    averages = {}
    for student in classroom["students"]:
        name = student["name"]
        grades = student["grades"]
        avg = sum(grades) / len(grades) if grades else 0
        averages[name] = avg
    return averages

averages = calculate_averages(classroom)
print("Averages:", averages)

# 4. Find student with highest average
def find_top_student(classroom):
    averages = calculate_averages(classroom)
    if not averages:
        return None
    top_student = max(averages, key=averages.get)
    return top_student, averages[top_student]

top_name, top_avg = find_top_student(classroom)
print(f"Top student: {top_name} with average {top_avg:.2f}")

# Pretty print
def print_classroom(classroom):
    print(f"Teacher: {classroom['teacher']}")
    print("Students:")
    for student in classroom["students"]:
        name = student["name"]
        grades = student["grades"]
        avg = sum(grades) / len(grades) if grades else 0
        print(f"  {name}: grades={grades}, average={avg:.2f}")

print_classroom(classroom)
```

### JavaScript Solution

```javascript
const classroom = {
    teacher: "Ms. Smith",
    students: [
        { name: "Alice", grades: [90, 85, 88] },
        { name: "Bob", grades: [75, 80, 78] },
        { name: "Charlie", grades: [95, 92, 94] }
    ]
};

// 1. Add new student
const addStudent = (classroom, name, grades = []) => {
    classroom.students.push({ name, grades });
};

addStudent(classroom, "Diana", [88, 90, 85]);

// 2. Add grade
const addGrade = (classroom, studentName, grade) => {
    const student = classroom.students.find(s => s.name === studentName);
    if (student) {
        student.grades.push(grade);
        return true;
    }
    return false;
};

addGrade(classroom, "Alice", 92);

// 3. Calculate averages
const calculateAverages = (classroom) => {
    return classroom.students.map(student => ({
        name: student.name,
        average: student.grades.reduce((a, b) => a + b, 0) / student.grades.length
    }));
};

const averages = calculateAverages(classroom);
console.log("Averages:", averages);

// 4. Find top student
const findTopStudent = (classroom) => {
    const avgs = calculateAverages(classroom);
    return avgs.reduce((top, student) =>
        student.average > top.average ? student : top
    );
};

const topStudent = findTopStudent(classroom);
console.log(`Top student: ${topStudent.name} with average ${topStudent.average.toFixed(2)}`);
```

**Key Insights:**
- Nested structures combine lists and dictionaries
- Access with chaining: `classroom["students"][0]["name"]`
- JavaScript's `find()` and `map()` simplify operations
- Always check for empty arrays before averaging

---

## Exercise 8: Matrix Operations

**Task:** Implement matrix operations with nested lists

### Python Solution

```python
matrix = [
    [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9]
]

# 1. Get element
def get(matrix, row, col):
    return matrix[row][col]

print(get(matrix, 1, 2))  # 6

# 2. Transpose
def transpose(matrix):
    rows = len(matrix)
    cols = len(matrix[0])
    result = [[matrix[r][c] for r in range(rows)] for c in range(cols)]
    return result

# Alternative using zip
def transpose_zip(matrix):
    return [list(row) for row in zip(*matrix)]

transposed = transpose(matrix)
print("Transposed:")
for row in transposed:
    print(row)
# [[1, 4, 7],
#  [2, 5, 8],
#  [3, 6, 9]]

# 3. Add matrices
def add_matrices(A, B):
    rows = len(A)
    cols = len(A[0])
    result = [[A[r][c] + B[r][c] for c in range(cols)] for r in range(rows)]
    return result

matrix2 = [
    [9, 8, 7],
    [6, 5, 4],
    [3, 2, 1]
]

result = add_matrices(matrix, matrix2)
print("Sum:")
for row in result:
    print(row)

# 4. Print nicely
def print_matrix(matrix):
    for row in matrix:
        print(" ".join(f"{val:4}" for val in row))

print_matrix(matrix)

# Bonus: Matrix multiplication
def multiply_matrices(A, B):
    rows_A = len(A)
    cols_A = len(A[0])
    cols_B = len(B[0])

    # Result is rows_A x cols_B
    result = [[0 for _ in range(cols_B)] for _ in range(rows_A)]

    for i in range(rows_A):
        for j in range(cols_B):
            for k in range(cols_A):
                result[i][j] += A[i][k] * B[k][j]

    return result

product = multiply_matrices(matrix, matrix2)
print("Product:")
print_matrix(product)
```

### Haskell Solution

```haskell
type Matrix = [[Int]]

matrix :: Matrix
matrix = [[1, 2, 3],
          [4, 5, 6],
          [7, 8, 9]]

-- 1. Get element
getElem :: Matrix -> Int -> Int -> Int
getElem m row col = (m !! row) !! col

-- 2. Transpose
transposeMatrix :: Matrix -> Matrix
transposeMatrix ([]:_) = []
transposeMatrix m = map head m : transposeMatrix (map tail m)

-- Or using built-in
import Data.List (transpose)
transposeBuiltin :: Matrix -> Matrix
transposeBuiltin = transpose

-- 3. Add matrices
addMatrices :: Matrix -> Matrix -> Matrix
addMatrices = zipWith (zipWith (+))

-- 4. Print matrix
printMatrix :: Matrix -> IO ()
printMatrix = mapM_ print

main :: IO ()
main = do
    putStrLn "Original:"
    printMatrix matrix
    putStrLn "\nTransposed:"
    printMatrix (transpose matrix)
```

**Key Insights:**
- Matrices are lists of lists (rows of columns)
- Transpose swaps rows and columns
- Matrix multiplication: `result[i][j] = sum(A[i][k] * B[k][j])`
- List comprehensions make matrix operations concise

---

## Exercise 9: Word Frequency Counter

**Task:** Count word frequencies in text

### Python Solution

```python
def word_frequency(text):
    """Count word frequencies"""
    # Basic version
    words = text.split()
    freq = {}
    for word in words:
        freq[word] = freq.get(word, 0) + 1
    return freq

# Bonus: Ignore case and punctuation
import re

def word_frequency_clean(text):
    """Count with normalization"""
    # Convert to lowercase and remove punctuation
    words = re.findall(r'\b\w+\b', text.lower())
    freq = {}
    for word in words:
        freq[word] = freq.get(word, 0) + 1
    return freq

# Sort by frequency
def print_frequencies(freq):
    """Print sorted by frequency (descending)"""
    sorted_items = sorted(freq.items(), key=lambda x: x[1], reverse=True)
    for word, count in sorted_items:
        print(f"  {word}: {count}")

# Test
text = "the quick brown fox jumps over the lazy dog the"
freq = word_frequency(text)
print("Word frequencies:")
print_frequencies(freq)

# Using Counter (built-in)
from collections import Counter

def word_frequency_counter(text):
    words = text.lower().split()
    return Counter(words)

freq2 = word_frequency_counter(text)
print("\nUsing Counter:")
for word, count in freq2.most_common():
    print(f"  {word}: {count}")
```

### JavaScript Solution

```javascript
function wordFrequency(text) {
    const words = text.toLowerCase().split(/\s+/);
    const freq = {};

    for (const word of words) {
        freq[word] = (freq[word] || 0) + 1;
    }

    return freq;
}

// Clean version
function wordFrequencyClean(text) {
    const words = text.toLowerCase().match(/\b\w+\b/g) || [];
    const freq = {};

    for (const word of words) {
        freq[word] = (freq[word] || 0) + 1;
    }

    return freq;
}

// Sort by frequency
function printFrequencies(freq) {
    const sorted = Object.entries(freq)
        .sort((a, b) => b[1] - a[1]);

    for (const [word, count] of sorted) {
        console.log(`  ${word}: ${count}`);
    }
}

// Test
const text = "the quick brown fox jumps over the lazy dog the";
const freq = wordFrequency(text);
console.log("Word frequencies:");
printFrequencies(freq);

// Using Map
function wordFrequencyMap(text) {
    const words = text.toLowerCase().match(/\b\w+\b/g) || [];
    const freq = new Map();

    for (const word of words) {
        freq.set(word, (freq.get(word) || 0) + 1);
    }

    return freq;
}
```

**Key Insights:**
- Dictionaries/maps perfect for counting
- `get(key, default)` pattern is common
- Sorting by value requires converting to list/array
- Regular expressions help clean input
- Python's Counter is purpose-built for this

---

## Exercise 10: Shopping Cart

**Task:** Implement shopping cart operations

### Python Solution (Mutable)

```python
# Shopping cart: list of items
# Each item: {"name": str, "price": float, "quantity": int}

def add_item(cart, name, price, quantity):
    """Add or update item"""
    # Check if item exists
    for item in cart:
        if item["name"] == name:
            item["quantity"] += quantity
            return cart

    # Item doesn't exist, add it
    cart.append({"name": name, "price": price, "quantity": quantity})
    return cart

def remove_item(cart, name):
    """Remove item by name"""
    for i, item in enumerate(cart):
        if item["name"] == name:
            cart.pop(i)
            return cart
    return cart

def update_quantity(cart, name, quantity):
    """Update quantity"""
    for item in cart:
        if item["name"] == name:
            item["quantity"] = quantity
            return cart
    return cart

def calculate_total(cart):
    """Sum of price × quantity"""
    return sum(item["price"] * item["quantity"] for item in cart)

def apply_discount(cart, percentage):
    """Reduce all prices"""
    for item in cart:
        item["price"] *= (1 - percentage / 100)
    return cart

# Test
cart = []
add_item(cart, "Apple", 0.50, 10)
add_item(cart, "Banana", 0.30, 5)
add_item(cart, "Orange", 0.60, 8)

print(f"Total: ${calculate_total(cart):.2f}")  # $11.80

apply_discount(cart, 10)  # 10% off
print(f"After discount: ${calculate_total(cart):.2f}")  # $10.62

update_quantity(cart, "Apple", 5)
print(f"After update: ${calculate_total(cart):.2f}")  # $7.87

remove_item(cart, "Banana")
print(f"After removal: ${calculate_total(cart):.2f}")  # $7.20
```

### Haskell Solution (Immutable)

```haskell
data Item = Item
    { itemName :: String
    , itemPrice :: Double
    , itemQuantity :: Int
    } deriving (Show)

type Cart = [Item]

-- Add or update item
addItem :: Cart -> String -> Double -> Int -> Cart
addItem cart name price qty =
    case findItem cart name of
        Just item -> updateQuantity cart name (itemQuantity item + qty)
        Nothing   -> Item name price qty : cart

findItem :: Cart -> String -> Maybe Item
findItem cart name = find (\item -> itemName item == name) cart
  where
    find _ [] = Nothing
    find pred (x:xs)
        | pred x    = Just x
        | otherwise = find pred xs

-- Remove item
removeItem :: Cart -> String -> Cart
removeItem cart name = filter (\item -> itemName item /= name) cart

-- Update quantity
updateQuantity :: Cart -> String -> Int -> Cart
updateQuantity cart name newQty =
    map updateIfMatch cart
  where
    updateIfMatch item
        | itemName item == name = item { itemQuantity = newQty }
        | otherwise = item

-- Calculate total
calculateTotal :: Cart -> Double
calculateTotal cart =
    sum [itemPrice item * fromIntegral (itemQuantity item) | item <- cart]

-- Apply discount
applyDiscount :: Cart -> Double -> Cart
applyDiscount cart percentage =
    map discountItem cart
  where
    discountItem item =
        item { itemPrice = itemPrice item * (1 - percentage / 100) }

-- Test
main :: IO ()
main = do
    let cart1 = []
    let cart2 = addItem cart1 "Apple" 0.50 10
    let cart3 = addItem cart2 "Banana" 0.30 5
    let cart4 = addItem cart3 "Orange" 0.60 8

    putStrLn $ "Total: $" ++ show (calculateTotal cart4)

    let cart5 = applyDiscount cart4 10
    putStrLn $ "After discount: $" ++ show (calculateTotal cart5)
```

**Key Insights:**
- Mutable: Modify cart in place
- Immutable: Return new cart each time
- Immutable is safer but more verbose
- Both approaches work, choose based on paradigm
- Functional programming emphasizes transformations

---

## Summary

Data structures are fundamental to programming:

- **Lists/Arrays**: Sequential collections
- **Dictionaries/Maps**: Key-value pairs
- **Sets**: Unique elements
- **Nested structures**: Combine primitives

**Mutability vs Immutability:**
- **Mutable**: Efficient, but requires care to avoid bugs
- **Immutable**: Safer, easier to reason about, but less efficient for updates

**Language Differences:**
- **Python**: Mutable by default, easy to use
- **JavaScript**: Mutable, but spread operator enables immutable patterns
- **Haskell**: Immutable by default, persistent data structures
- **Java**: Mutable collections, verbose but explicit

Understanding these tradeoffs makes you a better programmer across all paradigms!

Continue to Lesson 6 to explore **Recursion** in depth.
