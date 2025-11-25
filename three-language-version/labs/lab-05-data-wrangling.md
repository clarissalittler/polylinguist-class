# Lab 5: Data Wrangling

**Quarter 1, Week 5**
**Duration:** 90 minutes
**Format:** Pair programming

## Overview

Real programs work with collections of data. In this lab, you'll practice manipulating lists, dictionaries, and other data structures while learning to think about data transformations.

## Objectives

By the end of this lab, you will:
- [ ] Create and manipulate lists/arrays in all three languages
- [ ] Use dictionaries/maps for key-value data
- [ ] Transform data using iteration and functional operations
- [ ] Read data from a file

## Setup

- Partner up
- Create folder: `lab05-data/`
- Download the sample data file (or create it - see below)

**Create `students.txt`:**
```
Alice,85,92,78
Bob,90,88,95
Charlie,72,68,75
Diana,95,97,100
Eve,80,82,79
```

---

## Part 1: Working with Lists (25 minutes)

### Activity 1.1: List Basics

Create a list of numbers and perform basic operations.

**Python:**
```python
# Creating lists
numbers = [5, 2, 8, 1, 9, 3]

# Basic operations
print(f"Length: {len(numbers)}")
print(f"First element: {numbers[0]}")
print(f"Last element: {numbers[-1]}")
print(f"Slice [1:4]: {numbers[1:4]}")

# Modifying
numbers.append(7)
numbers.insert(0, 10)
numbers.remove(2)  # Remove first occurrence of 2
print(f"After modifications: {numbers}")

# Useful functions
print(f"Sum: {sum(numbers)}")
print(f"Min: {min(numbers)}")
print(f"Max: {max(numbers)}")
print(f"Sorted: {sorted(numbers)}")
```

**C++:**
```cpp
#include <iostream>
#include <vector>
#include <algorithm>
#include <numeric>
using namespace std;

int main() {
    vector<int> numbers = {5, 2, 8, 1, 9, 3};

    // Basic operations
    cout << "Length: " << numbers.size() << endl;
    cout << "First: " << numbers[0] << endl;
    cout << "Last: " << numbers.back() << endl;

    // Modifying
    numbers.push_back(7);
    numbers.insert(numbers.begin(), 10);

    // Remove first occurrence of 2
    auto it = find(numbers.begin(), numbers.end(), 2);
    if (it != numbers.end()) numbers.erase(it);

    // Useful functions
    cout << "Sum: " << accumulate(numbers.begin(), numbers.end(), 0) << endl;
    cout << "Min: " << *min_element(numbers.begin(), numbers.end()) << endl;
    cout << "Max: " << *max_element(numbers.begin(), numbers.end()) << endl;

    sort(numbers.begin(), numbers.end());
    cout << "Sorted: ";
    for (int n : numbers) cout << n << " ";
    cout << endl;

    return 0;
}
```

**Haskell:**
```haskell
import Data.List (sort)

main :: IO ()
main = do
    let numbers = [5, 2, 8, 1, 9, 3]

    -- Basic operations
    putStrLn $ "Length: " ++ show (length numbers)
    putStrLn $ "First: " ++ show (head numbers)
    putStrLn $ "Last: " ++ show (last numbers)
    putStrLn $ "Take 3: " ++ show (take 3 numbers)
    putStrLn $ "Drop 2: " ++ show (drop 2 numbers)

    -- Building new lists (immutable!)
    let withSeven = numbers ++ [7]
    let withTenFirst = 10 : numbers

    -- Useful functions
    putStrLn $ "Sum: " ++ show (sum numbers)
    putStrLn $ "Min: " ++ show (minimum numbers)
    putStrLn $ "Max: " ++ show (maximum numbers)
    putStrLn $ "Sorted: " ++ show (sort numbers)
```

### Activity 1.2: List Comprehensions/Transformations

Create a new list by transforming each element.

**Python** (list comprehension):
```python
numbers = [1, 2, 3, 4, 5]

# Square each number
squares = [x**2 for x in numbers]
print(squares)  # [1, 4, 9, 16, 25]

# Filter even numbers
evens = [x for x in numbers if x % 2 == 0]
print(evens)  # [2, 4]

# Combined: square of evens
even_squares = [x**2 for x in numbers if x % 2 == 0]
print(even_squares)  # [4, 16]
```

**C++:**
```cpp
#include <iostream>
#include <vector>
#include <algorithm>
using namespace std;

int main() {
    vector<int> numbers = {1, 2, 3, 4, 5};

    // Square each number
    vector<int> squares;
    transform(numbers.begin(), numbers.end(), back_inserter(squares),
              [](int x) { return x * x; });

    // Filter even numbers
    vector<int> evens;
    copy_if(numbers.begin(), numbers.end(), back_inserter(evens),
            [](int x) { return x % 2 == 0; });

    // Print
    for (int s : squares) cout << s << " ";
    cout << endl;

    return 0;
}
```

**Haskell** (very natural!):
```haskell
main :: IO ()
main = do
    let numbers = [1, 2, 3, 4, 5]

    -- Square each number
    let squares = [x^2 | x <- numbers]
    -- Or: let squares = map (^2) numbers
    print squares  -- [1,4,9,16,25]

    -- Filter even numbers
    let evens = [x | x <- numbers, even x]
    -- Or: let evens = filter even numbers
    print evens  -- [2,4]

    -- Combined
    let evenSquares = [x^2 | x <- numbers, even x]
    print evenSquares  -- [4,16]
```

### Activity 1.3: Practice Problems

Solve these list problems in at least 2 languages:

1. **Double all elements:** `[1,2,3]` → `[2,4,6]`
2. **Keep only positive:** `[-1, 2, -3, 4]` → `[2, 4]`
3. **Get lengths of strings:** `["hi", "hello", "hey"]` → `[2, 5, 3]`
4. **Sum of squares:** `[1,2,3]` → `14` (1+4+9)

### ✅ Checkpoint 1

Verify:
- [ ] Can create and modify lists in all 3 languages
- [ ] Solved at least 2 practice problems

---

## Part 2: Dictionaries/Maps (20 minutes)

### Activity 2.1: Dictionary Basics

**Python:**
```python
# Creating dictionaries
student = {
    "name": "Alice",
    "age": 20,
    "major": "Computer Science"
}

# Accessing values
print(student["name"])      # Alice
print(student.get("gpa", 0))  # 0 (default if missing)

# Modifying
student["age"] = 21
student["gpa"] = 3.8

# Iterating
for key, value in student.items():
    print(f"{key}: {value}")

# Checking membership
if "name" in student:
    print("Has name!")
```

**C++:**
```cpp
#include <iostream>
#include <map>
#include <string>
using namespace std;

int main() {
    map<string, string> student;
    student["name"] = "Alice";
    student["age"] = "20";
    student["major"] = "Computer Science";

    // Accessing
    cout << student["name"] << endl;

    // Iterating
    for (auto& [key, value] : student) {
        cout << key << ": " << value << endl;
    }

    // Check if key exists
    if (student.find("name") != student.end()) {
        cout << "Has name!" << endl;
    }

    return 0;
}
```

**Haskell** (using Data.Map):
```haskell
import qualified Data.Map as Map

main :: IO ()
main = do
    let student = Map.fromList [("name", "Alice"),
                                 ("age", "20"),
                                 ("major", "Computer Science")]

    -- Accessing
    putStrLn $ case Map.lookup "name" student of
        Just n  -> n
        Nothing -> "Not found"

    -- Map is immutable - "insert" returns new map
    let student' = Map.insert "gpa" "3.8" student

    -- Iterating
    mapM_ (\(k, v) -> putStrLn $ k ++ ": " ++ v) (Map.toList student')
```

### Activity 2.2: Word Counter

Count word frequencies in a string.

**Python:**
```python
def count_words(text):
    words = text.lower().split()
    counts = {}
    for word in words:
        counts[word] = counts.get(word, 0) + 1
    return counts

text = "the quick brown fox jumps over the lazy dog the fox"
print(count_words(text))
# {'the': 3, 'quick': 1, 'brown': 1, 'fox': 2, ...}
```

**Your task:** Implement word counting in C++ or Haskell.

### Activity 2.3: Grade Book

Create a simple grade book.

**Python:**
```python
grades = {}

def add_student(name, scores):
    grades[name] = scores

def get_average(name):
    if name in grades:
        return sum(grades[name]) / len(grades[name])
    return None

def get_class_average():
    all_averages = [get_average(name) for name in grades]
    return sum(all_averages) / len(all_averages)

# Test it
add_student("Alice", [85, 92, 78])
add_student("Bob", [90, 88, 95])
add_student("Charlie", [72, 68, 75])

print(f"Alice's average: {get_average('Alice'):.1f}")
print(f"Class average: {get_class_average():.1f}")
```

### ✅ Checkpoint 2

Verify:
- [ ] Can use dictionaries/maps in at least 2 languages
- [ ] Word counter or grade book works

---

## Part 3: File Processing (25 minutes)

### Activity 3.1: Reading a File

Read the `students.txt` file and parse the data.

**Python:**
```python
def read_students(filename):
    students = []
    with open(filename, 'r') as f:
        for line in f:
            parts = line.strip().split(',')
            name = parts[0]
            scores = [int(x) for x in parts[1:]]
            students.append({'name': name, 'scores': scores})
    return students

students = read_students('students.txt')
for s in students:
    avg = sum(s['scores']) / len(s['scores'])
    print(f"{s['name']}: {avg:.1f}")
```

**C++:**
```cpp
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
using namespace std;

struct Student {
    string name;
    vector<int> scores;
};

vector<Student> readStudents(const string& filename) {
    vector<Student> students;
    ifstream file(filename);
    string line;

    while (getline(file, line)) {
        Student s;
        stringstream ss(line);
        string token;

        getline(ss, s.name, ',');
        while (getline(ss, token, ',')) {
            s.scores.push_back(stoi(token));
        }
        students.push_back(s);
    }
    return students;
}

int main() {
    auto students = readStudents("students.txt");
    for (const auto& s : students) {
        double avg = 0;
        for (int score : s.scores) avg += score;
        avg /= s.scores.size();
        cout << s.name << ": " << avg << endl;
    }
    return 0;
}
```

**Haskell:**
```haskell
import Data.List.Split (splitOn)

data Student = Student { name :: String, scores :: [Int] }
    deriving Show

parseStudent :: String -> Student
parseStudent line = Student n (map read ss)
  where
    parts = splitOn "," line
    n = head parts
    ss = tail parts

main :: IO ()
main = do
    contents <- readFile "students.txt"
    let students = map parseStudent (lines contents)
    mapM_ printStudent students
  where
    printStudent s = putStrLn $ name s ++ ": " ++ show (average (scores s))
    average xs = fromIntegral (sum xs) / fromIntegral (length xs)
```

*Note: For Haskell, you may need `cabal install split` or use a different parsing approach.*

### Activity 3.2: Data Analysis

Using the student data, calculate:

1. **Class average** for each test
2. **Highest scoring student** overall
3. **Students above class average**

**Python:**
```python
students = read_students('students.txt')

# 1. Average per test
num_tests = len(students[0]['scores'])
for i in range(num_tests):
    test_scores = [s['scores'][i] for s in students]
    avg = sum(test_scores) / len(test_scores)
    print(f"Test {i+1} average: {avg:.1f}")

# 2. Highest scoring student
def overall_avg(student):
    return sum(student['scores']) / len(student['scores'])

top_student = max(students, key=overall_avg)
print(f"Top student: {top_student['name']} ({overall_avg(top_student):.1f})")

# 3. Above average students
class_avg = sum(overall_avg(s) for s in students) / len(students)
above_avg = [s['name'] for s in students if overall_avg(s) > class_avg]
print(f"Above average: {above_avg}")
```

### Activity 3.3: Write Results to File

Save the analysis to an output file.

**Python:**
```python
def save_report(students, filename):
    with open(filename, 'w') as f:
        f.write("GRADE REPORT\n")
        f.write("=" * 30 + "\n\n")

        for s in students:
            avg = sum(s['scores']) / len(s['scores'])
            f.write(f"{s['name']}: {avg:.1f}\n")

        # Overall stats
        all_avgs = [sum(s['scores'])/len(s['scores']) for s in students]
        f.write(f"\nClass Average: {sum(all_avgs)/len(all_avgs):.1f}\n")

save_report(students, 'report.txt')
print("Report saved!")
```

### ✅ Checkpoint 3

Verify:
- [ ] Can read the students file in at least 1 language
- [ ] Calculated at least one statistic (class average, top student, etc.)

---

## Part 4: Putting It Together (15 minutes)

### Activity 4.1: Mini Data Pipeline

Build a complete data processing pipeline:

1. Read raw data from file
2. Clean/transform the data
3. Analyze and compute statistics
4. Output results

**Example: Process survey data**

Create `survey.txt`:
```
What's your favorite language?
Python
python
PYTHON
C++
c++
Haskell
Python
JavaScript
java
Java
haskell
Python
```

**Python solution:**
```python
def process_survey(filename):
    # 1. Read data
    with open(filename, 'r') as f:
        lines = f.readlines()

    # Skip header, clean data
    responses = [line.strip().lower() for line in lines[1:] if line.strip()]

    # 2. Count responses
    counts = {}
    for response in responses:
        counts[response] = counts.get(response, 0) + 1

    # 3. Sort by count
    sorted_results = sorted(counts.items(), key=lambda x: -x[1])

    # 4. Output
    print("Survey Results")
    print("=" * 30)
    total = len(responses)
    for lang, count in sorted_results:
        pct = count / total * 100
        print(f"{lang}: {count} ({pct:.1f}%)")

process_survey('survey.txt')
```

### Activity 4.2: Challenge - Your Own Analysis

Choose a dataset (or create one) and analyze it:

**Ideas:**
- Weather data (temp, humidity, conditions)
- Game scores (player, score, level)
- Book data (title, author, pages, rating)
- Music playlist (song, artist, duration, plays)

---

## Extensions

### Extension 1: CSV Library

Use Python's `csv` module for cleaner file handling:

```python
import csv

with open('students.csv', 'r') as f:
    reader = csv.DictReader(f)
    for row in reader:
        print(row)
```

### Extension 2: JSON Data

Work with JSON format:

```python
import json

data = {
    "students": [
        {"name": "Alice", "scores": [85, 92, 78]},
        {"name": "Bob", "scores": [90, 88, 95]}
    ]
}

# Write JSON
with open('data.json', 'w') as f:
    json.dump(data, f, indent=2)

# Read JSON
with open('data.json', 'r') as f:
    loaded = json.load(f)
```

### Extension 3: Data Visualization

If you have matplotlib installed:

```python
import matplotlib.pyplot as plt

names = ["Alice", "Bob", "Charlie"]
averages = [85.0, 91.0, 71.7]

plt.bar(names, averages)
plt.ylabel('Average Score')
plt.title('Student Averages')
plt.savefig('grades.png')
```

---

## Wrap-Up

**Key takeaways:**

1. **Lists** are ordered collections; use them when order matters
2. **Dictionaries/Maps** are key-value stores; use them for lookups
3. **File I/O** follows a similar pattern: open, read/write, close
4. **Data processing** often follows: read → transform → analyze → output
5. **Haskell's immutability** means "modifying" creates new data structures

**Next lab:** We'll explore recursion and recursive thinking!
