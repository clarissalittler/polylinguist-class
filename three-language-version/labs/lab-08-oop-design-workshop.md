# Lab 8: OOP Design Workshop

**Quarter 1, Week 8**
**Duration:** 90 minutes
**Format:** Pair programming and design exercise

## Overview

Object-Oriented Programming (OOP) is about organizing code around "things" (objects) that have data (attributes) and behaviors (methods). This lab focuses on designing good class hierarchies before writing code.

## Objectives

By the end of this lab, you will:
- [ ] Design a class hierarchy on paper
- [ ] Implement classes in Python and C++
- [ ] Understand inheritance and composition
- [ ] Know when to use OOP (and when not to)

## Setup

- Partner up
- Create folder: `lab08-oop/`
- Paper and pencil for design work

---

## Part 1: OOP Concepts Review (15 minutes)

### Activity 1.1: The Four Pillars

Discuss with your partner:

| Pillar | Meaning | Example |
|--------|---------|---------|
| **Encapsulation** | Bundling data and methods; hiding internal details | A `BankAccount` class hides balance calculations |
| **Abstraction** | Exposing only necessary interface | You drive a `Car` without knowing engine internals |
| **Inheritance** | Creating new classes from existing ones | A `Dog` IS-A `Animal` |
| **Polymorphism** | Different types responding to same interface | All `Shape` objects can `draw()` |

### Activity 1.2: Class Anatomy

**Python:**
```python
class Dog:
    # Class attribute (shared by all dogs)
    species = "Canis familiaris"

    # Constructor (initializer)
    def __init__(self, name, age):
        # Instance attributes (unique to each dog)
        self.name = name
        self.age = age

    # Instance method
    def bark(self):
        return f"{self.name} says woof!"

    # String representation
    def __str__(self):
        return f"{self.name}, age {self.age}"

# Create objects
buddy = Dog("Buddy", 5)
print(buddy.bark())  # Buddy says woof!
print(buddy)         # Buddy, age 5
```

**C++:**
```cpp
#include <iostream>
#include <string>
using namespace std;

class Dog {
private:
    string name;
    int age;

public:
    // Constructor
    Dog(string n, int a) : name(n), age(a) {}

    // Method
    string bark() {
        return name + " says woof!";
    }

    // Getter
    string getName() { return name; }
    int getAge() { return age; }
};

int main() {
    Dog buddy("Buddy", 5);
    cout << buddy.bark() << endl;
    return 0;
}
```

### Activity 1.3: Haskell's Approach (No Classes!)

Haskell doesn't have OOP classes, but has **type classes** and **data types**:

```haskell
-- Define a data type (like a struct)
data Dog = Dog { dogName :: String, dogAge :: Int }
    deriving (Show)

-- Define behavior as regular functions
bark :: Dog -> String
bark dog = dogName dog ++ " says woof!"

main :: IO ()
main = do
    let buddy = Dog "Buddy" 5
    putStrLn (bark buddy)  -- Buddy says woof!
```

**Discussion:** How does Haskell achieve similar organization without classes?

### ✅ Checkpoint 1

Verify with partner:
- [ ] Can explain encapsulation and inheritance
- [ ] Understand the basic class structure in Python and C++

---

## Part 2: Design Before Coding (25 minutes)

### Activity 2.1: The Problem - Library System

You're designing a library management system. It needs to track:
- Books (title, author, ISBN, available copies)
- Members (name, ID, borrowed books)
- Loans (which member borrowed which book, when, due date)

### Activity 2.2: Identify the Nouns

**Step 1:** List all the "things" (potential classes):

```
Things in our system:
- Book
- Member
- Loan
- Library (maybe?)
- Author (maybe separate class?)
```

**Step 2:** For each thing, list attributes:

```
Book:
- title (string)
- author (string)
- isbn (string)
- total_copies (int)
- available_copies (int)

Member:
- name (string)
- member_id (string)
- borrowed_books (list of Books)

Loan:
- book (Book)
- member (Member)
- borrow_date (date)
- due_date (date)
- returned (bool)
```

### Activity 2.3: Identify the Verbs (Methods)

**Step 3:** List actions each thing can do:

```
Book:
- is_available() -> bool
- borrow_copy() -> decrease available
- return_copy() -> increase available

Member:
- borrow(book) -> create loan
- return_book(book) -> complete loan
- list_borrowed() -> show current books

Loan:
- is_overdue() -> check if past due
- calculate_fine() -> if overdue
```

### Activity 2.4: Draw the Diagram

On paper, draw boxes for each class with:
- Class name at top
- Attributes in middle
- Methods at bottom
- Lines showing relationships (arrows for "has-a", triangles for "is-a")

```
+------------------+
|      Book        |
+------------------+
| - title          |
| - author         |
| - isbn           |
| - total_copies   |
| - available      |
+------------------+
| + is_available() |
| + borrow_copy()  |
| + return_copy()  |
+------------------+
        |
        | (borrowed by)
        v
+------------------+
|      Member      |
+------------------+
| - name           |
| - member_id      |
| - borrowed_books |
+------------------+
| + borrow(book)   |
| + return_book()  |
| + list_borrowed()|
+------------------+
```

### ✅ Checkpoint 2

Show instructor your design:
- [ ] Classes identified with attributes and methods
- [ ] Relationships drawn between classes

---

## Part 3: Implement the Design (30 minutes)

### Activity 3.1: Basic Book Class

**Python:**
```python
from datetime import date, timedelta

class Book:
    def __init__(self, title, author, isbn, copies=1):
        self.title = title
        self.author = author
        self.isbn = isbn
        self.total_copies = copies
        self.available_copies = copies

    def is_available(self):
        return self.available_copies > 0

    def borrow_copy(self):
        if self.is_available():
            self.available_copies -= 1
            return True
        return False

    def return_copy(self):
        if self.available_copies < self.total_copies:
            self.available_copies += 1
            return True
        return False

    def __str__(self):
        return f"{self.title} by {self.author}"

    def __repr__(self):
        return f"Book('{self.title}', '{self.author}', '{self.isbn}')"

# Test
book = Book("1984", "George Orwell", "978-0451524935", copies=3)
print(book)                    # 1984 by George Orwell
print(book.is_available())     # True
print(book.borrow_copy())      # True
print(book.available_copies)   # 2
```

### Activity 3.2: Member Class

**Your turn!** Implement the Member class:

```python
class Member:
    def __init__(self, name, member_id):
        self.name = name
        self.member_id = member_id
        self.borrowed_books = []

    def borrow(self, book):
        # TODO: Check if book is available
        # TODO: If yes, borrow_copy() and add to borrowed_books
        # TODO: Return True/False
        pass

    def return_book(self, book):
        # TODO: Check if book is in borrowed_books
        # TODO: If yes, return_copy() and remove from list
        # TODO: Return True/False
        pass

    def list_borrowed(self):
        # TODO: Return list of borrowed book titles
        pass
```

### Activity 3.3: C++ Implementation

```cpp
#include <iostream>
#include <string>
#include <vector>
using namespace std;

class Book {
private:
    string title;
    string author;
    string isbn;
    int totalCopies;
    int availableCopies;

public:
    Book(string t, string a, string i, int copies = 1)
        : title(t), author(a), isbn(i),
          totalCopies(copies), availableCopies(copies) {}

    bool isAvailable() { return availableCopies > 0; }

    bool borrowCopy() {
        if (isAvailable()) {
            availableCopies--;
            return true;
        }
        return false;
    }

    bool returnCopy() {
        if (availableCopies < totalCopies) {
            availableCopies++;
            return true;
        }
        return false;
    }

    string getTitle() { return title; }
    string getAuthor() { return author; }

    void print() {
        cout << title << " by " << author << endl;
    }
};

int main() {
    Book book("1984", "George Orwell", "978-0451524935", 3);
    book.print();
    cout << "Available: " << book.isAvailable() << endl;
    book.borrowCopy();
    cout << "Borrowed a copy" << endl;
    return 0;
}
```

**Your task:** Implement Member class in C++.

### ✅ Checkpoint 3

Verify:
- [ ] Book class works in Python
- [ ] Member can borrow and return books
- [ ] At least one class works in C++

---

## Part 4: Inheritance (15 minutes)

### Activity 4.1: Different Types of Library Items

Libraries don't just have books! Let's add DVDs and Magazines.

**The Problem:** Books, DVDs, and Magazines share some attributes but differ in others.

**The Solution:** Create a base class `LibraryItem`:

```python
class LibraryItem:
    """Base class for all library items"""
    def __init__(self, title, item_id, copies=1):
        self.title = title
        self.item_id = item_id
        self.total_copies = copies
        self.available_copies = copies

    def is_available(self):
        return self.available_copies > 0

    def borrow_copy(self):
        if self.is_available():
            self.available_copies -= 1
            return True
        return False

    def return_copy(self):
        if self.available_copies < self.total_copies:
            self.available_copies += 1
            return True
        return False

    def get_loan_period(self):
        """Override in subclasses"""
        return 14  # Default 2 weeks


class Book(LibraryItem):
    """Book extends LibraryItem"""
    def __init__(self, title, author, isbn, copies=1):
        super().__init__(title, isbn, copies)  # Call parent constructor
        self.author = author

    def get_loan_period(self):
        return 21  # Books: 3 weeks

    def __str__(self):
        return f"Book: {self.title} by {self.author}"


class DVD(LibraryItem):
    """DVD extends LibraryItem"""
    def __init__(self, title, director, runtime, copies=1):
        super().__init__(title, f"DVD-{title[:3].upper()}", copies)
        self.director = director
        self.runtime = runtime  # in minutes

    def get_loan_period(self):
        return 7  # DVDs: 1 week

    def __str__(self):
        return f"DVD: {self.title} ({self.runtime} min)"


class Magazine(LibraryItem):
    """Magazine extends LibraryItem"""
    def __init__(self, title, issue, copies=1):
        super().__init__(title, f"MAG-{issue}", copies)
        self.issue = issue

    def get_loan_period(self):
        return 7  # Magazines: 1 week

    def __str__(self):
        return f"Magazine: {self.title}, Issue {self.issue}"


# Test polymorphism
items = [
    Book("1984", "George Orwell", "978-0451524935"),
    DVD("Inception", "Christopher Nolan", 148),
    Magazine("National Geographic", "Jan 2024")
]

for item in items:
    print(f"{item} - Loan period: {item.get_loan_period()} days")
```

### Activity 4.2: Inheritance in C++

```cpp
class LibraryItem {
protected:
    string title;
    string itemId;
    int totalCopies;
    int availableCopies;

public:
    LibraryItem(string t, string id, int copies = 1)
        : title(t), itemId(id), totalCopies(copies), availableCopies(copies) {}

    virtual int getLoanPeriod() { return 14; }  // virtual = can override
    virtual void print() { cout << title << endl; }

    bool isAvailable() { return availableCopies > 0; }
};

class Book : public LibraryItem {
private:
    string author;

public:
    Book(string t, string a, string isbn, int copies = 1)
        : LibraryItem(t, isbn, copies), author(a) {}

    int getLoanPeriod() override { return 21; }

    void print() override {
        cout << "Book: " << title << " by " << author << endl;
    }
};
```

### Activity 4.3: Composition vs Inheritance

**Inheritance (IS-A):**
- A Book IS-A LibraryItem
- Use when there's a true "type of" relationship

**Composition (HAS-A):**
- A Library HAS Books
- Use when one object contains/uses another

```python
class Library:
    """Uses composition - Library HAS items and members"""
    def __init__(self, name):
        self.name = name
        self.items = []      # HAS-A relationship
        self.members = []    # HAS-A relationship

    def add_item(self, item):
        self.items.append(item)

    def add_member(self, member):
        self.members.append(member)

    def find_item(self, title):
        for item in self.items:
            if item.title.lower() == title.lower():
                return item
        return None
```

### ✅ Checkpoint 4

Verify:
- [ ] Inheritance hierarchy works (Book extends LibraryItem)
- [ ] Polymorphism demonstrated (different loan periods)

---

## Part 5: Design Principles (5 minutes)

### Quick Tips for Good OOP Design

**1. Single Responsibility**
Each class should have ONE job.
- ✅ `Book` manages book data
- ❌ `Book` handles borrowing, printing reports, and sending emails

**2. Open/Closed**
Classes should be open for extension, closed for modification.
- ✅ Add new item types by creating subclasses
- ❌ Modify LibraryItem every time you add a new type

**3. Prefer Composition Over Inheritance**
Don't create deep inheritance hierarchies.
- ✅ Library HAS items (composition)
- ❌ Library IS-A CollectionManager IS-A DataStore (too deep!)

**4. Don't Overuse OOP!**
Not everything needs to be a class.
- Simple data? Use dictionaries or named tuples
- Utility functions? Module-level functions are fine
- Haskell approach? Often cleaner for data transformation

---

## Challenge Extensions

### Challenge 1: Complete the Library System

Add these features:
- Loan class that tracks borrow date, due date, returns
- Fine calculation for overdue items
- Search functionality (by title, author, etc.)

### Challenge 2: Add a User Interface

Create a simple menu:
```
Library System
1. Browse items
2. Borrow item
3. Return item
4. View my loans
5. Exit
```

### Challenge 3: Haskell Alternative

Design the same system using Haskell data types and functions (no classes):

```haskell
data ItemType = BookType | DVDType | MagazineType
    deriving (Show, Eq)

data LibraryItem = LibraryItem
    { itemTitle :: String
    , itemType :: ItemType
    , available :: Int
    }

getLoanPeriod :: LibraryItem -> Int
getLoanPeriod item = case itemType item of
    BookType -> 21
    DVDType -> 7
    MagazineType -> 7
```

---

## Wrap-Up

**Key takeaways:**

1. **Design first, code second** - Draw before you type
2. **Identify nouns and verbs** - Nouns become classes, verbs become methods
3. **Inheritance for IS-A** - When there's a true type hierarchy
4. **Composition for HAS-A** - When objects contain other objects
5. **OOP isn't always the answer** - Sometimes functions are simpler

**Next lab:** We'll dive into higher-order functions and functional programming!
