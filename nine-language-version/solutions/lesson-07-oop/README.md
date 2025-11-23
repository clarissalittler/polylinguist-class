# Lesson 7: Object-Oriented Programming - Solution Guide

This guide provides example solutions for the OOP exercises.

## General Notes

- **OOP is about modeling**: Use classes to represent real-world concepts
- **Four pillars**: Encapsulation, Inheritance, Polymorphism, Abstraction
- **Language differences**: Some languages are OOP-first (Java), others multi-paradigm (Python)
- **Design matters**: Choose inheritance vs composition carefully

---

## Exercise 1: Basic Class Creation

**Task:** Create a `Book` class with properties and methods

### Python Solution

```python
class Book:
    def __init__(self, title, author, pages):
        self.title = title
        self.author = author
        self.pages = pages
        self.current_page = 0

    def read(self, num_pages):
        self.current_page += num_pages
        if self.current_page > self.pages:
            self.current_page = self.pages

    def percent_complete(self):
        return (self.current_page / self.pages) * 100

    def is_finished(self):
        return self.current_page >= self.pages

    def __str__(self):
        return f"'{self.title}' by {self.author} ({self.current_page}/{self.pages} pages)"

# Test
book = Book("1984", "George Orwell", 328)
book.read(50)
print(f"Progress: {book.percent_complete():.2f}%")  # 15.24%
print(f"Finished: {book.is_finished()}")  # False
book.read(300)
print(f"Finished: {book.is_finished()}")  # True
```

### JavaScript Solution

```javascript
class Book {
    constructor(title, author, pages) {
        this.title = title;
        this.author = author;
        this.pages = pages;
        this.currentPage = 0;
    }

    read(numPages) {
        this.currentPage += numPages;
        if (this.currentPage > this.pages) {
            this.currentPage = this.pages;
        }
    }

    percentComplete() {
        return (this.currentPage / this.pages) * 100;
    }

    isFinished() {
        return this.currentPage >= this.pages;
    }

    toString() {
        return `'${this.title}' by ${this.author} (${this.currentPage}/${this.pages} pages)`;
    }
}

// Test
const book = new Book("1984", "George Orwell", 328);
book.read(50);
console.log(`Progress: ${book.percentComplete().toFixed(2)}%`);
console.log(`Finished: ${book.isFinished()}`);
```

### Java Solution

```java
public class Book {
    private String title;
    private String author;
    private int pages;
    private int currentPage;

    public Book(String title, String author, int pages) {
        this.title = title;
        this.author = author;
        this.pages = pages;
        this.currentPage = 0;
    }

    public void read(int numPages) {
        currentPage += numPages;
        if (currentPage > pages) {
            currentPage = pages;
        }
    }

    public double percentComplete() {
        return ((double) currentPage / pages) * 100;
    }

    public boolean isFinished() {
        return currentPage >= pages;
    }

    @Override
    public String toString() {
        return String.format("'%s' by %s (%d/%d pages)",
            title, author, currentPage, pages);
    }

    public static void main(String[] args) {
        Book book = new Book("1984", "George Orwell", 328);
        book.read(50);
        System.out.printf("Progress: %.2f%%\n", book.percentComplete());
        System.out.println("Finished: " + book.isFinished());
    }
}
```

**Key Insights:**
- Constructor initializes object state
- Methods operate on instance data
- Encapsulation keeps data and behavior together
- Each instance has independent state

---

## Exercise 2: Encapsulation Challenge

**Task:** Create a `Password` class with private data

### Python Solution

```python
import hashlib

class Password:
    def __init__(self, initial_password):
        if len(initial_password) < 8:
            raise ValueError("Password must be at least 8 characters")
        self._password_hash = self._hash(initial_password)

    def _hash(self, password):
        """Private method to hash password"""
        return hashlib.sha256(password.encode()).hexdigest()

    def check(self, password):
        """Check if password matches"""
        return self._hash(password) == self._password_hash

    def change_password(self, old_password, new_password):
        """Change password if old password is correct"""
        if not self.check(old_password):
            return False
        if len(new_password) < 8:
            raise ValueError("New password must be at least 8 characters")
        self._password_hash = self._hash(new_password)
        return True

# Test
pwd = Password("mypassword123")
print(pwd.check("mypassword123"))  # True
print(pwd.check("wrong"))  # False

pwd.change_password("mypassword123", "newsecurepass")
print(pwd.check("newsecurepass"))  # True
print(pwd.check("mypassword123"))  # False
```

### JavaScript Solution (Private Fields)

```javascript
class Password {
    #passwordHash;  // Private field (ES2022)

    constructor(initialPassword) {
        if (initialPassword.length < 8) {
            throw new Error("Password must be at least 8 characters");
        }
        this.#passwordHash = this.#hash(initialPassword);
    }

    #hash(password) {
        // Simple hash for demonstration (use crypto in production)
        return password.split('').reduce((hash, char) => {
            return ((hash << 5) - hash) + char.charCodeAt(0);
        }, 0).toString();
    }

    check(password) {
        return this.#hash(password) === this.#passwordHash;
    }

    changePassword(oldPassword, newPassword) {
        if (!this.check(oldPassword)) {
            return false;
        }
        if (newPassword.length < 8) {
            throw new Error("New password must be at least 8 characters");
        }
        this.#passwordHash = this.#hash(newPassword);
        return true;
    }
}

// Test
const pwd = new Password("mypassword123");
console.log(pwd.check("mypassword123"));  // true
console.log(pwd.check("wrong"));  // false
// console.log(pwd.#passwordHash);  // SyntaxError: Private field
```

### Java Solution

```java
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

public class Password {
    private String passwordHash;

    public Password(String initialPassword) throws Exception {
        if (initialPassword.length() < 8) {
            throw new IllegalArgumentException("Password must be at least 8 characters");
        }
        this.passwordHash = hash(initialPassword);
    }

    private String hash(String password) throws NoSuchAlgorithmException {
        MessageDigest md = MessageDigest.getInstance("SHA-256");
        byte[] hashBytes = md.digest(password.getBytes());
        StringBuilder sb = new StringBuilder();
        for (byte b : hashBytes) {
            sb.append(String.format("%02x", b));
        }
        return sb.toString();
    }

    public boolean check(String password) throws NoSuchAlgorithmException {
        return hash(password).equals(passwordHash);
    }

    public boolean changePassword(String oldPassword, String newPassword)
            throws NoSuchAlgorithmException {
        if (!check(oldPassword)) {
            return false;
        }
        if (newPassword.length() < 8) {
            throw new IllegalArgumentException("New password must be at least 8 characters");
        }
        passwordHash = hash(newPassword);
        return true;
    }
}
```

**Key Insights:**
- Encapsulation hides implementation details
- Python: `_` convention for private (not enforced)
- JavaScript: `#` creates truly private fields
- Java: `private` keyword enforces access control
- Never store passwords in plaintext!

---

## Exercise 3: Inheritance - Shape Hierarchy

**Task:** Create shape class hierarchy with inheritance

### Python Solution

```python
from abc import ABC, abstractmethod
import math

class Shape(ABC):
    def __init__(self, color):
        self.color = color

    @abstractmethod
    def area(self):
        pass

    @abstractmethod
    def perimeter(self):
        pass

    def describe(self):
        print(f"{self.__class__.__name__}: color={self.color}, "
              f"area={self.area():.2f}, perimeter={self.perimeter():.2f}")

class Square(Shape):
    def __init__(self, side, color):
        super().__init__(color)
        self.side = side

    def area(self):
        return self.side ** 2

    def perimeter(self):
        return 4 * self.side

class Circle(Shape):
    def __init__(self, radius, color):
        super().__init__(color)
        self.radius = radius

    def area(self):
        return math.pi * self.radius ** 2

    def perimeter(self):
        return 2 * math.pi * self.radius

class Parallelogram(Shape):
    def __init__(self, base, height, side, color):
        super().__init__(color)
        self.base = base
        self.height = height
        self.side = side

    def area(self):
        return self.base * self.height

    def perimeter(self):
        return 2 * (self.base + self.side)

# Test
shapes = [
    Square(5, "red"),
    Circle(3, "blue"),
    Parallelogram(4, 3, 5, "green")
]

for shape in shapes:
    shape.describe()
```

### Java Solution

```java
abstract class Shape {
    protected String color;

    public Shape(String color) {
        this.color = color;
    }

    public abstract double area();
    public abstract double perimeter();

    public void describe() {
        System.out.printf("%s: color=%s, area=%.2f, perimeter=%.2f\n",
            this.getClass().getSimpleName(), color, area(), perimeter());
    }
}

class Square extends Shape {
    private double side;

    public Square(double side, String color) {
        super(color);
        this.side = side;
    }

    public double area() {
        return side * side;
    }

    public double perimeter() {
        return 4 * side;
    }
}

class Circle extends Shape {
    private double radius;

    public Circle(double radius, String color) {
        super(color);
        this.radius = radius;
    }

    public double area() {
        return Math.PI * radius * radius;
    }

    public double perimeter() {
        return 2 * Math.PI * radius;
    }
}

// Test
public class ShapeDemo {
    public static void main(String[] args) {
        Shape[] shapes = {
            new Square(5, "red"),
            new Circle(3, "blue")
        };

        for (Shape shape : shapes) {
            shape.describe();
        }
    }
}
```

**Key Insights:**
- Abstract base class defines interface
- Subclasses implement specific behavior
- `super()` calls parent constructor
- Polymorphism: treat different shapes uniformly

---

## Exercise 4: Composition vs Inheritance

**Task:** Model vehicles using composition

### Part A: Inheritance Problems

```python
# Inheritance approach - breaks down!
class Vehicle:
    def __init__(self):
        pass

class EngineVehicle(Vehicle):
    def start_engine(self):
        print("Engine started")

class WheeledVehicle(Vehicle):
    def roll(self):
        print("Rolling on wheels")

# Problem: What about motorcycles?
# - Need engine (from EngineVehicle)
# - Need wheels (from WheeledVehicle)
# - Can't inherit from both cleanly

# What about boats? Have engine but no wheels
# What about bicycles? Have wheels but no engine
# Inheritance hierarchy becomes a mess!
```

### Part B: Composition Solution

```python
class Engine:
    def __init__(self, horsepower):
        self.horsepower = horsepower

    def start(self):
        print(f"Engine ({self.horsepower}HP) started")

    def stop(self):
        print("Engine stopped")

class Wheel:
    def __init__(self, diameter):
        self.diameter = diameter

    def roll(self):
        print(f"Wheel (diameter {self.diameter}) rolling")

class Door:
    def __init__(self):
        self.is_open = False

    def open(self):
        self.is_open = True
        print("Door opened")

    def close(self):
        self.is_open = False
        print("Door closed")

# Now compose vehicles from parts
class Car:
    def __init__(self):
        self.engine = Engine(200)
        self.wheels = [Wheel(18) for _ in range(4)]
        self.doors = [Door() for _ in range(4)]

    def start(self):
        self.engine.start()

    def drive(self):
        print("Car driving")
        for wheel in self.wheels:
            wheel.roll()

class Motorcycle:
    def __init__(self):
        self.engine = Engine(100)
        self.wheels = [Wheel(17) for _ in range(2)]
        # No doors!

    def start(self):
        self.engine.start()

    def ride(self):
        print("Motorcycle riding")

class Bicycle:
    def __init__(self):
        self.wheels = [Wheel(26) for _ in range(2)]
        # No engine!
        # No doors!

    def pedal(self):
        print("Pedaling bicycle")

class Boat:
    def __init__(self):
        self.engine = Engine(300)
        # No wheels!
        # No doors!

    def start(self):
        self.engine.start()

    def sail(self):
        print("Boat sailing")

# Test
car = Car()
car.start()
car.drive()

motorcycle = Motorcycle()
motorcycle.start()

bicycle = Bicycle()
bicycle.pedal()  # No engine to start

boat = Boat()
boat.start()  # Has engine but no wheels
```

**Why Composition is Better:**
- **Flexibility**: Mix and match components
- **No forced hierarchy**: Vehicles share only what they actually have
- **Easy to extend**: Add new parts without changing existing code
- **More realistic**: Models real-world assembly
- **Avoids diamond problem**: No multiple inheritance issues

**Key Insight:** "Favor composition over inheritance" - Gang of Four

---

## Exercise 5: Polymorphism with Animals

**Task:** Create animal simulation with polymorphism

### Python Solution

```python
from abc import ABC, abstractmethod

class Animal(ABC):
    def __init__(self, name):
        self.name = name

    @abstractmethod
    def make_sound(self):
        pass

    @abstractmethod
    def move(self):
        pass

class Dog(Animal):
    def make_sound(self):
        print(f"{self.name} barks: Woof!")

    def move(self):
        print(f"{self.name} runs on four legs")

class Fish(Animal):
    def make_sound(self):
        print(f"{self.name} makes bubbles: *blub blub*")

    def move(self):
        print(f"{self.name} swims through water")

class Bird(Animal):
    def make_sound(self):
        print(f"{self.name} chirps: Tweet!")

    def move(self):
        print(f"{self.name} flies in the sky")

class Snake(Animal):
    def make_sound(self):
        print(f"{self.name} hisses: Sssss!")

    def move(self):
        print(f"{self.name} slithers on the ground")

def make_all_animals_act(animals):
    """Polymorphism: works with any Animal"""
    for animal in animals:
        animal.make_sound()
        animal.move()
        print()

# Test
zoo = [
    Dog("Buddy"),
    Fish("Nemo"),
    Bird("Tweety"),
    Snake("Slinky")
]

make_all_animals_act(zoo)
```

### Java Solution

```java
abstract class Animal {
    protected String name;

    public Animal(String name) {
        this.name = name;
    }

    public abstract void makeSound();
    public abstract void move();
}

class Dog extends Animal {
    public Dog(String name) {
        super(name);
    }

    public void makeSound() {
        System.out.println(name + " barks: Woof!");
    }

    public void move() {
        System.out.println(name + " runs on four legs");
    }
}

class Fish extends Animal {
    public Fish(String name) {
        super(name);
    }

    public void makeSound() {
        System.out.println(name + " makes bubbles: *blub blub*");
    }

    public void move() {
        System.out.println(name + " swims through water");
    }
}

// Polymorphic function
class Zoo {
    public static void makeAllAnimalsAct(Animal[] animals) {
        for (Animal animal : animals) {
            animal.makeSound();
            animal.move();
            System.out.println();
        }
    }

    public static void main(String[] args) {
        Animal[] zoo = {
            new Dog("Buddy"),
            new Fish("Nemo")
        };
        makeAllAnimalsAct(zoo);
    }
}
```

**Key Insights:**
- Polymorphism: one interface, many implementations
- Abstract base class defines contract
- Each subclass provides specific behavior
- Client code works with abstraction, not concrete types
- "Program to an interface, not an implementation"

---

## Exercise 7: Bank Account with Transactions

**Task:** Banking system with inheritance

### Python Solution

```python
from datetime import datetime

class BankAccount:
    def __init__(self, account_number):
        self._balance = 0
        self.account_number = account_number
        self._transactions = []

    def deposit(self, amount):
        if amount <= 0:
            raise ValueError("Deposit amount must be positive")
        self._balance += amount
        self._transactions.append({
            'type': 'deposit',
            'amount': amount,
            'timestamp': datetime.now(),
            'balance': self._balance
        })
        return self._balance

    def withdraw(self, amount):
        if amount <= 0:
            raise ValueError("Withdrawal amount must be positive")
        if amount > self._balance:
            raise ValueError("Insufficient funds")
        self._balance -= amount
        self._transactions.append({
            'type': 'withdrawal',
            'amount': amount,
            'timestamp': datetime.now(),
            'balance': self._balance
        })
        return self._balance

    def get_balance(self):
        return self._balance

    def get_transaction_history(self):
        return self._transactions.copy()

class SavingsAccount(BankAccount):
    def __init__(self, account_number, interest_rate):
        super().__init__(account_number)
        self.interest_rate = interest_rate
        self._withdrawals_this_month = 0

    def withdraw(self, amount):
        if self._withdrawals_this_month >= 6:
            raise ValueError("Maximum 6 withdrawals per month exceeded")
        result = super().withdraw(amount)
        self._withdrawals_this_month += 1
        return result

    def apply_interest(self):
        interest = self._balance * (self.interest_rate / 100)
        self._balance += interest
        self._transactions.append({
            'type': 'interest',
            'amount': interest,
            'timestamp': datetime.now(),
            'balance': self._balance
        })
        return interest

    def reset_monthly_limit(self):
        """Call this at the start of each month"""
        self._withdrawals_this_month = 0

class CheckingAccount(BankAccount):
    OVERDRAFT_FEE = 35

    def __init__(self, account_number, overdraft_limit):
        super().__init__(account_number)
        self.overdraft_limit = overdraft_limit

    def withdraw(self, amount):
        if amount <= 0:
            raise ValueError("Withdrawal amount must be positive")

        # Allow overdraft up to limit
        if amount > self._balance + self.overdraft_limit:
            raise ValueError("Exceeds overdraft limit")

        self._balance -= amount

        # Charge fee if overdrafted
        if self._balance < 0:
            self._balance -= self.OVERDRAFT_FEE
            self._transactions.append({
                'type': 'overdraft_fee',
                'amount': self.OVERDRAFT_FEE,
                'timestamp': datetime.now(),
                'balance': self._balance
            })

        self._transactions.append({
            'type': 'withdrawal',
            'amount': amount,
            'timestamp': datetime.now(),
            'balance': self._balance
        })

        return self._balance

# Test
print("=== Savings Account ===")
savings = SavingsAccount("SAV-001", 2.5)
savings.deposit(1000)
print(f"Balance: ${savings.get_balance()}")
savings.withdraw(100)
savings.withdraw(50)
print(f"Balance after withdrawals: ${savings.get_balance()}")
interest = savings.apply_interest()
print(f"Interest earned: ${interest:.2f}")
print(f"Final balance: ${savings.get_balance():.2f}")

print("\n=== Checking Account ===")
checking = CheckingAccount("CHK-001", 200)
checking.deposit(100)
print(f"Balance: ${checking.get_balance()}")
checking.withdraw(250)  # Overdraft!
print(f"Balance after overdraft: ${checking.get_balance()}")
```

**Key Insights:**
- Inheritance models "is-a" relationships
- Subclasses extend base class behavior
- `super()` calls parent method
- Override methods to specialize behavior
- Protected members (prefixed with `_`)

---

## Summary

Object-Oriented Programming provides powerful tools for organizing code:

**The Four Pillars:**
1. **Encapsulation**: Hide implementation details, expose clean interfaces
2. **Inheritance**: Reuse code through "is-a" relationships
3. **Polymorphism**: One interface, many implementations
4. **Abstraction**: Focus on essential features, hide complexity

**Key Principles:**
- **Favor composition over inheritance** - more flexible
- **Program to interfaces** - depend on abstractions
- **Single Responsibility** - each class does one thing well
- **Open/Closed** - open for extension, closed for modification

**Language Differences:**
- **Python**: Duck typing, no true privacy, multiple inheritance
- **Java**: Strong typing, interfaces, single inheritance
- **JavaScript**: Prototypal inheritance, classes are syntactic sugar
- **Ruby**: Everything is an object, open classes, mixins

**When to Use OOP:**
- Modeling real-world entities
- Complex state management
- Large codebases with many developers
- Need for polymorphism and extensibility

**When to Avoid OOP:**
- Simple scripts or utilities
- Data transformations (functional is better)
- High-performance number crunching
- Systems with minimal state

Understanding OOP makes you a more versatile programmer, even if you don't always choose OOP solutions!
