# Lesson 7: Object-Oriented Programming

## Overview

Object-Oriented Programming (OOP) is a programming paradigm based on the concept of "objects" that contain both data (attributes) and code (methods). OOP organizes software design around data and objects, rather than functions and logic.

## Learning Objectives

By the end of this lesson, you will:
- Understand the four pillars of OOP: Encapsulation, Inheritance, Polymorphism, Abstraction
- Create classes and objects in multiple languages
- Use inheritance to create class hierarchies
- Understand composition vs inheritance
- See how different paradigms approach OOP
- Compare OOP with functional programming

## The Four Pillars of OOP

### 1. Encapsulation

**Bundling data and methods that operate on that data within a single unit (class).**

```python
class BankAccount:
    def __init__(self, balance):
        self._balance = balance  # "Private" attribute

    def deposit(self, amount):
        if amount > 0:
            self._balance += amount

    def get_balance(self):
        return self._balance
```

**Benefits:**
- Data hiding (protect internal state)
- Controlled access through methods
- Easier to maintain and modify

### 2. Inheritance

**Creating new classes based on existing ones, inheriting attributes and methods.**

```python
class Animal:
    def __init__(self, name):
        self.name = name

    def speak(self):
        pass  # Abstract method

class Dog(Animal):
    def speak(self):
        return f"{self.name} says Woof!"

class Cat(Animal):
    def speak(self):
        return f"{self.name} says Meow!"
```

**Benefits:**
- Code reuse
- Hierarchical organization
- "Is-a" relationships

### 3. Polymorphism

**Objects of different types can be accessed through the same interface.**

```python
animals = [Dog("Buddy"), Cat("Whiskers"), Dog("Max")]

for animal in animals:
    print(animal.speak())  # Each responds differently
```

**Types:**
- **Runtime polymorphism**: Method overriding (above example)
- **Compile-time polymorphism**: Method overloading (multiple methods with same name)

### 4. Abstraction

**Hiding complex implementation details and showing only essential features.**

```python
from abc import ABC, abstractmethod

class Shape(ABC):
    @abstractmethod
    def area(self):
        pass

    @abstractmethod
    def perimeter(self):
        pass

class Circle(Shape):
    def __init__(self, radius):
        self.radius = radius

    def area(self):
        return 3.14159 * self.radius ** 2

    def perimeter(self):
        return 2 * 3.14159 * self.radius
```

## Classes and Objects

### Basic Class Structure

**Components:**
- **Attributes** (data/fields/properties): Variables that belong to an object
- **Methods** (functions): Functions that belong to an object
- **Constructor**: Special method to initialize objects
- **Self/This**: Reference to the current instance

### Example: Person Class

```python
class Person:
    # Constructor
    def __init__(self, name, age):
        self.name = name      # Instance attribute
        self.age = age

    # Instance method
    def introduce(self):
        return f"Hi, I'm {self.name}, {self.age} years old"

    # Instance method
    def have_birthday(self):
        self.age += 1

# Creating objects
alice = Person("Alice", 30)
bob = Person("Bob", 25)

print(alice.introduce())  # "Hi, I'm Alice, 30 years old"
alice.have_birthday()
print(alice.age)  # 31
```

## Inheritance in Detail

### Single Inheritance

One class inherits from another:

```python
class Vehicle:
    def __init__(self, brand):
        self.brand = brand

    def start(self):
        return f"{self.brand} vehicle starting..."

class Car(Vehicle):
    def __init__(self, brand, model):
        super().__init__(brand)  # Call parent constructor
        self.model = model

    def start(self):
        # Override parent method
        return f"{self.brand} {self.model} car starting... Vroom!"

car = Car("Toyota", "Camry")
print(car.start())  # "Toyota Camry car starting... Vroom!"
```

### Multiple Inheritance

Class inherits from multiple parents (not all languages support this):

```python
class Flyable:
    def fly(self):
        return "Flying through the air"

class Swimmable:
    def swim(self):
        return "Swimming in water"

class Duck(Flyable, Swimmable):
    def quack(self):
        return "Quack!"

duck = Duck()
print(duck.fly())   # "Flying through the air"
print(duck.swim())  # "Swimming in water"
print(duck.quack()) # "Quack!"
```

**Issues with multiple inheritance:**
- Diamond problem (conflicting methods)
- Complexity
- Many languages prefer composition over multiple inheritance

## Composition vs Inheritance

### Inheritance ("Is-A" relationship)
```python
class Employee(Person):  # Employee IS-A Person
    pass
```

### Composition ("Has-A" relationship)
```python
class Engine:
    def start(self):
        return "Engine starting"

class Car:
    def __init__(self):
        self.engine = Engine()  # Car HAS-AN Engine

    def start(self):
        return self.engine.start()
```

**Prefer composition when:**
- Relationship is "has-a" rather than "is-a"
- You want more flexibility
- Avoiding tight coupling
- Multiple inheritance would be needed

**"Favor composition over inheritance"** - Common design principle

## Access Modifiers

Control visibility of class members:

| Modifier | Python | Java | JavaScript | C++ |
|----------|--------|------|------------|-----|
| Public | No keyword | `public` | Default | `public:` |
| Protected | `_name` (convention) | `protected` | `#name` | `protected:` |
| Private | `__name` (name mangling) | `private` | `#name` | `private:` |

**Python example:**
```python
class Example:
    def __init__(self):
        self.public = "Everyone can access"
        self._protected = "Subclasses can access (convention)"
        self.__private = "Only this class can access (name mangling)"
```

## Static vs Instance Members

### Instance Members
Belong to each object individually:

```python
class Counter:
    def __init__(self):
        self.count = 0  # Instance variable

    def increment(self):  # Instance method
        self.count += 1
```

### Static Members (Class Members)
Belong to the class itself, shared by all instances:

```python
class Counter:
    total_counters = 0  # Class variable

    def __init__(self):
        self.count = 0
        Counter.total_counters += 1

    @classmethod
    def get_total(cls):  # Class method
        return cls.total_counters

    @staticmethod
    def is_valid_count(n):  # Static method
        return n >= 0
```

## Interfaces and Abstract Classes

### Abstract Classes
Classes that cannot be instantiated, serve as templates:

```python
from abc import ABC, abstractmethod

class Animal(ABC):
    @abstractmethod
    def speak(self):
        pass

    def sleep(self):  # Concrete method
        return "Zzz..."

# animal = Animal()  # Error! Cannot instantiate abstract class

class Dog(Animal):
    def speak(self):
        return "Woof!"

dog = Dog()
print(dog.speak())  # "Woof!"
print(dog.sleep())  # "Zzz..."
```

### Interfaces (Java-style)
Pure abstract classes (all methods abstract):

```java
// Java
interface Drawable {
    void draw();
    void erase();
}

class Circle implements Drawable {
    public void draw() { /* ... */ }
    public void erase() { /* ... */ }
}
```

## Special Methods (Magic Methods / Dunder Methods)

Python example:

```python
class Point:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __str__(self):  # String representation
        return f"Point({self.x}, {self.y})"

    def __eq__(self, other):  # Equality comparison
        return self.x == other.x and self.y == other.y

    def __add__(self, other):  # Addition operator
        return Point(self.x + other.x, self.y + other.y)

p1 = Point(1, 2)
p2 = Point(3, 4)
print(p1)       # "Point(1, 2)"
print(p1 == p2) # False
p3 = p1 + p2
print(p3)       # "Point(4, 6)"
```

## OOP in Different Paradigms

### Object-Oriented Languages (Native OOP)

**Python, JavaScript, Ruby, Java:**
- Classes are first-class citizens
- Natural OOP syntax
- Extensive built-in OOP features

### Multi-Paradigm Languages

**Rust:**
- Structs with methods (no inheritance)
- Traits (like interfaces)
- Composition over inheritance

**JavaScript:**
- Prototype-based OOP
- ES6+ class syntax
- Flexible and dynamic

### Functional Languages with OOP Features

**Racket:**
- Has object system (classes, structs)
- Functional style preferred

**Haskell:**
- Type classes (different concept)
- Algebraic data types
- No traditional OOP

### Procedural Languages

**C:**
- No built-in OOP
- Can simulate with structs and function pointers
- Manual "vtables"

### Logic Programming

**Prolog:**
- Fundamentally different paradigm
- Uses facts and rules
- Can model objects as facts

## Design Patterns

Common OOP patterns:

### 1. Singleton
Ensure a class has only one instance:

```python
class Singleton:
    _instance = None

    def __new__(cls):
        if cls._instance is None:
            cls._instance = super().__new__(cls)
        return cls._instance
```

### 2. Factory
Create objects without specifying exact class:

```python
class AnimalFactory:
    @staticmethod
    def create_animal(animal_type):
        if animal_type == "dog":
            return Dog()
        elif animal_type == "cat":
            return Cat()
```

### 3. Observer
Subscribe to and receive updates:

```python
class Subject:
    def __init__(self):
        self._observers = []

    def attach(self, observer):
        self._observers.append(observer)

    def notify(self):
        for observer in self._observers:
            observer.update()
```

## OOP vs Functional Programming

| Aspect | OOP | Functional |
|--------|-----|------------|
| **Organization** | Objects (data + methods) | Functions (pure, composable) |
| **State** | Mutable objects | Immutable data |
| **Behavior** | Methods on objects | Functions on data |
| **Code reuse** | Inheritance | Higher-order functions |
| **Primary abstraction** | Classes | Functions |

**When to use OOP:**
- Modeling real-world entities
- Managing complex state
- GUI programming
- Game development
- Team knows OOP well

**When to use FP:**
- Data transformations
- Concurrent programming
- Mathematical computations
- Avoiding side effects

**Many modern languages support both!**

## Common OOP Pitfalls

### 1. Deep Inheritance Hierarchies
```python
# BAD: Too many levels
Animal -> Mammal -> Carnivore -> Feline -> Cat -> HouseCat

# BETTER: Shallow hierarchies, use composition
Animal -> Cat (with Carnivore trait/interface)
```

### 2. God Objects
```python
# BAD: One class does everything
class Application:
    def read_config(self): ...
    def connect_database(self): ...
    def render_ui(self): ...
    def process_payments(self): ...
    def send_emails(self): ...
```

### 3. Excessive Coupling
```python
# BAD: Classes know too much about each other
class Order:
    def __init__(self, customer):
        self.customer = customer
        # Directly accessing customer internals
        if customer._vip_status and customer._credit_score > 700:
            ...

# BETTER: Use methods
class Order:
    def __init__(self, customer):
        self.customer = customer
        if customer.is_eligible_for_discount():
            ...
```

## SOLID Principles

### S - Single Responsibility Principle
Class should have one reason to change.

### O - Open/Closed Principle
Open for extension, closed for modification.

### L - Liskov Substitution Principle
Subtypes must be substitutable for their base types.

### I - Interface Segregation Principle
Many specific interfaces better than one general interface.

### D - Dependency Inversion Principle
Depend on abstractions, not concretions.

## Examples Across Languages

We'll implement a classic example: A shape hierarchy with:
1. Abstract base class `Shape`
2. Concrete classes: `Circle`, `Rectangle`, `Triangle`
3. Methods: `area()`, `perimeter()`, `describe()`
4. Demonstrate inheritance and polymorphism

We'll also show:
- Bank account (encapsulation)
- Animal hierarchy (inheritance)
- Design patterns where applicable

## Language Files

- `oop.py` - Python (classic OOP)
- `oop.js` - JavaScript (prototype + ES6 classes)
- `oop.c` - C (struct-based OOP patterns)
- `OOPDemo.java` - Java (pure OOP)
- `oop.rb` - Ruby (everything is an object)
- `oop.hs` - Haskell (type classes, ADTs)
- `oop.rkt` - Racket (structs and classes)
- `oop.pl` - Prolog (fact-based modeling)
- `oop.rs` - Rust (structs, traits, no inheritance)

## Key Takeaways

1. **OOP organizes code around objects** combining data and behavior
2. **Four pillars**: Encapsulation, Inheritance, Polymorphism, Abstraction
3. **Composition often better than inheritance**
4. **Different languages implement OOP differently**
5. **OOP is one paradigm, not the only solution**
6. **Modern best practice**: Mix OOP with functional concepts

---

Let's explore how different languages approach object-oriented programming!
