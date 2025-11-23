# Lesson 7: Object-Oriented Programming

## Overview

Object-Oriented Programming (OOP) is a programming paradigm based on the concept of "objects" that contain both data (attributes) and code (methods). OOP organizes software design around data and objects, rather than functions and logic.

This lesson explores how three fundamentally different languages approach OOP:
- **Python**: Dynamic, duck-typed OOP with multiple inheritance
- **C++**: Static, compiled OOP with strong type safety and complex inheritance
- **Haskell**: Functional programming with type classes and algebraic data types as alternatives to traditional OOP

## Learning Objectives

By the end of this lesson, you will:
- Understand the four pillars of OOP: Encapsulation, Inheritance, Polymorphism, Abstraction
- Create classes and objects in Python and C++
- Understand how Haskell achieves polymorphism without traditional OOP
- Compare type classes (Haskell) with classes (Python/C++)
- Use inheritance effectively in imperative languages
- See how immutability changes object-oriented design
- Understand composition vs inheritance
- Compare OOP with functional programming approaches

## The Four Pillars of OOP

### 1. Encapsulation

**Definition:** Bundling data and methods that operate on that data within a single unit (class), hiding internal implementation details.

**Python:**
```python
class BankAccount:
    def __init__(self, balance):
        self._balance = balance  # Convention: single underscore = "protected"
        self.__account_id = id(self)  # Name mangling: double underscore = "private"

    def deposit(self, amount):
        if amount > 0:
            self._balance += amount
            return True
        return False

    def get_balance(self):
        return self._balance

    # Property decorator for clean access
    @property
    def balance(self):
        return self._balance

# Usage
account = BankAccount(1000)
account.deposit(500)
print(account.balance)  # 1500
# account._balance = -999  # Bad practice, but Python allows it
```

**C++:**
```cpp
class BankAccount {
private:
    double balance;  // Truly private
    int accountId;

public:
    // Constructor
    BankAccount(double initialBalance)
        : balance(initialBalance), accountId(rand()) {}

    // Public methods
    bool deposit(double amount) {
        if (amount > 0) {
            balance += amount;
            return true;
        }
        return false;
    }

    // Getter (accessor)
    double getBalance() const {
        return balance;
    }

    // Const member function - guarantees no modification
    void printBalance() const {
        std::cout << "Balance: $" << balance << std::endl;
    }
};

// Usage
BankAccount account(1000);
account.deposit(500);
std::cout << account.getBalance() << std::endl;  // 1500
// account.balance = -999;  // Compile error!
```

**Haskell (Alternative Approach):**
```haskell
-- Haskell uses modules for encapsulation
module BankAccount (
    BankAccount,  -- Export type but not constructor
    createAccount,
    deposit,
    getBalance
) where

data BankAccount = BankAccount {
    accountBalance :: Double,
    accountId :: Int
} deriving (Show)

-- Smart constructor
createAccount :: Double -> BankAccount
createAccount initialBalance = BankAccount initialBalance 0

-- Pure function - returns new account
deposit :: Double -> BankAccount -> Maybe BankAccount
deposit amount account
    | amount > 0 = Just account { accountBalance = accountBalance account + amount }
    | otherwise = Nothing

getBalance :: BankAccount -> Double
getBalance = accountBalance

-- In another module, the constructor is hidden:
-- import BankAccount
-- account = createAccount 1000
-- Can't access accountBalance directly or create invalid accounts
```

**Key Differences:**
- **Python**: Encapsulation by convention; uses `_` and `__` prefixes
- **C++**: True encapsulation with compile-time enforcement
- **Haskell**: Module-based encapsulation; data is immutable by default

### 2. Inheritance

**Definition:** Creating new classes based on existing ones, inheriting attributes and methods.

**Python:**
```python
class Animal:
    def __init__(self, name, age):
        self.name = name
        self.age = age

    def speak(self):
        raise NotImplementedError("Subclass must implement speak()")

    def describe(self):
        return f"{self.name} is {self.age} years old"

class Dog(Animal):
    def __init__(self, name, age, breed):
        super().__init__(name, age)  # Call parent constructor
        self.breed = breed

    def speak(self):
        return f"{self.name} says Woof!"

    def describe(self):
        # Extend parent method
        return f"{super().describe()} and is a {self.breed}"

class Cat(Animal):
    def speak(self):
        return f"{self.name} says Meow!"

# Polymorphism in action
animals = [Dog("Buddy", 3, "Golden Retriever"), Cat("Whiskers", 2)]
for animal in animals:
    print(animal.speak())  # Each calls its own implementation
```

**C++:**
```cpp
class Animal {
protected:
    std::string name;
    int age;

public:
    Animal(const std::string& n, int a) : name(n), age(a) {}

    // Virtual function - can be overridden
    virtual std::string speak() const {
        return name + " makes a sound";
    }

    // Pure virtual function - must be overridden (makes class abstract)
    virtual std::string getType() const = 0;

    std::string describe() const {
        return name + " is " + std::to_string(age) + " years old";
    }

    // Virtual destructor - important for polymorphism!
    virtual ~Animal() {}
};

class Dog : public Animal {
private:
    std::string breed;

public:
    Dog(const std::string& n, int a, const std::string& b)
        : Animal(n, a), breed(b) {}

    // Override virtual function
    std::string speak() const override {
        return name + " says Woof!";
    }

    std::string getType() const override {
        return "Dog (" + breed + ")";
    }
};

class Cat : public Animal {
public:
    Cat(const std::string& n, int a) : Animal(n, a) {}

    std::string speak() const override {
        return name + " says Meow!";
    }

    std::string getType() const override {
        return "Cat";
    }
};

// Polymorphism with pointers/references
void makeAnimalSpeak(const Animal& animal) {
    std::cout << animal.speak() << std::endl;
}

// Usage
Dog dog("Buddy", 3, "Golden Retriever");
Cat cat("Whiskers", 2);
makeAnimalSpeak(dog);  // "Buddy says Woof!"
makeAnimalSpeak(cat);  // "Whiskers says Meow!"
```

**Haskell (Type Classes Instead of Inheritance):**
```haskell
-- Haskell doesn't have inheritance, but type classes provide polymorphism

-- Define a type class (like an interface)
class Speakable a where
    speak :: a -> String
    describe :: a -> String

-- Define data types
data Dog = Dog { dogName :: String, dogAge :: Int, breed :: String }
data Cat = Cat { catName :: String, catAge :: Int }

-- Implement the type class for each type
instance Speakable Dog where
    speak dog = dogName dog ++ " says Woof!"
    describe dog = dogName dog ++ " is " ++ show (dogAge dog) ++
                   " years old and is a " ++ breed dog

instance Speakable Cat where
    speak cat = catName cat ++ " says Meow!"
    describe cat = catName cat ++ " is " ++ show (catAge cat) ++ " years old"

-- Polymorphic function using type class constraint
makeAnimalSpeak :: Speakable a => a -> IO ()
makeAnimalSpeak animal = putStrLn (speak animal)

-- Usage
main :: IO () = do
    let dog = Dog "Buddy" 3 "Golden Retriever"
    let cat = Cat "Whiskers" 2
    makeAnimalSpeak dog
    makeAnimalSpeak cat

-- Can't mix Dog and Cat in a list directly (different types)
-- But can use existential types or other advanced techniques
```

**Key Differences:**
- **Python**: Simple inheritance with `super()`, multiple inheritance supported
- **C++**: Complex inheritance with `virtual`, `override`, `protected`; requires pointers/references for polymorphism
- **Haskell**: No inheritance; type classes provide ad-hoc polymorphism

### 3. Polymorphism

**Definition:** Objects of different types can be accessed through the same interface.

**Types of Polymorphism:**

1. **Subtype Polymorphism (OOP)**: Derived classes can be used where base class is expected
2. **Parametric Polymorphism (Generics)**: Functions work with any type
3. **Ad-hoc Polymorphism (Overloading)**: Same name, different implementations

**Python - Duck Typing:**
```python
# Python uses "duck typing" - if it walks like a duck...
class Duck:
    def quack(self):
        return "Quack!"

    def fly(self):
        return "Flying with wings"

class Person:
    def quack(self):
        return "I'm imitating a duck: Quack!"

    def fly(self):
        return "I can't fly, but I can jump!"

class Airplane:
    def fly(self):
        return "Flying with jet engines"

    # No quack method!

def make_it_fly(thing):
    # No type checking - just tries to call the method
    print(thing.fly())

duck = Duck()
person = Person()
plane = Airplane()

make_it_fly(duck)    # Works
make_it_fly(person)  # Works
make_it_fly(plane)   # Works

# More formal: Protocol typing (Python 3.8+)
from typing import Protocol

class Flyable(Protocol):
    def fly(self) -> str:
        ...

def make_it_fly_typed(thing: Flyable):
    print(thing.fly())

make_it_fly_typed(duck)   # Type checker approves
make_it_fly_typed(plane)  # Type checker approves
```

**C++ - Templates and Virtual Functions:**
```cpp
// Compile-time polymorphism with templates
template<typename T>
void makeItFly(const T& thing) {
    std::cout << thing.fly() << std::endl;
}

// Runtime polymorphism with virtual functions
class Flyable {
public:
    virtual std::string fly() const = 0;
    virtual ~Flyable() {}
};

class Duck : public Flyable {
public:
    std::string fly() const override {
        return "Flying with wings";
    }

    std::string quack() const {
        return "Quack!";
    }
};

class Airplane : public Flyable {
public:
    std::string fly() const override {
        return "Flying with jet engines";
    }
};

// Runtime polymorphism
void makeItFlyRuntime(const Flyable& thing) {
    std::cout << thing.fly() << std::endl;
}

// Usage
Duck duck;
Airplane plane;

makeItFly(duck);          // Template: compile-time
makeItFly(plane);         // Template: compile-time
makeItFlyRuntime(duck);   // Virtual: runtime
makeItFlyRuntime(plane);  // Virtual: runtime
```

**Haskell - Type Classes:**
```haskell
-- Type class defines behavior
class Flyable a where
    fly :: a -> String

data Duck = Duck String
data Airplane = Airplane String

instance Flyable Duck where
    fly (Duck name) = name ++ " is flying with wings"

instance Flyable Airplane where
    fly (Airplane model) = model ++ " is flying with jet engines"

-- Polymorphic function
makeItFly :: Flyable a => a -> IO ()
makeItFly thing = putStrLn (fly thing)

-- Usage
main = do
    makeItFly (Duck "Donald")
    makeItFly (Airplane "Boeing 747")

-- Type-safe: Can't call makeItFly on types that aren't Flyable
```

### 4. Abstraction

**Definition:** Hiding complex implementation details and showing only essential features.

**Python - Abstract Base Classes:**
```python
from abc import ABC, abstractmethod

class Shape(ABC):
    def __init__(self, color):
        self.color = color

    @abstractmethod
    def area(self):
        """Calculate area - must be implemented by subclasses"""
        pass

    @abstractmethod
    def perimeter(self):
        """Calculate perimeter - must be implemented by subclasses"""
        pass

    def describe(self):
        """Concrete method available to all subclasses"""
        return f"A {self.color} {self.__class__.__name__} with area {self.area():.2f}"

class Circle(Shape):
    def __init__(self, color, radius):
        super().__init__(color)
        self.radius = radius

    def area(self):
        return 3.14159 * self.radius ** 2

    def perimeter(self):
        return 2 * 3.14159 * self.radius

class Rectangle(Shape):
    def __init__(self, color, width, height):
        super().__init__(color)
        self.width = width
        self.height = height

    def area(self):
        return self.width * self.height

    def perimeter(self):
        return 2 * (self.width + self.height)

# Cannot instantiate Shape directly
# shape = Shape("red")  # TypeError!

# Can use polymorphically
shapes = [Circle("red", 5), Rectangle("blue", 4, 6)]
for shape in shapes:
    print(shape.describe())
```

**C++ - Abstract Classes (Pure Virtual):**
```cpp
#include <iostream>
#include <string>
#include <vector>
#include <memory>

class Shape {
protected:
    std::string color;

public:
    Shape(const std::string& c) : color(c) {}

    // Pure virtual functions - make class abstract
    virtual double area() const = 0;
    virtual double perimeter() const = 0;

    // Concrete method
    virtual std::string describe() const {
        return "A " + color + " shape with area " +
               std::to_string(area());
    }

    // Virtual destructor essential for polymorphic deletion
    virtual ~Shape() {}
};

class Circle : public Shape {
private:
    double radius;

public:
    Circle(const std::string& c, double r)
        : Shape(c), radius(r) {}

    double area() const override {
        return 3.14159 * radius * radius;
    }

    double perimeter() const override {
        return 2 * 3.14159 * radius;
    }
};

class Rectangle : public Shape {
private:
    double width, height;

public:
    Rectangle(const std::string& c, double w, double h)
        : Shape(c), width(w), height(h) {}

    double area() const override {
        return width * height;
    }

    double perimeter() const override {
        return 2 * (width + height);
    }
};

// Usage with polymorphism - requires pointers or references
void printShapeInfo(const Shape& shape) {
    std::cout << shape.describe() << std::endl;
}

int main() {
    // Use smart pointers for automatic memory management
    std::vector<std::unique_ptr<Shape>> shapes;
    shapes.push_back(std::make_unique<Circle>("red", 5));
    shapes.push_back(std::make_unique<Rectangle>("blue", 4, 6));

    for (const auto& shape : shapes) {
        printShapeInfo(*shape);
    }

    return 0;
}
```

**Haskell - Data Types with Type Classes:**
```haskell
-- Define the abstraction with a type class
class Measurable a where
    area :: a -> Double
    perimeter :: a -> Double
    describe :: a -> String

-- Define concrete types
data Circle = Circle {
    circleColor :: String,
    radius :: Double
} deriving (Show)

data Rectangle = Rectangle {
    rectColor :: String,
    width :: Double,
    height :: Double
} deriving (Show)

-- Implement the abstraction for each type
instance Measurable Circle where
    area (Circle _ r) = pi * r * r
    perimeter (Circle _ r) = 2 * pi * r
    describe c = circleColor c ++ " circle with area " ++
                 show (area c)

instance Measurable Rectangle where
    area (Rectangle _ w h) = w * h
    perimeter (Rectangle _ w h) = 2 * (w + h)
    describe r = rectColor r ++ " rectangle with area " ++
                 show (area r)

-- Polymorphic function using constraint
printShapeInfo :: Measurable a => a -> IO ()
printShapeInfo shape = putStrLn (describe shape)

-- Usage
main :: IO () = do
    let circle = Circle "red" 5
    let rect = Rectangle "blue" 4 6
    printShapeInfo circle
    printShapeInfo rect

-- Note: Can't put Circle and Rectangle in same list without wrapping
-- They're different types!
```

## Classes and Objects

### Python: Dynamic and Flexible

**Basic Class Structure:**
```python
class Person:
    # Class variable (shared by all instances)
    species = "Homo sapiens"
    population = 0

    def __init__(self, name, age):
        # Instance variables (unique to each instance)
        self.name = name
        self.age = age
        Person.population += 1

    # Instance method
    def introduce(self):
        return f"Hi, I'm {self.name}, {self.age} years old"

    def have_birthday(self):
        self.age += 1
        return self.age

    # Class method (operates on class, not instance)
    @classmethod
    def get_population(cls):
        return cls.population

    # Static method (no access to class or instance)
    @staticmethod
    def is_adult(age):
        return age >= 18

    # Property (computed attribute)
    @property
    def birth_year(self):
        from datetime import datetime
        return datetime.now().year - self.age

    # Special methods (magic methods / dunder methods)
    def __str__(self):
        return f"Person({self.name}, {self.age})"

    def __repr__(self):
        return f"Person('{self.name}', {self.age})"

    def __eq__(self, other):
        return self.name == other.name and self.age == other.age

    def __lt__(self, other):
        return self.age < other.age

# Usage
alice = Person("Alice", 30)
bob = Person("Bob", 25)

print(alice.introduce())        # Instance method
print(Person.get_population())  # Class method: 2
print(Person.is_adult(20))      # Static method: True
print(alice.birth_year)         # Property
print(alice)                    # __str__: Person(Alice, 30)
print(alice < bob)              # __lt__: False
```

### C++: Static and Type-Safe

**Basic Class Structure:**
```cpp
#include <string>
#include <iostream>

class Person {
private:
    std::string name;
    int age;

    // Static member variable (shared by all instances)
    static int population;

public:
    // Constructor
    Person(const std::string& n, int a) : name(n), age(a) {
        ++population;
    }

    // Copy constructor
    Person(const Person& other)
        : name(other.name), age(other.age) {
        ++population;
    }

    // Destructor
    ~Person() {
        --population;
    }

    // Getters (const member functions)
    std::string getName() const { return name; }
    int getAge() const { return age; }

    // Setters
    void setAge(int a) { age = a; }

    // Member function
    std::string introduce() const {
        return "Hi, I'm " + name + ", " + std::to_string(age) + " years old";
    }

    void haveBirthday() {
        ++age;
    }

    // Static member function
    static int getPopulation() {
        return population;
    }

    // Operator overloading
    bool operator<(const Person& other) const {
        return age < other.age;
    }

    bool operator==(const Person& other) const {
        return name == other.name && age == other.age;
    }

    // Friend function (can access private members)
    friend std::ostream& operator<<(std::ostream& os, const Person& p);
};

// Initialize static member (outside class)
int Person::population = 0;

// Friend function definition
std::ostream& operator<<(std::ostream& os, const Person& p) {
    os << "Person(" << p.name << ", " << p.age << ")";
    return os;
}

// Usage
int main() {
    Person alice("Alice", 30);
    Person bob("Bob", 25);

    std::cout << alice.introduce() << std::endl;
    std::cout << Person::getPopulation() << std::endl;  // 2
    std::cout << alice << std::endl;
    std::cout << (alice < bob) << std::endl;  // 0 (false)

    return 0;
}
```

### Haskell: Algebraic Data Types

**Data Types and Type Classes:**
```haskell
-- Define a data type
data Person = Person {
    personName :: String,
    personAge :: Int
} deriving (Show, Eq)  -- Automatically derive type class instances

-- Smart constructor with validation
createPerson :: String -> Int -> Maybe Person
createPerson name age
    | age < 0 || age > 150 = Nothing
    | null name = Nothing
    | otherwise = Just (Person name age)

-- Functions on Person (not methods)
introduce :: Person -> String
introduce (Person name age) =
    "Hi, I'm " ++ name ++ ", " ++ show age ++ " years old"

haveBirthday :: Person -> Person
haveBirthday person = person { personAge = personAge person + 1 }

isAdult :: Person -> Bool
isAdult (Person _ age) = age >= 18

-- Pattern matching
greet :: Person -> String
greet (Person name age)
    | age < 18 = "Hello, young " ++ name
    | age < 65 = "Hello, " ++ name
    | otherwise = "Hello, wise " ++ name

-- Type classes for comparison (already derived Eq)
-- Can manually implement Ord for custom ordering
instance Ord Person where
    compare (Person _ age1) (Person _ age2) = compare age1 age2

-- Usage
main :: IO () = do
    let alice = Person "Alice" 30
    let bob = Person "Bob" 25

    putStrLn $ introduce alice
    putStrLn $ show alice
    putStrLn $ show (alice < bob)  -- False

    -- With smart constructor
    case createPerson "Charlie" 35 of
        Just person -> putStrLn $ introduce person
        Nothing -> putStrLn "Invalid person data"
```

## Inheritance in Depth

### Python: Multiple Inheritance and MRO

**Single Inheritance:**
```python
class Vehicle:
    def __init__(self, brand, year):
        self.brand = brand
        self.year = year

    def start(self):
        return f"{self.brand} vehicle starting..."

    def stop(self):
        return "Vehicle stopping..."

class Car(Vehicle):
    def __init__(self, brand, year, model, doors):
        super().__init__(brand, year)
        self.model = model
        self.doors = doors

    def start(self):
        # Call parent method and extend
        parent_msg = super().start()
        return f"{parent_msg} Vroom! {self.model} engine engaged."

    def honk(self):
        return "Beep beep!"

car = Car("Toyota", 2020, "Camry", 4)
print(car.start())
print(car.honk())
```

**Multiple Inheritance:**
```python
class Flyable:
    def fly(self):
        return "Flying through the air"

    def land(self):
        return "Landing..."

class Swimmable:
    def swim(self):
        return "Swimming in water"

    def dive(self):
        return "Diving deep"

class Duck(Flyable, Swimmable):
    def __init__(self, name):
        self.name = name

    def quack(self):
        return f"{self.name} says Quack!"

duck = Duck("Donald")
print(duck.fly())    # From Flyable
print(duck.swim())   # From Swimmable
print(duck.quack())  # Own method

# Method Resolution Order (MRO)
print(Duck.__mro__)
# (<class '__main__.Duck'>, <class '__main__.Flyable'>,
#  <class '__main__.Swimmable'>, <class 'object'>)
```

**Diamond Problem:**
```python
class A:
    def method(self):
        return "A"

class B(A):
    def method(self):
        return "B"

class C(A):
    def method(self):
        return "C"

class D(B, C):
    pass

d = D()
print(d.method())  # "B" - follows MRO: D -> B -> C -> A
print(D.__mro__)
```

### C++: Complex Inheritance

**Single Inheritance:**
```cpp
class Vehicle {
protected:
    std::string brand;
    int year;

public:
    Vehicle(const std::string& b, int y) : brand(b), year(y) {}

    virtual std::string start() const {
        return brand + " vehicle starting...";
    }

    std::string stop() const {
        return "Vehicle stopping...";
    }

    virtual ~Vehicle() {}
};

class Car : public Vehicle {
private:
    std::string model;
    int doors;

public:
    Car(const std::string& b, int y, const std::string& m, int d)
        : Vehicle(b, y), model(m), doors(d) {}

    std::string start() const override {
        return Vehicle::start() + " Vroom! " + model + " engine engaged.";
    }

    std::string honk() const {
        return "Beep beep!";
    }
};
```

**Multiple Inheritance:**
```cpp
class Flyable {
public:
    virtual std::string fly() const {
        return "Flying through the air";
    }

    virtual std::string land() const {
        return "Landing...";
    }

    virtual ~Flyable() {}
};

class Swimmable {
public:
    virtual std::string swim() const {
        return "Swimming in water";
    }

    virtual std::string dive() const {
        return "Diving deep";
    }

    virtual ~Swimmable() {}
};

class Duck : public Flyable, public Swimmable {
private:
    std::string name;

public:
    Duck(const std::string& n) : name(n) {}

    std::string quack() const {
        return name + " says Quack!";
    }
};

// Usage
Duck duck("Donald");
std::cout << duck.fly() << std::endl;
std::cout << duck.swim() << std::endl;
std::cout << duck.quack() << std::endl;
```

**Diamond Problem in C++:**
```cpp
// Without virtual inheritance - duplicate base
class A {
public:
    int value;
    A() : value(0) {}
};

class B : public A {};
class C : public A {};
class D : public B, public C {};  // D has TWO copies of A!

// D d;
// d.value = 5;  // Error: ambiguous!
// d.B::value = 5;  // Must specify which A

// Solution: Virtual inheritance
class A {
public:
    int value;
    A() : value(0) {}
};

class B : virtual public A {};
class C : virtual public A {};
class D : public B, public C {};  // D has ONE copy of A

D d;
d.value = 5;  // OK!
```

### Haskell: No Inheritance, Use Composition

Haskell doesn't have inheritance. Instead:

**Type Class Extension:**
```haskell
-- Base type class
class Describable a where
    describe :: a -> String

-- Extended type class
class Describable a => Detailed a where
    detailedDescription :: a -> String
    -- Can use describe here since Detailed implies Describable

data Animal = Animal String Int

instance Describable Animal where
    describe (Animal name age) = name ++ " (" ++ show age ++ ")"

instance Detailed Animal where
    detailedDescription animal =
        "Detailed: " ++ describe animal ++ ", species: unknown"
```

**Algebraic Data Types for Variants:**
```haskell
-- Instead of inheritance hierarchy, use sum types
data Vehicle
    = Car String Int String Int      -- brand year model doors
    | Motorcycle String Int String   -- brand year model
    | Bicycle String                 -- brand
    deriving (Show, Eq)

-- Pattern match instead of virtual methods
startVehicle :: Vehicle -> String
startVehicle (Car brand _ model _) =
    brand ++ " " ++ model ++ " car starting... Vroom!"
startVehicle (Motorcycle brand _ model) =
    brand ++ " " ++ model ++ " motorcycle starting... Vrrr!"
startVehicle (Bicycle brand) =
    brand ++ " bicycle... pedal harder!"

-- All possible cases handled at compile time!
```

**Composition with Records:**
```haskell
-- Define reusable components
data Engine = Engine {
    horsepower :: Int,
    fuelType :: String
} deriving (Show)

data Wheel = Wheel {
    diameter :: Int,
    material :: String
} deriving (Show)

-- Compose into larger structures
data Car = Car {
    carBrand :: String,
    carEngine :: Engine,
    carWheels :: [Wheel],
    doors :: Int
} deriving (Show)

-- Functions operate on the composition
totalHorsepower :: Car -> Int
totalHorsepower = horsepower . carEngine

-- This is more flexible than inheritance!
```

## Composition vs Inheritance

### When to Use Each

**"Favor composition over inheritance"** - Gang of Four

**Use Inheritance when:**
- True "is-a" relationship exists
- Shared interface is needed
- You need polymorphism
- Relationship is stable and won't change

**Use Composition when:**
- "Has-a" relationship
- You need flexibility
- Want to avoid tight coupling
- Need to combine multiple behaviors

**Example: Vehicles**

**Bad - Inheritance:**
```python
class Vehicle:
    pass

class FourWheeled(Vehicle):
    pass

class TwoWheeled(Vehicle):
    pass

class Motorized(FourWheeled):  # What about motorized two-wheeled?
    pass

# This becomes complex quickly!
```

**Good - Composition:**
```python
class Engine:
    def __init__(self, horsepower):
        self.horsepower = horsepower

    def start(self):
        return f"Engine starting ({self.horsepower} HP)"

class Wheel:
    def __init__(self, diameter):
        self.diameter = diameter

class Vehicle:
    def __init__(self, engine=None, num_wheels=4):
        self.engine = engine
        self.wheels = [Wheel(16) for _ in range(num_wheels)]

    def start(self):
        if self.engine:
            return self.engine.start()
        return "No engine - use pedal power!"

# Easy to create any combination
car = Vehicle(Engine(200), 4)
motorcycle = Vehicle(Engine(50), 2)
bicycle = Vehicle(None, 2)
```

**C++ Composition:**
```cpp
class Engine {
private:
    int horsepower;

public:
    Engine(int hp) : horsepower(hp) {}

    std::string start() const {
        return "Engine starting (" + std::to_string(horsepower) + " HP)";
    }
};

class Vehicle {
private:
    std::unique_ptr<Engine> engine;  // Owns the engine
    int numWheels;

public:
    Vehicle(int hp, int wheels)
        : engine(std::make_unique<Engine>(hp)), numWheels(wheels) {}

    // Move constructor for unique_ptr
    Vehicle(Vehicle&&) = default;

    std::string start() const {
        if (engine) {
            return engine->start();
        }
        return "No engine!";
    }
};
```

**Haskell Composition (Natural):**
```haskell
data Engine = Engine { horsepower :: Int }
data Wheel = Wheel { diameter :: Int }

data Vehicle = Vehicle {
    engine :: Maybe Engine,
    wheels :: [Wheel]
}

startVehicle :: Vehicle -> String
startVehicle (Vehicle (Just (Engine hp)) _) =
    "Engine starting (" ++ show hp ++ " HP)"
startVehicle (Vehicle Nothing _) =
    "No engine - use pedal power!"

-- Create vehicles
car = Vehicle (Just (Engine 200)) [Wheel 16, Wheel 16, Wheel 16, Wheel 16]
bicycle = Vehicle Nothing [Wheel 20, Wheel 20]
```

## Paradigm Comparison: OOP vs Functional

### How Haskell Achieves OOP-like Features Without OOP

| OOP Concept | Python/C++ | Haskell Alternative |
|-------------|------------|---------------------|
| **Classes** | `class Person` | `data Person` (ADT) |
| **Objects** | Instances with mutable state | Immutable values |
| **Methods** | Functions tied to objects | Pure functions on data |
| **Encapsulation** | Private members | Module exports |
| **Inheritance** | Class hierarchy | Type class extension |
| **Polymorphism** | Virtual functions | Type classes, parametric types |
| **Interfaces** | Abstract classes | Type classes |

**Example: A Stack Data Structure**

**Python (OOP):**
```python
class Stack:
    def __init__(self):
        self._items = []

    def push(self, item):
        self._items.append(item)

    def pop(self):
        if self.is_empty():
            raise IndexError("Pop from empty stack")
        return self._items.pop()

    def peek(self):
        if self.is_empty():
            raise IndexError("Peek at empty stack")
        return self._items[-1]

    def is_empty(self):
        return len(self._items) == 0

    def size(self):
        return len(self._items)

# Usage - mutable state
stack = Stack()
stack.push(1)
stack.push(2)
print(stack.pop())  # 2 - stack is modified
```

**C++ (OOP):**
```cpp
template<typename T>
class Stack {
private:
    std::vector<T> items;

public:
    void push(const T& item) {
        items.push_back(item);
    }

    T pop() {
        if (isEmpty()) {
            throw std::runtime_error("Pop from empty stack");
        }
        T item = items.back();
        items.pop_back();
        return item;
    }

    const T& peek() const {
        if (isEmpty()) {
            throw std::runtime_error("Peek at empty stack");
        }
        return items.back();
    }

    bool isEmpty() const {
        return items.empty();
    }

    size_t size() const {
        return items.size();
    }
};

// Usage - mutable state
Stack<int> stack;
stack.push(1);
stack.push(2);
std::cout << stack.pop() << std::endl;  // 2 - stack is modified
```

**Haskell (Functional):**
```haskell
-- Stack as immutable data structure
data Stack a = Stack [a] deriving (Show, Eq)

-- Create empty stack
empty :: Stack a
empty = Stack []

-- Push returns NEW stack
push :: a -> Stack a -> Stack a
push item (Stack items) = Stack (item : items)

-- Pop returns (item, new stack) - original unchanged
pop :: Stack a -> Maybe (a, Stack a)
pop (Stack []) = Nothing
pop (Stack (x:xs)) = Just (x, Stack xs)

-- Peek doesn't modify
peek :: Stack a -> Maybe a
peek (Stack []) = Nothing
peek (Stack (x:_)) = Just x

isEmpty :: Stack a -> Bool
isEmpty (Stack []) = True
isEmpty _ = False

size :: Stack a -> Int
size (Stack items) = length items

-- Usage - immutable
main :: IO () = do
    let stack1 = empty
    let stack2 = push 1 stack1
    let stack3 = push 2 stack2

    case pop stack3 of
        Just (item, stack4) -> do
            print item  -- 2
            print stack3  -- Original unchanged!
            print stack4  -- New stack without 2
        Nothing -> putStrLn "Empty stack"
```

**Key Insight:** Haskell achieves the benefits of encapsulation and abstraction without mutable objects. Each operation returns a new value rather than modifying existing state.

## Advanced OOP Concepts

### Static vs Instance Members

**Python:**
```python
class Counter:
    # Class variable - shared by all instances
    total_count = 0

    def __init__(self, name):
        self.name = name  # Instance variable
        self.count = 0    # Instance variable
        Counter.total_count += 1

    # Instance method - operates on instance
    def increment(self):
        self.count += 1

    # Class method - operates on class
    @classmethod
    def get_total(cls):
        return cls.total_count

    @classmethod
    def reset_total(cls):
        cls.total_count = 0

    # Static method - doesn't access class or instance
    @staticmethod
    def is_valid_count(n):
        return n >= 0

c1 = Counter("first")
c2 = Counter("second")
c1.increment()
c1.increment()
c2.increment()

print(c1.count)               # 2 (instance)
print(c2.count)               # 1 (instance)
print(Counter.get_total())    # 2 (class)
print(Counter.is_valid_count(-1))  # False (static)
```

**C++:**
```cpp
class Counter {
private:
    std::string name;
    int count;

    // Static member - shared by all instances
    static int totalCount;

public:
    Counter(const std::string& n) : name(n), count(0) {
        ++totalCount;
    }

    // Instance method
    void increment() {
        ++count;
    }

    int getCount() const {
        return count;
    }

    // Static method
    static int getTotalCount() {
        return totalCount;
    }

    static void resetTotal() {
        totalCount = 0;
    }
};

// Initialize static member
int Counter::totalCount = 0;

// Usage
Counter c1("first");
Counter c2("second");
c1.increment();
c1.increment();
c2.increment();

std::cout << c1.getCount() << std::endl;        // 2
std::cout << c2.getCount() << std::endl;        // 1
std::cout << Counter::getTotalCount() << std::endl;  // 2
```

**Haskell (No Real Equivalent):**
```haskell
-- Haskell doesn't have static members
-- But can use State monad or IORef for shared state

import Data.IORef

data Counter = Counter {
    counterName :: String,
    counterValue :: IORef Int
}

createCounter :: String -> IO Counter
createCounter name = do
    ref <- newIORef 0
    return $ Counter name ref

increment :: Counter -> IO ()
increment counter = modifyIORef (counterValue counter) (+1)

getCount :: Counter -> IO Int
getCount counter = readIORef (counterValue counter)

-- For "static" shared state, use a top-level IORef
-- (Generally avoided in Haskell - prefer pure approaches)
```

### Operator Overloading

**Python:**
```python
class Vector2D:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __add__(self, other):
        return Vector2D(self.x + other.x, self.y + other.y)

    def __sub__(self, other):
        return Vector2D(self.x - other.x, self.y - other.y)

    def __mul__(self, scalar):
        return Vector2D(self.x * scalar, self.y * scalar)

    def __eq__(self, other):
        return self.x == other.x and self.y == other.y

    def __str__(self):
        return f"Vector2D({self.x}, {self.y})"

    def __repr__(self):
        return f"Vector2D({self.x}, {self.y})"

v1 = Vector2D(3, 4)
v2 = Vector2D(1, 2)
v3 = v1 + v2      # Vector2D(4, 6)
v4 = v1 * 2       # Vector2D(6, 8)
print(v1 == v2)   # False
print(v3)         # Vector2D(4, 6)
```

**C++:**
```cpp
class Vector2D {
private:
    double x, y;

public:
    Vector2D(double x, double y) : x(x), y(y) {}

    // Operator overloading
    Vector2D operator+(const Vector2D& other) const {
        return Vector2D(x + other.x, y + other.y);
    }

    Vector2D operator-(const Vector2D& other) const {
        return Vector2D(x - other.x, y - other.y);
    }

    Vector2D operator*(double scalar) const {
        return Vector2D(x * scalar, y * scalar);
    }

    bool operator==(const Vector2D& other) const {
        return x == other.x && y == other.y;
    }

    // Friend function for scalar * vector
    friend Vector2D operator*(double scalar, const Vector2D& v) {
        return v * scalar;
    }

    // Friend function for stream output
    friend std::ostream& operator<<(std::ostream& os, const Vector2D& v) {
        os << "Vector2D(" << v.x << ", " << v.y << ")";
        return os;
    }
};

Vector2D v1(3, 4);
Vector2D v2(1, 2);
Vector2D v3 = v1 + v2;
Vector2D v4 = v1 * 2;
Vector2D v5 = 2 * v1;  // Uses friend function
std::cout << v3 << std::endl;
```

**Haskell (Type Classes):**
```haskell
data Vector2D = Vector2D Double Double deriving (Show, Eq)

-- Use Num type class for arithmetic operators
instance Num Vector2D where
    (Vector2D x1 y1) + (Vector2D x2 y2) = Vector2D (x1 + x2) (y1 + y2)
    (Vector2D x1 y1) - (Vector2D x2 y2) = Vector2D (x1 - x2) (y1 - y2)
    (Vector2D x1 y1) * (Vector2D x2 y2) = Vector2D (x1 * x2) (y1 * y2)  -- Element-wise
    abs (Vector2D x y) = Vector2D (abs x) (abs y)
    signum (Vector2D x y) = Vector2D (signum x) (signum y)
    fromInteger n = Vector2D (fromInteger n) (fromInteger n)

-- Scalar multiplication (separate function)
scale :: Double -> Vector2D -> Vector2D
scale s (Vector2D x y) = Vector2D (s * x) (s * y)

-- Usage
v1 = Vector2D 3 4
v2 = Vector2D 1 2
v3 = v1 + v2
v4 = scale 2 v1
```

### Properties and Descriptors

**Python:**
```python
class Temperature:
    def __init__(self, celsius):
        self._celsius = celsius

    @property
    def celsius(self):
        """Get temperature in Celsius"""
        return self._celsius

    @celsius.setter
    def celsius(self, value):
        """Set temperature in Celsius"""
        if value < -273.15:
            raise ValueError("Temperature below absolute zero!")
        self._celsius = value

    @property
    def fahrenheit(self):
        """Get temperature in Fahrenheit"""
        return self._celsius * 9/5 + 32

    @fahrenheit.setter
    def fahrenheit(self, value):
        """Set temperature in Fahrenheit"""
        self.celsius = (value - 32) * 5/9  # Uses celsius setter for validation

    @property
    def kelvin(self):
        """Get temperature in Kelvin"""
        return self._celsius + 273.15

    @kelvin.setter
    def kelvin(self, value):
        """Set temperature in Kelvin"""
        self.celsius = value - 273.15

# Usage
temp = Temperature(0)
print(temp.fahrenheit)  # 32.0
temp.fahrenheit = 212
print(temp.celsius)     # 100.0
# temp.kelvin = 0  # Raises ValueError!
```

**C++ (Getters/Setters):**
```cpp
class Temperature {
private:
    double celsius;

public:
    Temperature(double c) {
        setCelsius(c);
    }

    // Getters
    double getCelsius() const {
        return celsius;
    }

    double getFahrenheit() const {
        return celsius * 9.0/5.0 + 32;
    }

    double getKelvin() const {
        return celsius + 273.15;
    }

    // Setters
    void setCelsius(double c) {
        if (c < -273.15) {
            throw std::invalid_argument("Temperature below absolute zero!");
        }
        celsius = c;
    }

    void setFahrenheit(double f) {
        setCelsius((f - 32) * 5.0/9.0);
    }

    void setKelvin(double k) {
        setCelsius(k - 273.15);
    }
};

// Usage
Temperature temp(0);
std::cout << temp.getFahrenheit() << std::endl;  // 32
temp.setFahrenheit(212);
std::cout << temp.getCelsius() << std::endl;     // 100
```

**Haskell (Smart Constructors and Lenses):**
```haskell
-- Simple approach with smart constructors
data Temperature = Temperature { getCelsius :: Double } deriving (Show)

-- Smart constructor validates
mkTemperature :: Double -> Maybe Temperature
mkTemperature c
    | c < -273.15 = Nothing
    | otherwise = Just (Temperature c)

toFahrenheit :: Temperature -> Double
toFahrenheit (Temperature c) = c * 9/5 + 32

fromFahrenheit :: Double -> Maybe Temperature
fromFahrenheit f = mkTemperature ((f - 32) * 5/9)

toKelvin :: Temperature -> Double
toKelvin (Temperature c) = c + 273.15

fromKelvin :: Double -> Maybe Temperature
fromKelvin k = mkTemperature (k - 273.15)

-- Usage
main :: IO () = do
    case mkTemperature 0 of
        Just temp -> do
            print $ toFahrenheit temp  -- 32.0
            case fromFahrenheit 212 of
                Just temp2 -> print $ getCelsius temp2  -- 100.0
                Nothing -> putStrLn "Invalid temperature"
        Nothing -> putStrLn "Invalid temperature"
```

## Design Patterns in OOP

### Singleton Pattern

**Python:**
```python
class Singleton:
    _instance = None

    def __new__(cls):
        if cls._instance is None:
            cls._instance = super().__new__(cls)
            cls._instance._initialized = False
        return cls._instance

    def __init__(self):
        if self._initialized:
            return
        self._initialized = True
        self.data = []

    def add_data(self, item):
        self.data.append(item)

s1 = Singleton()
s2 = Singleton()
s1.add_data("Hello")
print(s2.data)  # ["Hello"]
print(s1 is s2)  # True
```

**C++:**
```cpp
class Singleton {
private:
    static Singleton* instance;
    std::vector<std::string> data;

    // Private constructor
    Singleton() {}

    // Delete copy constructor and assignment
    Singleton(const Singleton&) = delete;
    Singleton& operator=(const Singleton&) = delete;

public:
    static Singleton* getInstance() {
        if (instance == nullptr) {
            instance = new Singleton();
        }
        return instance;
    }

    void addData(const std::string& item) {
        data.push_back(item);
    }

    const std::vector<std::string>& getData() const {
        return data;
    }
};

Singleton* Singleton::instance = nullptr;

// Modern C++11 thread-safe singleton
class ModernSingleton {
private:
    ModernSingleton() {}

public:
    static ModernSingleton& getInstance() {
        static ModernSingleton instance;  // Thread-safe in C++11
        return instance;
    }

    ModernSingleton(const ModernSingleton&) = delete;
    ModernSingleton& operator=(const ModernSingleton&) = delete;
};
```

**Haskell (Not Applicable):**
```haskell
-- Haskell doesn't need singletons - global state is managed differently
-- Use top-level definitions for "singleton-like" behavior

-- This is effectively a "singleton" - only one exists
globalConfig :: Config
globalConfig = Config "settings.ini"

-- Or use IORef for mutable singleton-like state
import Data.IORef

type GlobalState = IORef [String]

-- Create once at program start
initGlobalState :: IO GlobalState
initGlobalState = newIORef []

-- Note: Haskell philosophy avoids this pattern
-- Prefer passing state explicitly or using Reader monad
```

### Factory Pattern

**Python:**
```python
from abc import ABC, abstractmethod

class Animal(ABC):
    @abstractmethod
    def speak(self):
        pass

class Dog(Animal):
    def speak(self):
        return "Woof!"

class Cat(Animal):
    def speak(self):
        return "Meow!"

class Bird(Animal):
    def speak(self):
        return "Chirp!"

class AnimalFactory:
    @staticmethod
    def create_animal(animal_type):
        if animal_type == "dog":
            return Dog()
        elif animal_type == "cat":
            return Cat()
        elif animal_type == "bird":
            return Bird()
        else:
            raise ValueError(f"Unknown animal type: {animal_type}")

# Usage
factory = AnimalFactory()
animals = [
    factory.create_animal("dog"),
    factory.create_animal("cat"),
    factory.create_animal("bird")
]

for animal in animals:
    print(animal.speak())
```

**C++:**
```cpp
class Animal {
public:
    virtual std::string speak() const = 0;
    virtual ~Animal() {}
};

class Dog : public Animal {
public:
    std::string speak() const override {
        return "Woof!";
    }
};

class Cat : public Animal {
public:
    std::string speak() const override {
        return "Meow!";
    }
};

class AnimalFactory {
public:
    static std::unique_ptr<Animal> createAnimal(const std::string& type) {
        if (type == "dog") {
            return std::make_unique<Dog>();
        } else if (type == "cat") {
            return std::make_unique<Cat>();
        } else {
            throw std::invalid_argument("Unknown animal type");
        }
    }
};

// Usage
auto dog = AnimalFactory::createAnimal("dog");
std::cout << dog->speak() << std::endl;
```

**Haskell (Sum Types - Better!):**
```haskell
-- No factory needed - use algebraic data types
data Animal = Dog | Cat | Bird deriving (Show, Eq)

speak :: Animal -> String
speak Dog = "Woof!"
speak Cat = "Meow!"
speak Bird = "Chirp!"

-- "Factory" is just parsing
createAnimal :: String -> Maybe Animal
createAnimal "dog" = Just Dog
createAnimal "cat" = Just Cat
createAnimal "bird" = Just Bird
createAnimal _ = Nothing

-- Usage
main :: IO () = do
    case createAnimal "dog" of
        Just animal -> putStrLn $ speak animal
        Nothing -> putStrLn "Unknown animal"

-- Type-safe and exhaustive!
```

### Observer Pattern

**Python:**
```python
class Subject:
    def __init__(self):
        self._observers = []
        self._state = None

    def attach(self, observer):
        if observer not in self._observers:
            self._observers.append(observer)

    def detach(self, observer):
        self._observers.remove(observer)

    def notify(self):
        for observer in self._observers:
            observer.update(self)

    def set_state(self, state):
        self._state = state
        self.notify()

    def get_state(self):
        return self._state

class Observer:
    def update(self, subject):
        print(f"Observer notified. New state: {subject.get_state()}")

# Usage
subject = Subject()
observer1 = Observer()
observer2 = Observer()

subject.attach(observer1)
subject.attach(observer2)
subject.set_state("State 1")  # Both observers notified
```

**C++:**
```cpp
#include <vector>
#include <algorithm>

class Observer {
public:
    virtual void update(int state) = 0;
    virtual ~Observer() {}
};

class Subject {
private:
    std::vector<Observer*> observers;
    int state;

public:
    void attach(Observer* observer) {
        observers.push_back(observer);
    }

    void detach(Observer* observer) {
        observers.erase(
            std::remove(observers.begin(), observers.end(), observer),
            observers.end()
        );
    }

    void notify() {
        for (auto observer : observers) {
            observer->update(state);
        }
    }

    void setState(int s) {
        state = s;
        notify();
    }

    int getState() const {
        return state;
    }
};

class ConcreteObserver : public Observer {
private:
    std::string name;

public:
    ConcreteObserver(const std::string& n) : name(n) {}

    void update(int state) override {
        std::cout << name << " notified. New state: " << state << std::endl;
    }
};
```

**Haskell (Functional Reactive Programming):**
```haskell
-- In Haskell, use FRP libraries or callbacks
-- Simple callback approach:

type Observer a = a -> IO ()

data Subject a = Subject {
    subjectState :: IORef a,
    observers :: IORef [Observer a]
}

createSubject :: a -> IO (Subject a)
createSubject initial = do
    state <- newIORef initial
    obs <- newIORef []
    return $ Subject state obs

attach :: Subject a -> Observer a -> IO ()
attach subject observer = do
    modifyIORef (observers subject) (observer :)

setState :: Subject a -> a -> IO ()
setState subject newState = do
    writeIORef (subjectState subject) newState
    obs <- readIORef (observers subject)
    state <- readIORef (subjectState subject)
    mapM_ (\f -> f state) obs

-- Better: Use libraries like reactive-banana or reflex
```

## SOLID Principles

### S - Single Responsibility Principle

A class should have one, and only one, reason to change.

**Bad:**
```python
class Employee:
    def __init__(self, name, salary):
        self.name = name
        self.salary = salary

    def calculate_pay(self):
        # Payment logic
        pass

    def save_to_database(self):
        # Database logic
        pass

    def generate_report(self):
        # Reporting logic
        pass

    # Too many responsibilities!
```

**Good:**
```python
class Employee:
    def __init__(self, name, salary):
        self.name = name
        self.salary = salary

class PayrollCalculator:
    def calculate_pay(self, employee):
        # Payment logic
        pass

class EmployeeRepository:
    def save(self, employee):
        # Database logic
        pass

class ReportGenerator:
    def generate_report(self, employee):
        # Reporting logic
        pass
```

### O - Open/Closed Principle

Open for extension, closed for modification.

**Bad:**
```python
class Rectangle:
    def __init__(self, width, height):
        self.width = width
        self.height = height

class AreaCalculator:
    def calculate_area(self, shapes):
        total = 0
        for shape in shapes:
            if isinstance(shape, Rectangle):
                total += shape.width * shape.height
            elif isinstance(shape, Circle):  # Adding new shape requires modification!
                total += 3.14 * shape.radius ** 2
        return total
```

**Good:**
```python
from abc import ABC, abstractmethod

class Shape(ABC):
    @abstractmethod
    def area(self):
        pass

class Rectangle(Shape):
    def __init__(self, width, height):
        self.width = width
        self.height = height

    def area(self):
        return self.width * self.height

class Circle(Shape):
    def __init__(self, radius):
        self.radius = radius

    def area(self):
        return 3.14 * self.radius ** 2

class AreaCalculator:
    def calculate_area(self, shapes):
        return sum(shape.area() for shape in shapes)

    # Can add new shapes without modifying this class!
```

### L - Liskov Substitution Principle

Subtypes must be substitutable for their base types.

**Bad:**
```python
class Bird:
    def fly(self):
        return "Flying"

class Penguin(Bird):
    def fly(self):
        raise Exception("Penguins can't fly!")  # Violates LSP!
```

**Good:**
```python
class Bird:
    def move(self):
        return "Moving"

class FlyingBird(Bird):
    def fly(self):
        return "Flying"

class Penguin(Bird):
    def swim(self):
        return "Swimming"

class Sparrow(FlyingBird):
    pass
```

### I - Interface Segregation Principle

Many specific interfaces are better than one general-purpose interface.

**Bad:**
```python
class Worker:
    def work(self):
        pass

    def eat(self):
        pass

    def sleep(self):
        pass

class Robot(Worker):
    def work(self):
        return "Working"

    def eat(self):
        raise Exception("Robots don't eat!")  # Forced to implement!

    def sleep(self):
        raise Exception("Robots don't sleep!")
```

**Good:**
```python
class Workable:
    def work(self):
        pass

class Eatable:
    def eat(self):
        pass

class Sleepable:
    def sleep(self):
        pass

class Human(Workable, Eatable, Sleepable):
    def work(self):
        return "Working"

    def eat(self):
        return "Eating"

    def sleep(self):
        return "Sleeping"

class Robot(Workable):
    def work(self):
        return "Working"
```

### D - Dependency Inversion Principle

Depend on abstractions, not concretions.

**Bad:**
```python
class MySQLDatabase:
    def save(self, data):
        print(f"Saving {data} to MySQL")

class UserService:
    def __init__(self):
        self.database = MySQLDatabase()  # Tight coupling!

    def save_user(self, user):
        self.database.save(user)
```

**Good:**
```python
from abc import ABC, abstractmethod

class Database(ABC):
    @abstractmethod
    def save(self, data):
        pass

class MySQLDatabase(Database):
    def save(self, data):
        print(f"Saving {data} to MySQL")

class PostgreSQLDatabase(Database):
    def save(self, data):
        print(f"Saving {data} to PostgreSQL")

class UserService:
    def __init__(self, database: Database):  # Depends on abstraction
        self.database = database

    def save_user(self, user):
        self.database.save(user)

# Usage - can swap implementations
service1 = UserService(MySQLDatabase())
service2 = UserService(PostgreSQLDatabase())
```

## Real-World Applications

### When to Use OOP

**Good Use Cases:**
1. **GUI Applications**: Objects naturally represent UI components
2. **Game Development**: Characters, items, worlds are objects
3. **Simulation Systems**: Model real-world entities
4. **Domain Modeling**: Business objects (Customer, Order, Invoice)
5. **Frameworks**: Plugin systems, extensible architectures

**Example: GUI Button (Python):**
```python
class Button:
    def __init__(self, text, x, y, width, height):
        self.text = text
        self.x = x
        self.y = y
        self.width = width
        self.height = height
        self.is_pressed = False
        self.click_handlers = []

    def add_click_handler(self, handler):
        self.click_handlers.append(handler)

    def click(self):
        self.is_pressed = True
        for handler in self.click_handlers:
            handler(self)
        self.is_pressed = False

    def render(self):
        # Rendering logic
        pass

# Extend for specific button types
class SubmitButton(Button):
    def __init__(self, x, y):
        super().__init__("Submit", x, y, 100, 40)

    def click(self):
        # Validation before clicking
        super().click()
```

### When NOT to Use OOP

**Better Use Functional:**
1. **Data Transformations**: Pipelines of pure functions
2. **Mathematical Computations**: Formulas, algorithms
3. **Concurrent Programming**: Immutability helps
4. **Web APIs**: Request → Process → Response

**Example: Data Pipeline (Haskell better than OOP):**
```haskell
-- Functional pipeline
processData :: [Int] -> [Int]
processData = filter (> 0)
            . map (* 2)
            . filter even

-- Clean, composable, testable

-- vs OOP version would be overly complex:
-- class DataProcessor with methods for each step
```

## Language Comparison Summary

| Feature | Python | C++ | Haskell |
|---------|--------|-----|---------|
| **Class Definition** | Simple, dynamic | Complex, static | No classes (ADTs) |
| **Inheritance** | Single/multiple | Single/multiple/virtual | None (type classes) |
| **Encapsulation** | By convention | True private/protected | Module exports |
| **Polymorphism** | Duck typing | Virtual functions/templates | Type classes/parametric |
| **Memory Model** | Garbage collected | Manual/smart pointers | Garbage collected |
| **Mutability** | Mutable by default | Mutable by default | Immutable by default |
| **Type System** | Dynamic | Static (compile-time) | Static (strong inference) |
| **Abstraction** | ABC, protocols | Pure virtual | Type classes |

## Key Takeaways

1. **OOP is one paradigm among many** - not always the best choice

2. **Python OOP**:
   - Dynamic and flexible
   - Duck typing enables polymorphism without strict types
   - Multiple inheritance with MRO
   - Convention-based encapsulation

3. **C++ OOP**:
   - Static type safety
   - Complex inheritance (virtual, override, protected)
   - True encapsulation
   - Memory management crucial
   - Templates for compile-time polymorphism

4. **Haskell Alternative**:
   - No traditional OOP
   - Type classes provide polymorphism
   - Algebraic data types instead of classes
   - Composition over inheritance (natural)
   - Immutability changes design patterns

5. **Favor composition over inheritance** in all languages

6. **SOLID principles** apply across paradigms

7. **Modern best practice**: Mix OOP with functional concepts

## Looking Ahead

While we focused on Python, C++, and Haskell, other languages offer interesting OOP variations:

- **Java**: Pure OOP, everything is an object
- **Rust**: Structs + traits, no inheritance, ownership system
- **JavaScript**: Prototype-based OOP, ES6 classes
- **Ruby**: Everything is an object, open classes, mixins

## Exercises

See `EXERCISES.md` for hands-on practice implementing OOP concepts in Python, C++, and Haskell.

---

**Next Lesson**: Higher-Order Functions - where we explore how functions can be treated as first-class citizens, complementing what we've learned about objects.
