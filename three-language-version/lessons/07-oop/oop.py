"""
Lesson 7: Object-Oriented Programming in Python

This file demonstrates OOP concepts in Python:
- Classes and objects
- Encapsulation
- Inheritance (single and multiple)
- Polymorphism
- Abstraction
- Special methods
- Properties
- Class/static methods
- Design patterns

Python's OOP Features:
- Dynamic typing with duck typing
- Multiple inheritance with MRO
- Convention-based encapsulation (_protected, __private)
- Rich special methods (__init__, __str__, __add__, etc.)
- Decorators (@property, @classmethod, @staticmethod)
"""

from abc import ABC, abstractmethod
from typing import List, Protocol
import math


# ============================================================================
# PART 1: BASIC CLASSES AND OBJECTS
# ============================================================================

class Person:
    """Basic class demonstrating core OOP concepts"""

    # Class variable (shared by all instances)
    species = "Homo sapiens"
    population = 0

    def __init__(self, name: str, age: int):
        """Constructor - initializes instance variables"""
        self.name = name  # Public instance variable
        self.age = age
        self._id = id(self)  # Protected (convention)
        self.__secret = "secret data"  # Private (name mangling)
        Person.population += 1

    def introduce(self) -> str:
        """Instance method"""
        return f"Hi, I'm {self.name}, {self.age} years old"

    def have_birthday(self) -> None:
        """Modifies instance state"""
        self.age += 1
        print(f"Happy birthday {self.name}! Now {self.age} years old.")

    @classmethod
    def get_population(cls) -> int:
        """Class method - operates on class, not instance"""
        return cls.population

    @staticmethod
    def is_adult(age: int) -> bool:
        """Static method - doesn't access class or instance"""
        return age >= 18

    @property
    def birth_year(self) -> int:
        """Property - computed attribute"""
        from datetime import datetime
        return datetime.now().year - self.age

    # Special methods (magic methods / dunder methods)
    def __str__(self) -> str:
        """String representation for users"""
        return f"{self.name} ({self.age})"

    def __repr__(self) -> str:
        """String representation for developers"""
        return f"Person('{self.name}', {self.age})"

    def __eq__(self, other) -> bool:
        """Equality comparison"""
        if not isinstance(other, Person):
            return False
        return self.name == other.name and self.age == other.age

    def __lt__(self, other) -> bool:
        """Less than comparison"""
        return self.age < other.age

    def __del__(self):
        """Destructor - called when object is garbage collected"""
        Person.population -= 1


# ============================================================================
# PART 2: ENCAPSULATION
# ============================================================================

class BankAccount:
    """Demonstrates encapsulation and data hiding"""

    def __init__(self, account_number: str, initial_balance: float = 0.0):
        self.account_number = account_number  # Public
        self._balance = initial_balance  # Protected (convention)
        self.__transactions = []  # Private (name mangling)

    def deposit(self, amount: float) -> bool:
        """Public method controlling access to private data"""
        if amount > 0:
            self._balance += amount
            self.__add_transaction("deposit", amount)
            return True
        return False

    def withdraw(self, amount: float) -> bool:
        """Validates before modifying state"""
        if 0 < amount <= self._balance:
            self._balance -= amount
            self.__add_transaction("withdrawal", amount)
            return True
        return False

    def __add_transaction(self, type: str, amount: float) -> None:
        """Private helper method"""
        from datetime import datetime
        self.__transactions.append({
            'type': type,
            'amount': amount,
            'balance_after': self._balance,
            'timestamp': datetime.now()
        })

    @property
    def balance(self) -> float:
        """Property for clean access"""
        return self._balance

    def get_transaction_history(self) -> List[dict]:
        """Returns copy to prevent external modification"""
        return self.__transactions.copy()


# ============================================================================
# PART 3: INHERITANCE - SINGLE INHERITANCE
# ============================================================================

class Animal:
    """Base class for animals"""

    def __init__(self, name: str, age: int):
        self.name = name
        self.age = age

    def speak(self) -> str:
        """Will be overridden by subclasses"""
        return f"{self.name} makes a sound"

    def describe(self) -> str:
        """Concrete method available to all subclasses"""
        return f"{self.name} is a {self.age} year old {self.__class__.__name__}"


class Dog(Animal):
    """Derived class - inherits from Animal"""

    def __init__(self, name: str, age: int, breed: str):
        super().__init__(name, age)  # Call parent constructor
        self.breed = breed

    def speak(self) -> str:
        """Override parent method"""
        return f"{self.name} says Woof!"

    def fetch(self) -> str:
        """New method specific to Dog"""
        return f"{self.name} is fetching the ball!"


class Cat(Animal):
    """Another derived class"""

    def speak(self) -> str:
        return f"{self.name} says Meow!"

    def climb(self) -> str:
        return f"{self.name} is climbing a tree!"


# ============================================================================
# PART 4: MULTIPLE INHERITANCE
# ============================================================================

class Flyable:
    """Mixin for flying behavior"""

    def fly(self) -> str:
        return f"{self.name} is flying through the air"

    def land(self) -> str:
        return f"{self.name} is landing"


class Swimmable:
    """Mixin for swimming behavior"""

    def swim(self) -> str:
        return f"{self.name} is swimming in water"

    def dive(self) -> str:
        return f"{self.name} is diving deep"


class Duck(Animal, Flyable, Swimmable):
    """Multiple inheritance - combines multiple parent classes"""

    def __init__(self, name: str, age: int):
        Animal.__init__(self, name, age)

    def speak(self) -> str:
        return f"{self.name} says Quack!"

    def waddle(self) -> str:
        return f"{self.name} is waddling"


# Method Resolution Order (MRO) example
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
    """Diamond problem - resolved by MRO"""
    pass


# ============================================================================
# PART 5: ABSTRACTION - ABSTRACT BASE CLASSES
# ============================================================================

class Shape(ABC):
    """Abstract base class - cannot be instantiated"""

    def __init__(self, color: str):
        self.color = color

    @abstractmethod
    def area(self) -> float:
        """Abstract method - must be implemented by subclasses"""
        pass

    @abstractmethod
    def perimeter(self) -> float:
        """Abstract method"""
        pass

    def describe(self) -> str:
        """Concrete method in abstract class"""
        return f"A {self.color} {self.__class__.__name__} with area {self.area():.2f}"


class Circle(Shape):
    """Concrete implementation of Shape"""

    def __init__(self, color: str, radius: float):
        super().__init__(color)
        self.radius = radius

    def area(self) -> float:
        return math.pi * self.radius ** 2

    def perimeter(self) -> float:
        return 2 * math.pi * self.radius


class Rectangle(Shape):
    """Another concrete implementation"""

    def __init__(self, color: str, width: float, height: float):
        super().__init__(color)
        self.width = width
        self.height = height

    def area(self) -> float:
        return self.width * self.height

    def perimeter(self) -> float:
        return 2 * (self.width + self.height)


# ============================================================================
# PART 6: POLYMORPHISM
# ============================================================================

def print_animal_sounds(animals: List[Animal]) -> None:
    """Polymorphism - same interface, different implementations"""
    for animal in animals:
        print(animal.speak())  # Each animal responds differently


# Duck typing - doesn't require inheritance
class Robot:
    """Not an Animal, but has speak() method"""

    def __init__(self, name: str):
        self.name = name

    def speak(self) -> str:
        return f"{self.name} says Beep boop!"


# Protocol typing (Python 3.8+) - structural subtyping
class Speakable(Protocol):
    """Protocol defines an interface"""
    name: str

    def speak(self) -> str:
        ...


def make_it_speak(thing: Speakable) -> None:
    """Works with anything matching the protocol"""
    print(thing.speak())


# ============================================================================
# PART 7: OPERATOR OVERLOADING
# ============================================================================

class Vector2D:
    """Demonstrates operator overloading"""

    def __init__(self, x: float, y: float):
        self.x = x
        self.y = y

    def __add__(self, other):
        """Overload + operator"""
        return Vector2D(self.x + other.x, self.y + other.y)

    def __sub__(self, other):
        """Overload - operator"""
        return Vector2D(self.x - other.x, self.y - other.y)

    def __mul__(self, scalar):
        """Overload * operator for scalar multiplication"""
        return Vector2D(self.x * scalar, self.y * scalar)

    def __rmul__(self, scalar):
        """Reverse multiply for scalar * vector"""
        return self.__mul__(scalar)

    def __eq__(self, other):
        """Overload == operator"""
        return self.x == other.x and self.y == other.y

    def __str__(self):
        """String representation"""
        return f"Vector2D({self.x}, {self.y})"

    def __repr__(self):
        return f"Vector2D({self.x}, {self.y})"

    def __abs__(self):
        """Overload abs() function - returns magnitude"""
        return math.sqrt(self.x ** 2 + self.y ** 2)


# ============================================================================
# PART 8: PROPERTIES AND DESCRIPTORS
# ============================================================================

class Temperature:
    """Demonstrates properties for computed attributes"""

    def __init__(self, celsius: float):
        self._celsius = celsius

    @property
    def celsius(self) -> float:
        """Getter for celsius"""
        return self._celsius

    @celsius.setter
    def celsius(self, value: float):
        """Setter with validation"""
        if value < -273.15:
            raise ValueError("Temperature below absolute zero!")
        self._celsius = value

    @property
    def fahrenheit(self) -> float:
        """Computed property"""
        return self._celsius * 9/5 + 32

    @fahrenheit.setter
    def fahrenheit(self, value: float):
        """Set celsius from fahrenheit"""
        self.celsius = (value - 32) * 5/9

    @property
    def kelvin(self) -> float:
        return self._celsius + 273.15

    @kelvin.setter
    def kelvin(self, value: float):
        self.celsius = value - 273.15


# ============================================================================
# PART 9: COMPOSITION OVER INHERITANCE
# ============================================================================

class Engine:
    """Component class"""

    def __init__(self, horsepower: int, fuel_type: str):
        self.horsepower = horsepower
        self.fuel_type = fuel_type
        self.running = False

    def start(self) -> str:
        self.running = True
        return f"Engine starting... {self.horsepower} HP {self.fuel_type} engine roaring!"

    def stop(self) -> str:
        self.running = False
        return "Engine stopping..."


class Vehicle:
    """Uses composition instead of inheritance"""

    def __init__(self, brand: str, engine: Engine = None):
        self.brand = brand
        self.engine = engine  # HAS-A relationship

    def start(self) -> str:
        if self.engine:
            return f"{self.brand}: {self.engine.start()}"
        return f"{self.brand}: No engine - use pedal power!"


# ============================================================================
# PART 10: DESIGN PATTERNS
# ============================================================================

# Singleton Pattern
class Logger:
    """Singleton - only one instance exists"""

    _instance = None

    def __new__(cls):
        if cls._instance is None:
            cls._instance = super().__new__(cls)
            cls._instance._logs = []
        return cls._instance

    def log(self, message: str) -> None:
        from datetime import datetime
        self._logs.append(f"[{datetime.now()}] {message}")

    def get_logs(self) -> List[str]:
        return self._logs.copy()


# Factory Pattern
class AnimalFactory:
    """Factory - creates objects without specifying exact class"""

    @staticmethod
    def create_animal(animal_type: str, name: str, age: int):
        if animal_type == "dog":
            return Dog(name, age, "Unknown")
        elif animal_type == "cat":
            return Cat(name, age)
        elif animal_type == "duck":
            return Duck(name, age)
        else:
            raise ValueError(f"Unknown animal type: {animal_type}")


# Observer Pattern
class Subject:
    """Subject in observer pattern"""

    def __init__(self):
        self._observers = []
        self._state = None

    def attach(self, observer) -> None:
        if observer not in self._observers:
            self._observers.append(observer)

    def detach(self, observer) -> None:
        self._observers.remove(observer)

    def notify(self) -> None:
        for observer in self._observers:
            observer.update(self)

    def set_state(self, state) -> None:
        self._state = state
        self.notify()

    def get_state(self):
        return self._state


class Observer:
    """Observer in observer pattern"""

    def __init__(self, name: str):
        self.name = name

    def update(self, subject: Subject) -> None:
        print(f"{self.name} notified. New state: {subject.get_state()}")


# ============================================================================
# DEMONSTRATION AND TESTING
# ============================================================================

def main():
    print("=" * 70)
    print("LESSON 7: OBJECT-ORIENTED PROGRAMMING IN PYTHON")
    print("=" * 70)

    # Part 1: Basic Classes
    print("\n--- PART 1: BASIC CLASSES ---")
    alice = Person("Alice", 30)
    bob = Person("Bob", 25)
    print(alice.introduce())
    print(f"Population: {Person.get_population()}")
    print(f"Is adult? {Person.is_adult(alice.age)}")
    print(f"Birth year: {alice.birth_year}")
    print(f"String representation: {alice}")
    print(f"Repr: {repr(alice)}")
    print(f"Alice < Bob? {alice < bob}")

    # Part 2: Encapsulation
    print("\n--- PART 2: ENCAPSULATION ---")
    account = BankAccount("ACC001", 1000)
    account.deposit(500)
    account.withdraw(200)
    print(f"Balance: ${account.balance}")
    print(f"Transactions: {len(account.get_transaction_history())}")

    # Part 3: Inheritance
    print("\n--- PART 3: INHERITANCE ---")
    dog = Dog("Buddy", 3, "Golden Retriever")
    cat = Cat("Whiskers", 2)
    print(dog.describe())
    print(dog.speak())
    print(dog.fetch())
    print(cat.speak())
    print(cat.climb())

    # Part 4: Multiple Inheritance
    print("\n--- PART 4: MULTIPLE INHERITANCE ---")
    duck = Duck("Donald", 1)
    print(duck.speak())
    print(duck.fly())
    print(duck.swim())
    print(duck.waddle())
    print(f"MRO for Duck: {Duck.__mro__}")
    print(f"MRO for D (diamond): {D.__mro__}")
    print(f"D.method() returns: {D().method()}")  # Uses MRO

    # Part 5: Abstraction
    print("\n--- PART 5: ABSTRACTION ---")
    shapes = [
        Circle("red", 5),
        Rectangle("blue", 4, 6),
        Circle("green", 3)
    ]
    for shape in shapes:
        print(shape.describe())

    # Part 6: Polymorphism
    print("\n--- PART 6: POLYMORPHISM ---")
    animals = [dog, cat, duck]
    print_animal_sounds(animals)

    robot = Robot("R2D2")
    make_it_speak(dog)    # Works with Animal
    make_it_speak(robot)  # Works with Robot (duck typing)

    # Part 7: Operator Overloading
    print("\n--- PART 7: OPERATOR OVERLOADING ---")
    v1 = Vector2D(3, 4)
    v2 = Vector2D(1, 2)
    v3 = v1 + v2
    v4 = v1 * 2
    v5 = 2 * v1  # Uses __rmul__
    print(f"v1 = {v1}")
    print(f"v2 = {v2}")
    print(f"v1 + v2 = {v3}")
    print(f"v1 * 2 = {v4}")
    print(f"2 * v1 = {v5}")
    print(f"|v1| = {abs(v1):.2f}")

    # Part 8: Properties
    print("\n--- PART 8: PROPERTIES ---")
    temp = Temperature(0)
    print(f"0°C = {temp.fahrenheit}°F = {temp.kelvin}K")
    temp.fahrenheit = 212
    print(f"212°F = {temp.celsius}°C")

    # Part 9: Composition
    print("\n--- PART 9: COMPOSITION ---")
    engine = Engine(200, "gasoline")
    car = Vehicle("Toyota", engine)
    bicycle = Vehicle("Schwinn", None)
    print(car.start())
    print(bicycle.start())

    # Part 10: Design Patterns
    print("\n--- PART 10: DESIGN PATTERNS ---")

    # Singleton
    logger1 = Logger()
    logger2 = Logger()
    logger1.log("First message")
    logger2.log("Second message")
    print(f"Logger is singleton? {logger1 is logger2}")
    print(f"Logs: {logger1.get_logs()}")

    # Factory
    factory = AnimalFactory()
    factory_dog = factory.create_animal("dog", "Max", 2)
    print(f"Factory created: {factory_dog.speak()}")

    # Observer
    subject = Subject()
    obs1 = Observer("Observer 1")
    obs2 = Observer("Observer 2")
    subject.attach(obs1)
    subject.attach(obs2)
    subject.set_state("State Changed!")

    print("\n" + "=" * 70)
    print("Python OOP demonstration complete!")
    print("=" * 70)


if __name__ == "__main__":
    main()
