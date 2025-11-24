#!/usr/bin/env python3
"""
Lesson 7: Object-Oriented Programming in Python

Python is a multi-paradigm language with full OOP support.
Everything in Python is an object!
"""

from abc import ABC, abstractmethod
import math

# ====================
# 1. Basic Class
# ====================

class Person:
    """Basic class demonstrating encapsulation"""

    def __init__(self, name, age):
        self.name = name
        self._age = age  # "Protected" (convention only)

    def introduce(self):
        return f"Hi, I'm {self.name}, {self._age} years old"

    def have_birthday(self):
        self._age += 1

    def get_age(self):
        return self._age


# ====================
# 2. Inheritance
# ====================

class Animal:
    """Base class for animals"""

    def __init__(self, name, species):
        self.name = name
        self.species = species

    def speak(self):
        return "Some generic animal sound"

    def sleep(self):
        return f"{self.name} is sleeping... Zzz"


class Dog(Animal):
    """Dog inherits from Animal"""

    def __init__(self, name, breed):
        super().__init__(name, "Canine")
        self.breed = breed

    def speak(self):  # Override
        return f"{self.name} says Woof!"

    def fetch(self):
        return f"{self.name} is fetching the ball!"


class Cat(Animal):
    """Cat inherits from Animal"""

    def __init__(self, name, indoor=True):
        super().__init__(name, "Feline")
        self.indoor = indoor

    def speak(self):  # Override
        return f"{self.name} says Meow!"

    def scratch(self):
        return f"{self.name} is scratching the furniture!"


# ====================
# 3. Abstract Classes
# ====================

class Shape(ABC):
    """Abstract base class for shapes"""

    @abstractmethod
    def area(self):
        """Calculate area - must be implemented by subclasses"""
        pass

    @abstractmethod
    def perimeter(self):
        """Calculate perimeter - must be implemented by subclasses"""
        pass

    def describe(self):
        """Concrete method available to all shapes"""
        return f"{self.__class__.__name__}: area={self.area():.2f}, perimeter={self.perimeter():.2f}"


class Circle(Shape):
    """Circle implementation"""

    def __init__(self, radius):
        self.radius = radius

    def area(self):
        return math.pi * self.radius ** 2

    def perimeter(self):
        return 2 * math.pi * self.radius


class Rectangle(Shape):
    """Rectangle implementation"""

    def __init__(self, width, height):
        self.width = width
        self.height = height

    def area(self):
        return self.width * self.height

    def perimeter(self):
        return 2 * (self.width + self.height)


class Triangle(Shape):
    """Triangle implementation"""

    def __init__(self, side_a, side_b, side_c):
        self.side_a = side_a
        self.side_b = side_b
        self.side_c = side_c

    def area(self):
        # Heron's formula
        s = self.perimeter() / 2
        return math.sqrt(s * (s - self.side_a) * (s - self.side_b) * (s - self.side_c))

    def perimeter(self):
        return self.side_a + self.side_b + self.side_c


# ====================
# 4. Encapsulation (Bank Account)
# ====================

class BankAccount:
    """Demonstrates encapsulation and data hiding"""

    def __init__(self, account_number, initial_balance=0):
        self.account_number = account_number
        self.__balance = initial_balance  # Private attribute
        self.__transactions = []

    def deposit(self, amount):
        if amount > 0:
            self.__balance += amount
            self.__transactions.append(f"Deposit: +${amount}")
            return True
        return False

    def withdraw(self, amount):
        if 0 < amount <= self.__balance:
            self.__balance -= amount
            self.__transactions.append(f"Withdrawal: -${amount}")
            return True
        return False

    def get_balance(self):
        return self.__balance

    def get_transaction_history(self):
        return self.__transactions.copy()


# ====================
# 5. Class and Static Methods
# ====================

class Temperature:
    """Demonstrates class and static methods"""

    # Class variable
    conversion_count = 0

    def __init__(self, celsius):
        self.celsius = celsius

    @classmethod
    def from_fahrenheit(cls, fahrenheit):
        """Factory method using class method"""
        cls.conversion_count += 1
        celsius = (fahrenheit - 32) * 5 / 9
        return cls(celsius)

    @classmethod
    def from_kelvin(cls, kelvin):
        """Factory method using class method"""
        cls.conversion_count += 1
        celsius = kelvin - 273.15
        return cls(celsius)

    @staticmethod
    def is_freezing(celsius):
        """Utility method that doesn't need instance or class"""
        return celsius <= 0

    def to_fahrenheit(self):
        return (self.celsius * 9 / 5) + 32

    def to_kelvin(self):
        return self.celsius + 273.15


# ====================
# 6. Composition
# ====================

class Engine:
    """Component class"""

    def __init__(self, horsepower):
        self.horsepower = horsepower
        self.running = False

    def start(self):
        self.running = True
        return f"Engine starting... {self.horsepower}hp engine now running"

    def stop(self):
        self.running = False
        return "Engine stopped"


class Car:
    """Uses composition - Car HAS-AN Engine"""

    def __init__(self, brand, model, horsepower):
        self.brand = brand
        self.model = model
        self.engine = Engine(horsepower)  # Composition!

    def start(self):
        return f"{self.brand} {self.model}: {self.engine.start()}"

    def stop(self):
        return f"{self.brand} {self.model}: {self.engine.stop()}"


# ====================
# 7. Special Methods (Magic Methods)
# ====================

class Point:
    """Demonstrates operator overloading with magic methods"""

    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __str__(self):
        """String representation"""
        return f"Point({self.x}, {self.y})"

    def __repr__(self):
        """Official representation"""
        return f"Point({self.x}, {self.y})"

    def __eq__(self, other):
        """Equality operator =="""
        if not isinstance(other, Point):
            return False
        return self.x == other.x and self.y == other.y

    def __add__(self, other):
        """Addition operator +"""
        return Point(self.x + other.x, self.y + other.y)

    def __sub__(self, other):
        """Subtraction operator -"""
        return Point(self.x - other.x, self.y - other.y)

    def distance_from_origin(self):
        return math.sqrt(self.x ** 2 + self.y ** 2)


# ====================
# 8. Property Decorators
# ====================

class Circle2:
    """Demonstrates @property decorator"""

    def __init__(self, radius):
        self._radius = radius

    @property
    def radius(self):
        """Getter for radius"""
        return self._radius

    @radius.setter
    def radius(self, value):
        """Setter with validation"""
        if value < 0:
            raise ValueError("Radius cannot be negative")
        self._radius = value

    @property
    def diameter(self):
        """Computed property"""
        return self._radius * 2

    @property
    def area(self):
        """Computed property"""
        return math.pi * self._radius ** 2


# ====================
# 9. Design Pattern: Singleton
# ====================

class Singleton:
    """Singleton pattern - only one instance allowed"""

    _instance = None

    def __new__(cls):
        if cls._instance is None:
            cls._instance = super().__new__(cls)
            cls._instance.initialized = False
        return cls._instance

    def __init__(self):
        if not self.initialized:
            self.data = []
            self.initialized = True


# ====================
# 10. Design Pattern: Factory
# ====================

class AnimalFactory:
    """Factory pattern for creating animals"""

    @staticmethod
    def create_animal(animal_type, name):
        if animal_type.lower() == "dog":
            return Dog(name, "Mixed")
        elif animal_type.lower() == "cat":
            return Cat(name)
        else:
            return Animal(name, "Unknown")


# ====================
# Tests and Examples
# ====================

def main():
    print("=== Object-Oriented Programming in Python ===\n")

    # 1. Basic class
    print("1. Basic Class:")
    alice = Person("Alice", 30)
    print(f"   {alice.introduce()}")
    alice.have_birthday()
    print(f"   After birthday: age = {alice.get_age()}")

    # 2. Inheritance and Polymorphism
    print("\n2. Inheritance and Polymorphism:")
    animals = [
        Dog("Buddy", "Golden Retriever"),
        Cat("Whiskers", indoor=True),
        Dog("Max", "German Shepherd")
    ]

    for animal in animals:
        print(f"   {animal.speak()}")

    print(f"   {animals[0].fetch()}")
    print(f"   {animals[1].scratch()}")

    # 3. Abstract classes and shapes
    print("\n3. Abstract Classes (Shapes):")
    shapes = [
        Circle(5),
        Rectangle(4, 6),
        Triangle(3, 4, 5)
    ]

    for shape in shapes:
        print(f"   {shape.describe()}")

    # 4. Encapsulation (Bank Account)
    print("\n4. Encapsulation (Bank Account):")
    account = BankAccount("ACC001", 1000)
    print(f"   Initial balance: ${account.get_balance()}")
    account.deposit(500)
    print(f"   After deposit: ${account.get_balance()}")
    account.withdraw(200)
    print(f"   After withdrawal: ${account.get_balance()}")
    print(f"   Transactions: {account.get_transaction_history()}")

    # 5. Class and static methods
    print("\n5. Class and Static Methods:")
    temp1 = Temperature(0)
    temp2 = Temperature.from_fahrenheit(32)
    temp3 = Temperature.from_kelvin(273.15)

    print(f"   0°C = {temp1.to_fahrenheit():.1f}°F")
    print(f"   32°F = {temp2.celsius:.1f}°C")
    print(f"   273.15K = {temp3.celsius:.1f}°C")
    print(f"   Is 0°C freezing? {Temperature.is_freezing(0)}")
    print(f"   Conversions made: {Temperature.conversion_count}")

    # 6. Composition
    print("\n6. Composition:")
    car = Car("Toyota", "Camry", 200)
    print(f"   {car.start()}")
    print(f"   {car.stop()}")

    # 7. Special methods
    print("\n7. Special Methods (Operator Overloading):")
    p1 = Point(3, 4)
    p2 = Point(1, 2)
    print(f"   p1 = {p1}")
    print(f"   p2 = {p2}")
    print(f"   p1 + p2 = {p1 + p2}")
    print(f"   p1 - p2 = {p1 - p2}")
    print(f"   p1 == p2? {p1 == p2}")
    print(f"   p1 distance from origin: {p1.distance_from_origin():.2f}")

    # 8. Properties
    print("\n8. Properties:")
    circle = Circle2(5)
    print(f"   Radius: {circle.radius}")
    print(f"   Diameter: {circle.diameter}")
    print(f"   Area: {circle.area:.2f}")
    circle.radius = 10
    print(f"   After setting radius to 10:")
    print(f"   Diameter: {circle.diameter}")
    print(f"   Area: {circle.area:.2f}")

    # 9. Singleton
    print("\n9. Singleton Pattern:")
    s1 = Singleton()
    s2 = Singleton()
    print(f"   s1 is s2? {s1 is s2}")
    s1.data.append("item1")
    print(f"   s1.data: {s1.data}")
    print(f"   s2.data: {s2.data}")

    # 10. Factory
    print("\n10. Factory Pattern:")
    dog = AnimalFactory.create_animal("dog", "Rover")
    cat = AnimalFactory.create_animal("cat", "Mittens")
    print(f"   {dog.speak()}")
    print(f"   {cat.speak()}")


if __name__ == "__main__":
    main()
