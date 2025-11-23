# Lesson 7: Object-Oriented Programming - Exercises

## Instructions

Complete these exercises to practice object-oriented programming concepts across different paradigms. Pay special attention to how different languages approach encapsulation, inheritance, polymorphism, and abstraction.

---

## Exercise 1: Basic Class Creation (Warmup)

**Difficulty:** Very Easy

Create a `Book` class with the following:
- Properties: `title` (string), `author` (string), `pages` (number), `currentPage` (number, default 0)
- Methods:
  - `read(numPages)` - advances currentPage
  - `percentComplete()` - returns percentage read
  - `isFinished()` - returns true if currentPage >= pages

**Implement in:** Python, JavaScript, or Java

**Test your class:**
```
book = Book("1984", "George Orwell", 328)
book.read(50)
print(book.percentComplete())  # ~15.24%
```

---

## Exercise 2: Encapsulation Challenge

**Difficulty:** Easy

Create a `Password` class that:
- Stores a password (private/protected)
- Has a method `check(password)` that returns true/false
- Has a method `changePassword(oldPassword, newPassword)`
- Never exposes the actual password

**Rules:**
- Password should NOT be directly accessible
- Passwords must be at least 8 characters

**Implement in:** Python, Java, or JavaScript (with private fields)

**Bonus:** Hash the password instead of storing it in plain text.

---

## Exercise 3: Inheritance - Shape Hierarchy

**Difficulty:** Easy to Medium

Create a shape class hierarchy:

1. Base class `Shape` with:
   - `color` property
   - Abstract method `area()`
   - Abstract method `perimeter()`
   - Concrete method `describe()` that prints shape info

2. Subclasses:
   - `Square(side, color)`
   - `Circle(radius, color)`
   - `Parallelogram(base, height, side, color)`

**Implement in:** Python, Java, or Ruby

**Test:**
```
shapes = [Square(5, "red"), Circle(3, "blue"), Parallelogram(4, 3, 5, "green")]
for shape in shapes:
    shape.describe()
```

---

## Exercise 4: Composition vs Inheritance

**Difficulty:** Medium

You need to model vehicles. You have:
- Cars (have engine, 4 wheels, doors)
- Motorcycles (have engine, 2 wheels, no doors)
- Bicycles (no engine, 2 wheels, no doors)
- Boats (have engine, no wheels, no doors)

**Part A:** Try to model this with inheritance. Where does it break down?

**Part B:** Remodel using composition:
- Create `Engine`, `Wheel`, `Door` classes
- Create vehicle classes that compose these parts

**Implement in:** Your choice of OOP language

**Question:** Why is composition better here?

---

## Exercise 5: Polymorphism with Animals

**Difficulty:** Medium

Create an animal simulation:

1. Base class/interface `Animal` with:
   - `name` property
   - `makeSound()` method
   - `move()` method

2. Implement at least 4 different animals:
   - `Dog` - barks, runs
   - `Fish` - silent (or bubbles), swims
   - `Bird` - chirps, flies
   - `Snake` - hisses, slithers

3. Create a function `makeAllAnimalsAct(animals)` that takes a list of animals and calls both methods on each

**Implement in:** Python, Java, JavaScript, or Rust (traits)

**Test:**
```
zoo = [Dog("Buddy"), Fish("Nemo"), Bird("Tweety"), Snake("Slinky")]
makeAllAnimalsAct(zoo)
```

---

## Exercise 6: Design Pattern - Singleton Logger

**Difficulty:** Medium

Implement a `Logger` singleton that:
- Only has one instance (enforce this!)
- Has method `log(message)` that stores log messages
- Has method `getLogMessages()` that returns all messages
- Has method `clearLogs()` that clears all messages

**Implement in:** Python, Java, or JavaScript

**Test:**
```
logger1 = Logger.getInstance()
logger2 = Logger.getInstance()
logger1.log("First message")
logger2.log("Second message")
logger1.getLogMessages()  # Should show both messages
logger1 === logger2  # Should be true
```

**Questions:**
- Why might you want a singleton?
- What are downsides of singletons?

---

## Exercise 7: Bank Account with Transactions

**Difficulty:** Medium to Hard

Create a banking system with:

1. `BankAccount` class:
   - Private balance
   - Account number
   - `deposit(amount)` - validates amount > 0
   - `withdraw(amount)` - validates sufficient funds
   - `getBalance()`
   - `getTransactionHistory()` - returns list of all transactions

2. `SavingsAccount` extends `BankAccount`:
   - Adds `interestRate` property
   - Method `applyInterest()` - adds interest to balance
   - Overrides `withdraw()` to limit to 6 withdrawals per month

3. `CheckingAccount` extends `BankAccount`:
   - Has `overdraftLimit` property
   - Overrides `withdraw()` to allow overdraft up to limit
   - Charges $35 fee for overdrafts

**Implement in:** Python, Java, or Ruby

**Test various scenarios:**
- Normal deposits/withdrawals
- Savings account interest application
- Withdrawal limits on savings
- Overdraft on checking

---

## Exercise 8: Interfaces and Multiple Inheritance

**Difficulty:** Medium

Create interfaces for different capabilities:

1. Interfaces:
   - `Flyable` - `fly()`, `land()`
   - `Swimmable` - `swim()`, `dive()`
   - `Walkable` - `walk()`, `run()`

2. Implement classes that combine these:
   - `Duck` - can fly, swim, walk
   - `Penguin` - can swim, walk (but not fly!)
   - `Airplane` - can fly (mechanical, not an animal)
   - `Fish` - can swim only

**Implement in:** Java (interfaces), Rust (traits), or Python (abstract base classes)

**Challenge:** Make sure `Duck` and `Airplane` can both be passed to a function that expects `Flyable`, even though they're unrelated classes.

---

## Exercise 9: Factory Pattern

**Difficulty:** Medium

Create a notification system:

1. Interface/base class `Notification`:
   - `send(message, recipient)`

2. Implementations:
   - `EmailNotification` - prints "Sending email to {recipient}: {message}"
   - `SMSNotification` - prints "Sending SMS to {recipient}: {message}"
   - `PushNotification` - prints "Sending push notification to {recipient}: {message}"

3. `NotificationFactory`:
   - Method `createNotification(type)` that returns the appropriate notification object

**Implement in:** Python, JavaScript, or Java

**Test:**
```
factory = NotificationFactory()
notif = factory.createNotification("email")
notif.send("Hello!", "user@example.com")
```

**Bonus:** Add a `SlackNotification` type without modifying existing code.

---

## Exercise 10: OOP in Functional Languages

**Difficulty:** Hard (Conceptual)

**Part A: Haskell**

Model a simple RPG character system in Haskell:
- Characters have name, health, strength
- Different character types: Warrior, Mage, Rogue
- Each has a different `attack()` behavior
- Use type classes to achieve polymorphism

**Part B: Compare**

Implement the same system in Python with classes, then compare:
- Which is more concise?
- Which is easier to extend?
- What guarantees does the Haskell version provide?

---

## Exercise 11: Method Overloading vs Overriding

**Difficulty:** Medium (Conceptual)

**Part A:**
Create a `Calculator` class with method overloading (if your language supports it):
- `add(a, b)` - adds two numbers
- `add(a, b, c)` - adds three numbers
- `add(numbers)` - adds a list/array of numbers

**Implement in:** Java (true overloading) and Python (default parameters)

**Part B:**
Create a base `Shape` class with `area()` method, and override it in `Circle` and `Rectangle` subclasses.

**Questions:**
- What's the difference between overloading and overriding?
- Which languages support method overloading?

---

## Exercise 12: Static vs Instance Members

**Difficulty:** Easy to Medium

Create a `Car` class with:

**Instance members:**
- `make`, `model`, `year` (properties)
- `honk()` - instance method

**Static members:**
- `totalCarsCreated` - class variable (incremented in constructor)
- `compareAge(car1, car2)` - static method that returns which car is older

**Implement in:** Python, Java, or JavaScript

**Test:**
```
car1 = Car("Toyota", "Camry", 2010)
car2 = Car("Honda", "Civic", 2015)
car1.honk()
Car.totalCarsCreated  # Should be 2
Car.compareAge(car1, car2)  # Should return car1
```

---

## Exercise 13: Operator Overloading

**Difficulty:** Medium

Create a `Vector2D` class representing a 2D vector:
- Properties: `x`, `y`
- Overload operators:
  - `+` - vector addition
  - `-` - vector subtraction
  - `*` - scalar multiplication
  - `==` - equality check
  - `str/toString` - string representation

**Implement in:** Python, Ruby, or C++

**Test:**
```
v1 = Vector2D(3, 4)
v2 = Vector2D(1, 2)
v3 = v1 + v2  # Vector2D(4, 6)
v4 = v1 * 2   # Vector2D(6, 8)
print(v1)     # "Vector2D(3, 4)"
```

**Note:** Not all languages support operator overloading. How would you handle this in Java?

---

## Exercise 14: Property Decorators and Getters/Setters

**Difficulty:** Medium

Create a `Temperature` class:
- Store temperature in Celsius (private)
- Use properties/getters/setters for:
  - `celsius` - get/set in Celsius
  - `fahrenheit` - get/set in Fahrenheit (converts)
  - `kelvin` - get/set in Kelvin (converts)
- Validate: temperature cannot be below absolute zero (-273.15°C)

**Implement in:** Python (@property), JavaScript (get/set), or Java (getters/setters)

**Test:**
```
temp = Temperature(0)
temp.fahrenheit = 212
print(temp.celsius)  # 100
temp.kelvin = 0  # Should raise error (below absolute zero)
```

---

## Exercise 15: Mixin Pattern (Advanced)

**Difficulty:** Hard

Create a mixin system for adding capabilities to objects:

1. Create mixins:
   - `TimestampMixin` - adds `createdAt` and `updatedAt` tracking
   - `SerializableMixin` - adds `toJSON()` and `fromJSON()` methods
   - `ValidatableMixin` - adds `validate()` method

2. Create a `User` class that uses all three mixins

**Implement in:** Ruby, Python, or JavaScript

**Bonus:** Create a `Product` class that uses only `TimestampMixin` and `SerializableMixin`.

---

## Challenge Exercise: Mini Object System

**Difficulty:** Very Hard

Implement a minimal OOP system in a non-OOP language:

**Choose one:**

**Option A: C**
- Create a class system using structs and function pointers
- Implement a vtable for polymorphism
- Create at least two classes with inheritance

**Option B: Prolog**
- Model objects as facts
- Implement inheritance through rules
- Create polymorphic behavior

**Option C: Haskell**
- Model OOP concepts using type classes
- Create a class hierarchy using algebraic data types
- Implement polymorphism with type class constraints

This exercise shows you what OOP languages do "under the hood"!

---

## Reflection Questions

After completing these exercises, reflect on:

1. **When is OOP the right choice?** What problems does it solve well?

2. **When might OOP not be ideal?** Where do functional or procedural approaches shine?

3. **Inheritance vs Composition:** When did you reach for each? What guided your decision?

4. **Language differences:** How do different languages approach OOP? Which felt most natural?

5. **The four pillars:** Which pillar (encapsulation, inheritance, polymorphism, abstraction) did you find most useful? Most confusing?

6. **Real-world modeling:** How well does OOP map to real-world concepts? Where does the metaphor break down?
