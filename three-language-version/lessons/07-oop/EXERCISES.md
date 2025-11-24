# Lesson 7: Object-Oriented Programming - Exercises

## Instructions

Complete these exercises to practice object-oriented programming concepts in Python, C++, and Haskell. Focus on understanding how each language approaches OOP differently, and how Haskell achieves similar results using functional programming techniques.

**Target Languages:** Python (traditional OOP), C++ (static OOP), Haskell (type classes/ADTs)

---

## Exercise 1: Basic Class Creation (Warmup)

**Difficulty:** Easy
**Recommended Languages:** Python, C++

Create a `Book` class with the following:

**Properties:**
- `title` (string)
- `author` (string)
- `pages` (integer)
- `currentPage` (integer, starts at 0)

**Methods:**
- `read(numPages)` - advances currentPage by numPages (don't exceed total pages)
- `percentComplete()` - returns percentage read (0-100)
- `isFinished()` - returns true if currentPage >= pages

**Python Test:**
```python
book = Book("1984", "George Orwell", 328)
book.read(50)
print(book.percent_complete())  # ~15.24
print(book.is_finished())        # False
book.read(300)
print(book.is_finished())        # True
```

**C++ Test:**
```cpp
Book book("1984", "George Orwell", 328);
book.read(50);
std::cout << book.percentComplete() << std::endl;  // ~15.24
std::cout << book.isFinished() << std::endl;       // 0 (false)
```

**Bonus Challenge (Haskell):**
Implement the same concept using immutable data types where each operation returns a new Book.

---

## Exercise 2: Encapsulation Challenge

**Difficulty:** Easy to Medium
**Recommended Languages:** Python, C++, Haskell

Create a `Password` class that:
- Stores a password securely (private/encapsulated)
- Has a method `check(password)` that returns true/false
- Has a method `changePassword(oldPassword, newPassword)`
- Never exposes the actual password directly
- Enforces minimum length of 8 characters

**Python Requirements:**
- Use name mangling (`__password`) for true privacy
- Use properties if appropriate

**C++ Requirements:**
- Use private members
- Proper const correctness

**Haskell Requirements:**
- Use module exports to hide the constructor
- Return `Maybe Password` for validation

**Bonus:** Hash the password using a simple algorithm instead of storing plain text.

---

## Exercise 3: Inheritance - Shape Hierarchy

**Difficulty:** Medium
**Recommended Languages:** Python, C++

Create a shape class hierarchy:

**1. Base class `Shape`:**
- Property: `color` (string)
- Abstract methods: `area()`, `perimeter()`
- Concrete method: `describe()` that returns shape information

**2. Subclasses:**
- `Circle(radius, color)`
  - area = π × r²
  - perimeter = 2 × π × r
- `Rectangle(width, height, color)`
  - area = width × height
  - perimeter = 2 × (width + height)
- `Triangle(a, b, c, color)` (three sides)
  - area = √(s(s-a)(s-b)(s-c)) where s = (a+b+c)/2
  - perimeter = a + b + c

**Test with polymorphism:**
```python
shapes = [
    Circle(5, "red"),
    Rectangle(4, 6, "blue"),
    Triangle(3, 4, 5, "green")
]

for shape in shapes:
    print(shape.describe())
    print(f"Area: {shape.area():.2f}")
```

**Haskell Challenge:**
Implement the same using algebraic data types and type classes instead of inheritance.

---

## Exercise 4: Composition vs Inheritance

**Difficulty:** Medium to Hard
**Recommended Languages:** Python, C++

Model a vehicle system using **composition** (not inheritance).

**Components (separate classes):**
- `Engine(horsepower, fuelType)`
  - Methods: `start()`, `stop()`, `getHorsepower()`
- `Wheel(diameter, type)` (type: "alloy", "steel", etc.)
  - Methods: `rotate()`, `getDiameter()`
- `Door(side, isOpen)`
  - Methods: `open()`, `close()`, `lock()`, `unlock()`

**Vehicles (composed of components):**
- `Car`: has 1 engine, 4 wheels, 4 doors
- `Motorcycle`: has 1 engine, 2 wheels, 0 doors
- `Bicycle`: has 0 engines, 2 wheels, 0 doors

**Each vehicle should have:**
- `start()` - starts engine if it has one
- `getInfo()` - returns description with all components

**Questions to answer:**
1. Why is composition better than inheritance for this problem?
2. How would inheritance break down here?
3. What's easier to extend: composition or inheritance?

**Haskell Note:**
This is naturally handled with record composition in Haskell!

---

## Exercise 5: Polymorphism - Plugin System

**Difficulty:** Medium
**Recommended Languages:** Python, C++

Create a plugin system for a text editor:

**1. Define interface `Plugin`:**
- `getName()` - returns plugin name
- `getVersion()` - returns version string
- `execute(text)` - performs operation on text, returns modified text

**2. Implement at least 3 plugins:**
- `UpperCasePlugin` - converts text to uppercase
- `WordCountPlugin` - adds word count at the end
- `ReversePlugin` - reverses the text
- Your choice of 3rd plugin!

**3. Create `PluginManager`:**
- `registerPlugin(plugin)` - adds a plugin
- `listPlugins()` - shows all registered plugins
- `executePlugin(name, text)` - runs specified plugin on text
- `executeAll(text)` - runs all plugins in order

**Test:**
```python
manager = PluginManager()
manager.register_plugin(UpperCasePlugin())
manager.register_plugin(WordCountPlugin())

result = manager.execute_plugin("UpperCasePlugin", "hello world")
print(result)  # "HELLO WORLD"

result = manager.execute_all("hello world")
print(result)  # Text processed by all plugins
```

**Haskell Version:**
Use type classes to define the plugin interface.

---

## Exercise 6: Multiple Inheritance - The Duck Problem

**Difficulty:** Medium
**Recommended Languages:** Python, C++

Implement the classic multiple inheritance example with proper design:

**Interfaces/Capabilities:**
- `Flyable`: `fly()`, `land()`
- `Swimmable`: `swim()`, `dive()`
- `Walkable`: `walk()`, `run()`

**Creatures:**
- `Duck`: can fly, swim, and walk
- `Penguin`: can swim and walk (but NOT fly!)
- `Airplane`: can fly (but not swim or walk)
- `Fish`: can swim only
- `Human`: can walk and swim

**Implement a function that:**
```python
def make_it_fly(flyable_thing):
    print(flyable_thing.fly())

# Should work for Duck and Airplane
# Should NOT work for Penguin or Fish
```

**C++ Note:** Use multiple inheritance carefully. Consider interfaces.

**Haskell Note:** Use type classes for each capability.

**Question:** How do you prevent Penguin from being passed to `make_it_fly`?

---

## Exercise 7: Bank Account System (Full OOP Design)

**Difficulty:** Hard
**Recommended Languages:** Python, C++

Create a comprehensive banking system:

**1. Base class `BankAccount`:**
- Private: `accountNumber`, `balance`, `transactions` (list)
- Public methods:
  - `deposit(amount)` - validates amount > 0, records transaction
  - `withdraw(amount)` - validates sufficient funds, records transaction
  - `getBalance()` - returns current balance
  - `getTransactionHistory()` - returns list of all transactions
  - Protected: `addTransaction(description, amount)`

**2. `SavingsAccount` extends `BankAccount`:**
- Additional: `interestRate` (e.g., 0.02 for 2%)
- Method: `applyInterest()` - adds interest to balance
- Override: `withdraw()` - maximum 6 withdrawals per month
- Tracks: number of withdrawals this month

**3. `CheckingAccount` extends `BankAccount`:**
- Additional: `overdraftLimit` (e.g., 500)
- Override: `withdraw()` - allows overdraft up to limit
- Charges $35 fee for each overdraft

**4. Transaction class:**
- `type` (deposit/withdrawal/fee/interest)
- `amount`
- `date`
- `balanceAfter`

**Test scenarios:**
```python
# Savings
savings = SavingsAccount("SA001", 1000, 0.02)
savings.deposit(500)
savings.apply_interest()
print(savings.get_balance())  # 1530 (1500 + 2% interest)

# Checking with overdraft
checking = CheckingAccount("CH001", 100, 500)
checking.withdraw(400)  # Uses overdraft, charges $35 fee
print(checking.get_balance())  # -335
```

**Haskell Challenge:**
Model this using immutable data structures and type classes.

---

## Exercise 8: Static vs Instance Members

**Difficulty:** Easy to Medium
**Recommended Languages:** Python, C++

Create a `Car` class demonstrating static and instance members:

**Instance Members:**
- `make`, `model`, `year` (properties)
- `mileage` (starts at 0)
- `drive(miles)` - increases mileage
- `honk()` - returns "Beep beep!"

**Static/Class Members:**
- `totalCarsCreated` - incremented in constructor
- `totalMilesDriven` - sum of all cars' mileage
- `getFleetInfo()` - class method returning fleet statistics
- `isVintage(year)` - static method returning if year < 1990

**Test:**
```python
car1 = Car("Toyota", "Camry", 2010)
car2 = Car("Honda", "Civic", 2015)
car3 = Car("Ford", "Model T", 1920)

car1.drive(1000)
car2.drive(500)

print(Car.total_cars_created)  # 3
print(Car.total_miles_driven)  # 1500
print(Car.is_vintage(1920))    # True
print(Car.get_fleet_info())    # Summary of all cars
```

---

## Exercise 9: Operator Overloading

**Difficulty:** Medium
**Recommended Languages:** Python, C++, Haskell

Create a `Fraction` class representing rational numbers:

**Properties:**
- `numerator` (integer)
- `denominator` (integer, never 0)

**Methods:**
- Automatically reduce to lowest terms
- `toDecimal()` - returns floating point value

**Operators to overload:**
- `+` - add two fractions
- `-` - subtract two fractions
- `*` - multiply two fractions
- `/` - divide two fractions
- `==` - equality comparison
- `<`, `>` - comparison
- `str/toString` - string representation (e.g., "3/4")

**Helper:**
- `gcd(a, b)` - greatest common divisor for reduction

**Test:**
```python
f1 = Fraction(1, 2)   # 1/2
f2 = Fraction(1, 3)   # 1/3
f3 = f1 + f2          # 5/6
f4 = f1 * f2          # 1/6
f5 = Fraction(2, 4)   # Automatically reduces to 1/2

print(f1)             # "1/2"
print(f1 == f5)       # True
print(f3 > f1)        # True
```

**Haskell:**
Implement Num, Fractional, Ord, and Show type classes.

---

## Exercise 10: Properties and Validation

**Difficulty:** Medium
**Recommended Languages:** Python, C++

Create a `Rectangle` class with validated properties:

**Properties (with validation):**
- `width` - must be positive, max 1000
- `height` - must be positive, max 1000

**Computed Properties (read-only):**
- `area` - width × height
- `perimeter` - 2 × (width + height)
- `diagonal` - √(width² + height²)
- `isSquare` - true if width == height

**Methods:**
- `scale(factor)` - multiply both dimensions by factor
- `resize(newWidth, newHeight)` - change dimensions

**Python:**
Use `@property` decorators with setters.

**C++:**
Use getter/setter methods with validation.

**Test:**
```python
rect = Rectangle(10, 20)
print(rect.area)        # 200
print(rect.is_square)   # False

rect.width = 20         # Uses setter with validation
print(rect.is_square)   # True

rect.scale(2)
print(rect.width)       # 40
print(rect.area)        # 1600

# rect.width = -5       # Should raise exception
# rect.width = 2000     # Should raise exception
```

---

## Exercise 11: Abstract Factory Pattern

**Difficulty:** Hard
**Recommended Languages:** Python, C++

Create an abstract factory for creating themed UI components:

**1. Abstract Products:**
- `Button` - `render()`, `click()`
- `TextBox` - `render()`, `getText()`, `setText(text)`

**2. Concrete Products (two themes):**

**Light Theme:**
- `LightButton` - renders with light colors
- `LightTextBox` - renders with light background

**Dark Theme:**
- `DarkButton` - renders with dark colors
- `DarkTextBox` - renders with dark background

**3. Abstract Factory:**
- `UIFactory` - `createButton()`, `createTextBox()`

**4. Concrete Factories:**
- `LightThemeFactory` - creates light theme components
- `DarkThemeFactory` - creates dark theme components

**Usage:**
```python
def create_ui(factory: UIFactory):
    button = factory.create_button()
    textbox = factory.create_textbox()
    return button, textbox

# Switch themes by changing factory
light_ui = create_ui(LightThemeFactory())
dark_ui = create_ui(DarkThemeFactory())
```

**Benefit:** Can switch entire theme by changing one object!

---

## Exercise 12: Design Patterns - Decorator

**Difficulty:** Hard
**Recommended Languages:** Python, C++

Implement the Decorator pattern for a coffee shop:

**1. Base Component:**
- `Coffee` - `cost()`, `description()`

**2. Concrete Components:**
- `Espresso` - base cost $2.00
- `HouseBlend` - base cost $1.50

**3. Decorators (add-ons):**
- `Milk` - adds $0.50 to cost
- `Mocha` - adds $0.75 to cost
- `Whip` - adds $0.25 to cost

**Each decorator wraps a coffee and adds to its cost/description:**

```python
# Order: House Blend with Milk and Mocha
coffee = HouseBlend()          # $1.50
coffee = Milk(coffee)          # $2.00
coffee = Mocha(coffee)         # $2.75

print(coffee.description())    # "House Blend, Milk, Mocha"
print(coffee.cost())           # 2.75

# Order: Double Mocha Espresso
coffee2 = Espresso()           # $2.00
coffee2 = Mocha(coffee2)       # $2.75
coffee2 = Mocha(coffee2)       # $3.50

print(coffee2.description())   # "Espresso, Mocha, Mocha"
print(coffee2.cost())          # 3.50
```

**Key:** Each decorator is also a Coffee, so can be decorated further!

---

## Exercise 13: Haskell Type Classes (OOP Alternative)

**Difficulty:** Medium to Hard
**Recommended Language:** Haskell

Create a type class hierarchy for a simple RPG game:

**1. Define type classes:**
```haskell
class Entity a where
    getName :: a -> String
    getHealth :: a -> Int
    isAlive :: a -> Bool

class Entity a => Combatant a where
    attack :: a -> Int  -- Returns damage dealt
    takeDamage :: Int -> a -> a  -- Returns new entity with reduced health
```

**2. Define data types:**
- `Warrior` - high health, medium attack
- `Mage` - low health, high attack
- `Rogue` - medium health, medium attack
- `Monster` - various stats

**3. Implement type class instances for each**

**4. Create combat function:**
```haskell
combat :: (Combatant a, Combatant b) => a -> b -> IO ()
-- Simulates combat between two combatants
```

**5. Test:**
```haskell
main :: IO () = do
    let warrior = Warrior "Conan" 100 20
    let mage = Mage "Gandalf" 50 35
    let monster = Monster "Dragon" 150 25

    combat warrior monster
    combat mage monster
```

**Key Learning:** How type classes provide polymorphism without inheritance!

---

## Exercise 14: SOLID Principles Refactoring

**Difficulty:** Hard (Conceptual)
**Recommended Languages:** Python, C++

**Given this poorly designed code:**

```python
class UserManager:
    def __init__(self):
        self.users = []
        self.db_connection = DatabaseConnection("localhost")

    def create_user(self, name, email, password):
        # Validation
        if len(password) < 8:
            raise ValueError("Password too short")

        # Password hashing
        hashed = hash_password(password)

        # Create user
        user = {
            'name': name,
            'email': email,
            'password': hashed
        }

        # Save to database
        self.db_connection.execute(
            f"INSERT INTO users VALUES ('{name}', '{email}', '{hashed}')"
        )

        # Send welcome email
        email_client = EmailClient()
        email_client.send(email, "Welcome!", "Thanks for joining!")

        # Log action
        with open('logs.txt', 'a') as f:
            f.write(f"User {name} created\n")

        self.users.append(user)
        return user
```

**Your task:**
Refactor this to follow SOLID principles:

1. **Single Responsibility**: Separate concerns
2. **Open/Closed**: Make it extensible
3. **Liskov Substitution**: Proper abstractions
4. **Interface Segregation**: Specific interfaces
5. **Dependency Inversion**: Depend on abstractions

**Hint:** You should end up with multiple classes:
- `User` (data model)
- `PasswordValidator`
- `PasswordHasher`
- `UserRepository` (database operations)
- `EmailService`
- `Logger`
- `UserService` (orchestrates everything)

---

## Exercise 15: Comparing Paradigms

**Difficulty:** Hard (Conceptual)
**Recommended Languages:** Python (OOP) vs Haskell (Functional)

Implement a **shopping cart** system in both OOP (Python) and Functional (Haskell):

**Requirements:**
1. Add items to cart (name, price, quantity)
2. Remove items from cart
3. Update item quantity
4. Calculate total price
5. Apply discount codes (percentage off)
6. Calculate tax (based on total)
7. Get final price

**Python (OOP):**
- `ShoppingCart` class with mutable state
- `Item` class
- `DiscountCode` class
- Methods modify cart state

**Haskell (Functional):**
- Immutable data structures
- Pure functions that return new cart
- No mutable state

**Compare:**
1. Which is easier to test?
2. Which is easier to reason about?
3. Which is more concise?
4. How do you handle "cart history" in each?
5. Which is easier to make thread-safe?

**Write a short reflection (200-300 words) on your findings.**

---

## Bonus Challenge: Mini OOP System in C

**Difficulty:** Very Hard
**Recommended Language:** C (as comparison point)

Implement a simple OOP system in C to understand what happens "under the hood":

**Requirements:**
1. Create a "class" using structs and function pointers
2. Implement a vtable for polymorphism
3. Create a base "class" `Animal` with virtual method `speak()`
4. Create derived "classes" `Dog` and `Cat`
5. Demonstrate polymorphism

**Template:**
```c
// Base "class"
typedef struct {
    // Virtual table pointer
    void (*speak)(void*);
} Animal_vtable;

typedef struct {
    Animal_vtable* vtable;
    char* name;
} Animal;

// Derived "class"
typedef struct {
    Animal base;  // Inheritance
    char* breed;
} Dog;

void Dog_speak(void* self) {
    Dog* dog = (Dog*)self;
    printf("%s says Woof!\n", dog->base.name);
}

// Constructor
Dog* Dog_new(char* name, char* breed) {
    Dog* dog = malloc(sizeof(Dog));
    // Setup vtable
    // Initialize fields
    return dog;
}
```

This exercise shows you what C++/Python/Java do automatically!

---

## Reflection Questions

After completing these exercises, consider:

1. **Paradigm Differences:**
   - How does Haskell achieve polymorphism without inheritance?
   - What are the trade-offs of immutability vs. mutability?

2. **Language Comparison:**
   - Which language made encapsulation easiest?
   - Which language's polymorphism felt most natural?

3. **Design Patterns:**
   - Which patterns translate well to functional programming?
   - Which patterns are OOP-specific?

4. **SOLID Principles:**
   - Do SOLID principles apply to Haskell? How?
   - Which principle is most important? Why?

5. **Real-World Application:**
   - When would you choose OOP over functional?
   - Can you mix both paradigms effectively?

6. **Type Systems:**
   - How does Haskell's type system compare to C++'s?
   - What role does Python's duck typing play in OOP?

---

**Completion Goal:** Aim to complete exercises 1-10 at minimum. Advanced students should tackle exercises 11-15 and the bonus challenge.

**Time Estimate:**
- Exercises 1-5: 3-4 hours
- Exercises 6-10: 4-5 hours
- Exercises 11-15: 5-6 hours
- Bonus: 2-3 hours

Good luck, and remember: The goal is to understand **why** each language approaches OOP differently, not just **how**!
