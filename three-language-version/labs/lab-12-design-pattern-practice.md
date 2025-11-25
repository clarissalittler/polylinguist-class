# Lab 12: Design Pattern Practice

**Quarter 2, Week 1**
**Duration:** 90 minutes
**Format:** Pair programming

## Overview

Design patterns are reusable solutions to common programming problems. They're not code you copy-paste, but templates for how to structure your code. This lab introduces three fundamental patterns.

## Objectives

By the end of this lab, you will:
- [ ] Implement the Factory pattern
- [ ] Understand the Strategy pattern
- [ ] See when patterns help (and when they don't)
- [ ] Refactor code to use patterns

## Setup

- Partner up
- Create folder: `lab12-patterns/`
- Files for Python implementations

---

## Part 1: Factory Pattern (30 minutes)

### The Problem

You need to create different types of objects based on some condition:

```python
# Without pattern - messy conditional logic
def create_document(doc_type, content):
    if doc_type == "pdf":
        return PDFDocument(content)
    elif doc_type == "word":
        return WordDocument(content)
    elif doc_type == "html":
        return HTMLDocument(content)
    else:
        raise ValueError(f"Unknown type: {doc_type}")
```

Every time you add a new document type, you modify this function. Not ideal!

### The Solution: Factory Pattern

**Definition:** A factory is a method or class that creates objects for you.

### Activity 1.1: Simple Factory

```python
class Document:
    """Base class for all documents."""
    def __init__(self, content):
        self.content = content

    def render(self):
        raise NotImplementedError


class PDFDocument(Document):
    def render(self):
        return f"[PDF] {self.content}"


class WordDocument(Document):
    def render(self):
        return f"[WORD] {self.content}"


class HTMLDocument(Document):
    def render(self):
        return f"<html><body>{self.content}</body></html>"


class DocumentFactory:
    """Factory that creates documents."""

    # Registry of document types
    _creators = {
        'pdf': PDFDocument,
        'word': WordDocument,
        'html': HTMLDocument
    }

    @classmethod
    def create(cls, doc_type, content):
        """Create a document of the specified type."""
        if doc_type not in cls._creators:
            raise ValueError(f"Unknown document type: {doc_type}")
        return cls._creators[doc_type](content)

    @classmethod
    def register(cls, doc_type, creator):
        """Register a new document type."""
        cls._creators[doc_type] = creator


# Usage
doc1 = DocumentFactory.create('pdf', 'Hello World')
doc2 = DocumentFactory.create('html', 'Welcome')

print(doc1.render())  # [PDF] Hello World
print(doc2.render())  # <html><body>Welcome</body></html>
```

### Activity 1.2: Extend the Factory

**Your turn:** Add a new document type without modifying DocumentFactory:

```python
class MarkdownDocument(Document):
    def render(self):
        return f"# Markdown\n{self.content}"

# Register it
DocumentFactory.register('markdown', MarkdownDocument)

# Now it works!
doc = DocumentFactory.create('markdown', 'My content')
print(doc.render())
```

### Activity 1.3: Real-World Application

Create a game character factory:

```python
class Character:
    def __init__(self, name):
        self.name = name
        self.health = 100
        self.attack = 10
        self.defense = 10

    def describe(self):
        return f"{self.name}: HP={self.health}, ATK={self.attack}, DEF={self.defense}"


class Warrior(Character):
    def __init__(self, name):
        super().__init__(name)
        self.health = 150
        self.attack = 20
        self.defense = 15


class Mage(Character):
    def __init__(self, name):
        super().__init__(name)
        self.health = 80
        self.attack = 30
        self.defense = 5


class Rogue(Character):
    def __init__(self, name):
        super().__init__(name)
        self.health = 100
        self.attack = 25
        self.defense = 10


# TODO: Create CharacterFactory
# Should support: 'warrior', 'mage', 'rogue'
```

### ✅ Checkpoint 1

Verify:
- [ ] DocumentFactory works
- [ ] Can add new types without modifying factory
- [ ] CharacterFactory implemented

---

## Part 2: Strategy Pattern (30 minutes)

### The Problem

You have an algorithm that needs to vary based on context:

```python
# Without pattern - lots of conditionals
def calculate_price(base_price, discount_type):
    if discount_type == "none":
        return base_price
    elif discount_type == "percentage":
        return base_price * 0.9
    elif discount_type == "fixed":
        return base_price - 10
    elif discount_type == "bulk":
        return base_price * 0.8
    # ... more types = more conditionals
```

### The Solution: Strategy Pattern

**Definition:** Define a family of algorithms, encapsulate each one, and make them interchangeable.

### Activity 2.1: Implement Strategy Pattern

```python
from abc import ABC, abstractmethod

class DiscountStrategy(ABC):
    """Base class for discount strategies."""

    @abstractmethod
    def calculate(self, price):
        """Calculate discounted price."""
        pass


class NoDiscount(DiscountStrategy):
    def calculate(self, price):
        return price


class PercentageDiscount(DiscountStrategy):
    def __init__(self, percent):
        self.percent = percent

    def calculate(self, price):
        return price * (1 - self.percent / 100)


class FixedDiscount(DiscountStrategy):
    def __init__(self, amount):
        self.amount = amount

    def calculate(self, price):
        return max(0, price - self.amount)


class BulkDiscount(DiscountStrategy):
    def __init__(self, threshold, percent):
        self.threshold = threshold
        self.percent = percent

    def calculate(self, price):
        if price >= self.threshold:
            return price * (1 - self.percent / 100)
        return price


class ShoppingCart:
    """Cart that uses discount strategy."""

    def __init__(self):
        self.items = []
        self.discount_strategy = NoDiscount()

    def add_item(self, name, price):
        self.items.append({"name": name, "price": price})

    def set_discount(self, strategy):
        """Change the discount strategy."""
        self.discount_strategy = strategy

    def total(self):
        subtotal = sum(item["price"] for item in self.items)
        return self.discount_strategy.calculate(subtotal)


# Usage
cart = ShoppingCart()
cart.add_item("Book", 20)
cart.add_item("Pen", 5)

print(f"No discount: ${cart.total()}")  # $25

cart.set_discount(PercentageDiscount(10))
print(f"10% off: ${cart.total()}")  # $22.50

cart.set_discount(FixedDiscount(5))
print(f"$5 off: ${cart.total()}")  # $20

cart.set_discount(BulkDiscount(50, 20))
print(f"Bulk (need $50): ${cart.total()}")  # $25 (no bulk discount)
```

### Activity 2.2: Sorting Strategies

Create different sorting strategies:

```python
class SortStrategy(ABC):
    @abstractmethod
    def sort(self, data):
        pass


class BubbleSort(SortStrategy):
    def sort(self, data):
        result = data.copy()
        n = len(result)
        for i in range(n):
            for j in range(0, n-i-1):
                if result[j] > result[j+1]:
                    result[j], result[j+1] = result[j+1], result[j]
        return result


class QuickSort(SortStrategy):
    def sort(self, data):
        if len(data) <= 1:
            return data
        pivot = data[len(data) // 2]
        left = [x for x in data if x < pivot]
        middle = [x for x in data if x == pivot]
        right = [x for x in data if x > pivot]
        return self.sort(left) + middle + self.sort(right)


class Sorter:
    def __init__(self, strategy=None):
        self.strategy = strategy or BubbleSort()

    def set_strategy(self, strategy):
        self.strategy = strategy

    def sort(self, data):
        return self.strategy.sort(data)


# Usage
data = [64, 34, 25, 12, 22, 11, 90]
sorter = Sorter()

sorter.set_strategy(BubbleSort())
print(f"Bubble: {sorter.sort(data)}")

sorter.set_strategy(QuickSort())
print(f"Quick: {sorter.sort(data)}")
```

### Activity 2.3: Your Turn

Create strategies for text formatting:

```python
# TODO: Create strategies for:
# - UppercaseFormatter: converts to uppercase
# - LowercaseFormatter: converts to lowercase
# - TitleCaseFormatter: capitalizes each word
# - NoFormatter: returns as-is

class TextFormatter:
    def __init__(self, strategy=None):
        self.strategy = strategy

    def format(self, text):
        if self.strategy:
            return self.strategy.format(text)
        return text
```

### ✅ Checkpoint 2

Verify:
- [ ] Discount strategies work
- [ ] Can swap strategies at runtime
- [ ] Text formatters implemented

---

## Part 3: Observer Pattern (20 minutes)

### The Problem

When one object changes, others need to know:

```python
# Without pattern - tight coupling
class WeatherStation:
    def update_temperature(self, temp):
        self.temperature = temp
        # Directly update all displays - messy!
        self.phone_display.show(temp)
        self.web_display.show(temp)
        self.desktop_display.show(temp)
```

### The Solution: Observer Pattern

**Definition:** Define a one-to-many dependency so that when one object changes state, all dependents are notified.

### Activity 3.1: Implement Observer

```python
from abc import ABC, abstractmethod

class Observer(ABC):
    """Base class for observers."""

    @abstractmethod
    def update(self, data):
        """Called when subject changes."""
        pass


class Subject:
    """Base class for subjects (things being observed)."""

    def __init__(self):
        self._observers = []

    def attach(self, observer):
        """Add an observer."""
        self._observers.append(observer)

    def detach(self, observer):
        """Remove an observer."""
        self._observers.remove(observer)

    def notify(self, data):
        """Notify all observers."""
        for observer in self._observers:
            observer.update(data)


class WeatherStation(Subject):
    """Weather station that broadcasts temperature."""

    def __init__(self):
        super().__init__()
        self._temperature = 0

    @property
    def temperature(self):
        return self._temperature

    @temperature.setter
    def temperature(self, value):
        self._temperature = value
        self.notify(value)


class PhoneDisplay(Observer):
    def update(self, temp):
        print(f"📱 Phone: Temperature is {temp}°C")


class WebDisplay(Observer):
    def update(self, temp):
        print(f"🌐 Web: Current temperature: {temp}°C")


class DesktopWidget(Observer):
    def update(self, temp):
        print(f"🖥️ Desktop: {temp}°C")


# Usage
station = WeatherStation()

phone = PhoneDisplay()
web = WebDisplay()
desktop = DesktopWidget()

station.attach(phone)
station.attach(web)
station.attach(desktop)

station.temperature = 25
# All three displays update automatically!

station.detach(web)
station.temperature = 30
# Only phone and desktop update
```

### Activity 3.2: Event System

Create a simple event system:

```python
class EventEmitter(Subject):
    """Emits named events."""

    def __init__(self):
        self._listeners = {}  # event_name -> list of callbacks

    def on(self, event_name, callback):
        """Listen for an event."""
        if event_name not in self._listeners:
            self._listeners[event_name] = []
        self._listeners[event_name].append(callback)

    def off(self, event_name, callback):
        """Stop listening for an event."""
        if event_name in self._listeners:
            self._listeners[event_name].remove(callback)

    def emit(self, event_name, data=None):
        """Emit an event."""
        if event_name in self._listeners:
            for callback in self._listeners[event_name]:
                callback(data)


# Usage
emitter = EventEmitter()

def on_login(user):
    print(f"Welcome, {user}!")

def on_login_log(user):
    print(f"[LOG] User {user} logged in")

emitter.on('login', on_login)
emitter.on('login', on_login_log)

emitter.emit('login', 'Alice')
# Welcome, Alice!
# [LOG] User Alice logged in
```

### ✅ Checkpoint 3

Verify:
- [ ] WeatherStation notifies all observers
- [ ] Can add/remove observers
- [ ] Understand observer use cases

---

## Part 4: Pattern Recognition (10 minutes)

### Activity 4.1: Identify the Pattern

Which pattern would you use?

| Scenario | Pattern |
|----------|---------|
| Creating enemies in a game based on difficulty level | ? |
| Different ways to compress files (zip, gzip, bz2) | ? |
| Notifying UI components when data changes | ? |
| Different payment methods (credit, PayPal, crypto) | ? |
| Creating database connections for different DBs | ? |

### Activity 4.2: When NOT to Use Patterns

Patterns can be overused! Don't use them when:

- **Simple solution works** - Don't add Factory for 2 types
- **YAGNI** - You Ain't Gonna Need It
- **Adds complexity** - If teammates can't understand it
- **Premature** - Wait until the need is clear

---

## Challenges

### Challenge 1: Combine Patterns

Create a game that uses all three patterns:
- Factory for creating characters
- Strategy for combat styles
- Observer for game events

### Challenge 2: Implement in Haskell

How would you implement Strategy pattern in a functional language?

```haskell
-- Hint: Functions are first-class!
type DiscountStrategy = Double -> Double

noDiscount :: DiscountStrategy
noDiscount price = price

percentOff :: Double -> DiscountStrategy
percentOff percent price = price * (1 - percent / 100)

applyDiscount :: DiscountStrategy -> Double -> Double
applyDiscount strategy price = strategy price

-- Usage
-- applyDiscount (percentOff 10) 100  -- 90.0
```

---

## Wrap-Up

**Key takeaways:**

1. **Factory**: Creates objects without specifying exact class
2. **Strategy**: Swappable algorithms at runtime
3. **Observer**: Notify dependents of changes

**When to use patterns:**
- When you see the problem they solve
- When code becomes hard to extend
- When the pattern simplifies, not complicates

**Next lab:** Pattern Matching - a different kind of pattern!
