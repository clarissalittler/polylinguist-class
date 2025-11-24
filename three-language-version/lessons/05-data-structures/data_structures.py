#!/usr/bin/env python3
"""
Lesson 5: Data Structures in Python
Demonstrates mutable lists, dicts, sets, and tuples
"""

def demonstrate_lists():
    """Demonstrate Python lists - dynamic, mutable arrays"""
    print("=== 1. Lists (Mutable, Dynamic) ===\n")

    # Creating lists
    numbers = [1, 2, 3, 4, 5]
    fruits = ["apple", "banana", "cherry"]
    mixed = [1, "hello", 3.14, True]

    print(f"Numbers: {numbers}")
    print(f"Fruits: {fruits}")
    print(f"Mixed types: {mixed}")

    # Accessing elements
    print(f"\nFirst element: {numbers[0]}")
    print(f"Last element: {numbers[-1]}")
    print(f"Slice [1:4]: {numbers[1:4]}")

    # Modifying lists (MUTABLE)
    numbers.append(6)
    print(f"\nAfter append(6): {numbers}")

    numbers.insert(0, 0)
    print(f"After insert(0, 0): {numbers}")

    numbers[1] = 10
    print(f"After numbers[1] = 10: {numbers}")

    popped = numbers.pop()
    print(f"Popped: {popped}, List: {numbers}")

    # Aliasing demonstration
    print("\n--- Aliasing Issue ---")
    list1 = [1, 2, 3]
    list2 = list1  # Both refer to same list!
    list2.append(4)
    print(f"list1: {list1}")  # Also changed!
    print(f"list2: {list2}")

    # Copying
    print("\n--- Proper Copying ---")
    list3 = list1.copy()  # or list1[:]
    list3.append(5)
    print(f"list1: {list1}")  # Unchanged
    print(f"list3: {list3}")  # Changed


def demonstrate_list_comprehensions():
    """Demonstrate list comprehensions - concise list creation"""
    print("\n=== 2. List Comprehensions ===\n")

    # Basic comprehension
    squared = [x * x for x in range(1, 6)]
    print(f"Squared: {squared}")

    # With condition
    evens = [x for x in range(10) if x % 2 == 0]
    print(f"Evens 0-9: {evens}")

    # Nested comprehension
    matrix = [[i * j for j in range(1, 4)] for i in range(1, 4)]
    print(f"Multiplication table:\n{matrix}")

    # FizzBuzz with comprehension
    def fizzbuzz(n):
        if n % 15 == 0:
            return "FizzBuzz"
        elif n % 3 == 0:
            return "Fizz"
        elif n % 5 == 0:
            return "Buzz"
        return str(n)

    fb = [fizzbuzz(i) for i in range(1, 21)]
    print(f"\nFizzBuzz 1-20: {fb}")


def demonstrate_dictionaries():
    """Demonstrate dictionaries - mutable key-value storage"""
    print("\n=== 3. Dictionaries (Mutable) ===\n")

    # Creating dictionaries
    person = {"name": "Alice", "age": 30, "city": "NYC"}
    print(f"Person: {person}")

    # Accessing elements
    print(f"\nName: {person['name']}")
    print(f"Age: {person.get('age')}")
    print(f"Country (default): {person.get('country', 'USA')}")

    # Modifying dictionaries
    person["age"] = 31
    person["email"] = "alice@example.com"
    print(f"\nAfter modifications: {person}")

    del person["city"]
    print(f"After deleting 'city': {person}")

    # Dictionary methods
    print(f"\nKeys: {list(person.keys())}")
    print(f"Values: {list(person.values())}")
    print(f"Items: {list(person.items())}")

    # Checking membership
    if "name" in person:
        print("\n'name' key exists")

    # Dictionary comprehension
    squared_dict = {x: x * x for x in range(5)}
    print(f"\nSquared dict: {squared_dict}")

    # Iteration
    print("\nIterating over person:")
    for key, value in person.items():
        print(f"  {key}: {value}")


def demonstrate_sets():
    """Demonstrate sets - mutable, unique elements"""
    print("\n=== 4. Sets (Mutable, Unique) ===\n")

    # Creating sets
    numbers = {1, 2, 3, 4, 5}
    print(f"Numbers: {numbers}")

    # Adding elements
    numbers.add(6)
    numbers.add(3)  # Duplicate, ignored
    print(f"After adding 6 and 3: {numbers}")

    # Removing elements
    numbers.remove(1)  # Error if not present
    numbers.discard(10)  # No error if not present
    print(f"After remove(1) and discard(10): {numbers}")

    # Set operations
    evens = {2, 4, 6, 8}
    odds = {1, 3, 5, 7}

    print(f"\nEvens: {evens}")
    print(f"Odds: {odds}")
    print(f"Union (|): {evens | odds}")
    print(f"Intersection (&): {evens & odds}")
    print(f"Difference (-): {evens - odds}")
    print(f"Symmetric diff (^): {evens ^ odds}")

    # Membership testing
    if 3 in odds:
        print("\n3 is in odds")

    # Remove duplicates from list
    duplicates = [1, 2, 2, 3, 3, 3, 4, 4, 4, 4]
    unique = list(set(duplicates))
    print(f"\nRemove duplicates: {duplicates} -> {unique}")


def demonstrate_tuples():
    """Demonstrate tuples - immutable sequences"""
    print("\n=== 5. Tuples (Immutable) ===\n")

    # Creating tuples
    point = (3, 4)
    person = ("Alice", 30, "NYC")

    print(f"Point: {point}")
    print(f"Person: {person}")

    # Accessing elements
    print(f"\nPoint x: {point[0]}, y: {point[1]}")

    # Unpacking
    x, y = point
    name, age, city = person
    print(f"Unpacked: x={x}, y={y}")
    print(f"Unpacked: {name}, {age}, {city}")

    # Tuples are immutable
    try:
        point[0] = 5
    except TypeError as e:
        print(f"\nCannot modify tuple: {e}")

    # Named tuples
    from collections import namedtuple
    Person = namedtuple('Person', ['name', 'age', 'city'])
    alice = Person("Alice", 30, "NYC")
    print(f"\nNamed tuple: {alice}")
    print(f"Name: {alice.name}, Age: {alice.age}")

    # Tuples as dictionary keys
    locations = {
        (0, 0): "origin",
        (1, 0): "east",
        (0, 1): "north",
        (-1, 0): "west",
        (0, -1): "south"
    }
    print(f"\nLocations: {locations}")
    print(f"Location at (1, 0): {locations[(1, 0)]}")


def demonstrate_nested_structures():
    """Demonstrate nested data structures"""
    print("\n=== 6. Nested Structures ===\n")

    # Matrix (nested lists)
    matrix = [
        [1, 2, 3],
        [4, 5, 6],
        [7, 8, 9]
    ]
    print("Matrix:")
    for row in matrix:
        print(f"  {row}")

    print(f"\nElement [1][2]: {matrix[1][2]}")  # 6

    # Transpose matrix
    transposed = [[matrix[j][i] for j in range(3)] for i in range(3)]
    print("\nTransposed:")
    for row in transposed:
        print(f"  {row}")

    # Complex nested structure
    classroom = {
        "teacher": "Ms. Smith",
        "room": 101,
        "students": [
            {"name": "Alice", "grades": [90, 85, 88]},
            {"name": "Bob", "grades": [75, 80, 78]},
            {"name": "Charlie", "grades": [95, 92, 94]}
        ]
    }

    print("\nClassroom:")
    print(f"  Teacher: {classroom['teacher']}")
    print(f"  Room: {classroom['room']}")
    print("  Students:")
    for student in classroom["students"]:
        avg = sum(student["grades"]) / len(student["grades"])
        print(f"    {student['name']}: avg = {avg:.1f}")


def demonstrate_performance():
    """Demonstrate performance characteristics"""
    print("\n=== 7. Performance Notes ===\n")

    import time

    # List append (amortized O(1))
    start = time.time()
    lst = []
    for i in range(100000):
        lst.append(i)
    elapsed = time.time() - start
    print(f"Append 100k elements: {elapsed:.4f}s")

    # List insert at beginning (O(n))
    start = time.time()
    lst = []
    for i in range(1000):  # Fewer iterations!
        lst.insert(0, i)
    elapsed = time.time() - start
    print(f"Insert 1k at beginning: {elapsed:.4f}s (slow!)")

    # Dictionary lookup (O(1))
    big_dict = {i: i * i for i in range(100000)}
    start = time.time()
    for i in range(10000):
        _ = big_dict[50000]
    elapsed = time.time() - start
    print(f"10k dict lookups: {elapsed:.4f}s (fast!)")

    # Set membership (O(1))
    big_set = set(range(100000))
    start = time.time()
    for i in range(10000):
        _ = 50000 in big_set
    elapsed = time.time() - start
    print(f"10k set membership tests: {elapsed:.4f}s (fast!)")


def word_frequency_counter(text):
    """Count word frequency in text"""
    print("\n=== 8. Word Frequency Counter ===\n")

    # Split and clean
    words = text.lower().replace(',', '').replace('.', '').split()

    # Count frequencies
    freq = {}
    for word in words:
        freq[word] = freq.get(word, 0) + 1

    # Sort by frequency (descending)
    sorted_freq = sorted(freq.items(), key=lambda x: x[1], reverse=True)

    print(f"Text: {text}\n")
    print("Word frequencies:")
    for word, count in sorted_freq:
        print(f"  {word}: {count}")

    return freq


def shopping_cart_demo():
    """Demonstrate a shopping cart with mutable operations"""
    print("\n=== 9. Shopping Cart (Mutable) ===\n")

    cart = []

    def add_item(cart, name, price, quantity):
        """Add or update item in cart"""
        for item in cart:
            if item["name"] == name:
                item["quantity"] += quantity
                return
        cart.append({"name": name, "price": price, "quantity": quantity})

    def calculate_total(cart):
        """Calculate total cost"""
        return sum(item["price"] * item["quantity"] for item in cart)

    def print_cart(cart):
        """Print cart contents"""
        print("Cart contents:")
        for item in cart:
            subtotal = item["price"] * item["quantity"]
            print(f"  {item['name']}: ${item['price']:.2f} x {item['quantity']} = ${subtotal:.2f}")
        print(f"Total: ${calculate_total(cart):.2f}")

    # Use the cart
    add_item(cart, "Apple", 0.50, 5)
    add_item(cart, "Banana", 0.30, 3)
    add_item(cart, "Orange", 0.75, 4)
    print_cart(cart)

    print("\nAdding more apples...")
    add_item(cart, "Apple", 0.50, 3)
    print_cart(cart)


def main():
    """Run all demonstrations"""
    print("=" * 60)
    print("Python Data Structures Demonstration")
    print("=" * 60)

    demonstrate_lists()
    demonstrate_list_comprehensions()
    demonstrate_dictionaries()
    demonstrate_sets()
    demonstrate_tuples()
    demonstrate_nested_structures()
    demonstrate_performance()

    word_frequency_counter("the quick brown fox jumps over the lazy dog the fox")

    shopping_cart_demo()

    print("\n" + "=" * 60)
    print("Key Takeaways:")
    print("- Lists are mutable, dynamic arrays")
    print("- Dictionaries provide O(1) key-value lookup")
    print("- Sets ensure uniqueness and fast membership testing")
    print("- Tuples are immutable and can be dict keys")
    print("- Watch out for aliasing with mutable structures!")
    print("=" * 60)


if __name__ == "__main__":
    main()
