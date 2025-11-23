#!/usr/bin/env python3
"""
Lesson 5: Data Structures in Python
Demonstrates mutable lists, dicts, sets, and tuples
"""

def main():
    print("=== Python Data Structures ===\n")

    # 1. Lists (mutable, dynamic)
    print("1. Lists (Mutable):")
    numbers = [1, 2, 3, 4, 5]
    print(f"  Original: {numbers}")

    numbers.append(6)
    print(f"  After append(6): {numbers}")

    numbers[0] = 10
    print(f"  After numbers[0] = 10: {numbers}")

    # Aliasing demonstration
    list1 = [1, 2, 3]
    list2 = list1  # Both refer to same list!
    list2.append(4)
    print(f"\n  Aliasing demo:")
    print(f"    list1: {list1}")  # Also changed!
    print(f"    list2: {list2}")

    # Copying
    list3 = list1.copy()  # or list1[:]
    list3.append(5)
    print(f"  After copying:")
    print(f"    list1: {list1}")  # Unchanged
    print(f"    list3: {list3}")  # Changed

    # 2. List comprehensions
    print("\n2. List Comprehensions:")
    squared = [x * x for x in range(1, 6)]
    print(f"  Squared: {squared}")

    evens = [x for x in range(10) if x % 2 == 0]
    print(f"  Evens: {evens}")

    # 3. Dictionaries (mutable)
    print("\n3. Dictionaries (Mutable):")
    person = {"name": "Alice", "age": 30, "city": "NYC"}
    print(f"  Person: {person}")

    person["age"] = 31
    person["email"] = "alice@example.com"
    print(f"  After modifications: {person}")

    # Dict methods
    print(f"  Keys: {list(person.keys())}")
    print(f"  Values: {list(person.values())}")
    print(f"  Get with default: {person.get('country', 'USA')}")

    # 4. Sets (mutable, unique elements)
    print("\n4. Sets (Mutable, Unique):")
    numbers_set = {1, 2, 3, 4, 5}
    print(f"  Set: {numbers_set}")

    numbers_set.add(6)
    numbers_set.add(3)  # Duplicate, ignored
    print(f"  After adding 6 and 3: {numbers_set}")

    # Set operations
    evens = {2, 4, 6, 8}
    odds = {1, 3, 5, 7}
    print(f"  Union: {evens | odds}")
    print(f"  Intersection: {evens & odds}")
    print(f"  Difference: {evens - odds}")

    # 5. Tuples (immutable)
    print("\n5. Tuples (Immutable):")
    point = (3, 4)
    print(f"  Point: {point}")
    print(f"  x={point[0]}, y={point[1]}")

    # Unpacking
    x, y = point
    print(f"  Unpacked: x={x}, y={y}")

    # Tuples are immutable
    try:
        point[0] = 5
    except TypeError as e:
        print(f"  Cannot modify tuple: {e}")

    # 6. Performance comparison
    print("\n6. Performance Notes:")
    print("  - Lists: O(1) append, O(n) insert at beginning")
    print("  - Dicts: O(1) average lookup/insert")
    print("  - Sets: O(1) average membership test")
    print("  - Tuples: Faster than lists (immutable)")

    # 7. Nested structures
    print("\n7. Nested Structures:")
    matrix = [
        [1, 2, 3],
        [4, 5, 6],
        [7, 8, 9]
    ]
    print(f"  Matrix: {matrix}")
    print(f"  matrix[1][2] = {matrix[1][2]}")  # 6

    # 8. Immutability benefits
    print("\n8. Immutability (using tuples):")
    config = ("localhost", 8080, True)  # Immutable config
    print(f"  Config: {config}")
    print("  - Safe to share across threads")
    print("  - Can be dict keys (lists can't!)")

    tuple_as_key = {(0, 0): "origin", (1, 0): "right", (0, 1): "up"}
    print(f"  Tuples as keys: {tuple_as_key}")

if __name__ == "__main__":
    main()
