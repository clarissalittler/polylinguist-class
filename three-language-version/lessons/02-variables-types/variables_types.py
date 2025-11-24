# variables_types.py

print("=== Python Variables and Types Demo ===\n")

# Numbers
print("--- Numbers ---")
age = 25                    # int
price = 19.99              # float
complex_num = 3 + 4j       # complex

print(f"Age: {age} (type: {type(age).__name__})")
print(f"Price: {price} (type: {type(price).__name__})")
print(f"Complex: {complex_num} (type: {type(complex_num).__name__})")

# Strings
print("\n--- Strings ---")
name = "Alice"             # str
greeting = 'Hello'         # single or double quotes
multiline = """This is a
multiline string"""

print(f"Name: {name} (type: {type(name).__name__})")
print(f"Greeting: {greeting}")

# Booleans
print("\n--- Booleans ---")
is_student = True          # bool
has_graduated = False

print(f"Is student: {is_student} (type: {type(is_student).__name__})")
print(f"Has graduated: {has_graduated}")

# None (null/nil)
print("\n--- None ---")
nothing = None
print(f"Nothing: {nothing} (type: {type(nothing).__name__})")

# Python is dynamically typed - variables can change type
print("\n--- Dynamic Typing ---")
x = 42          # x is an int
print(f"x = {x} (type: {type(x).__name__})")
x = "now text"  # x is now a str (allowed!)
print(f"x = {x} (type: {type(x).__name__})")

# Type conversion
print("\n--- Type Conversion ---")
num_str = "123"
num = int(num_str)      # Convert string to int
print(f"String '{num_str}' converted to int: {num}")
print(f"Converted number + 1 = {num + 1}")

float_str = "3.14"
float_num = float(float_str)
print(f"String '{float_str}' converted to float: {float_num}")

# Converting to string
str_from_int = str(456)
print(f"Int 456 converted to string: '{str_from_int}'")

# Lists and other collections
print("\n--- Collections ---")
numbers = [1, 2, 3, 4, 5]
print(f"List: {numbers} (type: {type(numbers).__name__})")

person = ("Alice", 25, "Engineer")  # tuple
print(f"Tuple: {person} (type: {type(person).__name__})")

# Dictionary
student = {"name": "Bob", "age": 20, "grade": "A"}
print(f"Dictionary: {student} (type: {type(student).__name__})")

print("\n--- Type Checking ---")
print(f"isinstance(age, int): {isinstance(age, int)}")
print(f"isinstance(name, str): {isinstance(name, str)}")
print(f"isinstance(price, float): {isinstance(price, float)}")
