# variables_types.py

# Numbers
age = 25                    # int
price = 19.99              # float
complex_num = 3 + 4j       # complex

# Strings
name = "Alice"             # str
greeting = 'Hello'         # single or double quotes

# Booleans
is_student = True          # bool
has_graduated = False

# None (null/nil)
nothing = None

# Python is dynamically typed - variables can change type
x = 42          # x is an int
x = "now text"  # x is now a str (allowed!)

# Type checking
print(f"Type of age: {type(age)}")
print(f"Type of name: {type(name)}")
print(f"Type of x: {type(x)}")

# Type conversion
num_str = "123"
num = int(num_str)      # Convert string to int
print(f"Converted number + 1 = {num + 1}")
