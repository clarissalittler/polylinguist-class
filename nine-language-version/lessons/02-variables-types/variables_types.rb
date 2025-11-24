# variables_types.rb

# Numbers
age = 25                    # Integer
price = 19.99              # Float
big_num = 12345678901234567890  # Bignum (automatic for large integers)
rational = 2/3r            # Rational number
complex = 3 + 4i           # Complex number

# Strings
name = "Alice"             # String (double quotes)
greeting = 'Hello'         # String (single quotes)
interpolated = "Hello, #{name}!"  # String interpolation (only with double quotes)

# Symbols (immutable, interned strings)
status = :active           # Symbol
role = :admin

# Booleans
is_student = true          # TrueClass
has_graduated = false      # FalseClass

# Nil (null equivalent)
nothing = nil              # NilClass

# Ruby is dynamically typed - variables can change type
x = 42                     # x is an Integer
x = "now text"             # x is now a String (allowed!)

# Type checking
puts "Type of age: #{age.class}"
puts "Type of name: #{name.class}"
puts "Type of status: #{status.class}"
puts "Type of x: #{x.class}"

# Type conversion
num_str = "123"
num = num_str.to_i         # Convert string to integer
puts "Converted number + 1 = #{num + 1}"

float_str = "3.14"
float_val = float_str.to_f  # Convert to float
puts "Float value: #{float_val}"

# Check type
puts "Is age an Integer? #{age.is_a?(Integer)}"
puts "Is name a String? #{name.is_a?(String)}"

# Everything is an object in Ruby
puts "42 is an object: #{42.class}"
puts "Even nil is an object: #{nil.class}"
