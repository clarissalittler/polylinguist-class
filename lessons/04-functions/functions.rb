#!/usr/bin/env ruby
# Lesson 4: Functions in Ruby
# Demonstrates methods, blocks, lambdas, and procs

# ============================================
# 1. Basic Method Definition
# ============================================

def greet(name)
  "Hello, #{name}!"  # Implicit return
end

def add(x, y)
  x + y
end

def square(x)
  x * x
end

# ============================================
# 2. Parameters
# ============================================

# Default parameters
def describe_person(name, age, city = "Unknown")
  "#{name} is #{age} years old and lives in #{city}"
end

# Splat operator (variadic)
def sum_all(*numbers)
  numbers.sum
end

# Keyword arguments (Ruby 2.0+)
def describe_person_kwargs(name:, age:, city: "Unknown")
  "#{name} is #{age} years old and lives in #{city}"
end

# ============================================
# 3. Return Values
# ============================================

def divide(x, y)
  if y == 0
    return [nil, "Cannot divide by zero"]
  end
  [x.to_f / y, nil]
end

# ============================================
# 4. Scope and Closures
# ============================================

$global_var = "I'm global"

def scope_demo
  local_var = "I'm local"
  puts "Inside function: #{local_var}"
  puts "Can access global: #{$global_var}"
end

# ============================================
# 5. Closures
# ============================================

def make_multiplier(factor)
  lambda { |x| x * factor }  # Lambda captures 'factor'
end

# Alternative with stabby lambda
def make_adder(n)
  ->(x) { x + n }
end

def make_counter
  count = 0

  lambda do
    count += 1
    count
  end
end

# ============================================
# 6. Pure vs Impure Functions
# ============================================

# Pure function
def pure_add(x, y)
  x + y
end

# Impure function (side effect)
def impure_print(message)
  puts message
end

# Impure function (depends on/modifies global state)
$total = 0
def impure_add_to_total(x)
  $total += x
  $total
end

# Impure function (non-deterministic)
def impure_random
  rand(1..100)
end

# ============================================
# 7. First-Class Functions
# ============================================

def apply_operation(operation, x, y)
  operation.call(x, y)
end

def compose(f, g)
  ->(x) { f.call(g.call(x)) }
end

# ============================================
# 8. Blocks, Procs, and Lambdas
# ============================================

# Method that yields to a block
def repeat(n)
  n.times do |i|
    yield i
  end
end

# Method that takes a block as parameter
def apply_twice_block(&block)
  ->(x) { block.call(block.call(x)) }
end

# ============================================
# 9. Higher-Order Functions
# ============================================

def apply_twice(f, x)
  f.call(f.call(x))
end

def apply_n_times(f, x, n)
  result = x
  n.times do
    result = f.call(result)
  end
  result
end

# ============================================
# Main Program
# ============================================

def main
  puts "=== Ruby Functions ==="
  puts

  # 1. Basic methods
  puts "1. Basic Methods:"
  puts "  greet('Alice'): #{greet('Alice')}"
  puts "  add(5, 3): #{add(5, 3)}"
  puts "  square(7): #{square(7)}"

  # 2. Parameters
  puts "\n2. Parameters and Arguments:"
  puts "  describe_person('Alice', 30): #{describe_person('Alice', 30)}"
  puts "  describe_person('Bob', 25, 'NYC'): #{describe_person('Bob', 25, 'NYC')}"
  puts "  sum_all(1, 2, 3, 4, 5): #{sum_all(1, 2, 3, 4, 5)}"

  puts "\n  Keyword arguments:"
  puts "  describe_person_kwargs(name: 'Charlie', age: 35): #{describe_person_kwargs(name: 'Charlie', age: 35)}"

  # 3. Multiple returns
  puts "\n3. Multiple Return Values:"
  result, error = divide(10, 2)
  puts "  divide(10, 2): result=#{result}, error=#{error}"
  result, error = divide(10, 0)
  puts "  divide(10, 0): result=#{result}, error=#{error}"

  # 4. Lambdas and closures
  puts "\n4. Lambdas and Closures:"
  double = lambda { |x| x * 2 }
  square_lambda = ->(x) { x * x }

  puts "  double.call(5): #{double.call(5)}"
  puts "  square_lambda.call(7): #{square_lambda.call(7)}"

  times_two = make_multiplier(2)
  times_three = make_multiplier(3)
  puts "  times_two.call(5): #{times_two.call(5)}"
  puts "  times_three.call(5): #{times_three.call(5)}"

  add_five = make_adder(5)
  puts "  add_five.call(10): #{add_five.call(10)}"

  # 5. Counter (stateful closure)
  puts "\n  Counter (stateful closure):"
  counter1 = make_counter
  counter2 = make_counter
  puts "  counter1.call: #{counter1.call}"  # 1
  puts "  counter1.call: #{counter1.call}"  # 2
  puts "  counter2.call: #{counter2.call}"  # 1
  puts "  counter1.call: #{counter1.call}"  # 3

  # 6. Pure vs Impure
  puts "\n5. Pure vs Impure:"
  puts "  pure_add(5, 3): #{pure_add(5, 3)}"
  print "  impure_print('Hello'): "
  impure_print("Hello")
  $total = 0  # Reset
  puts "  impure_add_to_total(5): #{impure_add_to_total(5)}"
  puts "  impure_add_to_total(5): #{impure_add_to_total(5)} (different!)"

  # 7. First-class functions
  puts "\n6. First-Class Functions:"
  add_lambda = ->(x, y) { x + y }
  multiply_lambda = ->(x, y) { x * y }

  puts "  apply_operation(add, 5, 3): #{apply_operation(add_lambda, 5, 3)}"
  puts "  apply_operation(multiply, 5, 3): #{apply_operation(multiply_lambda, 5, 3)}"

  # 8. Function composition
  puts "\n7. Function Composition:"
  increment = ->(x) { x + 1 }
  square_then_increment = compose(increment, square_lambda)
  puts "  square_then_increment.call(5): #{square_then_increment.call(5)}"

  # 9. Blocks
  puts "\n8. Blocks:"
  puts "  repeat(3) with block:"
  repeat(3) { |i| puts "    Iteration #{i}" }

  # 10. Higher-order array methods
  puts "\n9. Higher-Order Array Methods:"
  numbers = [1, 2, 3, 4, 5]
  puts "  numbers: #{numbers}"

  squared = numbers.map { |x| x * x }
  puts "  map(square): #{squared}"

  evens = numbers.filter { |x| x.even? }
  puts "  filter(even): #{evens}"

  sum = numbers.reduce(0) { |acc, x| acc + x }
  puts "  reduce(add): #{sum}"

  # Symbol to proc (Ruby magic)
  doubled = numbers.map(&:to_s)
  puts "  map(&:to_s): #{doubled}"

  # 11. Apply n times
  puts "\n10. Apply Function Multiple Times:"
  puts "  apply_twice(double, 5): #{apply_twice(double, 5)}"
  puts "  apply_n_times(double, 5, 3): #{apply_n_times(double, 5, 3)}"
end

main
