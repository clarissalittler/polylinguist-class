#!/usr/bin/env ruby

# Lesson 9: Pattern Matching in Ruby
#
# Ruby has had case/when for a long time, and Ruby 2.7+ introduced
# experimental pattern matching with 'in' syntax (finalized in Ruby 3.0).
# - case/when for basic patterns
# - Pattern matching with case/in (Ruby 3.0+)
# - Array/Hash destructuring
# - Guard clauses
#
# This demonstrates Ruby's pattern matching capabilities.

# ====================
# 1. Basic Case/When
# ====================

def describe_number(n)
  case n
  when 0 then "zero"
  when 1 then "one"
  when 2 then "two"
  else "many: #{n}"
  end
end

# ====================
# 2. Array Patterns (Ruby 3.0+)
# ====================

def describe_point(point)
  case point
  in [0, 0]
    "origin"
  in [0, y]
    "on y-axis at y=#{y}"
  in [x, 0]
    "on x-axis at x=#{x}"
  in [x, y]
    "point at (#{x}, #{y})"
  end
end

def describe_list(lst)
  case lst
  in []
    "empty list"
  in [x]
    "single element: #{x}"
  in [x, y]
    "two elements: #{x}, #{y}"
  in [first, *rest]
    "first: #{first}, rest: #{rest}"
  end
end

# ====================
# 3. Hash Patterns (Ruby 3.0+)
# ====================

def process_command(cmd)
  case cmd
  in { action: "quit" }
    "Quitting..."
  in { action: "move", direction: dir }
    "Moving #{dir}"
  in { action: "attack", target: target, damage: dmg }
    "Attacking #{target} for #{dmg} damage"
  in { action: action }
    "Unknown action: #{action}"
  else
    "Invalid command"
  end
end

# ====================
# 4. Class/Object Patterns
# ====================

class Shape
end

class Circle < Shape
  attr_reader :radius

  def initialize(radius)
    @radius = radius
  end
end

class Rectangle < Shape
  attr_reader :width, :height

  def initialize(width, height)
    @width = width
    @height = height
  end
end

class Triangle < Shape
  attr_reader :a, :b, :c

  def initialize(a, b, c)
    @a, @b, @c = a, b, c
  end
end

def area(shape)
  case shape
  when Circle
    Math::PI * shape.radius ** 2
  when Rectangle
    shape.width * shape.height
  when Triangle
    s = (shape.a + shape.b + shape.c) / 2.0
    Math.sqrt(s * (s - shape.a) * (s - shape.b) * (s - shape.c))
  else
    raise "Unknown shape"
  end
end

# ====================
# 5. Guards (with if)
# ====================

def classify(n)
  case
  when n < 0
    "negative"
  when n == 0
    "zero"
  when n < 10
    "small positive"
  when n < 100
    "medium positive"
  else
    "large positive"
  end
end

# Modern pattern matching with guards (Ruby 3.0+)
def classify_modern(n)
  case n
  in x if x < 0
    "negative"
  in 0
    "zero"
  in x if x < 10
    "small positive"
  in x if x < 100
    "medium positive"
  else
    "large positive"
  end
end

# ====================
# 6. Multiple Values (OR patterns)
# ====================

def is_weekend(day)
  case day.downcase
  when "saturday", "sunday"
    true
  else
    false
  end
end

# ====================
# 7. Range Patterns
# ====================

def grade_letter(score)
  case score
  when 90..100 then 'A'
  when 80...90 then 'B'
  when 70...80 then 'C'
  when 60...70 then 'D'
  else 'F'
  end
end

# ====================
# 8. Expression Evaluator
# ====================

class Expr
end

class Num < Expr
  attr_reader :value

  def initialize(value)
    @value = value
  end

  def eval
    @value
  end
end

class Add < Expr
  attr_reader :left, :right

  def initialize(left, right)
    @left, @right = left, right
  end

  def eval
    @left.eval + @right.eval
  end
end

class Mul < Expr
  attr_reader :left, :right

  def initialize(left, right)
    @left, @right = left, right
  end

  def eval
    @left.eval * @right.eval
  end
end

class Neg < Expr
  attr_reader :expr

  def initialize(expr)
    @expr = expr
  end

  def eval
    -@expr.eval
  end
end

# Alternative using pattern matching
def eval_expr(expr)
  case expr
  when Num
    expr.value
  when Add
    eval_expr(expr.left) + eval_expr(expr.right)
  when Mul
    eval_expr(expr.left) * eval_expr(expr.right)
  when Neg
    -eval_expr(expr.expr)
  end
end

# ====================
# 9. Nested Patterns (Ruby 3.0+)
# ====================

def analyze_nested(data)
  case data
  in { user: { name:, age: }, active: true }
    "Active user: #{name} (#{age})"
  in { user: { name: }, active: false }
    "Inactive user: #{name}"
  in { error: message }
    "Error: #{message}"
  else
    "Unknown data"
  end
rescue NoMatchingPatternError
  "Unknown data"
end

# ====================
# 10. Variable Binding (Pin operator ^)
# ====================

def match_with_variable(value, expected)
  case value
  in ^expected
    "Matched expected value: #{expected}"
  else
    "Did not match, got: #{value}"
  end
rescue NoMatchingPatternError
  "No match"
end

# ====================
# 11. Alternative Patterns (|)
# ====================

def describe_value(val)
  case val
  in Integer
    "integer: #{val}"
  in Float
    "float: #{val.round(2)}"
  in String
    "string: '#{val}'"
  in Array
    "array with #{val.length} items"
  else
    "unknown type"
  end
end

# ====================
# 12. Find Pattern (Ruby 3.0+)
# ====================

def find_adults(people)
  case people
  in [*, { name:, age: } => adult, *] if age >= 18
    "Found adult: #{name}"
  else
    "No adults found"
  end
rescue NoMatchingPatternError
  "No adults found"
end

# ====================
# 13. State Machine
# ====================

def traffic_light_next(current, action)
  if action == "emergency"
    return "red"
  end

  case [current, action]
  when ["red", "timer"]
    "green"
  when ["green", "timer"]
    "yellow"
  when ["yellow", "timer"]
    "red"
  else
    current  # No change
  end
end

# ====================
# 14. Rightward Assignment (Ruby 3.0+)
# ====================

def demonstrate_rightward
  { name: "Alice", age: 30 } => { name:, age: }
  "Name: #{name}, Age: #{age}"
rescue NoMatchingPatternError
  "No match"
end

# ====================
# Main Demonstration
# ====================

def main
  puts "=== Pattern Matching in Ruby ===\n\n"

  # 1. Basic case/when
  puts "1. Basic Case/When:"
  [0, 1, 2, 5].each do |n|
    puts "   #{n} -> #{describe_number(n)}"
  end

  # 2. Array patterns
  if RUBY_VERSION >= "3.0"
    puts "\n2. Array Patterns (Ruby 3.0+):"
    [[0, 0], [0, 5], [3, 0], [2, 3]].each do |p|
      puts "   #{p} -> #{describe_point(p)}"
    end

    # 3. List patterns
    puts "\n3. List Patterns:"
    [[], [1], [1, 2], [1, 2, 3, 4]].each do |lst|
      puts "   #{lst} -> #{describe_list(lst)}"
    end

    # 4. Hash patterns
    puts "\n4. Hash Patterns:"
    [
      { action: "quit" },
      { action: "move", direction: "north" },
      { action: "attack", target: "orc", damage: 10 }
    ].each do |cmd|
      puts "   #{cmd}"
      puts "   -> #{process_command(cmd)}"
    end
  else
    puts "\n2-4. Advanced pattern matching requires Ruby 3.0+"
  end

  # 5. Class patterns
  puts "\n5. Class/Object Patterns:"
  shapes = [
    Circle.new(5),
    Rectangle.new(4, 6),
    Triangle.new(3, 4, 5)
  ]
  shapes.each do |shape|
    puts "   #{shape.class.name}: area = #{area(shape).round(2)}"
  end

  # 6. Guards
  puts "\n6. Guards (Conditional Patterns):"
  [-5, 0, 3, 50, 500].each do |n|
    puts "   #{n} -> #{classify(n)}"
  end

  # 7. Multiple values
  puts "\n7. Multiple Values (OR patterns):"
  ["Monday", "Saturday", "Sunday", "Wednesday"].each do |day|
    puts "   #{day} is weekend? #{is_weekend(day)}"
  end

  # 8. Range patterns
  puts "\n8. Range Patterns:"
  [95, 85, 75, 65, 55].each do |score|
    puts "   #{score} -> #{grade_letter(score)}"
  end

  # 9. Expression evaluator
  puts "\n9. Expression Evaluator:"
  expr = Mul.new(Add.new(Num.new(2), Num.new(3)), Num.new(4))
  puts "   (2 + 3) * 4 = #{expr.eval}"
  expr2 = Neg.new(Add.new(Num.new(5), Num.new(3)))
  puts "   -(5 + 3) = #{expr2.eval}"

  # 10. Type patterns
  puts "\n10. Type Patterns:"
  [42, 3.14, "hello", [1, 2, 3]].each do |val|
    puts "   #{describe_value(val)}"
  end

  # 11. Nested patterns (Ruby 3.0+)
  if RUBY_VERSION >= "3.0"
    puts "\n11. Nested Patterns:"
    [
      { user: { name: "Alice", age: 30 }, active: true },
      { user: { name: "Bob" }, active: false },
      { error: "Not found" }
    ].each do |data|
      puts "   #{analyze_nested(data)}"
    end
  end

  # 12. State machine
  puts "\n12. State Machine (Traffic Light):"
  state = "red"
  ["timer", "timer", "timer", "emergency"].each do |action|
    state = traffic_light_next(state, action)
    puts "   After '#{action}': #{state}"
  end

  # 13. Rightward assignment (Ruby 3.0+)
  if RUBY_VERSION >= "3.0"
    puts "\n13. Rightward Assignment (=>) Pattern:"
    puts "   #{demonstrate_rightward}"
  end

  puts "\n=== Ruby Pattern Matching Notes ==="
  puts "- case/when available in all versions"
  puts "- case/in pattern matching (Ruby 3.0+)"
  puts "- Array/Hash destructuring"
  puts "- Guards with if/unless"
  puts "- Pin operator (^) for matching variables"
  puts "- Find patterns for searching arrays"
end

main
