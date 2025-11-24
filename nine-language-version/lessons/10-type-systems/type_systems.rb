#!/usr/bin/env ruby

# Lesson 10: Type Systems in Ruby
#
# Ruby features:
# - Dynamic typing (types at runtime)
# - Strong typing (no implicit coercion between incompatible types)
# - Duck typing (if it responds to the method, it works)
# - Optional type signatures with RBS (Ruby 3+)
# - Sorbet for gradual typing
#
# This demonstrates Ruby's type system.

# ====================
# 1. Dynamic Typing
# ====================

def increment(x)
  x + 1
end

def add(x, y)
  x + y
end

puts "=== Type Systems in Ruby ===\n\n"

puts "1. Dynamic Typing:"
puts "   add(1, 2) = #{add(1, 2)}"
puts "   add('hello', ' world') = #{add('hello', ' world')}"
puts "   add([1], [2]) = #{add([1], [2]).inspect}"

# ====================
# 2. Duck Typing
# ====================

class Circle
  attr_reader :radius

  def initialize(radius)
    @radius = radius
  end

  def area
    Math::PI * @radius ** 2
  end

  def draw
    "Drawing circle with radius #{@radius}"
  end
end

class Rectangle
  attr_reader :width, :height

  def initialize(width, height)
    @width = width
    @height = height
  end

  def area
    @width * @height
  end

  def draw
    "Drawing rectangle #{@width}x#{@height}"
  end
end

def render(shape)
  # Duck typing: if it responds to draw, we can use it
  shape.draw if shape.respond_to?(:draw)
end

puts "\n2. Duck Typing:"
circle = Circle.new(5)
rectangle = Rectangle.new(4, 6)
puts "   #{render(circle)}"
puts "   #{render(rectangle)}"

# ====================
# 3. Type Checking with is_a? and kind_of?
# ====================

puts "\n3. Runtime Type Checking:"
puts "   42.is_a?(Integer) = #{42.is_a?(Integer)}"
puts "   'hello'.is_a?(String) = #{'hello'.is_a?(String)}"
puts "   circle.is_a?(Circle) = #{circle.is_a?(Circle)}"
puts "   circle.kind_of?(Object) = #{circle.kind_of?(Object)}"

# ====================
# 4. respond_to? for Duck Typing
# ====================

def safe_call(obj, method)
  obj.respond_to?(method) ? obj.send(method) : "Method not available"
end

puts "\n4. respond_to? (Duck Typing Check):"
puts "   safe_call(circle, :area) = #{safe_call(circle, :area)}"
puts "   safe_call(circle, :fly) = #{safe_call(circle, :fly)}"

# ====================
# 5. Modules for Type Constraints
# ====================

module Drawable
  def draw
    raise NotImplementedError, "Subclass must implement draw"
  end
end

class Triangle
  include Drawable

  attr_reader :a, :b, :c

  def initialize(a, b, c)
    @a, @b, @c = a, b, c
  end

  def area
    s = (@a + @b + @c) / 2.0
    Math.sqrt(s * (s - @a) * (s - @b) * (s - @c))
  end

  def draw
    "Drawing triangle with sides #{@a}, #{@b}, #{@c}"
  end
end

puts "\n5. Modules (Mixin Types):"
triangle = Triangle.new(3, 4, 5)
puts "   triangle.is_a?(Drawable) = #{triangle.is_a?(Drawable)}"
puts "   #{triangle.draw}"

# ====================
# 6. RBS Type Signatures (Ruby 3+)
# ====================

# RBS files (.rbs) define type signatures:
# class Person
#   attr_reader name: String
#   attr_reader age: Integer
#   def initialize: (String name, Integer age) -> void
#   def adult?: () -> bool
# end

class Person
  attr_reader :name, :age

  def initialize(name, age)
    @name = name
    @age = age
  end

  def adult?
    @age >= 18
  end
end

puts "\n6. Ruby Classes (RBS signatures available):"
person = Person.new("Alice", 30)
puts "   #{person.inspect}"
puts "   adult? = #{person.adult?}"

# ====================
# 7. Generics via Methods
# ====================

def identity(x)
  x
end

def first(array)
  array.first
end

puts "\n7. Generic Methods:"
puts "   identity(42) = #{identity(42)}"
puts "   identity('hello') = #{identity('hello')}"
puts "   first([1,2,3]) = #{first([1,2,3])}"

# ====================
# 8. Type Coercion
# ====================

puts "\n8. Strong Typing (Limited Coercion):"
begin
  result = "5" + 3
  puts "   'This won't execute'"
rescue TypeError => e
  puts "   '5' + 3 raises TypeError: #{e.message}"
end

puts "   '5'.to_i + 3 = #{'5'.to_i + 3} (explicit conversion)"

# ====================
# 9. Symbols as Types
# ====================

puts "\n9. Symbols (Unique Identifiers):"
sym1 = :id
sym2 = :id
puts "   :id == :id: #{sym1 == sym2}"
puts "   :id.class: #{sym1.class}"
puts "   'id'.to_sym == :id: #{'id'.to_sym == sym1}"

# ====================
# 10. Sorbet Example
# ====================

puts "\n10. Sorbet (Gradual Typing):"
puts "   Sorbet adds static type checking to Ruby"
puts "   Example (requires Sorbet):"
puts "   # typed: true"
puts "   class Calculator"
puts "     extend T::Sig"
puts "     sig { params(x: Integer, y: Integer).returns(Integer) }"
puts "     def add(x, y)"
puts "       x + y"
puts "     end"
puts "   end"

# ====================
# Summary
# ====================

puts "\n=== Ruby Type System Features ==="
puts "- Dynamic typing (types at runtime)"
puts "- Strong typing (limited implicit coercion)"
puts "- Duck typing (respond_to?)"
puts "- is_a? and kind_of? for type checks"
puts "- Modules for mixins and traits"
puts "- RBS for type signatures (Ruby 3+)"
puts "- Sorbet for gradual static typing"
puts "- Everything is an object"
puts "- Flexible and expressive"
