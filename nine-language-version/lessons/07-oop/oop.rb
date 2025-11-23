#!/usr/bin/env ruby
# Lesson 7: Object-Oriented Programming in Ruby
#
# Ruby is a pure OOP language - EVERYTHING is an object!
# Even primitives like numbers and booleans are objects.

# ====================
# 1. Basic Class
# ====================

class Person
  # attr_reader creates getter methods
  # attr_accessor creates both getter and setter
  attr_reader :name
  attr_accessor :age

  def initialize(name, age)
    @name = name  # @ denotes instance variable
    @age = age
  end

  def introduce
    "Hi, I'm #{@name}, #{@age} years old"
  end

  def have_birthday
    @age += 1
  end
end

# ====================
# 2. Inheritance
# ====================

class Animal
  attr_reader :name, :species

  def initialize(name, species)
    @name = name
    @species = species
  end

  def speak
    "Some generic animal sound"
  end

  def sleep
    "#{@name} is sleeping... Zzz"
  end
end

class Dog < Animal  # Inheritance with <
  attr_reader :breed

  def initialize(name, breed)
    super(name, "Canine")  # Call parent constructor
    @breed = breed
  end

  def speak  # Override
    "#{@name} says Woof!"
  end

  def fetch
    "#{@name} is fetching the ball!"
  end
end

class Cat < Animal
  attr_reader :indoor

  def initialize(name, indoor = true)
    super(name, "Feline")
    @indoor = indoor
  end

  def speak  # Override
    "#{@name} says Meow!"
  end

  def scratch
    "#{@name} is scratching the furniture!"
  end
end

# ====================
# 3. Abstract-like Classes and Modules
# ====================

# Ruby doesn't have abstract classes, but we can simulate them
class Shape
  def initialize
    raise "Cannot instantiate abstract class Shape" if instance_of?(Shape)
  end

  def area
    raise NotImplementedError, "Subclass must implement area"
  end

  def perimeter
    raise NotImplementedError, "Subclass must implement perimeter"
  end

  def describe
    "#{self.class.name}: area=%.2f, perimeter=%.2f" % [area, perimeter]
  end
end

class Circle < Shape
  attr_reader :radius

  def initialize(radius)
    @radius = radius
    super()  # Call Shape's initialize (which won't raise since we're Circle)
  end

  def area
    Math::PI * @radius ** 2
  end

  def perimeter
    2 * Math::PI * @radius
  end
end

class Rectangle < Shape
  attr_reader :width, :height

  def initialize(width, height)
    @width = width
    @height = height
    super()
  end

  def area
    @width * @height
  end

  def perimeter
    2 * (@width + @height)
  end
end

class Triangle < Shape
  attr_reader :side_a, :side_b, :side_c

  def initialize(side_a, side_b, side_c)
    @side_a = side_a
    @side_b = side_b
    @side_c = side_c
    super()
  end

  def area
    # Heron's formula
    s = perimeter / 2.0
    Math.sqrt(s * (s - @side_a) * (s - @side_b) * (s - @side_c))
  end

  def perimeter
    @side_a + @side_b + @side_c
  end
end

# ====================
# 4. Modules (Mixins - Ruby's approach to multiple inheritance)
# ====================

module Flyable
  def fly
    "#{@name} is flying through the air"
  end
end

module Swimmable
  def swim
    "#{@name} is swimming in water"
  end
end

class Duck < Animal
  include Flyable  # Mix in Flyable module
  include Swimmable  # Mix in Swimmable module

  def initialize(name)
    super(name, "Waterfowl")
  end

  def quack
    "Quack!"
  end
end

# ====================
# 5. Encapsulation (Bank Account)
# ====================

class BankAccount
  attr_reader :account_number, :balance  # Read-only from outside

  def initialize(account_number, initial_balance = 0)
    @account_number = account_number
    @balance = initial_balance
    @transactions = []
  end

  def deposit(amount)
    if amount > 0
      @balance += amount
      @transactions << "Deposit: +$#{amount}"
      true
    else
      false
    end
  end

  def withdraw(amount)
    if amount > 0 && amount <= @balance
      @balance -= amount
      @transactions << "Withdrawal: -$#{amount}"
      true
    else
      false
    end
  end

  def transaction_history
    @transactions.dup  # Return copy
  end

  private  # Everything below is private

  def internal_audit
    # Private method - only accessible within class
    puts "Performing internal audit..."
  end
end

# ====================
# 6. Class Methods and Variables
# ====================

class Temperature
  @@conversion_count = 0  # @@ denotes class variable

  attr_reader :celsius

  def initialize(celsius)
    @celsius = celsius
  end

  # Class methods (self. prefix)
  def self.from_fahrenheit(fahrenheit)
    @@conversion_count += 1
    celsius = (fahrenheit - 32) * 5.0 / 9.0
    new(celsius)  # Calls Temperature.new
  end

  def self.from_kelvin(kelvin)
    @@conversion_count += 1
    celsius = kelvin - 273.15
    new(celsius)
  end

  def self.freezing?(celsius)
    celsius <= 0
  end

  def self.conversion_count
    @@conversion_count
  end

  def to_fahrenheit
    (@celsius * 9.0 / 5.0) + 32
  end

  def to_kelvin
    @celsius + 273.15
  end
end

# ====================
# 7. Composition
# ====================

class Engine
  attr_reader :horsepower, :running

  def initialize(horsepower)
    @horsepower = horsepower
    @running = false
  end

  def start
    @running = true
    "Engine starting... #{@horsepower}hp engine now running"
  end

  def stop
    @running = false
    "Engine stopped"
  end
end

class Car
  attr_reader :brand, :model, :engine

  def initialize(brand, model, horsepower)
    @brand = brand
    @model = model
    @engine = Engine.new(horsepower)  # Composition!
  end

  def start
    "#{@brand} #{@model}: #{@engine.start}"
  end

  def stop
    "#{@brand} #{@model}: #{@engine.stop}"
  end
end

# ====================
# 8. Operator Overloading
# ====================

class Point
  attr_reader :x, :y

  def initialize(x, y)
    @x = x
    @y = y
  end

  def to_s
    "Point(#{@x}, #{@y})"
  end

  def ==(other)
    return false unless other.is_a?(Point)
    @x == other.x && @y == other.y
  end

  def +(other)
    Point.new(@x + other.x, @y + other.y)
  end

  def -(other)
    Point.new(@x - other.x, @y - other.y)
  end

  def distance_from_origin
    Math.sqrt(@x ** 2 + @y ** 2)
  end
end

# ====================
# 9. Design Pattern: Singleton
# ====================

require 'singleton'

class MySingleton
  include Singleton  # Ruby provides Singleton module!

  attr_accessor :data

  def initialize
    @data = []
  end
end

# Manual Singleton implementation
class ManualSingleton
  @instance = nil

  def self.instance
    @instance ||= new  # ||= is "or-equals" (create if nil)
  end

  private_class_method :new  # Make constructor private

  def initialize
    @data = []
  end

  attr_accessor :data
end

# ====================
# 10. Design Pattern: Factory
# ====================

class AnimalFactory
  def self.create_animal(type, name)
    case type.downcase
    when "dog"
      Dog.new(name, "Mixed")
    when "cat"
      Cat.new(name)
    else
      Animal.new(name, "Unknown")
    end
  end
end

# ====================
# 11. Method Visibility
# ====================

class VisibilityExample
  def public_method
    "Anyone can call this"
  end

  protected

  def protected_method
    "Only this class and subclasses can call this"
  end

  private

  def private_method
    "Only this instance can call this"
  end
end

# ====================
# 12. Struct (Lightweight Class)
# ====================

# Struct creates a simple class automatically
Person2 = Struct.new(:name, :age) do
  def introduce
    "Hi, I'm #{name}, #{age} years old"
  end
end

# ====================
# 13. Open Classes (Monkey Patching)
# ====================
# Ruby allows modifying existing classes!

class String
  def shout
    upcase + "!!!"
  end
end

# ====================
# 14. attr_* Methods (Metaprogramming)
# ====================

class AttributeExample
  attr_reader :read_only      # Getter only
  attr_writer :write_only     # Setter only
  attr_accessor :read_write   # Both getter and setter

  def initialize
    @read_only = "can read"
    @write_only = "can write"
    @read_write = "can read and write"
  end
end

# ====================
# Tests and Examples
# ====================

def main
  puts "=== Object-Oriented Programming in Ruby ===\n\n"

  # 1. Basic class
  puts "1. Basic Class:"
  alice = Person.new("Alice", 30)
  puts "   #{alice.introduce}"
  alice.have_birthday
  puts "   After birthday: age = #{alice.age}"

  # 2. Inheritance and Polymorphism
  puts "\n2. Inheritance and Polymorphism:"
  animals = [
    Dog.new("Buddy", "Golden Retriever"),
    Cat.new("Whiskers", true),
    Dog.new("Max", "German Shepherd")
  ]

  animals.each do |animal|
    puts "   #{animal.speak}"
  end

  puts "   #{animals[0].fetch}"
  puts "   #{animals[1].scratch}"

  # 3. Abstract-like classes and shapes
  puts "\n3. Abstract-like Classes (Shapes):"
  shapes = [
    Circle.new(5),
    Rectangle.new(4, 6),
    Triangle.new(3, 4, 5)
  ]

  shapes.each do |shape|
    puts "   #{shape.describe}"
  end

  # 4. Modules (Mixins)
  puts "\n4. Modules (Mixins):"
  duck = Duck.new("Donald")
  puts "   #{duck.quack}"
  puts "   #{duck.fly}"
  puts "   #{duck.swim}"

  # 5. Encapsulation (Bank Account)
  puts "\n5. Encapsulation (Bank Account):"
  account = BankAccount.new("ACC001", 1000)
  puts "   Initial balance: $#{account.balance}"
  account.deposit(500)
  puts "   After deposit: $#{account.balance}"
  account.withdraw(200)
  puts "   After withdrawal: $#{account.balance}"
  puts "   Transactions: #{account.transaction_history}"

  # 6. Class methods
  puts "\n6. Class Methods:"
  temp1 = Temperature.new(0)
  temp2 = Temperature.from_fahrenheit(32)
  temp3 = Temperature.from_kelvin(273.15)

  puts "   0°C = %.1f°F" % temp1.to_fahrenheit
  puts "   32°F = %.1f°C" % temp2.celsius
  puts "   273.15K = %.1f°C" % temp3.celsius
  puts "   Is 0°C freezing? #{Temperature.freezing?(0)}"
  puts "   Conversions made: #{Temperature.conversion_count}"

  # 7. Composition
  puts "\n7. Composition:"
  car = Car.new("Toyota", "Camry", 200)
  puts "   #{car.start}"
  puts "   #{car.stop}"

  # 8. Operator overloading
  puts "\n8. Operator Overloading:"
  p1 = Point.new(3, 4)
  p2 = Point.new(1, 2)
  puts "   p1 = #{p1}"
  puts "   p2 = #{p2}"
  puts "   p1 + p2 = #{p1 + p2}"
  puts "   p1 - p2 = #{p1 - p2}"
  puts "   p1 == p2? #{p1 == p2}"
  puts "   p1 distance from origin: %.2f" % p1.distance_from_origin

  # 9. Singleton
  puts "\n9. Singleton Pattern:"
  s1 = ManualSingleton.instance
  s2 = ManualSingleton.instance
  puts "   s1 == s2? #{s1 == s2}"
  s1.data << "item1"
  puts "   s1.data: #{s1.data}"
  puts "   s2.data: #{s2.data}"

  # 10. Factory
  puts "\n10. Factory Pattern:"
  dog = AnimalFactory.create_animal("dog", "Rover")
  cat = AnimalFactory.create_animal("cat", "Mittens")
  puts "   #{dog.speak}"
  puts "   #{cat.speak}"

  # 11. Struct
  puts "\n11. Struct (Lightweight Class):"
  bob = Person2.new("Bob", 25)
  puts "   #{bob.introduce}"

  # 12. Open classes (monkey patching)
  puts "\n12. Open Classes (String extension):"
  puts "   #{"hello".shout}"

  # 13. Everything is an object in Ruby!
  puts "\n13. Everything is an Object:"
  puts "   5.class = #{5.class}"
  puts "   5.next = #{5.next}"
  puts "   true.class = #{true.class}"
  puts "   nil.class = #{nil.class}"
  puts "   String.class = #{String.class}"
end

# Run if executed directly
main if __FILE__ == $0
