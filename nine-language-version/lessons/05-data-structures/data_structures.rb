#!/usr/bin/env ruby
# Lesson 5: Data Structures in Ruby
# Demonstrates arrays, hashes, sets

require 'set'

def main
  puts "=== Ruby Data Structures ==="
  puts

  # 1. Arrays (mutable, dynamic)
  puts "1. Arrays (Mutable, Dynamic):"
  numbers = [1, 2, 3, 4, 5]
  puts "  Original: #{numbers}"

  numbers << 6  # Push operator
  puts "  After << 6: #{numbers}"

  numbers[0] = 10
  puts "  After numbers[0] = 10: #{numbers}"

  # Array methods
  puts "\n  Array methods:"
  puts "  First: #{numbers.first}"
  puts "  Last: #{numbers.last}"
  puts "  Length: #{numbers.length}"
  puts "  Take 3: #{numbers.take(3)}"

  # 2. Array operations (functional style)
  puts "\n2. Array Operations (Functional):"
  nums = [1, 2, 3, 4, 5]

  squared = nums.map { |x| x * x }
  puts "  map(square): #{squared}"

  evens = nums.select { |x| x.even? }
  puts "  select(even): #{evens}"

  sum = nums.reduce(0) { |acc, x| acc + x }
  puts "  reduce(sum): #{sum}"

  # 3. Hashes (mutable dictionaries)
  puts "\n3. Hashes (Mutable Dictionaries):"
  person = { name: "Alice", age: 30, city: "NYC" }
  puts "  Person: #{person}"

  # Modify
  person[:age] = 31
  person[:email] = "alice@example.com"
  puts "  After modifications: #{person}"

  # Access
  puts "  Name: #{person[:name]}"
  puts "  Has :age? #{person.has_key?(:age)}"

  # Iteration
  puts "  Entries:"
  person.each do |key, value|
    puts "    #{key}: #{value}"
  end

  # 4. Sets (unique values)
  puts "\n4. Sets (Unique Values):"
  set = Set.new([1, 2, 3, 4, 5])
  puts "  Set: #{set.to_a}"

  set.add(6)
  set.add(3)  # Duplicate, ignored
  puts "  After adding 6 and 3: #{set.to_a}"

  puts "  Include 3? #{set.include?(3)}"
  puts "  Size: #{set.size}"

  # Set operations
  evens = Set.new([2, 4, 6, 8])
  odds = Set.new([1, 3, 5, 7])
  puts "  Union: #{(evens | odds).to_a}"
  puts "  Intersection: #{(evens & odds).to_a}"
  puts "  Difference: #{(evens - odds).to_a}"

  # 5. Ranges
  puts "\n5. Ranges:"
  range = (1..5)
  puts "  Range 1..5: #{range.to_a}"
  puts "  Include 3? #{range.include?(3)}"

  # Range to array
  arr = (1..10).to_a
  puts "  Range to array: #{arr}"

  # 6. Symbols vs strings
  puts "\n6. Symbols vs Strings:"
  puts "  Symbol: :name (immutable, memory-efficient)"
  puts "  String: \"name\" (mutable)"
  puts "  Symbols are often used as hash keys"

  # 7. Destructuring
  puts "\n7. Destructuring:"
  point = [3, 4]
  x, y = point
  puts "  [x, y] = [3, 4]: x=#{x}, y=#{y}"

  first, *rest = [1, 2, 3, 4, 5]
  puts "  first, *rest = [1,2,3,4,5]: first=#{first}, rest=#{rest}"

  # 8. Nested structures
  puts "\n8. Nested Structures:"
  matrix = [
    [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9]
  ]
  puts "  Matrix: #{matrix}"
  puts "  matrix[1][2] = #{matrix[1][2]}"

  # 9. Freezing (immutability)
  puts "\n9. Freezing (Immutability):"
  frozen = [1, 2, 3].freeze
  puts "  Frozen array: #{frozen}"
  begin
    frozen << 4
  rescue => e
    puts "  Cannot modify frozen: #{e.message}"
  end

  # 10. Key insights
  puts "\n10. Key Insights:"
  puts "  - Everything is mutable by default"
  puts "  - Rich set of built-in methods"
  puts "  - Symbols for efficient constants"
  puts "  - freeze for immutability"
  puts "  - Functional methods (map, select, reduce)"
end

main
