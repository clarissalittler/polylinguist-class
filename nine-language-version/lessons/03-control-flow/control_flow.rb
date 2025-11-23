#!/usr/bin/env ruby
# Lesson 3: Control Flow in Ruby
# Demonstrates conditionals, loops, and boolean logic

def main
  puts "=== Ruby Control Flow ==="
  puts

  # 1. Basic conditionals
  puts "1. Basic Conditionals:"
  age = 20
  if age >= 18
    puts "  Age #{age}: Adult"
  elsif age >= 13
    puts "  Age #{age}: Teenager"
  else
    puts "  Age #{age}: Child"
  end

  # Ternary expression
  status = age >= 18 ? "Adult" : "Minor"
  puts "  Status: #{status}"

  # Postfix if (very Ruby)
  puts "  Can vote!" if age >= 18

  # Unless (negative if)
  puts "  Not a teenager" unless age >= 13 && age < 18

  # 2. For loops (Ruby style with iterators)
  puts "\n2. Iteration (Ruby style):"
  puts "  Count to 5:"
  print "   "
  5.times do |i|
    print " #{i}"
  end
  puts

  puts "  Iterate array:"
  fruits = ["apple", "banana", "cherry"]
  fruits.each do |fruit|
    puts "    #{fruit}"
  end

  puts "  Each with index:"
  fruits.each_with_index do |fruit, i|
    puts "    #{i}: #{fruit}"
  end

  # 3. While loop
  puts "\n3. While Loop:"
  count = 0
  while count < 3
    puts "  Count: #{count}"
    count += 1
  end

  # Until loop (negative while)
  puts "\n  Until Loop:"
  countdown = 3
  until countdown == 0
    puts "  Countdown: #{countdown}"
    countdown -= 1
  end

  # 4. Boolean logic
  puts "\n4. Boolean Logic:"
  x = 5
  y = 10
  puts "  x=#{x}, y=#{y}"
  puts "  x > 3 && y < 20: #{x > 3 && y < 20}"
  puts "  x > 10 || y > 5: #{x > 10 || y > 5}"
  puts "  !(x == y): #{!(x == y)}"

  puts "\n  Truthiness (only nil and false are falsy):"
  values = [true, false, nil, 0, 1, "", "hello", [], [1, 2]]
  values.each do |val|
    truthy = val ? "truthy" : "falsy"
    puts "    #{val.inspect}: #{truthy}"
  end

  # 5. FizzBuzz
  puts "\n5. FizzBuzz (1-20):"
  print " "
  (1..20).each do |i|
    if i % 15 == 0
      print " FizzBuzz"
    elsif i % 3 == 0
      print " Fizz"
    elsif i % 5 == 0
      print " Buzz"
    else
      print " #{i}"
    end
  end
  puts

  # 6. Case statement
  puts "\n6. Case Statement:"
  day = 3 # Wednesday
  day_type = case day
  when 0, 6
    "Weekend"
  when 1, 2, 3, 4, 5
    "Weekday"
  else
    "Invalid day"
  end
  puts "  Day #{day}: #{day_type}"

  # Case with ranges
  score = 85
  grade = case score
  when 90..100
    "A"
  when 80..89
    "B"
  when 70..79
    "C"
  when 60..69
    "D"
  else
    "F"
  end
  puts "  Score #{score}: Grade #{grade}"

  # 7. Loop with break
  puts "\n7. Loop with Break:"
  i = 0
  loop do
    puts "  Iteration: #{i}"
    i += 1
    break if i >= 3
  end
end

main
