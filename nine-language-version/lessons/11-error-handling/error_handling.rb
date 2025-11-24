#!/usr/bin/env ruby

# Lesson 11: Error Handling & Debugging in Ruby
#
# Ruby features:
# - begin/rescue/ensure/else blocks
# - Multiple rescue clauses
# - Custom exceptions
# - retry mechanism
# - raise to throw exceptions
#
# This demonstrates Ruby's error handling.

puts "=== Error Handling in Ruby ===\n\n"

# ====================
# 1. Basic begin/rescue
# ====================

puts "1. Basic begin/rescue:"

def divide(x, y)
  begin
    result = x / y
    result.to_f
  rescue ZeroDivisionError
    puts "   Error: Cannot divide by zero!"
    Float::INFINITY
  rescue TypeError => e
    puts "   Error: Invalid types - #{e.message}"
    Float::NAN
  end
end

puts "   divide(10, 2) = #{divide(10, 2)}"
puts "   divide(10, 0) = #{divide(10, 0)}"

# ====================
# 2. rescue/else/ensure
# ====================

puts "\n2. rescue/else/ensure:"

def process_file(filename)
  begin
    puts "   Opening #{filename}..."
    file = File.open(filename, 'r')
    content = file.read
    puts "   Read #{content.length} characters"
  rescue Errno::ENOENT
    puts "   Error: File '#{filename}' not found"
    return false
  rescue Errno::EACCES
    puts "   Error: No permission to read '#{filename}'"
    return false
  else
    puts "   Successfully processed '#{filename}'"
    return true
  ensure
    file&.close
    puts "   Cleanup complete"
  end
end

# Create test file
File.write('/tmp/test.txt', 'Hello, World!')

process_file('/tmp/test.txt')
process_file('/nonexistent/file.txt')

# ====================
# 3. Custom exceptions
# ====================

puts "\n3. Custom Exceptions:"

class ValidationError < StandardError; end

class InvalidEmailError < ValidationError
  attr_reader :email, :reason

  def initialize(email, reason)
    @email = email
    @reason = reason
    super("Invalid email '#{email}': #{reason}")
  end
end

def validate_email(email)
  raise InvalidEmailError.new(email, "missing @ symbol") unless email.include?('@')
  domain = email.split('@')[1]
  raise InvalidEmailError.new(email, "missing domain extension") unless domain&.include?('.')
  puts "   Valid email: #{email}"
end

begin
  validate_email("user@example.com")
  validate_email("invalid-email")
rescue InvalidEmailError => e
  puts "   Caught: #{e.message}"
end

# ====================
# 4. Inline rescue
# ====================

puts "\n4. Inline rescue:"

def safe_parse(s)
  Integer(s) rescue nil
end

puts "   safe_parse('42') = #{safe_parse('42')}"
puts "   safe_parse('invalid') = #{safe_parse('invalid').inspect}"

# ====================
# 5. retry mechanism
# ====================

puts "\n5. retry mechanism:"

attempt = 0
begin
  attempt += 1
  puts "   Attempt #{attempt}"
  raise "Simulated failure" if attempt < 3
  puts "   Success!"
rescue StandardError => e
  retry if attempt < 3
  puts "   All attempts failed: #{e.message}"
end

# ====================
# 6. raise to throw exceptions
# ====================

puts "\n6. Raising exceptions:"

def validate_age(age)
  raise TypeError, "Age must be an integer" unless age.is_a?(Integer)
  raise ArgumentError, "Age cannot be negative" if age < 0
  raise ArgumentError, "Age #{age} seems unrealistic" if age > 150
  puts "   Valid age: #{age}"
end

begin
  validate_age(25)
  validate_age(-5)
rescue ArgumentError => e
  puts "   Caught: #{e.message}"
end

# ====================
# 7. Error recovery
# ====================

puts "\n7. Error Recovery:"

def robust_parse(items)
  results = []
  errors = 0

  items.each_with_index do |item, i|
    begin
      results << Integer(item)
    rescue ArgumentError, TypeError
      errors += 1
    end
  end

  puts "   Parsed #{results.length}/#{items.length} items"
  puts "   Results: #{results}"
  puts "   Errors: #{errors}" if errors > 0

  results
end

robust_parse(['1', '2', 'bad', '4', nil, '6'])

# ====================
# 8. Stack traces
# ====================

puts "\n8. Stack Traces:"

def method_a
  method_b
end

def method_b
  method_c
end

def method_c
  begin
    raise "Error in method_c"
  rescue StandardError => e
    puts "   Error: #{e.message}"
    puts "   Stack trace:"
    puts e.backtrace.first(3).map { |line| "      #{line}" }
  end
end

method_a

# ====================
# Summary
# ====================

puts "\n=== Ruby Error Handling Features ==="
puts "- begin/rescue/ensure/else blocks"
puts "- Multiple rescue clauses"
puts "- Custom exception hierarchies"
puts "- Inline rescue for simple cases"
puts "- retry mechanism for error recovery"
puts "- raise to throw exceptions"
puts "- Stack traces for debugging"
puts "- Similar to Python's try/except"
