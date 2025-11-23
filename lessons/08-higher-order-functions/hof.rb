#!/usr/bin/env ruby
# Lesson 8: Higher-Order Functions in Ruby
#
# Ruby is extremely functional-friendly with:
# - Blocks (Ruby's unique feature!)
# - Procs and lambdas
# - Built-in enumerable methods (map, select, reduce)
# - Everything is an object (even methods!)

# ====================
# 1. Functions as First-Class Values
# ====================

def demonstrate_first_class
  greet = lambda { |name| "Hello, #{name}!" }

  # Assign to variable
  say_hello = greet
  puts "   #{say_hello.call('Alice')}"

  # Store in data structure
  operations = {
    greet: greet,
    shout: lambda { |name| "HEY #{name.upcase}!" }
  }
  puts "   #{operations[:greet].call('Bob')}"
  puts "   #{operations[:shout].call('Bob')}"

  # Store in array
  funcs = [
    lambda { |x| x + 1 },
    lambda { |x| x * 2 },
    lambda { |x| x ** 2 }
  ]
  puts "   #{funcs.map { |f| f.call(5) }}"
end

# ====================
# 2. Blocks - Ruby's Special Feature
# ====================

def apply_twice(&block)
  # & converts block to proc
  lambda { |x| block.call(block.call(x)) }
end

def apply_n_times(n, &block)
  lambda do |x|
    result = x
    n.times { result = block.call(result) }
    result
  end
end

# ====================
# 3. Procs vs Lambdas
# ====================

def demonstrate_proc_vs_lambda
  # Lambda - strict arity, local return
  my_lambda = lambda { |x| x * 2 }

  # Proc - flexible arity, non-local return
  my_proc = Proc.new { |x| x * 2 }

  # Shorthand lambda syntax
  my_lambda2 = ->(x) { x * 2 }

  puts "   Lambda: #{my_lambda.call(5)}"
  puts "   Proc: #{my_proc.call(5)}"
  puts "   Shorthand lambda: #{my_lambda2.call(5)}"
end

# ====================
# 4. Functions Returning Functions
# ====================

def make_multiplier(n)
  lambda { |x| x * n }
end

def make_adder(n)
  ->(x) { x + n }  # Shorthand syntax
end

# ====================
# 5. Map - Transform Each Element
# ====================

def demonstrate_map
  numbers = [1, 2, 3, 4, 5]

  # Using map with block
  doubled = numbers.map { |x| x * 2 }
  puts "   Doubled: #{doubled}"

  # Using map with symbol-to-proc
  strings = ['hello', 'world']
  upcase = strings.map(&:upcase)
  puts "   Upcase: #{upcase}"

  # Multi-line block
  result = numbers.map do |x|
    x * 2 + 1
  end
  puts "   Complex: #{result}"

  # map with index
  with_index = numbers.map.with_index { |x, i| "#{i}: #{x}" }
  puts "   With index: #{with_index}"
end

# ====================
# 6. Select/Reject - Filter Elements
# ====================

def demonstrate_filter
  numbers = (1..10).to_a

  # select = filter (keep matching)
  evens = numbers.select { |x| x.even? }
  puts "   Evens (select): #{evens}"

  # reject = inverse filter (remove matching)
  odds = numbers.reject { |x| x.even? }
  puts "   Odds (reject): #{odds}"

  # Complex predicate
  big_evens = numbers.select { |x| x.even? && x > 5 }
  puts "   Big evens: #{big_evens}"

  # Symbol-to-proc magic
  positive = [-1, 0, 1, 2].select(&:positive?)
  puts "   Positive: #{positive}"
end

# ====================
# 7. Reduce/Inject - Combine to Single Value
# ====================

def demonstrate_reduce
  numbers = [1, 2, 3, 4, 5]

  # reduce with symbol
  sum = numbers.reduce(:+)
  puts "   Sum: #{sum}"

  # reduce with block and initial value
  product = numbers.reduce(1) { |acc, x| acc * x }
  puts "   Product: #{product}"

  # inject (alias for reduce)
  max_val = numbers.inject { |max, x| x > max ? x : max }
  puts "   Max: #{max_val}"

  # Build a hash
  fruits = ['apple', 'banana', 'apple', 'orange', 'banana', 'apple']
  counts = fruits.reduce(Hash.new(0)) do |acc, fruit|
    acc[fruit] += 1
    acc
  end
  puts "   Fruit counts: #{counts}"
end

# ====================
# 8. Closures
# ====================

def make_counter
  count = 0  # Captured variable

  lambda do
    count += 1
    count
  end
end

def make_bank_account(initial_balance)
  balance = initial_balance  # Private!

  {
    deposit: lambda do |amount|
      balance += amount if amount > 0
      balance
    end,
    withdraw: lambda do |amount|
      balance -= amount if amount > 0 && amount <= balance
      balance
    end,
    get_balance: lambda { balance }
  }
end

# ====================
# 9. Partial Application (Currying)
# ====================

def demonstrate_currying
  # Manual currying
  add = lambda { |a| lambda { |b| lambda { |c| a + b + c } } }
  puts "   Manual curry: #{add.call(1).call(2).call(3)}"

  # Ruby's curry method
  multiply = lambda { |a, b, c| a * b * c }
  curried = multiply.curry

  times_2 = curried.call(2)
  times_2_3 = times_2.call(3)
  result = times_2_3.call(4)
  puts "   Curried multiply: #{result}"

  # Partial application with curry
  power = lambda { |base, exp| base ** exp }
  square = power.curry.call(2)
  puts "   square(5) = #{square.call(5)}"
end

# ====================
# 10. Function Composition
# ====================

def compose(*funcs)
  lambda do |x|
    funcs.reverse.reduce(x) { |val, func| func.call(val) }
  end
end

def pipe(*funcs)
  lambda do |x|
    funcs.reduce(x) { |val, func| func.call(val) }
  end
end

def demonstrate_composition
  add_one = ->(x) { x + 1 }
  double = ->(x) { x * 2 }
  square = ->(x) { x ** 2 }

  # Right-to-left
  f = compose(double, add_one)
  puts "   compose(double, add_one)(5) = #{f.call(5)}"

  # Left-to-right
  g = pipe(add_one, double, square)
  puts "   pipe(add_one, double, square)(5) = #{g.call(5)}"
end

# ====================
# 11. Common Higher-Order Functions
# ====================

def demonstrate_common
  numbers = [1, 2, 3, 4, 5]

  # all? - all elements satisfy predicate
  puts "   all? positive: #{numbers.all? { |x| x > 0 }}"
  puts "   all? even: #{numbers.all?(&:even?)}"

  # any? - any element satisfies predicate
  puts "   any? even: #{numbers.any?(&:even?)}"

  # none? - no elements satisfy predicate
  puts "   none? negative: #{numbers.none? { |x| x < 0 }}"

  # find - first element matching predicate
  first_even = numbers.find(&:even?)
  puts "   find even: #{first_even}"

  # partition - split into [matching, non-matching]
  evens, odds = numbers.partition(&:even?)
  puts "   partition: evens=#{evens}, odds=#{odds}"

  # sort_by - sort using key function
  words = ['apple', 'pie', 'zoo', 'a']
  by_length = words.sort_by(&:length)
  puts "   sort_by length: #{by_length}"

  # group_by - group elements by key
  grouped = numbers.group_by { |x| x.even? ? 'even' : 'odd' }
  puts "   group_by: #{grouped}"

  # zip - combine two arrays
  list1 = [1, 2, 3]
  list2 = ['a', 'b', 'c']
  puts "   zip: #{list1.zip(list2)}"
end

# ====================
# 12. Method to Proc (&:symbol)
# ====================

def demonstrate_symbol_to_proc
  # &:method_name is syntactic sugar
  numbers = [1, 2, 3, 4, 5]

  # These are equivalent:
  doubled1 = numbers.map { |x| x * 2 }
  # Can't use &:* for this, but can for method calls:

  strings = ['hello', 'world', 'ruby']
  upcase1 = strings.map { |s| s.upcase }
  upcase2 = strings.map(&:upcase)  # Magic!

  puts "   #{upcase1}"
  puts "   #{upcase2}"

  # Works with chaining
  result = strings.map(&:upcase).map(&:reverse).map(&:length)
  puts "   Chained: #{result}"
end

# ====================
# 13. Each and Variants
# ====================

def demonstrate_each_variants
  numbers = [1, 2, 3, 4, 5]

  # each - iterate (returns original array)
  puts "   each:"
  numbers.each { |x| print "   #{x} " }
  puts

  # each_with_index
  numbers.each_with_index { |x, i| print "   #{i}:#{x} " }
  puts

  # each_cons - consecutive elements
  puts "   each_cons(2):"
  numbers.each_cons(2) { |pair| print "   #{pair} " }
  puts

  # each_slice - chunks
  puts "   each_slice(2):"
  numbers.each_slice(2) { |slice| print "   #{slice} " }
  puts
end

# ====================
# 14. Custom HOFs
# ====================

def custom_map(array, &block)
  result = []
  array.each { |item| result << block.call(item) }
  result
end

def custom_select(array, &block)
  result = []
  array.each { |item| result << item if block.call(item) }
  result
end

def custom_reduce(array, initial = nil, &block)
  enumerator = array.each
  accumulator = initial || enumerator.next

  enumerator.each do |item|
    accumulator = block.call(accumulator, item)
  end

  accumulator
end

# ====================
# 15. Lazy Evaluation
# ====================

def demonstrate_lazy
  # Lazy allows infinite sequences!
  infinite = (1..Float::INFINITY).lazy

  result = infinite
    .select { |x| x.even? }
    .map { |x| x * 2 }
    .take(5)
    .to_a

  puts "   First 5 doubled evens: #{result}"

  # Without lazy, this would never finish!
  # (1..Float::INFINITY).select {...} would try to create infinite array
end

# ====================
# 16. Blocks for Control Flow
# ====================

def with_timing
  start = Time.now
  yield  # Execute the block
  elapsed = Time.now - start
  puts "   Execution took #{elapsed} seconds"
end

def demonstrate_yield
  puts "   Using yield for custom control flow:"
  with_timing do
    sum = (1..1000000).reduce(:+)
  end
end

# ====================
# 17. Real-World Example: Data Pipeline
# ====================

def process_users(users)
  users
    .select { |u| u[:active] }
    .map do |u|
      u.merge(
        name: u[:name].strip.downcase,
        category: u[:age] >= 18 ? 'adult' : 'minor'
      )
    end
    .sort_by { |u| -u[:age] }  # Descending
    .take(10)
end

# ====================
# Main Demonstration
# ====================

def main
  puts "=== Higher-Order Functions in Ruby ===\n"

  # 1. First-class functions
  puts "1. Functions as First-Class Values:"
  demonstrate_first_class

  # 2. Proc vs Lambda
  puts "\n2. Procs vs Lambdas:"
  demonstrate_proc_vs_lambda

  # 3. Blocks
  puts "\n3. Blocks - Ruby's Special Feature:"
  f = apply_twice { |x| x + 1 }
  puts "   apply_twice { |x| x + 1 }(5) = #{f.call(5)}"
  g = apply_n_times(3) { |x| x * 2 }
  puts "   apply_n_times(3) { |x| x * 2 }(2) = #{g.call(2)}"

  # 4. Functions returning functions
  puts "\n4. Functions Returning Functions:"
  times_three = make_multiplier(3)
  add_ten = make_adder(10)
  puts "   times_three(7) = #{times_three.call(7)}"
  puts "   add_ten(5) = #{add_ten.call(5)}"

  # 5. Map
  puts "\n5. Map - Transform Each Element:"
  demonstrate_map

  # 6. Select/Reject
  puts "\n6. Select/Reject - Filter Elements:"
  demonstrate_filter

  # 7. Reduce
  puts "\n7. Reduce - Combine to Single Value:"
  demonstrate_reduce

  # 8. Closures
  puts "\n8. Closures:"
  counter = make_counter
  puts "   counter() = #{counter.call}"
  puts "   counter() = #{counter.call}"
  puts "   counter() = #{counter.call}"

  account = make_bank_account(1000)
  puts "   Initial balance: $#{account[:get_balance].call}"
  account[:deposit].call(500)
  puts "   After deposit: $#{account[:get_balance].call}"
  account[:withdraw].call(200)
  puts "   After withdrawal: $#{account[:get_balance].call}"

  # 9. Currying
  puts "\n9. Currying and Partial Application:"
  demonstrate_currying

  # 10. Composition
  puts "\n10. Function Composition:"
  demonstrate_composition

  # 11. Common HOFs
  puts "\n11. Common Higher-Order Functions:"
  demonstrate_common

  # 12. Symbol to proc
  puts "\n12. Symbol-to-Proc Magic (&:method):"
  demonstrate_symbol_to_proc

  # 13. Each variants
  puts "\n13. Each and Its Variants:"
  demonstrate_each_variants

  # 14. Custom implementations
  puts "\n14. Custom HOF Implementations:"
  numbers = [1, 2, 3, 4, 5]
  puts "   custom_map: #{custom_map(numbers) { |x| x * 2 }}"
  puts "   custom_select: #{custom_select(numbers, &:even?)}"
  puts "   custom_reduce: #{custom_reduce(numbers, 0) { |acc, x| acc + x }}"

  # 15. Lazy evaluation
  puts "\n15. Lazy Evaluation:"
  demonstrate_lazy

  # 16. Yield
  puts "\n16. Blocks for Custom Control Flow:"
  demonstrate_yield

  # 17. Real-world example
  puts "\n17. Real-World Data Pipeline:"
  users = [
    { name: ' Alice ', age: 25, active: true },
    { name: 'BOB', age: 17, active: true },
    { name: 'Charlie ', age: 30, active: false },
    { name: 'DIANA', age: 28, active: true },
  ]
  processed = process_users(users)
  puts "   Processed: #{processed}"

  # 18. Method chaining
  puts "\n18. Method Chaining:"
  result = [1, -2, 3, -4, 5, 6, 7, 8, 9, 10]
    .select(&:positive?)
    .map { |x| x ** 2 }
    .reduce(:+)
  puts "   Sum of squares of positive numbers: #{result}"

  puts "\n19. Ruby's Unique Features:"
  puts "   - Blocks are not objects (unless converted to Proc)"
  puts "   - Symbol-to-proc (&:method_name) is magical"
  puts "   - Lazy evaluation for infinite sequences"
  puts "   - Enumerables have 100+ built-in HOFs!"
end

main if __FILE__ == $0
