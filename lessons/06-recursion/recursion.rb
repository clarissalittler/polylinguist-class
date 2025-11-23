#!/usr/bin/env ruby
# Lesson 6: Recursion in Ruby
#
# Ruby supports recursion but has limited stack depth (default ~10,000 calls).
# No tail call optimization by default (can be enabled with a flag).

# ====================
# 1. Simple Recursion
# ====================

def factorial(n)
  return 1 if n <= 1
  n * factorial(n - 1)
end

def factorial_tail(n, acc = 1)
  return acc if n <= 1
  factorial_tail(n - 1, n * acc)
end

# ====================
# 2. Fibonacci
# ====================

def fibonacci(n)
  return n if n <= 1
  fibonacci(n - 1) + fibonacci(n - 2)
end

def fibonacci_memo(n, memo = {})
  return memo[n] if memo.key?(n)
  return n if n <= 1

  memo[n] = fibonacci_memo(n - 1, memo) + fibonacci_memo(n - 2, memo)
  memo[n]
end

# ====================
# 3. Array Recursion
# ====================

def sum_array(arr)
  return 0 if arr.empty?
  arr.first + sum_array(arr[1..-1])
end

def array_length(arr)
  return 0 if arr.empty?
  1 + array_length(arr[1..-1])
end

def reverse_array(arr)
  return [] if arr.empty?
  reverse_array(arr[1..-1]) + [arr.first]
end

def max_element(arr)
  return arr.first if arr.length == 1
  rest_max = max_element(arr[1..-1])
  arr.first > rest_max ? arr.first : rest_max
end

# ====================
# 4. String Recursion
# ====================

def reverse_string(s)
  return s if s.length <= 1
  reverse_string(s[1..-1]) + s[0]
end

def is_palindrome(s)
  clean = s.gsub(/\s+/, '').downcase
  check_palindrome(clean)
end

def check_palindrome(s)
  return true if s.length <= 1
  return false if s[0] != s[-1]
  check_palindrome(s[1..-2])
end

# ====================
# 5. Binary Search
# ====================

def binary_search(arr, target, low = 0, high = arr.length - 1)
  return -1 if low > high

  mid = (low + high) / 2

  if arr[mid] == target
    mid
  elsif arr[mid] > target
    binary_search(arr, target, low, mid - 1)
  else
    binary_search(arr, target, mid + 1, high)
  end
end

# ====================
# 6. Quicksort
# ====================

def quicksort(arr)
  return arr if arr.length <= 1

  pivot = arr[arr.length / 2]
  left = arr.select { |x| x < pivot }
  middle = arr.select { |x| x == pivot }
  right = arr.select { |x| x > pivot }

  quicksort(left) + middle + quicksort(right)
end

# ====================
# 7. Tower of Hanoi
# ====================

def hanoi(n, source, target, auxiliary, moves = [])
  if n == 1
    moves << "Move disk 1 from #{source} to #{target}"
    return moves
  end

  hanoi(n - 1, source, auxiliary, target, moves)
  moves << "Move disk #{n} from #{source} to #{target}"
  hanoi(n - 1, auxiliary, target, source, moves)

  moves
end

# ====================
# 8. Tree Traversal
# ====================

class TreeNode
  attr_accessor :value, :left, :right

  def initialize(value, left = nil, right = nil)
    @value = value
    @left = left
    @right = right
  end
end

def tree_height(node)
  return 0 if node.nil?

  left_height = tree_height(node.left)
  right_height = tree_height(node.right)

  1 + [left_height, right_height].max
end

def tree_sum(node)
  return 0 if node.nil?
  node.value + tree_sum(node.left) + tree_sum(node.right)
end

def inorder_traversal(node, result = [])
  return result if node.nil?

  inorder_traversal(node.left, result)
  result << node.value
  inorder_traversal(node.right, result)

  result
end

# ====================
# 9. Mutual Recursion
# ====================

def is_even(n)
  return true if n == 0
  is_odd(n - 1)
end

def is_odd(n)
  return false if n == 0
  is_even(n - 1)
end

# ====================
# 10. GCD
# ====================

def gcd(a, b)
  return a if b == 0
  gcd(b, a % b)
end

# ====================
# Tests
# ====================

def main
  puts "=== Recursion Examples in Ruby ===\n\n"

  # Factorial
  puts "1. Factorial:"
  puts "   factorial(5) = #{factorial(5)}"
  puts "   factorial_tail(5) = #{factorial_tail(5)}"

  # Fibonacci
  puts "\n2. Fibonacci:"
  puts "   fibonacci(10) = #{fibonacci(10)}"
  puts "   fibonacci_memo(30) = #{fibonacci_memo(30)}"

  # Array operations
  puts "\n3. Array Operations:"
  numbers = [1, 2, 3, 4, 5]
  puts "   sum_array(#{numbers}) = #{sum_array(numbers)}"
  puts "   array_length(#{numbers}) = #{array_length(numbers)}"
  puts "   reverse_array(#{numbers}) = #{reverse_array(numbers)}"
  puts "   max_element(#{numbers}) = #{max_element(numbers)}"

  # String operations
  puts "\n4. String Operations:"
  puts "   reverse_string('hello') = #{reverse_string('hello')}"
  puts "   is_palindrome('racecar') = #{is_palindrome('racecar')}"
  puts "   is_palindrome('A man a plan a canal Panama') = #{is_palindrome('A man a plan a canal Panama')}"

  # Binary search
  puts "\n5. Binary Search:"
  sorted = [1, 3, 5, 7, 9, 11, 13, 15]
  puts "   binary_search(#{sorted}, 7) = #{binary_search(sorted, 7)}"
  puts "   binary_search(#{sorted}, 4) = #{binary_search(sorted, 4)}"

  # Quicksort
  puts "\n6. Quicksort:"
  unsorted = [3, 6, 8, 10, 1, 2, 1]
  puts "   quicksort(#{unsorted}) = #{quicksort(unsorted)}"

  # Tower of Hanoi
  puts "\n7. Tower of Hanoi (3 disks):"
  moves = hanoi(3, 'A', 'C', 'B')
  moves.each { |move| puts "   #{move}" }

  # Tree
  puts "\n8. Binary Tree:"
  tree = TreeNode.new(5,
                      TreeNode.new(3,
                                   TreeNode.new(1),
                                   TreeNode.new(4)),
                      TreeNode.new(8,
                                   nil,
                                   TreeNode.new(9)))

  puts "   tree_height() = #{tree_height(tree)}"
  puts "   tree_sum() = #{tree_sum(tree)}"
  puts "   inorder_traversal() = #{inorder_traversal(tree)}"

  # Mutual recursion
  puts "\n9. Mutual Recursion:"
  puts "   is_even(10) = #{is_even(10)}"
  puts "   is_odd(10) = #{is_odd(10)}"
  puts "   is_even(7) = #{is_even(7)}"
  puts "   is_odd(7) = #{is_odd(7)}"

  # GCD
  puts "\n10. Greatest Common Divisor:"
  puts "   gcd(48, 18) = #{gcd(48, 18)}"
  puts "   gcd(100, 35) = #{gcd(100, 35)}"
end

main if __FILE__ == $PROGRAM_NAME
