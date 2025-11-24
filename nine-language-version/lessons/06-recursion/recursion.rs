#!/usr/bin/env rustc
/**
 * Lesson 6: Recursion in Rust
 *
 * Rust supports recursion but stack space is limited (default ~2-8 MB).
 * Rust does NOT have tail call optimization (yet), so be careful with
 * deep recursion. The type system and ownership model add safety.
 */

// ====================
// 1. Simple Recursion
// ====================

fn factorial(n: u64) -> u64 {
    // Calculate n! recursively
    if n <= 1 {
        1
    } else {
        n * factorial(n - 1)
    }
}

fn factorial_tail(n: u64) -> u64 {
    // Tail-recursive factorial (not optimized in current Rust)
    fn go(n: u64, acc: u64) -> u64 {
        if n <= 1 {
            acc
        } else {
            go(n - 1, n * acc)
        }
    }
    go(n, 1)
}

// ====================
// 2. Fibonacci
// ====================

fn fibonacci(n: u32) -> u64 {
    // Fibonacci (inefficient)
    if n <= 1 {
        n as u64
    } else {
        fibonacci(n - 1) + fibonacci(n - 2)
    }
}

use std::collections::HashMap;

fn fibonacci_memo(n: u32, memo: &mut HashMap<u32, u64>) -> u64 {
    // Fibonacci with memoization
    if let Some(&result) = memo.get(&n) {
        return result;
    }

    let result = if n <= 1 {
        n as u64
    } else {
        fibonacci_memo(n - 1, memo) + fibonacci_memo(n - 2, memo)
    };

    memo.insert(n, result);
    result
}

// ====================
// 3. Slice Recursion
// ====================

fn sum_slice(arr: &[i32]) -> i32 {
    // Sum all elements recursively
    if arr.is_empty() {
        0
    } else {
        arr[0] + sum_slice(&arr[1..])
    }
}

fn slice_length(arr: &[i32]) -> usize {
    // Calculate length recursively
    if arr.is_empty() {
        0
    } else {
        1 + slice_length(&arr[1..])
    }
}

fn reverse_vec(vec: Vec<i32>) -> Vec<i32> {
    // Reverse vector recursively
    if vec.is_empty() {
        return vec;
    }

    let mut result = reverse_vec(vec[1..].to_vec());
    result.push(vec[0]);
    result
}

fn max_element(arr: &[i32]) -> Option<i32> {
    // Find maximum element
    match arr.len() {
        0 => None,
        1 => Some(arr[0]),
        _ => {
            let rest_max = max_element(&arr[1..]).unwrap();
            Some(if arr[0] > rest_max { arr[0] } else { rest_max })
        }
    }
}

// ====================
// 4. String Recursion
// ====================

fn reverse_string(s: &str) -> String {
    // Reverse string recursively
    if s.len() <= 1 {
        return s.to_string();
    }

    let mut chars: Vec<char> = s.chars().collect();
    let first = chars.remove(0);
    let rest: String = chars.into_iter().collect();

    reverse_string(&rest) + &first.to_string()
}

fn is_palindrome(s: &str) -> bool {
    // Check if palindrome
    let clean: String = s.chars()
        .filter(|c| !c.is_whitespace())
        .map(|c| c.to_lowercase().to_string())
        .collect();

    check_palindrome(&clean)
}

fn check_palindrome(s: &str) -> bool {
    if s.len() <= 1 {
        return true;
    }

    let chars: Vec<char> = s.chars().collect();
    if chars[0] != chars[chars.len() - 1] {
        return false;
    }

    let middle: String = chars[1..chars.len()-1].iter().collect();
    check_palindrome(&middle)
}

// ====================
// 5. Binary Search
// ====================

fn binary_search(arr: &[i32], target: i32) -> Option<usize> {
    // Binary search recursively
    fn search(arr: &[i32], target: i32, low: usize, high: usize, offset: usize) -> Option<usize> {
        if low > high {
            return None;
        }

        let mid = (low + high) / 2;

        if arr[mid] == target {
            Some(mid + offset)
        } else if arr[mid] > target {
            if mid == 0 {
                None
            } else {
                search(arr, target, low, mid - 1, offset)
            }
        } else {
            search(arr, target, mid + 1, high, offset)
        }
    }

    if arr.is_empty() {
        None
    } else {
        search(arr, target, 0, arr.len() - 1, 0)
    }
}

// ====================
// 6. Quicksort
// ====================

fn quicksort(arr: Vec<i32>) -> Vec<i32> {
    // Sort using quicksort
    if arr.len() <= 1 {
        return arr;
    }

    let pivot = arr[arr.len() / 2];
    let left: Vec<i32> = arr.iter().filter(|&&x| x < pivot).copied().collect();
    let middle: Vec<i32> = arr.iter().filter(|&&x| x == pivot).copied().collect();
    let right: Vec<i32> = arr.iter().filter(|&&x| x > pivot).copied().collect();

    let mut result = quicksort(left);
    result.extend(middle);
    result.extend(quicksort(right));
    result
}

// ====================
// 7. Tower of Hanoi
// ====================

fn hanoi(n: u32, source: &str, target: &str, auxiliary: &str) -> Vec<String> {
    // Solve Tower of Hanoi
    if n == 1 {
        return vec![format!("Move disk 1 from {} to {}", source, target)];
    }

    let mut moves = Vec::new();

    // Move n-1 from source to auxiliary
    moves.extend(hanoi(n - 1, source, auxiliary, target));

    // Move largest from source to target
    moves.push(format!("Move disk {} from {} to {}", n, source, target));

    // Move n-1 from auxiliary to target
    moves.extend(hanoi(n - 1, auxiliary, target, source));

    moves
}

// ====================
// 8. Tree Traversal
// ====================

#[derive(Debug, Clone)]
struct TreeNode {
    value: i32,
    left: Option<Box<TreeNode>>,
    right: Option<Box<TreeNode>>,
}

impl TreeNode {
    fn new(value: i32) -> Self {
        TreeNode {
            value,
            left: None,
            right: None,
        }
    }

    fn with_children(value: i32, left: Option<Box<TreeNode>>, right: Option<Box<TreeNode>>) -> Self {
        TreeNode { value, left, right }
    }
}

fn tree_height(node: &Option<Box<TreeNode>>) -> u32 {
    // Calculate height
    match node {
        None => 0,
        Some(n) => {
            let left_height = tree_height(&n.left);
            let right_height = tree_height(&n.right);
            1 + left_height.max(right_height)
        }
    }
}

fn tree_sum(node: &Option<Box<TreeNode>>) -> i32 {
    // Sum all values
    match node {
        None => 0,
        Some(n) => n.value + tree_sum(&n.left) + tree_sum(&n.right),
    }
}

fn inorder_traversal(node: &Option<Box<TreeNode>>, result: &mut Vec<i32>) {
    // Left, root, right
    if let Some(n) = node {
        inorder_traversal(&n.left, result);
        result.push(n.value);
        inorder_traversal(&n.right, result);
    }
}

fn preorder_traversal(node: &Option<Box<TreeNode>>, result: &mut Vec<i32>) {
    // Root, left, right
    if let Some(n) = node {
        result.push(n.value);
        preorder_traversal(&n.left, result);
        preorder_traversal(&n.right, result);
    }
}

fn postorder_traversal(node: &Option<Box<TreeNode>>, result: &mut Vec<i32>) {
    // Left, right, root
    if let Some(n) = node {
        postorder_traversal(&n.left, result);
        postorder_traversal(&n.right, result);
        result.push(n.value);
    }
}

// ====================
// 9. Mutual Recursion
// ====================

fn is_even(n: u32) -> bool {
    // Check if even using mutual recursion
    if n == 0 {
        true
    } else {
        is_odd(n - 1)
    }
}

fn is_odd(n: u32) -> bool {
    // Check if odd
    if n == 0 {
        false
    } else {
        is_even(n - 1)
    }
}

// ====================
// 10. Greatest Common Divisor
// ====================

fn gcd(a: u32, b: u32) -> u32 {
    // Euclid's algorithm
    if b == 0 {
        a
    } else {
        gcd(b, a % b)
    }
}

// ====================
// Tests and Examples
// ====================

fn main() {
    println!("=== Recursion Examples in Rust ===\n");

    // Factorial
    println!("1. Factorial:");
    println!("   factorial(5) = {}", factorial(5));
    println!("   factorial_tail(5) = {}", factorial_tail(5));

    // Fibonacci
    println!("\n2. Fibonacci:");
    println!("   fibonacci(10) = {}", fibonacci(10));
    let mut memo = HashMap::new();
    println!("   fibonacci_memo(30) = {}", fibonacci_memo(30, &mut memo));

    // Slice operations
    println!("\n3. Slice Operations:");
    let numbers = vec![1, 2, 3, 4, 5];
    println!("   sum_slice({:?}) = {}", numbers, sum_slice(&numbers));
    println!("   slice_length({:?}) = {}", numbers, slice_length(&numbers));
    println!("   reverse_vec({:?}) = {:?}", numbers, reverse_vec(numbers.clone()));
    println!("   max_element({:?}) = {:?}", numbers, max_element(&numbers));

    // String operations
    println!("\n4. String Operations:");
    println!("   reverse_string('hello') = {}", reverse_string("hello"));
    println!("   is_palindrome('racecar') = {}", is_palindrome("racecar"));
    println!("   is_palindrome('A man a plan a canal Panama') = {}",
             is_palindrome("A man a plan a canal Panama"));

    // Binary search
    println!("\n5. Binary Search:");
    let sorted_arr = vec![1, 3, 5, 7, 9, 11, 13, 15];
    println!("   binary_search({:?}, 7) = {:?}", sorted_arr, binary_search(&sorted_arr, 7));
    println!("   binary_search({:?}, 4) = {:?}", sorted_arr, binary_search(&sorted_arr, 4));

    // Quicksort
    println!("\n6. Quicksort:");
    let unsorted = vec![3, 6, 8, 10, 1, 2, 1];
    println!("   quicksort({:?}) = {:?}", unsorted, quicksort(unsorted));

    // Tower of Hanoi
    println!("\n7. Tower of Hanoi (3 disks):");
    let moves = hanoi(3, "A", "C", "B");
    for move_str in moves {
        println!("   {}", move_str);
    }

    // Tree operations
    println!("\n8. Binary Tree:");
    //       5
    //      / \
    //     3   8
    //    / \   \
    //   1   4   9
    let tree = Some(Box::new(TreeNode::with_children(
        5,
        Some(Box::new(TreeNode::with_children(
            3,
            Some(Box::new(TreeNode::new(1))),
            Some(Box::new(TreeNode::new(4))),
        ))),
        Some(Box::new(TreeNode::with_children(
            8,
            None,
            Some(Box::new(TreeNode::new(9))),
        ))),
    )));

    println!("   tree_height() = {}", tree_height(&tree));
    println!("   tree_sum() = {}", tree_sum(&tree));

    let mut inorder = Vec::new();
    inorder_traversal(&tree, &mut inorder);
    println!("   inorder_traversal() = {:?}", inorder);

    let mut preorder = Vec::new();
    preorder_traversal(&tree, &mut preorder);
    println!("   preorder_traversal() = {:?}", preorder);

    let mut postorder = Vec::new();
    postorder_traversal(&tree, &mut postorder);
    println!("   postorder_traversal() = {:?}", postorder);

    // Mutual recursion
    println!("\n9. Mutual Recursion:");
    println!("   is_even(10) = {}", is_even(10));
    println!("   is_odd(10) = {}", is_odd(10));
    println!("   is_even(7) = {}", is_even(7));
    println!("   is_odd(7) = {}", is_odd(7));

    // GCD
    println!("\n10. Greatest Common Divisor:");
    println!("   gcd(48, 18) = {}", gcd(48, 18));
    println!("   gcd(100, 35) = {}", gcd(100, 35));
}
