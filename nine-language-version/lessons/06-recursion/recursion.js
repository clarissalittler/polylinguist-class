#!/usr/bin/env node
/**
 * Lesson 6: Recursion in JavaScript
 *
 * JavaScript supports recursion. Modern engines have some tail call
 * optimization in strict mode, but it's not widely supported.
 * Default stack size varies by engine (~10,000-50,000 calls).
 */

// ====================
// 1. Simple Recursion
// ====================

function factorial(n) {
    // Calculate n! recursively
    if (n <= 1) {  // Base case
        return 1;
    }
    return n * factorial(n - 1);  // Recursive case
}

function factorialTail(n, accumulator = 1) {
    // Tail-recursive factorial
    if (n <= 1) {
        return accumulator;
    }
    return factorialTail(n - 1, n * accumulator);
}

// ====================
// 2. Fibonacci
// ====================

function fibonacci(n) {
    // Calculate nth Fibonacci number (inefficient)
    if (n <= 1) {
        return n;
    }
    return fibonacci(n - 1) + fibonacci(n - 2);
}

function fibonacciMemo(n, memo = {}) {
    // Fibonacci with memoization
    if (n in memo) {
        return memo[n];
    }

    if (n <= 1) {
        return n;
    }

    memo[n] = fibonacciMemo(n - 1, memo) + fibonacciMemo(n - 2, memo);
    return memo[n];
}

// ====================
// 3. Array Recursion
// ====================

function sumArray(arr) {
    // Sum all elements recursively
    if (arr.length === 0) {
        return 0;
    }
    return arr[0] + sumArray(arr.slice(1));
}

function arrayLength(arr) {
    // Calculate length recursively
    if (arr.length === 0) {
        return 0;
    }
    return 1 + arrayLength(arr.slice(1));
}

function reverseArray(arr) {
    // Reverse array recursively
    if (arr.length === 0) {
        return [];
    }
    return [...reverseArray(arr.slice(1)), arr[0]];
}

function maxElement(arr) {
    // Find maximum element recursively
    if (arr.length === 1) {
        return arr[0];
    }

    const restMax = maxElement(arr.slice(1));
    return arr[0] > restMax ? arr[0] : restMax;
}

// ====================
// 4. String Recursion
// ====================

function reverseString(s) {
    // Reverse string recursively
    if (s.length <= 1) {
        return s;
    }
    return reverseString(s.slice(1)) + s[0];
}

function isPalindrome(s) {
    // Check if string is palindrome recursively
    s = s.replace(/\s/g, '').toLowerCase();

    if (s.length <= 1) {
        return true;
    }

    if (s[0] !== s[s.length - 1]) {
        return false;
    }

    return isPalindrome(s.slice(1, -1));
}

// ====================
// 5. Binary Search
// ====================

function binarySearch(arr, target, low = 0, high = arr.length - 1) {
    // Binary search recursively
    if (low > high) {
        return -1;  // Not found
    }

    const mid = Math.floor((low + high) / 2);

    if (arr[mid] === target) {
        return mid;
    } else if (arr[mid] > target) {
        return binarySearch(arr, target, low, mid - 1);
    } else {
        return binarySearch(arr, target, mid + 1, high);
    }
}

// ====================
// 6. Quicksort
// ====================

function quicksort(arr) {
    // Sort array using quicksort
    if (arr.length <= 1) {
        return arr;
    }

    const pivot = arr[Math.floor(arr.length / 2)];
    const left = arr.filter(x => x < pivot);
    const middle = arr.filter(x => x === pivot);
    const right = arr.filter(x => x > pivot);

    return [...quicksort(left), ...middle, ...quicksort(right)];
}

// ====================
// 7. Tower of Hanoi
// ====================

function hanoi(n, source, target, auxiliary, moves = []) {
    // Solve Tower of Hanoi puzzle
    if (n === 1) {
        moves.push(`Move disk 1 from ${source} to ${target}`);
        return moves;
    }

    // Move n-1 disks from source to auxiliary
    hanoi(n - 1, source, auxiliary, target, moves);

    // Move largest disk from source to target
    moves.push(`Move disk ${n} from ${source} to ${target}`);

    // Move n-1 disks from auxiliary to target
    hanoi(n - 1, auxiliary, target, source, moves);

    return moves;
}

// ====================
// 8. Tree Traversal
// ====================

class TreeNode {
    constructor(value, left = null, right = null) {
        this.value = value;
        this.left = left;
        this.right = right;
    }
}

function treeHeight(node) {
    // Calculate height of tree recursively
    if (node === null) {
        return 0;
    }

    const leftHeight = treeHeight(node.left);
    const rightHeight = treeHeight(node.right);

    return 1 + Math.max(leftHeight, rightHeight);
}

function treeSum(node) {
    // Sum all values in tree recursively
    if (node === null) {
        return 0;
    }

    return node.value + treeSum(node.left) + treeSum(node.right);
}

function inorderTraversal(node, result = []) {
    // In-order: left, root, right
    if (node === null) {
        return result;
    }

    inorderTraversal(node.left, result);
    result.push(node.value);
    inorderTraversal(node.right, result);

    return result;
}

function preorderTraversal(node, result = []) {
    // Pre-order: root, left, right
    if (node === null) {
        return result;
    }

    result.push(node.value);
    preorderTraversal(node.left, result);
    preorderTraversal(node.right, result);

    return result;
}

function postorderTraversal(node, result = []) {
    // Post-order: left, right, root
    if (node === null) {
        return result;
    }

    postorderTraversal(node.left, result);
    postorderTraversal(node.right, result);
    result.push(node.value);

    return result;
}

// ====================
// 9. Mutual Recursion
// ====================

function isEven(n) {
    // Check if even using mutual recursion
    if (n === 0) {
        return true;
    }
    return isOdd(n - 1);
}

function isOdd(n) {
    // Check if odd using mutual recursion
    if (n === 0) {
        return false;
    }
    return isEven(n - 1);
}

// ====================
// 10. Greatest Common Divisor
// ====================

function gcd(a, b) {
    // GCD using Euclid's algorithm
    if (b === 0) {
        return a;
    }
    return gcd(b, a % b);
}

// ====================
// Tests and Examples
// ====================

function main() {
    console.log("=== Recursion Examples in JavaScript ===\n");

    // Factorial
    console.log("1. Factorial:");
    console.log(`   factorial(5) = ${factorial(5)}`);
    console.log(`   factorialTail(5) = ${factorialTail(5)}`);

    // Fibonacci
    console.log("\n2. Fibonacci:");
    console.log(`   fibonacci(10) = ${fibonacci(10)}`);
    console.log(`   fibonacciMemo(30) = ${fibonacciMemo(30)}`);

    // Array operations
    console.log("\n3. Array Operations:");
    const numbers = [1, 2, 3, 4, 5];
    console.log(`   sumArray(${JSON.stringify(numbers)}) = ${sumArray(numbers)}`);
    console.log(`   arrayLength(${JSON.stringify(numbers)}) = ${arrayLength(numbers)}`);
    console.log(`   reverseArray(${JSON.stringify(numbers)}) = ${JSON.stringify(reverseArray(numbers))}`);
    console.log(`   maxElement(${JSON.stringify(numbers)}) = ${maxElement(numbers)}`);

    // String operations
    console.log("\n4. String Operations:");
    console.log(`   reverseString('hello') = ${reverseString('hello')}`);
    console.log(`   isPalindrome('racecar') = ${isPalindrome('racecar')}`);
    console.log(`   isPalindrome('A man a plan a canal Panama') = ${isPalindrome('A man a plan a canal Panama')}`);

    // Binary search
    console.log("\n5. Binary Search:");
    const sortedArr = [1, 3, 5, 7, 9, 11, 13, 15];
    console.log(`   binarySearch(${JSON.stringify(sortedArr)}, 7) = ${binarySearch(sortedArr, 7)}`);
    console.log(`   binarySearch(${JSON.stringify(sortedArr)}, 4) = ${binarySearch(sortedArr, 4)}`);

    // Quicksort
    console.log("\n6. Quicksort:");
    const unsorted = [3, 6, 8, 10, 1, 2, 1];
    console.log(`   quicksort(${JSON.stringify(unsorted)}) = ${JSON.stringify(quicksort(unsorted))}`);

    // Tower of Hanoi
    console.log("\n7. Tower of Hanoi (3 disks):");
    const moves = hanoi(3, 'A', 'C', 'B');
    moves.forEach(move => console.log(`   ${move}`));

    // Tree operations
    console.log("\n8. Binary Tree:");
    //       5
    //      / \
    //     3   8
    //    / \   \
    //   1   4   9
    const tree = new TreeNode(5,
        new TreeNode(3,
            new TreeNode(1),
            new TreeNode(4)),
        new TreeNode(8,
            null,
            new TreeNode(9)));

    console.log(`   treeHeight() = ${treeHeight(tree)}`);
    console.log(`   treeSum() = ${treeSum(tree)}`);
    console.log(`   inorderTraversal() = ${JSON.stringify(inorderTraversal(tree))}`);
    console.log(`   preorderTraversal() = ${JSON.stringify(preorderTraversal(tree))}`);
    console.log(`   postorderTraversal() = ${JSON.stringify(postorderTraversal(tree))}`);

    // Mutual recursion
    console.log("\n9. Mutual Recursion:");
    console.log(`   isEven(10) = ${isEven(10)}`);
    console.log(`   isOdd(10) = ${isOdd(10)}`);
    console.log(`   isEven(7) = ${isEven(7)}`);
    console.log(`   isOdd(7) = ${isOdd(7)}`);

    // GCD
    console.log("\n10. Greatest Common Divisor:");
    console.log(`   gcd(48, 18) = ${gcd(48, 18)}`);
    console.log(`   gcd(100, 35) = ${gcd(100, 35)}`);
}

if (require.main === module) {
    main();
}

module.exports = {
    factorial,
    factorialTail,
    fibonacci,
    fibonacciMemo,
    sumArray,
    reverseArray,
    binarySearch,
    quicksort,
    TreeNode,
    treeHeight,
    gcd
};
