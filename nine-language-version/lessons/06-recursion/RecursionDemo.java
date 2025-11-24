/**
 * Lesson 6: Recursion in Java
 *
 * Java supports recursion but has limited stack space.
 * No tail call optimization. Default stack size varies (~1 MB).
 */

import java.util.*;

public class RecursionDemo {

    // ====================
    // 1. Simple Recursion
    // ====================

    public static long factorial(int n) {
        if (n <= 1) return 1;
        return n * factorial(n - 1);
    }

    public static long factorialTail(int n) {
        return factorialTailHelper(n, 1);
    }

    private static long factorialTailHelper(int n, long acc) {
        if (n <= 1) return acc;
        return factorialTailHelper(n - 1, n * acc);
    }

    // ====================
    // 2. Fibonacci
    // ====================

    public static long fibonacci(int n) {
        if (n <= 1) return n;
        return fibonacci(n - 1) + fibonacci(n - 2);
    }

    public static long fibonacciMemo(int n, Map<Integer, Long> memo) {
        if (memo.containsKey(n)) {
            return memo.get(n);
        }

        long result;
        if (n <= 1) {
            result = n;
        } else {
            result = fibonacciMemo(n - 1, memo) + fibonacciMemo(n - 2, memo);
        }

        memo.put(n, result);
        return result;
    }

    // ====================
    // 3. Array Recursion
    // ====================

    public static int sumArray(int[] arr, int index) {
        if (index >= arr.length) return 0;
        return arr[index] + sumArray(arr, index + 1);
    }

    public static int maxElement(int[] arr, int index) {
        if (index == arr.length - 1) return arr[index];
        int restMax = maxElement(arr, index + 1);
        return Math.max(arr[index], restMax);
    }

    // ====================
    // 4. String Recursion
    // ====================

    public static String reverseString(String s) {
        if (s.length() <= 1) return s;
        return reverseString(s.substring(1)) + s.charAt(0);
    }

    public static boolean isPalindrome(String s) {
        // Remove spaces and convert to lowercase
        s = s.replaceAll("\\s+", "").toLowerCase();
        return checkPalindrome(s);
    }

    private static boolean checkPalindrome(String s) {
        if (s.length() <= 1) return true;
        if (s.charAt(0) != s.charAt(s.length() - 1)) return false;
        return checkPalindrome(s.substring(1, s.length() - 1));
    }

    // ====================
    // 5. Binary Search
    // ====================

    public static int binarySearch(int[] arr, int target, int low, int high) {
        if (low > high) return -1;

        int mid = (low + high) / 2;

        if (arr[mid] == target) return mid;
        if (arr[mid] > target) return binarySearch(arr, target, low, mid - 1);
        return binarySearch(arr, target, mid + 1, high);
    }

    // ====================
    // 6. Quicksort
    // ====================

    public static List<Integer> quicksort(List<Integer> arr) {
        if (arr.size() <= 1) return arr;

        int pivot = arr.get(arr.size() / 2);
        List<Integer> left = new ArrayList<>();
        List<Integer> middle = new ArrayList<>();
        List<Integer> right = new ArrayList<>();

        for (int x : arr) {
            if (x < pivot) left.add(x);
            else if (x == pivot) middle.add(x);
            else right.add(x);
        }

        List<Integer> result = new ArrayList<>();
        result.addAll(quicksort(left));
        result.addAll(middle);
        result.addAll(quicksort(right));
        return result;
    }

    // ====================
    // 7. Tower of Hanoi
    // ====================

    public static void hanoi(int n, char source, char target, char auxiliary) {
        if (n == 1) {
            System.out.println("   Move disk 1 from " + source + " to " + target);
            return;
        }

        hanoi(n - 1, source, auxiliary, target);
        System.out.println("   Move disk " + n + " from " + source + " to " + target);
        hanoi(n - 1, auxiliary, target, source);
    }

    // ====================
    // 8. Tree Traversal
    // ====================

    static class TreeNode {
        int value;
        TreeNode left;
        TreeNode right;

        TreeNode(int value) {
            this.value = value;
            this.left = null;
            this.right = null;
        }
    }

    public static int treeHeight(TreeNode node) {
        if (node == null) return 0;
        return 1 + Math.max(treeHeight(node.left), treeHeight(node.right));
    }

    public static int treeSum(TreeNode node) {
        if (node == null) return 0;
        return node.value + treeSum(node.left) + treeSum(node.right);
    }

    public static List<Integer> inorderTraversal(TreeNode node) {
        List<Integer> result = new ArrayList<>();
        inorderHelper(node, result);
        return result;
    }

    private static void inorderHelper(TreeNode node, List<Integer> result) {
        if (node == null) return;
        inorderHelper(node.left, result);
        result.add(node.value);
        inorderHelper(node.right, result);
    }

    // ====================
    // 9. Mutual Recursion
    // ====================

    public static boolean isEven(int n) {
        if (n == 0) return true;
        return isOdd(n - 1);
    }

    public static boolean isOdd(int n) {
        if (n == 0) return false;
        return isEven(n - 1);
    }

    // ====================
    // 10. GCD
    // ====================

    public static int gcd(int a, int b) {
        if (b == 0) return a;
        return gcd(b, a % b);
    }

    // ====================
    // Tests
    // ====================

    public static void main(String[] args) {
        System.out.println("=== Recursion Examples in Java ===\n");

        // Factorial
        System.out.println("1. Factorial:");
        System.out.println("   factorial(5) = " + factorial(5));
        System.out.println("   factorialTail(5) = " + factorialTail(5));

        // Fibonacci
        System.out.println("\n2. Fibonacci:");
        System.out.println("   fibonacci(10) = " + fibonacci(10));
        Map<Integer, Long> memo = new HashMap<>();
        System.out.println("   fibonacciMemo(30) = " + fibonacciMemo(30, memo));

        // Array operations
        System.out.println("\n3. Array Operations:");
        int[] numbers = {1, 2, 3, 4, 5};
        System.out.println("   sumArray([1,2,3,4,5]) = " + sumArray(numbers, 0));
        System.out.println("   maxElement([1,2,3,4,5]) = " + maxElement(numbers, 0));

        // String operations
        System.out.println("\n4. String Operations:");
        System.out.println("   reverseString('hello') = " + reverseString("hello"));
        System.out.println("   isPalindrome('racecar') = " + isPalindrome("racecar"));
        System.out.println("   isPalindrome('A man a plan a canal Panama') = " +
                           isPalindrome("A man a plan a canal Panama"));

        // Binary search
        System.out.println("\n5. Binary Search:");
        int[] sorted = {1, 3, 5, 7, 9, 11, 13, 15};
        System.out.println("   binarySearch([...], 7) = " +
                           binarySearch(sorted, 7, 0, sorted.length - 1));
        System.out.println("   binarySearch([...], 4) = " +
                           binarySearch(sorted, 4, 0, sorted.length - 1));

        // Quicksort
        System.out.println("\n6. Quicksort:");
        List<Integer> unsorted = Arrays.asList(3, 6, 8, 10, 1, 2, 1);
        System.out.println("   quicksort([3,6,8,10,1,2,1]) = " + quicksort(unsorted));

        // Tower of Hanoi
        System.out.println("\n7. Tower of Hanoi (3 disks):");
        hanoi(3, 'A', 'C', 'B');

        // Tree
        System.out.println("\n8. Binary Tree:");
        TreeNode tree = new TreeNode(5);
        tree.left = new TreeNode(3);
        tree.left.left = new TreeNode(1);
        tree.left.right = new TreeNode(4);
        tree.right = new TreeNode(8);
        tree.right.right = new TreeNode(9);

        System.out.println("   treeHeight() = " + treeHeight(tree));
        System.out.println("   treeSum() = " + treeSum(tree));
        System.out.println("   inorderTraversal() = " + inorderTraversal(tree));

        // Mutual recursion
        System.out.println("\n9. Mutual Recursion:");
        System.out.println("   isEven(10) = " + isEven(10));
        System.out.println("   isOdd(10) = " + isOdd(10));
        System.out.println("   isEven(7) = " + isEven(7));
        System.out.println("   isOdd(7) = " + isOdd(7));

        // GCD
        System.out.println("\n10. Greatest Common Divisor:");
        System.out.println("   gcd(48, 18) = " + gcd(48, 18));
        System.out.println("   gcd(100, 35) = " + gcd(100, 35));
    }
}
