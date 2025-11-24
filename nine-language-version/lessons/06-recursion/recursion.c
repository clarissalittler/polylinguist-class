/*
 * Lesson 6: Recursion in C
 *
 * C supports recursion but has limited stack space (typically 1-8 MB).
 * No tail call optimization in most C compilers. Be careful with deep recursion.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// ====================
// 1. Simple Recursion
// ====================

unsigned long factorial(int n) {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}

unsigned long factorial_tail_helper(int n, unsigned long acc) {
    if (n <= 1) return acc;
    return factorial_tail_helper(n - 1, n * acc);
}

unsigned long factorial_tail(int n) {
    return factorial_tail_helper(n, 1);
}

// ====================
// 2. Fibonacci
// ====================

unsigned long fibonacci(int n) {
    if (n <= 1) return n;
    return fibonacci(n - 1) + fibonacci(n - 2);
}

// ====================
// 3. Array Recursion
// ====================

int sum_array(int arr[], int len) {
    if (len == 0) return 0;
    return arr[0] + sum_array(arr + 1, len - 1);
}

int max_element(int arr[], int len) {
    if (len == 1) return arr[0];
    int rest_max = max_element(arr + 1, len - 1);
    return (arr[0] > rest_max) ? arr[0] : rest_max;
}

// ====================
// 4. String Recursion
// ====================

void reverse_string_helper(char* str, int start, int end) {
    if (start >= end) return;

    // Swap characters
    char temp = str[start];
    str[start] = str[end];
    str[end] = temp;

    reverse_string_helper(str, start + 1, end - 1);
}

void reverse_string(char* str) {
    reverse_string_helper(str, 0, strlen(str) - 1);
}

int is_palindrome_helper(const char* str, int start, int end) {
    if (start >= end) return 1;
    if (str[start] != str[end]) return 0;
    return is_palindrome_helper(str, start + 1, end - 1);
}

int is_palindrome(const char* str) {
    return is_palindrome_helper(str, 0, strlen(str) - 1);
}

// ====================
// 5. Binary Search
// ====================

int binary_search(int arr[], int target, int low, int high) {
    if (low > high) return -1;

    int mid = (low + high) / 2;

    if (arr[mid] == target) return mid;
    if (arr[mid] > target) return binary_search(arr, target, low, mid - 1);
    return binary_search(arr, target, mid + 1, high);
}

// ====================
// 6. Tower of Hanoi
// ====================

void hanoi(int n, char source, char target, char auxiliary) {
    if (n == 1) {
        printf("   Move disk 1 from %c to %c\n", source, target);
        return;
    }

    hanoi(n - 1, source, auxiliary, target);
    printf("   Move disk %d from %c to %c\n", n, source, target);
    hanoi(n - 1, auxiliary, target, source);
}

// ====================
// 7. Tree Structure
// ====================

typedef struct TreeNode {
    int value;
    struct TreeNode* left;
    struct TreeNode* right;
} TreeNode;

TreeNode* create_node(int value) {
    TreeNode* node = (TreeNode*)malloc(sizeof(TreeNode));
    node->value = value;
    node->left = NULL;
    node->right = NULL;
    return node;
}

int tree_height(TreeNode* node) {
    if (node == NULL) return 0;

    int left_height = tree_height(node->left);
    int right_height = tree_height(node->right);

    return 1 + (left_height > right_height ? left_height : right_height);
}

int tree_sum(TreeNode* node) {
    if (node == NULL) return 0;
    return node->value + tree_sum(node->left) + tree_sum(node->right);
}

void free_tree(TreeNode* node) {
    if (node == NULL) return;
    free_tree(node->left);
    free_tree(node->right);
    free(node);
}

// ====================
// 8. Mutual Recursion
// ====================

int is_odd(int n);  // Forward declaration

int is_even(int n) {
    if (n == 0) return 1;
    return is_odd(n - 1);
}

int is_odd(int n) {
    if (n == 0) return 0;
    return is_even(n - 1);
}

// ====================
// 9. GCD
// ====================

int gcd(int a, int b) {
    if (b == 0) return a;
    return gcd(b, a % b);
}

// ====================
// Tests
// ====================

int main() {
    printf("=== Recursion Examples in C ===\n\n");

    // Factorial
    printf("1. Factorial:\n");
    printf("   factorial(5) = %lu\n", factorial(5));
    printf("   factorial_tail(5) = %lu\n", factorial_tail(5));

    // Fibonacci
    printf("\n2. Fibonacci:\n");
    printf("   fibonacci(10) = %lu\n", fibonacci(10));

    // Array operations
    printf("\n3. Array Operations:\n");
    int numbers[] = {1, 2, 3, 4, 5};
    int len = sizeof(numbers) / sizeof(numbers[0]);
    printf("   sum_array([1,2,3,4,5]) = %d\n", sum_array(numbers, len));
    printf("   max_element([1,2,3,4,5]) = %d\n", max_element(numbers, len));

    // String operations
    printf("\n4. String Operations:\n");
    char str1[] = "hello";
    reverse_string(str1);
    printf("   reverse_string('hello') = %s\n", str1);

    char str2[] = "racecar";
    printf("   is_palindrome('racecar') = %d\n", is_palindrome(str2));

    // Binary search
    printf("\n5. Binary Search:\n");
    int sorted[] = {1, 3, 5, 7, 9, 11, 13, 15};
    int sorted_len = sizeof(sorted) / sizeof(sorted[0]);
    printf("   binary_search([...], 7) = %d\n",
           binary_search(sorted, 7, 0, sorted_len - 1));
    printf("   binary_search([...], 4) = %d\n",
           binary_search(sorted, 4, 0, sorted_len - 1));

    // Tower of Hanoi
    printf("\n6. Tower of Hanoi (3 disks):\n");
    hanoi(3, 'A', 'C', 'B');

    // Tree
    printf("\n7. Binary Tree:\n");
    TreeNode* tree = create_node(5);
    tree->left = create_node(3);
    tree->left->left = create_node(1);
    tree->left->right = create_node(4);
    tree->right = create_node(8);
    tree->right->right = create_node(9);

    printf("   tree_height() = %d\n", tree_height(tree));
    printf("   tree_sum() = %d\n", tree_sum(tree));

    free_tree(tree);

    // Mutual recursion
    printf("\n8. Mutual Recursion:\n");
    printf("   is_even(10) = %d\n", is_even(10));
    printf("   is_odd(10) = %d\n", is_odd(10));
    printf("   is_even(7) = %d\n", is_even(7));
    printf("   is_odd(7) = %d\n", is_odd(7));

    // GCD
    printf("\n9. Greatest Common Divisor:\n");
    printf("   gcd(48, 18) = %d\n", gcd(48, 18));
    printf("   gcd(100, 35) = %d\n", gcd(100, 35));

    return 0;
}
