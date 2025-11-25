# C Module Exercises

## Warmup Exercises

### Exercise C.W1: Hello Variants
Write C programs that:
1. Print "Hello, World!" 5 times using a loop
2. Print the numbers 1 to 10, one per line
3. Print a simple multiplication table (1-5)

### Exercise C.W2: Basic Functions
```c
// Implement these functions:

// Return the larger of two integers
int max(int a, int b);

// Return the absolute value of an integer
int abs_val(int x);

// Return 1 if n is even, 0 if odd
int is_even(int n);
```

### Exercise C.W3: Arrays
```c
// Write a program that:
// 1. Creates an array of 10 integers
// 2. Fills it with values 1-10
// 3. Prints all values
// 4. Prints the sum of all values
```

---

## Standard Exercises

### Exercise C.S1: Pointers
```c
// 1. Swap two integers using pointers
void swap(int *a, int *b);
// Test: swap x=5 and y=10, verify x=10, y=5

// 2. Find min and max in array, return via pointers
void find_min_max(int *arr, int n, int *min, int *max);

// 3. Reverse an array in place using pointers
void reverse(int *arr, int n);
```

### Exercise C.S2: String Functions
Without using string.h, implement:
```c
// Return the length of a string
int my_strlen(const char *s);

// Copy src to dest
void my_strcpy(char *dest, const char *src);

// Compare two strings (return <0, 0, or >0)
int my_strcmp(const char *s1, const char *s2);

// Find first occurrence of c in s (return pointer or NULL)
char *my_strchr(const char *s, char c);
```

### Exercise C.S3: Memory Allocation
```c
// 1. Create and return a dynamic array of n integers, all set to 0
int *create_zeros(int n);

// 2. Create a copy of an array on the heap
int *array_copy(const int *arr, int n);

// 3. Concatenate two arrays into a new array
int *array_concat(const int *a, int na, const int *b, int nb);
// Remember: caller must free the result!
```

### Exercise C.S4: Structs
```c
typedef struct {
    double x;
    double y;
} Point;

// Create a point on the heap
Point *point_create(double x, double y);

// Free a point
void point_free(Point *p);

// Calculate distance between two points
double point_distance(const Point *p1, const Point *p2);

// Move a point by (dx, dy)
void point_translate(Point *p, double dx, double dy);

// Create a midpoint between two points (on heap)
Point *point_midpoint(const Point *p1, const Point *p2);
```

### Exercise C.S5: File I/O
```c
// 1. Count lines in a file
int count_lines(const char *filename);

// 2. Count words in a file
int count_words(const char *filename);

// 3. Copy a file
int copy_file(const char *src, const char *dest);
// Return 0 on success, -1 on error
```

---

## Advanced Exercises

### Exercise C.A1: Linked List
Implement a singly linked list:
```c
typedef struct Node {
    int data;
    struct Node *next;
} Node;

typedef struct {
    Node *head;
    int size;
} List;

// Create an empty list
List *list_create(void);

// Free all memory
void list_free(List *list);

// Add to front
void list_prepend(List *list, int value);

// Add to back
void list_append(List *list, int value);

// Remove first occurrence of value
int list_remove(List *list, int value);

// Get element at index (return pointer or NULL)
int *list_get(List *list, int index);

// Print all elements
void list_print(const List *list);

// Reverse the list in place
void list_reverse(List *list);
```

### Exercise C.A2: Dynamic Array
Implement a resizable array (like C++ vector):
```c
typedef struct {
    int *data;
    int size;      // Number of elements
    int capacity;  // Allocated capacity
} DynArray;

DynArray *dynarray_create(int initial_capacity);
void dynarray_free(DynArray *arr);
void dynarray_push(DynArray *arr, int value);
int dynarray_pop(DynArray *arr);
int dynarray_get(DynArray *arr, int index);
void dynarray_set(DynArray *arr, int index, int value);
// Double capacity when full
```

### Exercise C.A3: Hash Table
Implement a simple hash table with string keys and int values:
```c
typedef struct Entry {
    char *key;
    int value;
    struct Entry *next;  // For chaining
} Entry;

typedef struct {
    Entry **buckets;
    int num_buckets;
    int size;
} HashTable;

HashTable *hashtable_create(int num_buckets);
void hashtable_free(HashTable *ht);
void hashtable_put(HashTable *ht, const char *key, int value);
int *hashtable_get(HashTable *ht, const char *key);  // NULL if not found
int hashtable_remove(HashTable *ht, const char *key);
```

### Exercise C.A4: Binary Tree
Implement a binary search tree:
```c
typedef struct TreeNode {
    int data;
    struct TreeNode *left;
    struct TreeNode *right;
} TreeNode;

TreeNode *tree_create(int data);
void tree_free(TreeNode *root);
void tree_insert(TreeNode **root, int data);
TreeNode *tree_find(TreeNode *root, int data);
void tree_inorder(TreeNode *root);  // Print in order
int tree_height(TreeNode *root);
```

### Exercise C.A5: Memory Debugger
Create a simple wrapper around malloc/free that tracks allocations:
```c
// Track allocations
void *debug_malloc(size_t size, const char *file, int line);
void debug_free(void *ptr, const char *file, int line);
void debug_report(void);  // Print any leaks

// Usage:
#define malloc(size) debug_malloc(size, __FILE__, __LINE__)
#define free(ptr) debug_free(ptr, __FILE__, __LINE__)
```

---

## Challenge Exercises

### Exercise C.C1: Memory Pool
Implement a simple memory pool allocator:
```c
typedef struct {
    char *pool;
    size_t size;
    size_t used;
} MemPool;

MemPool *pool_create(size_t size);
void *pool_alloc(MemPool *pool, size_t size);
void pool_reset(MemPool *pool);  // Reset to empty (fast)
void pool_destroy(MemPool *pool);
```

### Exercise C.C2: String Builder
Implement an efficient string builder:
```c
typedef struct {
    char *data;
    size_t length;
    size_t capacity;
} StringBuilder;

StringBuilder *sb_create(void);
void sb_free(StringBuilder *sb);
void sb_append(StringBuilder *sb, const char *str);
void sb_append_char(StringBuilder *sb, char c);
char *sb_to_string(StringBuilder *sb);  // Caller frees
void sb_clear(StringBuilder *sb);
```

### Exercise C.C3: Sort Comparison
Implement and compare:
```c
void bubble_sort(int *arr, int n);
void insertion_sort(int *arr, int n);
void quicksort(int *arr, int n);
void mergesort(int *arr, int n);

// Test with arrays of size 1000, 10000, 100000
// Measure and compare execution times
```

---

## Debugging Exercise

### Exercise C.D1: Find the Bugs
This code has 5 bugs. Find and fix them:
```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char *duplicate(char *s) {
    char *result = malloc(strlen(s));
    strcpy(result, s);
    return result;
}

int sum_array(int arr[], int n) {
    int sum;
    for (int i = 0; i <= n; i++) {
        sum += arr[i];
    }
    return sum;
}

void use_after_free() {
    int *p = malloc(sizeof(int));
    *p = 42;
    free(p);
    printf("%d\n", *p);
}

int main() {
    char *s = duplicate("hello");
    printf("%s\n", s);

    int arr[] = {1, 2, 3, 4, 5};
    printf("Sum: %d\n", sum_array(arr, 5));

    return 0;
}
```

---

## Reflection Questions

1. How does C's approach to memory management compare to Python's garbage collection?
2. What bugs can occur with manual memory management? How do you prevent them?
3. Why do you think C is still used despite being "unsafe"?
4. How does understanding C help you understand C++ better?
5. What would you miss most if you had to write only in C?
