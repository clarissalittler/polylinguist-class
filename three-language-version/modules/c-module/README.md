# C Module
## Systems Programming and Manual Memory Management

**Duration:** 1-2 weeks (Week 8)
**Prerequisites:** Lessons 1-7, especially Variables & Types and Data Structures
**Languages:** C

---

## Overview

C is one of the most influential programming languages ever created (1972). It's the language of operating systems, embedded systems, and high-performance software. This module introduces C to help you understand what higher-level languages (including C++) abstract away.

### Why Learn C?

1. **Foundation of systems**: Linux, Windows, and most OSes are written in C
2. **Understanding abstraction**: See what C++ does "under the hood"
3. **Direct hardware control**: Pointers, memory, no garbage collection
4. **Historical importance**: Most languages are influenced by C
5. **Performance critical**: When you need maximum speed

### What Makes C Different?

| Feature | Python/C++/Haskell | C |
|---------|-------------------|---|
| Memory | Managed | Manual (you allocate/free) |
| Types | Rich type systems | Basic types + pointers |
| Safety | Various guarantees | Very few (you're responsible) |
| Abstraction | High-level | Low-level |
| OOP | Supported | Not built-in |

---

## Installation

C compilers are usually already installed:

### Verify Installation
```bash
gcc --version
# or
clang --version
```

If not installed:
- **macOS**: `xcode-select --install`
- **Linux**: `sudo apt-get install build-essential`
- **Windows**: Install MinGW or use WSL

---

## Part 1: Basics (Day 1)

### Hello, World!

```c
#include <stdio.h>

int main(void) {
    printf("Hello, World!\n");
    return 0;
}
```

Compile and run:
```bash
gcc -o hello hello.c
./hello
```

### Structure of a C Program

```c
// Preprocessor directives (before compilation)
#include <stdio.h>   // Standard I/O library
#include <stdlib.h>  // Standard library (malloc, etc.)
#include <string.h>  // String functions

// Function declarations (prototypes)
int add(int a, int b);

// Main function - entry point
int main(void) {
    int result = add(3, 4);
    printf("Result: %d\n", result);
    return 0;  // 0 means success
}

// Function definitions
int add(int a, int b) {
    return a + b;
}
```

### Basic Types

```c
#include <stdio.h>

int main(void) {
    // Integer types
    char c = 'A';           // 1 byte
    short s = 32000;        // 2 bytes
    int i = 42;             // 4 bytes (usually)
    long l = 123456789L;    // 4-8 bytes
    long long ll = 9999999999LL;  // 8 bytes

    // Unsigned variants
    unsigned int ui = 42;
    unsigned char uc = 255;

    // Floating point
    float f = 3.14f;        // 4 bytes
    double d = 3.14159265;  // 8 bytes

    // Size check
    printf("int size: %zu bytes\n", sizeof(int));
    printf("double size: %zu bytes\n", sizeof(double));

    return 0;
}
```

### printf Formatting

```c
int i = 42;
double d = 3.14159;
char c = 'X';
char *s = "hello";

printf("Integer: %d\n", i);      // %d for int
printf("Float: %f\n", d);        // %f for double
printf("Scientific: %e\n", d);   // %e for scientific
printf("Char: %c\n", c);         // %c for char
printf("String: %s\n", s);       // %s for string
printf("Pointer: %p\n", &i);     // %p for pointer
printf("Hex: %x\n", 255);        // %x for hexadecimal
```

---

## Part 2: Pointers (Day 1-2)

### What Are Pointers?

A pointer is a variable that stores a **memory address**.

```c
#include <stdio.h>

int main(void) {
    int x = 42;      // A regular variable
    int *p = &x;     // A pointer to x (&x = address of x)

    printf("Value of x: %d\n", x);        // 42
    printf("Address of x: %p\n", &x);     // Some hex address
    printf("Value of p: %p\n", p);        // Same address
    printf("Value at p: %d\n", *p);       // 42 (*p = dereference)

    // Modify through pointer
    *p = 100;
    printf("New value of x: %d\n", x);    // 100

    return 0;
}
```

### Pointer Arithmetic

```c
int arr[] = {10, 20, 30, 40, 50};
int *p = arr;  // Points to first element

printf("%d\n", *p);       // 10
printf("%d\n", *(p + 1)); // 20
printf("%d\n", *(p + 2)); // 30

// p + 1 moves by sizeof(int) bytes, not 1 byte!
p++;  // Now points to arr[1]
printf("%d\n", *p);       // 20
```

### Arrays and Pointers

In C, arrays and pointers are closely related:

```c
int arr[5] = {1, 2, 3, 4, 5};

// These are equivalent:
arr[2]      // 3
*(arr + 2)  // 3

// Array name is a pointer to first element
int *p = arr;  // Same as &arr[0]

// But arrays are NOT pointers:
sizeof(arr)  // 20 (5 * 4 bytes)
sizeof(p)    // 8 (pointer size on 64-bit)
```

### Pass by Pointer

```c
#include <stdio.h>

// Pass by value - changes are local
void cannot_modify(int x) {
    x = 100;  // Only modifies local copy
}

// Pass by pointer - changes affect original
void can_modify(int *x) {
    *x = 100;  // Modifies original through pointer
}

int main(void) {
    int a = 42;
    cannot_modify(a);
    printf("a = %d\n", a);  // Still 42

    can_modify(&a);
    printf("a = %d\n", a);  // Now 100

    return 0;
}
```

---

## Part 3: Memory Management (Day 2-3)

### Stack vs Heap

```c
#include <stdio.h>
#include <stdlib.h>

int global_var = 100;  // Global/static segment

void function(void) {
    int stack_var = 42;            // Stack (automatic)
    int *heap_var = malloc(sizeof(int));  // Heap (manual)
    *heap_var = 99;

    // stack_var is automatically freed when function returns
    // heap_var MUST be freed manually!
    free(heap_var);
}
```

### malloc and free

```c
#include <stdio.h>
#include <stdlib.h>

int main(void) {
    // Allocate memory for one int
    int *p = malloc(sizeof(int));
    if (p == NULL) {
        printf("Allocation failed!\n");
        return 1;
    }
    *p = 42;
    printf("Value: %d\n", *p);
    free(p);  // MUST free when done

    // Allocate array of 10 ints
    int *arr = malloc(10 * sizeof(int));
    for (int i = 0; i < 10; i++) {
        arr[i] = i * i;
    }
    // Use the array...
    free(arr);  // Free the array

    return 0;
}
```

### Common Memory Bugs

```c
// 1. Memory leak - forgetting to free
int *leak(void) {
    int *p = malloc(sizeof(int));
    *p = 42;
    return p;  // Caller must free!
}
// If caller doesn't free, memory is leaked

// 2. Use after free
int *p = malloc(sizeof(int));
*p = 42;
free(p);
*p = 100;  // BUG! p is now invalid

// 3. Double free
int *p = malloc(sizeof(int));
free(p);
free(p);  // BUG! Double free

// 4. Buffer overflow
int arr[5];
arr[10] = 42;  // BUG! Writing past array bounds

// 5. Dangling pointer
int *dangling(void) {
    int x = 42;
    return &x;  // BUG! x doesn't exist after return
}
```

### Best Practices

```c
// 1. Always check malloc return value
int *p = malloc(sizeof(int));
if (p == NULL) {
    // Handle error
}

// 2. Set pointer to NULL after freeing
free(p);
p = NULL;

// 3. Use sizeof with the variable, not type
int *arr = malloc(n * sizeof(*arr));  // Better than sizeof(int)

// 4. Match every malloc with exactly one free
```

---

## Part 4: Strings in C (Day 3)

### C Strings Are Arrays

In C, strings are just arrays of characters ending with `\0` (null terminator):

```c
#include <stdio.h>
#include <string.h>

int main(void) {
    // These are equivalent:
    char s1[] = "hello";
    char s2[] = {'h', 'e', 'l', 'l', 'o', '\0'};

    // String length (not including \0)
    printf("Length: %zu\n", strlen(s1));  // 5

    // But array size includes \0
    printf("Size: %zu\n", sizeof(s1));    // 6

    // Access characters
    printf("First char: %c\n", s1[0]);    // 'h'

    return 0;
}
```

### String Functions

```c
#include <stdio.h>
#include <string.h>

int main(void) {
    char s1[100] = "Hello";
    char s2[] = " World";

    // Length
    size_t len = strlen(s1);  // 5

    // Copy
    char s3[100];
    strcpy(s3, s1);  // s3 = "Hello"

    // Concatenate
    strcat(s1, s2);  // s1 = "Hello World"

    // Compare
    int cmp = strcmp("abc", "abd");  // < 0 (abc < abd)

    // Find character
    char *pos = strchr(s1, 'o');  // Pointer to first 'o'

    // Find substring
    char *sub = strstr(s1, "World");  // Pointer to "World"

    return 0;
}
```

### Dynamic Strings

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char *create_greeting(const char *name) {
    // Calculate needed size
    size_t len = strlen("Hello, ") + strlen(name) + strlen("!") + 1;

    // Allocate
    char *result = malloc(len);
    if (result == NULL) return NULL;

    // Build string
    strcpy(result, "Hello, ");
    strcat(result, name);
    strcat(result, "!");

    return result;  // Caller must free!
}

int main(void) {
    char *greeting = create_greeting("Alice");
    printf("%s\n", greeting);  // "Hello, Alice!"
    free(greeting);
    return 0;
}
```

---

## Part 5: Structs (Day 3-4)

### Defining Structs

```c
#include <stdio.h>

// Define a struct
struct Point {
    int x;
    int y;
};

int main(void) {
    // Declare and initialize
    struct Point p1 = {10, 20};

    // Access members
    printf("x = %d, y = %d\n", p1.x, p1.y);

    // Modify members
    p1.x = 100;

    return 0;
}
```

### Typedef for Cleaner Syntax

```c
typedef struct {
    int x;
    int y;
} Point;

// Now use without 'struct' keyword
Point p1 = {10, 20};
```

### Structs and Pointers

```c
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    char *name;
    int age;
} Person;

int main(void) {
    // Struct on stack
    Person p1 = {"Alice", 30};
    printf("Name: %s\n", p1.name);

    // Pointer to struct
    Person *p2 = &p1;
    printf("Name: %s\n", (*p2).name);  // Dereference then access
    printf("Name: %s\n", p2->name);    // Arrow operator (cleaner)

    // Struct on heap
    Person *p3 = malloc(sizeof(Person));
    p3->name = "Bob";
    p3->age = 25;
    // Use p3...
    free(p3);

    return 0;
}
```

---

## Part 6: C vs C++ Comparison

### What C++ Adds

| Feature | C | C++ |
|---------|---|-----|
| Memory | malloc/free | new/delete, smart pointers |
| Strings | char arrays | std::string |
| Arrays | raw arrays | std::vector |
| OOP | Manual (structs + functions) | Classes, inheritance |
| Polymorphism | Function pointers | Virtual functions |
| Templates | None | Yes |
| Exceptions | None (error codes) | try/catch |
| RAII | Manual | Automatic (destructors) |

### Example: Dynamic Array

**C:**
```c
int *arr = malloc(10 * sizeof(int));
// Use array...
// Must remember to:
free(arr);
```

**C++:**
```cpp
std::vector<int> arr(10);
// Use vector...
// Automatically freed when out of scope!
```

---

## Exercises

### Exercise C1: Pointer Basics
```c
// 1. Swap two integers using pointers
void swap(int *a, int *b);

// 2. Return the maximum via pointer
void find_max(int *arr, int n, int *max);

// 3. Reverse an array in place
void reverse(int *arr, int n);
```

### Exercise C2: Memory Allocation
```c
// 1. Create a dynamic array of n integers initialized to 0
int *create_array(int n);

// 2. Resize an array (like realloc)
int *resize_array(int *arr, int old_size, int new_size);

// 3. Create a deep copy of an array
int *copy_array(int *arr, int n);
```

### Exercise C3: Strings
```c
// 1. Count occurrences of a character
int count_char(const char *s, char c);

// 2. Reverse a string in place
void reverse_string(char *s);

// 3. Check if string is palindrome
int is_palindrome(const char *s);
```

### Exercise C4: Structs
```c
typedef struct {
    char *name;
    int age;
} Person;

// 1. Create a Person on the heap
Person *person_create(const char *name, int age);

// 2. Free a Person
void person_free(Person *p);

// 3. Print a Person
void person_print(const Person *p);
```

### Exercise C5: Linked List
Implement a singly linked list:
```c
typedef struct Node {
    int data;
    struct Node *next;
} Node;

Node *list_create(int data);
void list_append(Node *head, int data);
void list_print(Node *head);
void list_free(Node *head);
int list_length(Node *head);
```

---

## Key Takeaways

1. **Pointers are addresses** - `*p` dereferences, `&x` gets address
2. **Manual memory management** - malloc allocates, free deallocates
3. **Arrays decay to pointers** - But they're not the same thing
4. **Strings are null-terminated char arrays**
5. **C trusts you** - No bounds checking, no type safety at runtime

---

## Resources

- **The C Programming Language** (K&R) - The classic
- **Modern C** by Jens Gustedt (free online)
- **C Programming Wikibook**: https://en.wikibooks.org/wiki/C_Programming
- **Compiler Explorer**: https://godbolt.org/ (see assembly!)

---

## Why This Matters

Understanding C helps you:
- Know what high-level languages do for you
- Debug memory issues in any language
- Write performance-critical code
- Understand operating systems
- Interface with hardware and low-level APIs

After this module, you'll appreciate C++'s abstractions more—and know when to bypass them.
