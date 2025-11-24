/*
 * Lesson 4: Functions in C
 * Demonstrates function definition, parameters, function pointers
 * Note: C has limited support for functional programming concepts
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

// ============================================
// 1. Basic Function Definition
// ============================================

// Function prototypes (declarations)
char* greet(char* name);
int add(int x, int y);
int square(int x);

// Function definitions
char* greet(char* name) {
    static char buffer[100];
    snprintf(buffer, sizeof(buffer), "Hello, %s!", name);
    return buffer;
}

int add(int x, int y) {
    return x + y;
}

int square(int x) {
    return x * x;
}

// ============================================
// 2. Parameters
// ============================================

// No default parameters in C!
// Must use function overloading or multiple functions

int multiply(int x, int y) {
    return x * y;
}

// Variadic function (variable number of arguments)
int sum_all(int count, ...) {
    va_list args;
    va_start(args, count);

    int total = 0;
    for (int i = 0; i < count; i++) {
        total += va_arg(args, int);
    }

    va_end(args);
    return total;
}

// ============================================
// 3. Return Values
// ============================================

// C can only return one value
// Use pointers to return multiple values
int divide_with_error(int x, int y, double* result) {
    if (y == 0) {
        return -1;  // Error code
    }
    *result = (double)x / y;
    return 0;  // Success
}

// Void function (no return value)
void print_message(char* message) {
    printf("%s\n", message);
}

// ============================================
// 4. Function Pointers (C's "first-class" functions)
// ============================================

// Function pointer type
typedef int (*operation_t)(int, int);

int apply_operation(operation_t op, int x, int y) {
    return op(x, y);
}

// ============================================
// 5. Pure vs Impure Functions
// ============================================

// Pure function
int pure_add(int x, int y) {
    return x + y;
}

// Impure function (side effect - I/O)
void impure_print(char* message) {
    printf("%s\n", message);
}

// Impure function (depends on/modifies global state)
int total = 0;
int impure_add_to_total(int x) {
    total += x;
    return total;
}

// Impure function (non-deterministic)
int impure_random() {
    return rand() % 100;
}

// ============================================
// 6. Higher-Order Functions (limited)
// ============================================

int apply_twice(int (*f)(int), int x) {
    return f(f(x));
}

int apply_n_times(int (*f)(int), int x, int n) {
    int result = x;
    for (int i = 0; i < n; i++) {
        result = f(result);
    }
    return result;
}

int double_value(int x) {
    return x * 2;
}

// ============================================
// 7. Simulating Closures (very limited)
// ============================================

// C doesn't have closures, but we can simulate with structs

typedef struct {
    int factor;
} Multiplier;

int apply_multiplier(Multiplier* m, int x) {
    return x * m->factor;
}

Multiplier* make_multiplier(int factor) {
    Multiplier* m = malloc(sizeof(Multiplier));
    m->factor = factor;
    return m;
}

// ============================================
// Main Program
// ============================================

int main() {
    printf("=== C Functions ===\n\n");

    // 1. Basic functions
    printf("1. Basic Functions:\n");
    printf("  greet('Alice'): %s\n", greet("Alice"));
    printf("  add(5, 3): %d\n", add(5, 3));
    printf("  square(7): %d\n", square(7));
    printf("  multiply(4, 5): %d\n", multiply(4, 5));

    // 2. Variadic function
    printf("\n2. Variadic Function:\n");
    printf("  sum_all(5, 1, 2, 3, 4, 5): %d\n", sum_all(5, 1, 2, 3, 4, 5));

    // 3. Multiple "returns" via pointers
    printf("\n3. Multiple Return Values (via pointers):\n");
    double result;
    int error = divide_with_error(10, 2, &result);
    printf("  divide_with_error(10, 2): result=%.2f, error=%d\n", result, error);

    error = divide_with_error(10, 0, &result);
    printf("  divide_with_error(10, 0): result=%.2f, error=%d\n", result, error);

    // 4. Function pointers
    printf("\n4. Function Pointers (First-Class-like):\n");
    printf("  apply_operation(add, 5, 3): %d\n", apply_operation(add, 5, 3));
    printf("  apply_operation(multiply, 5, 3): %d\n", apply_operation(multiply, 5, 3));

    // 5. Pure vs Impure
    printf("\n5. Pure vs Impure:\n");
    printf("  pure_add(5, 3): %d\n", pure_add(5, 3));
    printf("  impure_print('Hello'): ");
    impure_print("Hello");
    total = 0;  // Reset
    printf("  impure_add_to_total(5): %d\n", impure_add_to_total(5));
    printf("  impure_add_to_total(5): %d (different!)\n", impure_add_to_total(5));

    // 6. Higher-order functions (limited)
    printf("\n6. Higher-Order Functions:\n");
    printf("  apply_twice(double_value, 5): %d\n", apply_twice(double_value, 5));
    printf("  apply_n_times(double_value, 5, 3): %d\n", apply_n_times(double_value, 5, 3));

    // 7. Simulating closures
    printf("\n7. Simulating Closures (with structs):\n");
    Multiplier* times_two = make_multiplier(2);
    Multiplier* times_three = make_multiplier(3);
    printf("  times_two(5): %d\n", apply_multiplier(times_two, 5));
    printf("  times_three(5): %d\n", apply_multiplier(times_three, 5));

    // Clean up
    free(times_two);
    free(times_three);

    // 8. Array of function pointers
    printf("\n8. Array of Function Pointers:\n");
    operation_t operations[] = {add, multiply};
    char* op_names[] = {"add", "multiply"};

    for (int i = 0; i < 2; i++) {
        printf("  %s(5, 3): %d\n", op_names[i], operations[i](5, 3));
    }

    return 0;
}
