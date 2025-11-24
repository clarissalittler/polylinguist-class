/*
 * Lesson 8: Higher-Order Functions in C
 *
 * C has limited functional programming support:
 * - Function pointers allow passing functions
 * - No closures (but can simulate with structs)
 * - No built-in map/filter/reduce
 * - Manual memory management
 *
 * This demonstrates the low-level mechanics of HOFs!
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// ====================
// 1. Function Pointers - Passing Functions
// ====================

// Function type definitions
typedef int (*UnaryIntFunc)(int);
typedef int (*BinaryIntFunc)(int, int);

// Example functions
int add_one(int x) {
    return x + 1;
}

int double_it(int x) {
    return x * 2;
}

int square_it(int x) {
    return x * x;
}

// ====================
// 2. Functions Taking Functions
// ====================

int apply_twice(UnaryIntFunc func, int x) {
    return func(func(x));
}

int apply_n_times(UnaryIntFunc func, int n, int x) {
    int result = x;
    for (int i = 0; i < n; i++) {
        result = func(result);
    }
    return result;
}

// ====================
// 3. "Closures" with Structs
// ====================

// Struct to hold function + captured data
typedef struct {
    int n;
} MultiplierContext;

int multiplier_func(void* context, int x) {
    MultiplierContext* ctx = (MultiplierContext*)context;
    return x * ctx->n;
}

typedef struct {
    int (*func)(void*, int);
    void* context;
} Closure;

Closure make_multiplier(int n) {
    MultiplierContext* ctx = malloc(sizeof(MultiplierContext));
    ctx->n = n;

    Closure closure = {multiplier_func, ctx};
    return closure;
}

int call_closure(Closure* closure, int x) {
    return closure->func(closure->context, x);
}

void free_closure(Closure* closure) {
    free(closure->context);
}

// ====================
// 4. Map - Manual Implementation
// ====================

void map_int_array(int* arr, int size, UnaryIntFunc func) {
    for (int i = 0; i < size; i++) {
        arr[i] = func(arr[i]);
    }
}

int* map_int_array_new(int* arr, int size, UnaryIntFunc func) {
    int* result = malloc(size * sizeof(int));
    for (int i = 0; i < size; i++) {
        result[i] = func(arr[i]);
    }
    return result;
}

// ====================
// 5. Filter - Manual Implementation
// ====================

typedef int (*Predicate)(int);

int is_even(int x) {
    return x % 2 == 0;
}

int is_positive(int x) {
    return x > 0;
}

int filter_int_array(int* arr, int size, Predicate pred, int* out) {
    int count = 0;
    for (int i = 0; i < size; i++) {
        if (pred(arr[i])) {
            out[count++] = arr[i];
        }
    }
    return count;
}

// ====================
// 6. Reduce (Fold) - Manual Implementation
// ====================

int add(int a, int b) {
    return a + b;
}

int multiply(int a, int b) {
    return a * b;
}

int max_int(int a, int b) {
    return a > b ? a : b;
}

int reduce_int_array(int* arr, int size, BinaryIntFunc func, int initial) {
    int result = initial;
    for (int i = 0; i < size; i++) {
        result = func(result, arr[i]);
    }
    return result;
}

// ====================
// 7. Function Composition
// ====================

typedef struct {
    UnaryIntFunc f;
    UnaryIntFunc g;
} ComposedFunc;

int composed_apply(void* context, int x) {
    ComposedFunc* funcs = (ComposedFunc*)context;
    return funcs->f(funcs->g(x));
}

Closure compose(UnaryIntFunc f, UnaryIntFunc g) {
    ComposedFunc* funcs = malloc(sizeof(ComposedFunc));
    funcs->f = f;
    funcs->g = g;

    Closure closure = {composed_apply, funcs};
    return closure;
}

// ====================
// 8. Sorting with Compare Function (qsort)
// ====================

int compare_ascending(const void* a, const void* b) {
    return (*(int*)a - *(int*)b);
}

int compare_descending(const void* a, const void* b) {
    return (*(int*)b - *(int*)a);
}

int compare_by_abs(const void* a, const void* b) {
    int abs_a = abs(*(int*)a);
    int abs_b = abs(*(int*)b);
    return abs_a - abs_b;
}

// ====================
// 9. Callback Pattern
// ====================

typedef void (*Callback)(int);

void for_each(int* arr, int size, Callback callback) {
    for (int i = 0; i < size; i++) {
        callback(arr[i]);
    }
}

void print_number(int x) {
    printf("%d ", x);
}

void print_square(int x) {
    printf("%d ", x * x);
}

// ====================
// 10. Simulating Closures with Counter
// ====================

typedef struct {
    int count;
} CounterState;

int counter_increment(void* state) {
    CounterState* s = (CounterState*)state;
    s->count++;
    return s->count;
}

typedef struct {
    int (*func)(void*);
    void* state;
} Counter;

Counter make_counter() {
    CounterState* state = malloc(sizeof(CounterState));
    state->count = 0;

    Counter counter = {counter_increment, state};
    return counter;
}

int call_counter(Counter* counter) {
    return counter->func(counter->state);
}

void free_counter(Counter* counter) {
    free(counter->state);
}

// ====================
// Utility Functions
// ====================

void print_int_array(int* arr, int size) {
    printf("[");
    for (int i = 0; i < size; i++) {
        printf("%d", arr[i]);
        if (i < size - 1) printf(", ");
    }
    printf("]");
}

// ====================
// Main Demonstration
// ====================

int main() {
    printf("=== Higher-Order Functions in C ===\n\n");

    // 1. Function pointers
    printf("1. Function Pointers:\n");
    UnaryIntFunc ops[] = {add_one, double_it, square_it};
    for (int i = 0; i < 3; i++) {
        printf("   ops[%d](5) = %d\n", i, ops[i](5));
    }

    // 2. Functions taking functions
    printf("\n2. Functions Taking Functions:\n");
    printf("   apply_twice(add_one, 5) = %d\n", apply_twice(add_one, 5));
    printf("   apply_n_times(double_it, 3, 2) = %d\n", apply_n_times(double_it, 3, 2));

    // 3. "Closures" with structs
    printf("\n3. Simulated Closures:\n");
    Closure times_three = make_multiplier(3);
    Closure add_ten = make_multiplier(10);  // Reusing for addition
    printf("   times_three(7) = %d\n", call_closure(&times_three, 7));
    free_closure(&times_three);

    // 4. Map
    printf("\n4. Map - Transform Each Element:\n");
    int numbers[] = {1, 2, 3, 4, 5};
    int size = 5;

    int* doubled = map_int_array_new(numbers, size, double_it);
    printf("   Doubled: ");
    print_int_array(doubled, size);
    printf("\n");
    free(doubled);

    // 5. Filter
    printf("\n5. Filter - Select Elements:\n");
    int all_numbers[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    int all_size = 10;
    int evens[10];

    int evens_count = filter_int_array(all_numbers, all_size, is_even, evens);
    printf("   Evens: ");
    print_int_array(evens, evens_count);
    printf("\n");

    // 6. Reduce
    printf("\n6. Reduce - Combine to Single Value:\n");
    int sum = reduce_int_array(numbers, size, add, 0);
    printf("   Sum: %d\n", sum);

    int product = reduce_int_array(numbers, size, multiply, 1);
    printf("   Product: %d\n", product);

    int maximum = reduce_int_array(numbers, size, max_int, numbers[0]);
    printf("   Max: %d\n", maximum);

    // 7. Function composition
    printf("\n7. Function Composition:\n");
    Closure composed = compose(double_it, add_one);
    printf("   compose(double_it, add_one)(5) = %d\n", call_closure(&composed, 5));
    free_closure(&composed);

    // 8. qsort with compare functions
    printf("\n8. Sorting with Compare Functions (qsort):\n");
    int unsorted[] = {5, -2, 8, -1, 3};
    int sort_size = 5;

    // Ascending
    int ascending[5];
    memcpy(ascending, unsorted, sizeof(unsorted));
    qsort(ascending, sort_size, sizeof(int), compare_ascending);
    printf("   Ascending: ");
    print_int_array(ascending, sort_size);
    printf("\n");

    // By absolute value
    int by_abs[5];
    memcpy(by_abs, unsorted, sizeof(unsorted));
    qsort(by_abs, sort_size, sizeof(int), compare_by_abs);
    printf("   By abs value: ");
    print_int_array(by_abs, sort_size);
    printf("\n");

    // 9. Callbacks
    printf("\n9. Callbacks (for_each):\n");
    printf("   Print each: ");
    for_each(numbers, size, print_number);
    printf("\n");

    printf("   Print squares: ");
    for_each(numbers, size, print_square);
    printf("\n");

    // 10. Counter closure
    printf("\n10. Simulated Counter Closure:\n");
    Counter counter = make_counter();
    printf("   counter() = %d\n", call_counter(&counter));
    printf("   counter() = %d\n", call_counter(&counter));
    printf("   counter() = %d\n", call_counter(&counter));
    free_counter(&counter);

    printf("\n11. Key Limitations in C:\n");
    printf("   - No built-in map/filter/reduce\n");
    printf("   - No true closures (must simulate with structs)\n");
    printf("   - Function pointers don't capture environment\n");
    printf("   - Manual memory management required\n");
    printf("   - But: Understanding this shows what HOFs do under the hood!\n");

    return 0;
}
