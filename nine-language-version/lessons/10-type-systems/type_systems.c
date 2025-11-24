/*
 * Lesson 10: Type Systems in C
 *
 * C features:
 * - Static typing (types checked at compile time)
 * - Weak typing (lots of implicit conversions)
 * - Manual memory management
 * - No generics (use void* and casting)
 * - typedef for type aliases
 * - Structs and unions
 * - Pointer types
 *
 * This demonstrates C's type system.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <math.h>

// ====================
// 1. Basic Types
// ====================

int increment(int x) {
    return x + 1;
}

int add(int x, int y) {
    return x + y;
}

// ====================
// 2. Type Aliases (typedef)
// ====================

typedef int UserId;
typedef int ProductId;
typedef struct {
    double x;
    double y;
} Point;

// ====================
// 3. Structs (Product Types)
// ====================

typedef struct {
    char name[50];
    int age;
} Person;

bool is_adult(Person* p) {
    return p->age >= 18;
}

// ====================
// 4. Tagged Unions (Sum Types)
// ====================

typedef enum {
    SHAPE_CIRCLE,
    SHAPE_RECTANGLE
} ShapeType;

typedef struct {
    ShapeType type;
    union {
        struct { double radius; } circle;
        struct { double width; double height; } rectangle;
    } data;
} Shape;

double shape_area(const Shape* s) {
    switch (s->type) {
        case SHAPE_CIRCLE:
            return M_PI * s->data.circle.radius * s->data.circle.radius;
        case SHAPE_RECTANGLE:
            return s->data.rectangle.width * s->data.rectangle.height;
        default:
            return 0.0;
    }
}

// ====================
// 5. Pointers (Reference Types)
// ====================

void increment_by_value(int x) {
    x = x + 1;  // Doesn't affect caller
}

void increment_by_pointer(int* x) {
    *x = *x + 1;  // Modifies caller's value
}

// ====================
// 6. Generic Functions with void*
// ====================

// "Generic" identity (loses type safety!)
void* identity(void* x) {
    return x;
}

// Compare function pointer type for qsort
typedef int (*compare_func)(const void*, const void*);

int compare_ints(const void* a, const void* b) {
    int arg1 = *(const int*)a;
    int arg2 = *(const int*)b;
    return (arg1 > arg2) - (arg1 < arg2);
}

// ====================
// 7. Function Pointers
// ====================

typedef int (*binary_op)(int, int);

int apply_op(binary_op op, int x, int y) {
    return op(x, y);
}

// ====================
// 8. Enums (Limited Sum Types)
// ====================

typedef enum {
    RED,
    GREEN,
    BLUE
} Color;

const char* color_name(Color c) {
    switch (c) {
        case RED: return "red";
        case GREEN: return "green";
        case BLUE: return "blue";
        default: return "unknown";
    }
}

// ====================
// 9. Type Casting (Weak Typing)
// ====================

void demonstrate_weak_typing() {
    printf("9. Weak Typing (Implicit Conversions):\n");

    // Integer to double
    int i = 5;
    double d = i;  // Implicit conversion
    printf("   int 5 -> double: %.1f\n", d);

    // Double to int (truncation)
    double pi = 3.14159;
    int truncated = (int)pi;  // Explicit cast
    printf("   double 3.14159 -> int: %d\n", truncated);

    // Pointer arithmetic
    int arr[] = {10, 20, 30};
    int* ptr = arr;
    printf("   *ptr = %d, *(ptr+1) = %d\n", *ptr, *(ptr+1));
}

// ====================
// 10. sizeof Operator (Type Information)
// ====================

void demonstrate_sizeof() {
    printf("\n10. sizeof (Type Sizes):\n");
    printf("   sizeof(char) = %zu bytes\n", sizeof(char));
    printf("   sizeof(int) = %zu bytes\n", sizeof(int));
    printf("   sizeof(double) = %zu bytes\n", sizeof(double));
    printf("   sizeof(Point) = %zu bytes\n", sizeof(Point));
    printf("   sizeof(void*) = %zu bytes\n", sizeof(void*));
}

// ====================
// Main Demonstration
// ====================

int main() {
    printf("=== Type Systems in C ===\n\n");

    // 1. Basic types
    printf("1. Basic Types:\n");
    printf("   increment(5) = %d\n", increment(5));
    printf("   add(3, 7) = %d\n", add(3, 7));

    // 2. Type aliases
    printf("\n2. Type Aliases (typedef):\n");
    UserId user_id = 123;
    printf("   UserId: %d\n", user_id);
    Point p = {3.0, 4.0};
    printf("   Point: (%.1f, %.1f)\n", p.x, p.y);

    // 3. Structs
    printf("\n3. Structs:\n");
    Person person = {"Alice", 30};
    printf("   Person{name='%s', age=%d}\n", person.name, person.age);
    printf("   is_adult() = %s\n", is_adult(&person) ? "true" : "false");

    // 4. Tagged unions
    printf("\n4. Tagged Unions:\n");
    Shape circle = {SHAPE_CIRCLE, .data.circle = {5.0}};
    Shape rect = {SHAPE_RECTANGLE, .data.rectangle = {4.0, 6.0}};
    printf("   Circle area = %.2f\n", shape_area(&circle));
    printf("   Rectangle area = %.2f\n", shape_area(&rect));

    // 5. Pointers
    printf("\n5. Pointers (Reference vs Value):\n");
    int x = 5;
    increment_by_value(x);
    printf("   After increment_by_value: x = %d\n", x);
    increment_by_pointer(&x);
    printf("   After increment_by_pointer: x = %d\n", x);

    // 6. Generic with void*
    printf("\n6. Generic Functions (void*):\n");
    int num = 42;
    int* result = (int*)identity(&num);
    printf("   identity(&42) = %d (type safety lost!)\n", *result);

    // 7. Function pointers
    printf("\n7. Function Pointers:\n");
    printf("   apply_op(add, 3, 7) = %d\n", apply_op(add, 3, 7));

    // 8. Enums
    printf("\n8. Enums:\n");
    Color c = RED;
    printf("   Color: %s\n", color_name(c));

    // 9. Weak typing
    demonstrate_weak_typing();

    // 10. sizeof
    demonstrate_sizeof();

    printf("\n=== C Type System Features ===\n");
    printf("- Static typing (compile-time checks)\n");
    printf("- Weak typing (many implicit conversions)\n");
    printf("- Type aliases with typedef\n");
    printf("- Structs (product types)\n");
    printf("- Tagged unions (manual sum types)\n");
    printf("- Pointers (manual memory management)\n");
    printf("- No generics (use void* with care)\n");
    printf("- Function pointers for callbacks\n");
    printf("- Fast but unsafe\n");

    return 0;
}
