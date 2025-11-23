// variables_types.c
#include <stdio.h>
#include <stdbool.h>

int main() {
    // Must declare type before use

    // Integers (various sizes)
    char small = 127;              // 8-bit integer
    short medium = 32000;          // 16-bit integer
    int age = 25;                  // 32-bit integer (usually)
    long big = 2147483647L;        // 64-bit integer

    // Floating point
    float price = 19.99f;          // 32-bit float
    double precise = 3.14159265359; // 64-bit float

    // Character
    char letter = 'A';             // single character

    // Boolean (need stdbool.h)
    bool is_student = true;

    // Strings (character arrays)
    char name[] = "Alice";

    // Cannot change type!
    // age = "text";  // ERROR: incompatible types

    printf("Age: %d\n", age);
    printf("Price: %.2f\n", price);
    printf("Name: %s\n", name);
    printf("Letter: %c\n", letter);
    printf("Is student: %d\n", is_student);

    return 0;
}
