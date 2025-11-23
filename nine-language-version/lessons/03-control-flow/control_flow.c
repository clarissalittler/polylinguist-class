/*
 * Lesson 3: Control Flow in C
 * Demonstrates conditionals, loops, and boolean logic
 */

#include <stdio.h>
#include <stdbool.h>

int main() {
    printf("=== C Control Flow ===\n\n");

    // 1. Basic conditionals
    printf("1. Basic Conditionals:\n");
    int age = 20;
    if (age >= 18) {
        printf("  Age %d: Adult\n", age);
    } else if (age >= 13) {
        printf("  Age %d: Teenager\n", age);
    } else {
        printf("  Age %d: Child\n", age);
    }

    // Ternary expression
    char* status = (age >= 18) ? "Adult" : "Minor";
    printf("  Status: %s\n", status);

    // 2. For loops
    printf("\n2. For Loops:\n");
    printf("  Count to 5:\n   ");
    for (int i = 0; i < 5; i++) {
        printf(" %d", i);
    }
    printf("\n");

    printf("  Iterate array:\n");
    char* fruits[] = {"apple", "banana", "cherry"};
    int num_fruits = 3;
    for (int i = 0; i < num_fruits; i++) {
        printf("    %s\n", fruits[i]);
    }

    // 3. While loop
    printf("\n3. While Loop:\n");
    int count = 0;
    while (count < 3) {
        printf("  Count: %d\n", count);
        count++;
    }

    // 4. Boolean logic
    printf("\n4. Boolean Logic:\n");
    int x = 5, y = 10;
    printf("  x=%d, y=%d\n", x, y);
    printf("  x > 3 && y < 20: %d\n", x > 3 && y < 20);
    printf("  x > 10 || y > 5: %d\n", x > 10 || y > 5);
    printf("  !(x == y): %d\n", !(x == y));

    printf("\n  Truthiness (0 = false, non-zero = true):\n");
    int test_values[] = {0, 1, -1, 42};
    for (int i = 0; i < 4; i++) {
        int val = test_values[i];
        char* truthy = val ? "truthy" : "falsy";
        printf("    %d: %s\n", val, truthy);
    }

    // 5. FizzBuzz
    printf("\n5. FizzBuzz (1-20):\n ");
    for (int i = 1; i <= 20; i++) {
        if (i % 15 == 0) {
            printf(" FizzBuzz");
        } else if (i % 3 == 0) {
            printf(" Fizz");
        } else if (i % 5 == 0) {
            printf(" Buzz");
        } else {
            printf(" %d", i);
        }
    }
    printf("\n");

    // 6. Switch statement
    printf("\n6. Switch Statement:\n");
    int day = 3; // Wednesday
    char* day_type;
    switch (day) {
        case 0:
        case 6:
            day_type = "Weekend";
            break;
        case 1:
        case 2:
        case 3:
        case 4:
        case 5:
            day_type = "Weekday";
            break;
        default:
            day_type = "Invalid day";
    }
    printf("  Day %d: %s\n", day, day_type);

    // 7. Do-while loop
    printf("\n7. Do-While Loop:\n");
    int i = 0;
    do {
        printf("  Iteration: %d\n", i);
        i++;
    } while (i < 3);

    return 0;
}
