/*
 * Lesson 5: Data Structures in C
 * Demonstrates arrays and manual memory management
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Simple struct for key-value pairs
typedef struct {
    char key[50];
    char value[50];
} KeyValue;

int main() {
    printf("=== C Data Structures ===\n\n");

    // 1. Fixed-size arrays
    printf("1. Fixed-Size Arrays:\n");
    int numbers[5] = {1, 2, 3, 4, 5};
    printf("  Array: ");
    for (int i = 0; i < 5; i++) {
        printf("%d ", numbers[i]);
    }
    printf("\n");

    // Modify
    numbers[0] = 10;
    printf("  After numbers[0] = 10: ");
    for (int i = 0; i < 5; i++) {
        printf("%d ", numbers[i]);
    }
    printf("\n");

    // 2. Dynamic arrays (manual memory management)
    printf("\n2. Dynamic Arrays (malloc):\n");
    int *dynamic_array = (int*)malloc(5 * sizeof(int));
    for (int i = 0; i < 5; i++) {
        dynamic_array[i] = i + 1;
    }

    printf("  Dynamic array: ");
    for (int i = 0; i < 5; i++) {
        printf("%d ", dynamic_array[i]);
    }
    printf("\n");

    // Resize (manual realloc)
    dynamic_array = (int*)realloc(dynamic_array, 6 * sizeof(int));
    dynamic_array[5] = 6;
    printf("  After realloc and adding 6: ");
    for (int i = 0; i < 6; i++) {
        printf("%d ", dynamic_array[i]);
    }
    printf("\n");

    free(dynamic_array);  // Manual cleanup!

    // 3. Strings (arrays of chars)
    printf("\n3. Strings (Character Arrays):\n");
    char greeting[] = "Hello";
    printf("  String: %s\n", greeting);
    printf("  Length: %lu\n", strlen(greeting));

    // 4. Structs (composite data)
    printf("\n4. Structs (Composite Data):\n");
    typedef struct {
        char name[50];
        int age;
        char city[50];
    } Person;

    Person alice = {"Alice", 30, "NYC"};
    printf("  Person: %s, %d, %s\n", alice.name, alice.age, alice.city);

    // Modify
    alice.age = 31;
    strcpy(alice.city, "LA");
    printf("  Modified: %s, %d, %s\n", alice.name, alice.age, alice.city);

    // 5. Simple "map" using array of structs
    printf("\n5. Simple Map (Array of KeyValue):\n");
    KeyValue map[3] = {
        {"name", "Bob"},
        {"age", "25"},
        {"city", "SF"}
    };

    printf("  Map entries:\n");
    for (int i = 0; i < 3; i++) {
        printf("    %s: %s\n", map[i].key, map[i].value);
    }

    // 6. Multi-dimensional arrays
    printf("\n6. Multi-Dimensional Arrays:\n");
    int matrix[3][3] = {
        {1, 2, 3},
        {4, 5, 6},
        {7, 8, 9}
    };

    printf("  Matrix:\n");
    for (int i = 0; i < 3; i++) {
        printf("    ");
        for (int j = 0; j < 3; j++) {
            printf("%d ", matrix[i][j]);
        }
        printf("\n");
    }

    // 7. Key insights
    printf("\n7. Key Insights:\n");
    printf("  - Fixed-size arrays at compile time\n");
    printf("  - Manual memory management (malloc/free)\n");
    printf("  - No built-in dynamic resizing\n");
    printf("  - No bounds checking (unsafe!)\n");
    printf("  - Everything is mutable\n");
    printf("  - Direct memory control = fast but dangerous\n");

    return 0;
}
