/*
 * Lesson 3: Control Flow in C++
 * Demonstrates conditionals, loops, and boolean logic
 */

#include <iostream>
#include <string>
#include <vector>
#include <iomanip>

// Helper function for FizzBuzz
std::string fizzbuzz(int n) {
    if (n % 15 == 0) {
        return "FizzBuzz";
    } else if (n % 3 == 0) {
        return "Fizz";
    } else if (n % 5 == 0) {
        return "Buzz";
    } else {
        return std::to_string(n);
    }
}

// Helper function for letter grade
char letterGrade(int score) {
    if (score >= 90) {
        return 'A';
    } else if (score >= 80) {
        return 'B';
    } else if (score >= 70) {
        return 'C';
    } else if (score >= 60) {
        return 'D';
    } else {
        return 'F';
    }
}

int main() {
    std::cout << "=== C++ Control Flow ===" << std::endl << std::endl;

    // 1. Basic conditionals
    std::cout << "1. Basic Conditionals:" << std::endl;
    int age = 20;
    if (age >= 18) {
        std::cout << "  Age " << age << ": Adult" << std::endl;
    } else if (age >= 13) {
        std::cout << "  Age " << age << ": Teenager" << std::endl;
    } else {
        std::cout << "  Age " << age << ": Child" << std::endl;
    }

    // Ternary expression
    std::string status = (age >= 18) ? "Adult" : "Minor";
    std::cout << "  Status (ternary): " << status << std::endl;

    // C++17: if with initializer
    if (int score = 95; score >= 90) {
        std::cout << "  High score: " << score << " (C++17 feature)" << std::endl;
    }

    // 2. For loops
    std::cout << std::endl << "2. For Loops:" << std::endl;
    std::cout << "  Count to 5:" << std::endl << "   ";
    for (int i = 0; i < 5; i++) {
        std::cout << " " << i;
    }
    std::cout << std::endl;

    std::cout << "  Iterate vector (range-based for):" << std::endl;
    std::vector<std::string> fruits = {"apple", "banana", "cherry"};
    for (const auto& fruit : fruits) {
        std::cout << "    " << fruit << std::endl;
    }

    std::cout << "  With index (traditional for):" << std::endl;
    for (size_t i = 0; i < fruits.size(); i++) {
        std::cout << "    " << i << ": " << fruits[i] << std::endl;
    }

    // 3. While loop
    std::cout << std::endl << "3. While Loop:" << std::endl;
    int count = 0;
    while (count < 3) {
        std::cout << "  Count: " << count << std::endl;
        count++;
    }

    // 4. Do-While loop
    std::cout << std::endl << "4. Do-While Loop (executes at least once):" << std::endl;
    int i = 0;
    do {
        std::cout << "  Iteration: " << i << std::endl;
        i++;
    } while (i < 3);

    // 5. Boolean logic
    std::cout << std::endl << "5. Boolean Logic:" << std::endl;
    int x = 5, y = 10;
    std::cout << "  x=" << x << ", y=" << y << std::endl;
    std::cout << std::boolalpha;  // Print bool as true/false
    std::cout << "  x > 3 && y < 20: " << (x > 3 && y < 20) << std::endl;
    std::cout << "  x > 10 || y > 5: " << (x > 10 || y > 5) << std::endl;
    std::cout << "  !(x == y): " << !(x == y) << std::endl;

    std::cout << std::endl << "  Truthiness (0 = false, non-zero = true):" << std::endl;
    int test_values[] = {0, 1, -1, 42};
    for (int val : test_values) {
        std::string truthy = val ? "truthy" : "falsy";
        std::cout << "    " << std::setw(3) << val << ": " << truthy << std::endl;
    }

    std::cout << std::endl << "  Pointer truthiness:" << std::endl;
    int* ptr = nullptr;
    if (ptr == nullptr) {
        std::cout << "    nullptr is falsy" << std::endl;
    }
    int value = 42;
    ptr = &value;
    if (ptr != nullptr) {
        std::cout << "    Valid pointer is truthy: " << *ptr << std::endl;
    }

    // 6. FizzBuzz
    std::cout << std::endl << "6. FizzBuzz (1-20):" << std::endl << " ";
    for (int i = 1; i <= 20; i++) {
        std::cout << " " << fizzbuzz(i);
    }
    std::cout << std::endl;

    // 7. Switch statement
    std::cout << std::endl << "7. Switch Statement:" << std::endl;
    int day = 3; // Wednesday
    std::string dayType;
    switch (day) {
        case 0:
        case 6:
            dayType = "Weekend";
            break;
        case 1:
        case 2:
        case 3:
        case 4:
        case 5:
            dayType = "Weekday";
            break;
        default:
            dayType = "Invalid day";
    }
    std::cout << "  Day " << day << ": " << dayType << std::endl;

    // C++17: switch with initializer
    switch (int result = 42; result % 10) {
        case 0:
            std::cout << "  Result ends in 0" << std::endl;
            break;
        case 2:
            std::cout << "  Result " << result << " ends in 2 (C++17)" << std::endl;
            break;
        default:
            std::cout << "  Result ends in " << (result % 10) << std::endl;
    }

    // 8. Break and continue
    std::cout << std::endl << "8. Break and Continue:" << std::endl;
    std::cout << "  Finding first even number:" << std::endl;
    for (int i = 0; i < 10; i++) {
        if (i % 2 == 0) {
            std::cout << "    Found: " << i << std::endl;
            break;
        }
    }

    std::cout << "  Skipping odd numbers:" << std::endl << "   ";
    for (int i = 0; i < 10; i++) {
        if (i % 2 != 0) {
            continue;
        }
        std::cout << " " << i;
    }
    std::cout << std::endl;

    // 9. Multiple conditions (Grade Calculator)
    std::cout << std::endl << "9. Multiple Conditions (Grade Calculator):" << std::endl;
    int scores[] = {95, 85, 75, 65, 55};
    for (int score : scores) {
        std::cout << "  Score " << score << ": Grade " << letterGrade(score) << std::endl;
    }

    // 10. Nested loops (Multiplication Table)
    std::cout << std::endl << "10. Nested Loops (Multiplication Table for 5):" << std::endl;
    int n = 5;
    for (int i = 1; i <= 5; i++) {
        std::cout << "  " << n << " x " << i << " = " << (n * i) << std::endl;
    }

    // 11. Range-based iteration with structured bindings (C++17)
    std::cout << std::endl << "11. Structured Bindings (C++17):" << std::endl;
    std::vector<std::pair<std::string, int>> items = {
        {"apple", 3},
        {"banana", 5},
        {"cherry", 2}
    };

    for (const auto& [name, count] : items) {
        std::cout << "  " << name << ": " << count << std::endl;
    }

    return 0;
}
