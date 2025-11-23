// variables_types.cpp
#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <typeinfo>
#include <iomanip>

int main() {
    std::cout << "=== C++ Variables and Types Demo ===\n\n";

    // Numbers
    std::cout << "--- Numbers ---\n";
    char small = 127;              // 8-bit integer
    short medium = 32000;          // 16-bit integer
    int age = 25;                  // 32-bit integer (usually)
    long big = 2147483647L;        // 32 or 64-bit integer
    long long huge = 9223372036854775807LL; // 64-bit integer

    // Floating point
    float price = 19.99f;          // 32-bit float
    double precise = 3.14159265359; // 64-bit float

    std::cout << "Age: " << age << " (int)\n";
    std::cout << "Price: " << price << " (float)\n";
    std::cout << "Precise: " << std::setprecision(11) << precise << " (double)\n";
    std::cout << "Small: " << static_cast<int>(small) << " (char/8-bit)\n";
    std::cout << "Huge: " << huge << " (long long)\n";

    // Character and Strings
    std::cout << "\n--- Characters and Strings ---\n";
    char letter = 'A';             // single character
    std::string name = "Alice";    // C++ string class

    std::cout << "Letter: " << letter << " (char)\n";
    std::cout << "Name: " << name << " (std::string)\n";

    // Boolean
    std::cout << "\n--- Booleans ---\n";
    bool is_student = true;
    bool has_graduated = false;

    std::cout << std::boolalpha;  // Print booleans as true/false
    std::cout << "Is student: " << is_student << " (bool)\n";
    std::cout << "Has graduated: " << has_graduated << " (bool)\n";

    // Type inference with auto (C++11+)
    std::cout << "\n--- Type Inference (auto) ---\n";
    auto inferred_int = 42;        // int
    auto inferred_double = 3.14;   // double
    auto inferred_string = std::string("text"); // std::string

    std::cout << "Inferred int: " << inferred_int << "\n";
    std::cout << "Inferred double: " << inferred_double << "\n";
    std::cout << "Inferred string: " << inferred_string << "\n";

    // Constants
    std::cout << "\n--- Constants ---\n";
    const double PI = 3.14159265359;
    constexpr int MAX_SIZE = 100;  // Compile-time constant

    std::cout << "PI (const): " << PI << "\n";
    std::cout << "MAX_SIZE (constexpr): " << MAX_SIZE << "\n";

    // Cannot change const values:
    // PI = 3.14;  // Error: assignment of read-only variable

    // Type conversion
    std::cout << "\n--- Type Conversion ---\n";
    std::string num_str = "123";
    int num = std::stoi(num_str);  // String to int
    std::cout << "String \"" << num_str << "\" converted to int: " << num << "\n";
    std::cout << "Converted number + 1 = " << (num + 1) << "\n";

    std::string float_str = "3.14";
    double float_num = std::stod(float_str);  // String to double
    std::cout << "String \"" << float_str << "\" converted to double: " << float_num << "\n";

    // Int to string
    std::string str_from_int = std::to_string(456);
    std::cout << "Int 456 converted to string: \"" << str_from_int << "\"\n";

    // Casting
    double d = 3.99;
    int truncated = static_cast<int>(d);  // C++ style cast
    std::cout << "Double " << d << " cast to int: " << truncated << " (truncated)\n";

    // Collections
    std::cout << "\n--- Collections ---\n";

    // Vector (dynamic array)
    std::vector<int> numbers = {1, 2, 3, 4, 5};
    std::cout << "Vector: [";
    for (size_t i = 0; i < numbers.size(); ++i) {
        std::cout << numbers[i];
        if (i < numbers.size() - 1) std::cout << ", ";
    }
    std::cout << "]\n";

    // Map (key-value pairs)
    std::map<std::string, int> student;
    student["age"] = 20;
    student["grade"] = 85;
    std::cout << "Map: {age: " << student["age"]
              << ", grade: " << student["grade"] << "}\n";

    // Null pointer
    std::cout << "\n--- Null Pointer ---\n";
    int* ptr = nullptr;  // C++11 null pointer
    std::cout << "Null pointer: " << ptr << "\n";

    if (ptr == nullptr) {
        std::cout << "Pointer is null\n";
    }

    // Size information
    std::cout << "\n--- Type Sizes ---\n";
    std::cout << "sizeof(char): " << sizeof(char) << " byte(s)\n";
    std::cout << "sizeof(short): " << sizeof(short) << " byte(s)\n";
    std::cout << "sizeof(int): " << sizeof(int) << " byte(s)\n";
    std::cout << "sizeof(long): " << sizeof(long) << " byte(s)\n";
    std::cout << "sizeof(long long): " << sizeof(long long) << " byte(s)\n";
    std::cout << "sizeof(float): " << sizeof(float) << " byte(s)\n";
    std::cout << "sizeof(double): " << sizeof(double) << " byte(s)\n";

    std::cout << "\n--- Static Typing ---\n";
    std::cout << "C++ is statically typed - types are checked at compile time.\n";
    std::cout << "This prevents many type-related bugs before the program runs.\n";

    // This would cause a compile error:
    // age = "text";  // Error: invalid conversion from 'const char*' to 'int'

    return 0;
}
