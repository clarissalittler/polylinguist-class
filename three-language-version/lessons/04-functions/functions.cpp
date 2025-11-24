/*
 * Lesson 4: Functions in C++
 * Demonstrates function definition, parameters, closures, lambdas, and templates
 */

#include <iostream>
#include <string>
#include <functional>
#include <vector>
#include <algorithm>
#include <numeric>
#include <optional>

// ============================================
// 1. Basic Function Definition
// ============================================

std::string greet(const std::string& name) {
    return "Hello, " + name + "!";
}

int add(int x, int y) {
    return x + y;
}

int square(int x) {
    return x * x;
}

// ============================================
// 2. Function Overloading
// ============================================

// Same name, different parameter types
double add(double x, double y) {
    return x + y;
}

// Different number of parameters
int add(int x, int y, int z) {
    return x + y + z;
}

// ============================================
// 3. Default Parameters
// ============================================

std::string describePerson(const std::string& name, int age, const std::string& city = "Unknown") {
    return name + " is " + std::to_string(age) + " years old and lives in " + city;
}

double power(double base, int exponent = 2) {
    double result = 1.0;
    for (int i = 0; i < exponent; i++) {
        result *= base;
    }
    return result;
}

// ============================================
// 4. Return Values
// ============================================

// Using std::optional for potentially missing values
std::optional<double> divide(double x, double y) {
    if (y == 0) {
        return std::nullopt;  // Represents "no value"
    }
    return x / y;
}

// Multiple returns using std::pair
std::pair<std::optional<double>, std::optional<std::string>>
divideWithError(double x, double y) {
    if (y == 0) {
        return {std::nullopt, "Cannot divide by zero"};
    }
    return {x / y, std::nullopt};
}

// Void function (no return value)
void printGreeting(const std::string& name) {
    std::cout << "Hello, " << name << "!" << std::endl;
}

// ============================================
// 5. Function Pointers and std::function
// ============================================

// Function pointer
int applyOperationPtr(int (*operation)(int, int), int x, int y) {
    return operation(x, y);
}

// std::function (more flexible)
int applyOperation(std::function<int(int, int)> operation, int x, int y) {
    return operation(x, y);
}

// ============================================
// 6. Lambdas (Anonymous Functions)
// ============================================

void demonstrateLambdas() {
    std::cout << "\n6. Lambda Functions:" << std::endl;

    // Simple lambda
    auto doubleVal = [](int x) { return x * 2; };
    std::cout << "  doubleVal(5): " << doubleVal(5) << std::endl;

    // Lambda with capture
    int factor = 3;
    auto multiplyByFactor = [factor](int x) { return x * factor; };
    std::cout << "  multiplyByFactor(5): " << multiplyByFactor(5) << std::endl;

    // Capture by reference
    int total = 0;
    auto addToTotal = [&total](int x) { total += x; };
    addToTotal(5);
    addToTotal(10);
    std::cout << "  total after additions: " << total << std::endl;

    // Generic lambda (C++14)
    auto square = [](auto x) { return x * x; };
    std::cout << "  square(7): " << square(7) << std::endl;
    std::cout << "  square(3.5): " << square(3.5) << std::endl;
}

// ============================================
// 7. Closures
// ============================================

std::function<int(int)> makeMultiplier(int factor) {
    // Lambda captures 'factor' by value, creating a closure
    return [factor](int x) { return x * factor; };
}

std::function<int()> makeCounter() {
    // Shared state captured by reference
    auto count = std::make_shared<int>(0);
    return [count]() {
        (*count)++;
        return *count;
    };
}

// ============================================
// 8. Function Templates (Generic Functions)
// ============================================

template<typename T>
T genericAdd(T x, T y) {
    return x + y;
}

template<typename T, typename Func>
T applyTwice(Func f, T x) {
    return f(f(x));
}

template<typename T, typename Func>
T applyNTimes(Func f, T x, int n) {
    T result = x;
    for (int i = 0; i < n; i++) {
        result = f(result);
    }
    return result;
}

// ============================================
// 9. Function Composition
// ============================================

template<typename F, typename G>
auto compose(F f, G g) {
    return [f, g](auto x) { return f(g(x)); };
}

// ============================================
// 10. Higher-Order Functions on Collections
// ============================================

void demonstrateHigherOrder() {
    std::cout << "\n10. Higher-Order Functions on Collections:" << std::endl;

    std::vector<int> numbers = {1, 2, 3, 4, 5};
    std::cout << "  Original: ";
    for (int n : numbers) std::cout << n << " ";
    std::cout << std::endl;

    // Map (transform)
    std::vector<int> squared(numbers.size());
    std::transform(numbers.begin(), numbers.end(), squared.begin(),
                   [](int x) { return x * x; });
    std::cout << "  Squared: ";
    for (int n : squared) std::cout << n << " ";
    std::cout << std::endl;

    // Filter (copy_if)
    std::vector<int> evens;
    std::copy_if(numbers.begin(), numbers.end(), std::back_inserter(evens),
                 [](int x) { return x % 2 == 0; });
    std::cout << "  Evens: ";
    for (int n : evens) std::cout << n << " ";
    std::cout << std::endl;

    // Reduce (accumulate)
    int sum = std::accumulate(numbers.begin(), numbers.end(), 0);
    std::cout << "  Sum: " << sum << std::endl;

    // Reduce with custom operation
    int product = std::accumulate(numbers.begin(), numbers.end(), 1,
                                   [](int acc, int x) { return acc * x; });
    std::cout << "  Product: " << product << std::endl;
}

// ============================================
// 11. Pure vs Impure Functions
// ============================================

// Pure function
int pureAdd(int x, int y) {
    return x + y;  // Deterministic, no side effects
}

// Impure function (side effect)
void impurePrint(const std::string& message) {
    std::cout << message << std::endl;  // I/O is a side effect
}

// Impure function (modifies external state)
int total = 0;
int impureAddToTotal(int x) {
    total += x;  // Modifies global variable
    return total;
}

// ============================================
// Main Program
// ============================================

int main() {
    std::cout << "=== C++ Functions ===" << std::endl << std::endl;

    // 1. Basic functions
    std::cout << "1. Basic Functions:" << std::endl;
    std::cout << "  greet(\"Alice\"): " << greet("Alice") << std::endl;
    std::cout << "  add(5, 3): " << add(5, 3) << std::endl;
    std::cout << "  square(7): " << square(7) << std::endl;

    // 2. Function overloading
    std::cout << "\n2. Function Overloading:" << std::endl;
    std::cout << "  add(5, 3): " << add(5, 3) << std::endl;
    std::cout << "  add(5.5, 3.2): " << add(5.5, 3.2) << std::endl;
    std::cout << "  add(1, 2, 3): " << add(1, 2, 3) << std::endl;

    // 3. Default parameters
    std::cout << "\n3. Default Parameters:" << std::endl;
    std::cout << "  describePerson(\"Alice\", 30): "
              << describePerson("Alice", 30) << std::endl;
    std::cout << "  describePerson(\"Bob\", 25, \"NYC\"): "
              << describePerson("Bob", 25, "NYC") << std::endl;
    std::cout << "  power(5.0): " << power(5.0) << std::endl;
    std::cout << "  power(5.0, 3): " << power(5.0, 3) << std::endl;

    // 4. Optional return values
    std::cout << "\n4. Optional Return Values:" << std::endl;
    auto result1 = divide(10.0, 2.0);
    if (result1) {
        std::cout << "  divide(10, 2): " << *result1 << std::endl;
    }

    auto result2 = divide(10.0, 0.0);
    if (!result2) {
        std::cout << "  divide(10, 0): No value (division by zero)" << std::endl;
    }

    // 5. First-class functions
    std::cout << "\n5. First-Class Functions:" << std::endl;
    std::cout << "  applyOperation(add, 5, 3): "
              << applyOperation(add, 5, 3) << std::endl;

    auto multiply = [](int x, int y) { return x * y; };
    std::cout << "  applyOperation(multiply, 5, 3): "
              << applyOperation(multiply, 5, 3) << std::endl;

    // 6. Lambdas
    demonstrateLambdas();

    // 7. Closures
    std::cout << "\n7. Closures:" << std::endl;
    auto timesTwo = makeMultiplier(2);
    auto timesThree = makeMultiplier(3);
    std::cout << "  timesTwo(5): " << timesTwo(5) << std::endl;
    std::cout << "  timesThree(5): " << timesThree(5) << std::endl;

    std::cout << "\n  Counter (stateful closure):" << std::endl;
    auto counter1 = makeCounter();
    auto counter2 = makeCounter();
    std::cout << "  counter1(): " << counter1() << std::endl;  // 1
    std::cout << "  counter1(): " << counter1() << std::endl;  // 2
    std::cout << "  counter2(): " << counter2() << std::endl;  // 1
    std::cout << "  counter1(): " << counter1() << std::endl;  // 3

    // 8. Templates
    std::cout << "\n8. Function Templates:" << std::endl;
    std::cout << "  genericAdd(5, 3): " << genericAdd(5, 3) << std::endl;
    std::cout << "  genericAdd(5.5, 3.2): " << genericAdd(5.5, 3.2) << std::endl;

    auto increment = [](int x) { return x + 1; };
    std::cout << "  applyTwice(increment, 5): "
              << applyTwice(increment, 5) << std::endl;

    auto doubleVal = [](int x) { return x * 2; };
    std::cout << "  applyNTimes(doubleVal, 2, 4): "
              << applyNTimes(doubleVal, 2, 4) << std::endl;

    // 9. Function composition
    std::cout << "\n9. Function Composition:" << std::endl;
    auto squareFunc = [](int x) { return x * x; };
    auto squareThenIncrement = compose(increment, squareFunc);
    std::cout << "  squareThenIncrement(5): "
              << squareThenIncrement(5) << std::endl;  // 26

    auto incrementThenSquare = compose(squareFunc, increment);
    std::cout << "  incrementThenSquare(5): "
              << incrementThenSquare(5) << std::endl;  // 36

    // 10. Higher-order functions
    demonstrateHigherOrder();

    // 11. Pure vs Impure
    std::cout << "\n11. Pure vs Impure Functions:" << std::endl;
    std::cout << "  pureAdd(5, 3): " << pureAdd(5, 3) << std::endl;
    std::cout << "  impurePrint(\"Hello\"): ";
    impurePrint("Hello");

    total = 0;  // Reset for demo
    std::cout << "  impureAddToTotal(5): " << impureAddToTotal(5) << std::endl;
    std::cout << "  impureAddToTotal(5): " << impureAddToTotal(5) << std::endl;  // Different!

    std::cout << "\n=== Summary ===" << std::endl;
    std::cout << "C++ functions support:" << std::endl;
    std::cout << "  - Function overloading (same name, different parameters)" << std::endl;
    std::cout << "  - Default parameters" << std::endl;
    std::cout << "  - Lambdas and closures (C++11+)" << std::endl;
    std::cout << "  - Templates for generic programming" << std::endl;
    std::cout << "  - std::function for type-erased function objects" << std::endl;
    std::cout << "  - First-class function support (modern C++)" << std::endl;

    return 0;
}
