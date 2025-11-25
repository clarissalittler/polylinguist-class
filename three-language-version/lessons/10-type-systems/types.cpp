/**
 * Lesson 10: Type Systems - C++ Examples
 * C++ has strong static typing with templates for generics
 */

#include <iostream>
#include <string>
#include <vector>
#include <optional>
#include <variant>
#include <functional>
#include <memory>
#include <type_traits>

// =============================================================================
// BASIC STATIC TYPING
// =============================================================================

void static_typing_demo() {
    int x = 42;           // x is always an int
    double y = 3.14;      // y is always a double
    std::string s = "hi"; // s is always a string

    // x = "hello";  // ERROR: cannot assign string to int
    // This is caught at compile time!

    std::cout << "x = " << x << ", y = " << y << ", s = " << s << std::endl;
}

// =============================================================================
// TYPE INFERENCE (auto)
// =============================================================================

void auto_demo() {
    auto i = 42;           // int
    auto d = 3.14;         // double
    auto s = std::string("hello");  // std::string
    auto v = std::vector<int>{1, 2, 3};  // std::vector<int>

    std::cout << "auto types: " << i << ", " << d << ", " << s << std::endl;
}

// =============================================================================
// TEMPLATES (Generics)
// =============================================================================

template<typename T>
T identity(T x) {
    return x;
}

template<typename T>
T max_of(T a, T b) {
    return (a > b) ? a : b;
}

template<typename T>
class Box {
private:
    T value;
public:
    Box(T v) : value(v) {}
    T get() const { return value; }

    template<typename F>
    Box<T> map(F f) const {
        return Box<T>(f(value));
    }
};

// =============================================================================
// CONSTEXPR - Compile-time Computation
// =============================================================================

constexpr int factorial(int n) {
    return (n <= 1) ? 1 : n * factorial(n - 1);
}

constexpr int fib(int n) {
    return (n <= 1) ? n : fib(n-1) + fib(n-2);
}

// These are computed at compile time!
constexpr int fact5 = factorial(5);   // 120
constexpr int fib10 = fib(10);        // 55

// =============================================================================
// TYPE ALIASES
// =============================================================================

using UserId = int;
using UserName = std::string;
using UserDatabase = std::vector<std::pair<UserId, UserName>>;

std::optional<UserName> find_user(const UserDatabase& db, UserId id) {
    for (const auto& [uid, name] : db) {
        if (uid == id) return name;
    }
    return std::nullopt;
}

// =============================================================================
// OPTIONAL TYPES
// =============================================================================

std::optional<double> safe_divide(double a, double b) {
    if (b == 0) return std::nullopt;
    return a / b;
}

std::optional<int> parse_int(const std::string& s) {
    try {
        return std::stoi(s);
    } catch (...) {
        return std::nullopt;
    }
}

// =============================================================================
// VARIANT (Sum Types / Tagged Union)
// =============================================================================

using Value = std::variant<int, double, std::string>;

std::string describe_value(const Value& v) {
    return std::visit([](auto&& arg) -> std::string {
        using T = std::decay_t<decltype(arg)>;
        if constexpr (std::is_same_v<T, int>) {
            return "int: " + std::to_string(arg);
        } else if constexpr (std::is_same_v<T, double>) {
            return "double: " + std::to_string(arg);
        } else {
            return "string: " + arg;
        }
    }, v);
}

// =============================================================================
// CONCEPTS (C++20) - Constraining Templates
// =============================================================================

#if __cplusplus >= 202002L
template<typename T>
concept Numeric = std::is_arithmetic_v<T>;

template<Numeric T>
T add(T a, T b) {
    return a + b;
}
#else
// Pre-C++20: use SFINAE or static_assert
template<typename T>
typename std::enable_if<std::is_arithmetic<T>::value, T>::type
add(T a, T b) {
    return a + b;
}
#endif

// =============================================================================
// TYPE TRAITS
// =============================================================================

template<typename T>
void describe_type() {
    std::cout << "Type properties:" << std::endl;
    std::cout << "  is_integral: " << std::is_integral<T>::value << std::endl;
    std::cout << "  is_floating_point: " << std::is_floating_point<T>::value << std::endl;
    std::cout << "  is_pointer: " << std::is_pointer<T>::value << std::endl;
    std::cout << "  is_class: " << std::is_class<T>::value << std::endl;
}

// =============================================================================
// ABSTRACT CLASSES (Interfaces)
// =============================================================================

class Shape {
public:
    virtual ~Shape() = default;
    virtual double area() const = 0;
    virtual double perimeter() const = 0;
};

class Circle : public Shape {
    double radius;
public:
    Circle(double r) : radius(r) {}
    double area() const override { return 3.14159 * radius * radius; }
    double perimeter() const override { return 2 * 3.14159 * radius; }
};

class Rectangle : public Shape {
    double width, height;
public:
    Rectangle(double w, double h) : width(w), height(h) {}
    double area() const override { return width * height; }
    double perimeter() const override { return 2 * (width + height); }
};

// =============================================================================
// FUNCTION TYPES
// =============================================================================

template<typename T>
T apply_twice(std::function<T(T)> f, T x) {
    return f(f(x));
}

// =============================================================================
// MAIN
// =============================================================================

int main() {
    std::cout << "=== Static Typing ===" << std::endl;
    static_typing_demo();

    std::cout << "\n=== Type Inference (auto) ===" << std::endl;
    auto_demo();

    std::cout << "\n=== Templates ===" << std::endl;
    std::cout << "identity(42) = " << identity(42) << std::endl;
    std::cout << "identity(\"hello\") = " << identity(std::string("hello")) << std::endl;
    std::cout << "max_of(3, 7) = " << max_of(3, 7) << std::endl;
    std::cout << "max_of(3.14, 2.71) = " << max_of(3.14, 2.71) << std::endl;

    std::cout << "\n=== Generic Box ===" << std::endl;
    Box<int> intBox(42);
    std::cout << "Box(42).get() = " << intBox.get() << std::endl;
    std::cout << "Box(42).map(x*2) = " << intBox.map([](int x) { return x * 2; }).get() << std::endl;

    std::cout << "\n=== Constexpr ===" << std::endl;
    std::cout << "factorial(5) = " << fact5 << " (computed at compile time)" << std::endl;
    std::cout << "fib(10) = " << fib10 << " (computed at compile time)" << std::endl;

    std::cout << "\n=== Optional ===" << std::endl;
    auto r1 = safe_divide(10, 2);
    auto r2 = safe_divide(10, 0);
    std::cout << "safe_divide(10, 2) = " << (r1 ? std::to_string(*r1) : "none") << std::endl;
    std::cout << "safe_divide(10, 0) = " << (r2 ? std::to_string(*r2) : "none") << std::endl;

    std::cout << "\n=== Type Aliases ===" << std::endl;
    UserDatabase db = {{1, "Alice"}, {2, "Bob"}};
    auto user = find_user(db, 1);
    std::cout << "find_user(db, 1) = " << user.value_or("not found") << std::endl;

    std::cout << "\n=== Variant ===" << std::endl;
    std::vector<Value> values = {42, 3.14, std::string("hello")};
    for (const auto& v : values) {
        std::cout << describe_value(v) << std::endl;
    }

    std::cout << "\n=== Type Traits (int) ===" << std::endl;
    describe_type<int>();

    std::cout << "\n=== Polymorphism ===" << std::endl;
    std::vector<std::unique_ptr<Shape>> shapes;
    shapes.push_back(std::make_unique<Circle>(5));
    shapes.push_back(std::make_unique<Rectangle>(3, 4));
    for (const auto& s : shapes) {
        std::cout << "Area: " << s->area() << ", Perimeter: " << s->perimeter() << std::endl;
    }

    std::cout << "\n=== Function Types ===" << std::endl;
    auto dbl = [](int x) { return x * 2; };
    std::cout << "apply_twice(double, 3) = " << apply_twice<int>(dbl, 3) << std::endl;

    return 0;
}
