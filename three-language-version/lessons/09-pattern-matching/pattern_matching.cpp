/**
 * Lesson 9: Pattern Matching - C++ Examples
 * C++ uses std::variant and std::visit for type-safe pattern matching
 */

#include <iostream>
#include <variant>
#include <string>
#include <vector>
#include <optional>
#include <functional>

// =============================================================================
// VARIANT-BASED PATTERN MATCHING
// =============================================================================

// Define a variant that can hold different types
using Value = std::variant<int, double, std::string, std::vector<int>>;

std::string describe_value(const Value& v) {
    return std::visit([](auto&& arg) -> std::string {
        using T = std::decay_t<decltype(arg)>;
        if constexpr (std::is_same_v<T, int>) {
            return "Integer: " + std::to_string(arg);
        } else if constexpr (std::is_same_v<T, double>) {
            return "Double: " + std::to_string(arg);
        } else if constexpr (std::is_same_v<T, std::string>) {
            return "String: " + arg;
        } else if constexpr (std::is_same_v<T, std::vector<int>>) {
            return "Vector with " + std::to_string(arg.size()) + " elements";
        }
    }, v);
}

// =============================================================================
// SHAPES EXAMPLE (Classic pattern matching use case)
// =============================================================================

struct Point {
    double x, y;
};

struct Circle {
    Point center;
    double radius;
};

struct Rectangle {
    Point corner;
    double width, height;
};

struct Triangle {
    Point a, b, c;
};

using Shape = std::variant<Circle, Rectangle, Triangle>;

double area(const Shape& shape) {
    return std::visit([](auto&& s) -> double {
        using T = std::decay_t<decltype(s)>;
        if constexpr (std::is_same_v<T, Circle>) {
            return 3.14159 * s.radius * s.radius;
        } else if constexpr (std::is_same_v<T, Rectangle>) {
            return s.width * s.height;
        } else if constexpr (std::is_same_v<T, Triangle>) {
            // Using cross product formula
            return 0.5 * std::abs(
                (s.b.x - s.a.x) * (s.c.y - s.a.y) -
                (s.c.x - s.a.x) * (s.b.y - s.a.y)
            );
        }
    }, shape);
}

std::string describe_shape(const Shape& shape) {
    return std::visit([](auto&& s) -> std::string {
        using T = std::decay_t<decltype(s)>;
        if constexpr (std::is_same_v<T, Circle>) {
            if (s.center.x == 0 && s.center.y == 0) {
                return "Circle at origin with radius " + std::to_string(s.radius);
            }
            return "Circle at (" + std::to_string(s.center.x) + ", " +
                   std::to_string(s.center.y) + ") with radius " +
                   std::to_string(s.radius);
        } else if constexpr (std::is_same_v<T, Rectangle>) {
            if (s.width == s.height) {
                return "Square with side " + std::to_string(s.width);
            }
            return "Rectangle " + std::to_string(s.width) + "x" +
                   std::to_string(s.height);
        } else if constexpr (std::is_same_v<T, Triangle>) {
            return "Triangle";
        }
    }, shape);
}

// =============================================================================
// EXPRESSION EVALUATION
// =============================================================================

struct Literal { double value; };
struct Add { };
struct Sub { };
struct Mul { };
struct Div { };

using Op = std::variant<Add, Sub, Mul, Div>;

struct BinaryExpr;

using Expr = std::variant<
    Literal,
    std::shared_ptr<BinaryExpr>
>;

struct BinaryExpr {
    Expr left;
    Op op;
    Expr right;
};

double evaluate(const Expr& expr);  // Forward declaration

double apply_op(const Op& op, double left, double right) {
    return std::visit([=](auto&& o) -> double {
        using T = std::decay_t<decltype(o)>;
        if constexpr (std::is_same_v<T, Add>) return left + right;
        else if constexpr (std::is_same_v<T, Sub>) return left - right;
        else if constexpr (std::is_same_v<T, Mul>) return left * right;
        else if constexpr (std::is_same_v<T, Div>) return left / right;
    }, op);
}

double evaluate(const Expr& expr) {
    return std::visit([](auto&& e) -> double {
        using T = std::decay_t<decltype(e)>;
        if constexpr (std::is_same_v<T, Literal>) {
            return e.value;
        } else if constexpr (std::is_same_v<T, std::shared_ptr<BinaryExpr>>) {
            double left = evaluate(e->left);
            double right = evaluate(e->right);
            return apply_op(e->op, left, right);
        }
    }, expr);
}

// =============================================================================
// OPTIONAL PATTERN MATCHING
// =============================================================================

std::optional<int> find_in_vector(const std::vector<int>& vec, int target) {
    for (size_t i = 0; i < vec.size(); i++) {
        if (vec[i] == target) {
            return static_cast<int>(i);
        }
    }
    return std::nullopt;
}

void demonstrate_optional() {
    std::vector<int> nums = {10, 20, 30, 40, 50};

    auto result1 = find_in_vector(nums, 30);
    auto result2 = find_in_vector(nums, 99);

    // "Pattern matching" on optional
    if (result1.has_value()) {
        std::cout << "Found 30 at index " << result1.value() << std::endl;
    } else {
        std::cout << "30 not found" << std::endl;
    }

    // Or using value_or
    std::cout << "Index of 99: " << result2.value_or(-1) << std::endl;
}

// =============================================================================
// HELPER: Overload pattern for cleaner visit syntax
// =============================================================================

template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

void demonstrate_overloaded() {
    std::cout << "\n=== Using Overloaded Pattern ===" << std::endl;

    Value v1 = 42;
    Value v2 = 3.14;
    Value v3 = std::string("hello");

    for (const auto& v : {v1, v2, v3}) {
        std::visit(overloaded {
            [](int i)    { std::cout << "int: " << i << std::endl; },
            [](double d) { std::cout << "double: " << d << std::endl; },
            [](const std::string& s) { std::cout << "string: " << s << std::endl; },
            [](const std::vector<int>&) { std::cout << "vector" << std::endl; }
        }, v);
    }
}

// =============================================================================
// MAIN
// =============================================================================

int main() {
    std::cout << "=== Variant Pattern Matching ===" << std::endl;
    std::vector<Value> values = {42, 3.14, std::string("hello"), std::vector<int>{1, 2, 3}};
    for (const auto& v : values) {
        std::cout << describe_value(v) << std::endl;
    }

    std::cout << "\n=== Shape Pattern Matching ===" << std::endl;
    std::vector<Shape> shapes = {
        Circle{{0, 0}, 5},
        Circle{{1, 2}, 3},
        Rectangle{{0, 0}, 4, 4},
        Rectangle{{0, 0}, 3, 5},
        Triangle{{0, 0}, {4, 0}, {2, 3}}
    };
    for (const auto& s : shapes) {
        std::cout << describe_shape(s) << " - Area: " << area(s) << std::endl;
    }

    std::cout << "\n=== Expression Evaluation ===" << std::endl;
    // Build: (3 + 4) * 2
    auto expr = std::make_shared<BinaryExpr>(BinaryExpr{
        std::make_shared<BinaryExpr>(BinaryExpr{
            Literal{3}, Add{}, Literal{4}
        }),
        Mul{},
        Literal{2}
    });
    std::cout << "(3 + 4) * 2 = " << evaluate(expr) << std::endl;

    std::cout << "\n=== Optional Pattern Matching ===" << std::endl;
    demonstrate_optional();

    demonstrate_overloaded();

    return 0;
}
