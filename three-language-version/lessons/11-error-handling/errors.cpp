/**
 * Lesson 11: Error Handling - C++ Examples
 * C++ uses exceptions and optional types for error handling
 */

#include <iostream>
#include <stdexcept>
#include <optional>
#include <variant>
#include <expected>  // C++23
#include <string>
#include <fstream>
#include <memory>
#include <cassert>

// =============================================================================
// BASIC EXCEPTION HANDLING
// =============================================================================

double divide(double a, double b) {
    if (b == 0) {
        throw std::runtime_error("Division by zero");
    }
    return a / b;
}

void basic_exception_demo() {
    std::cout << "=== Basic Exceptions ===" << std::endl;

    try {
        std::cout << "10 / 2 = " << divide(10, 2) << std::endl;
    } catch (const std::runtime_error& e) {
        std::cout << "Error: " << e.what() << std::endl;
    }

    try {
        std::cout << "10 / 0 = " << divide(10, 0) << std::endl;
    } catch (const std::runtime_error& e) {
        std::cout << "Error: " << e.what() << std::endl;
    }
}

// =============================================================================
// EXCEPTION HIERARCHY
// =============================================================================

void exception_hierarchy_demo() {
    std::cout << "\n=== Exception Hierarchy ===" << std::endl;

    // std::exception
    //   ├── std::logic_error
    //   │     ├── std::invalid_argument
    //   │     ├── std::domain_error
    //   │     ├── std::length_error
    //   │     └── std::out_of_range
    //   └── std::runtime_error
    //         ├── std::range_error
    //         ├── std::overflow_error
    //         └── std::underflow_error

    try {
        throw std::out_of_range("Index out of bounds");
    } catch (const std::logic_error& e) {
        std::cout << "Logic error: " << e.what() << std::endl;
    } catch (const std::exception& e) {
        std::cout << "Exception: " << e.what() << std::endl;
    }
}

// =============================================================================
// MULTIPLE CATCH BLOCKS
// =============================================================================

void risky_operation(int value) {
    if (value < 0) {
        throw std::invalid_argument("Negative value not allowed");
    }
    if (value == 0) {
        throw std::runtime_error("Zero not allowed");
    }
    std::cout << "100 / " << value << " = " << (100 / value) << std::endl;
}

void multiple_catch_demo() {
    std::cout << "\n=== Multiple Catch Blocks ===" << std::endl;

    for (int val : {5, 0, -3}) {
        try {
            risky_operation(val);
        } catch (const std::invalid_argument& e) {
            std::cout << "Invalid argument: " << e.what() << std::endl;
        } catch (const std::runtime_error& e) {
            std::cout << "Runtime error: " << e.what() << std::endl;
        }
    }
}

// =============================================================================
// CUSTOM EXCEPTIONS
// =============================================================================

class InsufficientFundsError : public std::exception {
private:
    std::string message;
    double balance;
    double requested;

public:
    InsufficientFundsError(double bal, double req)
        : balance(bal), requested(req) {
        message = "Insufficient funds: balance=" + std::to_string(bal) +
                  ", requested=" + std::to_string(req);
    }

    const char* what() const noexcept override {
        return message.c_str();
    }

    double getBalance() const { return balance; }
    double getRequested() const { return requested; }
};

class BankAccount {
    double balance;
public:
    BankAccount(double initial) : balance(initial) {}

    void withdraw(double amount) {
        if (amount > balance) {
            throw InsufficientFundsError(balance, amount);
        }
        balance -= amount;
    }

    double getBalance() const { return balance; }
};

void custom_exception_demo() {
    std::cout << "\n=== Custom Exceptions ===" << std::endl;

    BankAccount account(100);
    try {
        account.withdraw(150);
    } catch (const InsufficientFundsError& e) {
        std::cout << "Error: " << e.what() << std::endl;
        std::cout << "Balance: " << e.getBalance()
                  << ", Attempted: " << e.getRequested() << std::endl;
    }
}

// =============================================================================
// OPTIONAL TYPE (Alternative to exceptions)
// =============================================================================

std::optional<double> safe_divide(double a, double b) {
    if (b == 0) return std::nullopt;
    return a / b;
}

std::optional<double> safe_sqrt(double x) {
    if (x < 0) return std::nullopt;
    return std::sqrt(x);
}

void optional_demo() {
    std::cout << "\n=== std::optional ===" << std::endl;

    auto result1 = safe_divide(10, 2);
    auto result2 = safe_divide(10, 0);

    std::cout << "safe_divide(10, 2) = "
              << (result1 ? std::to_string(*result1) : "none") << std::endl;
    std::cout << "safe_divide(10, 0) = "
              << (result2 ? std::to_string(*result2) : "none") << std::endl;

    // Using value_or
    std::cout << "safe_divide(10, 0).value_or(-1) = "
              << safe_divide(10, 0).value_or(-1) << std::endl;
}

// =============================================================================
// VARIANT FOR RESULT TYPE
// =============================================================================

struct DivError { std::string message; };

using DivResult = std::variant<double, DivError>;

DivResult safe_divide_v(double a, double b) {
    if (b == 0) return DivError{"Division by zero"};
    return a / b;
}

void variant_result_demo() {
    std::cout << "\n=== Variant as Result ===" << std::endl;

    auto result = safe_divide_v(10, 2);
    if (auto* val = std::get_if<double>(&result)) {
        std::cout << "Success: " << *val << std::endl;
    }

    result = safe_divide_v(10, 0);
    if (auto* err = std::get_if<DivError>(&result)) {
        std::cout << "Error: " << err->message << std::endl;
    }
}

// =============================================================================
// RAII - Resource Acquisition Is Initialization
// =============================================================================

class FileHandler {
    std::unique_ptr<std::ofstream> file;
public:
    FileHandler(const std::string& filename) {
        file = std::make_unique<std::ofstream>(filename);
        if (!file->is_open()) {
            throw std::runtime_error("Could not open file");
        }
        std::cout << "File opened" << std::endl;
    }

    ~FileHandler() {
        if (file && file->is_open()) {
            file->close();
            std::cout << "File closed automatically" << std::endl;
        }
    }

    void write(const std::string& data) {
        *file << data;
    }
};

void raii_demo() {
    std::cout << "\n=== RAII ===" << std::endl;

    try {
        FileHandler fh("test.txt");
        fh.write("Hello, RAII!");
        // File is automatically closed when fh goes out of scope
    } catch (const std::exception& e) {
        std::cout << "Error: " << e.what() << std::endl;
    }
}

// =============================================================================
// NOEXCEPT SPECIFICATION
// =============================================================================

void safe_function() noexcept {
    // This function promises not to throw
    std::cout << "This function won't throw" << std::endl;
}

int safe_add(int a, int b) noexcept {
    return a + b;  // Simple arithmetic won't throw
}

void noexcept_demo() {
    std::cout << "\n=== noexcept ===" << std::endl;

    std::cout << "safe_function noexcept: "
              << noexcept(safe_function()) << std::endl;
    std::cout << "divide noexcept: "
              << noexcept(divide(1, 1)) << std::endl;
}

// =============================================================================
// ASSERTIONS
// =============================================================================

double calculate_average(const std::vector<double>& numbers) {
    assert(!numbers.empty() && "Cannot calculate average of empty vector");

    double sum = 0;
    for (double n : numbers) sum += n;
    return sum / numbers.size();
}

void assertion_demo() {
    std::cout << "\n=== Assertions ===" << std::endl;

    std::vector<double> nums = {1, 2, 3, 4, 5};
    std::cout << "Average: " << calculate_average(nums) << std::endl;

    // This would trigger assertion failure:
    // calculate_average({});
}

// =============================================================================
// RE-THROWING EXCEPTIONS
// =============================================================================

void process_data(const std::string& data) {
    try {
        int value = std::stoi(data);
        std::cout << "Processed: " << value << std::endl;
    } catch (const std::exception& e) {
        std::cout << "Error processing '" << data << "' - rethrowing" << std::endl;
        throw;  // Re-throw the same exception
    }
}

void rethrow_demo() {
    std::cout << "\n=== Re-throwing ===" << std::endl;

    try {
        process_data("not a number");
    } catch (const std::exception& e) {
        std::cout << "Caught re-thrown exception: " << e.what() << std::endl;
    }
}

// =============================================================================
// MAIN
// =============================================================================

int main() {
    basic_exception_demo();
    exception_hierarchy_demo();
    multiple_catch_demo();
    custom_exception_demo();
    optional_demo();
    variant_result_demo();
    raii_demo();
    noexcept_demo();
    assertion_demo();
    rethrow_demo();

    return 0;
}
