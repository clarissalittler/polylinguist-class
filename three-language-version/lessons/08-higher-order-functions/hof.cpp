// Lesson 8: Higher-Order Functions in C++
// Demonstrates modern C++ functional programming features

#include <iostream>
#include <vector>
#include <algorithm>
#include <functional>
#include <numeric>
#include <chrono>

int main() {
    std::cout << std::string(70, '=') << std::endl;
    std::cout << "LESSON 8: HIGHER-ORDER FUNCTIONS IN C++" << std::endl;
    std::cout << std::string(70, '=') << std::endl;

    // PART 1: First-Class Functions
    std::cout << "\n--- PART 1: FIRST-CLASS FUNCTIONS ---" << std::endl;

    auto square = [](int x) { return x * x; };
    auto cube = [](int x) { return x * x * x; };

    std::vector<std::function<int(int)>> operations = {square, cube, [](int x) { return x * 2; }};
    std::cout << "Operations on 5: ";
    for (const auto& op : operations) {
        std::cout << op(5) << " ";
    }
    std::cout << std::endl;

    // PART 2: Map, Filter, Reduce
    std::cout << "\n--- PART 2: MAP, FILTER, REDUCE ---" << std::endl;

    std::vector<int> numbers = {1, 2, 3, 4, 5};

    // Map (transform)
    std::vector<int> doubled;
    std::transform(numbers.begin(), numbers.end(), std::back_inserter(doubled),
                   [](int x) { return x * 2; });
    std::cout << "Map (double): ";
    for (int n : doubled) std::cout << n << " ";
    std::cout << std::endl;

    // Filter (copy_if)
    std::vector<int> evens;
    std::copy_if(numbers.begin(), numbers.end(), std::back_inserter(evens),
                 [](int x) { return x % 2 == 0; });
    std::cout << "Filter (evens): ";
    for (int n : evens) std::cout << n << " ";
    std::cout << std::endl;

    // Reduce (accumulate)
    int sum = std::accumulate(numbers.begin(), numbers.end(), 0);
    int product = std::accumulate(numbers.begin(), numbers.end(), 1,
                                  std::multiplies<int>());
    std::cout << "Reduce (sum): " << sum << std::endl;
    std::cout << "Reduce (product): " << product << std::endl;

    // PART 3: Higher-Order Functions
    std::cout << "\n--- PART 3: HIGHER-ORDER FUNCTIONS ---" << std::endl;

    auto applyTwice = [](auto func, auto x) {
        return func(func(x));
    };

    auto compose = [](auto f, auto g) {
        return [=](auto x) { return f(g(x)); };
    };

    std::cout << "applyTwice(+1, 5) = " << applyTwice([](int x) { return x + 1; }, 5) << std::endl;

    auto addOne = [](int x) { return x + 1; };
    auto timesTwo = [](int x) { return x * 2; };
    auto composed = compose(timesTwo, addOne);
    std::cout << "compose(timesTwo, addOne)(5) = " << composed(5) << std::endl;

    // PART 4: Functions Returning Functions
    std::cout << "\n--- PART 4: FUNCTIONS RETURNING FUNCTIONS ---" << std::endl;

    auto makeMultiplier = [](int n) {
        return [n](int x) { return x * n; };
    };

    auto timesThree = makeMultiplier(3);
    auto timesFive = makeMultiplier(5);
    std::cout << "timesThree(10) = " << timesThree(10) << std::endl;
    std::cout << "timesFive(4) = " << timesFive(4) << std::endl;

    // PART 5: Practical Examples
    std::cout << "\n--- PART 5: PRACTICAL EXAMPLES ---" << std::endl;

    struct Person {
        std::string name;
        int age;
    };

    std::vector<Person> people = {
        {"Alice", 30},
        {"Bob", 25},
        {"Charlie", 35}
    };

    std::sort(people.begin(), people.end(),
              [](const Person& a, const Person& b) { return a.age < b.age; });
    std::cout << "Sorted by age: ";
    for (const auto& p : people) std::cout << p.name << " ";
    std::cout << std::endl;

    std::cout << "\n" << std::string(70, '=') << std::endl;
    std::cout << "C++ supports powerful functional programming!" << std::endl;
    std::cout << std::string(70, '=') << std::endl;

    return 0;
}
