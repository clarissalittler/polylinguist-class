/*
 * Lesson 5: Data Structures in C++
 * Demonstrates STL containers: vector, map, set, tuple
 */

#include <iostream>
#include <vector>
#include <map>
#include <unordered_map>
#include <set>
#include <unordered_set>
#include <tuple>
#include <string>
#include <algorithm>
#include <numeric>
#include <iomanip>
#include <sstream>

using namespace std;

// Helper function to print vectors
template<typename T>
void printVector(const string& label, const vector<T>& vec) {
    cout << label << ": [";
    for (size_t i = 0; i < vec.size(); ++i) {
        cout << vec[i];
        if (i < vec.size() - 1) cout << ", ";
    }
    cout << "]" << endl;
}

void demonstrateVectors() {
    cout << "=== 1. Vectors (Mutable, Dynamic) ===" << endl << endl;

    // Creating vectors
    vector<int> numbers = {1, 2, 3, 4, 5};
    vector<string> fruits = {"apple", "banana", "cherry"};

    printVector("Numbers", numbers);
    printVector("Fruits", fruits);

    // Accessing elements
    cout << "\nFirst element: " << numbers[0] << endl;
    cout << "Last element: " << numbers.back() << endl;
    cout << "Element at index 2 (safe): " << numbers.at(2) << endl;

    // Modifying vectors (MUTABLE)
    numbers.push_back(6);
    printVector("\nAfter push_back(6)", numbers);

    numbers.insert(numbers.begin(), 0);
    printVector("After insert at beginning", numbers);

    numbers[1] = 10;
    printVector("After numbers[1] = 10", numbers);

    numbers.pop_back();
    printVector("After pop_back()", numbers);

    // Size and capacity
    cout << "\nSize: " << numbers.size() << endl;
    cout << "Capacity: " << numbers.capacity() << endl;

    // Aliasing demonstration
    cout << "\n--- Aliasing with References ---" << endl;
    vector<int> vec1 = {1, 2, 3};
    vector<int>& vec2 = vec1;  // Reference to same vector
    vec2.push_back(4);
    printVector("vec1", vec1);  // Also changed!
    printVector("vec2", vec2);

    // Copying
    cout << "\n--- Proper Copying ---" << endl;
    vector<int> vec3 = vec1;  // Copy constructor
    vec3.push_back(5);
    printVector("vec1 (unchanged)", vec1);
    printVector("vec3 (modified)", vec3);

    // Iteration
    cout << "\n--- Iteration ---" << endl;
    cout << "Range-based for: ";
    for (int x : numbers) {
        cout << x << " ";
    }
    cout << endl;

    // Algorithms
    cout << "\n--- Algorithms ---" << endl;
    vector<int> squared = numbers;
    transform(squared.begin(), squared.end(), squared.begin(),
              [](int x) { return x * x; });
    printVector("Squared", squared);

    vector<int> evens;
    copy_if(numbers.begin(), numbers.end(), back_inserter(evens),
            [](int x) { return x % 2 == 0; });
    printVector("Evens", evens);
}

void demonstrateMapsAndUnorderedMaps() {
    cout << "\n=== 2. Maps and Unordered Maps ===\n" << endl;

    // std::map - ordered (red-black tree)
    cout << "--- std::map (Ordered, O(log n)) ---" << endl;
    map<string, int> ages;
    ages["Alice"] = 30;
    ages["Bob"] = 25;
    ages["Charlie"] = 35;

    cout << "Ages:" << endl;
    for (const auto& [name, age] : ages) {
        cout << "  " << name << ": " << age << endl;
    }

    // Access
    cout << "\nAlice's age: " << ages["Alice"] << endl;

    // Find (safe access)
    auto it = ages.find("Bob");
    if (it != ages.end()) {
        cout << "Bob found: " << it->second << endl;
    }

    // Update
    ages["Alice"] = 31;
    cout << "Updated Alice: " << ages["Alice"] << endl;

    // Erase
    ages.erase("Bob");
    cout << "\nAfter erasing Bob:" << endl;
    for (const auto& [name, age] : ages) {
        cout << "  " << name << ": " << age << endl;
    }

    // std::unordered_map - hash table
    cout << "\n--- std::unordered_map (Hash Table, O(1) avg) ---" << endl;
    unordered_map<string, string> person;
    person["name"] = "Alice";
    person["city"] = "NYC";
    person["email"] = "alice@example.com";

    cout << "Person:" << endl;
    for (const auto& [key, value] : person) {
        cout << "  " << key << ": " << value << endl;
    }

    // Check key existence (C++20)
    #if __cplusplus >= 202002L
    if (person.contains("name")) {
        cout << "\n'name' key exists" << endl;
    }
    #else
    if (person.find("name") != person.end()) {
        cout << "\n'name' key exists" << endl;
    }
    #endif
}

void demonstrateSetsAndUnorderedSets() {
    cout << "\n=== 3. Sets and Unordered Sets ===\n" << endl;

    // std::set - ordered
    cout << "--- std::set (Ordered, O(log n)) ---" << endl;
    set<int> numbers = {1, 2, 3, 4, 5};

    // Add elements
    numbers.insert(6);
    numbers.insert(3);  // Duplicate, ignored

    cout << "Numbers: ";
    for (int x : numbers) {
        cout << x << " ";
    }
    cout << endl;

    // Remove element
    numbers.erase(1);
    cout << "After erase(1): ";
    for (int x : numbers) {
        cout << x << " ";
    }
    cout << endl;

    // Membership testing
    if (numbers.find(3) != numbers.end()) {
        cout << "\n3 is in the set" << endl;
    }

    // Set operations
    cout << "\n--- Set Operations ---" << endl;
    set<int> evens = {2, 4, 6, 8};
    set<int> odds = {1, 3, 5, 7};

    set<int> union_set;
    set_union(evens.begin(), evens.end(),
              odds.begin(), odds.end(),
              inserter(union_set, union_set.begin()));

    set<int> intersection_set;
    set_intersection(evens.begin(), evens.end(),
                     odds.begin(), odds.end(),
                     inserter(intersection_set, intersection_set.begin()));

    set<int> difference_set;
    set_difference(evens.begin(), evens.end(),
                   odds.begin(), odds.end(),
                   inserter(difference_set, difference_set.begin()));

    cout << "Evens: ";
    for (int x : evens) cout << x << " ";
    cout << endl;

    cout << "Odds: ";
    for (int x : odds) cout << x << " ";
    cout << endl;

    cout << "Union: ";
    for (int x : union_set) cout << x << " ";
    cout << endl;

    cout << "Intersection: ";
    for (int x : intersection_set) cout << x << " ";
    cout << "(empty)" << endl;

    cout << "Difference: ";
    for (int x : difference_set) cout << x << " ";
    cout << endl;

    // std::unordered_set - hash table
    cout << "\n--- std::unordered_set (Hash Table) ---" << endl;
    unordered_set<string> fruits = {"apple", "banana", "cherry"};
    fruits.insert("date");

    cout << "Fruits: ";
    for (const auto& fruit : fruits) {
        cout << fruit << " ";
    }
    cout << endl;

    // Remove duplicates from vector
    cout << "\n--- Remove Duplicates ---" << endl;
    vector<int> duplicates = {1, 2, 2, 3, 3, 3, 4, 4, 4, 4};
    set<int> unique(duplicates.begin(), duplicates.end());

    printVector("Original", duplicates);
    cout << "Unique: ";
    for (int x : unique) cout << x << " ";
    cout << endl;
}

void demonstrateTuples() {
    cout << "\n=== 4. Tuples and Pairs ===\n" << endl;

    // Pair (2 elements)
    pair<int, int> point(3, 4);
    cout << "Point: (" << point.first << ", " << point.second << ")" << endl;

    // Tuple (arbitrary size)
    tuple<string, int, string> person("Alice", 30, "NYC");

    cout << "\nPerson tuple:" << endl;
    cout << "  Name: " << get<0>(person) << endl;
    cout << "  Age: " << get<1>(person) << endl;
    cout << "  City: " << get<2>(person) << endl;

    // Structured bindings (C++17)
    #if __cplusplus >= 201703L
    auto [name, age, city] = person;
    cout << "\nStructured binding: " << name << ", " << age << ", " << city << endl;
    #endif

    // Make tuple
    auto point3d = make_tuple(1, 2, 3);
    cout << "\n3D Point: (" << get<0>(point3d) << ", " << get<1>(point3d)
         << ", " << get<2>(point3d) << ")" << endl;

    // Tuple comparison
    tuple<int, int> p1(1, 2);
    tuple<int, int> p2(1, 3);
    if (p1 < p2) {
        cout << "\np1 < p2 (lexicographic comparison)" << endl;
    }

    // Use in map
    map<pair<int, int>, string> grid;
    grid[{0, 0}] = "origin";
    grid[{1, 0}] = "east";
    grid[{0, 1}] = "north";

    cout << "\nGrid:" << endl;
    for (const auto& [pos, label] : grid) {
        cout << "  (" << pos.first << ", " << pos.second << "): " << label << endl;
    }
}

void demonstrateNestedStructures() {
    cout << "\n=== 5. Nested Structures ===\n" << endl;

    // Matrix (vector of vectors)
    vector<vector<int>> matrix = {
        {1, 2, 3},
        {4, 5, 6},
        {7, 8, 9}
    };

    cout << "Matrix:" << endl;
    for (const auto& row : matrix) {
        cout << "  ";
        for (int val : row) {
            cout << setw(3) << val;
        }
        cout << endl;
    }

    cout << "\nElement [1][2]: " << matrix[1][2] << endl;

    // Transpose matrix
    vector<vector<int>> transposed(3, vector<int>(3));
    for (size_t i = 0; i < 3; ++i) {
        for (size_t j = 0; j < 3; ++j) {
            transposed[j][i] = matrix[i][j];
        }
    }

    cout << "\nTransposed:" << endl;
    for (const auto& row : transposed) {
        cout << "  ";
        for (int val : row) {
            cout << setw(3) << val;
        }
        cout << endl;
    }
}

void wordFrequencyCounter() {
    cout << "\n=== 6. Word Frequency Counter ===\n" << endl;

    string text = "the quick brown fox jumps over the lazy dog the fox";

    // Split into words
    istringstream iss(text);
    map<string, int> freq;
    string word;

    while (iss >> word) {
        freq[word]++;
    }

    cout << "Text: " << text << "\n" << endl;
    cout << "Word frequencies:" << endl;

    // Convert to vector for sorting
    vector<pair<string, int>> freqVec(freq.begin(), freq.end());
    sort(freqVec.begin(), freqVec.end(),
         [](const auto& a, const auto& b) {
             return a.second > b.second;  // Sort by frequency descending
         });

    for (const auto& [w, count] : freqVec) {
        cout << "  " << w << ": " << count << endl;
    }
}

void shoppingCartDemo() {
    cout << "\n=== 7. Shopping Cart (Mutable) ===\n" << endl;

    struct Item {
        string name;
        double price;
        int quantity;
    };

    vector<Item> cart;

    auto addItem = [&cart](const string& name, double price, int quantity) {
        for (auto& item : cart) {
            if (item.name == name) {
                item.quantity += quantity;
                return;
            }
        }
        cart.push_back({name, price, quantity});
    };

    auto calculateTotal = [&cart]() {
        return accumulate(cart.begin(), cart.end(), 0.0,
                         [](double sum, const Item& item) {
                             return sum + item.price * item.quantity;
                         });
    };

    auto printCart = [&cart, &calculateTotal]() {
        cout << "Cart contents:" << endl;
        for (const auto& item : cart) {
            double subtotal = item.price * item.quantity;
            cout << "  " << item.name << ": $" << fixed << setprecision(2)
                 << item.price << " x " << item.quantity
                 << " = $" << subtotal << endl;
        }
        cout << "Total: $" << calculateTotal() << endl;
    };

    // Use the cart
    addItem("Apple", 0.50, 5);
    addItem("Banana", 0.30, 3);
    addItem("Orange", 0.75, 4);
    printCart();

    cout << "\nAdding more apples..." << endl;
    addItem("Apple", 0.50, 3);
    printCart();
}

void performanceNotes() {
    cout << "\n=== 8. Performance Notes ===\n" << endl;

    cout << "Time Complexities:" << endl;
    cout << "\nvector:" << endl;
    cout << "  - Random access: O(1)" << endl;
    cout << "  - push_back: O(1) amortized" << endl;
    cout << "  - insert at beginning: O(n)" << endl;
    cout << "  - insert in middle: O(n)" << endl;

    cout << "\nmap (ordered):" << endl;
    cout << "  - Lookup: O(log n)" << endl;
    cout << "  - Insert: O(log n)" << endl;
    cout << "  - Delete: O(log n)" << endl;

    cout << "\nunordered_map (hash table):" << endl;
    cout << "  - Lookup: O(1) average, O(n) worst" << endl;
    cout << "  - Insert: O(1) average, O(n) worst" << endl;
    cout << "  - Delete: O(1) average, O(n) worst" << endl;

    cout << "\nset (ordered):" << endl;
    cout << "  - Membership: O(log n)" << endl;
    cout << "  - Insert: O(log n)" << endl;

    cout << "\nunordered_set (hash table):" << endl;
    cout << "  - Membership: O(1) average" << endl;
    cout << "  - Insert: O(1) average" << endl;
}

int main() {
    cout << string(60, '=') << endl;
    cout << "C++ Data Structures Demonstration (STL)" << endl;
    cout << string(60, '=') << endl << endl;

    demonstrateVectors();
    demonstrateMapsAndUnorderedMaps();
    demonstrateSetsAndUnorderedSets();
    demonstrateTuples();
    demonstrateNestedStructures();
    wordFrequencyCounter();
    shoppingCartDemo();
    performanceNotes();

    cout << "\n" << string(60, '=') << endl;
    cout << "Key Takeaways:" << endl;
    cout << "- STL provides powerful, type-safe containers" << endl;
    cout << "- vector: dynamic array, fast random access" << endl;
    cout << "- map/unordered_map: key-value storage" << endl;
    cout << "- set/unordered_set: unique elements" << endl;
    cout << "- Choose ordered vs unordered based on needs" << endl;
    cout << "- RAII ensures automatic memory management" << endl;
    cout << string(60, '=') << endl;

    return 0;
}
