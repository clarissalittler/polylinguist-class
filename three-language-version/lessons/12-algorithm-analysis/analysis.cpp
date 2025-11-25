/**
 * Lesson 12: Algorithm Analysis - C++ Examples
 * Understanding time and space complexity through practical examples
 */

#include <iostream>
#include <vector>
#include <chrono>
#include <random>
#include <algorithm>
#include <unordered_set>
#include <map>

using namespace std;
using namespace std::chrono;

// =============================================================================
// TIMING UTILITY
// =============================================================================

template<typename Func>
double time_function(Func f) {
    auto start = high_resolution_clock::now();
    f();
    auto end = high_resolution_clock::now();
    return duration<double>(end - start).count();
}

// =============================================================================
// O(1) - CONSTANT TIME
// =============================================================================

int get_first(const vector<int>& items) {
    // O(1) - Direct access regardless of size
    return items.empty() ? -1 : items[0];
}

int get_by_index(const vector<int>& items, size_t index) {
    // O(1) - Array indexing is constant time
    return index < items.size() ? items[index] : -1;
}

bool is_even(int n) {
    // O(1) - Single arithmetic operation
    return n % 2 == 0;
}

void constant_time_demo() {
    cout << "=== O(1) Constant Time ===" << endl;

    vector<int> small(1000);
    vector<int> large(1000000);

    double small_time = time_function([&]() { get_first(small); });
    double large_time = time_function([&]() { get_first(large); });

    cout << "get_first on 1,000 items: " << small_time << "s" << endl;
    cout << "get_first on 1,000,000 items: " << large_time << "s" << endl;
    cout << "Both are nearly instant - that's O(1)!" << endl;
}

// =============================================================================
// O(log n) - LOGARITHMIC TIME
// =============================================================================

int binary_search(const vector<int>& sorted_list, int target, int& comparisons) {
    // O(log n) - Halves search space each iteration
    int left = 0;
    int right = sorted_list.size() - 1;
    comparisons = 0;

    while (left <= right) {
        comparisons++;
        int mid = left + (right - left) / 2;

        if (sorted_list[mid] == target) {
            return mid;
        } else if (sorted_list[mid] < target) {
            left = mid + 1;
        } else {
            right = mid - 1;
        }
    }
    return -1;
}

void logarithmic_time_demo() {
    cout << "\n=== O(log n) Logarithmic Time ===" << endl;

    vector<int> sorted_data(1000000);
    for (int i = 0; i < 1000000; i++) {
        sorted_data[i] = i;
    }

    int comparisons;
    binary_search(sorted_data, 750000, comparisons);
    cout << "Binary search in 1,000,000 items: " << comparisons << " comparisons" << endl;
    cout << "log2(1,000,000) ≈ 20" << endl;
}

// =============================================================================
// O(n) - LINEAR TIME
// =============================================================================

int linear_search(const vector<int>& items, int target) {
    // O(n) - Must check each element
    for (size_t i = 0; i < items.size(); i++) {
        if (items[i] == target) {
            return i;
        }
    }
    return -1;
}

int find_max(const vector<int>& items) {
    // O(n) - Must examine every element
    if (items.empty()) return -1;
    int maximum = items[0];
    for (int item : items) {
        if (item > maximum) {
            maximum = item;
        }
    }
    return maximum;
}

long long sum_all(const vector<int>& items) {
    // O(n) - Process every element once
    long long total = 0;
    for (int item : items) {
        total += item;
    }
    return total;
}

void linear_time_demo() {
    cout << "\n=== O(n) Linear Time ===" << endl;

    vector<int> data(100000);
    for (int i = 0; i < 100000; i++) {
        data[i] = i;
    }

    double elapsed = time_function([&]() { sum_all(data); });
    cout << "sum_all on 100,000 items: " << elapsed << "s" << endl;
}

// =============================================================================
// O(n log n) - LINEARITHMIC TIME
// =============================================================================

void merge(vector<int>& arr, int left, int mid, int right) {
    vector<int> temp(right - left + 1);
    int i = left, j = mid + 1, k = 0;

    while (i <= mid && j <= right) {
        if (arr[i] <= arr[j]) {
            temp[k++] = arr[i++];
        } else {
            temp[k++] = arr[j++];
        }
    }
    while (i <= mid) temp[k++] = arr[i++];
    while (j <= right) temp[k++] = arr[j++];

    for (int i = 0; i < k; i++) {
        arr[left + i] = temp[i];
    }
}

void merge_sort(vector<int>& arr, int left, int right) {
    // O(n log n) - Efficient divide-and-conquer
    if (left < right) {
        int mid = left + (right - left) / 2;
        merge_sort(arr, left, mid);
        merge_sort(arr, mid + 1, right);
        merge(arr, left, mid, right);
    }
}

void linearithmic_time_demo() {
    cout << "\n=== O(n log n) Linearithmic Time ===" << endl;

    random_device rd;
    mt19937 gen(rd());
    uniform_int_distribution<> dis(0, 10000);

    vector<int> data(10000);
    for (auto& x : data) x = dis(gen);

    double elapsed = time_function([&]() {
        merge_sort(data, 0, data.size() - 1);
    });
    cout << "merge_sort on 10,000 items: " << elapsed << "s" << endl;
}

// =============================================================================
// O(n²) - QUADRATIC TIME
// =============================================================================

void bubble_sort(vector<int>& arr) {
    // O(n²) - Nested loops comparing all pairs
    int n = arr.size();
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n - i - 1; j++) {
            if (arr[j] > arr[j + 1]) {
                swap(arr[j], arr[j + 1]);
            }
        }
    }
}

bool has_duplicates_naive(const vector<int>& items) {
    // O(n²) - Compare each pair
    int n = items.size();
    for (int i = 0; i < n; i++) {
        for (int j = i + 1; j < n; j++) {
            if (items[i] == items[j]) {
                return true;
            }
        }
    }
    return false;
}

bool has_duplicates_efficient(const vector<int>& items) {
    // O(n) - Use hash set for O(1) lookups
    unordered_set<int> seen;
    for (int item : items) {
        if (seen.count(item)) {
            return true;
        }
        seen.insert(item);
    }
    return false;
}

void quadratic_time_demo() {
    cout << "\n=== O(n²) Quadratic Time ===" << endl;

    random_device rd;
    mt19937 gen(rd());
    uniform_int_distribution<> dis(0, 1000);

    vector<int> data(1000);
    for (auto& x : data) x = dis(gen);

    double elapsed = time_function([&]() { bubble_sort(data); });
    cout << "bubble_sort on 1,000 items: " << elapsed << "s" << endl;

    // Compare duplicate checking
    cout << "\n=== Comparing O(n²) vs O(n) ===" << endl;

    vector<int> test_data(5000);
    for (int i = 0; i < 5000; i++) test_data[i] = i;
    test_data.push_back(2500);  // Add duplicate

    double naive_time = time_function([&]() { has_duplicates_naive(test_data); });
    double efficient_time = time_function([&]() { has_duplicates_efficient(test_data); });

    cout << "Naive O(n²): " << naive_time << "s" << endl;
    cout << "Efficient O(n): " << efficient_time << "s" << endl;
    cout << "Speedup: " << (naive_time / efficient_time) << "x faster" << endl;
}

// =============================================================================
// O(2^n) - EXPONENTIAL TIME
// =============================================================================

long long fibonacci_naive(int n) {
    // O(2^n) - Exponential due to redundant computation
    if (n <= 1) return n;
    return fibonacci_naive(n - 1) + fibonacci_naive(n - 2);
}

long long fibonacci_memo(int n, map<int, long long>& memo) {
    // O(n) - Memoization eliminates redundant work
    if (memo.count(n)) return memo[n];
    if (n <= 1) return n;
    memo[n] = fibonacci_memo(n - 1, memo) + fibonacci_memo(n - 2, memo);
    return memo[n];
}

void exponential_time_demo() {
    cout << "\n=== O(2^n) vs O(n) - Fibonacci ===" << endl;

    int n = 35;

    double naive_time = time_function([&]() { fibonacci_naive(n); });

    map<int, long long> memo;
    double memo_time = time_function([&]() { fibonacci_memo(n, memo); });

    cout << "fibonacci(" << n << ") naive O(2^n): " << naive_time << "s" << endl;
    cout << "fibonacci(" << n << ") memo O(n): " << memo_time << "s" << endl;
}

// =============================================================================
// SPACE COMPLEXITY
// =============================================================================

long long space_constant(int n) {
    // O(1) space - Fixed number of variables
    long long total = 0;
    for (int i = 0; i < n; i++) {
        total += i;
    }
    return total;
}

vector<int> space_linear(int n) {
    // O(n) space - Creates vector proportional to input
    vector<int> result(n);
    for (int i = 0; i < n; i++) {
        result[i] = i * 2;
    }
    return result;
}

vector<vector<int>> space_quadratic(int n) {
    // O(n²) space - Creates n x n matrix
    vector<vector<int>> matrix(n, vector<int>(n));
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            matrix[i][j] = i * j;
        }
    }
    return matrix;
}

void space_complexity_demo() {
    cout << "\n=== Space Complexity ===" << endl;
    cout << "O(1) space: Uses fixed variables regardless of n" << endl;
    cout << "O(n) space: Creates data structure of size n" << endl;
    cout << "O(n²) space: Creates n×n matrix" << endl;
}

// =============================================================================
// COMPARISON TABLE
// =============================================================================

void print_comparison() {
    cout << "\n=== Complexity Comparison ===" << endl;
    cout << R"(
| Complexity | n=10    | n=100   | n=1000   | n=10000    |
|------------|---------|---------|----------|------------|
| O(1)       | 1       | 1       | 1        | 1          |
| O(log n)   | 3       | 7       | 10       | 13         |
| O(n)       | 10      | 100     | 1000     | 10000      |
| O(n log n) | 33      | 664     | 9966     | 132877     |
| O(n²)      | 100     | 10000   | 1000000  | 100000000  |
| O(2^n)     | 1024    | 10^30   | 10^301   | ∞          |
)" << endl;
}

// =============================================================================
// MAIN
// =============================================================================

int main() {
    constant_time_demo();
    logarithmic_time_demo();
    linear_time_demo();
    linearithmic_time_demo();
    quadratic_time_demo();
    exponential_time_demo();
    space_complexity_demo();
    print_comparison();

    cout << "\n=== Practical Analysis Tips ===" << endl;
    cout << R"(
1. Count the loops:
   - Single loop over n items → O(n)
   - Nested loops → O(n²)
   - Loop that halves input → O(log n)

2. Watch for hidden complexity:
   - std::find on vector → O(n)
   - std::find on set/map → O(log n)
   - unordered_set/map lookup → O(1) average
   - std::sort → O(n log n)

3. Space-time tradeoffs:
   - Caching/memoization trades space for time
   - Sorting first can enable faster algorithms
)" << endl;

    return 0;
}
