/**
 * Lesson 6: Recursion - C++ Examples
 * Demonstrating recursive thinking in C++
 */

#include <iostream>
#include <vector>
#include <string>
#include <map>

// =============================================================================
// BASIC RECURSION: Factorial
// =============================================================================

long long factorial(int n) {
    if (n <= 1) {        // Base case
        return 1;
    }
    return n * factorial(n - 1);  // Recursive case
}

// =============================================================================
// FIBONACCI SEQUENCE
// =============================================================================

// Naive recursive (slow for large n)
long long fibonacci(int n) {
    if (n <= 1) {
        return n;
    }
    return fibonacci(n - 1) + fibonacci(n - 2);
}

// Memoized version (much faster)
std::map<int, long long> fib_cache;

long long fibonacci_memo(int n) {
    if (n <= 1) return n;

    if (fib_cache.find(n) != fib_cache.end()) {
        return fib_cache[n];
    }

    fib_cache[n] = fibonacci_memo(n - 1) + fibonacci_memo(n - 2);
    return fib_cache[n];
}

// =============================================================================
// LIST PROCESSING RECURSIVELY
// =============================================================================

int sum_vector(const std::vector<int>& vec, size_t index = 0) {
    if (index >= vec.size()) {  // Base case: past the end
        return 0;
    }
    return vec[index] + sum_vector(vec, index + 1);  // Recursive case
}

int length(const std::vector<int>& vec, size_t index = 0) {
    if (index >= vec.size()) {
        return 0;
    }
    return 1 + length(vec, index + 1);
}

bool contains(const std::vector<int>& vec, int target, size_t index = 0) {
    if (index >= vec.size()) {
        return false;
    }
    if (vec[index] == target) {
        return true;
    }
    return contains(vec, target, index + 1);
}

// =============================================================================
// BINARY SEARCH (Recursive)
// =============================================================================

int binary_search(const std::vector<int>& arr, int target, int low, int high) {
    if (low > high) {  // Base case: not found
        return -1;
    }

    int mid = low + (high - low) / 2;  // Avoid overflow

    if (arr[mid] == target) {
        return mid;
    } else if (arr[mid] < target) {
        return binary_search(arr, target, mid + 1, high);
    } else {
        return binary_search(arr, target, low, mid - 1);
    }
}

// Wrapper function
int binary_search(const std::vector<int>& arr, int target) {
    return binary_search(arr, target, 0, arr.size() - 1);
}

// =============================================================================
// TAIL RECURSION
// =============================================================================

long long factorial_tail(int n, long long accumulator = 1) {
    if (n <= 1) {
        return accumulator;
    }
    return factorial_tail(n - 1, n * accumulator);
}

// =============================================================================
// TOWERS OF HANOI
// =============================================================================

void hanoi(int n, char source, char target, char auxiliary) {
    if (n == 1) {
        std::cout << "Move disk 1 from " << source << " to " << target << std::endl;
        return;
    }

    hanoi(n - 1, source, auxiliary, target);
    std::cout << "Move disk " << n << " from " << source << " to " << target << std::endl;
    hanoi(n - 1, auxiliary, target, source);
}

// =============================================================================
// MUTUAL RECURSION
// =============================================================================

bool is_odd(int n);  // Forward declaration

bool is_even(int n) {
    if (n == 0) return true;
    return is_odd(n - 1);
}

bool is_odd(int n) {
    if (n == 0) return false;
    return is_even(n - 1);
}

// =============================================================================
// STRING REVERSAL
// =============================================================================

std::string reverse_string(const std::string& s) {
    if (s.length() <= 1) {
        return s;
    }
    return reverse_string(s.substr(1)) + s[0];
}

// =============================================================================
// MAIN
// =============================================================================

int main() {
    std::cout << "=== Factorial ===" << std::endl;
    for (int i = 0; i < 6; i++) {
        std::cout << i << "! = " << factorial(i) << std::endl;
    }

    std::cout << "\n=== Fibonacci ===" << std::endl;
    std::cout << "First 10 Fibonacci numbers: ";
    for (int i = 0; i < 10; i++) {
        std::cout << fibonacci_memo(i) << " ";
    }
    std::cout << std::endl;

    std::cout << "\n=== List Processing ===" << std::endl;
    std::vector<int> numbers = {1, 2, 3, 4, 5};
    std::cout << "sum_vector = " << sum_vector(numbers) << std::endl;
    std::cout << "length = " << length(numbers) << std::endl;
    std::cout << "contains(3) = " << (contains(numbers, 3) ? "true" : "false") << std::endl;
    std::cout << "contains(9) = " << (contains(numbers, 9) ? "true" : "false") << std::endl;

    std::cout << "\n=== Binary Search ===" << std::endl;
    std::vector<int> sorted_arr = {1, 3, 5, 7, 9, 11, 13, 15};
    std::cout << "binary_search(arr, 7) = " << binary_search(sorted_arr, 7) << std::endl;
    std::cout << "binary_search(arr, 10) = " << binary_search(sorted_arr, 10) << std::endl;

    std::cout << "\n=== Tail Recursion ===" << std::endl;
    std::cout << "factorial_tail(5) = " << factorial_tail(5) << std::endl;

    std::cout << "\n=== Towers of Hanoi (3 disks) ===" << std::endl;
    hanoi(3, 'A', 'C', 'B');

    std::cout << "\n=== Mutual Recursion ===" << std::endl;
    for (int i = 0; i < 5; i++) {
        std::cout << "is_even(" << i << ") = " << (is_even(i) ? "true" : "false")
                  << ", is_odd(" << i << ") = " << (is_odd(i) ? "true" : "false") << std::endl;
    }

    std::cout << "\n=== String Reversal ===" << std::endl;
    std::cout << "reverse(\"hello\") = " << reverse_string("hello") << std::endl;

    return 0;
}
