/**
 * Lesson 13: Sorting and Searching - C++ Examples
 * Implementations and comparisons of fundamental algorithms
 */

#include <iostream>
#include <vector>
#include <algorithm>
#include <chrono>
#include <random>
#include <functional>

using namespace std;
using namespace std::chrono;

// =============================================================================
// SEARCHING ALGORITHMS
// =============================================================================

template<typename T>
int linear_search(const vector<T>& arr, const T& target) {
    // O(n) - Works on any array
    for (size_t i = 0; i < arr.size(); i++) {
        if (arr[i] == target) {
            return i;
        }
    }
    return -1;
}

template<typename T>
int binary_search_impl(const vector<T>& arr, const T& target) {
    // O(log n) - Requires sorted input
    int left = 0;
    int right = arr.size() - 1;

    while (left <= right) {
        int mid = left + (right - left) / 2;
        if (arr[mid] == target) {
            return mid;
        } else if (arr[mid] < target) {
            left = mid + 1;
        } else {
            right = mid - 1;
        }
    }
    return -1;
}

template<typename T>
int binary_search_recursive(const vector<T>& arr, const T& target, int left, int right) {
    if (left > right) return -1;

    int mid = left + (right - left) / 2;
    if (arr[mid] == target) return mid;
    if (arr[mid] < target) {
        return binary_search_recursive(arr, target, mid + 1, right);
    }
    return binary_search_recursive(arr, target, left, mid - 1);
}

void search_demo() {
    cout << "=== Searching Algorithms ===" << endl;

    vector<int> numbers = {1, 3, 5, 7, 9, 11, 13, 15, 17, 19};
    cout << "Array: ";
    for (int n : numbers) cout << n << " ";
    cout << endl;

    cout << "Linear search for 11: index " << linear_search(numbers, 11) << endl;
    cout << "Binary search for 11: index " << binary_search_impl(numbers, 11) << endl;
    cout << "Binary search for 10: index " << binary_search_impl(numbers, 10) << endl;
}

// =============================================================================
// BASIC SORTING ALGORITHMS - O(n²)
// =============================================================================

template<typename T>
void bubble_sort(vector<T>& arr) {
    // O(n²) - Simple but inefficient
    int n = arr.size();
    for (int i = 0; i < n; i++) {
        bool swapped = false;
        for (int j = 0; j < n - i - 1; j++) {
            if (arr[j] > arr[j + 1]) {
                swap(arr[j], arr[j + 1]);
                swapped = true;
            }
        }
        if (!swapped) break;  // Optimization
    }
}

template<typename T>
void selection_sort(vector<T>& arr) {
    // O(n²) - Finds minimum and places at front
    int n = arr.size();
    for (int i = 0; i < n; i++) {
        int min_idx = i;
        for (int j = i + 1; j < n; j++) {
            if (arr[j] < arr[min_idx]) {
                min_idx = j;
            }
        }
        swap(arr[i], arr[min_idx]);
    }
}

template<typename T>
void insertion_sort(vector<T>& arr) {
    // O(n²) - Good for small or nearly-sorted arrays
    for (size_t i = 1; i < arr.size(); i++) {
        T key = arr[i];
        int j = i - 1;
        while (j >= 0 && arr[j] > key) {
            arr[j + 1] = arr[j];
            j--;
        }
        arr[j + 1] = key;
    }
}

void basic_sort_demo() {
    cout << "\n=== O(n²) Sorting Algorithms ===" << endl;

    vector<int> original = {64, 34, 25, 12, 22, 11, 90};
    cout << "Original: ";
    for (int n : original) cout << n << " ";
    cout << endl;

    vector<int> arr = original;
    bubble_sort(arr);
    cout << "Bubble sort: ";
    for (int n : arr) cout << n << " ";
    cout << endl;

    arr = original;
    selection_sort(arr);
    cout << "Selection sort: ";
    for (int n : arr) cout << n << " ";
    cout << endl;

    arr = original;
    insertion_sort(arr);
    cout << "Insertion sort: ";
    for (int n : arr) cout << n << " ";
    cout << endl;
}

// =============================================================================
// EFFICIENT SORTING ALGORITHMS - O(n log n)
// =============================================================================

template<typename T>
void merge(vector<T>& arr, int left, int mid, int right) {
    vector<T> temp;
    int i = left, j = mid + 1;

    while (i <= mid && j <= right) {
        if (arr[i] <= arr[j]) {
            temp.push_back(arr[i++]);
        } else {
            temp.push_back(arr[j++]);
        }
    }
    while (i <= mid) temp.push_back(arr[i++]);
    while (j <= right) temp.push_back(arr[j++]);

    for (size_t k = 0; k < temp.size(); k++) {
        arr[left + k] = temp[k];
    }
}

template<typename T>
void merge_sort(vector<T>& arr, int left, int right) {
    // O(n log n) - Divide and conquer, stable
    if (left < right) {
        int mid = left + (right - left) / 2;
        merge_sort(arr, left, mid);
        merge_sort(arr, mid + 1, right);
        merge(arr, left, mid, right);
    }
}

template<typename T>
int partition(vector<T>& arr, int low, int high) {
    T pivot = arr[high];
    int i = low - 1;

    for (int j = low; j < high; j++) {
        if (arr[j] <= pivot) {
            i++;
            swap(arr[i], arr[j]);
        }
    }
    swap(arr[i + 1], arr[high]);
    return i + 1;
}

template<typename T>
void quick_sort(vector<T>& arr, int low, int high) {
    // O(n log n) average - In-place, not stable
    if (low < high) {
        int pivot_idx = partition(arr, low, high);
        quick_sort(arr, low, pivot_idx - 1);
        quick_sort(arr, pivot_idx + 1, high);
    }
}

template<typename T>
void heapify(vector<T>& arr, int n, int i) {
    int largest = i;
    int left = 2 * i + 1;
    int right = 2 * i + 2;

    if (left < n && arr[left] > arr[largest]) {
        largest = left;
    }
    if (right < n && arr[right] > arr[largest]) {
        largest = right;
    }
    if (largest != i) {
        swap(arr[i], arr[largest]);
        heapify(arr, n, largest);
    }
}

template<typename T>
void heap_sort(vector<T>& arr) {
    // O(n log n) - Uses binary heap
    int n = arr.size();

    // Build max heap
    for (int i = n / 2 - 1; i >= 0; i--) {
        heapify(arr, n, i);
    }

    // Extract elements
    for (int i = n - 1; i > 0; i--) {
        swap(arr[0], arr[i]);
        heapify(arr, i, 0);
    }
}

void efficient_sort_demo() {
    cout << "\n=== O(n log n) Sorting Algorithms ===" << endl;

    vector<int> original = {64, 34, 25, 12, 22, 11, 90};
    cout << "Original: ";
    for (int n : original) cout << n << " ";
    cout << endl;

    vector<int> arr = original;
    merge_sort(arr, 0, arr.size() - 1);
    cout << "Merge sort: ";
    for (int n : arr) cout << n << " ";
    cout << endl;

    arr = original;
    quick_sort(arr, 0, arr.size() - 1);
    cout << "Quick sort: ";
    for (int n : arr) cout << n << " ";
    cout << endl;

    arr = original;
    heap_sort(arr);
    cout << "Heap sort: ";
    for (int n : arr) cout << n << " ";
    cout << endl;
}

// =============================================================================
// COUNTING SORT
// =============================================================================

void counting_sort(vector<int>& arr) {
    // O(n + k) where k is range
    if (arr.empty()) return;

    int max_val = *max_element(arr.begin(), arr.end());
    vector<int> count(max_val + 1, 0);

    for (int num : arr) {
        count[num]++;
    }

    int idx = 0;
    for (int num = 0; num <= max_val; num++) {
        while (count[num]-- > 0) {
            arr[idx++] = num;
        }
    }
}

void counting_sort_demo() {
    cout << "\n=== Counting Sort O(n + k) ===" << endl;

    vector<int> arr = {4, 2, 2, 8, 3, 3, 1};
    cout << "Original: ";
    for (int n : arr) cout << n << " ";
    cout << endl;

    counting_sort(arr);
    cout << "Counting sort: ";
    for (int n : arr) cout << n << " ";
    cout << endl;
}

// =============================================================================
// BENCHMARK
// =============================================================================

void benchmark() {
    cout << "\n=== Performance Comparison (n=1000) ===" << endl;

    random_device rd;
    mt19937 gen(rd());
    uniform_int_distribution<> dis(0, 10000);

    vector<int> data(1000);
    for (auto& x : data) x = dis(gen);

    // Bubble sort
    {
        vector<int> arr = data;
        auto start = high_resolution_clock::now();
        bubble_sort(arr);
        auto end = high_resolution_clock::now();
        cout << "Bubble sort: " << duration<double>(end - start).count() << "s" << endl;
    }

    // Insertion sort
    {
        vector<int> arr = data;
        auto start = high_resolution_clock::now();
        insertion_sort(arr);
        auto end = high_resolution_clock::now();
        cout << "Insertion sort: " << duration<double>(end - start).count() << "s" << endl;
    }

    // Merge sort
    {
        vector<int> arr = data;
        auto start = high_resolution_clock::now();
        merge_sort(arr, 0, arr.size() - 1);
        auto end = high_resolution_clock::now();
        cout << "Merge sort: " << duration<double>(end - start).count() << "s" << endl;
    }

    // Quick sort
    {
        vector<int> arr = data;
        auto start = high_resolution_clock::now();
        quick_sort(arr, 0, arr.size() - 1);
        auto end = high_resolution_clock::now();
        cout << "Quick sort: " << duration<double>(end - start).count() << "s" << endl;
    }

    // std::sort
    {
        vector<int> arr = data;
        auto start = high_resolution_clock::now();
        sort(arr.begin(), arr.end());
        auto end = high_resolution_clock::now();
        cout << "std::sort: " << duration<double>(end - start).count() << "s" << endl;
    }
}

// =============================================================================
// STL ALGORITHMS
// =============================================================================

void stl_demo() {
    cout << "\n=== C++ STL Sorting & Searching ===" << endl;

    vector<int> arr = {5, 2, 8, 1, 9, 3, 7};

    // std::sort - Introsort O(n log n)
    sort(arr.begin(), arr.end());
    cout << "std::sort: ";
    for (int n : arr) cout << n << " ";
    cout << endl;

    // std::binary_search
    cout << "binary_search for 5: " << boolalpha
         << binary_search(arr.begin(), arr.end(), 5) << endl;

    // std::lower_bound / upper_bound
    auto lb = lower_bound(arr.begin(), arr.end(), 5);
    cout << "lower_bound(5): index " << (lb - arr.begin()) << endl;

    // std::stable_sort
    vector<pair<int, string>> students = {{85, "Alice"}, {90, "Bob"}, {85, "Charlie"}};
    stable_sort(students.begin(), students.end(),
                [](auto& a, auto& b) { return a.first < b.first; });
    cout << "Stable sort students by grade: ";
    for (auto& s : students) cout << s.second << " ";
    cout << endl;

    // std::partial_sort - Only sort first k elements
    vector<int> arr2 = {5, 2, 8, 1, 9, 3, 7};
    partial_sort(arr2.begin(), arr2.begin() + 3, arr2.end());
    cout << "partial_sort (first 3): ";
    for (int n : arr2) cout << n << " ";
    cout << endl;

    // std::nth_element - Find kth smallest
    vector<int> arr3 = {5, 2, 8, 1, 9, 3, 7};
    nth_element(arr3.begin(), arr3.begin() + 3, arr3.end());
    cout << "4th smallest element: " << arr3[3] << endl;
}

// =============================================================================
// MAIN
// =============================================================================

int main() {
    search_demo();
    basic_sort_demo();
    efficient_sort_demo();
    counting_sort_demo();
    benchmark();
    stl_demo();

    cout << "\n=== When to Use Each Algorithm ===" << endl;
    cout << R"(
Binary Search: O(log n) - Sorted data, repeated searches
Linear Search: O(n) - Unsorted data, single search

Bubble/Selection: O(n²) - Educational, tiny arrays
Insertion Sort: O(n²) - Small arrays, nearly sorted data

Merge Sort: O(n log n) - Stability needed, linked lists
Quick Sort: O(n log n) avg - General purpose, in-memory
Heap Sort: O(n log n) - Guaranteed performance

std::sort: Use this! Highly optimized introsort
std::stable_sort: When order of equal elements matters
)" << endl;

    return 0;
}
