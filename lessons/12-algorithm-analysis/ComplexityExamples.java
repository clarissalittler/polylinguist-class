/**
 * Lesson 12: Algorithm Complexity Examples in Java
 *
 * Demonstrates various complexity classes with working examples.
 */
public class ComplexityExamples {

    // ========================================================================
    // O(1) - CONSTANT TIME
    // ========================================================================

    /**
     * O(1) - Array access is constant time
     */
    public static int constantTimeAccess(int[] arr, int index) {
        return arr[index];
    }

    // ========================================================================
    // O(log n) - LOGARITHMIC TIME
    // ========================================================================

    /**
     * O(log n) - Binary search on sorted array
     * Each iteration eliminates half the remaining elements
     */
    public static int binarySearch(int[] sortedArr, int target) {
        int left = 0;
        int right = sortedArr.length - 1;

        while (left <= right) {
            int mid = left + (right - left) / 2;

            if (sortedArr[mid] == target) {
                return mid;
            } else if (sortedArr[mid] < target) {
                left = mid + 1;
            } else {
                right = mid - 1;
            }
        }

        return -1;  // Not found
    }

    /**
     * O(log n) - Find largest power of 2 less than n
     */
    public static int powerOfTwoBelow(int n) {
        int power = 1;
        while (power * 2 <= n) {
            power *= 2;
        }
        return power;
    }

    // ========================================================================
    // O(n) - LINEAR TIME
    // ========================================================================

    /**
     * O(n) - Linear search through array
     */
    public static int linearSearch(int[] arr, int target) {
        for (int i = 0; i < arr.length; i++) {
            if (arr[i] == target) {
                return i;
            }
        }
        return -1;
    }

    /**
     * O(n) - Find maximum element
     */
    public static int findMax(int[] arr) {
        if (arr.length == 0) {
            throw new IllegalArgumentException("Array is empty");
        }

        int maxVal = arr[0];
        for (int num : arr) {
            if (num > maxVal) {
                maxVal = num;
            }
        }
        return maxVal;
    }

    /**
     * O(n) - Sum all elements
     */
    public static int sumArray(int[] arr) {
        int total = 0;
        for (int num : arr) {
            total += num;
        }
        return total;
    }

    // ========================================================================
    // O(n log n) - LOG-LINEAR TIME
    // ========================================================================

    /**
     * O(n log n) - Merge sort
     * Divides array log(n) times, merges in O(n) at each level
     */
    public static int[] mergeSort(int[] arr) {
        if (arr.length <= 1) {
            return arr;
        }

        // Divide
        int mid = arr.length / 2;
        int[] left = new int[mid];
        int[] right = new int[arr.length - mid];

        System.arraycopy(arr, 0, left, 0, mid);
        System.arraycopy(arr, mid, right, 0, arr.length - mid);

        left = mergeSort(left);
        right = mergeSort(right);

        // Conquer (merge)
        return merge(left, right);
    }

    /**
     * Helper for mergeSort - O(n) to merge two sorted arrays
     */
    private static int[] merge(int[] left, int[] right) {
        int[] result = new int[left.length + right.length];
        int i = 0, j = 0, k = 0;

        while (i < left.length && j < right.length) {
            if (left[i] <= right[j]) {
                result[k++] = left[i++];
            } else {
                result[k++] = right[j++];
            }
        }

        while (i < left.length) {
            result[k++] = left[i++];
        }

        while (j < right.length) {
            result[k++] = right[j++];
        }

        return result;
    }

    // ========================================================================
    // O(n²) - QUADRATIC TIME
    // ========================================================================

    /**
     * O(n²) - Bubble sort (simple but slow)
     */
    public static int[] bubbleSort(int[] arr) {
        int[] result = arr.clone();
        int n = result.length;

        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n - i - 1; j++) {
                if (result[j] > result[j + 1]) {
                    // Swap
                    int temp = result[j];
                    result[j] = result[j + 1];
                    result[j + 1] = temp;
                }
            }
        }

        return result;
    }

    /**
     * O(n²) - Count pairs that sum to target
     */
    public static int countPairsSum(int[] arr, int target) {
        int count = 0;
        for (int i = 0; i < arr.length; i++) {
            for (int j = i + 1; j < arr.length; j++) {
                if (arr[i] + arr[j] == target) {
                    count++;
                }
            }
        }
        return count;
    }

    // ========================================================================
    // O(2^n) - EXPONENTIAL TIME
    // ========================================================================

    /**
     * O(2^n) - Naive Fibonacci (VERY SLOW!)
     * Each call makes two recursive calls, creating exponential tree
     */
    public static int fibonacciSlow(int n) {
        if (n <= 1) {
            return n;
        }
        return fibonacciSlow(n - 1) + fibonacciSlow(n - 2);
    }

    // ========================================================================
    // IMPROVED VERSIONS
    // ========================================================================

    /**
     * O(n) - Fibonacci with memoization (much faster!)
     */
    public static int fibonacciFast(int n) {
        int[] memo = new int[n + 1];
        return fibonacciMemo(n, memo);
    }

    private static int fibonacciMemo(int n, int[] memo) {
        if (n <= 1) {
            return n;
        }

        if (memo[n] != 0) {
            return memo[n];
        }

        memo[n] = fibonacciMemo(n - 1, memo) + fibonacciMemo(n - 2, memo);
        return memo[n];
    }

    /**
     * O(n) time, O(1) space - Iterative Fibonacci (most efficient!)
     */
    public static int fibonacciIterative(int n) {
        if (n <= 1) {
            return n;
        }

        int a = 0, b = 1;
        for (int i = 2; i <= n; i++) {
            int temp = b;
            b = a + b;
            a = temp;
        }
        return b;
    }

    // ========================================================================
    // DEMONSTRATION
    // ========================================================================

    public static void main(String[] args) {
        System.out.println("=" .repeat(60));
        System.out.println("ALGORITHM COMPLEXITY DEMONSTRATION (Java)");
        System.out.println("=".repeat(60));

        // Test binary search vs linear search
        System.out.println("\nBinary Search vs Linear Search:");
        System.out.println("-".repeat(40));

        int[] sizes = {1000, 10000, 100000};

        for (int size : sizes) {
            int[] arr = new int[size];
            for (int i = 0; i < size; i++) {
                arr[i] = i;
            }

            int target = size - 1;  // Worst case for linear search

            // Linear search
            long startLinear = System.nanoTime();
            linearSearch(arr, target);
            long endLinear = System.nanoTime();
            double linearTime = (endLinear - startLinear) / 1_000_000.0;

            // Binary search
            long startBinary = System.nanoTime();
            binarySearch(arr, target);
            long endBinary = System.nanoTime();
            double binaryTime = (endBinary - startBinary) / 1_000_000.0;

            double speedup = linearTime / binaryTime;

            System.out.printf("n = %6d: Linear = %8.4f ms, Binary = %8.4f ms (%.0fx faster)%n",
                    size, linearTime, binaryTime, speedup);
        }

        // Test sorting algorithms
        System.out.println("\nBubble Sort vs Merge Sort:");
        System.out.println("-".repeat(40));

        int[] sortSizes = {100, 500, 1000};

        for (int size : sortSizes) {
            int[] arr = new int[size];
            for (int i = 0; i < size; i++) {
                arr[i] = size - i;  // Reverse sorted (worst case)
            }

            // Bubble sort
            long startBubble = System.nanoTime();
            bubbleSort(arr);
            long endBubble = System.nanoTime();
            double bubbleTime = (endBubble - startBubble) / 1_000_000.0;

            // Merge sort
            long startMerge = System.nanoTime();
            mergeSort(arr);
            long endMerge = System.nanoTime();
            double mergeTime = (endMerge - startMerge) / 1_000_000.0;

            double speedup = bubbleTime / mergeTime;

            System.out.printf("n = %4d: Bubble = %8.4f ms, Merge = %8.4f ms (%.0fx faster)%n",
                    size, bubbleTime, mergeTime, speedup);
        }

        // Test Fibonacci implementations
        System.out.println("\nFibonacci Implementations:");
        System.out.println("-".repeat(40));

        int n = 30;
        System.out.println("Computing Fibonacci(" + n + "):");

        long startSlow = System.nanoTime();
        int resultSlow = fibonacciSlow(n);
        long endSlow = System.nanoTime();
        double slowTime = (endSlow - startSlow) / 1_000_000.0;

        long startFast = System.nanoTime();
        int resultFast = fibonacciFast(n);
        long endFast = System.nanoTime();
        double fastTime = (endFast - startFast) / 1_000_000.0;

        long startIter = System.nanoTime();
        int resultIter = fibonacciIterative(n);
        long endIter = System.nanoTime();
        double iterTime = (endIter - startIter) / 1_000_000.0;

        System.out.printf("Naive O(2^n):       %8.2f ms (result: %d)%n", slowTime, resultSlow);
        System.out.printf("Memoized O(n):      %8.2f ms (result: %d)%n", fastTime, resultFast);
        System.out.printf("Iterative O(n):     %8.2f ms (result: %d)%n", iterTime, resultIter);
        System.out.printf("Speedup: %.0fx faster!%n", slowTime / iterTime);

        System.out.println("\n" + "=".repeat(60));
        System.out.println("Key Insight: Algorithm choice matters more than language!");
        System.out.println("=".repeat(60));
    }
}
