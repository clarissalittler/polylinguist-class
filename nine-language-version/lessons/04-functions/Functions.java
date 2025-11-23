/*
 * Lesson 4: Functions in Java
 * Demonstrates methods, lambdas (Java 8+), and functional interfaces
 */

import java.util.Arrays;
import java.util.List;
import java.util.function.*;
import java.util.stream.Collectors;

public class Functions {

    // ============================================
    // 1. Basic Method Definition
    // ============================================

    public static String greet(String name) {
        return "Hello, " + name + "!";
    }

    public static int add(int x, int y) {
        return x + y;
    }

    public static int square(int x) {
        return x * x;
    }

    // ============================================
    // 2. Method Overloading (simulates default parameters)
    // ============================================

    public static String describePerson(String name, int age) {
        return describePerson(name, age, "Unknown");
    }

    public static String describePerson(String name, int age, String city) {
        return name + " is " + age + " years old and lives in " + city;
    }

    // Overloading with different types
    public static double add(double x, double y) {
        return x + y;
    }

    // ============================================
    // 3. Varargs (variadic functions)
    // ============================================

    public static int sumAll(int... numbers) {
        int total = 0;
        for (int n : numbers) {
            total += n;
        }
        return total;
    }

    // ============================================
    // 4. Return Values
    // ============================================

    // Java can only return one value
    // Use objects/arrays for multiple returns
    public static class DivisionResult {
        public Double result;
        public String error;

        public DivisionResult(Double result, String error) {
            this.result = result;
            this.error = error;
        }
    }

    public static DivisionResult divide(int x, int y) {
        if (y == 0) {
            return new DivisionResult(null, "Cannot divide by zero");
        }
        return new DivisionResult((double) x / y, null);
    }

    // ============================================
    // 5. Pure vs Impure Methods
    // ============================================

    // Pure method
    public static int pureAdd(int x, int y) {
        return x + y;
    }

    // Impure method (side effect)
    public static void impurePrint(String message) {
        System.out.println(message);
    }

    // Impure method (depends on/modifies static state)
    private static int total = 0;
    public static int impureAddToTotal(int x) {
        total += x;
        return total;
    }

    // ============================================
    // 6. First-Class Functions (Java 8+ Lambdas)
    // ============================================

    public static int applyOperation(BiFunction<Integer, Integer, Integer> operation, int x, int y) {
        return operation.apply(x, y);
    }

    public static Function<Integer, Integer> compose(Function<Integer, Integer> f, Function<Integer, Integer> g) {
        return x -> f.apply(g.apply(x));
    }

    // ============================================
    // 7. Higher-Order Functions
    // ============================================

    public static int applyTwice(Function<Integer, Integer> f, int x) {
        return f.apply(f.apply(x));
    }

    public static int applyNTimes(Function<Integer, Integer> f, int x, int n) {
        int result = x;
        for (int i = 0; i < n; i++) {
            result = f.apply(result);
        }
        return result;
    }

    // ============================================
    // 8. Closures (Java 8+)
    // ============================================

    public static Function<Integer, Integer> makeMultiplier(int factor) {
        return x -> x * factor;  // Captures 'factor'
    }

    public static Supplier<Integer> makeCounter() {
        // Note: Can't capture mutable variable directly
        // Need to use array or wrapper class
        final int[] count = {0};

        return () -> {
            count[0]++;
            return count[0];
        };
    }

    // ============================================
    // Main Method
    // ============================================

    public static void main(String[] args) {
        System.out.println("=== Java Functions ===\n");

        // 1. Basic methods
        System.out.println("1. Basic Methods:");
        System.out.println("  greet('Alice'): " + greet("Alice"));
        System.out.println("  add(5, 3): " + add(5, 3));
        System.out.println("  square(7): " + square(7));

        // 2. Method overloading
        System.out.println("\n2. Method Overloading:");
        System.out.println("  describePerson('Alice', 30): " + describePerson("Alice", 30));
        System.out.println("  describePerson('Bob', 25, 'NYC'): " + describePerson("Bob", 25, "NYC"));
        System.out.println("  add(5.5, 3.2): " + add(5.5, 3.2));

        // 3. Varargs
        System.out.println("\n3. Varargs:");
        System.out.println("  sumAll(1, 2, 3, 4, 5): " + sumAll(1, 2, 3, 4, 5));

        // 4. Multiple returns (via object)
        System.out.println("\n4. Multiple Return Values (via object):");
        DivisionResult result1 = divide(10, 2);
        System.out.println("  divide(10, 2): result=" + result1.result + ", error=" + result1.error);
        DivisionResult result2 = divide(10, 0);
        System.out.println("  divide(10, 0): result=" + result2.result + ", error=" + result2.error);

        // 5. Pure vs Impure
        System.out.println("\n5. Pure vs Impure:");
        System.out.println("  pureAdd(5, 3): " + pureAdd(5, 3));
        System.out.print("  impurePrint('Hello'): ");
        impurePrint("Hello");
        total = 0;  // Reset
        System.out.println("  impureAddToTotal(5): " + impureAddToTotal(5));
        System.out.println("  impureAddToTotal(5): " + impureAddToTotal(5) + " (different!)");

        // 6. Lambdas and first-class functions
        System.out.println("\n6. Lambdas and First-Class Functions:");
        Function<Integer, Integer> doubleFunc = x -> x * 2;
        Function<Integer, Integer> squareFunc = x -> x * x;

        System.out.println("  doubleFunc.apply(5): " + doubleFunc.apply(5));
        System.out.println("  applyOperation((x,y) -> x+y, 5, 3): " +
            applyOperation((x, y) -> x + y, 5, 3));

        // 7. Function composition
        System.out.println("\n7. Function Composition:");
        Function<Integer, Integer> increment = x -> x + 1;
        Function<Integer, Integer> squareThenIncrement = compose(increment, squareFunc);
        System.out.println("  squareThenIncrement.apply(5): " + squareThenIncrement.apply(5));

        // 8. Closures
        System.out.println("\n8. Closures:");
        Function<Integer, Integer> timesTwo = makeMultiplier(2);
        Function<Integer, Integer> timesThree = makeMultiplier(3);
        System.out.println("  timesTwo.apply(5): " + timesTwo.apply(5));
        System.out.println("  timesThree.apply(5): " + timesThree.apply(5));

        System.out.println("\n  Counter (stateful closure):");
        Supplier<Integer> counter1 = makeCounter();
        Supplier<Integer> counter2 = makeCounter();
        System.out.println("  counter1.get(): " + counter1.get());  // 1
        System.out.println("  counter1.get(): " + counter1.get());  // 2
        System.out.println("  counter2.get(): " + counter2.get());  // 1
        System.out.println("  counter1.get(): " + counter1.get());  // 3

        // 9. Streams and higher-order functions
        System.out.println("\n9. Streams and Higher-Order Functions:");
        List<Integer> numbers = Arrays.asList(1, 2, 3, 4, 5);
        System.out.println("  numbers: " + numbers);

        List<Integer> squared = numbers.stream()
            .map(x -> x * x)
            .collect(Collectors.toList());
        System.out.println("  map(square): " + squared);

        List<Integer> evens = numbers.stream()
            .filter(x -> x % 2 == 0)
            .collect(Collectors.toList());
        System.out.println("  filter(isEven): " + evens);

        int sum = numbers.stream()
            .reduce(0, (acc, x) -> acc + x);
        System.out.println("  reduce(add): " + sum);

        // 10. Apply n times
        System.out.println("\n10. Apply Function Multiple Times:");
        System.out.println("  applyTwice(double, 5): " + applyTwice(doubleFunc, 5));
        System.out.println("  applyNTimes(double, 5, 3): " + applyNTimes(doubleFunc, 5, 3));
    }
}
