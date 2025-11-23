/**
 * Lesson 8: Higher-Order Functions in Java
 *
 * Java 8+ has functional programming support:
 * - Lambda expressions
 * - Functional interfaces
 * - Stream API (map, filter, reduce)
 * - Method references
 * - Optional type
 */

import java.util.*;
import java.util.function.*;
import java.util.stream.*;

public class HOFDemo {

    // ====================
    // 1. Functional Interfaces
    // ====================

    @FunctionalInterface
    interface Operation {
        int apply(int x);
    }

    static void demonstrateFunctionalInterfaces() {
        // Lambda implementing functional interface
        Operation doubler = x -> x * 2;
        Operation squarer = x -> x * x;

        System.out.println("   doubler.apply(5) = " + doubler.apply(5));
        System.out.println("   squarer.apply(5) = " + squarer.apply(5));

        // Built-in functional interfaces
        Function<Integer, Integer> addOne = x -> x + 1;
        Predicate<Integer> isEven = x -> x % 2 == 0;
        Consumer<String> printer = s -> System.out.println("   " + s);

        printer.accept("Hello from lambda!");
    }

    // ====================
    // 2. Functions Taking Functions
    // ====================

    static int applyTwice(Function<Integer, Integer> func, int x) {
        return func.apply(func.apply(x));
    }

    static int applyNTimes(Function<Integer, Integer> func, int n, int x) {
        int result = x;
        for (int i = 0; i < n; i++) {
            result = func.apply(result);
        }
        return result;
    }

    // ====================
    // 3. Functions Returning Functions
    // ====================

    static Function<Integer, Integer> makeMultiplier(int n) {
        return x -> x * n;
    }

    static Function<Integer, Integer> makeAdder(int n) {
        return x -> x + n;
    }

    // ====================
    // 4. Map - Transform Each Element
    // ====================

    static void demonstrateMap() {
        List<Integer> numbers = Arrays.asList(1, 2, 3, 4, 5);

        // Map with lambda
        List<Integer> doubled = numbers.stream()
            .map(x -> x * 2)
            .collect(Collectors.toList());
        System.out.println("   Doubled: " + doubled);

        // Map with method reference
        List<String> strings = Arrays.asList("hello", "world");
        List<Integer> lengths = strings.stream()
            .map(String::length)
            .collect(Collectors.toList());
        System.out.println("   Lengths: " + lengths);

        // Chaining maps
        List<Integer> result = numbers.stream()
            .map(x -> x + 1)
            .map(x -> x * 2)
            .collect(Collectors.toList());
        System.out.println("   Chained: " + result);
    }

    // ====================
    // 5. Filter - Select Elements
    // ====================

    static void demonstrateFilter() {
        List<Integer> numbers = IntStream.rangeClosed(1, 10)
            .boxed()
            .collect(Collectors.toList());

        // Filter evens
        List<Integer> evens = numbers.stream()
            .filter(x -> x % 2 == 0)
            .collect(Collectors.toList());
        System.out.println("   Evens: " + evens);

        // Filter with complex predicate
        List<Integer> bigOdds = numbers.stream()
            .filter(x -> x % 2 != 0 && x > 5)
            .collect(Collectors.toList());
        System.out.println("   Big odds: " + bigOdds);
    }

    // ====================
    // 6. Reduce - Combine to Single Value
    // ====================

    static void demonstrateReduce() {
        List<Integer> numbers = Arrays.asList(1, 2, 3, 4, 5);

        // Sum
        int sum = numbers.stream()
            .reduce(0, (acc, x) -> acc + x);
        System.out.println("   Sum: " + sum);

        // Product
        int product = numbers.stream()
            .reduce(1, (acc, x) -> acc * x);
        System.out.println("   Product: " + product);

        // Maximum
        Optional<Integer> max = numbers.stream()
            .reduce((a, b) -> a > b ? a : b);
        System.out.println("   Max: " + max.orElse(0));

        // Build a string
        List<String> words = Arrays.asList("Hello", "world", "from", "Java");
        String sentence = words.stream()
            .reduce("", (acc, word) -> acc + " " + word);
        System.out.println("   Sentence:" + sentence);

        // Count frequencies
        List<String> fruits = Arrays.asList("apple", "banana", "apple", "orange", "banana");
        Map<String, Long> counts = fruits.stream()
            .collect(Collectors.groupingBy(f -> f, Collectors.counting()));
        System.out.println("   Fruit counts: " + counts);
    }

    // ====================
    // 7. Closures
    // ====================

    static Supplier<Integer> makeCounter() {
        // Java doesn't have mutable closures the same way
        // Use array or AtomicInteger as workaround
        final int[] count = {0};

        return () -> {
            count[0]++;
            return count[0];
        };
    }

    static class BankAccount {
        private int balance;

        public BankAccount(int initial) {
            this.balance = initial;
        }

        public void deposit(int amount) {
            if (amount > 0) balance += amount;
        }

        public void withdraw(int amount) {
            if (amount > 0 && amount <= balance) balance -= amount;
        }

        public int getBalance() {
            return balance;
        }
    }

    // ====================
    // 8. Function Composition
    // ====================

    static void demonstrateComposition() {
        Function<Integer, Integer> addOne = x -> x + 1;
        Function<Integer, Integer> doubleIt = x -> x * 2;

        // Compose: f(g(x))
        Function<Integer, Integer> composed = doubleIt.compose(addOne);
        System.out.println("   doubleIt.compose(addOne)(5) = " + composed.apply(5));

        // AndThen: g(f(x))
        Function<Integer, Integer> andThen = addOne.andThen(doubleIt);
        System.out.println("   addOne.andThen(doubleIt)(5) = " + andThen.apply(5));

        // Chain multiple
        Function<Integer, Integer> pipeline = addOne
            .andThen(doubleIt)
            .andThen(x -> x * x);
        System.out.println("   Complex pipeline(5) = " + pipeline.apply(5));
    }

    // ====================
    // 9. Common Stream Operations
    // ====================

    static void demonstrateCommon() {
        List<Integer> numbers = Arrays.asList(1, 2, 3, 4, 5);

        // allMatch - all elements satisfy predicate
        boolean allPositive = numbers.stream()
            .allMatch(x -> x > 0);
        System.out.println("   All positive? " + allPositive);

        // anyMatch - any element satisfies predicate
        boolean hasEven = numbers.stream()
            .anyMatch(x -> x % 2 == 0);
        System.out.println("   Has even? " + hasEven);

        // findFirst - first element
        Optional<Integer> first = numbers.stream()
            .filter(x -> x > 3)
            .findFirst();
        System.out.println("   First > 3: " + first.orElse(-1));

        // sorted - with comparator
        List<String> words = Arrays.asList("apple", "pie", "zoo", "a");
        List<String> byLength = words.stream()
            .sorted(Comparator.comparingInt(String::length))
            .collect(Collectors.toList());
        System.out.println("   Sorted by length: " + byLength);

        // distinct - unique elements
        List<Integer> unique = Arrays.asList(1, 2, 2, 3, 3, 3).stream()
            .distinct()
            .collect(Collectors.toList());
        System.out.println("   Distinct: " + unique);

        // limit - first n elements
        List<Integer> firstThree = numbers.stream()
            .limit(3)
            .collect(Collectors.toList());
        System.out.println("   Limit 3: " + firstThree);
    }

    // ====================
    // 10. Method References
    // ====================

    static void demonstrateMethodReferences() {
        List<String> strings = Arrays.asList("hello", "world", "java");

        // Instance method reference
        List<String> upper = strings.stream()
            .map(String::toUpperCase)
            .collect(Collectors.toList());
        System.out.println("   Uppercase: " + upper);

        // Static method reference
        List<Integer> numbers = Arrays.asList(1, 2, 3, 4, 5);
        int sum = numbers.stream()
            .reduce(0, Integer::sum);
        System.out.println("   Sum (method ref): " + sum);

        // Constructor reference
        List<Integer> lengths = strings.stream()
            .map(String::length)
            .collect(Collectors.toList());
        System.out.println("   Lengths: " + lengths);
    }

    // ====================
    // 11. Parallel Streams
    // ====================

    static void demonstrateParallel() {
        List<Integer> numbers = IntStream.rangeClosed(1, 1000)
            .boxed()
            .collect(Collectors.toList());

        // Sequential
        long start = System.nanoTime();
        int sum1 = numbers.stream()
            .map(x -> x * x)
            .reduce(0, Integer::sum);
        long sequential = System.nanoTime() - start;

        // Parallel
        start = System.nanoTime();
        int sum2 = numbers.parallelStream()
            .map(x -> x * x)
            .reduce(0, Integer::sum);
        long parallel = System.nanoTime() - start;

        System.out.println("   Sequential sum: " + sum1 + " (" + sequential/1000 + "μs)");
        System.out.println("   Parallel sum: " + sum2 + " (" + parallel/1000 + "μs)");
    }

    // ====================
    // 12. Custom Higher-Order Functions
    // ====================

    static <T, R> List<R> customMap(List<T> list, Function<T, R> func) {
        List<R> result = new ArrayList<>();
        for (T item : list) {
            result.add(func.apply(item));
        }
        return result;
    }

    static <T> List<T> customFilter(List<T> list, Predicate<T> predicate) {
        List<T> result = new ArrayList<>();
        for (T item : list) {
            if (predicate.test(item)) {
                result.add(item);
            }
        }
        return result;
    }

    static <T, R> R customReduce(List<T> list, R identity, BiFunction<R, T, R> accumulator) {
        R result = identity;
        for (T item : list) {
            result = accumulator.apply(result, item);
        }
        return result;
    }

    // ====================
    // 13. Real-World Example: Data Pipeline
    // ====================

    static class User {
        String name;
        int age;
        boolean active;

        User(String name, int age, boolean active) {
            this.name = name;
            this.age = age;
            this.active = active;
        }

        @Override
        public String toString() {
            return String.format("{name=%s, age=%d, active=%b}", name, age, active);
        }
    }

    static List<User> processUsers(List<User> users) {
        return users.stream()
            .filter(u -> u.active)
            .map(u -> {
                u.name = u.name.trim().toLowerCase();
                return u;
            })
            .sorted((a, b) -> Integer.compare(b.age, a.age))
            .limit(10)
            .collect(Collectors.toList());
    }

    // ====================
    // Main Demonstration
    // ====================

    public static void main(String[] args) {
        System.out.println("=== Higher-Order Functions in Java ===\n");

        // 1. Functional interfaces
        System.out.println("1. Functional Interfaces and Lambdas:");
        demonstrateFunctionalInterfaces();

        // 2. Functions taking functions
        System.out.println("\n2. Functions Taking Functions:");
        System.out.println("   applyTwice(x -> x + 1, 5) = " +
            applyTwice(x -> x + 1, 5));
        System.out.println("   applyNTimes(x -> x * 2, 3, 2) = " +
            applyNTimes(x -> x * 2, 3, 2));

        // 3. Functions returning functions
        System.out.println("\n3. Functions Returning Functions:");
        Function<Integer, Integer> timesThree = makeMultiplier(3);
        Function<Integer, Integer> addTen = makeAdder(10);
        System.out.println("   timesThree(7) = " + timesThree.apply(7));
        System.out.println("   addTen(5) = " + addTen.apply(5));

        // 4. Map
        System.out.println("\n4. Map - Transform Each Element:");
        demonstrateMap();

        // 5. Filter
        System.out.println("\n5. Filter - Select Elements:");
        demonstrateFilter();

        // 6. Reduce
        System.out.println("\n6. Reduce - Combine to Single Value:");
        demonstrateReduce();

        // 7. Closures
        System.out.println("\n7. Closures:");
        Supplier<Integer> counter = makeCounter();
        System.out.println("   counter() = " + counter.get());
        System.out.println("   counter() = " + counter.get());
        System.out.println("   counter() = " + counter.get());

        BankAccount account = new BankAccount(1000);
        System.out.println("   Initial balance: $" + account.getBalance());
        account.deposit(500);
        System.out.println("   After deposit: $" + account.getBalance());
        account.withdraw(200);
        System.out.println("   After withdrawal: $" + account.getBalance());

        // 8. Function composition
        System.out.println("\n8. Function Composition:");
        demonstrateComposition();

        // 9. Common stream operations
        System.out.println("\n9. Common Stream Operations:");
        demonstrateCommon();

        // 10. Method references
        System.out.println("\n10. Method References:");
        demonstrateMethodReferences();

        // 11. Parallel streams
        System.out.println("\n11. Parallel Streams:");
        demonstrateParallel();

        // 12. Custom implementations
        System.out.println("\n12. Custom HOF Implementations:");
        List<Integer> numbers = Arrays.asList(1, 2, 3, 4, 5);
        System.out.println("   customMap: " + customMap(numbers, x -> x * 2));
        System.out.println("   customFilter: " + customFilter(numbers, x -> x % 2 == 0));
        System.out.println("   customReduce: " + customReduce(numbers, 0, (a, b) -> a + b));

        // 13. Real-world example
        System.out.println("\n13. Real-World Data Pipeline:");
        List<User> users = Arrays.asList(
            new User(" Alice ", 25, true),
            new User("BOB", 17, true),
            new User("Charlie ", 30, false),
            new User("DIANA", 28, true)
        );
        List<User> processed = processUsers(users);
        System.out.println("   Processed: " + processed);

        // 14. Method chaining
        System.out.println("\n14. Method Chaining:");
        int result = Arrays.asList(1, -2, 3, -4, 5, 6, 7, 8, 9, 10).stream()
            .filter(x -> x > 0)
            .map(x -> x * x)
            .reduce(0, Integer::sum);
        System.out.println("   Sum of squares of positive numbers: " + result);

        System.out.println("\n15. Java's Functional Features (Since Java 8):");
        System.out.println("   - Lambda expressions");
        System.out.println("   - Stream API (map, filter, reduce, etc.)");
        System.out.println("   - Method references (::)");
        System.out.println("   - Functional interfaces");
        System.out.println("   - Optional type for null safety");
        System.out.println("   - Parallel streams for easy parallelization");
    }
}
