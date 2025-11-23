/**
 * Lesson 10: Type Systems in Java
 *
 * Java features:
 * - Strong static typing
 * - Nominal type system (names matter)
 * - Generics (with type erasure)
 * - Interfaces for polymorphism
 * - Type inference (Java 10+ with var)
 * - Primitive and reference types
 * - Autoboxing/unboxing
 * - Checked exceptions in types
 *
 * This demonstrates Java's type system.
 */

import java.util.*;
import java.util.function.*;

public class TypeSystems {

    // ====================
    // 1. Basic Types and Methods
    // ====================

    static int increment(int x) {
        return x + 1;
    }

    static int add(int x, int y) {
        return x + y;
    }

    // Type inference with var (Java 10+)
    static void demonstrateInference() {
        var x = 5;          // Inferred: int
        var s = "hello";    // Inferred: String
        var list = List.of(1, 2, 3);  // Inferred: List<Integer>

        System.out.println("   Inferred: x=" + x + ", s=" + s + ", list=" + list);
    }

    // ====================
    // 2. Algebraic Data Types (with Sealed Classes - Java 17+)
    // ====================

    sealed interface Shape permits Circle, Rectangle, Triangle {
        double area();
    }

    record Circle(double radius) implements Shape {
        public double area() {
            return Math.PI * radius * radius;
        }
    }

    record Rectangle(double width, double height) implements Shape {
        public double area() {
            return width * height;
        }
    }

    record Triangle(double a, double b, double c) implements Shape {
        public double area() {
            double s = (a + b + c) / 2;
            return Math.sqrt(s * (s - a) * (s - b) * (s - c));
        }
    }

    // ====================
    // 3. Records (Product Types - Java 14+)
    // ====================

    record Point(double x, double y) {
        double distance(Point other) {
            return Math.sqrt(Math.pow(x - other.x(), 2) + Math.pow(y - other.y(), 2));
        }
    }

    record Person(String name, int age) {
        boolean isAdult() {
            return age >= 18;
        }
    }

    // ====================
    // 4. Generic Types (Parametric Polymorphism)
    // ====================

    // Generic method
    static <T> T identity(T x) {
        return x;
    }

    // Generic class
    static class Pair<A, B> {
        private final A first;
        private final B second;

        public Pair(A first, B second) {
            this.first = first;
            this.second = second;
        }

        public A first() { return first; }
        public B second() { return second; }

        public <C, D> Pair<C, D> map(Function<A, C> f, Function<B, D> g) {
            return new Pair<>(f.apply(first), g.apply(second));
        }

        public Pair<B, A> swap() {
            return new Pair<>(second, first);
        }

        @Override
        public String toString() {
            return "Pair(" + first + ", " + second + ")";
        }
    }

    // ====================
    // 5. Optional Type (Null Safety)
    // ====================

    static Optional<Double> safeDivide(double x, double y) {
        if (y == 0) {
            return Optional.empty();
        }
        return Optional.of(x / y);
    }

    static <T> Optional<T> safeHead(List<T> list) {
        if (list.isEmpty()) {
            return Optional.empty();
        }
        return Optional.of(list.get(0));
    }

    // ====================
    // 6. Custom Result Type (Error Handling)
    // ====================

    sealed interface Result<T, E> permits Success, Failure {}

    record Success<T, E>(T value) implements Result<T, E> {}
    record Failure<T, E>(E error) implements Result<T, E> {}

    static Result<Double, String> divide(double x, double y) {
        if (y == 0) {
            return new Failure<>("Division by zero");
        }
        return new Success<>(x / y);
    }

    // ====================
    // 7. Interfaces (Type Classes)
    // ====================

    interface Describable {
        String describe();
    }

    static class DescribableCircle implements Describable {
        private final double radius;

        public DescribableCircle(double radius) {
            this.radius = radius;
        }

        public String describe() {
            return "Circle with radius " + radius;
        }
    }

    static class DescribablePoint implements Describable {
        private final Point point;

        public DescribablePoint(Point point) {
            this.point = point;
        }

        public String describe() {
            return "Point at (" + point.x() + ", " + point.y() + ")";
        }
    }

    static void printDescription(Describable d) {
        System.out.println("   " + d.describe());
    }

    // ====================
    // 8. Multiple Bounds
    // ====================

    static <T extends Comparable<T> & Cloneable> void demonstrateBounds(T value) {
        System.out.println("   Value with multiple bounds: " + value);
    }

    // ====================
    // 9. Wildcards (Variance)
    // ====================

    // Covariant (producer)
    static double sumAreas(List<? extends Shape> shapes) {
        return shapes.stream().mapToDouble(Shape::area).sum();
    }

    // Contravariant (consumer)
    static void addCircles(List<? super Circle> circles) {
        circles.add(new Circle(5.0));
    }

    // ====================
    // 10. Function Types
    // ====================

    static int applyTwice(Function<Integer, Integer> func, int x) {
        return func.apply(func.apply(x));
    }

    static Function<Integer, Integer> makeMultiplier(int n) {
        return x -> x * n;
    }

    // ====================
    // 11. Enum Types
    // ====================

    enum TrafficLight {
        RED, YELLOW, GREEN;

        TrafficLight next() {
            return switch(this) {
                case RED -> GREEN;
                case GREEN -> YELLOW;
                case YELLOW -> RED;
            };
        }
    }

    enum Color {
        RED("red"), GREEN("green"), BLUE("blue");

        private final String value;

        Color(String value) {
            this.value = value;
        }

        public String getValue() {
            return value;
        }
    }

    // ====================
    // 12. Type Aliases (Limited)
    // ====================

    // Java doesn't have type aliases, but we can use typedefs via inheritance
    static class UserId {
        private final int value;
        public UserId(int value) { this.value = value; }
        public int getValue() { return value; }
        @Override
        public String toString() { return "UserId(" + value + ")"; }
    }

    static class ProductId {
        private final int value;
        public ProductId(int value) { this.value = value; }
        public int getValue() { return value; }
        @Override
        public String toString() { return "ProductId(" + value + ")"; }
    }

    static String getUserName(UserId id) {
        return "User #" + id.getValue();
    }

    // ====================
    // 13. Streams and Functional Types
    // ====================

    static void demonstrateStreams() {
        var numbers = List.of(1, 2, 3, 4, 5);

        var doubled = numbers.stream()
            .map(x -> x * 2)
            .toList();

        var evens = numbers.stream()
            .filter(x -> x % 2 == 0)
            .toList();

        System.out.println("   Doubled: " + doubled);
        System.out.println("   Evens: " + evens);
    }

    // ====================
    // 14. Pattern Matching (Java 17+)
    // ====================

    static String describeShape(Shape shape) {
        return switch(shape) {
            case Circle c -> "Circle with radius " + c.radius();
            case Rectangle r -> "Rectangle " + r.width() + "x" + r.height();
            case Triangle t -> "Triangle with sides " + t.a() + ", " + t.b() + ", " + t.c();
        };
    }

    // ====================
    // 15. Checked Exceptions in Type Signatures
    // ====================

    static int parseIntChecked(String s) throws NumberFormatException {
        return Integer.parseInt(s);
    }

    // ====================
    // Main Demonstration
    // ====================

    public static void main(String[] args) {
        System.out.println("=== Type Systems in Java ===\n");

        // 1. Basic types
        System.out.println("1. Basic Types:");
        System.out.println("   increment(5) = " + increment(5));
        System.out.println("   add(3, 7) = " + add(3, 7));

        // 2. Type inference
        System.out.println("\n2. Type Inference (var):");
        demonstrateInference();

        // 3. Algebraic data types
        System.out.println("\n3. Sealed Types (Algebraic Data Types):");
        var shapes = List.of(
            new Circle(5.0),
            new Rectangle(4.0, 6.0),
            new Triangle(3.0, 4.0, 5.0)
        );
        for (var shape : shapes) {
            System.out.printf("   %s, area = %.2f%n", describeShape(shape), shape.area());
        }

        // 4. Records
        System.out.println("\n4. Records:");
        var person = new Person("Alice", 30);
        System.out.println("   " + person);
        System.out.println("   isAdult() = " + person.isAdult());

        var p1 = new Point(0, 0);
        var p2 = new Point(3, 4);
        System.out.printf("   distance(%s, %s) = %.2f%n", p1, p2, p1.distance(p2));

        // 5. Generics
        System.out.println("\n5. Generics:");
        System.out.println("   identity(42) = " + identity(42));
        System.out.println("   identity(\"hello\") = " + identity("hello"));
        var pair = new Pair<>(1, "one");
        System.out.println("   " + pair);
        System.out.println("   swap() = " + pair.swap());

        // 6. Optional
        System.out.println("\n6. Optional Type:");
        System.out.println("   safeDivide(10, 2) = " + safeDivide(10, 2));
        System.out.println("   safeDivide(10, 0) = " + safeDivide(10, 0));
        System.out.println("   safeHead([1,2,3]) = " + safeHead(List.of(1, 2, 3)));
        System.out.println("   safeHead([]) = " + safeHead(List.of()));

        // 7. Result type
        System.out.println("\n7. Result Type:");
        System.out.println("   divide(10, 2) = " + divide(10, 2));
        System.out.println("   divide(10, 0) = " + divide(10, 0));

        // 8. Interfaces
        System.out.println("\n8. Interfaces:");
        printDescription(new DescribableCircle(5.0));
        printDescription(new DescribablePoint(new Point(3, 4)));

        // 9. Wildcards
        System.out.println("\n9. Wildcards (Variance):");
        System.out.printf("   Total area = %.2f%n", sumAreas(shapes));

        // 10. Function types
        System.out.println("\n10. Function Types:");
        System.out.println("   applyTwice(x -> x + 1, 5) = " + applyTwice(x -> x + 1, 5));
        var timesThree = makeMultiplier(3);
        System.out.println("   makeMultiplier(3)(7) = " + timesThree.apply(7));

        // 11. Enums
        System.out.println("\n11. Enums:");
        var light = TrafficLight.RED;
        System.out.println("   Current: " + light);
        System.out.println("   Next: " + light.next());
        System.out.println("   Color.RED.getValue() = " + Color.RED.getValue());

        // 12. Custom types
        System.out.println("\n12. Custom Types (Newtype Pattern):");
        var userId = new UserId(123);
        System.out.println("   getUserName(" + userId + ") = " + getUserName(userId));

        // 13. Streams
        System.out.println("\n13. Streams and Functional Types:");
        demonstrateStreams();

        System.out.println("\n=== Java Type System Features ===");
        System.out.println("- Strong static typing (nominal)");
        System.out.println("- Generics with type erasure");
        System.out.println("- Interfaces for polymorphism");
        System.out.println("- Records for data classes (Java 14+)");
        System.out.println("- Sealed classes (Java 17+)");
        System.out.println("- Optional for null safety");
        System.out.println("- Pattern matching (Java 17+)");
        System.out.println("- Type inference with var (Java 10+)");
        System.out.println("- Checked exceptions in signatures");
    }
}
