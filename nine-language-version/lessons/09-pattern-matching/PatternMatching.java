/**
 * Lesson 9: Pattern Matching in Java
 *
 * Java has been adding pattern matching features since Java 16:
 * - Pattern matching for instanceof (Java 16+)
 * - Switch expressions (Java 14+)
 * - Pattern matching in switch (Java 17+ preview, Java 21 finalized)
 * - Record patterns (Java 19+ preview)
 *
 * This demonstrates modern Java pattern matching capabilities.
 * Requires Java 17 or later.
 */

import java.util.*;

public class PatternMatching {

    // ====================
    // 1. Basic Switch Expression
    // ====================

    static String describeNumber(int n) {
        return switch (n) {
            case 0 -> "zero";
            case 1 -> "one";
            case 2 -> "two";
            default -> "many: " + n;
        };
    }

    // ====================
    // 2. Tuple-like with Records
    // ====================

    record Point(int x, int y) {}

    static String describePoint(Point p) {
        if (p.x() == 0 && p.y() == 0) return "origin";
        if (p.x() == 0) return "on y-axis at y=" + p.y();
        if (p.y() == 0) return "on x-axis at x=" + p.x();
        return "point at (" + p.x() + ", " + p.y() + ")";
    }

    // ====================
    // 3. Pattern Matching for instanceof
    // ====================

    sealed interface Shape permits Circle, Rectangle, Triangle {}

    record Circle(double radius) implements Shape {}
    record Rectangle(double width, double height) implements Shape {}
    record Triangle(double a, double b, double c) implements Shape {}

    static double area(Shape shape) {
        if (shape instanceof Circle c) {
            return Math.PI * c.radius() * c.radius();
        } else if (shape instanceof Rectangle r) {
            return r.width() * r.height();
        } else if (shape instanceof Triangle t) {
            double s = (t.a() + t.b() + t.c()) / 2;
            return Math.sqrt(s * (s - t.a()) * (s - t.b()) * (s - t.c()));
        }
        throw new IllegalArgumentException("Unknown shape");
    }

    // ====================
    // 4. Switch with Pattern Matching (Java 17+)
    // ====================

    static String describeShape(Shape shape) {
        return switch (shape) {
            case Circle c -> "Circle with radius " + c.radius();
            case Rectangle r -> "Rectangle " + r.width() + "x" + r.height();
            case Triangle t -> "Triangle with sides " + t.a() + ", " + t.b() + ", " + t.c();
        };
    }

    // ====================
    // 5. Optional Pattern
    // ====================

    static String describeOptional(Optional<Integer> opt) {
        if (opt.isPresent()) {
            return "just " + opt.get();
        }
        return "nothing";
    }

    // ====================
    // 6. Guards (when clause - Java 19+ preview)
    // ====================

    static String classify(int n) {
        if (n < 0) return "negative";
        if (n == 0) return "zero";
        if (n < 10) return "small positive";
        if (n < 100) return "medium positive";
        return "large positive";
    }

    // Modern approach with pattern matching switch (requires --enable-preview in Java 19+)
    /*
    static String classifyModern(int n) {
        return switch (n) {
            case int i when i < 0 -> "negative";
            case 0 -> "zero";
            case int i when i < 10 -> "small positive";
            case int i when i < 100 -> "medium positive";
            default -> "large positive";
        };
    }
    */

    // ====================
    // 7. Multiple Values (OR patterns)
    // ====================

    static boolean isWeekend(String day) {
        return switch (day.toLowerCase()) {
            case "saturday", "sunday" -> true;
            default -> false;
        };
    }

    // ====================
    // 8. Range Patterns (simulated)
    // ====================

    static char gradeLetter(int score) {
        if (score >= 90 && score <= 100) return 'A';
        if (score >= 80) return 'B';
        if (score >= 70) return 'C';
        if (score >= 60) return 'D';
        return 'F';
    }

    // ====================
    // 9. Expression Evaluator with Sealed Classes
    // ====================

    sealed interface Expr permits Num, Add, Mul, Neg {}

    record Num(int value) implements Expr {}
    record Add(Expr left, Expr right) implements Expr {}
    record Mul(Expr left, Expr right) implements Expr {}
    record Neg(Expr expr) implements Expr {}

    static int eval(Expr expr) {
        return switch (expr) {
            case Num n -> n.value();
            case Add a -> eval(a.left()) + eval(a.right());
            case Mul m -> eval(m.left()) * eval(m.right());
            case Neg neg -> -eval(neg.expr());
        };
    }

    // ====================
    // 10. Nested Patterns
    // ====================

    record User(String name, int age, boolean active) {}

    static String describeUser(User user) {
        if (user.active() && user.age() >= 18) {
            return "Active adult: " + user.name();
        } else if (user.active()) {
            return "Active minor: " + user.name();
        } else {
            return "Inactive: " + user.name();
        }
    }

    // ====================
    // 11. Either/Result Pattern
    // ====================

    sealed interface Result<T, E> permits Success, Failure {}
    record Success<T, E>(T value) implements Result<T, E> {}
    record Failure<T, E>(E error) implements Result<T, E> {}

    static <T, E> String describeResult(Result<T, E> result) {
        return switch (result) {
            case Success<T, E> s -> "success: " + s.value();
            case Failure<T, E> f -> "error: " + f.error();
        };
    }

    // ====================
    // 12. List Patterns (manual)
    // ====================

    static String describeList(List<Integer> lst) {
        return switch (lst.size()) {
            case 0 -> "empty list";
            case 1 -> "single element: " + lst.get(0);
            case 2 -> "two elements: " + lst.get(0) + ", " + lst.get(1);
            default -> "first: " + lst.get(0) + ", rest: " + lst.subList(1, lst.size());
        };
    }

    // ====================
    // 13. State Machine
    // ====================

    enum TrafficLight { RED, GREEN, YELLOW }
    enum Action { TIMER, EMERGENCY }

    record State(TrafficLight light, Action action) {}

    static TrafficLight nextLight(TrafficLight current, Action action) {
        if (action == Action.EMERGENCY) return TrafficLight.RED;

        return switch (current) {
            case RED -> action == Action.TIMER ? TrafficLight.GREEN : current;
            case GREEN -> action == Action.TIMER ? TrafficLight.YELLOW : current;
            case YELLOW -> action == Action.TIMER ? TrafficLight.RED : current;
        };
    }

    // ====================
    // 14. Null Patterns
    // ====================

    static String describeNullable(String value) {
        return switch (value) {
            case null -> "null value";
            case String s when s.isEmpty() -> "empty string";
            case String s -> "string: " + s;
        };
    }

    // For older Java versions without null case in switch:
    static String describeNullableOld(String value) {
        if (value == null) return "null value";
        if (value.isEmpty()) return "empty string";
        return "string: " + value;
    }

    // ====================
    // Main Demonstration
    // ====================

    public static void main(String[] args) {
        System.out.println("=== Pattern Matching in Java ===\n");

        // 1. Basic switch
        System.out.println("1. Basic Switch Expressions:");
        int[] numbers = {0, 1, 2, 5};
        for (int n : numbers) {
            System.out.println("   " + n + " -> " + describeNumber(n));
        }

        // 2. Record patterns
        System.out.println("\n2. Record Patterns:");
        Point[] points = {new Point(0, 0), new Point(0, 5), new Point(3, 0), new Point(2, 3)};
        for (Point p : points) {
            System.out.println("   " + p + " -> " + describePoint(p));
        }

        // 3. Pattern matching for instanceof
        System.out.println("\n3. Pattern Matching for instanceof:");
        Shape[] shapes = {
            new Circle(5.0),
            new Rectangle(4.0, 6.0),
            new Triangle(3.0, 4.0, 5.0)
        };
        for (Shape shape : shapes) {
            System.out.println("   " + describeShape(shape));
            System.out.printf("   area = %.2f%n", area(shape));
        }

        // 4. Optional
        System.out.println("\n4. Optional Patterns:");
        System.out.println("   " + describeOptional(Optional.of(42)));
        System.out.println("   " + describeOptional(Optional.empty()));

        // 5. Guards
        System.out.println("\n5. Guards (Conditional Patterns):");
        int[] testNumbers = {-5, 0, 3, 50, 500};
        for (int n : testNumbers) {
            System.out.println("   " + n + " -> " + classify(n));
        }

        // 6. Multiple values
        System.out.println("\n6. Multiple Values (OR patterns):");
        String[] days = {"Monday", "Saturday", "Sunday", "Wednesday"};
        for (String day : days) {
            System.out.println("   " + day + " is weekend? " + isWeekend(day));
        }

        // 7. Range patterns
        System.out.println("\n7. Range Patterns:");
        int[] scores = {95, 85, 75, 65, 55};
        for (int score : scores) {
            System.out.println("   " + score + " -> " + gradeLetter(score));
        }

        // 8. Expression evaluator
        System.out.println("\n8. Expression Evaluator:");
        Expr expr = new Mul(new Add(new Num(2), new Num(3)), new Num(4));
        System.out.println("   (2 + 3) * 4 = " + eval(expr));
        Expr expr2 = new Neg(new Add(new Num(5), new Num(3)));
        System.out.println("   -(5 + 3) = " + eval(expr2));

        // 9. List patterns
        System.out.println("\n9. List Patterns:");
        List<List<Integer>> lists = List.of(
            List.of(),
            List.of(1),
            List.of(1, 2),
            List.of(1, 2, 3, 4)
        );
        for (List<Integer> lst : lists) {
            System.out.println("   " + lst + " -> " + describeList(lst));
        }

        // 10. User patterns
        System.out.println("\n10. User Patterns:");
        User[] users = {
            new User("Alice", 25, true),
            new User("Bob", 16, true),
            new User("Charlie", 30, false)
        };
        for (User user : users) {
            System.out.println("   " + describeUser(user));
        }

        // 11. Result patterns
        System.out.println("\n11. Result Patterns:");
        Result<Integer, String> success = new Success<>(100);
        Result<Integer, String> failure = new Failure<>("failed");
        System.out.println("   " + describeResult(success));
        System.out.println("   " + describeResult(failure));

        // 12. State machine
        System.out.println("\n12. State Machine (Traffic Light):");
        TrafficLight state = TrafficLight.RED;
        Action[] actions = {Action.TIMER, Action.TIMER, Action.TIMER, Action.EMERGENCY};
        for (Action action : actions) {
            state = nextLight(state, action);
            System.out.println("   After " + action + ": " + state);
        }

        // 13. Null patterns (use old version for compatibility)
        System.out.println("\n13. Null Patterns:");
        System.out.println("   " + describeNullableOld(null));
        System.out.println("   " + describeNullableOld(""));
        System.out.println("   " + describeNullableOld("hello"));

        System.out.println("\n=== Java Pattern Matching Notes ===");
        System.out.println("- Pattern matching for instanceof (Java 16+)");
        System.out.println("- Switch expressions (Java 14+)");
        System.out.println("- Pattern matching in switch (Java 21)");
        System.out.println("- Sealed classes ensure exhaustiveness");
        System.out.println("- Records work great with pattern matching");
    }
}
