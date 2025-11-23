/**
 * Lesson 11: Error Handling & Debugging in Java
 *
 * Java features:
 * - try/catch/finally blocks
 * - Checked vs unchecked exceptions
 * - try-with-resources (automatic cleanup)
 * - Custom exceptions
 * - Multi-catch (Java 7+)
 * - Stack traces
 *
 * Requires Java 8 or later.
 */

import java.io.*;
import java.util.*;

public class ErrorHandling {

    // ====================
    // 1. Basic try/catch
    // ====================

    public static double divide(double x, double y) {
        try {
            if (y == 0) {
                throw new ArithmeticException("Cannot divide by zero");
            }
            return x / y;
        } catch (ArithmeticException e) {
            System.out.println("   Error: " + e.getMessage());
            return Double.NaN;
        }
    }

    // ====================
    // 2. Multiple catch blocks
    // ====================

    public static Integer safeParse(String s) {
        try {
            return Integer.parseInt(s);
        } catch (NumberFormatException e) {
            System.out.println("   Error: Invalid number format");
            return null;
        } catch (NullPointerException e) {
            System.out.println("   Error: Null string");
            return null;
        }
    }

    // ====================
    // 3. Multi-catch (Java 7+)
    // ====================

    public static Object processValue(Object value) {
        try {
            String s = (String) value;
            return Integer.parseInt(s);
        } catch (ClassCastException | NumberFormatException e) {
            System.out.println("   Error: " + e.getClass().getSimpleName());
            return null;
        }
    }

    // ====================
    // 4. try-with-resources
    // ====================

    public static void readFile(String filename) {
        System.out.println("   Reading " + filename + "...");
        try (BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            String line = reader.readLine();
            System.out.println("   First line: " + line);
        } catch (FileNotFoundException e) {
            System.out.println("   Error: File not found");
        } catch (IOException e) {
            System.out.println("   Error: IO exception - " + e.getMessage());
        }
        // reader automatically closed
    }

    // ====================
    // 5. Custom exceptions
    // ====================

    static class ValidationException extends Exception {
        public ValidationException(String message) {
            super(message);
        }
    }

    static class InvalidEmailException extends ValidationException {
        private final String email;

        public InvalidEmailException(String email, String reason) {
            super("Invalid email '" + email + "': " + reason);
            this.email = email;
        }

        public String getEmail() {
            return email;
        }
    }

    public static void validateEmail(String email) throws InvalidEmailException {
        if (!email.contains("@")) {
            throw new InvalidEmailException(email, "missing @ symbol");
        }
        if (!email.substring(email.indexOf("@")).contains(".")) {
            throw new InvalidEmailException(email, "missing domain extension");
        }
        System.out.println("   Valid email: " + email);
    }

    // ====================
    // 6. finally block
    // ====================

    public static void demonstrateFinally() {
        System.out.println("   Starting operation...");
        try {
            System.out.println("   Executing...");
            throw new RuntimeException("Something went wrong");
        } catch (RuntimeException e) {
            System.out.println("   Caught: " + e.getMessage());
        } finally {
            System.out.println("   Cleanup (always runs)");
        }
    }

    // ====================
    // 7. Stack traces
    // ====================

    public static void methodA() {
        methodB();
    }

    public static void methodB() {
        methodC();
    }

    public static void methodC() {
        try {
            throw new RuntimeException("Error in methodC");
        } catch (RuntimeException e) {
            System.out.println("   Error: " + e.getMessage());
            System.out.println("   Stack trace:");
            for (int i = 0; i < Math.min(3, e.getStackTrace().length); i++) {
                System.out.println("      " + e.getStackTrace()[i]);
            }
        }
    }

    // ====================
    // 8. Error recovery
    // ====================

    public static List<Integer> robustParse(String[] items) {
        List<Integer> results = new ArrayList<>();
        int errors = 0;

        for (String item : items) {
            try {
                results.add(Integer.parseInt(item));
            } catch (NumberFormatException e) {
                errors++;
            }
        }

        System.out.println("   Parsed " + results.size() + "/" + items.length + " items");
        System.out.println("   Results: " + results);
        if (errors > 0) {
            System.out.println("   Errors: " + errors);
        }

        return results;
    }

    // ====================
    // 9. Checked exceptions
    // ====================

    public static void demonstrateChecked() throws IOException {
        // Checked exceptions must be declared or caught
        throw new IOException("This must be handled");
    }

    public static void callChecked() {
        try {
            demonstrateChecked();
        } catch (IOException e) {
            System.out.println("   Caught checked exception: " + e.getMessage());
        }
    }

    // ====================
    // Main demonstration
    // ====================

    public static void main(String[] args) {
        System.out.println("=== Error Handling in Java ===\n");

        // 1. Basic try/catch
        System.out.println("1. Basic try/catch:");
        System.out.println("   divide(10, 2) = " + divide(10, 2));
        System.out.println("   divide(10, 0) = " + divide(10, 0));

        // 2. Multiple catch
        System.out.println("\n2. Multiple catch blocks:");
        System.out.println("   safeParse('42') = " + safeParse("42"));
        safeParse("invalid");
        safeParse(null);

        // 3. Multi-catch
        System.out.println("\n3. Multi-catch (Java 7+):");
        processValue("123");
        processValue(123);
        processValue("invalid");

        // 4. try-with-resources
        System.out.println("\n4. try-with-resources:");
        // Create a test file
        try {
            PrintWriter writer = new PrintWriter("/tmp/test.txt");
            writer.println("Hello, World!");
            writer.close();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
        readFile("/tmp/test.txt");
        readFile("/nonexistent/file.txt");

        // 5. Custom exceptions
        System.out.println("\n5. Custom Exceptions:");
        try {
            validateEmail("user@example.com");
            validateEmail("invalid-email");
        } catch (InvalidEmailException e) {
            System.out.println("   Caught: " + e.getMessage());
        }

        // 6. finally
        System.out.println("\n6. finally block:");
        demonstrateFinally();

        // 7. Stack traces
        System.out.println("\n7. Stack Traces:");
        methodA();

        // 8. Error recovery
        System.out.println("\n8. Error Recovery:");
        robustParse(new String[]{"1", "2", "bad", "4", "invalid", "6"});

        // 9. Checked exceptions
        System.out.println("\n9. Checked Exceptions:");
        callChecked();

        System.out.println("\n=== Java Error Handling Features ===");
        System.out.println("- try/catch/finally blocks");
        System.out.println("- Checked vs unchecked exceptions");
        System.out.println("- try-with-resources for automatic cleanup");
        System.out.println("- Custom exception hierarchies");
        System.out.println("- Multi-catch (Java 7+)");
        System.out.println("- Stack traces for debugging");
        System.out.println("- Forced error handling (checked exceptions)");
    }
}
