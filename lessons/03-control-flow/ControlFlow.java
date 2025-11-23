/*
 * Lesson 3: Control Flow in Java
 * Demonstrates conditionals, loops, and boolean logic
 */

public class ControlFlow {
    public static void main(String[] args) {
        System.out.println("=== Java Control Flow ===\n");

        // 1. Basic conditionals
        System.out.println("1. Basic Conditionals:");
        int age = 20;
        if (age >= 18) {
            System.out.println("  Age " + age + ": Adult");
        } else if (age >= 13) {
            System.out.println("  Age " + age + ": Teenager");
        } else {
            System.out.println("  Age " + age + ": Child");
        }

        // Ternary expression
        String status = (age >= 18) ? "Adult" : "Minor";
        System.out.println("  Status: " + status);

        // 2. For loops
        System.out.println("\n2. For Loops:");
        System.out.print("  Count to 5:\n   ");
        for (int i = 0; i < 5; i++) {
            System.out.print(" " + i);
        }
        System.out.println();

        System.out.println("  Iterate array:");
        String[] fruits = {"apple", "banana", "cherry"};
        for (int i = 0; i < fruits.length; i++) {
            System.out.println("    " + fruits[i]);
        }

        System.out.println("  Enhanced for loop:");
        for (String fruit : fruits) {
            System.out.println("    " + fruit);
        }

        // 3. While loop
        System.out.println("\n3. While Loop:");
        int count = 0;
        while (count < 3) {
            System.out.println("  Count: " + count);
            count++;
        }

        // 4. Boolean logic
        System.out.println("\n4. Boolean Logic:");
        int x = 5, y = 10;
        System.out.println("  x=" + x + ", y=" + y);
        System.out.println("  x > 3 && y < 20: " + (x > 3 && y < 20));
        System.out.println("  x > 10 || y > 5: " + (x > 10 || y > 5));
        System.out.println("  !(x == y): " + !(x == y));

        System.out.println("\n  Only boolean type (no truthiness):");
        boolean t = true;
        boolean f = false;
        System.out.println("    true: " + (t ? "truthy" : "falsy"));
        System.out.println("    false: " + (f ? "truthy" : "falsy"));

        // 5. FizzBuzz
        System.out.println("\n5. FizzBuzz (1-20):");
        System.out.print(" ");
        for (int i = 1; i <= 20; i++) {
            if (i % 15 == 0) {
                System.out.print(" FizzBuzz");
            } else if (i % 3 == 0) {
                System.out.print(" Fizz");
            } else if (i % 5 == 0) {
                System.out.print(" Buzz");
            } else {
                System.out.print(" " + i);
            }
        }
        System.out.println();

        // 6. Switch statement
        System.out.println("\n6. Switch Statement:");
        int day = 3; // Wednesday
        String dayType;
        switch (day) {
            case 0:
            case 6:
                dayType = "Weekend";
                break;
            case 1:
            case 2:
            case 3:
            case 4:
            case 5:
                dayType = "Weekday";
                break;
            default:
                dayType = "Invalid day";
        }
        System.out.println("  Day " + day + ": " + dayType);

        // 7. Do-while loop
        System.out.println("\n7. Do-While Loop:");
        int i = 0;
        do {
            System.out.println("  Iteration: " + i);
            i++;
        } while (i < 3);

        // 8. Switch expressions (Java 14+)
        System.out.println("\n8. Switch Expression (Java 14+):");
        try {
            // This uses switch expression syntax
            int score = 85;
            String grade = switch (score / 10) {
                case 10, 9 -> "A";
                case 8 -> "B";
                case 7 -> "C";
                case 6 -> "D";
                default -> "F";
            };
            System.out.println("  Score " + score + ": Grade " + grade);
        } catch (Error e) {
            System.out.println("  (Switch expressions require Java 14+)");
        }
    }
}
