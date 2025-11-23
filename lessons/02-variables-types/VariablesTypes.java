// VariablesTypes.java
public class VariablesTypes {
    public static void main(String[] args) {
        // Primitive types
        byte smallNum = 127;           // 8-bit
        short mediumNum = 32000;       // 16-bit
        int age = 25;                  // 32-bit
        long bigNum = 9223372036854775807L;  // 64-bit

        float price = 19.99f;          // 32-bit float
        double precise = 3.14159265359; // 64-bit float

        char letter = 'A';             // Unicode character
        boolean isStudent = true;

        // Reference types (objects)
        String name = "Alice";         // String is an object
        Integer boxedAge = 25;         // Wrapper for int

        // Type inference (Java 10+)
        var inferredType = "I'm a String";  // type inferred
        var number = 42;               // inferred as int

        // Cannot change type
        // age = "text";  // ERROR: incompatible types

        System.out.println("Age: " + age);
        System.out.println("Name: " + name);
        System.out.println("Price: " + price);
        System.out.println("Letter: " + letter);
        System.out.println("Is student: " + isStudent);
    }
}
