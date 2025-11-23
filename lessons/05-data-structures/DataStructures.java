/*
 * Lesson 5: Data Structures in Java
 * Demonstrates ArrayList, HashMap, HashSet
 */

import java.util.*;

public class DataStructures {
    public static void main(String[] args) {
        System.out.println("=== Java Data Structures ===\n");

        // 1. ArrayList (dynamic, mutable)
        System.out.println("1. ArrayList (Dynamic, Mutable):");
        ArrayList<Integer> numbers = new ArrayList<>(Arrays.asList(1, 2, 3, 4, 5));
        System.out.println("  Original: " + numbers);

        numbers.add(6);
        System.out.println("  After add(6): " + numbers);

        numbers.set(0, 10);
        System.out.println("  After set(0, 10): " + numbers);

        numbers.remove(Integer.valueOf(10));
        System.out.println("  After remove(10): " + numbers);

        // 2. Arrays (fixed size)
        System.out.println("\n2. Arrays (Fixed Size):");
        int[] arr = {1, 2, 3, 4, 5};
        System.out.println("  Array: " + Arrays.toString(arr));
        System.out.println("  Length: " + arr.length);

        // 3. HashMap (key-value)
        System.out.println("\n3. HashMap (Key-Value):");
        HashMap<String, String> person = new HashMap<>();
        person.put("name", "Alice");
        person.put("age", "30");
        person.put("city", "NYC");
        System.out.println("  Person: " + person);

        // Modify
        person.put("age", "31");
        person.put("email", "alice@example.com");
        System.out.println("  After modifications: " + person);

        // Access
        System.out.println("  Get 'name': " + person.get("name"));
        System.out.println("  Contains 'age': " + person.containsKey("age"));

        // Iteration
        System.out.println("  Entries:");
        for (Map.Entry<String, String> entry : person.entrySet()) {
            System.out.println("    " + entry.getKey() + ": " + entry.getValue());
        }

        // 4. HashSet (unique values)
        System.out.println("\n4. HashSet (Unique Values):");
        HashSet<Integer> set = new HashSet<>(Arrays.asList(1, 2, 3, 4, 5));
        System.out.println("  Set: " + set);

        set.add(6);
        set.add(3);  // Duplicate, ignored
        System.out.println("  After adding 6 and 3: " + set);

        System.out.println("  Contains 3: " + set.contains(3));
        System.out.println("  Size: " + set.size());

        // 5. Streams (functional style)
        System.out.println("\n5. Streams (Functional Style):");
        List<Integer> nums = Arrays.asList(1, 2, 3, 4, 5);

        List<Integer> squared = nums.stream()
            .map(x -> x * x)
            .collect(java.util.stream.Collectors.toList());
        System.out.println("  map(square): " + squared);

        List<Integer> evens = nums.stream()
            .filter(x -> x % 2 == 0)
            .collect(java.util.stream.Collectors.toList());
        System.out.println("  filter(even): " + evens);

        int sum = nums.stream()
            .reduce(0, (a, b) -> a + b);
        System.out.println("  reduce(sum): " + sum);

        // 6. Immutable collections (Java 9+)
        System.out.println("\n6. Immutable Collections (Java 9+):");
        try {
            List<Integer> immutableList = List.of(1, 2, 3, 4, 5);
            System.out.println("  Immutable list: " + immutableList);
            // immutableList.add(6);  // Would throw UnsupportedOperationException

            Map<String, String> immutableMap = Map.of("name", "Bob", "age", "25");
            System.out.println("  Immutable map: " + immutableMap);
        } catch (Exception e) {
            System.out.println("  (Immutable collections require Java 9+)");
        }

        // 7. Key insights
        System.out.println("\n7. Key Insights:");
        System.out.println("  - Everything is an object (no primitives in collections)");
        System.out.println("  - Generics provide type safety");
        System.out.println("  - Collections are mutable by default");
        System.out.println("  - Streams enable functional programming");
        System.out.println("  - Automatic memory management (GC)");
    }
}
