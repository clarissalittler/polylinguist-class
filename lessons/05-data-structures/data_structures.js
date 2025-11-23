#!/usr/bin/env node
/**
 * Lesson 5: Data Structures in JavaScript
 * Demonstrates arrays, objects, Maps, Sets
 */

function main() {
    console.log("=== JavaScript Data Structures ===\n");

    // 1. Arrays (mutable)
    console.log("1. Arrays (Mutable):");
    const numbers = [1, 2, 3, 4, 5];
    console.log(`  Original: [${numbers}]`);

    numbers.push(6);
    console.log(`  After push(6): [${numbers}]`);

    numbers[0] = 10;
    console.log(`  After numbers[0] = 10: [${numbers}]`);

    // const doesn't prevent mutation!
    console.log("\n  const prevents reassignment, NOT mutation:");
    // numbers = [1, 2, 3];  // ERROR
    numbers.push(7);  // OK!
    console.log(`  After push(7): [${numbers}]`);

    // Aliasing
    console.log("\n  Aliasing demo:");
    const arr1 = [1, 2, 3];
    const arr2 = arr1;  // Same reference!
    arr2.push(4);
    console.log(`    arr1: [${arr1}]`);  // Also changed!
    console.log(`    arr2: [${arr2}]`);

    // Copying
    const arr3 = [...arr1];  // Spread operator creates copy
    arr3.push(5);
    console.log(`  After copying with spread:`);
    console.log(`    arr1: [${arr1}]`);  // Unchanged
    console.log(`    arr3: [${arr3}]`);  // Changed

    // 2. Array methods (functional style)
    console.log("\n2. Array Methods (Functional Style):");
    const nums = [1, 2, 3, 4, 5];

    const squared = nums.map(x => x * x);
    console.log(`  map(square): [${squared}]`);

    const evens = nums.filter(x => x % 2 === 0);
    console.log(`  filter(even): [${evens}]`);

    const sum = nums.reduce((acc, x) => acc + x, 0);
    console.log(`  reduce(sum): ${sum}`);

    // 3. Objects (like dictionaries)
    console.log("\n3. Objects (Like Dictionaries):");
    const person = {
        name: "Alice",
        age: 30,
        city: "NYC"
    };
    console.log(`  Person:`, person);

    // Modify
    person.age = 31;
    person.email = "alice@example.com";
    console.log(`  After modifications:`, person);

    // Access methods
    console.log(`  Keys: [${Object.keys(person)}]`);
    console.log(`  Values: [${Object.values(person)}]`);
    console.log(`  Has 'name': ${"name" in person}`);

    // 4. Map (better for key-value storage)
    console.log("\n4. Map (ES6 - Better Key-Value):");
    const map = new Map();
    map.set("name", "Bob");
    map.set("age", 25);
    map.set(42, "numeric key");  // Any type as key!

    console.log(`  map.get("name"): ${map.get("name")}`);
    console.log(`  map.has("age"): ${map.has("age")}`);
    console.log(`  map.size: ${map.size}`);

    // Iterate
    console.log(`  Entries:`);
    for (const [key, value] of map) {
        console.log(`    ${key}: ${value}`);
    }

    // 5. Set (unique values)
    console.log("\n5. Set (Unique Values):");
    const set = new Set([1, 2, 3, 4, 5]);
    console.log(`  Set: {${[...set]}}`);

    set.add(6);
    set.add(3);  // Duplicate, ignored
    console.log(`  After adding 6 and 3: {${[...set]}}`);

    console.log(`  Has 3: ${set.has(3)}`);
    console.log(`  Size: ${set.size}`);

    // Set operations (manual)
    const evensSet = new Set([2, 4, 6, 8]);
    const oddsSet = new Set([1, 3, 5, 7]);
    const union = new Set([...evensSet, ...oddsSet]);
    console.log(`  Union: {${[...union]}}`);

    // 6. Destructuring
    console.log("\n6. Destructuring:");
    const point = [3, 4];
    const [x, y] = point;
    console.log(`  [x, y] = [3, 4]: x=${x}, y=${y}`);

    const {name, age} = person;
    console.log(`  Destructured person: name=${name}, age=${age}`);

    // 7. Spread and rest
    console.log("\n7. Spread and Rest:");
    const arr4 = [1, 2, 3];
    const arr5 = [...arr4, 4, 5];
    console.log(`  [...arr4, 4, 5]: [${arr5}]`);

    const combined = [...arr4, ...arr5];
    console.log(`  Combined arrays: [${combined}]`);

    // Rest parameters
    function sum_all(...numbers) {
        return numbers.reduce((a, b) => a + b, 0);
    }
    console.log(`  sum_all(1,2,3,4,5): ${sum_all(1,2,3,4,5)}`);

    // 8. Immutability patterns
    console.log("\n8. Immutability Patterns:");
    const original = [1, 2, 3];

    // Adding to array immutably
    const withFour = [...original, 4];
    console.log(`  Original: [${original}]`);
    console.log(`  With 4: [${withFour}]`);

    // Updating object immutably
    const person2 = {...person, age: 32};
    console.log(`  Original age: ${person.age}`);
    console.log(`  Updated age: ${person2.age}`);

    // 9. Performance notes
    console.log("\n9. Performance Notes:");
    console.log("  - Arrays: Fast access, slow insertion at start");
    console.log("  - Objects: Fast lookup by key");
    console.log("  - Map: Better than Object for frequent add/delete");
    console.log("  - Set: Fast membership testing");
}

main();
