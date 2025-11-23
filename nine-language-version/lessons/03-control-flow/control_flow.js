#!/usr/bin/env node
/**
 * Lesson 3: Control Flow in JavaScript
 * Demonstrates conditionals, loops, and boolean logic
 */

function main() {
    console.log("=== JavaScript Control Flow ===\n");

    // 1. Basic conditionals
    console.log("1. Basic Conditionals:");
    const age = 20;
    if (age >= 18) {
        console.log(`  Age ${age}: Adult`);
    } else if (age >= 13) {
        console.log(`  Age ${age}: Teenager`);
    } else {
        console.log(`  Age ${age}: Child`);
    }

    // Ternary expression
    const status = age >= 18 ? "Adult" : "Minor";
    console.log(`  Status: ${status}`);

    // 2. For loops
    console.log("\n2. For Loops:");
    console.log("  Count to 5:");
    let output = "   ";
    for (let i = 0; i < 5; i++) {
        output += ` ${i}`;
    }
    console.log(output);

    console.log("  Iterate array:");
    const fruits = ["apple", "banana", "cherry"];
    for (const fruit of fruits) {
        console.log(`    ${fruit}`);
    }

    console.log("  forEach method:");
    fruits.forEach((fruit, i) => {
        console.log(`    ${i}: ${fruit}`);
    });

    // 3. While loop
    console.log("\n3. While Loop:");
    let count = 0;
    while (count < 3) {
        console.log(`  Count: ${count}`);
        count++;
    }

    // 4. Boolean logic and truthiness
    console.log("\n4. Boolean Logic:");
    const x = 5, y = 10;
    console.log(`  x=${x}, y=${y}`);
    console.log(`  x > 3 && y < 20: ${x > 3 && y < 20}`);
    console.log(`  x > 10 || y > 5: ${x > 10 || y > 5}`);
    console.log(`  !(x === y): ${!(x === y)}`);

    console.log("\n  Truthiness:");
    // Falsy: false, null, undefined, 0, NaN, ""
    const values = [true, false, 0, 1, "", "hello", [], [1, 2], null, undefined, NaN];
    values.forEach(val => {
        const truthy = val ? "truthy" : "falsy";
        console.log(`    ${JSON.stringify(val)}: ${truthy}`);
    });

    // 5. FizzBuzz
    console.log("\n5. FizzBuzz (1-20):");
    let fizzbuzzOutput = " ";
    for (let i = 1; i <= 20; i++) {
        if (i % 15 === 0) {
            fizzbuzzOutput += " FizzBuzz";
        } else if (i % 3 === 0) {
            fizzbuzzOutput += " Fizz";
        } else if (i % 5 === 0) {
            fizzbuzzOutput += " Buzz";
        } else {
            fizzbuzzOutput += ` ${i}`;
        }
    }
    console.log(fizzbuzzOutput);

    // 6. Switch statement
    console.log("\n6. Switch Statement:");
    function describeDay(day) {
        switch (day) {
            case 0:
            case 6:
                return "Weekend";
            case 1:
            case 2:
            case 3:
            case 4:
            case 5:
                return "Weekday";
            default:
                return "Invalid day";
        }
    }

    console.log(`  Day 0 (Sunday): ${describeDay(0)}`);
    console.log(`  Day 3 (Wednesday): ${describeDay(3)}`);
    console.log(`  Day 6 (Saturday): ${describeDay(6)}`);
}

main();
