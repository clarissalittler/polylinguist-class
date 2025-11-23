#!/usr/bin/env node
/**
 * Lesson 4: Functions in JavaScript
 * Demonstrates function definition, parameters, scope, closures, and purity
 */

// ============================================
// 1. Basic Function Definition
// ============================================

// Function declaration
function greet(name) {
    return `Hello, ${name}!`;
}

// Function expression
const add = function(x, y) {
    return x + y;
};

// Arrow function
const square = x => x * x;

// Arrow function with block
const multiply = (x, y) => {
    return x * y;
};

// ============================================
// 2. Parameters and Arguments
// ============================================

function describePerson(name, age, city = "Unknown") {
    return `${name} is ${age} years old and lives in ${city}`;
}

// Rest parameters (variadic)
function printInfo(...args) {
    console.log("Arguments:", args);
}

// Destructuring parameters
function describePersonObj({name, age, city = "Unknown"}) {
    return `${name} is ${age} years old and lives in ${city}`;
}

// ============================================
// 3. Return Values
// ============================================

function divide(x, y) {
    if (y === 0) {
        return [null, "Cannot divide by zero"];
    }
    return [x / y, null];
}

// ============================================
// 4. Scope and Closures
// ============================================

const globalVar = "I'm global";

function scopeDemo() {
    const localVar = "I'm local";
    console.log(`Inside function: ${localVar}`);
    console.log(`Can access global: ${globalVar}`);
}

function makeMultiplier(factor) {
    // Closure: inner function captures 'factor'
    return function(x) {
        return x * factor;
    };
}

// Arrow function closure
const makeAdder = n => x => x + n;

function makeCounter() {
    let count = 0;  // Private variable

    return function() {
        count++;
        return count;
    };
}

// ============================================
// 5. Pure vs Impure Functions
// ============================================

// Pure function
const pureAdd = (x, y) => x + y;

// Impure function (side effect)
function impurePrint(message) {
    console.log(message);
}

// Impure function (depends on external state)
let total = 0;
function impureAddToTotal(x) {
    total += x;
    return total;
}

// Impure function (non-deterministic)
function impureRandom() {
    return Math.random();
}

// ============================================
// 6. First-Class Functions
// ============================================

function applyOperation(operation, x, y) {
    return operation(x, y);
}

function compose(f, g) {
    return x => f(g(x));
}

// ============================================
// 7. Anonymous Functions (Lambdas/Arrows)
// ============================================

const double = x => x * 2;
const isEven = x => x % 2 === 0;
const addLambda = (x, y) => x + y;

// ============================================
// 8. Higher-Order Functions
// ============================================

function applyTwice(f, x) {
    return f(f(x));
}

function applyNTimes(f, x, n) {
    let result = x;
    for (let i = 0; i < n; i++) {
        result = f(result);
    }
    return result;
}

// ============================================
// 9. Currying
// ============================================

function curriedMultiply(x) {
    return function(y) {
        return x * y;
    };
}

// Arrow function currying
const curriedAdd = x => y => x + y;

// ============================================
// Main Program
// ============================================

function main() {
    console.log("=== JavaScript Functions ===\n");

    // 1. Basic functions
    console.log("1. Basic Functions:");
    console.log(`  greet('Alice'): ${greet('Alice')}`);
    console.log(`  add(5, 3): ${add(5, 3)}`);
    console.log(`  square(7): ${square(7)}`);
    console.log(`  multiply(4, 5): ${multiply(4, 5)}`);

    // 2. Parameters
    console.log("\n2. Parameters and Arguments:");
    console.log(`  describePerson('Alice', 30): ${describePerson('Alice', 30)}`);
    console.log(`  describePerson('Bob', 25, 'NYC'): ${describePerson('Bob', 25, 'NYC')}`);
    console.log("\n  Variadic function:");
    printInfo(1, 2, 3, 4, 5);
    console.log("\n  Object destructuring:");
    console.log(`  ${describePersonObj({name: "Charlie", age: 35, city: "LA"})}`);

    // 3. Multiple returns (arrays)
    console.log("\n3. Multiple Return Values:");
    const [result1, error1] = divide(10, 2);
    console.log(`  divide(10, 2): ${result1}, error: ${error1}`);
    const [result2, error2] = divide(10, 0);
    console.log(`  divide(10, 0): ${result2}, error: ${error2}`);

    // 4. Closures
    console.log("\n4. Closures:");
    const timesTwo = makeMultiplier(2);
    const timesThree = makeMultiplier(3);
    console.log(`  timesTwo(5): ${timesTwo(5)}`);
    console.log(`  timesThree(5): ${timesThree(5)}`);

    const addFive = makeAdder(5);
    console.log(`  addFive(10): ${addFive(10)}`);

    console.log("\n  Counter (stateful closure):");
    const counter1 = makeCounter();
    const counter2 = makeCounter();
    console.log(`  counter1(): ${counter1()}`);  // 1
    console.log(`  counter1(): ${counter1()}`);  // 2
    console.log(`  counter2(): ${counter2()}`);  // 1
    console.log(`  counter1(): ${counter1()}`);  // 3

    // 5. Pure vs Impure
    console.log("\n5. Pure vs Impure:");
    console.log(`  pureAdd(5, 3): ${pureAdd(5, 3)}`);
    console.log("  impurePrint('Hello'): ");
    impurePrint("  Hello");
    total = 0;  // Reset
    console.log(`  impureAddToTotal(5): ${impureAddToTotal(5)}`);
    console.log(`  impureAddToTotal(5): ${impureAddToTotal(5)}`);  // Different!

    // 6. First-class functions
    console.log("\n6. First-Class Functions:");
    console.log(`  applyOperation(add, 5, 3): ${applyOperation(add, 5, 3)}`);
    console.log(`  applyOperation((x,y) => x*y, 5, 3): ${applyOperation((x,y) => x*y, 5, 3)}`);

    // 7. Function composition
    console.log("\n7. Function Composition:");
    const increment = x => x + 1;
    const squareThenIncrement = compose(increment, square);
    console.log(`  squareThenIncrement(5): ${squareThenIncrement(5)}`);  // (5^2) + 1 = 26

    // 8. Lambdas
    console.log("\n8. Anonymous Functions (Arrow Functions):");
    console.log(`  double(5): ${double(5)}`);
    console.log(`  isEven(4): ${isEven(4)}`);
    console.log(`  isEven(5): ${isEven(5)}`);

    // 9. Higher-order functions with array methods
    console.log("\n9. Higher-Order Functions (map/filter/reduce):");
    const numbers = [1, 2, 3, 4, 5];
    console.log(`  numbers: [${numbers}]`);
    console.log(`  numbers.map(square): [${numbers.map(square)}]`);
    console.log(`  numbers.filter(isEven): [${numbers.filter(isEven)}]`);
    const sum = numbers.reduce((acc, x) => acc + x, 0);
    console.log(`  numbers.reduce(add): ${sum}`);

    // 10. Apply n times
    console.log("\n10. Apply Function Multiple Times:");
    console.log(`  applyTwice(double, 5): ${applyTwice(double, 5)}`);  // 5 * 2 * 2 = 20
    console.log(`  applyNTimes(double, 5, 3): ${applyNTimes(double, 5, 3)}`);  // 5 * 2^3 = 40

    // 11. Currying
    console.log("\n11. Currying:");
    const multiplyByTwo = curriedMultiply(2);
    console.log(`  multiplyByTwo(5): ${multiplyByTwo(5)}`);
    const addTen = curriedAdd(10);
    console.log(`  addTen(5): ${addTen(5)}`);
}

main();
