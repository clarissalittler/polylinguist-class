#!/usr/bin/env node
/**
 * Lesson 8: Higher-Order Functions in JavaScript
 *
 * JavaScript excels at functional programming:
 * - First-class functions
 * - Arrow functions (lambdas)
 * - Built-in array HOFs (map, filter, reduce)
 * - Closures everywhere
 * - Function composition
 */

// ====================
// 1. Functions as First-Class Values
// ====================

function demonstrateFirstClass() {
    const greet = (name) => `Hello, ${name}!`;

    // Assign to variable
    const sayHello = greet;
    console.log(`   ${sayHello('Alice')}`);

    // Store in data structure
    const operations = {
        greet: greet,
        shout: (name) => `HEY ${name.toUpperCase()}!`
    };
    console.log(`   ${operations.greet('Bob')}`);
    console.log(`   ${operations.shout('Bob')}`);

    // Store in array
    const funcs = [
        x => x + 1,
        x => x * 2,
        x => x ** 2
    ];
    console.log(`   ${funcs.map(f => f(5))}`);
}

// ====================
// 2. Functions Taking Functions
// ====================

const applyTwice = (func, x) => func(func(x));

const applyNTimes = (func, n, x) => {
    let result = x;
    for (let i = 0; i < n; i++) {
        result = func(result);
    }
    return result;
};

// ====================
// 3. Functions Returning Functions
// ====================

const makeMultiplier = (n) => (x) => x * n;

const makeAdder = (n) => (x) => x + n;

// Curried functions
const add = a => b => c => a + b + c;

// ====================
// 4. Map - Transform Each Element
// ====================

function demonstrateMap() {
    const numbers = [1, 2, 3, 4, 5];

    // Using map with arrow function
    const doubled = numbers.map(x => x * 2);
    console.log(`   Doubled: [${doubled}]`);

    // Map with index
    const withIndex = numbers.map((x, i) => `${i}: ${x}`);
    console.log(`   With index: [${withIndex}]`);

    // Chaining maps
    const result = numbers
        .map(x => x + 1)
        .map(x => x * 2);
    console.log(`   Chained: [${result}]`);
}

// ====================
// 5. Filter - Select Elements
// ====================

function demonstrateFilter() {
    const numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

    // Filter evens
    const evens = numbers.filter(x => x % 2 === 0);
    console.log(`   Evens: [${evens}]`);

    // Filter with complex predicate
    const bigOdds = numbers.filter(x => x % 2 !== 0 && x > 5);
    console.log(`   Big odds: [${bigOdds}]`);

    // Filter objects
    const people = [
        { name: 'Alice', age: 25 },
        { name: 'Bob', age: 17 },
        { name: 'Charlie', age: 30 }
    ];
    const adults = people.filter(p => p.age >= 18);
    console.log(`   Adults: ${JSON.stringify(adults.map(p => p.name))}`);
}

// ====================
// 6. Reduce - Combine to Single Value
// ====================

function demonstrateReduce() {
    const numbers = [1, 2, 3, 4, 5];

    // Sum
    const sum = numbers.reduce((acc, x) => acc + x, 0);
    console.log(`   Sum: ${sum}`);

    // Product
    const product = numbers.reduce((acc, x) => acc * x, 1);
    console.log(`   Product: ${product}`);

    // Build object
    const fruits = ['apple', 'banana', 'apple', 'orange', 'banana', 'apple'];
    const counts = fruits.reduce((acc, fruit) => {
        acc[fruit] = (acc[fruit] || 0) + 1;
        return acc;
    }, {});
    console.log(`   Fruit counts: ${JSON.stringify(counts)}`);

    // Flatten array
    const nested = [[1, 2], [3, 4], [5]];
    const flattened = nested.reduce((acc, arr) => acc.concat(arr), []);
    console.log(`   Flattened: [${flattened}]`);
}

// ====================
// 7. Closures
// ====================

function makeCounter() {
    let count = 0;  // Enclosed variable

    return function() {
        count++;
        return count;
    };
}

function makeBankAccount(initialBalance) {
    let balance = initialBalance;  // Private!

    return {
        deposit(amount) {
            if (amount > 0) balance += amount;
            return balance;
        },
        withdraw(amount) {
            if (amount > 0 && amount <= balance) balance -= amount;
            return balance;
        },
        getBalance() {
            return balance;
        }
    };
}

// ====================
// 8. Partial Application and Currying
// ====================

function demonstratePartial() {
    // Manual partial application
    const power = (base, exponent) => base ** exponent;

    const square = (x) => power(x, 2);
    const cube = (x) => power(x, 3);

    console.log(`   square(5) = ${square(5)}`);
    console.log(`   cube(5) = ${cube(5)}`);

    // Using bind for partial application
    const multiply = (a, b) => a * b;
    const double = multiply.bind(null, 2);
    const triple = multiply.bind(null, 3);

    console.log(`   double(5) = ${double(5)}`);
    console.log(`   triple(5) = ${triple(5)}`);

    // Curried function
    const curriedAdd = a => b => c => a + b + c;
    const add5 = curriedAdd(5);
    const add5And10 = add5(10);
    console.log(`   curriedAdd(5)(10)(20) = ${add5And10(20)}`);
}

// ====================
// 9. Function Composition
// ====================

const compose = (...fns) => x => fns.reduceRight((v, f) => f(v), x);

const pipe = (...fns) => x => fns.reduce((v, f) => f(v), x);

function demonstrateComposition() {
    const addOne = x => x + 1;
    const double = x => x * 2;
    const square = x => x ** 2;

    // Right-to-left
    const f = compose(double, addOne);
    console.log(`   compose(double, addOne)(5) = ${f(5)}`);

    // Left-to-right
    const g = pipe(addOne, double, square);
    console.log(`   pipe(addOne, double, square)(5) = ${g(5)}`);

    // Complex pipeline
    const processNumber = pipe(
        x => x + 1,
        x => x * 2,
        x => x ** 2,
        x => x - 10
    );
    console.log(`   processNumber(5) = ${processNumber(5)}`);
}

// ====================
// 10. Common HOFs
// ====================

function demonstrateCommonHOFs() {
    const numbers = [1, 2, 3, 4, 5];

    // every - all elements satisfy predicate
    const allPositive = numbers.every(x => x > 0);
    console.log(`   All positive? ${allPositive}`);

    // some - any element satisfies predicate
    const hasEven = numbers.some(x => x % 2 === 0);
    console.log(`   Has even? ${hasEven}`);

    // find - first element matching predicate
    const firstEven = numbers.find(x => x % 2 === 0);
    console.log(`   First even: ${firstEven}`);

    // findIndex
    const indexOfFirstEven = numbers.findIndex(x => x % 2 === 0);
    console.log(`   Index of first even: ${indexOfFirstEven}`);

    // sort with compare function
    const words = ['apple', 'pie', 'zoo', 'a'];
    const byLength = [...words].sort((a, b) => a.length - b.length);
    console.log(`   Sorted by length: [${byLength}]`);
}

// ====================
// 11. FlatMap and Advanced Operations
// ====================

function demonstrateFlatMap() {
    // flatMap = map + flatten
    const numbers = [1, 2, 3];
    const result = numbers.flatMap(x => [x, x * 2]);
    console.log(`   flatMap: [${result}]`);

    // Practical example
    const sentences = ['Hello world', 'How are you'];
    const words = sentences.flatMap(s => s.split(' '));
    console.log(`   Words: [${words}]`);
}

// ====================
// 12. Higher-Order Functions for Events
// ====================

function makeEventHandler(context) {
    // Closure captures context
    return function(event) {
        console.log(`   Event in context: ${context}`);
    };
}

// ====================
// 13. Memoization
// ====================

function memoize(fn) {
    const cache = new Map();

    return function(...args) {
        const key = JSON.stringify(args);
        if (cache.has(key)) {
            return cache.get(key);
        }
        const result = fn(...args);
        cache.set(key, result);
        return result;
    };
}

const fibonacci = memoize(function fib(n) {
    if (n <= 1) return n;
    return fib(n - 1) + fib(n - 2);
});

// ====================
// 14. Debounce and Throttle
// ====================

function debounce(fn, delay) {
    let timeoutId;
    return function(...args) {
        clearTimeout(timeoutId);
        timeoutId = setTimeout(() => fn(...args), delay);
    };
}

function throttle(fn, limit) {
    let inThrottle;
    return function(...args) {
        if (!inThrottle) {
            fn(...args);
            inThrottle = true;
            setTimeout(() => inThrottle = false, limit);
        }
    };
}

// ====================
// 15. Real-World Example: Data Pipeline
// ====================

function processUsers(users) {
    return users
        .filter(u => u.active)
        .map(u => ({
            ...u,
            name: u.name.trim().toLowerCase(),
            category: u.age >= 18 ? 'adult' : 'minor'
        }))
        .sort((a, b) => b.age - a.age)
        .slice(0, 10);  // Top 10
}

// ====================
// Main Demonstration
// ====================

function main() {
    console.log("=== Higher-Order Functions in JavaScript ===\n");

    // 1. First-class functions
    console.log("1. Functions as First-Class Values:");
    demonstrateFirstClass();

    // 2. Functions taking functions
    console.log("\n2. Functions Taking Functions:");
    console.log(`   applyTwice(x => x + 1, 5) = ${applyTwice(x => x + 1, 5)}`);
    console.log(`   applyNTimes(x => x * 2, 3, 2) = ${applyNTimes(x => x * 2, 3, 2)}`);

    // 3. Functions returning functions
    console.log("\n3. Functions Returning Functions:");
    const timesThree = makeMultiplier(3);
    const addTen = makeAdder(10);
    console.log(`   timesThree(7) = ${timesThree(7)}`);
    console.log(`   addTen(5) = ${addTen(5)}`);
    console.log(`   add(1)(2)(3) = ${add(1)(2)(3)}`);

    // 4. Map
    console.log("\n4. Map - Transform Each Element:");
    demonstrateMap();

    // 5. Filter
    console.log("\n5. Filter - Select Elements:");
    demonstrateFilter();

    // 6. Reduce
    console.log("\n6. Reduce - Combine to Single Value:");
    demonstrateReduce();

    // 7. Closures
    console.log("\n7. Closures:");
    const counter = makeCounter();
    console.log(`   counter() = ${counter()}`);
    console.log(`   counter() = ${counter()}`);
    console.log(`   counter() = ${counter()}`);

    const account = makeBankAccount(1000);
    console.log(`   Initial balance: $${account.getBalance()}`);
    account.deposit(500);
    console.log(`   After deposit: $${account.getBalance()}`);
    account.withdraw(200);
    console.log(`   After withdrawal: $${account.getBalance()}`);

    // 8. Partial application and currying
    console.log("\n8. Partial Application and Currying:");
    demonstratePartial();

    // 9. Function composition
    console.log("\n9. Function Composition:");
    demonstrateComposition();

    // 10. Common HOFs
    console.log("\n10. Common Higher-Order Functions:");
    demonstrateCommonHOFs();

    // 11. FlatMap
    console.log("\n11. FlatMap:");
    demonstrateFlatMap();

    // 12. Memoization
    console.log("\n12. Memoization:");
    console.log(`   fibonacci(10) = ${fibonacci(10)}`);
    console.log(`   fibonacci(10) again (cached) = ${fibonacci(10)}`);

    // 13. Real-world example
    console.log("\n13. Real-World Data Pipeline:");
    const users = [
        { name: ' Alice ', age: 25, active: true },
        { name: 'BOB', age: 17, active: true },
        { name: 'Charlie ', age: 30, active: false },
        { name: 'DIANA', age: 28, active: true },
    ];
    const processed = processUsers(users);
    console.log(`   Processed: ${JSON.stringify(processed, null, 2)}`);

    // 14. Method chaining
    console.log("\n14. Method Chaining:");
    const result = [1, -2, 3, -4, 5, 6, 7, 8, 9, 10]
        .filter(x => x > 0)
        .map(x => x ** 2)
        .reduce((sum, x) => sum + x, 0);
    console.log(`   Sum of squares of positive numbers: ${result}`);
}

// Run if executed directly
if (require.main === module) {
    main();
}

module.exports = {
    applyTwice,
    makeMultiplier,
    compose,
    pipe,
    memoize,
    debounce,
    throttle
};
