#!/usr/bin/env node

/**
 * Lesson 11: Error Handling & Debugging in JavaScript
 *
 * JavaScript features:
 * - try/catch/finally blocks
 * - Error types (Error, TypeError, ReferenceError, etc.)
 * - Custom errors
 * - Promise error handling (.catch())
 * - Async/await error handling
 * - console methods for debugging
 *
 * This demonstrates JavaScript's error handling.
 */

console.log("=== Error Handling in JavaScript ===\n");

// ====================
// 1. Basic try/catch
// ====================

console.log("1. Basic try/catch:");

function divide(x, y) {
    try {
        if (typeof x !== 'number' || typeof y !== 'number') {
            throw new TypeError("Arguments must be numbers");
        }
        if (y === 0) {
            throw new Error("Cannot divide by zero");
        }
        return x / y;
    } catch (error) {
        console.log(`   Error: ${error.message}`);
        return NaN;
    }
}

console.log(`   divide(10, 2) = ${divide(10, 2)}`);
console.log(`   divide(10, 0) = ${divide(10, 0)}`);
console.log(`   divide('10', 2) = ${divide('10', 2)}`);

// ====================
// 2. Error types
// ====================

console.log("\n2. Different Error Types:");

function processValue(value) {
    try {
        if (value === undefined) {
            throw new ReferenceError("Value is undefined");
        }
        if (typeof value !== 'number') {
            throw new TypeError("Expected a number");
        }
        if (value < 0) {
            throw new RangeError("Value must be non-negative");
        }
        console.log(`   Processing: ${value}`);
        return value * 2;
    } catch (error) {
        if (error instanceof TypeError) {
            console.log(`   Type error: ${error.message}`);
        } else if (error instanceof RangeError) {
            console.log(`   Range error: ${error.message}`);
        } else {
            console.log(`   Error: ${error.message}`);
        }
        return null;
    }
}

processValue(5);
processValue("hello");
processValue(-3);

// ====================
// 3. try/catch/finally
// ====================

console.log("\n3. try/catch/finally:");

function withCleanup() {
    console.log("   Starting operation...");
    try {
        console.log("   Executing...");
        throw new Error("Something went wrong");
    } catch (error) {
        console.log(`   Caught: ${error.message}`);
    } finally {
        console.log("   Cleanup (always runs)");
    }
}

withCleanup();

// ====================
// 4. Custom errors
// ====================

console.log("\n4. Custom Errors:");

class ValidationError extends Error {
    constructor(field, message) {
        super(message);
        this.name = "ValidationError";
        this.field = field;
    }
}

class EmailValidationError extends ValidationError {
    constructor(email, reason) {
        super('email', `Invalid email '${email}': ${reason}`);
        this.email = email;
        this.reason = reason;
    }
}

function validateEmail(email) {
    if (!email.includes('@')) {
        throw new EmailValidationError(email, "missing @ symbol");
    }
    if (!email.split('@')[1].includes('.')) {
        throw new EmailValidationError(email, "missing domain extension");
    }
    console.log(`   Valid email: ${email}`);
    return true;
}

try {
    validateEmail("user@example.com");
    validateEmail("invalid-email");
} catch (error) {
    if (error instanceof EmailValidationError) {
        console.log(`   ${error.message}`);
    }
}

// ====================
// 5. Promise error handling
// ====================

console.log("\n5. Promise Error Handling:");

function fetchUserData(userId) {
    return new Promise((resolve, reject) => {
        setTimeout(() => {
            if (userId < 0) {
                reject(new Error("Invalid user ID"));
            } else {
                resolve({ id: userId, name: `User${userId}` });
            }
        }, 100);
    });
}

// .then().catch()
fetchUserData(1)
    .then(user => console.log(`   Success: ${user.name}`))
    .catch(error => console.log(`   Error: ${error.message}`));

fetchUserData(-1)
    .then(user => console.log(`   Success: ${user.name}`))
    .catch(error => console.log(`   Error: ${error.message}`));

// ====================
// 6. Async/await error handling
// ====================

console.log("\n6. Async/Await Error Handling:");

async function loadUserData(userId) {
    try {
        console.log(`   Loading user ${userId}...`);
        const user = await fetchUserData(userId);
        console.log(`   Loaded: ${user.name}`);
        return user;
    } catch (error) {
        console.log(`   Failed to load user: ${error.message}`);
        return null;
    }
}

// Run async function
(async () => {
    await loadUserData(2);
    await loadUserData(-2);
})();

// ====================
// 7. Error recovery
// ====================

console.log("\n7. Error Recovery:");

function robustParse(items) {
    const results = [];
    const errors = [];

    for (let i = 0; i < items.length; i++) {
        try {
            const num = parseInt(items[i]);
            if (isNaN(num)) {
                throw new Error(`Not a number: ${items[i]}`);
            }
            results.push(num);
        } catch (error) {
            errors.push({ index: i, error: error.message });
        }
    }

    console.log(`   Parsed ${results.length}/${items.length} items`);
    console.log(`   Results: [${results.join(', ')}]`);
    if (errors.length > 0) {
        console.log(`   Errors: ${errors.length}`);
    }

    return results;
}

robustParse(['1', '2', 'bad', '4', 'invalid', '6']);

// ====================
// 8. Debugging console methods
// ====================

console.log("\n8. Console Methods for Debugging:");

const user = { name: "Alice", age: 30, roles: ["admin", "user"] };

console.log("   console.log:", "Simple message");
console.info("   console.info:", "Informational message");
console.warn("   console.warn:", "Warning message");
console.error("   console.error:", "Error message");

console.log("   Object:", user);
console.table([
    { name: "Alice", age: 30 },
    { name: "Bob", age: 25 }
]);

console.time("operation");
// Some operation
for (let i = 0; i < 1000000; i++) {}
console.timeEnd("operation");

// ====================
// 9. Stack traces
// ====================

console.log("\n9. Stack Traces:");

function functionA() {
    functionB();
}

function functionB() {
    functionC();
}

function functionC() {
    try {
        throw new Error("Error in functionC");
    } catch (error) {
        console.log("   Error:", error.message);
        console.log("   Stack trace:");
        console.log(error.stack.split('\n').slice(0, 4).join('\n'));
    }
}

functionA();

// ====================
// 10. Error handling patterns
// ====================

console.log("\n10. Common Patterns:");

// Retry pattern
async function retry(fn, maxAttempts = 3) {
    for (let attempt = 1; attempt <= maxAttempts; attempt++) {
        try {
            console.log(`   Attempt ${attempt}/${maxAttempts}`);
            return await fn();
        } catch (error) {
            if (attempt === maxAttempts) throw error;
            console.log(`   Failed, retrying...`);
        }
    }
}

// Fallback pattern
function getWithFallback(getter, fallback) {
    try {
        return getter();
    } catch (error) {
        console.log(`   Using fallback: ${error.message}`);
        return fallback;
    }
}

const result = getWithFallback(
    () => { throw new Error("Failed to get value"); },
    "default value"
);
console.log(`   Result: ${result}`);

// ====================
// Summary
// ====================

setTimeout(() => {
    console.log("\n=== JavaScript Error Handling Features ===");
    console.log("- try/catch/finally blocks");
    console.log("- Built-in error types (Error, TypeError, etc.)");
    console.log("- Custom error classes");
    console.log("- Promise error handling (.catch())");
    console.log("- Async/await with try/catch");
    console.log("- Console methods for debugging");
    console.log("- Stack traces for error context");
    console.log("- Error recovery patterns");
}, 500);
