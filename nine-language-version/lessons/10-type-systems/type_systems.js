#!/usr/bin/env node

/**
 * Lesson 10: Type Systems in JavaScript
 *
 * JavaScript is dynamically typed:
 * - No compile-time type checking
 * - Types determined at runtime
 * - Duck typing (if it quacks like a duck...)
 * - TypeScript adds optional static types
 * - JSDoc comments for type hints
 *
 * This demonstrates JavaScript's dynamic typing.
 */

// ====================
// 1. Dynamic Typing
// ====================

function increment(x) {
    return x + 1;
}

function add(x, y) {
    return x + y;
}

// Works with different types!
console.log("1. Dynamic Typing:");
console.log(`   add(1, 2) = ${add(1, 2)}`);
console.log(`   add("hello", " world") = ${add("hello", " world")}`);
console.log(`   add([1], [2]) = ${add([1], [2])}`);  // Array concatenation

// ====================
// 2. Type Checking with typeof
// ====================

function describe_type(value) {
    const type = typeof value;
    return `${value} is a ${type}`;
}

console.log("\n2. Runtime Type Checking (typeof):");
console.log(`   ${describe_type(42)}`);
console.log(`   ${describe_type("hello")}`);
console.log(`   ${describe_type(true)}`);
console.log(`   ${describe_type({name: "Alice"})}`);

// ====================
// 3. Duck Typing
// ====================

function draw(shape) {
    if (typeof shape.draw === 'function') {
        return shape.draw();
    }
    throw new Error("Object doesn't have a draw method");
}

const circle = {
    radius: 5,
    draw() { return `Drawing circle with radius ${this.radius}`; }
};

const rectangle = {
    width: 4,
    height: 6,
    draw() { return `Drawing rectangle ${this.width}x${this.height}`; }
};

console.log("\n3. Duck Typing:");
console.log(`   ${draw(circle)}`);
console.log(`   ${draw(rectangle)}`);

// ====================
// 4. Optional Chaining and Nullish Coalescing
// ====================

function safe_get_name(user) {
    return user?.name ?? "Unknown";
}

console.log("\n4. Optional Chaining:");
console.log(`   safe_get_name({name: "Alice"}) = ${safe_get_name({name: "Alice"})}`);
console.log(`   safe_get_name(null) = ${safe_get_name(null)}`);
console.log(`   safe_get_name({}) = ${safe_get_name({})}`);

// ====================
// 5. Classes with typeof checks
// ====================

class Shape {
    area() { throw new Error("Abstract method"); }
}

class Circle extends Shape {
    constructor(radius) {
        super();
        this.radius = radius;
    }

    area() {
        return Math.PI * this.radius ** 2;
    }
}

class Rectangle extends Shape {
    constructor(width, height) {
        super();
        this.width = width;
        this.height = height;
    }

    area() {
        return this.width * this.height;
    }
}

console.log("\n5. Classes with instanceof:");
const shapes = [new Circle(5), new Rectangle(4, 6)];
for (const shape of shapes) {
    console.log(`   ${shape.constructor.name}: area = ${shape.area().toFixed(2)}`);
}

// ====================
// 6. JSDoc Type Annotations (for tooling)
// ====================

/**
 * Divides two numbers safely
 * @param {number} x - Numerator
 * @param {number} y - Denominator
 * @returns {number|null} Result or null if division by zero
 */
function safe_divide(x, y) {
    if (y === 0) return null;
    return x / y;
}

console.log("\n6. JSDoc Annotations:");
console.log(`   safe_divide(10, 2) = ${safe_divide(10, 2)}`);
console.log(`   safe_divide(10, 0) = ${safe_divide(10, 0)}`);

// ====================
// 7. Type Coercion
// ====================

console.log("\n7. Type Coercion (Weak Typing):");
console.log(`   "5" + 3 = ${"5" + 3}`);  // String concatenation
console.log(`   "5" - 3 = ${"5" - 3}`);  // Numeric subtraction
console.log(`   true + 1 = ${true + 1}`);  // Boolean to number
console.log(`   [] + [] = ${[] + []}`);  // Empty string
console.log(`   [] + {} = ${[] + {}}`);  // "[object Object]"

// ====================
// 8. Symbols (Unique Types)
// ====================

const sym1 = Symbol("id");
const sym2 = Symbol("id");

console.log("\n8. Symbols:");
console.log(`   sym1 === sym2: ${sym1 === sym2}`);  // false
console.log(`   typeof sym1: ${typeof sym1}`);

// ====================
// 9. Generics via Functions
// ====================

function identity(x) {
    return x;
}

function first(array) {
    return array.length > 0 ? array[0] : null;
}

console.log("\n9. Generic Functions:");
console.log(`   identity(42) = ${identity(42)}`);
console.log(`   identity("hello") = ${identity("hello")}`);
console.log(`   first([1,2,3]) = ${first([1,2,3])}`);

// ====================
// 10. TypeScript Example (commented out - won't run in Node)
// ====================

/*
// TypeScript adds static types:

function add_typed(x: number, y: number): number {
    return x + y;
}

interface Point {
    x: number;
    y: number;
}

function distance(p1: Point, p2: Point): number {
    return Math.sqrt((p2.x - p1.x) ** 2 + (p2.y - p1.y) ** 2);
}

// Generics in TypeScript
function identity<T>(x: T): T {
    return x;
}

// Union types
type StringOrNumber = string | number;

// Type guards
function isString(value: any): value is string {
    return typeof value === "string";
}
*/

console.log("\n10. TypeScript (Static Typing for JS):");
console.log("   TypeScript adds optional static types to JavaScript");
console.log("   Compiles to JavaScript, types removed at runtime");
console.log("   See commented code in source for examples");

// ====================
// Summary
// ====================

console.log("\n=== JavaScript Type System Features ===");
console.log("- Dynamic typing (types at runtime)");
console.log("- Duck typing (structural)");
console.log("- Weak typing (implicit coercions)");
console.log("- typeof and instanceof for runtime checks");
console.log("- Optional chaining (?.) and nullish coalescing (??)");
console.log("- JSDoc for type hints");
console.log("- TypeScript for static typing");
console.log("- Flexible but error-prone");
