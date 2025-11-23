#!/usr/bin/env node

/**
 * Lesson 9: Pattern Matching in JavaScript
 *
 * JavaScript doesn't have native pattern matching (yet - there's a proposal),
 * but we can simulate it with various techniques:
 * - Object destructuring
 * - Switch statements with fall-through
 * - Guard patterns with if/else
 * - Libraries like ts-pattern
 *
 * This demonstrates pattern-like techniques available in JavaScript.
 */

// ====================
// 1. Basic Switch (Limited Pattern Matching)
// ====================

function describeNumber(n) {
    switch (n) {
        case 0: return "zero";
        case 1: return "one";
        case 2: return "two";
        default: return `many: ${n}`;
    }
}

// ====================
// 2. Destructuring (Tuple-like Patterns)
// ====================

function describePoint([x, y]) {
    if (x === 0 && y === 0) return "origin";
    if (x === 0) return `on y-axis at y=${y}`;
    if (y === 0) return `on x-axis at x=${x}`;
    return `point at (${x}, ${y})`;
}

function describeList(lst) {
    const [first, second, ...rest] = lst;

    if (lst.length === 0) return "empty list";
    if (lst.length === 1) return `single element: ${first}`;
    if (lst.length === 2) return `two elements: ${first}, ${second}`;
    return `first: ${first}, rest: [${rest.join(', ')}]`;
}

// ====================
// 3. Object Destructuring
// ====================

function processCommand(cmd) {
    const { action, direction, target, damage } = cmd;

    switch (action) {
        case "quit":
            return "Quitting...";
        case "move":
            return `Moving ${direction}`;
        case "attack":
            return `Attacking ${target} for ${damage} damage`;
        default:
            return `Unknown action: ${action}`;
    }
}

// ====================
// 4. Type-Based Pattern Simulation
// ====================

class Shape {
    getType() { return "Shape"; }
}

class Circle extends Shape {
    constructor(radius) {
        super();
        this.radius = radius;
    }
    getType() { return "Circle"; }
}

class Rectangle extends Shape {
    constructor(width, height) {
        super();
        this.width = width;
        this.height = height;
    }
    getType() { return "Rectangle"; }
}

class Triangle extends Shape {
    constructor(a, b, c) {
        super();
        this.a = a;
        this.b = b;
        this.c = c;
    }
    getType() { return "Triangle"; }
}

function area(shape) {
    if (shape instanceof Circle) {
        return Math.PI * shape.radius * shape.radius;
    } else if (shape instanceof Rectangle) {
        return shape.width * shape.height;
    } else if (shape instanceof Triangle) {
        const { a, b, c } = shape;
        const s = (a + b + c) / 2;
        return Math.sqrt(s * (s - a) * (s - b) * (s - c));
    }
    throw new Error("Unknown shape");
}

// ====================
// 5. Guards (Conditional Patterns)
// ====================

function classify(n) {
    if (n < 0) return "negative";
    if (n === 0) return "zero";
    if (n < 10) return "small positive";
    if (n < 100) return "medium positive";
    return "large positive";
}

// ====================
// 6. Multiple Values (OR patterns)
// ====================

function isWeekend(day) {
    const d = day.toLowerCase();
    return d === "saturday" || d === "sunday";
}

// ====================
// 7. Pattern Matching Helper Function
// ====================

function match(value, patterns) {
    for (const [predicate, result] of patterns) {
        if (typeof predicate === 'function') {
            if (predicate(value)) {
                return typeof result === 'function' ? result(value) : result;
            }
        } else if (predicate === value) {
            return typeof result === 'function' ? result(value) : result;
        }
    }
    throw new Error(`No pattern matched for value: ${value}`);
}

function gradeLetterUsingMatch(score) {
    return match(score, [
        [s => s >= 90 && s <= 100, 'A'],
        [s => s >= 80 && s < 90, 'B'],
        [s => s >= 70 && s < 80, 'C'],
        [s => s >= 60 && s < 70, 'D'],
        [s => s < 60, 'F']
    ]);
}

// ====================
// 8. Optional/Nullable Pattern
// ====================

function describeMaybe(opt) {
    if (opt === null || opt === undefined) {
        return "nothing";
    }
    return `just ${opt}`;
}

function describeResult(result) {
    if (result.ok) {
        return `success: ${result.value}`;
    }
    return `error: ${result.error}`;
}

// ====================
// 9. Expression Evaluator
// ====================

class Expr {
    eval() { throw new Error("Abstract method"); }
}

class Num extends Expr {
    constructor(value) {
        super();
        this.value = value;
    }
    eval() { return this.value; }
}

class Add extends Expr {
    constructor(left, right) {
        super();
        this.left = left;
        this.right = right;
    }
    eval() { return this.left.eval() + this.right.eval(); }
}

class Mul extends Expr {
    constructor(left, right) {
        super();
        this.left = left;
        this.right = right;
    }
    eval() { return this.left.eval() * this.right.eval(); }
}

class Neg extends Expr {
    constructor(expr) {
        super();
        this.expr = expr;
    }
    eval() { return -this.expr.eval(); }
}

// ====================
// 10. Nested Patterns
// ====================

function analyzeNested(data) {
    const { user, active, error } = data;

    if (error) {
        return `Error: ${error}`;
    }

    if (user) {
        const { name, age } = user;
        if (active) {
            return `Active user: ${name}${age ? ` (${age})` : ''}`;
        }
        return `Inactive user: ${name}`;
    }

    return "Unknown data";
}

// ====================
// 11. State Machine
// ====================

function trafficLightNext(current, action) {
    if (action === "emergency") return "red";

    const transitions = {
        "red": { "timer": "green" },
        "green": { "timer": "yellow" },
        "yellow": { "timer": "red" }
    };

    return transitions[current]?.[action] || current;
}

// ====================
// 12. Advanced Pattern Helper (Visitor-like)
// ====================

function matchObject(obj, patterns) {
    for (const pattern of patterns) {
        if (pattern.when(obj)) {
            return pattern.then(obj);
        }
    }
    return patterns.find(p => p.default)?.then(obj) || null;
}

function processUser(userData) {
    return matchObject(userData, [
        {
            when: u => u.type === "admin",
            then: u => `Admin ${u.name} with ${u.permissions?.length || 0} permissions`
        },
        {
            when: u => u.type === "user" && u.age >= 18,
            then: u => `Adult user: ${u.name}`
        },
        {
            when: u => u.type === "user",
            then: u => `Minor user: ${u.name}`
        },
        {
            when: u => u.type === "guest",
            then: u => `Guest session: ${u.session}`
        },
        {
            default: true,
            when: () => true,
            then: () => "Unknown user type"
        }
    ]);
}

// ====================
// Main Demonstration
// ====================

function main() {
    console.log("=== Pattern Matching in JavaScript ===\n");

    // 1. Basic switch
    console.log("1. Basic Switch Patterns:");
    [0, 1, 2, 5].forEach(n => {
        console.log(`   describeNumber(${n}) = ${describeNumber(n)}`);
    });

    // 2. Destructuring patterns
    console.log("\n2. Destructuring (Tuple-like) Patterns:");
    const points = [[0, 0], [0, 5], [3, 0], [2, 3]];
    points.forEach(p => {
        console.log(`   [${p}] -> ${describePoint(p)}`);
    });

    // 3. List patterns
    console.log("\n3. List Patterns:");
    const lists = [[], [1], [1, 2], [1, 2, 3, 4]];
    lists.forEach(lst => {
        console.log(`   [${lst}] -> ${describeList(lst)}`);
    });

    // 4. Object patterns
    console.log("\n4. Object Patterns:");
    const commands = [
        { action: "quit" },
        { action: "move", direction: "north" },
        { action: "attack", target: "orc", damage: 10 }
    ];
    commands.forEach(cmd => {
        console.log(`   ${JSON.stringify(cmd)}`);
        console.log(`   -> ${processCommand(cmd)}`);
    });

    // 5. Type-based patterns
    console.log("\n5. Type-Based Patterns (instanceof):");
    const shapes = [
        new Circle(5),
        new Rectangle(4, 6),
        new Triangle(3, 4, 5)
    ];
    shapes.forEach(shape => {
        console.log(`   ${shape.getType()}: area = ${area(shape).toFixed(2)}`);
    });

    // 6. Guards
    console.log("\n6. Guards (Conditional Patterns):");
    [-5, 0, 3, 50, 500].forEach(n => {
        console.log(`   ${n} -> ${classify(n)}`);
    });

    // 7. OR patterns
    console.log("\n7. Multiple Values (OR patterns):");
    ["Monday", "Saturday", "Sunday", "Wednesday"].forEach(day => {
        console.log(`   ${day} is weekend? ${isWeekend(day)}`);
    });

    // 8. Match helper
    console.log("\n8. Custom Match Helper:");
    [95, 85, 75, 65, 55].forEach(score => {
        console.log(`   ${score} -> ${gradeLetterUsingMatch(score)}`);
    });

    // 9. Optional patterns
    console.log("\n9. Optional/Nullable Patterns:");
    console.log(`   ${describeMaybe(42)}`);
    console.log(`   ${describeMaybe(null)}`);
    console.log(`   ${describeResult({ ok: true, value: 100 })}`);
    console.log(`   ${describeResult({ ok: false, error: "failed" })}`);

    // 10. Expression evaluator
    console.log("\n10. Expression Evaluator:");
    const expr = new Mul(new Add(new Num(2), new Num(3)), new Num(4));
    console.log(`   (2 + 3) * 4 = ${expr.eval()}`);
    const expr2 = new Neg(new Add(new Num(5), new Num(3)));
    console.log(`   -(5 + 3) = ${expr2.eval()}`);

    // 11. Nested patterns
    console.log("\n11. Nested Patterns:");
    const nestedData = [
        { user: { name: "Alice", age: 30 }, active: true },
        { user: { name: "Bob" }, active: false },
        { error: "Not found" }
    ];
    nestedData.forEach(data => {
        console.log(`   ${analyzeNested(data)}`);
    });

    // 12. State machine
    console.log("\n12. State Machine (Traffic Light):");
    let state = "red";
    ["timer", "timer", "timer", "emergency"].forEach(action => {
        state = trafficLightNext(state, action);
        console.log(`   After '${action}': ${state}`);
    });

    // 13. Advanced pattern matching
    console.log("\n13. Advanced Object Matching:");
    const users = [
        { type: "admin", name: "Alice", permissions: ["read", "write"] },
        { type: "user", name: "Bob", age: 25 },
        { type: "user", name: "Charlie", age: 16 },
        { type: "guest", session: "abc123" }
    ];
    users.forEach(user => {
        console.log(`   ${processUser(user)}`);
    });

    console.log("\n=== JavaScript Pattern Matching Notes ===");
    console.log("- No native pattern matching (yet - TC39 proposal in progress)");
    console.log("- Use destructuring for simple patterns");
    console.log("- instanceof for type-based patterns");
    console.log("- Custom helper functions for complex matching");
    console.log("- Libraries like ts-pattern provide full pattern matching");
}

main();
