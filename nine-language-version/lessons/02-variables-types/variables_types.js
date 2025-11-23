// variables_types.js

// Variable declarations (3 ways)
var oldStyle = "avoid var";     // function-scoped (old)
let mutable = "can change";     // block-scoped
const immutable = "cannot reassign";  // block-scoped, constant

// Numbers (only one number type!)
let age = 25;                   // integer value
let price = 19.99;              // floating-point value
// Both are actually 64-bit floats

// Strings
let name = "Alice";
let greeting = 'Hello';
let template = `Hello, ${name}!`;  // template literals

// Booleans
let isStudent = true;
let hasGraduated = false;

// Null and Undefined
let nothing = null;             // intentional absence
let notDefined;                 // undefined (not initialized)

// Type checking
console.log("Type of age:", typeof age);
console.log("Type of name:", typeof name);

// Weak typing - automatic conversions
console.log('"5" + 3 =', "5" + 3);       // "53" (string concatenation)
console.log('"5" - 3 =', "5" - 3);       // 2 (numeric subtraction)
console.log('true + 1 =', true + 1);     // 2 (boolean to number)
