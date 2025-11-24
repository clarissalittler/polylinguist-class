#!/usr/bin/env node
/**
 * Lesson 7: Object-Oriented Programming in JavaScript
 *
 * JavaScript is prototype-based, but ES6+ added class syntax.
 * Shows both classical OOP (classes) and prototypal inheritance.
 */

// ====================
// 1. Basic Class (ES6+)
// ====================

class Person {
    constructor(name, age) {
        this.name = name;
        this._age = age;  // Convention for "private"
    }

    introduce() {
        return `Hi, I'm ${this.name}, ${this._age} years old`;
    }

    haveBirthday() {
        this._age++;
    }

    getAge() {
        return this._age;
    }
}

// ====================
// 2. Inheritance
// ====================

class Animal {
    constructor(name, species) {
        this.name = name;
        this.species = species;
    }

    speak() {
        return "Some generic animal sound";
    }

    sleep() {
        return `${this.name} is sleeping... Zzz`;
    }
}

class Dog extends Animal {
    constructor(name, breed) {
        super(name, "Canine");  // Call parent constructor
        this.breed = breed;
    }

    speak() {  // Override
        return `${this.name} says Woof!`;
    }

    fetch() {
        return `${this.name} is fetching the ball!`;
    }
}

class Cat extends Animal {
    constructor(name, indoor = true) {
        super(name, "Feline");
        this.indoor = indoor;
    }

    speak() {  // Override
        return `${this.name} says Meow!`;
    }

    scratch() {
        return `${this.name} is scratching the furniture!`;
    }
}

// ====================
// 3. Abstract-like Classes
// ====================
// Note: JavaScript doesn't have built-in abstract classes,
// but we can simulate them

class Shape {
    constructor() {
        if (new.target === Shape) {
            throw new Error("Cannot instantiate abstract class Shape");
        }
    }

    area() {
        throw new Error("Must implement area()");
    }

    perimeter() {
        throw new Error("Must implement perimeter()");
    }

    describe() {
        return `${this.constructor.name}: area=${this.area().toFixed(2)}, perimeter=${this.perimeter().toFixed(2)}`;
    }
}

class Circle extends Shape {
    constructor(radius) {
        super();
        this.radius = radius;
    }

    area() {
        return Math.PI * this.radius ** 2;
    }

    perimeter() {
        return 2 * Math.PI * this.radius;
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

    perimeter() {
        return 2 * (this.width + this.height);
    }
}

class Triangle extends Shape {
    constructor(sideA, sideB, sideC) {
        super();
        this.sideA = sideA;
        this.sideB = sideB;
        this.sideC = sideC;
    }

    area() {
        // Heron's formula
        const s = this.perimeter() / 2;
        return Math.sqrt(s * (s - this.sideA) * (s - this.sideB) * (s - this.sideC));
    }

    perimeter() {
        return this.sideA + this.sideB + this.sideC;
    }
}

// ====================
// 4. Encapsulation with Private Fields (ES2022)
// ====================

class BankAccount {
    #balance;  // Private field (ES2022)
    #transactions;

    constructor(accountNumber, initialBalance = 0) {
        this.accountNumber = accountNumber;
        this.#balance = initialBalance;
        this.#transactions = [];
    }

    deposit(amount) {
        if (amount > 0) {
            this.#balance += amount;
            this.#transactions.push(`Deposit: +$${amount}`);
            return true;
        }
        return false;
    }

    withdraw(amount) {
        if (amount > 0 && amount <= this.#balance) {
            this.#balance -= amount;
            this.#transactions.push(`Withdrawal: -$${amount}`);
            return true;
        }
        return false;
    }

    getBalance() {
        return this.#balance;
    }

    getTransactionHistory() {
        return [...this.#transactions];  // Return copy
    }
}

// ====================
// 5. Static Methods and Properties
// ====================

class Temperature {
    static conversionCount = 0;  // Static property

    constructor(celsius) {
        this.celsius = celsius;
    }

    // Factory methods
    static fromFahrenheit(fahrenheit) {
        Temperature.conversionCount++;
        const celsius = (fahrenheit - 32) * 5 / 9;
        return new Temperature(celsius);
    }

    static fromKelvin(kelvin) {
        Temperature.conversionCount++;
        const celsius = kelvin - 273.15;
        return new Temperature(celsius);
    }

    static isFreezing(celsius) {
        return celsius <= 0;
    }

    toFahrenheit() {
        return (this.celsius * 9 / 5) + 32;
    }

    toKelvin() {
        return this.celsius + 273.15;
    }
}

// ====================
// 6. Composition
// ====================

class Engine {
    constructor(horsepower) {
        this.horsepower = horsepower;
        this.running = false;
    }

    start() {
        this.running = true;
        return `Engine starting... ${this.horsepower}hp engine now running`;
    }

    stop() {
        this.running = false;
        return "Engine stopped";
    }
}

class Car {
    constructor(brand, model, horsepower) {
        this.brand = brand;
        this.model = model;
        this.engine = new Engine(horsepower);  // Composition!
    }

    start() {
        return `${this.brand} ${this.model}: ${this.engine.start()}`;
    }

    stop() {
        return `${this.brand} ${this.model}: ${this.engine.stop()}`;
    }
}

// ====================
// 7. Getters and Setters
// ====================

class Circle2 {
    constructor(radius) {
        this._radius = radius;
    }

    get radius() {
        return this._radius;
    }

    set radius(value) {
        if (value < 0) {
            throw new Error("Radius cannot be negative");
        }
        this._radius = value;
    }

    get diameter() {
        return this._radius * 2;
    }

    get area() {
        return Math.PI * this._radius ** 2;
    }
}

// ====================
// 8. Design Pattern: Singleton
// ====================

class Singleton {
    static #instance = null;

    constructor() {
        if (Singleton.#instance) {
            return Singleton.#instance;
        }
        this.data = [];
        Singleton.#instance = this;
    }

    static getInstance() {
        if (!Singleton.#instance) {
            Singleton.#instance = new Singleton();
        }
        return Singleton.#instance;
    }
}

// ====================
// 9. Design Pattern: Factory
// ====================

class AnimalFactory {
    static createAnimal(type, name) {
        switch (type.toLowerCase()) {
            case "dog":
                return new Dog(name, "Mixed");
            case "cat":
                return new Cat(name);
            default:
                return new Animal(name, "Unknown");
        }
    }
}

// ====================
// 10. Prototype-Based OOP (Old-style JavaScript)
// ====================

// Constructor function (pre-ES6 way)
function Point(x, y) {
    this.x = x;
    this.y = y;
}

// Adding methods to prototype
Point.prototype.toString = function() {
    return `Point(${this.x}, ${this.y})`;
};

Point.prototype.distanceFromOrigin = function() {
    return Math.sqrt(this.x ** 2 + this.y ** 2);
};

Point.prototype.add = function(other) {
    return new Point(this.x + other.x, this.y + other.y);
};

Point.prototype.subtract = function(other) {
    return new Point(this.x - other.x, this.y - other.y);
};

// ====================
// 11. Mixins (Multiple Inheritance Alternative)
// ====================

// Mixin objects
const Flyable = {
    fly() {
        return `${this.name} is flying through the air`;
    }
};

const Swimmable = {
    swim() {
        return `${this.name} is swimming in water`;
    }
};

class Duck extends Animal {
    constructor(name) {
        super(name, "Waterfowl");
    }

    quack() {
        return "Quack!";
    }
}

// Apply mixins
Object.assign(Duck.prototype, Flyable, Swimmable);

// ====================
// 12. Symbol for "Private" Properties (pre-ES2022)
// ====================

const _balance = Symbol('balance');

class OldStyleAccount {
    constructor(initialBalance) {
        this[_balance] = initialBalance;
    }

    getBalance() {
        return this[_balance];
    }

    deposit(amount) {
        this[_balance] += amount;
    }
}

// ====================
// Tests and Examples
// ====================

function main() {
    console.log("=== Object-Oriented Programming in JavaScript ===\n");

    // 1. Basic class
    console.log("1. Basic Class:");
    const alice = new Person("Alice", 30);
    console.log(`   ${alice.introduce()}`);
    alice.haveBirthday();
    console.log(`   After birthday: age = ${alice.getAge()}`);

    // 2. Inheritance and Polymorphism
    console.log("\n2. Inheritance and Polymorphism:");
    const animals = [
        new Dog("Buddy", "Golden Retriever"),
        new Cat("Whiskers", true),
        new Dog("Max", "German Shepherd")
    ];

    animals.forEach(animal => {
        console.log(`   ${animal.speak()}`);
    });

    console.log(`   ${animals[0].fetch()}`);
    console.log(`   ${animals[1].scratch()}`);

    // 3. Abstract-like classes and shapes
    console.log("\n3. Abstract-like Classes (Shapes):");
    const shapes = [
        new Circle(5),
        new Rectangle(4, 6),
        new Triangle(3, 4, 5)
    ];

    shapes.forEach(shape => {
        console.log(`   ${shape.describe()}`);
    });

    // 4. Encapsulation (Bank Account with private fields)
    console.log("\n4. Encapsulation (Bank Account):");
    const account = new BankAccount("ACC001", 1000);
    console.log(`   Initial balance: $${account.getBalance()}`);
    account.deposit(500);
    console.log(`   After deposit: $${account.getBalance()}`);
    account.withdraw(200);
    console.log(`   After withdrawal: $${account.getBalance()}`);
    console.log(`   Transactions: ${JSON.stringify(account.getTransactionHistory())}`);

    // 5. Static methods
    console.log("\n5. Static Methods:");
    const temp1 = new Temperature(0);
    const temp2 = Temperature.fromFahrenheit(32);
    const temp3 = Temperature.fromKelvin(273.15);

    console.log(`   0°C = ${temp1.toFahrenheit().toFixed(1)}°F`);
    console.log(`   32°F = ${temp2.celsius.toFixed(1)}°C`);
    console.log(`   273.15K = ${temp3.celsius.toFixed(1)}°C`);
    console.log(`   Is 0°C freezing? ${Temperature.isFreezing(0)}`);
    console.log(`   Conversions made: ${Temperature.conversionCount}`);

    // 6. Composition
    console.log("\n6. Composition:");
    const car = new Car("Toyota", "Camry", 200);
    console.log(`   ${car.start()}`);
    console.log(`   ${car.stop()}`);

    // 7. Getters and Setters
    console.log("\n7. Getters and Setters:");
    const circle = new Circle2(5);
    console.log(`   Radius: ${circle.radius}`);
    console.log(`   Diameter: ${circle.diameter}`);
    console.log(`   Area: ${circle.area.toFixed(2)}`);
    circle.radius = 10;
    console.log(`   After setting radius to 10:`);
    console.log(`   Diameter: ${circle.diameter}`);
    console.log(`   Area: ${circle.area.toFixed(2)}`);

    // 8. Singleton
    console.log("\n8. Singleton Pattern:");
    const s1 = new Singleton();
    const s2 = new Singleton();
    console.log(`   s1 === s2? ${s1 === s2}`);
    s1.data.push("item1");
    console.log(`   s1.data: ${JSON.stringify(s1.data)}`);
    console.log(`   s2.data: ${JSON.stringify(s2.data)}`);

    // 9. Factory
    console.log("\n9. Factory Pattern:");
    const dog = AnimalFactory.createAnimal("dog", "Rover");
    const cat = AnimalFactory.createAnimal("cat", "Mittens");
    console.log(`   ${dog.speak()}`);
    console.log(`   ${cat.speak()}`);

    // 10. Prototype-based OOP
    console.log("\n10. Prototype-based OOP:");
    const p1 = new Point(3, 4);
    const p2 = new Point(1, 2);
    console.log(`   p1 = ${p1.toString()}`);
    console.log(`   p2 = ${p2.toString()}`);
    const p3 = p1.add(p2);
    console.log(`   p1 + p2 = ${p3.toString()}`);
    console.log(`   p1 distance from origin: ${p1.distanceFromOrigin().toFixed(2)}`);

    // 11. Mixins
    console.log("\n11. Mixins (Multiple Inheritance Alternative):");
    const duck = new Duck("Donald");
    console.log(`   ${duck.quack()}`);
    console.log(`   ${duck.fly()}`);
    console.log(`   ${duck.swim()}`);
}

// Run if executed directly
if (require.main === module) {
    main();
}

module.exports = {
    Person, Animal, Dog, Cat, Shape, Circle, Rectangle, Triangle,
    BankAccount, Temperature, Engine, Car, AnimalFactory, Point
};
