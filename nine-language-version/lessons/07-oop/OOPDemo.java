/**
 * Lesson 7: Object-Oriented Programming in Java
 *
 * Java is a pure object-oriented language - everything must be in a class.
 * Demonstrates all four pillars: Encapsulation, Inheritance, Polymorphism, Abstraction
 */

import java.util.*;

// ====================
// 1. Basic Class
// ====================

class Person {
    private String name;
    private int age;  // Private - encapsulation!

    public Person(String name, int age) {
        this.name = name;
        this.age = age;
    }

    public String introduce() {
        return String.format("Hi, I'm %s, %d years old", name, age);
    }

    public void haveBirthday() {
        age++;
    }

    public int getAge() {
        return age;
    }

    public String getName() {
        return name;
    }
}

// ====================
// 2. Inheritance
// ====================

class Animal {
    protected String name;
    protected String species;

    public Animal(String name, String species) {
        this.name = name;
        this.species = species;
    }

    public String speak() {
        return "Some generic animal sound";
    }

    public String sleep() {
        return name + " is sleeping... Zzz";
    }
}

class Dog extends Animal {
    private String breed;

    public Dog(String name, String breed) {
        super(name, "Canine");  // Call parent constructor
        this.breed = breed;
    }

    @Override  // Method overriding
    public String speak() {
        return name + " says Woof!";
    }

    public String fetch() {
        return name + " is fetching the ball!";
    }

    public String getBreed() {
        return breed;
    }
}

class Cat extends Animal {
    private boolean indoor;

    public Cat(String name, boolean indoor) {
        super(name, "Feline");
        this.indoor = indoor;
    }

    @Override
    public String speak() {
        return name + " says Meow!";
    }

    public String scratch() {
        return name + " is scratching the furniture!";
    }

    public boolean isIndoor() {
        return indoor;
    }
}

// ====================
// 3. Abstract Classes and Interfaces
// ====================

abstract class Shape {
    // Abstract methods - must be implemented by subclasses
    public abstract double area();
    public abstract double perimeter();

    // Concrete method - available to all shapes
    public String describe() {
        return String.format("%s: area=%.2f, perimeter=%.2f",
            this.getClass().getSimpleName(), area(), perimeter());
    }
}

class Circle extends Shape {
    private double radius;

    public Circle(double radius) {
        this.radius = radius;
    }

    @Override
    public double area() {
        return Math.PI * radius * radius;
    }

    @Override
    public double perimeter() {
        return 2 * Math.PI * radius;
    }

    public double getRadius() {
        return radius;
    }
}

class Rectangle extends Shape {
    private double width;
    private double height;

    public Rectangle(double width, double height) {
        this.width = width;
        this.height = height;
    }

    @Override
    public double area() {
        return width * height;
    }

    @Override
    public double perimeter() {
        return 2 * (width + height);
    }
}

class Triangle extends Shape {
    private double sideA;
    private double sideB;
    private double sideC;

    public Triangle(double sideA, double sideB, double sideC) {
        this.sideA = sideA;
        this.sideB = sideB;
        this.sideC = sideC;
    }

    @Override
    public double area() {
        // Heron's formula
        double s = perimeter() / 2;
        return Math.sqrt(s * (s - sideA) * (s - sideB) * (s - sideC));
    }

    @Override
    public double perimeter() {
        return sideA + sideB + sideC;
    }
}

// ====================
// 4. Interfaces
// ====================

interface Drawable {
    void draw();
    void erase();
}

interface Resizable {
    void resize(double factor);
}

// A class can implement multiple interfaces
class GraphicCircle extends Circle implements Drawable, Resizable {
    private double scaleFactor = 1.0;

    public GraphicCircle(double radius) {
        super(radius);
    }

    @Override
    public void draw() {
        System.out.println("   Drawing circle with radius " + getRadius() * scaleFactor);
    }

    @Override
    public void erase() {
        System.out.println("   Erasing circle");
    }

    @Override
    public void resize(double factor) {
        this.scaleFactor = factor;
        System.out.println("   Resizing circle by factor " + factor);
    }
}

// ====================
// 5. Encapsulation (Bank Account)
// ====================

class BankAccount {
    private String accountNumber;
    private double balance;  // Private!
    private List<String> transactions;

    public BankAccount(String accountNumber, double initialBalance) {
        this.accountNumber = accountNumber;
        this.balance = initialBalance;
        this.transactions = new ArrayList<>();
    }

    public boolean deposit(double amount) {
        if (amount > 0) {
            balance += amount;
            transactions.add(String.format("Deposit: +$%.2f", amount));
            return true;
        }
        return false;
    }

    public boolean withdraw(double amount) {
        if (amount > 0 && amount <= balance) {
            balance -= amount;
            transactions.add(String.format("Withdrawal: -$%.2f", amount));
            return true;
        }
        return false;
    }

    public double getBalance() {
        return balance;
    }

    public List<String> getTransactionHistory() {
        return new ArrayList<>(transactions);  // Return copy
    }

    public String getAccountNumber() {
        return accountNumber;
    }
}

// ====================
// 6. Static Members and Methods
// ====================

class Temperature {
    private double celsius;
    private static int conversionCount = 0;  // Class variable

    public Temperature(double celsius) {
        this.celsius = celsius;
    }

    // Factory methods using static
    public static Temperature fromFahrenheit(double fahrenheit) {
        conversionCount++;
        double celsius = (fahrenheit - 32) * 5.0 / 9.0;
        return new Temperature(celsius);
    }

    public static Temperature fromKelvin(double kelvin) {
        conversionCount++;
        double celsius = kelvin - 273.15;
        return new Temperature(celsius);
    }

    public static boolean isFreezing(double celsius) {
        return celsius <= 0;
    }

    public static int getConversionCount() {
        return conversionCount;
    }

    public double toFahrenheit() {
        return (celsius * 9.0 / 5.0) + 32;
    }

    public double toKelvin() {
        return celsius + 273.15;
    }

    public double getCelsius() {
        return celsius;
    }
}

// ====================
// 7. Composition
// ====================

class Engine {
    private int horsepower;
    private boolean running;

    public Engine(int horsepower) {
        this.horsepower = horsepower;
        this.running = false;
    }

    public String start() {
        running = true;
        return String.format("Engine starting... %dhp engine now running", horsepower);
    }

    public String stop() {
        running = false;
        return "Engine stopped";
    }

    public boolean isRunning() {
        return running;
    }
}

class Car {
    private String brand;
    private String model;
    private Engine engine;  // Composition - Car HAS-AN Engine

    public Car(String brand, String model, int horsepower) {
        this.brand = brand;
        this.model = model;
        this.engine = new Engine(horsepower);
    }

    public String start() {
        return String.format("%s %s: %s", brand, model, engine.start());
    }

    public String stop() {
        return String.format("%s %s: %s", brand, model, engine.stop());
    }
}

// ====================
// 8. Design Pattern: Singleton
// ====================

class Singleton {
    private static Singleton instance = null;
    private List<String> data;

    // Private constructor prevents instantiation
    private Singleton() {
        data = new ArrayList<>();
    }

    public static Singleton getInstance() {
        if (instance == null) {
            instance = new Singleton();
        }
        return instance;
    }

    public void addData(String item) {
        data.add(item);
    }

    public List<String> getData() {
        return data;
    }
}

// ====================
// 9. Design Pattern: Factory
// ====================

class AnimalFactory {
    public static Animal createAnimal(String type, String name) {
        switch (type.toLowerCase()) {
            case "dog":
                return new Dog(name, "Mixed");
            case "cat":
                return new Cat(name, true);
            default:
                return new Animal(name, "Unknown");
        }
    }
}

// ====================
// 10. Nested Classes
// ====================

class OuterClass {
    private String outerField = "Outer field";

    // Inner class has access to outer class members
    class InnerClass {
        public void display() {
            System.out.println("   Accessing from inner: " + outerField);
        }
    }

    // Static nested class
    static class StaticNestedClass {
        public void display() {
            System.out.println("   Static nested class");
        }
    }
}

// ====================
// 11. Enums (Special Class Type)
// ====================

enum Day {
    MONDAY("Start of work week"),
    TUESDAY("Second day"),
    WEDNESDAY("Midweek"),
    THURSDAY("Almost there"),
    FRIDAY("TGIF"),
    SATURDAY("Weekend!"),
    SUNDAY("Rest day");

    private final String description;

    Day(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }
}

// ====================
// Main Class with Examples
// ====================

public class OOPDemo {
    public static void main(String[] args) {
        System.out.println("=== Object-Oriented Programming in Java ===\n");

        // 1. Basic class
        System.out.println("1. Basic Class:");
        Person alice = new Person("Alice", 30);
        System.out.println("   " + alice.introduce());
        alice.haveBirthday();
        System.out.println("   After birthday: age = " + alice.getAge());

        // 2. Inheritance and Polymorphism
        System.out.println("\n2. Inheritance and Polymorphism:");
        List<Animal> animals = new ArrayList<>();
        animals.add(new Dog("Buddy", "Golden Retriever"));
        animals.add(new Cat("Whiskers", true));
        animals.add(new Dog("Max", "German Shepherd"));

        for (Animal animal : animals) {
            System.out.println("   " + animal.speak());
        }

        System.out.println("   " + ((Dog)animals.get(0)).fetch());
        System.out.println("   " + ((Cat)animals.get(1)).scratch());

        // 3. Abstract classes and shapes
        System.out.println("\n3. Abstract Classes (Shapes):");
        List<Shape> shapes = new ArrayList<>();
        shapes.add(new Circle(5));
        shapes.add(new Rectangle(4, 6));
        shapes.add(new Triangle(3, 4, 5));

        for (Shape shape : shapes) {
            System.out.println("   " + shape.describe());
        }

        // 4. Interfaces
        System.out.println("\n4. Interfaces:");
        GraphicCircle gc = new GraphicCircle(10);
        gc.draw();
        gc.resize(1.5);
        gc.draw();
        gc.erase();

        // 5. Encapsulation (Bank Account)
        System.out.println("\n5. Encapsulation (Bank Account):");
        BankAccount account = new BankAccount("ACC001", 1000);
        System.out.println("   Initial balance: $" + account.getBalance());
        account.deposit(500);
        System.out.println("   After deposit: $" + account.getBalance());
        account.withdraw(200);
        System.out.println("   After withdrawal: $" + account.getBalance());
        System.out.println("   Transactions: " + account.getTransactionHistory());

        // 6. Static methods
        System.out.println("\n6. Static Methods:");
        Temperature temp1 = new Temperature(0);
        Temperature temp2 = Temperature.fromFahrenheit(32);
        Temperature temp3 = Temperature.fromKelvin(273.15);

        System.out.printf("   0°C = %.1f°F%n", temp1.toFahrenheit());
        System.out.printf("   32°F = %.1f°C%n", temp2.getCelsius());
        System.out.printf("   273.15K = %.1f°C%n", temp3.getCelsius());
        System.out.println("   Is 0°C freezing? " + Temperature.isFreezing(0));
        System.out.println("   Conversions made: " + Temperature.getConversionCount());

        // 7. Composition
        System.out.println("\n7. Composition:");
        Car car = new Car("Toyota", "Camry", 200);
        System.out.println("   " + car.start());
        System.out.println("   " + car.stop());

        // 8. Singleton
        System.out.println("\n8. Singleton Pattern:");
        Singleton s1 = Singleton.getInstance();
        Singleton s2 = Singleton.getInstance();
        System.out.println("   s1 == s2? " + (s1 == s2));
        s1.addData("item1");
        System.out.println("   s1.data: " + s1.getData());
        System.out.println("   s2.data: " + s2.getData());

        // 9. Factory
        System.out.println("\n9. Factory Pattern:");
        Animal dog = AnimalFactory.createAnimal("dog", "Rover");
        Animal cat = AnimalFactory.createAnimal("cat", "Mittens");
        System.out.println("   " + dog.speak());
        System.out.println("   " + cat.speak());

        // 10. Nested classes
        System.out.println("\n10. Nested Classes:");
        OuterClass outer = new OuterClass();
        OuterClass.InnerClass inner = outer.new InnerClass();
        inner.display();
        OuterClass.StaticNestedClass nested = new OuterClass.StaticNestedClass();
        nested.display();

        // 11. Enums
        System.out.println("\n11. Enums:");
        Day today = Day.FRIDAY;
        System.out.println("   Today is " + today);
        System.out.println("   Description: " + today.getDescription());
    }
}
