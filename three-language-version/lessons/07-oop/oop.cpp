/**
 * Lesson 7: Object-Oriented Programming in C++
 *
 * This file demonstrates OOP concepts in C++:
 * - Classes and objects
 * - Encapsulation with access modifiers
 * - Inheritance (single and multiple)
 * - Polymorphism (virtual functions and templates)
 * - Abstraction (pure virtual functions)
 * - Operator overloading
 * - Static members
 * - Const correctness
 * - Design patterns
 *
 * C++'s OOP Features:
 * - Static typing with compile-time checks
 * - True encapsulation (private/protected/public)
 * - Virtual functions for runtime polymorphism
 * - Templates for compile-time polymorphism
 * - Multiple inheritance with virtual base classes
 * - RAII and smart pointers for memory management
 */

#include <iostream>
#include <string>
#include <vector>
#include <memory>
#include <cmath>
#include <stdexcept>
#include <algorithm>

using namespace std;

// ============================================================================
// PART 1: BASIC CLASSES AND OBJECTS
// ============================================================================

class Person {
private:
    string name;
    int age;
    // Static members - shared by all instances
    static int population;

public:
    // Constructor
    Person(const string& n, int a) : name(n), age(a) {
        ++population;
    }

    // Copy constructor
    Person(const Person& other) : name(other.name), age(other.age) {
        ++population;
    }

    // Destructor
    ~Person() {
        --population;
    }

    // Getters (const member functions - don't modify object)
    string getName() const { return name; }
    int getAge() const { return age; }

    // Setters
    void setAge(int a) { age = a; }

    // Instance methods
    string introduce() const {
        return "Hi, I'm " + name + ", " + to_string(age) + " years old";
    }

    void haveBirthday() {
        ++age;
        cout << "Happy birthday " << name << "! Now " << age << " years old." << endl;
    }

    // Static methods - operate on class, not instance
    static int getPopulation() {
        return population;
    }

    static bool isAdult(int age) {
        return age >= 18;
    }

    // Operator overloading
    bool operator<(const Person& other) const {
        return age < other.age;
    }

    bool operator==(const Person& other) const {
        return name == other.name && age == other.age;
    }

    // Friend function for stream output
    friend ostream& operator<<(ostream& os, const Person& p);
};

// Initialize static member
int Person::population = 0;

// Friend function definition
ostream& operator<<(ostream& os, const Person& p) {
    os << p.name << " (" << p.age << ")";
    return os;
}

// ============================================================================
// PART 2: ENCAPSULATION
// ============================================================================

class BankAccount {
private:
    string accountNumber;
    double balance;  // Truly private - compile error if accessed directly

    struct Transaction {
        string type;
        double amount;
        double balanceAfter;
    };

    vector<Transaction> transactions;

    // Private helper method
    void addTransaction(const string& type, double amount) {
        transactions.push_back({type, amount, balance});
    }

public:
    // Constructor with default parameter
    BankAccount(const string& accNum, double initialBalance = 0.0)
        : accountNumber(accNum), balance(initialBalance) {}

    // Public methods control access to private data
    bool deposit(double amount) {
        if (amount > 0) {
            balance += amount;
            addTransaction("deposit", amount);
            return true;
        }
        return false;
    }

    bool withdraw(double amount) {
        if (amount > 0 && amount <= balance) {
            balance -= amount;
            addTransaction("withdrawal", amount);
            return true;
        }
        return false;
    }

    // Const getter - doesn't modify object
    double getBalance() const {
        return balance;
    }

    const vector<Transaction>& getTransactionHistory() const {
        return transactions;
    }
};

// ============================================================================
// PART 3: INHERITANCE - SINGLE INHERITANCE
// ============================================================================

class Animal {
protected:  // Accessible to derived classes
    string name;
    int age;

public:
    Animal(const string& n, int a) : name(n), age(a) {}

    // Virtual function - can be overridden
    virtual string speak() const {
        return name + " makes a sound";
    }

    // Non-virtual function
    string describe() const {
        return name + " is a " + to_string(age) + " year old animal";
    }

    // Virtual destructor - ESSENTIAL for polymorphism!
    virtual ~Animal() {}
};

class Dog : public Animal {
private:
    string breed;

public:
    Dog(const string& n, int a, const string& b)
        : Animal(n, a), breed(b) {}

    // Override virtual function
    string speak() const override {
        return name + " says Woof!";
    }

    // New method specific to Dog
    string fetch() const {
        return name + " is fetching the ball!";
    }

    string getBreed() const { return breed; }
};

class Cat : public Animal {
public:
    Cat(const string& n, int a) : Animal(n, a) {}

    string speak() const override {
        return name + " says Meow!";
    }

    string climb() const {
        return name + " is climbing a tree!";
    }
};

// ============================================================================
// PART 4: MULTIPLE INHERITANCE
// ============================================================================

class Flyable {
protected:
    string name;  // Note: can cause issues with multiple inheritance

public:
    virtual string fly() const {
        return name + " is flying through the air";
    }

    virtual string land() const {
        return name + " is landing";
    }

    virtual ~Flyable() {}
};

class Swimmable {
protected:
    string name;

public:
    virtual string swim() const {
        return name + " is swimming in water";
    }

    virtual string dive() const {
        return name + " is diving deep";
    }

    virtual ~Swimmable() {}
};

class Duck : public Animal, public Flyable, public Swimmable {
public:
    Duck(const string& n, int a) : Animal(n, a) {
        // Must set name in all base classes due to multiple inheritance
        Flyable::name = n;
        Swimmable::name = n;
    }

    string speak() const override {
        return Animal::name + " says Quack!";
    }

    string waddle() const {
        return Animal::name + " is waddling";
    }

    // Note: fly(), swim() inherited from Flyable and Swimmable
};

// Diamond Problem Solution - Virtual Inheritance
class Base {
protected:
    int value;

public:
    Base(int v = 0) : value(v) {}
    virtual ~Base() {}
};

class Left : virtual public Base {
public:
    Left(int v = 0) : Base(v) {}
};

class Right : virtual public Base {
public:
    Right(int v = 0) : Base(v) {}
};

class Bottom : public Left, public Right {
public:
    Bottom(int v = 0) : Base(v), Left(v), Right(v) {}
    // With virtual inheritance, only ONE Base object exists
};

// ============================================================================
// PART 5: ABSTRACTION - PURE VIRTUAL FUNCTIONS
// ============================================================================

class Shape {
protected:
    string color;

public:
    Shape(const string& c) : color(c) {}

    // Pure virtual functions - make class abstract
    virtual double area() const = 0;
    virtual double perimeter() const = 0;

    // Concrete method in abstract class
    virtual string describe() const {
        return "A " + color + " shape with area " + to_string(area());
    }

    // Virtual destructor
    virtual ~Shape() {}
};

class Circle : public Shape {
private:
    double radius;

public:
    Circle(const string& c, double r) : Shape(c), radius(r) {}

    double area() const override {
        return M_PI * radius * radius;
    }

    double perimeter() const override {
        return 2 * M_PI * radius;
    }
};

class Rectangle : public Shape {
private:
    double width, height;

public:
    Rectangle(const string& c, double w, double h)
        : Shape(c), width(w), height(h) {}

    double area() const override {
        return width * height;
    }

    double perimeter() const override {
        return 2 * (width + height);
    }
};

// ============================================================================
// PART 6: POLYMORPHISM
// ============================================================================

// Runtime polymorphism with virtual functions
void printAnimalSounds(const vector<Animal*>& animals) {
    for (const auto& animal : animals) {
        cout << animal->speak() << endl;
    }
}

// Compile-time polymorphism with templates
template<typename T>
void makeItSpeak(const T& thing) {
    cout << thing.speak() << endl;
}

// Template specialization
template<>
void makeItSpeak<Dog>(const Dog& dog) {
    cout << dog.speak() << " (Woof is from a " << dog.getBreed() << "!)" << endl;
}

// ============================================================================
// PART 7: OPERATOR OVERLOADING
// ============================================================================

class Vector2D {
private:
    double x, y;

public:
    Vector2D(double x = 0, double y = 0) : x(x), y(y) {}

    // Arithmetic operators
    Vector2D operator+(const Vector2D& other) const {
        return Vector2D(x + other.x, y + other.y);
    }

    Vector2D operator-(const Vector2D& other) const {
        return Vector2D(x - other.x, y - other.y);
    }

    Vector2D operator*(double scalar) const {
        return Vector2D(x * scalar, y * scalar);
    }

    // Comparison operators
    bool operator==(const Vector2D& other) const {
        return x == other.x && y == other.y;
    }

    // Compound assignment
    Vector2D& operator+=(const Vector2D& other) {
        x += other.x;
        y += other.y;
        return *this;
    }

    // Unary operators
    Vector2D operator-() const {
        return Vector2D(-x, -y);
    }

    // Getters
    double getX() const { return x; }
    double getY() const { return y; }

    // Magnitude
    double magnitude() const {
        return sqrt(x * x + y * y);
    }

    // Friend functions for non-member operators
    friend Vector2D operator*(double scalar, const Vector2D& v);
    friend ostream& operator<<(ostream& os, const Vector2D& v);
};

// Friend function definitions
Vector2D operator*(double scalar, const Vector2D& v) {
    return v * scalar;
}

ostream& operator<<(ostream& os, const Vector2D& v) {
    os << "Vector2D(" << v.x << ", " << v.y << ")";
    return os;
}

// ============================================================================
// PART 8: PROPERTIES (GETTERS/SETTERS WITH VALIDATION)
// ============================================================================

class Temperature {
private:
    double celsius;

public:
    Temperature(double c) {
        setCelsius(c);
    }

    // Getters
    double getCelsius() const {
        return celsius;
    }

    double getFahrenheit() const {
        return celsius * 9.0/5.0 + 32;
    }

    double getKelvin() const {
        return celsius + 273.15;
    }

    // Setters with validation
    void setCelsius(double c) {
        if (c < -273.15) {
            throw invalid_argument("Temperature below absolute zero!");
        }
        celsius = c;
    }

    void setFahrenheit(double f) {
        setCelsius((f - 32) * 5.0/9.0);
    }

    void setKelvin(double k) {
        setCelsius(k - 273.15);
    }
};

// ============================================================================
// PART 9: COMPOSITION OVER INHERITANCE
// ============================================================================

class Engine {
private:
    int horsepower;
    string fuelType;
    bool running;

public:
    Engine(int hp, const string& fuel)
        : horsepower(hp), fuelType(fuel), running(false) {}

    string start() {
        running = true;
        return "Engine starting... " + to_string(horsepower) +
               " HP " + fuelType + " engine roaring!";
    }

    string stop() {
        running = false;
        return "Engine stopping...";
    }

    bool isRunning() const { return running; }
};

class Vehicle {
private:
    string brand;
    unique_ptr<Engine> engine;  // Owns the engine (composition)

public:
    // Constructor with optional engine
    Vehicle(const string& b, unique_ptr<Engine> eng = nullptr)
        : brand(b), engine(move(eng)) {}

    string start() {
        if (engine) {
            return brand + ": " + engine->start();
        }
        return brand + ": No engine - use pedal power!";
    }

    string stop() {
        if (engine) {
            return brand + ": " + engine->stop();
        }
        return brand + ": No engine to stop!";
    }
};

// ============================================================================
// PART 10: DESIGN PATTERNS
// ============================================================================

// Singleton Pattern
class Logger {
private:
    vector<string> logs;

    // Private constructor
    Logger() {}

    // Delete copy constructor and assignment operator
    Logger(const Logger&) = delete;
    Logger& operator=(const Logger&) = delete;

public:
    static Logger& getInstance() {
        static Logger instance;  // Thread-safe in C++11
        return instance;
    }

    void log(const string& message) {
        logs.push_back(message);
    }

    const vector<string>& getLogs() const {
        return logs;
    }

    void clearLogs() {
        logs.clear();
    }
};

// Factory Pattern
class AnimalFactory {
public:
    static unique_ptr<Animal> createAnimal(const string& type,
                                           const string& name,
                                           int age) {
        if (type == "dog") {
            return make_unique<Dog>(name, age, "Unknown");
        } else if (type == "cat") {
            return make_unique<Cat>(name, age);
        } else {
            throw invalid_argument("Unknown animal type: " + type);
        }
    }
};

// Observer Pattern
class Observer {
public:
    virtual void update(int state) = 0;
    virtual ~Observer() {}
};

class Subject {
private:
    vector<Observer*> observers;
    int state;

public:
    void attach(Observer* observer) {
        observers.push_back(observer);
    }

    void detach(Observer* observer) {
        observers.erase(
            remove(observers.begin(), observers.end(), observer),
            observers.end()
        );
    }

    void notify() {
        for (auto observer : observers) {
            observer->update(state);
        }
    }

    void setState(int s) {
        state = s;
        notify();
    }

    int getState() const {
        return state;
    }
};

class ConcreteObserver : public Observer {
private:
    string name;

public:
    ConcreteObserver(const string& n) : name(n) {}

    void update(int state) override {
        cout << name << " notified. New state: " << state << endl;
    }
};

// ============================================================================
// DEMONSTRATION AND TESTING
// ============================================================================

int main() {
    cout << string(70, '=') << endl;
    cout << "LESSON 7: OBJECT-ORIENTED PROGRAMMING IN C++" << endl;
    cout << string(70, '=') << endl;

    // Part 1: Basic Classes
    cout << "\n--- PART 1: BASIC CLASSES ---" << endl;
    Person alice("Alice", 30);
    Person bob("Bob", 25);
    cout << alice.introduce() << endl;
    cout << "Population: " << Person::getPopulation() << endl;
    cout << "Is adult? " << Person::isAdult(alice.getAge()) << endl;
    cout << "String representation: " << alice << endl;
    cout << "Alice < Bob? " << (alice < bob) << endl;

    // Part 2: Encapsulation
    cout << "\n--- PART 2: ENCAPSULATION ---" << endl;
    BankAccount account("ACC001", 1000);
    account.deposit(500);
    account.withdraw(200);
    cout << "Balance: $" << account.getBalance() << endl;
    cout << "Transactions: " << account.getTransactionHistory().size() << endl;

    // Part 3: Inheritance
    cout << "\n--- PART 3: INHERITANCE ---" << endl;
    Dog dog("Buddy", 3, "Golden Retriever");
    Cat cat("Whiskers", 2);
    cout << dog.describe() << endl;
    cout << dog.speak() << endl;
    cout << dog.fetch() << endl;
    cout << cat.speak() << endl;
    cout << cat.climb() << endl;

    // Part 4: Multiple Inheritance
    cout << "\n--- PART 4: MULTIPLE INHERITANCE ---" << endl;
    Duck duck("Donald", 1);
    cout << duck.speak() << endl;
    cout << duck.fly() << endl;
    cout << duck.swim() << endl;
    cout << duck.waddle() << endl;

    // Virtual inheritance demo
    Bottom b(42);
    cout << "Virtual inheritance prevents diamond duplication" << endl;

    // Part 5: Abstraction
    cout << "\n--- PART 5: ABSTRACTION ---" << endl;
    vector<unique_ptr<Shape>> shapes;
    shapes.push_back(make_unique<Circle>("red", 5));
    shapes.push_back(make_unique<Rectangle>("blue", 4, 6));
    shapes.push_back(make_unique<Circle>("green", 3));

    for (const auto& shape : shapes) {
        cout << shape->describe() << endl;
    }

    // Part 6: Polymorphism
    cout << "\n--- PART 6: POLYMORPHISM ---" << endl;

    // Runtime polymorphism
    vector<Animal*> animals = {&dog, &cat, &duck};
    printAnimalSounds(animals);

    // Compile-time polymorphism
    makeItSpeak(dog);  // Uses template specialization
    makeItSpeak(cat);

    // Part 7: Operator Overloading
    cout << "\n--- PART 7: OPERATOR OVERLOADING ---" << endl;
    Vector2D v1(3, 4);
    Vector2D v2(1, 2);
    Vector2D v3 = v1 + v2;
    Vector2D v4 = v1 * 2;
    Vector2D v5 = 2 * v1;  // Uses friend function

    cout << "v1 = " << v1 << endl;
    cout << "v2 = " << v2 << endl;
    cout << "v1 + v2 = " << v3 << endl;
    cout << "v1 * 2 = " << v4 << endl;
    cout << "2 * v1 = " << v5 << endl;
    cout << "|v1| = " << v1.magnitude() << endl;

    // Part 8: Properties (Getters/Setters)
    cout << "\n--- PART 8: PROPERTIES ---" << endl;
    Temperature temp(0);
    cout << "0°C = " << temp.getFahrenheit() << "°F = "
         << temp.getKelvin() << "K" << endl;
    temp.setFahrenheit(212);
    cout << "212°F = " << temp.getCelsius() << "°C" << endl;

    // Part 9: Composition
    cout << "\n--- PART 9: COMPOSITION ---" << endl;
    auto engine = make_unique<Engine>(200, "gasoline");
    Vehicle car("Toyota", move(engine));
    Vehicle bicycle("Schwinn", nullptr);

    cout << car.start() << endl;
    cout << bicycle.start() << endl;

    // Part 10: Design Patterns
    cout << "\n--- PART 10: DESIGN PATTERNS ---" << endl;

    // Singleton
    Logger& logger1 = Logger::getInstance();
    Logger& logger2 = Logger::getInstance();
    logger1.log("First message");
    logger2.log("Second message");
    cout << "Logger is singleton? " << (&logger1 == &logger2) << endl;
    cout << "Logs: " << logger1.getLogs().size() << endl;

    // Factory
    auto factoryDog = AnimalFactory::createAnimal("dog", "Max", 2);
    cout << "Factory created: " << factoryDog->speak() << endl;

    // Observer
    Subject subject;
    ConcreteObserver obs1("Observer 1");
    ConcreteObserver obs2("Observer 2");
    subject.attach(&obs1);
    subject.attach(&obs2);
    subject.setState(100);

    cout << "\n" << string(70, '=') << endl;
    cout << "C++ OOP demonstration complete!" << endl;
    cout << string(70, '=') << endl;

    return 0;
}
