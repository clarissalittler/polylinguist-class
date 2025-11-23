/*
 * Lesson 7: Object-Oriented Programming in C
 *
 * C doesn't have built-in OOP, but we can simulate it using:
 * - Structs (for data)
 * - Function pointers (for methods/vtables)
 * - Manual memory management
 * - Naming conventions
 *
 * This demonstrates the underlying mechanisms that OOP languages use!
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

// ====================
// 1. Basic "Class" with Functions
// ====================

typedef struct {
    char name[50];
    int age;
} Person;

// Constructor-like function
Person* Person_new(const char* name, int age) {
    Person* p = (Person*)malloc(sizeof(Person));
    strncpy(p->name, name, 49);
    p->name[49] = '\0';
    p->age = age;
    return p;
}

// "Methods" - functions that take the struct as first parameter
void Person_introduce(Person* self) {
    printf("   Hi, I'm %s, %d years old\n", self->name, self->age);
}

void Person_haveBirthday(Person* self) {
    self->age++;
}

void Person_destroy(Person* self) {
    free(self);
}

// ====================
// 2. "Inheritance" with Composition
// ====================

typedef struct {
    char name[50];
    char species[50];
} Animal;

Animal* Animal_new(const char* name, const char* species) {
    Animal* a = (Animal*)malloc(sizeof(Animal));
    strncpy(a->name, name, 49);
    a->name[49] = '\0';
    strncpy(a->species, species, 49);
    a->species[49] = '\0';
    return a;
}

void Animal_speak(Animal* self) {
    printf("   %s says: Some generic animal sound\n", self->name);
}

void Animal_destroy(Animal* self) {
    free(self);
}

// Dog "inherits" from Animal by embedding it
typedef struct {
    Animal base;  // Embedded base "class"
    char breed[50];
} Dog;

Dog* Dog_new(const char* name, const char* breed) {
    Dog* d = (Dog*)malloc(sizeof(Dog));
    strncpy(d->base.name, name, 49);
    d->base.name[49] = '\0';
    strncpy(d->base.species, "Canine", 49);
    strncpy(d->breed, breed, 49);
    d->breed[49] = '\0';
    return d;
}

void Dog_speak(Dog* self) {
    printf("   %s says Woof!\n", self->base.name);
}

void Dog_fetch(Dog* self) {
    printf("   %s is fetching the ball!\n", self->base.name);
}

void Dog_destroy(Dog* self) {
    free(self);
}

// Cat "inherits" from Animal
typedef struct {
    Animal base;
    int indoor;
} Cat;

Cat* Cat_new(const char* name, int indoor) {
    Cat* c = (Cat*)malloc(sizeof(Cat));
    strncpy(c->base.name, name, 49);
    c->base.name[49] = '\0';
    strncpy(c->base.species, "Feline", 49);
    c->indoor = indoor;
    return c;
}

void Cat_speak(Cat* self) {
    printf("   %s says Meow!\n", self->base.name);
}

void Cat_scratch(Cat* self) {
    printf("   %s is scratching the furniture!\n", self->base.name);
}

void Cat_destroy(Cat* self) {
    free(self);
}

// ====================
// 3. Polymorphism with Function Pointers (VTable)
// ====================

// Shape interface
typedef struct Shape Shape;

typedef struct {
    double (*area)(Shape* self);
    double (*perimeter)(Shape* self);
    const char* (*name)(Shape* self);
} ShapeVTable;

struct Shape {
    ShapeVTable* vtable;
};

void Shape_describe(Shape* self) {
    printf("   %s: area=%.2f, perimeter=%.2f\n",
           self->vtable->name(self),
           self->vtable->area(self),
           self->vtable->perimeter(self));
}

// Circle implementation
typedef struct {
    Shape base;  // Inherit from Shape
    double radius;
} Circle;

double Circle_area(Shape* self) {
    Circle* c = (Circle*)self;
    return M_PI * c->radius * c->radius;
}

double Circle_perimeter(Shape* self) {
    Circle* c = (Circle*)self;
    return 2 * M_PI * c->radius;
}

const char* Circle_name(Shape* self) {
    return "Circle";
}

ShapeVTable Circle_vtable = {
    .area = Circle_area,
    .perimeter = Circle_perimeter,
    .name = Circle_name
};

Circle* Circle_new(double radius) {
    Circle* c = (Circle*)malloc(sizeof(Circle));
    c->base.vtable = &Circle_vtable;
    c->radius = radius;
    return c;
}

void Circle_destroy(Circle* self) {
    free(self);
}

// Rectangle implementation
typedef struct {
    Shape base;
    double width;
    double height;
} Rectangle;

double Rectangle_area(Shape* self) {
    Rectangle* r = (Rectangle*)self;
    return r->width * r->height;
}

double Rectangle_perimeter(Shape* self) {
    Rectangle* r = (Rectangle*)self;
    return 2 * (r->width + r->height);
}

const char* Rectangle_name(Shape* self) {
    return "Rectangle";
}

ShapeVTable Rectangle_vtable = {
    .area = Rectangle_area,
    .perimeter = Rectangle_perimeter,
    .name = Rectangle_name
};

Rectangle* Rectangle_new(double width, double height) {
    Rectangle* r = (Rectangle*)malloc(sizeof(Rectangle));
    r->base.vtable = &Rectangle_vtable;
    r->width = width;
    r->height = height;
    return r;
}

void Rectangle_destroy(Rectangle* self) {
    free(self);
}

// Triangle implementation
typedef struct {
    Shape base;
    double side_a;
    double side_b;
    double side_c;
} Triangle;

double Triangle_perimeter(Shape* self) {
    Triangle* t = (Triangle*)self;
    return t->side_a + t->side_b + t->side_c;
}

double Triangle_area(Shape* self) {
    Triangle* t = (Triangle*)self;
    double s = Triangle_perimeter(self) / 2.0;
    return sqrt(s * (s - t->side_a) * (s - t->side_b) * (s - t->side_c));
}

const char* Triangle_name(Shape* self) {
    return "Triangle";
}

ShapeVTable Triangle_vtable = {
    .area = Triangle_area,
    .perimeter = Triangle_perimeter,
    .name = Triangle_name
};

Triangle* Triangle_new(double side_a, double side_b, double side_c) {
    Triangle* t = (Triangle*)malloc(sizeof(Triangle));
    t->base.vtable = &Triangle_vtable;
    t->side_a = side_a;
    t->side_b = side_b;
    t->side_c = side_c;
    return t;
}

void Triangle_destroy(Triangle* self) {
    free(self);
}

// ====================
// 4. Encapsulation (using opaque pointers)
// ====================

// In header file, we'd only expose the typedef
typedef struct BankAccountImpl BankAccount;

// In implementation file, we define the struct
typedef struct BankAccountImpl {
    char account_number[20];
    double balance;  // "Private" - not accessible outside this file
    char** transactions;
    int transaction_count;
    int transaction_capacity;
} BankAccountImpl;

BankAccount* BankAccount_new(const char* account_number, double initial_balance) {
    BankAccount* account = (BankAccount*)malloc(sizeof(BankAccount));
    strncpy(account->account_number, account_number, 19);
    account->account_number[19] = '\0';
    account->balance = initial_balance;
    account->transaction_count = 0;
    account->transaction_capacity = 10;
    account->transactions = (char**)malloc(sizeof(char*) * account->transaction_capacity);
    return account;
}

int BankAccount_deposit(BankAccount* self, double amount) {
    if (amount > 0) {
        self->balance += amount;
        if (self->transaction_count >= self->transaction_capacity) {
            self->transaction_capacity *= 2;
            self->transactions = (char**)realloc(self->transactions,
                                                  sizeof(char*) * self->transaction_capacity);
        }
        char* transaction = (char*)malloc(100);
        snprintf(transaction, 100, "Deposit: +$%.2f", amount);
        self->transactions[self->transaction_count++] = transaction;
        return 1;
    }
    return 0;
}

int BankAccount_withdraw(BankAccount* self, double amount) {
    if (amount > 0 && amount <= self->balance) {
        self->balance -= amount;
        if (self->transaction_count >= self->transaction_capacity) {
            self->transaction_capacity *= 2;
            self->transactions = (char**)realloc(self->transactions,
                                                  sizeof(char*) * self->transaction_capacity);
        }
        char* transaction = (char*)malloc(100);
        snprintf(transaction, 100, "Withdrawal: -$%.2f", amount);
        self->transactions[self->transaction_count++] = transaction;
        return 1;
    }
    return 0;
}

double BankAccount_getBalance(BankAccount* self) {
    return self->balance;
}

void BankAccount_printTransactions(BankAccount* self) {
    printf("   Transactions: [");
    for (int i = 0; i < self->transaction_count; i++) {
        printf("%s", self->transactions[i]);
        if (i < self->transaction_count - 1) printf(", ");
    }
    printf("]\n");
}

void BankAccount_destroy(BankAccount* self) {
    for (int i = 0; i < self->transaction_count; i++) {
        free(self->transactions[i]);
    }
    free(self->transactions);
    free(self);
}

// ====================
// 5. Composition
// ====================

typedef struct {
    int horsepower;
    int running;
} Engine;

Engine* Engine_new(int horsepower) {
    Engine* e = (Engine*)malloc(sizeof(Engine));
    e->horsepower = horsepower;
    e->running = 0;
    return e;
}

void Engine_start(Engine* self) {
    self->running = 1;
    printf("Engine starting... %dhp engine now running", self->horsepower);
}

void Engine_stop(Engine* self) {
    self->running = 0;
    printf("Engine stopped");
}

void Engine_destroy(Engine* self) {
    free(self);
}

typedef struct {
    char brand[50];
    char model[50];
    Engine* engine;  // Composition!
} Car;

Car* Car_new(const char* brand, const char* model, int horsepower) {
    Car* c = (Car*)malloc(sizeof(Car));
    strncpy(c->brand, brand, 49);
    c->brand[49] = '\0';
    strncpy(c->model, model, 49);
    c->model[49] = '\0';
    c->engine = Engine_new(horsepower);
    return c;
}

void Car_start(Car* self) {
    printf("   %s %s: ", self->brand, self->model);
    Engine_start(self->engine);
    printf("\n");
}

void Car_stop(Car* self) {
    printf("   %s %s: ", self->brand, self->model);
    Engine_stop(self->engine);
    printf("\n");
}

void Car_destroy(Car* self) {
    Engine_destroy(self->engine);
    free(self);
}

// ====================
// 6. "Static" members (using file-scope variables)
// ====================

static int temperature_conversion_count = 0;

typedef struct {
    double celsius;
} Temperature;

Temperature* Temperature_new(double celsius) {
    Temperature* t = (Temperature*)malloc(sizeof(Temperature));
    t->celsius = celsius;
    return t;
}

Temperature* Temperature_fromFahrenheit(double fahrenheit) {
    temperature_conversion_count++;
    double celsius = (fahrenheit - 32) * 5.0 / 9.0;
    return Temperature_new(celsius);
}

Temperature* Temperature_fromKelvin(double kelvin) {
    temperature_conversion_count++;
    double celsius = kelvin - 273.15;
    return Temperature_new(celsius);
}

int Temperature_isFreezing(double celsius) {
    return celsius <= 0;
}

double Temperature_toFahrenheit(Temperature* self) {
    return (self->celsius * 9.0 / 5.0) + 32;
}

double Temperature_toKelvin(Temperature* self) {
    return self->celsius + 273.15;
}

void Temperature_destroy(Temperature* self) {
    free(self);
}

// ====================
// 7. Design Pattern: Singleton
// ====================

typedef struct {
    char** data;
    int count;
    int capacity;
} Singleton;

static Singleton* singleton_instance = NULL;

Singleton* Singleton_getInstance() {
    if (singleton_instance == NULL) {
        singleton_instance = (Singleton*)malloc(sizeof(Singleton));
        singleton_instance->count = 0;
        singleton_instance->capacity = 10;
        singleton_instance->data = (char**)malloc(sizeof(char*) * 10);
    }
    return singleton_instance;
}

void Singleton_addData(Singleton* self, const char* item) {
    if (self->count >= self->capacity) {
        self->capacity *= 2;
        self->data = (char**)realloc(self->data, sizeof(char*) * self->capacity);
    }
    self->data[self->count] = (char*)malloc(strlen(item) + 1);
    strcpy(self->data[self->count], item);
    self->count++;
}

void Singleton_printData(Singleton* self) {
    printf("[");
    for (int i = 0; i < self->count; i++) {
        printf("%s", self->data[i]);
        if (i < self->count - 1) printf(", ");
    }
    printf("]");
}

// ====================
// Main Function
// ====================

int main() {
    printf("=== Object-Oriented Programming in C ===\n\n");

    // 1. Basic "class"
    printf("1. Basic \"Class\":\n");
    Person* alice = Person_new("Alice", 30);
    Person_introduce(alice);
    Person_haveBirthday(alice);
    printf("   After birthday: age = %d\n", alice->age);
    Person_destroy(alice);

    // 2. "Inheritance" and Polymorphism
    printf("\n2. \"Inheritance\" and Composition:\n");
    Dog* buddy = Dog_new("Buddy", "Golden Retriever");
    Cat* whiskers = Cat_new("Whiskers", 1);
    Dog* max = Dog_new("Max", "German Shepherd");

    Dog_speak(buddy);
    Cat_speak(whiskers);
    Dog_speak(max);

    Dog_fetch(buddy);
    Cat_scratch(whiskers);

    Dog_destroy(buddy);
    Cat_destroy(whiskers);
    Dog_destroy(max);

    // 3. Polymorphism with VTables
    printf("\n3. Polymorphism with VTables (Shapes):\n");
    Circle* circle = Circle_new(5.0);
    Rectangle* rectangle = Rectangle_new(4.0, 6.0);
    Triangle* triangle = Triangle_new(3.0, 4.0, 5.0);

    // Use as Shape* (polymorphic)
    Shape* shapes[3] = {(Shape*)circle, (Shape*)rectangle, (Shape*)triangle};
    for (int i = 0; i < 3; i++) {
        Shape_describe(shapes[i]);
    }

    Circle_destroy(circle);
    Rectangle_destroy(rectangle);
    Triangle_destroy(triangle);

    // 4. Encapsulation
    printf("\n4. Encapsulation (Bank Account):\n");
    BankAccount* account = BankAccount_new("ACC001", 1000.0);
    printf("   Initial balance: $%.2f\n", BankAccount_getBalance(account));
    BankAccount_deposit(account, 500.0);
    printf("   After deposit: $%.2f\n", BankAccount_getBalance(account));
    BankAccount_withdraw(account, 200.0);
    printf("   After withdrawal: $%.2f\n", BankAccount_getBalance(account));
    BankAccount_printTransactions(account);
    BankAccount_destroy(account);

    // 5. "Static" methods
    printf("\n5. \"Static\" Methods:\n");
    Temperature* temp1 = Temperature_new(0.0);
    Temperature* temp2 = Temperature_fromFahrenheit(32.0);
    Temperature* temp3 = Temperature_fromKelvin(273.15);

    printf("   0°C = %.1f°F\n", Temperature_toFahrenheit(temp1));
    printf("   32°F = %.1f°C\n", temp2->celsius);
    printf("   273.15K = %.1f°C\n", temp3->celsius);
    printf("   Is 0°C freezing? %s\n", Temperature_isFreezing(0.0) ? "true" : "false");
    printf("   Conversions made: %d\n", temperature_conversion_count);

    Temperature_destroy(temp1);
    Temperature_destroy(temp2);
    Temperature_destroy(temp3);

    // 6. Composition
    printf("\n6. Composition:\n");
    Car* car = Car_new("Toyota", "Camry", 200);
    Car_start(car);
    Car_stop(car);
    Car_destroy(car);

    // 7. Singleton
    printf("\n7. Singleton Pattern:\n");
    Singleton* s1 = Singleton_getInstance();
    Singleton* s2 = Singleton_getInstance();
    printf("   s1 == s2? %s\n", s1 == s2 ? "true" : "false");
    Singleton_addData(s1, "item1");
    printf("   s1.data: ");
    Singleton_printData(s1);
    printf("\n   s2.data: ");
    Singleton_printData(s2);
    printf("\n");

    return 0;
}
