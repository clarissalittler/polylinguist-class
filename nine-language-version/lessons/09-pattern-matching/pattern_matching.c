/*
 * Lesson 9: Pattern Matching in C
 *
 * C doesn't have native pattern matching, but we can simulate it using:
 * - Switch statements for basic matching
 * - If/else chains for complex conditions
 * - Tagged unions for algebraic data types
 * - Function pointers for dispatch
 * - Structs for destructuring
 *
 * This demonstrates pattern-matching-like techniques in C.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdbool.h>

// ====================
// 1. Basic Switch Pattern
// ====================

const char* describe_number(int n) {
    switch (n) {
        case 0: return "zero";
        case 1: return "one";
        case 2: return "two";
        default: {
            static char buffer[50];
            snprintf(buffer, sizeof(buffer), "many: %d", n);
            return buffer;
        }
    }
}

// ====================
// 2. Tuple-like Patterns (Structs)
// ====================

typedef struct {
    int x;
    int y;
} Point;

const char* describe_point(Point p) {
    static char buffer[100];

    if (p.x == 0 && p.y == 0) {
        return "origin";
    } else if (p.x == 0) {
        snprintf(buffer, sizeof(buffer), "on y-axis at y=%d", p.y);
    } else if (p.y == 0) {
        snprintf(buffer, sizeof(buffer), "on x-axis at x=%d", p.x);
    } else {
        snprintf(buffer, sizeof(buffer), "point at (%d, %d)", p.x, p.y);
    }

    return buffer;
}

// ====================
// 3. Tagged Unions (Algebraic Data Types)
// ====================

typedef enum {
    SHAPE_CIRCLE,
    SHAPE_RECTANGLE,
    SHAPE_TRIANGLE
} ShapeType;

typedef struct {
    ShapeType type;
    union {
        struct { double radius; } circle;
        struct { double width; double height; } rectangle;
        struct { double a; double b; double c; } triangle;
    } data;
} Shape;

Shape make_circle(double radius) {
    Shape s = { .type = SHAPE_CIRCLE, .data.circle = { radius } };
    return s;
}

Shape make_rectangle(double width, double height) {
    Shape s = { .type = SHAPE_RECTANGLE, .data.rectangle = { width, height } };
    return s;
}

Shape make_triangle(double a, double b, double c) {
    Shape s = { .type = SHAPE_TRIANGLE, .data.triangle = { a, b, c } };
    return s;
}

double area(const Shape* shape) {
    switch (shape->type) {
        case SHAPE_CIRCLE: {
            double r = shape->data.circle.radius;
            return M_PI * r * r;
        }
        case SHAPE_RECTANGLE: {
            return shape->data.rectangle.width * shape->data.rectangle.height;
        }
        case SHAPE_TRIANGLE: {
            double a = shape->data.triangle.a;
            double b = shape->data.triangle.b;
            double c = shape->data.triangle.c;
            double s = (a + b + c) / 2.0;
            return sqrt(s * (s - a) * (s - b) * (s - c));
        }
        default:
            fprintf(stderr, "Unknown shape type\n");
            exit(1);
    }
}

const char* describe_shape(const Shape* shape) {
    static char buffer[100];

    switch (shape->type) {
        case SHAPE_CIRCLE:
            snprintf(buffer, sizeof(buffer), "Circle with radius %.1f",
                     shape->data.circle.radius);
            break;
        case SHAPE_RECTANGLE:
            snprintf(buffer, sizeof(buffer), "Rectangle %.1fx%.1f",
                     shape->data.rectangle.width, shape->data.rectangle.height);
            break;
        case SHAPE_TRIANGLE:
            snprintf(buffer, sizeof(buffer), "Triangle with sides %.1f, %.1f, %.1f",
                     shape->data.triangle.a, shape->data.triangle.b, shape->data.triangle.c);
            break;
        default:
            snprintf(buffer, sizeof(buffer), "Unknown shape");
    }

    return buffer;
}

// ====================
// 4. Guards (Conditional Patterns)
// ====================

const char* classify(int n) {
    if (n < 0) return "negative";
    if (n == 0) return "zero";
    if (n < 10) return "small positive";
    if (n < 100) return "medium positive";
    return "large positive";
}

// ====================
// 5. Multiple Values (OR patterns)
// ====================

bool is_weekend(const char* day) {
    char lower[20];
    int i = 0;
    while (day[i] && i < 19) {
        lower[i] = tolower(day[i]);
        i++;
    }
    lower[i] = '\0';

    return strcmp(lower, "saturday") == 0 || strcmp(lower, "sunday") == 0;
}

// ====================
// 6. Range Patterns
// ====================

char grade_letter(int score) {
    if (score >= 90 && score <= 100) return 'A';
    if (score >= 80) return 'B';
    if (score >= 70) return 'C';
    if (score >= 60) return 'D';
    return 'F';
}

// ====================
// 7. Expression Evaluator
// ====================

typedef enum {
    EXPR_NUM,
    EXPR_ADD,
    EXPR_MUL,
    EXPR_NEG
} ExprType;

typedef struct Expr Expr;

struct Expr {
    ExprType type;
    union {
        int num;
        struct { Expr* left; Expr* right; } binary;
        struct { Expr* expr; } unary;
    } data;
};

Expr* make_num(int value) {
    Expr* e = malloc(sizeof(Expr));
    e->type = EXPR_NUM;
    e->data.num = value;
    return e;
}

Expr* make_add(Expr* left, Expr* right) {
    Expr* e = malloc(sizeof(Expr));
    e->type = EXPR_ADD;
    e->data.binary.left = left;
    e->data.binary.right = right;
    return e;
}

Expr* make_mul(Expr* left, Expr* right) {
    Expr* e = malloc(sizeof(Expr));
    e->type = EXPR_MUL;
    e->data.binary.left = left;
    e->data.binary.right = right;
    return e;
}

Expr* make_neg(Expr* expr) {
    Expr* e = malloc(sizeof(Expr));
    e->type = EXPR_NEG;
    e->data.unary.expr = expr;
    return e;
}

int eval_expr(const Expr* expr) {
    switch (expr->type) {
        case EXPR_NUM:
            return expr->data.num;
        case EXPR_ADD:
            return eval_expr(expr->data.binary.left) + eval_expr(expr->data.binary.right);
        case EXPR_MUL:
            return eval_expr(expr->data.binary.left) * eval_expr(expr->data.binary.right);
        case EXPR_NEG:
            return -eval_expr(expr->data.unary.expr);
        default:
            fprintf(stderr, "Unknown expression type\n");
            exit(1);
    }
}

void free_expr(Expr* expr) {
    if (!expr) return;

    switch (expr->type) {
        case EXPR_ADD:
        case EXPR_MUL:
            free_expr(expr->data.binary.left);
            free_expr(expr->data.binary.right);
            break;
        case EXPR_NEG:
            free_expr(expr->data.unary.expr);
            break;
        case EXPR_NUM:
            break;
    }

    free(expr);
}

// ====================
// 8. Option Pattern
// ====================

typedef struct {
    bool is_some;
    int value;
} Option;

Option some(int value) {
    return (Option){ .is_some = true, .value = value };
}

Option none() {
    return (Option){ .is_some = false, .value = 0 };
}

const char* describe_option(Option opt) {
    static char buffer[50];

    if (opt.is_some) {
        snprintf(buffer, sizeof(buffer), "just %d", opt.value);
    } else {
        snprintf(buffer, sizeof(buffer), "nothing");
    }

    return buffer;
}

// ====================
// 9. Result Pattern
// ====================

typedef struct {
    bool is_ok;
    union {
        int value;
        const char* error;
    } data;
} Result;

Result ok(int value) {
    Result r = { .is_ok = true };
    r.data.value = value;
    return r;
}

Result err(const char* error) {
    Result r = { .is_ok = false };
    r.data.error = error;
    return r;
}

const char* describe_result(Result res) {
    static char buffer[100];

    if (res.is_ok) {
        snprintf(buffer, sizeof(buffer), "success: %d", res.data.value);
    } else {
        snprintf(buffer, sizeof(buffer), "error: %s", res.data.error);
    }

    return buffer;
}

// ====================
// 10. State Machine
// ====================

typedef enum {
    LIGHT_RED,
    LIGHT_GREEN,
    LIGHT_YELLOW
} TrafficLight;

typedef enum {
    ACTION_TIMER,
    ACTION_EMERGENCY
} Action;

TrafficLight traffic_light_next(TrafficLight current, Action action) {
    if (action == ACTION_EMERGENCY) {
        return LIGHT_RED;
    }

    switch (current) {
        case LIGHT_RED:
            return (action == ACTION_TIMER) ? LIGHT_GREEN : current;
        case LIGHT_GREEN:
            return (action == ACTION_TIMER) ? LIGHT_YELLOW : current;
        case LIGHT_YELLOW:
            return (action == ACTION_TIMER) ? LIGHT_RED : current;
        default:
            return current;
    }
}

const char* light_to_string(TrafficLight light) {
    switch (light) {
        case LIGHT_RED: return "red";
        case LIGHT_GREEN: return "green";
        case LIGHT_YELLOW: return "yellow";
        default: return "unknown";
    }
}

const char* action_to_string(Action action) {
    switch (action) {
        case ACTION_TIMER: return "timer";
        case ACTION_EMERGENCY: return "emergency";
        default: return "unknown";
    }
}

// ====================
// 11. User Pattern
// ====================

typedef struct {
    char name[50];
    int age;
    bool active;
} User;

const char* describe_user(const User* user) {
    static char buffer[100];

    if (user->active && user->age >= 18) {
        snprintf(buffer, sizeof(buffer), "Active adult: %s", user->name);
    } else if (user->active) {
        snprintf(buffer, sizeof(buffer), "Active minor: %s", user->name);
    } else {
        snprintf(buffer, sizeof(buffer), "Inactive: %s", user->name);
    }

    return buffer;
}

// ====================
// Main Demonstration
// ====================

int main() {
    printf("=== Pattern Matching in C ===\n\n");

    // 1. Basic switch
    printf("1. Basic Switch Patterns:\n");
    int nums[] = {0, 1, 2, 5};
    for (int i = 0; i < 4; i++) {
        printf("   %d -> %s\n", nums[i], describe_number(nums[i]));
    }

    // 2. Tuple-like patterns
    printf("\n2. Tuple-like Patterns (Structs):\n");
    Point points[] = {{0, 0}, {0, 5}, {3, 0}, {2, 3}};
    for (int i = 0; i < 4; i++) {
        printf("   (%d, %d) -> %s\n", points[i].x, points[i].y, describe_point(points[i]));
    }

    // 3. Tagged unions
    printf("\n3. Tagged Unions (Algebraic Data Types):\n");
    Shape shapes[] = {
        make_circle(5.0),
        make_rectangle(4.0, 6.0),
        make_triangle(3.0, 4.0, 5.0)
    };
    for (int i = 0; i < 3; i++) {
        printf("   %s\n", describe_shape(&shapes[i]));
        printf("   area = %.2f\n", area(&shapes[i]));
    }

    // 4. Guards
    printf("\n4. Guards (Conditional Patterns):\n");
    int test_nums[] = {-5, 0, 3, 50, 500};
    for (int i = 0; i < 5; i++) {
        printf("   %d -> %s\n", test_nums[i], classify(test_nums[i]));
    }

    // 5. OR patterns
    printf("\n5. Multiple Values (OR patterns):\n");
    const char* days[] = {"Monday", "Saturday", "Sunday", "Wednesday"};
    for (int i = 0; i < 4; i++) {
        printf("   %s is weekend? %s\n", days[i], is_weekend(days[i]) ? "true" : "false");
    }

    // 6. Range patterns
    printf("\n6. Range Patterns:\n");
    int scores[] = {95, 85, 75, 65, 55};
    for (int i = 0; i < 5; i++) {
        printf("   %d -> %c\n", scores[i], grade_letter(scores[i]));
    }

    // 7. Option patterns
    printf("\n7. Option Patterns:\n");
    printf("   %s\n", describe_option(some(42)));
    printf("   %s\n", describe_option(none()));

    // 8. Result patterns
    printf("\n8. Result Patterns:\n");
    printf("   %s\n", describe_result(ok(100)));
    printf("   %s\n", describe_result(err("failed")));

    // 9. Expression evaluator
    printf("\n9. Expression Evaluator:\n");
    // (2 + 3) * 4 = 20
    Expr* expr = make_mul(make_add(make_num(2), make_num(3)), make_num(4));
    printf("   (2 + 3) * 4 = %d\n", eval_expr(expr));
    free_expr(expr);

    // -(5 + 3) = -8
    Expr* expr2 = make_neg(make_add(make_num(5), make_num(3)));
    printf("   -(5 + 3) = %d\n", eval_expr(expr2));
    free_expr(expr2);

    // 10. User patterns
    printf("\n10. User Patterns:\n");
    User users[] = {
        {"Alice", 25, true},
        {"Bob", 16, true},
        {"Charlie", 30, false}
    };
    for (int i = 0; i < 3; i++) {
        printf("   %s\n", describe_user(&users[i]));
    }

    // 11. State machine
    printf("\n11. State Machine (Traffic Light):\n");
    TrafficLight state = LIGHT_RED;
    Action actions[] = {ACTION_TIMER, ACTION_TIMER, ACTION_TIMER, ACTION_EMERGENCY};
    for (int i = 0; i < 4; i++) {
        state = traffic_light_next(state, actions[i]);
        printf("   After '%s': %s\n", action_to_string(actions[i]), light_to_string(state));
    }

    printf("\n=== C Pattern Matching Notes ===\n");
    printf("- No native pattern matching\n");
    printf("- Use switch statements for basic patterns\n");
    printf("- Tagged unions simulate algebraic data types\n");
    printf("- If/else chains for guards and complex conditions\n");
    printf("- Structs for tuple-like patterns\n");
    printf("- Manual memory management required\n");

    return 0;
}
