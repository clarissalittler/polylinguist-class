/* Lesson 11: Error Handling in C - No exceptions, use error codes */
#include <stdio.h>
#include <errno.h>
#include <string.h>

typedef enum { SUCCESS = 0, ERR_DIV_ZERO = -1, ERR_INVALID = -2 } ErrorCode;

ErrorCode divide(int x, int y, double *result) {
    if (y == 0) return ERR_DIV_ZERO;
    *result = (double)x / y;
    return SUCCESS;
}

int main() {
    printf("=== Error Handling in C ===\n\n1. Return codes:\n");
    double result;
    if (divide(10, 2, &result) == SUCCESS)
        printf("   divide(10, 2) = %.1f\n", result);
    if (divide(10, 0, &result) == ERR_DIV_ZERO)
        printf("   Error: Division by zero\n");
    
    printf("\n2. errno for system calls:\n");
    FILE *f = fopen("/nonexistent.txt", "r");
    if (!f) {
        printf("   Error: %s\n", strerror(errno));
    }
    
    printf("\n=== C Error Handling ===\n- Return codes (0=success, negative=error)\n- errno for system calls\n- No exceptions\n");
    return 0;
}
