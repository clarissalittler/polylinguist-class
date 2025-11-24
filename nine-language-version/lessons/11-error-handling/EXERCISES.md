# Lesson 11: Error Handling & Debugging - Exercises

Practice handling errors gracefully and debugging code systematically.

## Level 1: Basic Error Handling

### Exercise 1: Safe Division
Implement safe division that handles all edge cases:
- Division by zero
- Type errors (non-numeric inputs)
- Overflow (very large numbers)
- Return appropriate error messages

**Concepts**: Basic try/catch, error messages

### Exercise 2: Safe List Access
Create a function that safely accesses list elements:
- Returns None/null for out-of-bounds
- Handles negative indices
- Works with empty lists

**Concepts**: Index validation, optional returns

### Exercise 3: Parse with Default
Parse a string to integer, return default on failure:
```
parseInt("42", 0) → 42
parseInt("invalid", 0) → 0
```

**Concepts**: Try-parse pattern, fallback values

### Exercise 4: File Reader
Read a file safely:
- Handle file not found
- Handle permission errors
- Handle encoding errors
- Close file properly (finally/ensure)

**Concepts**: Resource management, multiple error types

### Exercise 5: Input Validation
Validate user input for age:
- Must be integer
- Must be 0-150
- Return specific error messages

**Concepts**: Validation, custom error messages

## Level 2: Custom Exceptions

### Exercise 6: Custom Error Hierarchy
Create a hierarchy of custom exceptions:
```
ValidationError
├── EmailValidationError
├── PasswordValidationError
└── UsernameValidationError
```

Implement validators that throw appropriate errors.

**Concepts**: Exception hierarchies, custom exceptions

### Exercise 7: Detailed Error Context
Create errors that include context:
- What operation failed
- What input caused the failure
- What was expected
- How to fix it

**Concepts**: Error context, helpful messages

### Exercise 8: Error Codes
Implement an error system using codes:
```
enum ErrorCode {
    OK = 0,
    NOT_FOUND = 404,
    UNAUTHORIZED = 401,
    INVALID_INPUT = 400
}
```

**Concepts**: Error codes (C-style), enums

### Exercise 9: Result Type
Implement a Result<T, E> type (if language doesn't have it):
```
Result<T, E> = Ok(T) | Err(E)
```

Implement map, flatMap, unwrap, etc.

**Concepts**: Result types, functional error handling

### Exercise 10: Option Chaining
Implement safe property access chain:
```
user?.address?.city?.name
```

**Concepts**: Optional chaining, null safety

## Level 3: Error Recovery

### Exercise 11: Retry Logic
Implement retry with exponential backoff:
- Retry up to N times
- Increase delay between attempts
- Give up after max attempts

**Concepts**: Retry pattern, resilience

### Exercise 12: Batch Processing
Process a list of items, continue on errors:
- Process all items
- Collect successful results
- Collect all errors
- Return (successes, failures)

**Concepts**: Error recovery, partial success

### Exercise 13: Fallback Chain
Try multiple sources until one succeeds:
```
getData(primary) || getData(secondary) || getData(cache) || default
```

**Concepts**: Fallback pattern, redundancy

### Exercise 14: Circuit Breaker
Implement a circuit breaker:
- Track failure rate
- Open circuit after threshold
- Close after cooldown period

**Concepts**: Fault tolerance, resilience patterns

### Exercise 15: Graceful Degradation
Service that degrades gracefully:
- Full featured (all services up)
- Limited (some services down)
- Minimal (only core features)

**Concepts**: Graceful degradation, fallbacks

## Level 4: Debugging Exercises

### Exercise 16: Fix the Bugs
Given buggy code, find and fix:
- Off-by-one errors
- Null pointer exceptions
- Type mismatches
- Logic errors

**Concepts**: Debugging, common bugs

### Exercise 17: Add Logging
Add appropriate logging to code:
- DEBUG for detailed info
- INFO for important events
- WARN for recoverable issues
- ERROR for failures

**Concepts**: Logging levels, observability

### Exercise 18: Use a Debugger
Practice with a debugger:
- Set breakpoints
- Step through code
- Inspect variables
- Watch expressions

**Concepts**: Interactive debugging

### Exercise 19: Read Stack Traces
Practice reading stack traces:
- Find where error occurred
- Trace call stack
- Identify root cause

**Concepts**: Stack trace analysis

### Exercise 20: Binary Search Debug
Find bug using binary search:
- Identify working commit
- Identify broken commit
- Bisect to find culprit

**Concepts**: Systematic debugging

## Level 5: Advanced Patterns

### Exercise 21: Error Aggregation
Collect multiple errors before failing:
```
validateUser(user):
    errors = []
    if invalid_email: errors.append(...)
    if invalid_password: errors.append(...)
    if errors: raise AggregateError(errors)
```

**Concepts**: Multiple errors, aggregate validation

### Exercise 22: Context Manager
Create a custom context manager:
```
with Timer() as t:
    # code
print(f"Took {t.elapsed}s")
```

**Concepts**: Resource management, RAII

### Exercise 23: Error Middleware
Create middleware that wraps functions:
```
@handle_errors
def risky_operation():
    ...
```

**Concepts**: Decorators, middleware, AOP

### Exercise 24: Error Boundaries
Implement error boundaries (like React):
```
<ErrorBoundary fallback={<ErrorUI/>}>
    <Component/>
</ErrorBoundary>
```

**Concepts**: Isolation, fallback UI

### Exercise 25: Panic Recovery
Handle panics/crashes gracefully:
- Catch unhandled exceptions
- Log error details
- Restart gracefully
- Notify monitoring

**Concepts**: Crash handling, recovery

## Level 6: Real-World Scenarios

### Exercise 26: HTTP Client
Build HTTP client with error handling:
- Network timeouts
- Connection failures
- HTTP error codes (404, 500, etc.)
- Retry on transient errors

**Concepts**: Network errors, retries

### Exercise 27: Database Operations
Handle database errors:
- Connection failures
- Query errors
- Transactions (commit/rollback)
- Deadlocks

**Concepts**: Database error handling

### Exercise 28: Configuration Loading
Load configuration with validation:
- Parse config file
- Validate all required fields
- Type check values
- Provide helpful errors

**Concepts**: Configuration validation

### Exercise 29: User Input Processing
Process user input safely:
- Sanitize input
- Validate format
- Prevent injection attacks
- Give helpful feedback

**Concepts**: Input validation, security

### Exercise 30: Error Monitoring
Implement error tracking:
- Log errors with context
- Track error rates
- Alert on spikes
- Generate reports

**Concepts**: Monitoring, observability

## Bonus Challenges

### Challenge 1: Debugger Implementation
Build a simple debugger with:
- Breakpoints
- Step through
- Variable inspection

### Challenge 2: Error Recovery Framework
Build a framework for automatic error recovery.

### Challenge 3: Distributed Tracing
Implement error tracing across services.

### Challenge 4: Fault Injection Testing
Build a tool for injecting errors to test resilience.

### Challenge 5: Error Analysis Tool
Analyze logs to find patterns in errors.

## Debugging Scenarios

Practice these common scenarios:

### Scenario 1: Silent Failure
Function returns wrong result, no error thrown.
**Debug**: Add assertions, logging, test cases.

### Scenario 2: Intermittent Bug
Bug only happens sometimes.
**Debug**: Add logging, look for race conditions.

### Scenario 3: Performance Regression
Code suddenly slow.
**Debug**: Profile, compare with working version.

### Scenario 4: Memory Leak
Memory usage grows over time.
**Debug**: Monitor memory, check for unclosed resources.

### Scenario 5: Stack Overflow
Recursion crashes with stack overflow.
**Debug**: Check base case, trace recursion depth.

## Testing Error Handling

For each exercise, write tests for:

1. **Happy path**: No errors occur
2. **Expected errors**: Handle known error cases
3. **Unexpected errors**: Handle unknown errors
4. **Edge cases**: Boundary conditions
5. **Recovery**: Verify recovery works

Example test structure:
```python
def test_divide():
    # Happy path
    assert divide(10, 2) == 5

    # Expected error
    with pytest.raises(ZeroDivisionError):
        divide(10, 0)

    # Type error
    with pytest.raises(TypeError):
        divide("10", 2)
```

## Best Practices Checklist

- [ ] Errors have clear, actionable messages
- [ ] Errors include context (what, why, how to fix)
- [ ] Resources are cleaned up (finally/ensure)
- [ ] Errors are logged appropriately
- [ ] Error handling doesn't hide bugs
- [ ] Recovery is tested
- [ ] Stack traces are preserved
- [ ] Custom errors extend base error class
- [ ] Errors are documented
- [ ] Monitoring is in place

## Reflection Questions

1. What errors should be caught vs propagated?
2. When to use exceptions vs error codes/Result?
3. How much context should errors include?
4. What's the difference between expected and unexpected errors?
5. How to balance error handling and code clarity?
6. When to fail fast vs recover gracefully?
7. How to test error handling code?

---

**Remember**: Good error handling makes your code robust and maintainable. Practice with real-world scenarios to build intuition!
