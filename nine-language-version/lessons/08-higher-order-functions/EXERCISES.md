# Lesson 8: Higher-Order Functions - Exercises

## Instructions

Complete these exercises to practice higher-order functions, map/filter/reduce, closures, and functional programming concepts across different languages.

---

## Exercise 1: Basic Map (Warmup)

**Difficulty:** Very Easy

Given a list of numbers `[1, 2, 3, 4, 5]`:

1. Use `map` to double each number
2. Use `map` to square each number
3. Use `map` to convert each to a string

**Implement in:** Python, JavaScript, or Haskell

**Bonus:** Do the same thing with a list comprehension (Python) or loop (JavaScript).

---

## Exercise 2: Filter Practice

**Difficulty:** Easy

Given a list of numbers `[1, -5, 3, -2, 8, -9, 4, 6, -1, 10]`:

1. Filter to get only positive numbers
2. Filter to get only even numbers
3. Filter to get numbers greater than 5
4. Filter to get positive AND even numbers

**Implement in:** Python, JavaScript, or Ruby

---

## Exercise 3: Reduce/Fold

**Difficulty:** Easy to Medium

Given a list of numbers `[1, 2, 3, 4, 5]`:

1. Use reduce to find the sum
2. Use reduce to find the product
3. Use reduce to find the maximum
4. Use reduce to build a string like "1, 2, 3, 4, 5"

**Implement in:** Python, JavaScript, or Haskell

**Challenge:** Implement your own `reduce` function from scratch!

---

## Exercise 4: Function Pipeline

**Difficulty:** Medium

Create a data processing pipeline that:

1. Starts with `[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]`
2. Filters out odd numbers
3. Squares each remaining number
4. Sums the results

**Do this in two ways:**
- Separate steps (filter, then map, then reduce)
- Single chain (method chaining or function composition)

**Implement in:** Your choice of language

---

## Exercise 5: Custom Map/Filter/Reduce

**Difficulty:** Medium

Implement your own versions of:
- `myMap(func, list)` - applies func to each element
- `myFilter(predicate, list)` - keeps elements where predicate is true
- `myReduce(func, initial, list)` - combines elements with binary function

**Don't use built-in map/filter/reduce!** Use loops or recursion.

**Implement in:** Python, JavaScript, Haskell, or Rust

**Test them:**
```python
myMap(lambda x: x * 2, [1, 2, 3])  # [2, 4, 6]
myFilter(lambda x: x > 2, [1, 2, 3, 4])  # [3, 4]
myReduce(lambda a, b: a + b, 0, [1, 2, 3])  # 6
```

---

## Exercise 6: Closure Counter

**Difficulty:** Medium

Create a `makeCounter()` function that returns a counter:
- Each call to the counter increments and returns the count
- Each counter should have its own independent state

**Implement in:** Python, JavaScript, Ruby, or Rust

**Test:**
```javascript
const counter1 = makeCounter();
const counter2 = makeCounter();
counter1();  // 1
counter1();  // 2
counter2();  // 1 (independent!)
counter1();  // 3
```

---

## Exercise 7: Function Composition

**Difficulty:** Medium

Write a `compose` function that takes two functions and returns their composition.

`compose(f, g)` should return a function that does: `x => f(g(x))`

**Implement in:** Python, JavaScript, or Haskell

**Test:**
```python
add_one = lambda x: x + 1
double = lambda x: x * 2
f = compose(double, add_one)
f(5)  # (5 + 1) * 2 = 12
```

**Bonus:** Make it work with any number of functions: `compose(f, g, h, ...)`

---

## Exercise 8: Partial Application

**Difficulty:** Medium

Create a function `power(base, exponent)` that raises base to exponent.

Then create specialized versions:
- `square = partial(power, exponent=2)`
- `cube = partial(power, exponent=3)`

**Implement in:** Python (functools.partial), JavaScript (bind), or Haskell (currying)

**Bonus:** Implement your own `partial` function!

---

## Exercise 9: Array Processing Challenge

**Difficulty:** Medium to Hard

Given this array of user objects:
```javascript
users = [
  { name: 'Alice', age: 25, active: true, score: 85 },
  { name: 'Bob', age: 17, active: false, score: 92 },
  { name: 'Charlie', age: 30, active: true, score: 78 },
  { name: 'Diana', age: 22, active: true, score: 95 },
  { name: 'Eve', age: 19, active: true, score: 88 }
]
```

Write a pipeline that:
1. Filters to only active users
2. Filters to only users 18 or older
3. Maps to extract just names and scores: `{name, score}`
4. Sorts by score (descending)
5. Takes the top 3

**Implement in:** JavaScript, Python, or Ruby

**Expected output:**
```
[
  { name: 'Diana', score: 95 },
  { name: 'Eve', score: 88 },
  { name: 'Alice', score: 85 }
]
```

---

## Exercise 10: Closure for Private State

**Difficulty:** Medium to Hard

Create a `createBankAccount(initialBalance)` function that returns an object with:
- `deposit(amount)` - adds to balance
- `withdraw(amount)` - subtracts from balance (if sufficient funds)
- `getBalance()` - returns current balance

The balance should be **private** - not directly accessible!

**Implement in:** JavaScript, Python, or Ruby

**Test:**
```javascript
const account = createBankAccount(1000);
account.deposit(500);      // balance is now 1500
account.withdraw(200);     // balance is now 1300
account.getBalance();      // returns 1300
account.balance;           // should be undefined!
```

---

## Exercise 11: Higher-Order Function - Retry

**Difficulty:** Hard

Write a `retry(fn, maxAttempts)` function that:
- Calls `fn()`
- If it throws an error, retries up to `maxAttempts` times
- Returns the result if successful
- Throws the error if all attempts fail

**Implement in:** Python or JavaScript

**Test:**
```python
attempts = 0
def unreliable_function():
    global attempts
    attempts += 1
    if attempts < 3:
        raise Exception("Failed!")
    return "Success!"

result = retry(unreliable_function, 5)  # Succeeds on 3rd try
```

---

## Exercise 12: Memoization

**Difficulty:** Hard

Write a `memoize(fn)` decorator/HOF that caches function results.

**Implement in:** Python (decorator) or JavaScript

**Test with Fibonacci:**
```python
@memoize
def fib(n):
    if n <= 1:
        return n
    return fib(n-1) + fib(n-2)

fib(100)  # Should be fast due to memoization!
```

---

## Exercise 13: FlatMap

**Difficulty:** Medium

Implement `flatMap(func, list)` which:
1. Maps `func` over each element
2. Flattens the result by one level

**Implement in:** JavaScript, Python, or Haskell

**Test:**
```javascript
flatMap(x => [x, x * 2], [1, 2, 3])
// [1, 2, 2, 4, 3, 6]

words = ["Hello world", "How are you"]
flatMap(s => s.split(' '), words)
// ["Hello", "world", "How", "are", "you"]
```

---

## Exercise 14: Debounce

**Difficulty:** Hard

Write a `debounce(fn, delay)` function that:
- Returns a new function
- When called multiple times rapidly, only executes `fn` after `delay` ms of silence

This is useful for search boxes, resize handlers, etc.

**Implement in:** JavaScript

**Test:**
```javascript
const log = debounce(console.log, 1000);
log("a");  // Called
log("b");  // Cancels "a", schedules "b"
log("c");  // Cancels "b", schedules "c"
// After 1000ms of silence, only logs "c"
```

---

## Exercise 15: Pipe Function

**Difficulty:** Medium to Hard

Create a `pipe(...functions)` that composes functions left-to-right:

`pipe(f, g, h)(x)` should compute `h(g(f(x)))`

**Implement in:** JavaScript, Python, or Haskell

**Test:**
```javascript
const process = pipe(
  x => x + 1,
  x => x * 2,
  x => x ** 2
);

process(5);  // ((5 + 1) * 2) ** 2 = 144
```

---

## Exercise 16: Every and Some

**Difficulty:** Easy to Medium

Implement your own versions of:
- `every(predicate, list)` - returns true if ALL elements satisfy predicate
- `some(predicate, list)` - returns true if ANY element satisfies predicate

**Implement in:** Python, JavaScript, or Haskell

**Test:**
```python
every(lambda x: x > 0, [1, 2, 3])     # True
every(lambda x: x > 0, [1, -2, 3])    # False
some(lambda x: x < 0, [1, -2, 3])     # True
some(lambda x: x < 0, [1, 2, 3])      # False
```

---

## Exercise 17: Zip and ZipWith

**Difficulty:** Medium

Implement:
- `zip(list1, list2)` - combines two lists into pairs
- `zipWith(func, list1, list2)` - combines using a function

**Implement in:** Python, JavaScript, or Haskell

**Test:**
```python
zip([1, 2, 3], ['a', 'b', 'c'])
# [(1, 'a'), (2, 'b'), (3, 'c')]

zipWith(lambda x, y: x + y, [1, 2, 3], [4, 5, 6])
# [5, 7, 9]
```

---

## Exercise 18: GroupBy

**Difficulty:** Hard

Write a `groupBy(keyFunc, list)` that groups elements by a key function.

**Implement in:** Python, JavaScript, or Ruby

**Test:**
```javascript
students = [
  { name: 'Alice', grade: 'A' },
  { name: 'Bob', grade: 'B' },
  { name: 'Charlie', grade: 'A' },
  { name: 'Diana', grade: 'B' }
];

groupBy(s => s.grade, students)
// {
//   'A': [{name: 'Alice', grade: 'A'}, {name: 'Charlie', grade: 'A'}],
//   'B': [{name: 'Bob', grade: 'B'}, {name: 'Diana', grade: 'B'}]
// }
```

---

## Exercise 19: Currying

**Difficulty:** Medium to Hard

Write a `curry(func)` function that converts a multi-argument function into a curried version.

**Implement in:** JavaScript or Python

**Test:**
```javascript
function add(a, b, c) {
  return a + b + c;
}

const curriedAdd = curry(add);
curriedAdd(1)(2)(3);  // 6
curriedAdd(1, 2)(3);  // 6 (should handle partial application)
curriedAdd(1)(2, 3);  // 6
```

---

## Exercise 20: Functional Data Transformation

**Difficulty:** Hard (Comprehensive)

You have sales data:
```javascript
sales = [
  { product: 'Laptop', price: 1000, quantity: 2, category: 'Electronics' },
  { product: 'Mouse', price: 25, quantity: 10, category: 'Electronics' },
  { product: 'Desk', price: 300, quantity: 1, category: 'Furniture' },
  { product: 'Chair', price: 150, quantity: 4, category: 'Furniture' },
  { product: 'Monitor', price: 400, quantity: 3, category: 'Electronics' }
]
```

Using only HOFs (map/filter/reduce), calculate:
1. Total revenue (price * quantity for all items)
2. Total revenue per category
3. Most expensive single item (by total: price * quantity)
4. Average price across all products

**Implement in:** JavaScript, Python, or Ruby

---

## Challenge Exercise: Function Pipeline Builder

**Difficulty:** Very Hard

Create a `Pipeline` class/function that allows method chaining:

```javascript
const result = Pipeline([1, 2, 3, 4, 5])
  .filter(x => x > 2)
  .map(x => x * 2)
  .reduce((a, b) => a + b, 0)
  .value();  // 24
```

The pipeline should be **lazy** - not executing until `.value()` is called!

**Implement in:** JavaScript or Python

---

## Reflection Questions

After completing these exercises, consider:

1. **When to use HOFs:** Which problems were easier with HOFs vs loops?

2. **Performance:** Did you notice any performance differences? When might loops be faster?

3. **Readability:** Which style (imperative loops vs functional HOFs) felt more readable?

4. **Language differences:** How did HOF support differ between languages you tried?

5. **Practical use:** Where might you use map/filter/reduce in real projects?

6. **Closures:** What problems do closures solve that would be hard otherwise?
