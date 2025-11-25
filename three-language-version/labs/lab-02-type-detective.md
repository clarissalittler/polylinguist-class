# Lab 2: Type Detective

**Quarter 1, Week 2**
**Duration:** 75-90 minutes
**Format:** Pair programming

## Overview

Types are fundamental to how programming languages work. In this lab, you'll investigate how Python, C++, and Haskell handle types differently, and learn to predict what types expressions have.

## Objectives

By the end of this lab, you will:
- [ ] Understand the difference between static and dynamic typing
- [ ] Be able to determine types in all three languages
- [ ] Know how to trigger and read type errors
- [ ] Appreciate why types matter

## Setup

- Partner up with someone at your table
- Open terminals/editors for all three languages
- Have the lesson 02 materials accessible

---

## Part 1: Type Discovery (20 minutes)

### Activity 1.1: Python Type Exploration

In Python REPL, investigate types:

```python
>>> type(42)
>>> type(3.14)
>>> type("hello")
>>> type(True)
>>> type([1, 2, 3])
>>> type({"name": "Alice"})
>>> type(None)
```

**Record your findings:**

| Expression | Type |
|------------|------|
| 42 | |
| 3.14 | |
| "hello" | |
| True | |
| [1, 2, 3] | |
| {"name": "Alice"} | |
| None | |

**Discussion question:** When did Python decide these types?

### Activity 1.2: Haskell Type Exploration

In GHCi, use `:type` (or `:t`) to find types:

```haskell
ghci> :type 42
ghci> :type 3.14
ghci> :type "hello"
ghci> :type True
ghci> :type [1, 2, 3]
ghci> :type ('a', 1)
```

**Record your findings:**

| Expression | Type |
|------------|------|
| 42 | |
| 3.14 | |
| "hello" | |
| True | |
| [1, 2, 3] | |
| ('a', 1) | |

**Discussion question:** Why does `42` have a weird type in Haskell?

### Activity 1.3: C++ Types

Create a file `types.cpp`:

```cpp
#include <iostream>
#include <typeinfo>

int main() {
    std::cout << typeid(42).name() << std::endl;
    std::cout << typeid(3.14).name() << std::endl;
    std::cout << typeid("hello").name() << std::endl;
    std::cout << typeid(true).name() << std::endl;
    return 0;
}
```

Compile and run. The output will be cryptic (like `i`, `d`, `A6_c`, `b`).

**Decode:**
- `i` = int
- `d` = double
- `b` = bool
- `A6_c` = array of 6 chars (the string "hello" + null terminator)

### ✅ Checkpoint 1

Verify with your partner:
- [ ] You can find types in Python with `type()`
- [ ] You can find types in Haskell with `:type`
- [ ] You understand C++ has types even if `typeid` is awkward

---

## Part 2: Type Inference Challenge (20 minutes)

### Activity 2.1: Predict the Types

WITHOUT running the code, predict the type of each expression. Then verify.

**Python predictions:**

| Expression | Your Prediction | Actual Type |
|------------|-----------------|-------------|
| `5 + 3` | | |
| `5 + 3.0` | | |
| `"5" + "3"` | | |
| `[1, 2] + [3, 4]` | | |
| `len("hello")` | | |
| `5 > 3` | | |
| `5 / 2` | | |
| `5 // 2` | | |

**Haskell predictions:**

| Expression | Your Prediction | Actual Type |
|------------|-----------------|-------------|
| `5 + 3` | | |
| `length "hello"` | | |
| `head [1,2,3]` | | |
| `null []` | | |
| `[1,2] ++ [3,4]` | | |
| `(+)` | | |

**Discussion:** What surprised you?

### Activity 2.2: Type Inference in Haskell

Haskell can infer types. Try this in GHCi:

```haskell
ghci> let x = 5
ghci> :type x
ghci> let y = "hello"
ghci> :type y
ghci> let add a b = a + b
ghci> :type add
```

Now try with type annotations:

```haskell
ghci> let x :: Int; x = 5
ghci> :type x
ghci> let add :: Int -> Int -> Int; add a b = a + b
ghci> :type add
```

**Question:** What's the difference between `Num a => a` and `Int`?

### ✅ Checkpoint 2

Verify with your partner:
- [ ] You correctly predicted at least 5/8 Python types
- [ ] You understand Haskell's polymorphic types (like `Num a => a`)

---

## Part 3: Type Errors (20 minutes)

### Activity 3.1: Break Python

Try these in Python and observe the errors:

```python
>>> "5" + 3
>>> len(42)
>>> [1, 2, 3][10]
>>> int("hello")
```

**Record the error messages:**

| Code | Error Type | Message Summary |
|------|------------|-----------------|
| `"5" + 3` | | |
| `len(42)` | | |
| `[1,2,3][10]` | | |
| `int("hello")` | | |

### Activity 3.2: Break C++

Try to compile these (create separate files or use one file with errors):

**File 1:** `type_error1.cpp`
```cpp
#include <iostream>
int main() {
    int x = "hello";  // String to int
    return 0;
}
```

**File 2:** `type_error2.cpp`
```cpp
#include <iostream>
int main() {
    std::cout << "5" + 3 << std::endl;  // String + int
    return 0;
}
```

**Record:**
- Do these compile?
- What error messages do you see?

### Activity 3.3: Break Haskell

Try these in GHCi:

```haskell
ghci> "5" + 3
ghci> length 42
ghci> head []
ghci> True + False
```

**Record the error messages:**

| Code | Error Message Summary |
|------|----------------------|
| `"5" + 3` | |
| `length 42` | |
| `head []` | |
| `True + False` | |

### Activity 3.4: When are errors caught?

Fill in this table:

| Error | Python | C++ | Haskell |
|-------|--------|-----|---------|
| Wrong type addition | Runtime / Compile | Runtime / Compile | Runtime / Compile |
| Function on wrong type | Runtime / Compile | Runtime / Compile | Runtime / Compile |
| Empty list head | Runtime / Compile | Runtime / Compile | Runtime / Compile |

### ✅ Checkpoint 3

Verify with your partner:
- [ ] You can explain when Python catches type errors
- [ ] You can explain when C++ catches type errors
- [ ] You can explain when Haskell catches type errors

---

## Part 4: Type Conversion (15 minutes)

### Activity 4.1: Explicit Conversion

**Python:**
```python
>>> int("42")
>>> str(42)
>>> float("3.14")
>>> list("hello")
>>> bool(0)
>>> bool(1)
>>> bool("")
>>> bool("hello")
```

**C++:**
```cpp
#include <iostream>
#include <string>

int main() {
    int a = static_cast<int>(3.14);  // double to int
    double b = static_cast<double>(5);  // int to double
    std::string s = std::to_string(42);  // int to string
    int c = std::stoi("42");  // string to int

    std::cout << a << " " << b << " " << s << " " << c << std::endl;
    return 0;
}
```

**Haskell:**
```haskell
ghci> show 42  -- to String
ghci> read "42" :: Int  -- from String
ghci> fromIntegral (5 :: Int) :: Double
ghci> round 3.7
```

### Activity 4.2: Implicit Conversion

Which languages allow implicit conversion?

```python
# Python: Does this work?
x = 5 + 3.0
print(x, type(x))
```

```cpp
// C++: Does this work?
int a = 5;
double b = 3.0;
auto c = a + b;  // What's the type of c?
```

```haskell
-- Haskell: Does this work?
x = 5 + 3.0  -- Both literals
y = (5 :: Int) + 3.0  -- Explicit Int
```

**Discussion:** Which approach is safer? Which is more convenient?

---

## Part 5: Synthesis (10 minutes)

### Discussion Questions

With your partner, discuss and write brief answers:

1. **Static vs. Dynamic:** What does it mean for a language to have "static" types?

2. **Type Safety:** Which language feels "safest" in terms of types? Why?

3. **Trade-offs:** What do you give up for static typing? What do you gain?

4. **Real-world impact:** When might type errors cause serious problems in the real world?

### Create a Summary Table

| Feature | Python | C++ | Haskell |
|---------|--------|-----|---------|
| Typing discipline | Dynamic | Static | Static |
| Type inference | None / Some / Full | None / Some / Full | None / Some / Full |
| When errors caught | Runtime | Compile | Compile |
| Implicit conversion | Yes / No / Limited | Yes / No / Limited | Yes / No / Limited |

---

## Extensions (If You Finish Early)

### Extension 1: Union Types

Python 3.10+ has union types. Explore:

```python
def process(x: int | str) -> str:
    if isinstance(x, int):
        return f"Number: {x}"
    else:
        return f"String: {x}"
```

### Extension 2: Haskell Maybe

```haskell
ghci> :type Just 5
ghci> :type Nothing
ghci> :type head  -- Can fail!
ghci> import Data.Maybe
ghci> :type listToMaybe  -- Safe version
ghci> listToMaybe [1,2,3]
ghci> listToMaybe []
```

### Extension 3: C++ auto

```cpp
auto x = 5;  // int
auto y = 5.0;  // double
auto z = "hello";  // const char*
auto w = std::string("hello");  // std::string
```

When should you use `auto` vs. explicit types?

---

## Wrap-Up

**Key takeaways:**

1. **Python** checks types at runtime - flexible but errors happen late
2. **C++** checks types at compile time - catches errors early
3. **Haskell** has the strictest type system - catches the most errors at compile time
4. There are trade-offs between safety and flexibility

**Next lab:** We'll use control flow to make decisions in our programs!
