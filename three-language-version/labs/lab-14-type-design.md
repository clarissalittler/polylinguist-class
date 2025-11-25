# Lab 14: Type Design

**Quarter 2, Week 3**
**Duration:** 90 minutes
**Format:** Pair programming

## Overview

Types aren't just annotations—they're a design tool. Well-designed types can make illegal states unrepresentable and catch bugs at compile time. This lab explores generic programming and type-driven design.

## Objectives

By the end of this lab, you will:
- [ ] Write generic/polymorphic functions
- [ ] Design custom types to model domains
- [ ] Use type aliases for clarity
- [ ] Understand type safety benefits

## Setup

- Partner up
- Create folder: `lab14-types/`
- Files: `types.py`, `types.cpp`, `types.hs`

---

## Part 1: Generic Programming (25 minutes)

### Activity 1.1: Why Generics?

**Without generics - code duplication:**
```python
def first_int(lst: list[int]) -> int:
    return lst[0]

def first_str(lst: list[str]) -> str:
    return lst[0]

def first_float(lst: list[float]) -> float:
    return lst[0]
# ...repeat for every type!
```

**With generics - one function for all:**
```python
from typing import TypeVar, List

T = TypeVar('T')

def first(lst: List[T]) -> T:
    """Get first element of any list."""
    return lst[0]

# Works with any type
print(first([1, 2, 3]))        # 1
print(first(["a", "b", "c"]))  # "a"
print(first([1.5, 2.5]))       # 1.5
```

### Activity 1.2: Generics in All Languages

**Python (typing module):**
```python
from typing import TypeVar, List, Dict, Optional, Callable

T = TypeVar('T')
K = TypeVar('K')
V = TypeVar('V')

def identity(x: T) -> T:
    """Return the input unchanged."""
    return x

def map_list(func: Callable[[T], V], lst: List[T]) -> List[V]:
    """Apply function to each element."""
    return [func(x) for x in lst]

def lookup(d: Dict[K, V], key: K, default: V) -> V:
    """Look up key with default value."""
    return d.get(key, default)
```

**C++ (templates):**
```cpp
#include <iostream>
#include <vector>
#include <string>
using namespace std;

template<typename T>
T identity(T x) {
    return x;
}

template<typename T>
T first(const vector<T>& vec) {
    return vec[0];
}

template<typename T>
void printVector(const vector<T>& vec) {
    for (const T& item : vec) {
        cout << item << " ";
    }
    cout << endl;
}

int main() {
    cout << identity(42) << endl;       // 42
    cout << identity("hello") << endl;  // hello

    vector<int> nums = {1, 2, 3};
    vector<string> words = {"a", "b", "c"};

    cout << first(nums) << endl;   // 1
    cout << first(words) << endl;  // a

    printVector(nums);   // 1 2 3
    printVector(words);  // a b c

    return 0;
}
```

**Haskell (parametric polymorphism):**
```haskell
-- Haskell infers polymorphic types automatically!

identity :: a -> a
identity x = x

first :: [a] -> a
first (x:_) = x

-- Works with any type
main :: IO ()
main = do
    print (identity 42)       -- 42
    print (identity "hello")  -- "hello"
    print (first [1, 2, 3])   -- 1
    print (first ["a", "b"])  -- "a"
```

### Activity 1.3: Practice - Generic Functions

**Your turn:** Implement these generic functions:

```python
from typing import TypeVar, List, Tuple

T = TypeVar('T')

def last(lst: List[T]) -> T:
    """Return last element of a list."""
    pass

def swap(pair: Tuple[T, T]) -> Tuple[T, T]:
    """Swap elements of a pair."""
    pass

def repeat(x: T, n: int) -> List[T]:
    """Create a list with x repeated n times."""
    pass
```

### ✅ Checkpoint 1

Verify:
- [ ] Understand why generics reduce duplication
- [ ] Can write generic functions in Python
- [ ] Implemented practice functions

---

## Part 2: Custom Types for Domain Modeling (25 minutes)

### Activity 2.1: Type Aliases

**Python:**
```python
from typing import List, Dict, Tuple

# Type aliases make code more readable
UserId = int
Username = str
Email = str

# Complex type aliases
UserRecord = Dict[str, str]
UserList = List[UserRecord]
Coordinate = Tuple[float, float]

def get_user(user_id: UserId) -> UserRecord:
    # More meaningful than Dict[str, str]
    pass

def distance(p1: Coordinate, p2: Coordinate) -> float:
    # Clear what p1 and p2 represent
    import math
    return math.sqrt((p2[0] - p1[0])**2 + (p2[1] - p1[1])**2)
```

**Haskell:**
```haskell
-- Type synonyms (aliases)
type UserId = Int
type Username = String
type Email = String

type UserRecord = [(String, String)]
type Coordinate = (Double, Double)

-- More meaningful signatures
getUser :: UserId -> UserRecord
getUser uid = [("id", show uid)]

distance :: Coordinate -> Coordinate -> Double
distance (x1, y1) (x2, y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)
```

### Activity 2.2: NewType Pattern

Type aliases don't prevent mixing up types:

```python
UserId = int
ProductId = int

def get_user(user_id: UserId): pass
def get_product(product_id: ProductId): pass

# This is WRONG but Python won't catch it!
user = get_user(product_id)  # Oops! Passed ProductId to get_user
```

**Solution - NewType (Python):**
```python
from typing import NewType

UserId = NewType('UserId', int)
ProductId = NewType('ProductId', int)

def get_user(user_id: UserId) -> str:
    return f"User {user_id}"

def get_product(product_id: ProductId) -> str:
    return f"Product {product_id}"

# Now type checker catches mistakes
uid = UserId(123)
pid = ProductId(456)

get_user(uid)  # OK
get_user(pid)  # Type error! (with mypy)
```

**Haskell - newtype:**
```haskell
newtype UserId = UserId Int deriving (Show, Eq)
newtype ProductId = ProductId Int deriving (Show, Eq)

getUser :: UserId -> String
getUser (UserId uid) = "User " ++ show uid

getProduct :: ProductId -> String
getProduct (ProductId pid) = "Product " ++ show pid

-- Compiler prevents mixing!
-- getUser (ProductId 123)  -- Won't compile!
```

### Activity 2.3: Algebraic Data Types

**Modeling with sum types (enums):**

```haskell
-- Sum type: one of several variants
data PaymentMethod = Cash
                   | CreditCard String String  -- number, expiry
                   | PayPal String             -- email
                   | Crypto String             -- wallet address
    deriving Show

processPayment :: PaymentMethod -> Double -> String
processPayment Cash amount =
    "Received $" ++ show amount ++ " in cash"
processPayment (CreditCard num _) amount =
    "Charging $" ++ show amount ++ " to card ending " ++ take 4 (reverse num)
processPayment (PayPal email) amount =
    "Requesting $" ++ show amount ++ " via PayPal to " ++ email
processPayment (Crypto wallet) amount =
    "Sending to wallet " ++ take 8 wallet ++ "..."
```

**Python equivalent with dataclasses:**
```python
from dataclasses import dataclass
from typing import Union

@dataclass
class Cash:
    pass

@dataclass
class CreditCard:
    number: str
    expiry: str

@dataclass
class PayPal:
    email: str

@dataclass
class Crypto:
    wallet: str

PaymentMethod = Union[Cash, CreditCard, PayPal, Crypto]

def process_payment(method: PaymentMethod, amount: float) -> str:
    match method:
        case Cash():
            return f"Received ${amount} in cash"
        case CreditCard(num, _):
            return f"Charging ${amount} to card ending {num[-4:]}"
        case PayPal(email):
            return f"Requesting ${amount} via PayPal to {email}"
        case Crypto(wallet):
            return f"Sending to wallet {wallet[:8]}..."
```

### Activity 2.4: Practice - Model a Domain

Design types for a library system:

```python
from dataclasses import dataclass
from typing import List, Optional
from datetime import date
from enum import Enum

# TODO: Define these types

class BookStatus(Enum):
    AVAILABLE = "available"
    BORROWED = "borrowed"
    RESERVED = "reserved"

@dataclass
class Book:
    # What fields does a book have?
    pass

@dataclass
class Member:
    # What fields does a member have?
    pass

@dataclass
class Loan:
    # What fields does a loan have?
    pass

# Now design functions that use these types
def borrow_book(member: Member, book: Book) -> Optional[Loan]:
    pass

def return_book(loan: Loan) -> Book:
    pass
```

### ✅ Checkpoint 2

Verify:
- [ ] Understand type aliases
- [ ] Know when to use NewType
- [ ] Can model domains with custom types

---

## Part 3: Making Illegal States Unrepresentable (20 minutes)

### Activity 3.1: The Problem

```python
# Bad design: too many invalid states possible
@dataclass
class User:
    email: str
    is_verified: bool
    verification_code: Optional[str]  # Only needed if not verified
    verified_at: Optional[date]       # Only set if verified

# These states are INVALID but possible:
# - is_verified=True but verification_code is set
# - is_verified=False but verified_at is set
# - is_verified=True but verified_at is None
```

### Activity 3.2: The Solution

```python
from dataclasses import dataclass
from datetime import datetime
from typing import Union

@dataclass
class UnverifiedUser:
    email: str
    verification_code: str

@dataclass
class VerifiedUser:
    email: str
    verified_at: datetime

# User is either unverified or verified - no invalid states!
User = Union[UnverifiedUser, VerifiedUser]

def verify_user(user: UnverifiedUser, code: str) -> User:
    """Verify a user with the correct code."""
    if code == user.verification_code:
        return VerifiedUser(user.email, datetime.now())
    return user  # Still unverified

def send_welcome_email(user: VerifiedUser) -> None:
    """Can only send to verified users - enforced by types!"""
    print(f"Welcome, {user.email}!")
```

**Haskell version:**
```haskell
import Data.Time (UTCTime)

data UnverifiedUser = UnverifiedUser
    { uvEmail :: String
    , verificationCode :: String
    }

data VerifiedUser = VerifiedUser
    { vEmail :: String
    , verifiedAt :: UTCTime
    }

data User = Unverified UnverifiedUser
          | Verified VerifiedUser

-- This function ONLY accepts verified users
sendWelcomeEmail :: VerifiedUser -> IO ()
sendWelcomeEmail user = putStrLn $ "Welcome, " ++ vEmail user ++ "!"
```

### Activity 3.3: More Examples

**Order states:**
```haskell
data Order
    = Draft [Item]                      -- Can be modified
    | Submitted [Item] UTCTime          -- Waiting for payment
    | Paid [Item] UTCTime PaymentInfo   -- Ready to ship
    | Shipped [Item] TrackingNumber     -- In transit
    | Delivered [Item] UTCTime          -- Complete

-- Functions only accept valid states
shipOrder :: Paid -> TrackingNumber -> Shipped
shipOrder (Paid items _ _) tracking = Shipped items tracking

-- Can't ship a Draft - won't compile!
```

**Form validation:**
```python
from dataclasses import dataclass

@dataclass
class UnvalidatedForm:
    """Raw user input - might be invalid."""
    email: str
    age: str

@dataclass
class ValidatedForm:
    """Guaranteed to be valid."""
    email: str  # Verified format
    age: int    # Verified to be positive integer

def validate_form(form: UnvalidatedForm) -> ValidatedForm | list[str]:
    """Returns ValidatedForm or list of error messages."""
    errors = []

    if "@" not in form.email:
        errors.append("Invalid email format")

    try:
        age = int(form.age)
        if age < 0:
            errors.append("Age must be positive")
    except ValueError:
        errors.append("Age must be a number")
        age = 0

    if errors:
        return errors
    return ValidatedForm(form.email, age)

def process_form(form: ValidatedForm) -> None:
    """Only accepts validated forms - safe to use!"""
    print(f"Processing {form.email}, age {form.age}")
```

### Activity 3.4: Redesign Exercise

Redesign this problematic type:

```python
# Problem: Many invalid states possible
@dataclass
class ShoppingCart:
    items: List[Item]
    is_empty: bool           # Redundant with len(items)
    total: float             # Can be wrong
    discount_applied: bool
    discount_code: Optional[str]  # Can be inconsistent with discount_applied
    checked_out: bool
    order_id: Optional[str]  # Should only exist if checked_out
```

**Your redesign:**
```python
# TODO: Create separate types for different cart states
# CartEmpty, CartWithItems, CartWithDiscount, CheckedOutCart
```

### ✅ Checkpoint 3

Verify:
- [ ] Understand "make illegal states unrepresentable"
- [ ] Can identify problematic type designs
- [ ] Redesigned the ShoppingCart

---

## Part 4: Type Constraints (15 minutes)

### Activity 4.1: Bounded Generics

Sometimes generics need constraints:

**Python (Protocol/ABC):**
```python
from typing import TypeVar, Protocol

class Comparable(Protocol):
    def __lt__(self, other) -> bool: ...

T = TypeVar('T', bound=Comparable)

def minimum(x: T, y: T) -> T:
    """Return smaller of two comparable values."""
    return x if x < y else y

print(minimum(3, 5))        # 3
print(minimum("a", "b"))    # "a"
```

**Haskell (type classes):**
```haskell
-- Ord constraint ensures values can be compared
minimum' :: Ord a => a -> a -> a
minimum' x y = if x < y then x else y

-- Num constraint ensures numeric operations
double :: Num a => a -> a
double x = x + x

-- Multiple constraints
showAndCompare :: (Show a, Ord a) => a -> a -> String
showAndCompare x y =
    show x ++ " vs " ++ show y ++ ": " ++
    if x < y then "first is smaller" else "second is smaller or equal"
```

**C++ (concepts, C++20):**
```cpp
#include <concepts>

template<typename T>
requires std::totally_ordered<T>
T minimum(T a, T b) {
    return a < b ? a : b;
}

// Or older style with SFINAE
template<typename T>
typename std::enable_if<std::is_arithmetic<T>::value, T>::type
double_value(T x) {
    return x + x;
}
```

### Activity 4.2: Common Type Classes (Haskell)

```haskell
-- Eq: equality testing
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys) = x == y || elem' x ys

-- Ord: ordering
sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = sort [y | y <- xs, y < x]
           ++ [x]
           ++ sort [y | y <- xs, y >= x]

-- Show: convert to string
showList' :: Show a => [a] -> String
showList' xs = "[" ++ intercalate ", " (map show xs) ++ "]"

-- Num: numeric operations
sumList :: Num a => [a] -> a
sumList = foldr (+) 0

-- Combining constraints
describeRange :: (Show a, Ord a) => [a] -> String
describeRange [] = "empty"
describeRange xs = "from " ++ show (minimum xs) ++ " to " ++ show (maximum xs)
```

---

## Part 5: Practical Type Design (5 minutes)

### Activity 5.1: Guidelines

1. **Start with types** - Design types before functions
2. **Use specific types** - `Email` not `String`
3. **Make illegal states unrepresentable**
4. **Use enums for fixed choices**
5. **Use generics to reduce duplication**
6. **Add constraints when needed**

### Activity 5.2: Type-Driven Development Process

```
1. Define the domain types
2. Write function signatures (types only)
3. Let the types guide implementation
4. The compiler catches mistakes
```

---

## Challenges

### Challenge 1: Type-Safe Builder

Create a builder pattern where the compiler ensures all required fields are set:

```python
# Goal: Can't call .build() until all required fields are set
user = UserBuilder()
    .set_name("Alice")
    .set_email("alice@example.com")
    .build()  # Only works if name AND email are set
```

### Challenge 2: State Machine Types

Model a TCP connection state machine where invalid transitions are compile-time errors.

### Challenge 3: Units of Measure

Create types for units that prevent adding incompatible units:

```python
# Should be a type error:
distance = Meters(100) + Seconds(5)  # Can't add meters and seconds!

# Should work:
total = Meters(100) + Meters(50)  # OK
```

---

## Wrap-Up

**Key takeaways:**

1. **Generics** reduce code duplication
2. **Type aliases** improve readability
3. **NewType** prevents mixing up similar types
4. **Sum types** model "one of" relationships
5. **Product types** (records) model "all of" relationships
6. **Make illegal states unrepresentable**

**Type design mantra:**
> "If it compiles, it works" (mostly!)

**Next lab:** Error Handling - because things still go wrong!
