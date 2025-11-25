# Lab 17: Functional Refactoring

**Quarter 2, Week 6**
**Duration:** 90 minutes
**Format:** Pair programming

## Overview

Functional programming isn't just for Haskell—it's a mindset that can improve code in any language. This lab transforms imperative code into functional style, making it more readable, testable, and maintainable.

## Objectives

By the end of this lab, you will:
- [ ] Identify imperative patterns that can be functionalized
- [ ] Convert loops to map/filter/reduce
- [ ] Eliminate mutable state where possible
- [ ] Write pure functions

## Setup

- Partner up
- Create folder: `lab17-refactoring/`
- We'll work primarily in Python

---

## Part 1: Identifying Refactoring Opportunities (15 minutes)

### Activity 1.1: Code Smells for Functional Refactoring

**Smell 1: Accumulator loops**
```python
# Before: Accumulating results in a loop
result = []
for item in items:
    result.append(transform(item))

# After: Use map
result = list(map(transform, items))
# Or: result = [transform(item) for item in items]
```

**Smell 2: Conditional accumulation**
```python
# Before: Filter in a loop
result = []
for item in items:
    if predicate(item):
        result.append(item)

# After: Use filter
result = list(filter(predicate, items))
# Or: result = [item for item in items if predicate(item)]
```

**Smell 3: Running total**
```python
# Before: Accumulate a single value
total = 0
for item in items:
    total += item.price

# After: Use reduce or sum
total = sum(item.price for item in items)
```

**Smell 4: Nested mutation**
```python
# Before: Mutating nested data
for user in users:
    user['name'] = user['name'].upper()

# After: Create new data
users = [
    {**user, 'name': user['name'].upper()}
    for user in users
]
```

### Activity 1.2: Find the Smells

Identify refactoring opportunities in this code:

```python
def process_orders(orders):
    # Calculate total revenue from completed orders
    total = 0
    for order in orders:
        if order['status'] == 'completed':
            for item in order['items']:
                total += item['price'] * item['quantity']

    # Get names of customers with large orders
    big_customers = []
    for order in orders:
        order_total = 0
        for item in order['items']:
            order_total += item['price'] * item['quantity']
        if order_total > 100:
            big_customers.append(order['customer_name'])

    # Remove duplicates
    unique_customers = []
    for name in big_customers:
        if name not in unique_customers:
            unique_customers.append(name)

    return total, unique_customers
```

List the smells you found:
1. _______________________
2. _______________________
3. _______________________

### ✅ Checkpoint 1

Verify:
- [ ] Can identify accumulator loops
- [ ] Can identify conditional accumulation
- [ ] Found smells in example code

---

## Part 2: Step-by-Step Refactoring (30 minutes)

### Activity 2.1: Extract Helper Functions

First, identify repeated logic and extract it:

```python
# Before
def process_orders(orders):
    total = 0
    for order in orders:
        if order['status'] == 'completed':
            for item in order['items']:
                total += item['price'] * item['quantity']
    ...

# Step 1: Extract order_total calculation
def order_total(order):
    """Calculate total price for an order."""
    return sum(
        item['price'] * item['quantity']
        for item in order['items']
    )

# Now the main function is clearer
def process_orders(orders):
    total = 0
    for order in orders:
        if order['status'] == 'completed':
            total += order_total(order)
    ...
```

### Activity 2.2: Convert to Functional Style

```python
# Step 2: Convert to filter + map + sum
def process_orders(orders):
    completed = filter(lambda o: o['status'] == 'completed', orders)
    totals = map(order_total, completed)
    total = sum(totals)
    ...

# Step 3: Or as a single expression
def get_revenue(orders):
    return sum(
        order_total(order)
        for order in orders
        if order['status'] == 'completed'
    )
```

### Activity 2.3: Full Refactoring

**Original code:**
```python
def process_orders(orders):
    total = 0
    for order in orders:
        if order['status'] == 'completed':
            for item in order['items']:
                total += item['price'] * item['quantity']

    big_customers = []
    for order in orders:
        order_total = 0
        for item in order['items']:
            order_total += item['price'] * item['quantity']
        if order_total > 100:
            big_customers.append(order['customer_name'])

    unique_customers = []
    for name in big_customers:
        if name not in unique_customers:
            unique_customers.append(name)

    return total, unique_customers
```

**Refactored code:**
```python
def item_total(item):
    """Calculate total for a single item."""
    return item['price'] * item['quantity']

def order_total(order):
    """Calculate total for an order."""
    return sum(item_total(item) for item in order['items'])

def get_revenue(orders):
    """Get total revenue from completed orders."""
    return sum(
        order_total(order)
        for order in orders
        if order['status'] == 'completed'
    )

def get_big_customers(orders, threshold=100):
    """Get unique customer names with orders over threshold."""
    big_order_names = (
        order['customer_name']
        for order in orders
        if order_total(order) > threshold
    )
    return list(set(big_order_names))  # set removes duplicates

def process_orders(orders):
    """Process orders and return revenue and big customers."""
    return get_revenue(orders), get_big_customers(orders)
```

### Activity 2.4: Your Turn - Refactor This

```python
def analyze_students(students):
    # Find average grade
    total_grade = 0
    count = 0
    for student in students:
        total_grade += student['grade']
        count += 1
    average = total_grade / count if count > 0 else 0

    # Find failing students
    failing = []
    for student in students:
        if student['grade'] < 60:
            failing.append(student['name'])

    # Find students who improved
    improved = []
    for student in students:
        if student['current_grade'] > student['previous_grade']:
            improved.append({
                'name': student['name'],
                'improvement': student['current_grade'] - student['previous_grade']
            })

    # Sort improved by improvement amount
    for i in range(len(improved)):
        for j in range(i + 1, len(improved)):
            if improved[j]['improvement'] > improved[i]['improvement']:
                improved[i], improved[j] = improved[j], improved[i]

    return average, failing, improved
```

Write your refactored version:

```python
# Your refactored code here
```

### ✅ Checkpoint 2

Verify:
- [ ] Extracted helper functions
- [ ] Converted loops to functional style
- [ ] Refactored the student analysis code

---

## Part 3: Eliminating Mutable State (20 minutes)

### Activity 3.1: Pure Functions

**Impure function (has side effects):**
```python
total = 0  # Global state

def add_to_total(x):
    global total
    total += x  # Modifies global state
    return total

add_to_total(5)  # Returns 5
add_to_total(3)  # Returns 8
add_to_total(5)  # Returns 13 - different result for same input!
```

**Pure function (no side effects):**
```python
def add(total, x):
    return total + x  # Returns new value, doesn't modify anything

total = 0
total = add(total, 5)  # 5
total = add(total, 3)  # 8
```

### Activity 3.2: Immutable Data Transformations

**Mutating approach:**
```python
def process_user(user):
    user['name'] = user['name'].strip().title()
    user['email'] = user['email'].lower()
    user['active'] = True
    return user

user = {'name': '  john doe  ', 'email': 'JOHN@EXAMPLE.COM'}
process_user(user)
# Original user is now modified!
```

**Immutable approach:**
```python
def process_user(user):
    return {
        **user,  # Copy existing fields
        'name': user['name'].strip().title(),
        'email': user['email'].lower(),
        'active': True
    }

user = {'name': '  john doe  ', 'email': 'JOHN@EXAMPLE.COM'}
new_user = process_user(user)
# Original user is unchanged!
```

### Activity 3.3: Functional Data Pipelines

```python
from functools import reduce

def pipeline(*functions):
    """Create a pipeline of functions."""
    def apply(data):
        return reduce(lambda x, f: f(x), functions, data)
    return apply

# Define transformation steps
def normalize_name(user):
    return {**user, 'name': user['name'].strip().title()}

def normalize_email(user):
    return {**user, 'email': user['email'].lower()}

def set_active(user):
    return {**user, 'active': True}

def add_timestamp(user):
    from datetime import datetime
    return {**user, 'processed_at': datetime.now().isoformat()}

# Compose into a pipeline
process_user = pipeline(
    normalize_name,
    normalize_email,
    set_active,
    add_timestamp
)

# Use the pipeline
users = [
    {'name': '  john doe  ', 'email': 'JOHN@EXAMPLE.COM'},
    {'name': 'JANE SMITH', 'email': 'Jane@Example.Com'},
]

processed = [process_user(u) for u in users]
```

### Activity 3.4: Practice - Make It Pure

Convert these impure functions to pure functions:

```python
# Impure: modifies the list in place
def remove_negatives(numbers):
    i = 0
    while i < len(numbers):
        if numbers[i] < 0:
            numbers.pop(i)
        else:
            i += 1

# Impure: uses global state
counter = 0
def count_calls(x):
    global counter
    counter += 1
    return x * 2

# Impure: modifies nested structure
def update_prices(products, increase_percent):
    for product in products:
        product['price'] *= (1 + increase_percent / 100)
```

Write pure versions:

```python
# Your pure versions here
```

### ✅ Checkpoint 3

Verify:
- [ ] Understand pure vs impure functions
- [ ] Can transform data immutably
- [ ] Converted impure functions to pure

---

## Part 4: Real-World Refactoring (20 minutes)

### Activity 4.1: Refactoring a Data Processor

**Original imperative code:**
```python
def process_sales_data(sales):
    # Clean data
    cleaned = []
    for sale in sales:
        if sale['amount'] > 0 and sale['customer_id'] is not None:
            cleaned.append(sale)

    # Group by customer
    by_customer = {}
    for sale in cleaned:
        cid = sale['customer_id']
        if cid not in by_customer:
            by_customer[cid] = []
        by_customer[cid].append(sale)

    # Calculate stats per customer
    stats = []
    for customer_id, customer_sales in by_customer.items():
        total = 0
        count = 0
        for sale in customer_sales:
            total += sale['amount']
            count += 1
        stats.append({
            'customer_id': customer_id,
            'total': total,
            'count': count,
            'average': total / count
        })

    # Sort by total descending
    for i in range(len(stats)):
        for j in range(i + 1, len(stats)):
            if stats[j]['total'] > stats[i]['total']:
                stats[i], stats[j] = stats[j], stats[i]

    return stats
```

**Refactored functional code:**
```python
from itertools import groupby
from operator import itemgetter
from statistics import mean

def is_valid_sale(sale):
    """Check if sale is valid."""
    return sale['amount'] > 0 and sale['customer_id'] is not None

def calculate_customer_stats(customer_id, sales):
    """Calculate statistics for a customer's sales."""
    amounts = [s['amount'] for s in sales]
    return {
        'customer_id': customer_id,
        'total': sum(amounts),
        'count': len(amounts),
        'average': mean(amounts) if amounts else 0
    }

def process_sales_data(sales):
    """Process sales data into customer statistics."""
    # Clean data
    valid_sales = filter(is_valid_sale, sales)

    # Sort by customer_id for groupby
    sorted_sales = sorted(valid_sales, key=itemgetter('customer_id'))

    # Group by customer and calculate stats
    stats = [
        calculate_customer_stats(customer_id, list(customer_sales))
        for customer_id, customer_sales
        in groupby(sorted_sales, key=itemgetter('customer_id'))
    ]

    # Sort by total descending
    return sorted(stats, key=itemgetter('total'), reverse=True)
```

### Activity 4.2: Your Challenge

Refactor this event processing code:

```python
def process_events(events):
    # Filter to only error events from last 24 hours
    from datetime import datetime, timedelta
    cutoff = datetime.now() - timedelta(hours=24)

    recent_errors = []
    for event in events:
        if event['type'] == 'error':
            event_time = datetime.fromisoformat(event['timestamp'])
            if event_time > cutoff:
                recent_errors.append(event)

    # Group by error code
    by_code = {}
    for event in recent_errors:
        code = event['error_code']
        if code not in by_code:
            by_code[code] = []
        by_code[code].append(event)

    # Find most common errors
    error_counts = []
    for code, events_list in by_code.items():
        error_counts.append({
            'code': code,
            'count': len(events_list),
            'messages': []
        })
        for e in events_list:
            if e['message'] not in error_counts[-1]['messages']:
                error_counts[-1]['messages'].append(e['message'])

    # Sort by count
    error_counts.sort(key=lambda x: x['count'], reverse=True)

    return error_counts[:10]  # Top 10
```

### ✅ Checkpoint 4

Verify:
- [ ] Refactored the event processing code
- [ ] Code is more readable and testable

---

## Part 5: When NOT to Refactor (5 minutes)

### Activity 5.1: Know When to Stop

**Keep imperative style when:**
- Performance is critical (functional can be slower)
- The loop is simple and clear
- Breaking early is needed
- Side effects are the point (I/O, database)

**Example - Early return is cleaner:**
```python
# Functional (processes all items)
def has_error(items):
    return any(item.is_error for item in items)

# Imperative (stops at first error - sometimes better)
def has_error(items):
    for item in items:
        if item.is_error:
            return True
    return False
```

**Example - Side effects are the goal:**
```python
# This SHOULD be imperative
def save_all_users(users, database):
    for user in users:
        database.save(user)
        log.info(f"Saved user {user.id}")
```

---

## Challenges

### Challenge 1: Refactor a Parser

Convert this CSV parser to functional style:

```python
def parse_csv(text):
    lines = text.strip().split('\n')
    headers = lines[0].split(',')
    result = []
    for line in lines[1:]:
        values = line.split(',')
        row = {}
        for i in range(len(headers)):
            row[headers[i]] = values[i]
        result.append(row)
    return result
```

### Challenge 2: Compose Validators

Create a functional validation pipeline:

```python
# Goal: compose validators that can be combined
validators = compose(
    validate_not_empty('name'),
    validate_email('email'),
    validate_range('age', 0, 150),
)

result = validators(user_data)  # Returns errors or validated data
```

---

## Wrap-Up

**Key takeaways:**

1. **Extract helper functions** first
2. **Convert loops** to map/filter/reduce
3. **Prefer pure functions** - no side effects
4. **Transform data immutably** - create new, don't modify
5. **Know when to stop** - imperative is sometimes better

**Refactoring mindset:**
- Small, incremental changes
- Keep tests passing
- Improve readability
- Make testing easier

**Next lab:** Thinking in Lisp - functional programming in Racket!
