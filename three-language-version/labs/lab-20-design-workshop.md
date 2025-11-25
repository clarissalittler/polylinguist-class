# Lab 20: Design Workshop

**Quarter 2, Week 10**
**Duration:** 90 minutes
**Format:** Group design exercise

## Overview

Good software starts with good design. This lab practices designing systems before writing code—thinking through architecture, data models, and interfaces before implementation.

## Objectives

By the end of this lab, you will:
- [ ] Break down a problem into components
- [ ] Design data models and interfaces
- [ ] Create system diagrams
- [ ] Practice design review

## Setup

- Form groups of 3-4
- Whiteboard or large paper
- Markers
- No coding today—just design!

---

## Part 1: Design Thinking Process (10 minutes)

### The Design Process

1. **Understand** - What problem are we solving?
2. **Explore** - What are possible approaches?
3. **Define** - What are the key components?
4. **Design** - How do components interact?
5. **Review** - What could go wrong?

### Questions to Ask

**About the problem:**
- Who are the users?
- What are the key use cases?
- What are the constraints?
- What could go wrong?

**About the solution:**
- What are the main components?
- How do they communicate?
- What data do we need to store?
- What are the interfaces?

---

## Part 2: Design Exercise - Social Media App (30 minutes)

### The Problem

Design a simple social media platform with:
- User profiles
- Posts (text only)
- Following other users
- A feed showing posts from followed users
- Likes on posts

### Activity 2.1: Identify Entities (10 minutes)

**What "things" exist in this system?**

List the main entities:
1. _______________
2. _______________
3. _______________
4. _______________

**For each entity, what data does it have?**

```
User:
- id
- username
- email
- bio
- created_at
- ???

Post:
- ???

Follow:
- ???

Like:
- ???
```

### Activity 2.2: Define Relationships (10 minutes)

Draw boxes for each entity and connect them:

```
+--------+         +--------+
|  User  |-------->|  Post  |
+--------+         +--------+
    |                  |
    |                  v
    |             +--------+
    +------------>|  Like  |
                  +--------+
```

**Questions:**
- Can a user follow themselves?
- Can a user like their own post?
- Can a user like a post multiple times?
- What happens when a user is deleted?

### Activity 2.3: Define Operations (10 minutes)

**What actions can users take?**

```
User operations:
- create_user(username, email, password) -> User
- get_user(user_id) -> User
- update_profile(user_id, bio) -> User
- delete_user(user_id) -> void

Post operations:
- create_post(user_id, content) -> Post
- ???

Follow operations:
- ???

Like operations:
- ???

Feed operations:
- get_feed(user_id) -> List[Post]
- ???
```

### Activity 2.4: Design the Feed Algorithm

**How do we generate a user's feed?**

Simple approach:
```
1. Get all users this user follows
2. Get all posts from those users
3. Sort by timestamp (newest first)
4. Return top N posts
```

**Discussion:**
- How does this scale with millions of users?
- Should we cache the feed?
- How do we handle new posts?
- What about posts from very active users?

### ✅ Checkpoint 1

Present your design:
- [ ] Entity diagram drawn
- [ ] Relationships defined
- [ ] Operations listed
- [ ] Feed algorithm explained

---

## Part 3: API Design (20 minutes)

### Activity 3.1: RESTful API

Design the HTTP endpoints:

```
# Users
POST   /users              # Create user
GET    /users/{id}         # Get user
PUT    /users/{id}         # Update user
DELETE /users/{id}         # Delete user

# Posts
POST   /posts              # Create post
GET    /posts/{id}         # Get post
DELETE /posts/{id}         # Delete post
GET    /users/{id}/posts   # Get user's posts

# Follows
POST   /users/{id}/follow  # Follow a user
DELETE /users/{id}/follow  # Unfollow
GET    /users/{id}/followers  # Get followers
GET    /users/{id}/following  # Get following

# Likes
POST   /posts/{id}/like    # Like a post
DELETE /posts/{id}/like    # Unlike
GET    /posts/{id}/likes   # Get likes

# Feed
GET    /feed               # Get current user's feed
```

### Activity 3.2: Request/Response Formats

Define the JSON structures:

```json
// Create user request
POST /users
{
    "username": "alice",
    "email": "alice@example.com",
    "password": "..."
}

// User response
{
    "id": 123,
    "username": "alice",
    "email": "alice@example.com",
    "bio": null,
    "created_at": "2024-01-15T10:30:00Z",
    "followers_count": 0,
    "following_count": 0
}

// Create post request
POST /posts
{
    ???
}

// Post response
{
    ???
}

// Feed response
{
    ???
}
```

### Activity 3.3: Error Handling

**What errors can occur?**

```json
// User not found
{
    "error": "not_found",
    "message": "User with id 999 not found"
}

// Validation error
{
    "error": "validation_error",
    "message": "Invalid email format",
    "field": "email"
}

// Already following
{
    "error": "conflict",
    "message": "Already following this user"
}
```

**HTTP status codes:**
- 200 OK - Success
- 201 Created - Resource created
- 400 Bad Request - Invalid input
- 401 Unauthorized - Not logged in
- 403 Forbidden - Not allowed
- 404 Not Found - Resource doesn't exist
- 409 Conflict - Already exists

---

## Part 4: Architecture Decisions (20 minutes)

### Activity 4.1: Data Storage

**Questions to consider:**
- SQL or NoSQL?
- How to store follows (adjacency list, separate table)?
- How to count followers efficiently?
- How to implement the feed?

**Option A: Relational (SQL)**
```sql
CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    username VARCHAR(50) UNIQUE,
    email VARCHAR(100) UNIQUE,
    bio TEXT,
    created_at TIMESTAMP
);

CREATE TABLE posts (
    id SERIAL PRIMARY KEY,
    user_id INTEGER REFERENCES users(id),
    content TEXT,
    created_at TIMESTAMP
);

CREATE TABLE follows (
    follower_id INTEGER REFERENCES users(id),
    followed_id INTEGER REFERENCES users(id),
    created_at TIMESTAMP,
    PRIMARY KEY (follower_id, followed_id)
);

CREATE TABLE likes (
    user_id INTEGER REFERENCES users(id),
    post_id INTEGER REFERENCES posts(id),
    created_at TIMESTAMP,
    PRIMARY KEY (user_id, post_id)
);
```

**Option B: Document (NoSQL)**
```javascript
// User document
{
    _id: ObjectId,
    username: "alice",
    email: "alice@example.com",
    bio: "...",
    following: [userId1, userId2, ...],
    followers: [userId3, userId4, ...]
}

// Post document
{
    _id: ObjectId,
    user_id: ObjectId,
    content: "Hello world!",
    likes: [userId1, userId2, ...],
    likes_count: 2
}
```

**Trade-offs:**
| Aspect | SQL | NoSQL |
|--------|-----|-------|
| Relationships | Strong | Embedded/Referenced |
| Consistency | ACID | Eventually consistent |
| Scaling | Vertical | Horizontal |
| Flexibility | Schema required | Schema-less |

### Activity 4.2: Feed Implementation Strategies

**Pull model (compute on request):**
```
get_feed(user_id):
    following = get_following(user_id)
    posts = get_posts_by_users(following, limit=100)
    return sort_by_time(posts)
```

**Push model (precompute):**
```
when_post_created(post):
    followers = get_followers(post.user_id)
    for follower in followers:
        add_to_feed_cache(follower, post)

get_feed(user_id):
    return get_feed_cache(user_id)
```

**Hybrid:**
```
# Pull for users with few followers
# Push for users following many accounts
```

### Activity 4.3: Draw the Architecture

```
                    +-------------+
                    |   Client    |
                    +------+------+
                           |
                           v
                    +------+------+
                    |  API Server |
                    +------+------+
                           |
          +----------------+----------------+
          |                |                |
          v                v                v
    +-----+----+    +------+-----+   +------+-----+
    | Database |    | Feed Cache |   |   Queue    |
    +----------+    +------------+   +------------+
```

### ✅ Checkpoint 2

Discuss:
- [ ] Data storage choice justified
- [ ] Feed strategy explained
- [ ] Architecture diagram drawn

---

## Part 5: Design Review (10 minutes)

### Activity 5.1: What Could Go Wrong?

For each issue, propose a solution:

| Issue | Solution |
|-------|----------|
| User posts 1000 times/minute | ??? |
| Celebrity with 10M followers | ??? |
| Database goes down | ??? |
| Spam accounts | ??? |
| Offensive content | ??? |

### Activity 5.2: Scalability Discussion

**How would you scale to:**
- 1,000 users?
- 100,000 users?
- 10,000,000 users?

**What changes at each level?**

### Activity 5.3: Present Your Design

Each group presents:
1. Entity model
2. API design
3. Key architecture decisions
4. How you'd handle scale

---

## Design Templates

### Entity Template

```
Entity: [Name]
Description: [What is this?]

Attributes:
- id: unique identifier
- [field]: [type] - [description]
- ...

Relationships:
- [has many/belongs to] [Other Entity]
- ...

Operations:
- create([params]) -> [Entity]
- ...
```

### API Endpoint Template

```
[METHOD] [path]
Description: [What does this do?]

Request:
- Headers: [required headers]
- Body: [JSON structure]

Response:
- Success (200): [JSON structure]
- Error (4xx): [JSON structure]
```

---

## Challenges

### Challenge 1: Add Features

Design extensions:
- Comments on posts
- Hashtags
- Notifications
- Direct messages

### Challenge 2: Different Domain

Apply the same process to:
- E-commerce platform
- Task management app
- Music streaming service

### Challenge 3: Design Document

Write a formal design document including:
- Problem statement
- Requirements (functional and non-functional)
- System architecture
- Data model
- API specification
- Security considerations

---

## Wrap-Up

**Key takeaways:**

1. **Design before code** - Think first, type later
2. **Start with entities** - What things exist?
3. **Define relationships** - How do things connect?
4. **Consider scale** - What happens with more users?
5. **Plan for failure** - What could go wrong?

**Good design is:**
- Simple enough to understand
- Flexible enough to change
- Robust enough to handle errors
- Scalable enough to grow

**Next week:** Quarter 2 Showcase - show off your projects!
