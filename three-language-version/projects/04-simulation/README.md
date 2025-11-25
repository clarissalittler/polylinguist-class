# Project 4: Simulation

**Quarter 2, Weeks 1-4**
**Prerequisites:** Lessons 1-7 (through Object-Oriented Programming)

---

## The Big Idea

Build a simulation that **models a system evolving over time**. Watch agents interact, populations grow, economies fluctuate, or particles collide.

Simulations let us explore "what if?" questions that would be impractical or impossible to test in real life.

---

## Base Requirements

Your simulation must:

1. **Model entities** with state (attributes that change)
2. **Evolve over time** in discrete steps
3. **Show emergent behavior** - the whole is more than the sum of parts
4. **Visualize state** - display what's happening (even if just text)
5. **Allow configuration** - adjustable parameters

### Core Features
- At least 2 types of entities or agents
- State changes based on rules
- Time progresses in steps (ticks/generations/days)
- Observable, interesting behavior emerges

---

## Simulation Ideas

### Life & Ecosystems
- **Predator-Prey**: Foxes and rabbits on a grid
- **Forest Fire**: Trees grow, fires spread, forests regenerate
- **Ant Colony**: Ants forage, leave pheromones, find food
- **Evolution**: Creatures with traits that mutate and compete

### Society & Economics
- **Traffic**: Cars on roads, congestion emerges
- **Market**: Buyers and sellers, prices fluctuate
- **Disease Spread**: Infection model (SIR/SEIR)
- **Opinion Dynamics**: People influence neighbors' beliefs

### Physics & Math
- **Particles**: Gravity, collisions, orbital mechanics
- **Cellular Automata**: Conway's Game of Life, Rule 110
- **Diffusion**: Heat spreading, ink in water
- **Flocking**: Birds or fish moving together (boids)

### Games & Systems
- **Colony Builder**: Resources, workers, growth
- **Battle Sim**: Units with AI fighting
- **Sports League**: Teams play season, rankings change
- **Dungeon Generator**: Rooms and corridors grow procedurally

---

## Example: Predator-Prey Ecosystem

```
═══════════════════════════════════════════════════════════════
              ECOSYSTEM SIMULATION - Day 47
═══════════════════════════════════════════════════════════════

  . . 🐰 . . . . 🦊 . .
  . 🌿 🌿 . 🐰 . . . . .
  . 🌿 🌿 🌿 . . . 🐰 . .
  . . 🌿 . . 🦊 . . . .
  🐰 . . . . . . . . 🐰
  . . . . 🌿 🌿 . . . .
  . 🦊 . 🐰 🌿 🌿 . . . .
  . . . . . . . . 🐰 .

POPULATION
  🐰 Rabbits: 12  (↑ 2 born, ↓ 1 eaten)
  🦊 Foxes: 4     (↓ 1 starved)
  🌿 Grass: 18    (↑ 3 grew)

STATS
  Avg rabbit age: 5.2 days
  Avg fox hunger: 3/10
  Ecosystem stability: Balanced

[Press Enter for next day, 'q' to quit, 's' for settings]
>

Day 48...
A fox catches a rabbit!
Two rabbits reproduce.
A patch of grass grows.
```

---

## Example: Traffic Simulation

```
═══════════════════════════════════════════════════════════════
              TRAFFIC SIMULATION - Tick 156
═══════════════════════════════════════════════════════════════

    →→→→→.....→→→.→→→→→→→...........→→
    ════════════════════════════════════
                    ↓ [RED]
    ←←←←←←←←←...←←←←←←............←←←
    ════════════════════════════════════

INTERSECTION STATUS
  Light: RED (North-South) / GREEN (East-West)
  Changing in: 12 ticks

METRICS
  Cars passed this cycle: 23
  Average wait time: 8.3 ticks
  Current queue: N:5, S:3, E:0, W:2
  Traffic density: Medium

SETTINGS
  Light cycle: 30 ticks
  Car spawn rate: 0.3/tick
  Car speed: 1-2 cells/tick

[Space: pause, +/-: speed, 'c': change light timing]
```

---

## Technical Requirements

### Entity Design
Use OOP concepts to model your simulation:
- Classes/types for different entities
- Attributes for state (position, health, age, etc.)
- Methods for behavior (move, eat, reproduce, die)

### Simulation Loop
```
initialize_world()
while not done:
    for each entity:
        entity.decide_action()
    for each entity:
        entity.execute_action()
    handle_interactions()
    update_world()
    display_state()
    advance_time()
```

### Configurability
Let users adjust:
- Initial conditions (population sizes, starting state)
- Rules (reproduction rate, movement speed)
- World parameters (grid size, resource abundance)

### Visualization
At minimum: text-based display of state each step
Better: ASCII graphics, charts, or statistics

---

## Creative Extensions

### Visual (+5 each)
- **Animated display**: Smooth updating without flicker
- **ASCII graphics**: Icons, colors, box drawing
- **Charts**: Population graphs over time
- **Heatmaps**: Density, activity levels

### Complexity (+5 each)
- **More entity types**: Add complexity to ecosystem
- **Learning agents**: Entities that adapt behavior
- **Multiple resources**: Competition for different needs
- **Spatial structure**: Terrain, obstacles, regions

### Analysis (+5 each)
- **Statistics tracking**: Birth/death rates, averages
- **History playback**: Review past states
- **Equilibrium detection**: Detect stable states
- **Parameter sweeps**: Auto-test different settings

### Interactivity (+5 each)
- **User intervention**: Add/remove entities
- **Real-time parameter changes**: Adjust while running
- **Save/load states**: Pause and resume
- **Multiple runs**: Compare outcomes

### Multi-Language Bonus (+10)
- Implement core simulation in a second language
- Compare OOP approaches across languages

---

## Getting Started

### Step 1: Choose Your System
What do you want to simulate? Pick something that interests you!

### Step 2: Define Your Entities
What types of things exist? What properties do they have?

### Step 3: Define Your Rules
How do entities behave? How do they interact?

### Step 4: Build Minimal Version
- Create entity classes
- Implement basic behavior
- Get something moving on screen

### Step 5: Add Complexity
- More behaviors
- More interactions
- Better visualization

### Step 6: Tune and Explore
- Adjust parameters
- Find interesting behaviors
- Document what you discover

---

## Language Hints

### Python
```python
class Entity:
    def __init__(self, x, y):
        self.x = x
        self.y = y
        self.energy = 100

    def update(self, world):
        raise NotImplementedError

class Rabbit(Entity):
    def update(self, world):
        self.move_randomly(world)
        self.energy -= 1
        if self.energy > 150:
            self.reproduce(world)
        self.eat_grass(world)

class Fox(Entity):
    def update(self, world):
        nearby_rabbit = self.find_nearby(world, Rabbit)
        if nearby_rabbit:
            self.move_toward(nearby_rabbit)
        else:
            self.move_randomly(world)
        self.hunt(world)

class World:
    def __init__(self, width, height):
        self.entities = []
        self.grid = [[None] * width for _ in range(height)]

    def step(self):
        for entity in self.entities[:]:
            entity.update(self)
```

### C++
```cpp
class Entity {
public:
    int x, y;
    int energy;

    virtual void update(World& world) = 0;
    virtual ~Entity() = default;
};

class Rabbit : public Entity {
public:
    void update(World& world) override {
        moveRandomly(world);
        energy--;
        if (energy > 150) reproduce(world);
        eatGrass(world);
    }
};

class World {
    std::vector<std::unique_ptr<Entity>> entities;
public:
    void step() {
        for (auto& entity : entities) {
            entity->update(*this);
        }
    }
};
```

### Haskell
```haskell
-- Functional approach: state transformations
data Entity = Rabbit { pos :: (Int, Int), energy :: Int }
            | Fox { pos :: (Int, Int), hunger :: Int }
            deriving (Show)

data World = World
    { entities :: [Entity]
    , grass :: [(Int, Int)]
    , tick :: Int
    }

updateEntity :: World -> Entity -> Entity
updateEntity world (Rabbit p e) =
    Rabbit (moveRandom p) (e - 1 + grassBonus)
  where grassBonus = if p `elem` grass world then 10 else 0

stepWorld :: World -> World
stepWorld w = w
    { entities = map (updateEntity w) (entities w)
    , tick = tick w + 1
    }
```

---

## Reflection Questions

Answer in your `REFLECTION.md`:

1. **What did you simulate and why?** What drew you to this system?

2. **What emergent behavior did you observe?** Anything surprising?

3. **How did you use OOP (or choose not to)?** Classes, inheritance, composition?

4. **What parameters had the biggest effect?** What makes the system stable/unstable?

5. **How would a real scientist validate this simulation?** What's realistic vs simplified?

6. **What would you add to make it more realistic/interesting?**

---

## Submission Checklist

- [ ] Simulation has multiple entity types
- [ ] State evolves over time based on rules
- [ ] Emergent behavior is observable
- [ ] Parameters are configurable
- [ ] Visualization shows what's happening
- [ ] Code uses OOP concepts appropriately
- [ ] Git history shows development
- [ ] REFLECTION.md completed
- [ ] Ready for showcase!

---

## Showcase Presentation

5-7 minutes:
1. **Explain your system** (1-2 min) - What are you simulating? What entities and rules?
2. **Live demo** (3-4 min) - Run simulation, point out interesting behaviors
3. **What you discovered** (1-2 min) - Surprising emergent behavior, insights about the system

---

**Build a world and watch it come alive!**
