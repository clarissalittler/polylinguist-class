# Project 3: Interactive Story or Game

**Quarter 1, Weeks 8-11**
**Prerequisites:** Lessons 1-6 (through Recursion)

---

## The Big Idea

Create an **interactive experience** that responds to user choices. This could be:
- A branching narrative story
- A text-based adventure game
- A puzzle or mystery game
- A simulation with choices
- Something entirely unique!

This is the biggest project of Q1. Dream big, but scope wisely.

---

## Base Requirements

Your interactive experience must:

1. **Respond to user input** - Choices matter
2. **Maintain state** - Remember what's happened
3. **Have multiple paths** - At least 3 meaningfully different outcomes
4. **Be engaging** - Someone should want to play it
5. **Be completable** - Have clear ending(s)

### Minimum Scope
- At least 10 distinct "scenes" or decision points
- At least 3 different endings
- State that carries forward (inventory, stats, relationships, etc.)
- Clear win/lose conditions OR meaningful conclusion

---

## Project Types

### Branching Narrative
A story where your choices shape the outcome.
- Character-driven drama
- Mystery you solve through questioning
- Romance with relationship mechanics
- Horror with survival choices

### Text Adventure
Classic "go north, take sword" style exploration.
- Rooms to explore
- Items to collect and use
- Puzzles to solve
- Enemies or obstacles

### RPG-lite
Character stats and combat.
- Character creation/stats
- Turn-based combat
- Leveling or progression
- Multiple encounters

### Simulation Game
Manage resources and make decisions.
- Business/tycoon sim
- Survival management
- City/colony builder (simplified)
- Life simulator

### Puzzle Game
Logic challenges with a wrapper.
- Escape room
- Detective investigation
- Code-breaking
- Pattern recognition

---

## Example: Branching Story

```
═══════════════════════════════════════════════════════════════
                    THE MIDNIGHT LIBRARY
═══════════════════════════════════════════════════════════════

You wake up in a library that shouldn't exist. Infinite shelves
stretch in every direction, each book glowing faintly.

A librarian appears. "Each book contains a life you could have lived.
You may read three before you must choose one... or return to your own."

Your inventory: [Empty]
Lives explored: 0/3

What do you do?
  1. Ask the librarian about this place
  2. Grab the nearest glowing book
  3. Look for an exit
  4. Check your pockets

> 1

The librarian smiles sadly. "This is the space between lives. Everyone
comes here once, at a moment of great... uncertainty. The books show
paths not taken. But choose carefully—once you enter a book fully,
there's no coming back."

She hands you a small key. "This unlocks the restricted section.
Some lives there are... dangerous to explore."

[Obtained: Mysterious Key]

What do you do?
  1. Ask what makes a life "dangerous"
  2. Head to the restricted section immediately
  3. Explore the regular shelves first
  4. Try to remember how you got here

> _
```

---

## Example: Text Adventure

```
═══════════════════════════════════════════════════════════════
                    DUNGEON OF THE MAD WIZARD
═══════════════════════════════════════════════════════════════

ENTRANCE CHAMBER
You stand in a dusty stone chamber. Cobwebs hang from the ceiling.
A rusty torch flickers on the wall.

Exits: NORTH (dark corridor), EAST (wooden door)
You see: a rusty torch, a pile of bones

HP: 10/10 | Gold: 0 | Inventory: rope

> take torch

You take the rusty torch. It provides dim but steady light.
[Obtained: Rusty Torch]

> examine bones

Among the bones you find 5 gold coins and a crumpled note.
The note reads: "The wizard's weakness is hidden in the library."
[Obtained: 5 gold]

> go north

DARK CORRIDOR
Your torch illuminates a long corridor. You hear dripping water
and... something else. Movement in the shadows ahead.

A GIANT RAT blocks your path!

What do you do?
  1. Attack!
  2. Try to sneak past
  3. Offer food (you have none)
  4. Retreat south

> _
```

---

## Technical Requirements

### State Management
Your program needs to track:
- Current location/scene
- Player inventory or attributes
- Choices already made
- Any other relevant state

Use appropriate data structures (dictionaries, classes, records).

### Code Structure
- Separate content (story text) from logic where possible
- Use functions for repeated actions
- Organize scenes/rooms logically
- Handle invalid input gracefully

### Game Loop
```
while game_not_over:
    display_current_state()
    get_player_input()
    process_action()
    update_state()
    check_win_lose_conditions()
```

### Version Control
- Commit frequently as you build
- Good commit messages ("Add combat system", "Fix inventory bug")

---

## Creative Extensions

### Narrative Depth (+5 each)
- **Relationship tracking**: NPCs remember how you treated them
- **Parallel plotlines**: Multiple story threads that interweave
- **Hidden content**: Secret paths or Easter eggs
- **Multiple protagonists**: Switch between characters

### Gameplay Systems (+5 each)
- **Save/load game**: Persist state to file
- **Combat system**: Turn-based battles with strategy
- **Crafting**: Combine items to make new ones
- **Random events**: Procedurally generated encounters

### Polish (+5 each)
- **ASCII art**: Scenes, maps, or characters
- **Sound**: Terminal beep for events (or describe sounds vividly)
- **Color**: ANSI colors for atmosphere
- **Typing effect**: Text appears gradually

### Technical (+5 each)
- **Procedural generation**: Random dungeon layouts
- **Data-driven design**: Load story from external file
- **Parser**: Natural language input ("pick up the sword")
- **Achievement system**: Track accomplishments

### Multi-Language Bonus (+10)
- Implement core engine in a second language
- Reflect on how state management differs

---

## Getting Started

### Step 1: Concept
What kind of experience do you want to create? Write a one-paragraph pitch.

### Step 2: Map It Out
Sketch your story/game structure on paper first:
- What are the key scenes or locations?
- What choices lead where?
- What state needs to be tracked?
- How does it end?

### Step 3: Build the Core Loop
Get a minimal version working:
- Display text
- Get input
- Change state
- Repeat

### Step 4: Add Content
Expand your world:
- More scenes
- More choices
- More items/characters

### Step 5: Playtest
Have someone else play it. Watch what confuses them.

### Step 6: Polish
Add atmosphere, fix bugs, improve writing.

---

## Language Hints

### Python
```python
class Game:
    def __init__(self):
        self.current_scene = "start"
        self.inventory = []
        self.flags = {}  # story flags

    def run(self):
        while True:
            scene = SCENES[self.current_scene]
            print(scene["description"])

            choice = input("> ").lower()
            next_scene = self.process_choice(scene, choice)

            if next_scene == "END":
                break
            self.current_scene = next_scene

SCENES = {
    "start": {
        "description": "You wake up in a strange room...",
        "choices": {
            "look": "examine_room",
            "door": "try_door",
        }
    },
    # ... more scenes
}
```

### C++
```cpp
#include <map>
#include <string>
#include <vector>

struct GameState {
    std::string currentScene;
    std::vector<std::string> inventory;
    std::map<std::string, bool> flags;
};

struct Scene {
    std::string description;
    std::map<std::string, std::string> choices;
};

void runGame(GameState& state, const std::map<std::string, Scene>& scenes);
```

### Haskell
```haskell
data GameState = GameState
    { currentScene :: String
    , inventory :: [String]
    , flags :: Map String Bool
    }

data Scene = Scene
    { description :: String
    , choices :: Map String String
    }

gameLoop :: GameState -> Map String Scene -> IO ()
gameLoop state scenes = do
    let scene = scenes ! currentScene state
    putStrLn (description scene)
    input <- getLine
    -- process and recurse...
```

---

## Reflection Questions

Answer in your `REFLECTION.md`:

1. **What experience did you create and why?** What drew you to this concept?

2. **How did you structure state management?** What did you need to track?

3. **What was your design process?** How did planning on paper help (or not)?

4. **What would you add with more time?** Where could this go next?

5. **What did playtesting reveal?** Any surprises when others played?

6. **How did recursion or iteration factor in?** Where did you use loops vs recursion?

---

## Submission Checklist

- [ ] Interactive experience responds to input
- [ ] State is tracked and affects outcomes
- [ ] At least 10 scenes/decision points
- [ ] At least 3 meaningfully different endings
- [ ] Handles invalid input gracefully
- [ ] Code is organized with functions
- [ ] Git history shows development
- [ ] REFLECTION.md completed
- [ ] Ready for showcase presentation!

---

## Showcase Presentation

This is the Q1 finale! 5-7 minutes:

1. **Set the stage** (1 min) - What did you create? What's the concept?
2. **Live demo** (3-4 min) - Play through, showing different paths
3. **Behind the scenes** (1-2 min) - How does it work? What was hard?
4. **What you learned** (1 min) - Growth, surprises, next steps

---

**This is your chance to create something people will remember. Make it good!**
