# Project 6: Creative Tool

**Quarter 2, Weeks 8-11**
**Prerequisites:** Lessons 1-11 (through Error Handling)

---

## The Big Idea

Build a tool that **enables creativity**. Not something that's creative itself, but something that helps people create: art, music, writing, designs, games, or anything else.

The best creative tools disappear—they get out of the way and let the creator focus on creating.

---

## Base Requirements

Your creative tool must:

1. **Enable creation** of something (art, music, text, designs, etc.)
2. **Respond to user input** in real-time or near-real-time
3. **Produce output** the user can appreciate or use
4. **Be approachable** - someone can start creating within minutes
5. **Allow growth** - depth for those who want to explore

### Core Features
- Clear interface for creation
- Immediate feedback on actions
- Ability to see/hear/use the result
- Some form of export or save

---

## Project Ideas

### Visual Arts
- **ASCII art editor**: Draw with characters
- **Pattern generator**: Algorithmic designs
- **Pixel art tool**: Simple grid-based drawing
- **Banner maker**: Text with decorative borders
- **Diagram creator**: Boxes and arrows
- **Mandala generator**: Symmetric pattern creator

### Music & Sound
- **Drum machine**: Pattern-based beats
- **Melody maker**: Simple note sequencer
- **Chord progression tool**: Build harmonies
- **Sound effect generator**: Procedural audio descriptions
- **Music theory helper**: Scales, chords, suggestions

### Writing
- **Story prompt generator**: Random creative sparks
- **Character creator**: Generate character details
- **World builder**: Create settings, maps, lore
- **Poetry assistant**: Rhyme suggestions, meter helper
- **Dialogue generator**: Conversation starters
- **Plot structure tool**: Story beats, arc templates

### Games & Interactive
- **Level designer**: Create game levels/maps
- **Card creator**: Design game cards
- **Character sheet generator**: RPG characters
- **Random encounter builder**: D&D-style generators
- **Quest generator**: Story hooks and missions

### Design & Practical
- **Color palette creator**: Generate harmonious colors
- **Font sampler**: Preview text in styles
- **Layout sketcher**: Wireframe creator
- **Recipe remixer**: Combine ingredients creatively
- **Playlist builder**: Music selection assistant

### Code & Technical
- **Regex builder**: Visual regex construction
- **API mock creator**: Generate fake data
- **Test case generator**: Create test scenarios
- **Documentation template**: Structure creator

---

## Example: ASCII Art Editor

```
═══════════════════════════════════════════════════════════════
              ASCII ART STUDIO v1.0
═══════════════════════════════════════════════════════════════

CANVAS (20x10):
┌────────────────────┐
│    ██████████      │
│  ██          ██    │
│ █    ○    ○    █   │
│ █              █   │
│ █    ╲____╱    █   │
│  █            █    │
│   ██        ██     │
│     ████████       │
│                    │
│                    │
└────────────────────┘

CURRENT: █  POSITION: (10, 4)

TOOLS: [d]raw [e]rase [l]ine [r]ect [c]ircle [f]ill
CHARS: 1=█ 2=░ 3=▒ 4=▓ 5=○ 6=● 7=─ 8=│ 9=custom
ACTIONS: [s]ave [o]pen [n]ew [u]ndo [q]uit

> _
```

---

## Example: Story Prompt Generator

```
═══════════════════════════════════════════════════════════════
              STORY SPARK ✨
═══════════════════════════════════════════════════════════════

GENERATE:
  [1] Character
  [2] Setting
  [3] Conflict
  [4] Plot twist
  [5] Opening line
  [6] Full prompt (all of the above)
  [7] Genre-specific prompt

> 6

STORY PROMPT
════════════

CHARACTER:
  A retired astronaut who can no longer dream

SETTING:
  A lighthouse that appears on different coastlines each night

CONFLICT:
  They must deliver a message to someone who doesn't want to
  be found, before the next full moon

PLOT TWIST:
  The message is from their own future self

OPENING LINE:
  "The last time I saw Earth from space, I made a promise
  I've spent eleven years trying to break."

────────────────────────────────────────────────────────────
ACTIONS: [g]enerate new [s]ave [l]ock element [c]ustomize

> lock character

CHARACTER [LOCKED]:
  A retired astronaut who can no longer dream

Generating new elements around locked character...
```

---

## Example: Chord Progression Builder

```
═══════════════════════════════════════════════════════════════
              CHORD LAB 🎵
═══════════════════════════════════════════════════════════════

KEY: C Major                    TEMPO: 120 BPM

PROGRESSION (4 bars):
┌─────────┬─────────┬─────────┬─────────┐
│   C     │   Am    │   F     │   G     │
│  I      │  vi     │  IV     │  V      │
├─────────┼─────────┼─────────┼─────────┤
│ C-E-G   │ A-C-E   │ F-A-C   │ G-B-D   │
└─────────┴─────────┴─────────┴─────────┘

FEELING: Nostalgic, hopeful

SIMILAR PROGRESSIONS:
  • I-V-vi-IV (Pop standard)
  • I-vi-ii-V (Jazz turnaround)
  • I-IV-V-I  (Classic rock)

SUGGESTED NEXT CHORDS:
  After G: C (resolve), Am (emotional), Em (surprise)

COMMANDS:
  [a]dd chord  [r]emove  [c]hange key  [s]uggest
  [p]lay (export MIDI)  [v]ariations  [t]empo

> suggest emotional

SUGGESTIONS for more emotional feel:
  • Replace Am with A7 (dominant tension)
  • Add Dm before G (ii-V motion)
  • Try F -> Fm for melancholy
  • End on Am instead of resolving to C

> add Dm at position 3

PROGRESSION (5 bars):
┌─────────┬─────────┬─────────┬─────────┬─────────┐
│   C     │   Am    │   Dm    │   F     │   G     │
│  I      │  vi     │  ii     │  IV     │  V      │
└─────────┴─────────┴─────────┴─────────┴─────────┘

Nice! The ii chord adds forward motion.
```

---

## Technical Requirements

### User Experience
- Minimal learning curve to start
- Clear feedback on every action
- Undo capability (if modifications are possible)
- Help available but not intrusive

### Responsiveness
- Actions should feel immediate
- Visual/textual feedback for every input
- No long waits without progress indication

### Output
- Ability to see/use what you've created
- Some form of export (save to file, copy to clipboard)
- Output should be useful outside the tool

### Error Handling
- Graceful handling of invalid input
- Never crash on user actions
- Helpful error messages

---

## Creative Extensions

### Polish (+5 each)
- **Themes**: Multiple visual styles
- **Presets**: Starting templates
- **Gallery**: View past creations
- **Tutorial mode**: Guided introduction

### Power Features (+5 each)
- **Keyboard shortcuts**: Efficient workflows
- **Batch operations**: Apply to many items
- **History/undo**: Multi-level undo
- **Search**: Find within your creations

### Community (+5 each)
- **Export formats**: Multiple output options
- **Import**: Bring in external content
- **Sharing**: Easy-to-share output format
- **Remixing**: Build on existing work

### Advanced (+5 each)
- **Randomization controls**: Constrained randomness
- **Rules/constraints**: Limit possibilities creatively
- **Combinations**: Mix multiple elements
- **Procedural generation**: Algorithmic assistance

### Multi-Language Bonus (+10)
- Implement in a second language
- Reflect on how the languages affected design

---

## Getting Started

### Step 1: Choose Your Domain
What kind of creation do you want to enable? Pick something you care about!

### Step 2: Define the Core Action
What's the one thing users DO? Draw? Generate? Select? Build?

### Step 3: Minimum Viable Tool
Get the core loop working:
- Input → Action → Visible Result

### Step 4: Add Depth
- More options
- Better output
- Refinement controls

### Step 5: Polish the Experience
- Clear instructions
- Smooth workflow
- Satisfying feedback

---

## Language Hints

### Python
```python
class CreativeTool:
    def __init__(self):
        self.state = self.initial_state()
        self.history = []

    def run(self):
        self.show_welcome()
        while True:
            self.display()
            action = self.get_input()
            if action == 'quit':
                break
            self.save_history()
            self.apply_action(action)

    def undo(self):
        if self.history:
            self.state = self.history.pop()

class ASCIICanvas:
    def __init__(self, width, height):
        self.grid = [[' '] * width for _ in range(height)]

    def draw(self, x, y, char):
        if 0 <= x < len(self.grid[0]) and 0 <= y < len(self.grid):
            self.grid[y][x] = char

    def display(self):
        for row in self.grid:
            print(''.join(row))
```

### C++
```cpp
class Canvas {
    std::vector<std::vector<char>> grid;
    std::stack<decltype(grid)> history;

public:
    Canvas(int width, int height)
        : grid(height, std::vector<char>(width, ' ')) {}

    void draw(int x, int y, char c) {
        if (x >= 0 && x < width() && y >= 0 && y < height()) {
            saveHistory();
            grid[y][x] = c;
        }
    }

    void undo() {
        if (!history.empty()) {
            grid = history.top();
            history.pop();
        }
    }
};
```

### Haskell
```haskell
data ToolState = ToolState
    { canvas :: Canvas
    , cursor :: (Int, Int)
    , currentChar :: Char
    , history :: [Canvas]
    }

data Action = Draw | Erase | Move Direction | Undo | Quit

runTool :: ToolState -> IO ()
runTool state = do
    displayCanvas (canvas state)
    action <- getAction
    case action of
        Quit -> return ()
        Undo -> runTool (undoState state)
        _    -> runTool (applyAction action state)

applyAction :: Action -> ToolState -> ToolState
applyAction Draw state = state
    { canvas = drawAt (cursor state) (currentChar state) (canvas state)
    , history = canvas state : history state
    }
```

---

## Reflection Questions

Answer in your `REFLECTION.md`:

1. **What tool did you build and why?** What creation does it enable?

2. **Who is this for?** Who would use this tool?

3. **What makes a creative tool good?** How did you apply those principles?

4. **What was the hardest UX challenge?** How do you balance power and simplicity?

5. **What would you add with more time?** Where could this grow?

6. **Did you use your own tool to create something?** What did you make?

---

## Submission Checklist

- [ ] Tool enables creation of something
- [ ] Responds immediately to input
- [ ] Output is usable/viewable
- [ ] Someone can start creating within minutes
- [ ] Error handling is graceful
- [ ] Has some form of help/instructions
- [ ] Git history shows development
- [ ] REFLECTION.md completed
- [ ] Ready for Q2 showcase!

---

## Showcase Presentation

This is the Q2 finale! 5-7 minutes:

1. **What does it create?** (1 min) - Explain the tool's purpose
2. **Live creation** (3-4 min) - Actually make something during the demo!
3. **Design decisions** (1-2 min) - What makes it easy/powerful to use?
4. **What you learned** (1 min) - Insights about tools and creativity

---

**Build something that helps others create!**
