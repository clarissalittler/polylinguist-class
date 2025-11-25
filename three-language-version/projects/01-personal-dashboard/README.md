# Project 1: Personal Dashboard

**Quarter 1, Weeks 2-4**
**Prerequisites:** Lessons 1-4 (Hello World through Functions)

---

## The Big Idea

Build a command-line tool that displays information **you actually care about**. This is YOUR dashboard—make it useful for YOUR life.

When you run it, you should think "I'm glad I built this."

---

## Base Requirements

Your dashboard must display **at least 3 different types of information**, and it must be **personalized to you**.

The program should:
1. Display a greeting with your name and the current date/time
2. Show at least 3 categories of useful information
3. Format output clearly and readably
4. Run without crashing

### Example Categories (pick what matters to YOU):

**Daily Life:**
- Days until an important date (birthday, graduation, vacation)
- Current weather or forecast (can be hardcoded for your city)
- Daily motivational quote (from a list you curate)
- Task reminders or to-do items

**Academic:**
- Assignment due dates and countdown
- Study session tracker
- Grade calculator
- Class schedule for today

**Health & Fitness:**
- Water intake tracker
- Step goal progress
- Workout schedule
- Sleep tracking summary

**Hobbies:**
- Reading progress (pages/books)
- Gaming stats
- Music practice log
- Project progress tracker

**Finance:**
- Budget remaining this month
- Savings goal progress
- Bill due dates

**Tech/Coding:**
- GitHub contribution streak
- Project ideas list
- Learning goals tracker

---

## Example Output

```
╔════════════════════════════════════════════════════════════╗
║           GOOD MORNING, ALEX! ☀️                           ║
║           Tuesday, October 15, 2024                        ║
╚════════════════════════════════════════════════════════════╝

📅 COUNTDOWN
   → 12 days until Fall Break
   → 47 days until my birthday
   → 68 days until end of quarter

📚 ACADEMICS
   → CS 101 Assignment due in 3 days
   → Study group tomorrow at 4pm
   → Current GPA: 3.7

💪 FITNESS
   → Workout today: Upper body
   → This week: 2/4 workouts done
   → Water: Remember to drink more!

💡 TODAY'S QUOTE
   "The only way to do great work is to love what you do."
   - Steve Jobs

────────────────────────────────────────────────────────────
Have a great day! 🚀
```

Your dashboard will look different—and it should! Make it yours.

---

## Technical Requirements

### Input/Output
- Program runs from command line
- Output goes to terminal (stdout)
- May read from configuration/data files (optional)

### Code Structure
- Use functions to organize code (at least 3 functions)
- Use variables appropriately
- Include comments explaining your logic
- Handle basic errors gracefully (file not found, etc.)

### Version Control
- Initialize a git repository
- Make at least 5 commits showing progress
- Write meaningful commit messages

---

## Creative Extensions

Want to push further? Try these (or invent your own!):

### Data & Persistence (+5 each)
- **Read from files**: Store your data in text/JSON files
- **Configuration file**: Let users customize what shows
- **Track over time**: Save daily data and show trends
- **Multiple users**: Support different profiles

### Visual Polish (+5 each)
- **ASCII art**: Add visual flair with text art
- **Colors**: Use ANSI colors for terminal output
- **Box drawing**: Create nice borders and sections
- **Dynamic width**: Adapt to terminal size

### Real Data (+5 each)
- **Weather API**: Fetch real weather data
- **Calendar integration**: Read from a calendar file
- **System info**: Show CPU, memory, disk usage
- **Time zones**: Show time in multiple locations

### Interactivity (+5 each)
- **Menu system**: Navigate between views
- **Add/edit items**: Modify data from the dashboard
- **Refresh**: Update data without restarting

### Multi-Language Bonus (+10)
- Implement in a second core language
- Compare the experience in your reflection

---

## Getting Started

### Step 1: Brainstorm
What information would make YOUR morning better? Write down 5-10 ideas.

### Step 2: Pick Your Categories
Choose 3-4 categories that you'll actually use.

### Step 3: Start Simple
Get "Hello, [Your Name]!" working first. Then add one category at a time.

### Step 4: Make It Pretty
Once it works, make it look good.

### Step 5: Extend
If you have time, add creative extensions.

---

## Language Hints

### Python
```python
from datetime import datetime, date

def main():
    print_header()
    print_countdown_section()
    print_quote_section()
    # ... more sections

def days_until(target_date):
    today = date.today()
    delta = target_date - today
    return delta.days

def print_header():
    now = datetime.now()
    print(f"Good morning! Today is {now.strftime('%A, %B %d, %Y')}")
```

### C++
```cpp
#include <iostream>
#include <ctime>
#include <string>

void printHeader() {
    time_t now = time(0);
    std::cout << "Dashboard - " << ctime(&now);
}

int daysUntil(int year, int month, int day) {
    // Calculate days until target date
}
```

### Haskell
```haskell
import Data.Time

main :: IO ()
main = do
    today <- utctDay <$> getCurrentTime
    putStrLn $ "Today is: " ++ show today
    printCountdown today
    printQuote

printCountdown :: Day -> IO ()
printCountdown today = do
    let daysLeft = diffDays targetDate today
    putStrLn $ "Days until event: " ++ show daysLeft
```

---

## Reflection Questions

Answer these in your `REFLECTION.md`:

1. **What did you choose to display and why?** What makes this dashboard useful for YOU specifically?

2. **What was the hardest part?** How did you work through it?

3. **What would you add with more time?** What extensions interest you?

4. **Which language did you use and why?** If you tried multiple, what differences did you notice?

5. **What did you learn that surprised you?** Any "aha" moments?

---

## Submission Checklist

- [ ] Dashboard displays at least 3 categories of information
- [ ] Output is personalized to you (not generic)
- [ ] Code uses at least 3 functions
- [ ] Code has meaningful comments
- [ ] Git repo with 5+ commits
- [ ] REFLECTION.md completed
- [ ] Ready to present at showcase!

---

## Showcase Presentation

You'll have 5-7 minutes to:
1. **Demo your dashboard** (2-3 min) - Show it running, explain what it displays
2. **Explain your choices** (2 min) - Why these categories? What makes it useful?
3. **Share what you learned** (1-2 min) - Challenges, surprises, growth

---

**Remember: This is YOUR dashboard. Make something you'll actually want to run every morning!**
