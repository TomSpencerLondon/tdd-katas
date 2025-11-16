# Pharo Counter TDD Exercise

A Test-Driven Development exercise implementing a Counter class in Pharo Smalltalk, following Chapter 4 from "Pharo by Example".

## About Pharo

Pharo is a pure object-oriented programming language in the Smalltalk tradition. It offers a unique development experience with:
- Live programming and debugging
- Image-based development (save the entire state)
- Powerful IDE tools built into the environment
- Extreme TDD - code directly in the debugger
- Rich set of libraries and frameworks

## Learning Resources

### MOOC (Massive Open Online Courses)

**Pharo MOOC** - An excellent free online course covering Pharo fundamentals:
- Website: http://mooc.pharo.org
- Great testimonies - fun and engaging learning experience
- Perfect starting point for learning Pharo

**Advanced Object-Oriented Design MOOC** - For deeper design patterns and principles:
- Website: https://advanced-design-mooc.pharo.org
- Covers advanced OO design and development with Pharo
- Recommended after completing the basic MOOC

### Books

**Free Pharo Books** - Available at http://books.pharo.org

**Pharo by Example** - The recommended starting point:
- Based on Pharo 9.0
- GitHub: https://github.com/SquareBracketAssociates/PharoByExample9
- Available on Amazon: [Pharo 9 by Example](https://www.amazon.com/dp/B09XQWQFBP)
- Great hands-on introduction to Pharo

**Contributing** - Many Pharo books and booklets are open source:
- Repository: https://github.com/SquareBracketAssociates
- Community contributions welcome

## This Exercise: Counter Tutorial

This project follows the Counter exercise from Chapter 4 of "Pharo by Example", demonstrating:
- TDD workflow in Pharo
- Creating classes with instance variables (slots)
- Writing SUnit tests
- Using the System Browser
- Live coding and debugging

### The Counter Class

A simple counter that can:
- Increment and decrement
- Track a count value
- Reset to a starting value

## Setup Instructions

### 1. Install Pharo Launcher
- Download from https://pharo.org/download
- Launch the Pharo Launcher application

### 2. Create New Pharo Image
1. Open Pharo Launcher
2. Click "New" to create a new image
3. Select Pharo version (9.0 or later recommended)
4. Name it: `Counter-TDD`
5. Launch the image

### 3. Create the Counter Package
1. Open System Browser: `Cmd-O, B` (or `Ctrl-O, B` on Windows/Linux)
2. Right-click in the package pane → "New Package"
3. Name it: `MyCounter`

### 4. Create the Counter Class
In the System Browser, with `MyCounter` package selected, define:

```smalltalk
Object << #Counter
    slots: { #count };
    package: 'MyCounter'
```

Accept the definition (Cmd-S or Ctrl-S)

### 5. Create Tests
1. Create test class:
```smalltalk
TestCase << #CounterTest
    slots: { };
    package: 'MyCounter'
```

2. Add test methods (see counter.pdf for full examples)

### 6. Run Tests
- Open Test Runner: `Cmd-O, U` (or `Ctrl-O, U`)
- Find `MyCounter` package
- Run tests and see them turn green

## Keyboard Shortcuts (Pharo)

- `Cmd-O, B` - Open System Browser
- `Cmd-O, W` - Open Playground
- `Cmd-O, U` - Open Test Runner
- `Cmd-O, I` - Open Iceberg (Git)
- `Cmd-S` - Accept/Save code
- `Cmd-D` - Do it (execute selected code)
- `Cmd-P` - Print it (execute and print result)
- `Cmd-I` - Inspect it (execute and open inspector)

## Development Workflow

### Option 1: Work in Pharo, Export for Git
1. Write code in System Browser
2. Write tests and run them
3. When ready to commit:
   - Right-click package → "Export as Tonel"
   - Choose `MyCounter/` directory
   - Commit exported files to git

### Option 2: Use Iceberg (Advanced)
1. Open Iceberg (`Cmd-O, I`)
2. Add existing repository
3. Load package from repository
4. Work with full git integration

## File Structure

```
pharo/
├── README.md              # This file
├── counter.pdf            # Tutorial reference (Chapter 4)
└── MyCounter/             # Exported Tonel files
    ├── Counter.class.st   # Counter class definition
    └── CounterTest.class.st  # Test class
```

## Running Tests

1. In Pharo: Open Test Runner (`Cmd-O, U`)
2. Find `MyCounter` in the package list
3. Click "Run Selected"
4. All tests should be green

## Expected Results

By the end of this exercise, you will have:
- A working Counter class with increment/decrement functionality
- Complete test coverage using SUnit
- Hands-on experience with Pharo's live programming
- Understanding of TDD workflow in Smalltalk
- Experience with the System Browser and Test Runner

## Key Learnings

- **Live Programming**: Change code while it's running, see immediate results
- **Image-based Development**: Your entire working state is saved
- **Pure OO**: Everything is an object, even classes
- **Message Passing**: Objects communicate via messages
- **TDD in Practice**: Write test first, see it fail, make it pass, refactor
- **Extreme TDD**: Code directly in the debugger when tests fail

## Next Steps

After completing the Counter exercise:
1. Take the Pharo MOOC for comprehensive learning
2. Read more chapters from "Pharo by Example"
3. Try building more complex applications
4. Explore the Advanced Design MOOC
5. Contribute to the Pharo community

## References

- Tutorial: `counter.pdf` (Chapter 4 from Pharo by Example)
- Pharo Website: https://pharo.org
- MOOC: http://mooc.pharo.org
- Advanced MOOC: https://advanced-design-mooc.pharo.org
- Books: http://books.pharo.org
- Pharo by Example 9: https://github.com/SquareBracketAssociates/PharoByExample9

---

**Created**: November 2025
**Tutorial Source**: Pharo by Example, Chapter 4
