# TDD Katas Collection

A collection of Test-Driven Development (TDD) katas demonstrating the Red-Green-Refactor cycle.

## About This Repository

This repository contains multiple coding katas, each implemented using strict TDD methodology. Each kata showcases:
- **Red-Green-Refactor** cycle
- **Small, focused commits** documenting the journey
- **100% test coverage**
- **Clear documentation** for learning and teaching
- **Presentation materials** for sharing knowledge

## Katas Included

### 1. FizzBuzz Kata
**Problem**: Numbers → "Fizz" (÷3), "Buzz" (÷5), "FizzBuzz" (÷15)  
**Phases**: 5 phases (A-E)  
**Tests**: 10 passing  
**Coverage**: 100%  
**Key Learning**: Order matters - check combined rules before individual rules

📁 [View FizzBuzz →](fizz_buzz/)

### 2. Leap Year Kata
**Problem**: Determine if a year is a leap year (Gregorian Calendar)  
**Phases**: 4 phases (A-D)  
**Tests**: 13 passing  
**Coverage**: 100%  
**Key Learning**: Check most specific rules first (400 → 100 → 4)

📁 [View Leap Year →](leap_year/)

### 3. Fibonacci Kata
**Problem**: Calculate the nth Fibonacci number
**Phases**: 4 phases (A-D)
**Tests**: 9 passing
**Coverage**: 100%
**Key Learning**: Algorithm emerged from tests, not upfront design

📁 [View Fibonacci →](fibonacci/)

### 4. Stats Calculator Kata
**Problem**: Calculate statistics (min, max, count, average) for a sequence
**Phases**: 4 phases (A-D)
**Tests**: 10 passing
**Coverage**: 100%
**Key Learning**: Python built-ins handle complexity, simple is best

📁 [View Stats Calculator →](stats_calculator/)

### 5. Money Kata (Kent Beck's TDD Classic)
**Problem**: Build a multi-currency money system with exchange rates
**Chapters**: 17 chapters from Kent Beck's "TDD by Example"
**Tests**: 12 passing
**Coverage**: 93%
**Key Learning**: Design emerges from tests - Composite pattern, Expression metaphor, polymorphic reduction

**What makes this special**:
- Follow's Beck's original kata step-by-step
- Chapter-by-chapter documentation showing design evolution
- Demonstrates power of metaphor in shaping design
- Shows how duplication drives abstraction
- Complete journey from shameless duplication to clean design

📁 [View Money Kata →](money/)

### 6. 99 Bottles Kata (Sandi Metz's OOP Classic)
**Problem**: Generate the "99 Bottles of Beer" song with proper OO design
**Chapters**: 9 chapters from Sandi Metz's "99 Bottles of OOP"
**Tests**: 12 passing (7 unit + 3 integration + 2 generic)
**Language**: Ruby
**Key Learning**: Shameless duplication → Refactoring → Polymorphism → Context independence

**What makes this special**:
- Follows Sandi Metz's refactoring journey step-by-step
- Auto-registering factory pattern with Ruby's `inherited` hook
- Dependency Inversion and Law of Demeter in action
- Generic CountdownSong abstraction (works for any countdown song!)
- Complete sequence diagrams showing class interactions
- Journey from 1 monolithic class → 7 well-designed classes

**Chapters covered**:
- Ch 1-2: Shameless Green (get it working first)
- Ch 3-5: Remove duplication, extract classes, achieve polymorphism
- Ch 6: Extract BottleNumber hierarchy
- Ch 7: Auto-registering factory (Open/Closed Principle)
- Ch 8: Dependency Inversion, Law of Demeter
- Ch 9: Proper testing, context independence

📁 [View 99 Bottles →](99_bottles/)

## TDD Principles Demonstrated

Across all katas, you'll see these principles in action:

1. **Red-Green-Refactor**: Write failing test → Make it pass → Improve code
2. **Fake it till you make it**: Start with hardcoded values
3. **Triangulation**: Multiple tests confirm general solutions
4. **Small steps**: One test at a time, minimal code changes
5. **Incremental complexity**: Build from simple to complex
6. **Frequent commits**: Clear history showing progression
7. **Algorithm discovery**: Let tests drive the design

## Repository Structure

```
katas/
├── fizz_buzz/           # FizzBuzz kata
│   ├── src/
│   ├── README.md
│   ├── CLAUDE.md        # TDD guide
│   ├── presentation materials
│   └── git history (10+ commits)
│
├── leap_year/           # Leap Year kata
│   ├── src/
│   ├── README.md
│   ├── CLAUDE.md        # TDD guide
│   ├── presentation materials
│   └── git history (7 commits)
│
├── fibonacci/           # Fibonacci kata
│   ├── src/
│   ├── README.md
│   ├── CLAUDE.md        # TDD guide
│   ├── presentation materials
│   └── git history (5 commits)
│
├── stats_calculator/    # Stats Calculator kata
│   ├── src/
│   ├── README.md
│   ├── CLAUDE.md        # TDD guide
│   ├── STATISTICS_EXPLAINED.md  # Mean vs Median vs Mode
│   ├── presentation materials
│   └── git history (10 commits)
│
├── money/               # Money kata (Kent Beck's classic)
│   ├── src/
│   ├── beck-chapters/   # Beck's original chapter text
│   ├── README.md        # Chapter-by-chapter guide
│   ├── PRESENTATION_SUMMARY.md  # Complete presentation deck
│   └── git history (17+ commits showing design evolution)
│
├── 99_bottles/          # 99 Bottles kata (Sandi Metz's classic)
│   ├── lib/             # Ruby source code
│   ├── test/            # Minitest unit and integration tests
│   ├── book/            # Sandi Metz's chapter text
│   ├── README.md        # Chapter-by-chapter guide with sequence diagrams
│   └── git history (chapters 1-9)
│
└── README.md           # This file
```

## Quick Start

### Python Katas (FizzBuzz, Leap Year, Fibonacci, Stats Calculator, Money)

```bash
# Navigate to the kata directory
cd fizz_buzz  # or leap_year, fibonacci, stats_calculator, or money

# Activate virtual environment
source venv/bin/activate

# Run tests
pytest -v

# Run tests with coverage
pytest --cov=src --cov-report=term-missing

# Run tests in watch mode
ptw
```

### Ruby Kata (99 Bottles)

```bash
# Navigate to the kata directory
cd 99_bottles

# Run tests
ruby test/bottles_test.rb
ruby test/bottle_verse_test.rb
ruby test/countdown_song_test.rb

# Or run all tests
for f in test/*_test.rb; do ruby "$f"; done
```

## Git History

Each kata has a clean commit history showing the TDD journey:

```bash
# View commit history for any kata
cd fizz_buzz && git log --oneline

# View detailed commit for learning
git show <commit-hash>

# See the full diff history
git log -p
```

## Learning Path

**Recommended order**:
1. **FizzBuzz** - Learn basic TDD flow and rule ordering
2. **Leap Year** - Practice with conditional logic and edge cases
3. **Fibonacci** - Experience algorithm discovery through TDD
4. **Stats Calculator** - Master Python built-ins and simple solutions
5. **Money** - Advanced kata: design patterns, refactoring, metaphor power
6. **99 Bottles** - Master kata: refactoring journey, polymorphism, context independence

## Presentation Materials

**Python katas** (FizzBuzz, Leap Year, Fibonacci, Stats Calculator) include:
- **PowerPoint presentation** - Ready for meetups/teaching
- **PDF guide** - Detailed walkthrough
- **Presentation script** - Speaking notes and Q&A
- **Summary document** - Journey breakdown

**Ruby/Advanced katas** (Money, 99 Bottles) include:
- **Chapter-by-chapter documentation** - Following the original books
- **Sequence diagrams** (99 Bottles) - Visual class interaction flows
- **Detailed README** - Complete journey documentation
- **Code comments** - Inline explanations of design decisions

Perfect for:
- Teaching TDD to teams
- Presenting at meetups
- Code review discussions
- Personal study and practice

## Key Learnings Across All Katas

### FizzBuzz
- **Order matters**: Check combined conditions before individual ones
- **Refactoring safe**: With tests, we can confidently improve code
- **Multiple approaches**: Discovered if-else and string concatenation methods

### Leap Year
- **Specific → General**: Check 400 before 100 before 4
- **Explicit > Implicit**: Make rules clear, even if tests pass
- **Edge cases**: Century years revealed the need for additional rules

### Fibonacci
- **Algorithm discovery**: Didn't design upfront - emerged from tests
- **Efficiency naturally**: Simplest code was also most efficient (O(n), O(1))
- **Iterative > Recursive**: TDD led us to the optimal solution

### Stats Calculator
- **Python built-ins**: min(), max(), sum(), len() handle everything
- **Edge cases for free**: Built-in functions handle negatives, duplicates automatically
- **Simple is best**: 4 statements of logic, maximum clarity

### Money (Kent Beck's Classic)
- **Metaphor shapes design**: Expression metaphor led to cleaner code than 20+ previous attempts
- **Duplication drives abstraction**: Shameless copy-paste → gradual extraction → clean design
- **Polymorphism beats conditionals**: No type checking, just polymorphic dispatch
- **Design emerges**: Composite pattern emerged from tests, not planned upfront
- **Refactoring with confidence**: Tests enabled deleting entire classes safely

### 99 Bottles (Sandi Metz's Classic)
- **Shameless Green first**: Get it working with duplication, refactor later
- **Auto-registering factory**: Ruby's `inherited` hook enables Open/Closed Principle
- **Dependency Inversion**: Depend on abstractions (verse_template role), not concretions
- **Law of Demeter**: Only talk to direct collaborators, use forwarding methods
- **Context independence**: CountdownSong works for ANY countdown song, not just bottles
- **Power of naming**: Bottles → CountdownSong revealed hidden generality

## Statistics

| Kata | Tests | Coverage | Commits | Lines of Code | Language |
|------|-------|----------|---------|---------------|----------|
| FizzBuzz | 10 | 100% | 10+ | ~20 | Python |
| Leap Year | 13 | 100% | 7 | 8 | Python |
| Fibonacci | 9 | 100% | 5 | 7 | Python |
| Stats Calculator | 10 | 100% | 10 | 11 | Python |
| Money | 12 | 93% | 17+ | ~60 | Python |
| 99 Bottles | 12 | 100% | 9+ | ~145 | Ruby |
| **Total** | **66** | **~99%** | **58+** | **~251** | **2 languages** |

## Technologies Used

### Python Katas
- **Language**: Python 3.10+
- **Testing**: pytest, pytest-cov, pytest-watch
- **Linting**: flake8
- **Git Hooks**: pre-commit
- **Presentation**: python-pptx, reportlab

### Ruby Kata
- **Language**: Ruby 2.6+
- **Testing**: Minitest (built-in)
- **Book**: "99 Bottles of OOP" by Sandi Metz, Katrina Owen, and TJ Stankus

## Contributing

This repository is for personal learning and portfolio purposes. However, if you find it helpful:
- ⭐ Star the repository
- 🍴 Fork it and try the katas yourself
- 📝 Share your own TDD journey
- 💬 Open an issue for discussion

## Author

**Tom Spencer**
- Email: tomspencerlondon@gmail.com
- GitHub: [@TomSpencerLondon](https://github.com/TomSpencerLondon)

## License

This project is open source and available for educational purposes.

## Acknowledgments

- Inspired by Kent Beck's "Test Driven Development: By Example"
- Kata problems from cyber-dojo.org and coding dojo community
- Created with assistance from Claude Code

---

**Last Updated**: November 2025
