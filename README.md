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
└── README.md           # This file
```

## Quick Start

Each kata has its own virtual environment and dependencies. To run any kata:

```bash
# Navigate to the kata directory
cd fizz_buzz  # or leap_year, fibonacci, or stats_calculator

# Activate virtual environment
source venv/bin/activate

# Run tests
pytest -v

# Run tests with coverage
pytest --cov=src --cov-report=term-missing

# Run tests in watch mode
ptw
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

## Presentation Materials

Each kata includes:
- **PowerPoint presentation** - Ready for meetups/teaching
- **PDF guide** - Detailed walkthrough
- **Presentation script** - Speaking notes and Q&A
- **Summary document** - Journey breakdown

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

## Statistics

| Kata | Tests | Coverage | Commits | Lines of Code |
|------|-------|----------|---------|---------------|
| FizzBuzz | 10 | 100% | 10+ | ~20 |
| Leap Year | 13 | 100% | 7 | 8 |
| Fibonacci | 9 | 100% | 5 | 7 |
| Stats Calculator | 10 | 100% | 10 | 11 |
| **Total** | **42** | **100%** | **32+** | **~46** |

## Technologies Used

- **Language**: Python 3.10+
- **Testing**: pytest, pytest-cov, pytest-watch
- **Linting**: flake8
- **Git Hooks**: pre-commit
- **Presentation**: python-pptx, reportlab

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

**Generated with**: [Claude Code](https://claude.com/claude-code)  
**Last Updated**: November 2025
