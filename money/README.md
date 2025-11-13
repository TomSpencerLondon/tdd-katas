# Money Kata - Kent Beck's TDD by Example

This kata follows Kent Beck's seminal book "Test-Driven Development: By Example" - specifically Part I: The Money Example (Chapters 1-17).

## The Problem

Build a multi-currency money system that can:
- Multiply amounts by numbers (e.g., 5 USD * 2 = 10 USD)
- Add amounts in different currencies with exchange rates
- Convert between currencies

### Example Scenario

```
$5 + 10 CHF = $10 (if exchange rate is 2:1)
$5 * 2 = $10
```

## Kent Beck's Approach

This implementation follows Beck's TDD methodology exactly as presented in his book:
1. **Write a test** - Imagine the perfect API
2. **Make it compile** - Add stubs, do the minimum
3. **Make it run** - Get to green quickly (even with "sins")
4. **Make it right** - Refactor to remove duplication
5. **Repeat** - Small steps, one test at a time

## Chapters Implemented

Each chapter represents a specific TDD cycle:

- **Chapter 1**: Multi-Currency Money - Basic multiplication
- **Chapter 2**: Degenerate Objects - Return new objects, not side effects
- **Chapter 3**: Equality for All - Implement equals()
- **Chapter 4**: Privacy - Make amount private
- **Chapter 5**: Franc-ly Speaking - Duplicate Dollar for Franc
- **Chapter 6**: Equality for All, Redux - Compare Dollars to Francs
- **Chapter 7**: Apples and Oranges - Prevent mixed currency comparison
- **Chapter 8**: Makin' Objects - Introduce factory methods
- **Chapter 9**: Times We're Livin' In - Extract superclass Money
- **Chapter 10**: Interesting Times - Implement times() in Money
- **Chapter 11**: The Root of All Evil - Eliminate subclasses
- **Chapter 12**: Addition, Finally - Implement addition
- **Chapter 13**: Make It - Handle mixed currency addition
- **Chapter 14**: Change - Implement currency conversion
- **Chapter 15**: Mixed Currencies - Handle complex expressions
- **Chapter 16**: Abstraction, Finally - Clean up abstractions
- **Chapter 17**: Money Retrospective - Review and reflect

## Setup

### 1. Create Virtual Environment

```bash
cd money
python3 -m venv venv
source venv/bin/activate
```

### 2. Install Dependencies

```bash
pip install -e ".[dev]"
```

### 3. Install Pre-commit Hooks

```bash
pre-commit install
```

## TDD Workflow

Follow Kent Beck's Red-Green-Refactor cycle:

1. **Red**: Write a failing test
2. **Green**: Make it pass (commit sins if needed!)
3. **Refactor**: Remove duplication
4. **Commit**: Document the journey
5. **Repeat**: Next test from the to-do list

## Quick Start

```bash
source venv/bin/activate
ptw  # Watch mode
```

Follow [CLAUDE.md](CLAUDE.md) for the step-by-step TDD guide through all 17 chapters.

## Expected Results

- **Tests**: ~30+ passing (one or more per chapter)
- **Coverage**: 100%
- **Commits**: ~20+ (documenting the TDD journey)
- **Final Design**: Clean multi-currency money system with exchange rates

## Key Learnings

- **Small steps**: TDD enables tiny, safe increments
- **Duplication drives design**: Eliminating duplication reveals abstractions
- **Tests as documentation**: Each test tells a story
- **Refactoring with confidence**: Green bar enables bold refactoring
- **Design emerges**: Don't plan upfront, let tests guide you

## Technologies

- Python 3.10+
- pytest (testing framework)
- pytest-cov (coverage)
- pytest-watch (watch mode)

---

**Based on**: "Test Driven Development: By Example" by Kent Beck
**Generated with**: [Claude Code](https://claude.com/claude-code)
**Last Updated**: November 2025
**Author**: Tom Spencer
