# Leap Year Kata (Python)

A TDD implementation of the Leap Year kata using Python and pytest.

## Problem Statement

Prior to 1582, the Julian Calendar was in wide use and defined leap years as every year divisible by 4. However, it was found in the late 16th century that the calendar year had drifted from the solar year by approximately 10 days. The Gregorian Calendar was defined in order to thin out the number of leap years and to more closely align the calendar year with the solar year.

### User Story

As a user, I want to know if a year is a leap year, so that I can plan for an extra day on February 29th during those years.

### Acceptance Criteria

1. All years divisible by 400 ARE leap years (e.g., 2000)
2. All years divisible by 100 but not by 400 are NOT leap years (e.g., 1700, 1800, 1900, 2100)
3. All years divisible by 4 but not by 100 ARE leap years (e.g., 2008, 2012, 2016)
4. All years not divisible by 4 are NOT leap years (e.g., 2017, 2018, 2019)

## Setup

### 1. Create and Activate Virtual Environment

```bash
# Create virtual environment
python3 -m venv venv

# Activate virtual environment
source venv/bin/activate
```

### 2. Install Dependencies

```bash
pip install -e ".[dev]"
```

This will install:
- pytest (testing framework)
- pytest-cov (coverage reporting)
- pytest-watch (watch mode for auto-rerunning tests)
- flake8 (linting)
- pre-commit (git hooks for code quality)

### 3. Install Pre-commit Hooks

```bash
pre-commit install
```

## Project Structure

```
leap_year/
├── src/                          # Source code directory
│   ├── __init__.py
│   ├── leap_year.py              # Your implementation goes here
│   └── tests/                    # Test files
│       ├── __init__.py
│       └── test_leap_year.py     # Your tests go here
├── venv/                         # Virtual environment (gitignored)
├── .flake8                       # Flake8 linting configuration
├── .pre-commit-config.yaml       # Pre-commit hooks configuration
├── pyproject.toml                # Project configuration
├── .gitignore                    # Git ignore rules
├── CLAUDE.md                     # AI pairing guide
└── README.md                     # This file
```

## Code Quality & Testing

This project uses **pre-commit hooks** to automatically run flake8 (linting) and pytest (tests) before every commit. This ensures code quality and prevents broken code from being committed.

### Running Tests

#### Run all tests
```bash
pytest
```

#### Run tests with coverage
```bash
pytest --cov=src --cov-report=term-missing
```

#### Run tests in watch mode (auto-rerun on file changes)
```bash
ptw
```

#### Run a specific test file
```bash
pytest src/tests/test_leap_year.py
```

#### Run a specific test function
```bash
pytest src/tests/test_leap_year.py::test_2017_is_not_leap_year
```

#### Run tests with verbose output
```bash
pytest -v
```

### Linting

#### Check code style with flake8
```bash
flake8 src/
```

#### Run all pre-commit checks manually
```bash
pre-commit run --all-files
```

### Pre-commit Hooks

Pre-commit hooks are automatically installed and will run before each commit:
- Trailing whitespace removal
- End-of-file fixer
- YAML/TOML validation
- Large file checker
- Merge conflict checker
- **flake8** - Python linting
- **pytest** - All tests must pass

If any check fails, the commit will be blocked. Fix the issues and try again.

## TDD Workflow

Follow the Red-Green-Refactor cycle as described in [CLAUDE.md](CLAUDE.md):

1. **Red**: Write a failing test
2. **Green**: Write the simplest code to make it pass
3. **Refactor**: Improve the code without changing behavior
4. **Commit**: Commit with descriptive message documenting the phase
5. **Repeat**

### Leap Year Kata Phases

The kata progresses through 4 phases, each building on the previous:

#### Phase A: Non-leap years (not divisible by 4)
- Test 2017→False: Start with hardcoded return value
- Test 2018→False: Confirm logic
- Test 2019→False: **Triangulation** - confirms False is correct
- **Learning**: Fake it till you make it with the simplest case

#### Phase B: Basic leap years (divisible by 4 but not by 100)
- Test 2016→True: Add modulo 4 check (`year % 4 == 0`)
- Test 2012→True: **Triangulation** - confirms logic is general
- Test 2008→True: **Triangulation** - further confirmation
- **Learning**: Build incrementally, one behavior at a time

#### Phase C: Century years NOT divisible by 400
- Test 1900→False: **Initial failure** - returned True
- **Fix**: Check century case (`year % 100 == 0 and year % 400 != 0`)
- Test 1800→False: **Triangulation** - confirms logic works
- Test 1700→False: **Triangulation** - further confirmation
- Test 2100→False: **Future case** - confirms logic holds
- **Learning**: Order matters! Handle exceptions before general rules

#### Phase D: Century years divisible by 400
- Test 2000→True: Add 400-year check first
- Test 2400→True: **Triangulation** - confirms logic
- Test 1600→True: **Triangulation** - further confirmation
- **Learning**: Most specific rules should be checked first

### Git Commit History

Review the TDD progression:
```bash
git log --oneline
```

Each commit shows:
- Which phase was implemented
- Which tests were added
- How production code evolved
- Red-Green-Refactor details

## Quick Start

1. Create and activate the virtual environment:
   ```bash
   python3 -m venv venv
   source venv/bin/activate
   ```

2. Install dependencies:
   ```bash
   pip install -e ".[dev]"
   ```

3. Install pre-commit hooks:
   ```bash
   pre-commit install
   ```

4. Create your test file and implementation file (or use existing templates)

5. Run tests in watch mode to get instant feedback:
   ```bash
   ptw
   ```

6. Follow the step-by-step guide in [CLAUDE.md](CLAUDE.md)

## Useful Commands

```bash
# Deactivate virtual environment
deactivate

# Run tests with minimal output
pytest -q

# Run tests and stop at first failure
pytest -x

# Run tests matching a pattern
pytest -k "test_century"

# Show local variables in tracebacks
pytest -l

# Skip pre-commit hooks for a single commit (not recommended)
git commit --no-verify -m "message"

# Update pre-commit hooks to latest versions
pre-commit autoupdate
```

## Git Configuration

This repository should be configured to use your personal GitHub account:
- Email: tomspencerlondon@gmail.com
- Account: TomSpencerLondon

See the parent directory's GITHUB_SETUP.md for configuration details.

To configure this repository locally:

```bash
git config user.name "Tom Spencer"
git config user.email "tomspencerlondon@gmail.com"
```

Verify configuration:
```bash
git config user.email
git config user.name
```

## The Solution

After completing the TDD process, you'll arrive at a clean solution like:

```python
def is_leap_year(year):
    """
    Determine if a year is a leap year according to Gregorian Calendar rules.

    Args:
        year: An integer representing the year

    Returns:
        True if the year is a leap year, False otherwise
    """
    return (year % 4 == 0 and year % 100 != 0) or (year % 400 == 0)
```

But the journey of discovering this through TDD is the real value of the kata!

## Learning Objectives

This kata teaches:
- **TDD fundamentals**: Red-Green-Refactor cycle
- **Incremental development**: Building complex logic step by step
- **Triangulation**: Using multiple tests to confirm general solutions
- **Edge case handling**: Century years and the 400-year rule
- **Boolean logic**: Combining conditions effectively
- **Refactoring**: Simplifying code while maintaining correctness

## Extension: 4000-Year Rule

The Gregorian Calendar could be made more accurate by adding an additional rule that eliminates years divisible by 4000 as leap years. Consider adding this rule as a second story as an extension to the exercise.

If you implement this extension:
- Years divisible by 4000 are NOT leap years
- Test: `is_leap_year(4000) == False`
- Test: `is_leap_year(8000) == False`

## References

- [Leap Year - Wikipedia](https://en.wikipedia.org/wiki/Leap_year)
- [Gregorian Calendar - Wikipedia](https://en.wikipedia.org/wiki/Gregorian_calendar)
- Test Driven Development by Kent Beck
