# Leap Year Kata - Setup Instructions

This document provides a quick reference for setting up and using the Leap Year kata.

## Quick Start

### 1. Create and Activate Virtual Environment

```bash
cd leap_year
python3 -m venv venv
source venv/bin/activate
```

### 2. Install Dependencies

```bash
pip install -e ".[dev]"
```

This installs:
- pytest (testing framework)
- pytest-cov (coverage reporting)
- pytest-watch (auto-rerun tests)
- flake8 (linting)
- pre-commit (git hooks)

### 3. Install Pre-commit Hooks

```bash
pre-commit install
```

### 4. Configure Git (if not already done)

```bash
git init
git config user.name "Tom Spencer"
git config user.email "tomspencerlondon@gmail.com"
```

Verify:
```bash
git config user.email
git config user.name
```

## Project Structure

```
leap_year/
├── src/
│   ├── __init__.py
│   ├── leap_year.py              # Implementation (currently just a stub)
│   └── tests/
│       ├── __init__.py
│       └── test_leap_year.py     # Tests (currently just TODOs)
├── .flake8                       # Flake8 configuration
├── .pre-commit-config.yaml       # Pre-commit hooks
├── .gitignore                    # Git ignore rules
├── pyproject.toml                # Project configuration
├── CLAUDE.md                     # Detailed TDD guide for AI pairing
├── README.md                     # Main documentation
└── SETUP_INSTRUCTIONS.md         # This file
```

## Running Tests

```bash
# Run all tests
pytest

# Run tests in watch mode (auto-rerun on file changes)
ptw

# Run tests with coverage
pytest --cov=src --cov-report=term-missing

# Run specific test
pytest src/tests/test_leap_year.py::test_2017_is_not_leap_year
```

## TDD Workflow

Follow the Red-Green-Refactor cycle described in [CLAUDE.md](CLAUDE.md):

1. **Red**: Write one failing test
2. **Green**: Write simplest code to pass
3. **Refactor**: Improve without changing behavior
4. **Commit**: Document the phase
5. **Repeat**

### Phase Order

1. **Phase A**: Non-leap years (2017, 2018, 2019)
2. **Phase B**: Basic leap years (2016, 2012, 2008)
3. **Phase C**: Century years not divisible by 400 (1900, 1800, 1700, 2100)
4. **Phase D**: Century years divisible by 400 (2000, 2400, 1600)

## Key Commands

```bash
# Activate virtual environment
source venv/bin/activate

# Deactivate virtual environment
deactivate

# Run linter
flake8 src/

# Run pre-commit checks manually
pre-commit run --all-files

# Git commands
git add .
git commit -m "Phase A: Non-leap years"
git log --oneline
```

## Pre-commit Hooks

Automatically runs before each commit:
- Trailing whitespace removal
- End-of-file fixer
- YAML/TOML validation
- flake8 linting
- pytest (all tests must pass)

If checks fail, the commit is blocked. Fix issues and try again.

## Working with Claude Code

See [CLAUDE.md](CLAUDE.md) for detailed prompts to use with Claude Code for AI-assisted TDD pairing.

Example prompts:
- "Add a failing test: is_leap_year(2017) == False. Don't touch production code."
- "Make it green with the simplest change."
- "Add the next test that forces new behavior."

## Acceptance Criteria

Your implementation should satisfy:

1. Years divisible by 400 ARE leap years (2000 ✓)
2. Years divisible by 100 but not 400 are NOT leap years (1900 ✗)
3. Years divisible by 4 but not 100 ARE leap years (2016 ✓)
4. Years not divisible by 4 are NOT leap years (2017 ✗)

## Final Solution (Don't peek until you're done!)

<details>
<summary>Click to reveal the final solution</summary>

```python
def is_leap_year(year):
    """Determine if a year is a leap year according to Gregorian Calendar rules."""
    return (year % 4 == 0 and year % 100 != 0) or (year % 400 == 0)
```

Alternative implementation with explicit checks:
```python
def is_leap_year(year):
    """Determine if a year is a leap year according to Gregorian Calendar rules."""
    if year % 400 == 0:
        return True
    if year % 100 == 0:
        return False
    if year % 4 == 0:
        return True
    return False
```

</details>

## References

- [README.md](README.md) - Full documentation
- [CLAUDE.md](CLAUDE.md) - Detailed TDD guide
- [Leap Year Wikipedia](https://en.wikipedia.org/wiki/Leap_year)
