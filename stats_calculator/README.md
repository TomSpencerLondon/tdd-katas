# Stats Calculator Kata (Python)

A TDD implementation of a statistics calculator kata using Python and pytest.

## Problem Statement

Process a sequence of integer numbers to determine the following statistics:
- **Minimum value**: The smallest number in the sequence
- **Maximum value**: The largest number in the sequence
- **Number of elements**: Count of numbers in the sequence
- **Average value**: Mean of all numbers in the sequence

**Function signature**: `calculate_stats(numbers) -> dict`

### Examples
- `calculate_stats([5])` → `{'min': 5, 'max': 5, 'count': 1, 'average': 5.0}`
- `calculate_stats([1, 2, 3])` → `{'min': 1, 'max': 3, 'count': 3, 'average': 2.0}`
- `calculate_stats([3, 1, 2])` → `{'min': 1, 'max': 3, 'count': 3, 'average': 2.0}`
- `calculate_stats([-5, -1, -3])` → `{'min': -5, 'max': -1, 'count': 3, 'average': -3.0}`
- `calculate_stats([1, 2, 3, 4, 5])` → `{'min': 1, 'max': 5, 'count': 5, 'average': 3.0}`

### Edge Cases
- **Empty sequence**: `calculate_stats([])` → `{'min': None, 'max': None, 'count': 0, 'average': None}`
- **Duplicates**: Handle repeated values correctly
- **Negative numbers**: Support negative integers
- **Mixed positive/negative**: Handle both in same sequence

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

## TDD Workflow

Follow the Red-Green-Refactor cycle:

1. **Red**: Write a failing test
2. **Green**: Write the simplest code to make it pass
3. **Refactor**: Improve the code without changing behavior
4. **Commit**: Commit with descriptive message
5. **Repeat**

### Stats Calculator Kata Phases

#### Phase A: Empty/Single Element (Base Cases)
- Test empty sequence: Handle edge case
- Test single element [5]: All stats equal to that element
- **Learning**: Start with simplest and edge cases

#### Phase B: Simple Sequences (2-3 elements)
- Test two elements [1, 2]: Discover algorithm for min/max/average
- Test three elements [1, 2, 3]: Triangulation
- Test unordered [3, 1, 2]: Verify ordering independence
- **Learning**: Build incrementally, test assumptions

#### Phase C: Edge Cases (Negatives and Duplicates)
- Test negative numbers [-5, -1, -3]: Handle negatives
- Test mixed [-2, 0, 2]: Handle zero and both signs
- Test duplicates [2, 2, 2]: Handle repeated values
- Test duplicates at extremes [1, 2, 1]: Verify min/max with dupes
- **Learning**: Edge cases reveal algorithm robustness

#### Phase D: Larger Sequences
- Test larger sequence [1..10]: Validate for longer input
- **Learning**: Algorithm scales correctly

## Quick Start

```bash
source venv/bin/activate
ptw  # Watch mode
```

Follow [CLAUDE.md](CLAUDE.md) for step-by-step TDD guide.

## Running Tests

```bash
# Run all tests
pytest

# Run with coverage
pytest --cov=src --cov-report=term-missing

# Run in watch mode (auto-rerun on file changes)
ptw

# Run specific test
pytest src/tests/test_stats_calculator.py::test_single_element
```

## Expected Test Results

After completing all phases:
- **10 tests passing**
- **100% code coverage**
- **Clean git history** showing TDD progression

## Key Learning Goals

1. **TDD Rhythm**: Red-Green-Refactor cycle
2. **Fake it till you make it**: Start simple, generalize later
3. **Triangulation**: Multiple tests drive general solutions
4. **Edge case handling**: Empty sequences, negatives, duplicates
5. **Data structure selection**: Choosing dict for return value
6. **Algorithm discovery**: Let tests guide implementation

## Project Structure

```
stats_calculator/
├── src/
│   ├── __init__.py
│   ├── stats_calculator.py    # Implementation
│   └── tests/
│       ├── __init__.py
│       └── test_stats_calculator.py  # Test suite
├── pyproject.toml             # Project configuration
├── .flake8                    # Linting rules
├── .pre-commit-config.yaml    # Git hooks
├── .gitignore                 # Git ignore rules
├── README.md                  # This file
└── CLAUDE.md                  # TDD guide for Claude
```

## Technologies Used

- **Language**: Python 3.10+
- **Testing**: pytest, pytest-cov, pytest-watch
- **Linting**: flake8
- **Git Hooks**: pre-commit

## Author

**Tom Spencer**
- Email: tomspencerlondon@gmail.com
- GitHub: [@TomSpencerLondon](https://github.com/TomSpencerLondon)

---

**Generated with**: [Claude Code](https://claude.com/claude-code)
**Last Updated**: November 2025
