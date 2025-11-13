# Fibonacci Kata (Python)

A TDD implementation of the Fibonacci sequence kata using Python and pytest.

## Problem Statement

Write code to generate the Fibonacci number for the nth position.

**Function signature**: `fibonacci(position) -> int`

The Fibonacci sequence is a series of numbers where each number is the sum of the two preceding ones.

**Starting values**:
- Position 0: 0
- Position 1: 1

**Sequence**: 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144...

### Examples
- `fibonacci(0)` → 0
- `fibonacci(1)` → 1
- `fibonacci(2)` → 1  (0 + 1)
- `fibonacci(3)` → 2  (1 + 1)
- `fibonacci(4)` → 3  (1 + 2)
- `fibonacci(5)` → 5  (2 + 3)
- `fibonacci(6)` → 8  (3 + 5)
- `fibonacci(10)` → 55

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

### Fibonacci Kata Phases

#### Phase A: Base Cases (positions 0 and 1)
- Test fib(0)→0: Start with hardcoded return value
- Test fib(1)→1: Add simple conditional
- **Learning**: Start with the simplest cases first

#### Phase B: Simple Sequence (positions 2, 3, 4)
- Test fib(2)→1: Discover recursive/iterative pattern
- Test fib(3)→2: Triangulation
- Test fib(4)→3: Further validation
- **Learning**: Build incrementally

#### Phase C: Larger Numbers (positions 5, 6, 7)
- Test fib(5)→5: Validate algorithm
- Test fib(6)→8: Triangulation
- Test fib(7)→13: Further validation
- **Learning**: Algorithm works for any valid input

#### Phase D: Edge Cases
- Test fib(10)→55: Test larger position
- **Learning**: Handle edge cases

## Quick Start

```bash
source venv/bin/activate
ptw  # Watch mode
```

Follow [CLAUDE.md](CLAUDE.md) for step-by-step TDD guide.
