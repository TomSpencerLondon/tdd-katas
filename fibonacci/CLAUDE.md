# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Fibonacci Kata (AI Pairing Edition)

### Goal

Write a function that calculates the nth Fibonacci number.

**The Fibonacci sequence**: 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144...

Each number is the sum of the two preceding ones, starting from 0 and 1.

### APIs to Build

- `fibonacci(n) -> int` - Return the nth Fibonacci number (0-indexed)

### Ground Rules (TDD Flow)

1. **Red**: add one failing test (only one new assertion at a time)
2. **Green**: write the simplest code to pass all tests
3. **Refactor**: improve the code without changing behavior. Keep tests green
4. **Commit**: commit regularly with descriptive messages documenting the phase
5. **Repeat**

Keep each change small enough that an AI pair can follow and reason about it.

### Commit Strategy

**Commit frequently** after completing each phase or logical grouping of tests.

**Commit message format:**
```
Phase X: Brief description

TDD Red-Green-Refactor cycle:
- Test description: What changed in production code
- Test description: What changed in production code

Tests: X passing
Coverage: X%
```

### Step-by-Step Plan

#### Phase A — Base Cases (n=0 and n=1)

1. **Test 1 (Red)**: fibonacci(0) → 0
   - Prompt: "Add a failing test asserting fibonacci(0) == 0."
   - Green (Fake it): `return 0`
   - Run tests (all green)

2. **Test 2 (Red)**: fibonacci(1) → 1
   - Prompt: "Add a failing test asserting fibonacci(1) == 1."
   - Green: `if n == 1: return 1` / `else: return 0`
   - Or simpler: `return n`
   - Run tests (all green)

Rationale: Start with the simplest base cases. These are the foundation of the Fibonacci sequence.

#### Phase B — Simple Sequence (n=2, 3, 4)

3. **Test 3 (Red)**: fibonacci(2) → 1
   - Prompt: "Add a failing test asserting fibonacci(2) == 1."
   - Green: This is where the algorithm emerges. Options:
     - Iterative: Track previous two numbers
     - Recursive: `fibonacci(n-1) + fibonacci(n-2)`
   - Choose the approach that emerges naturally from simplest code
   - Run tests (all green)

4. **Test 4 (Red)**: fibonacci(3) → 2
   - Prompt: "Add a failing test asserting fibonacci(3) == 2."
   - Green: Should pass with existing logic (triangulation)
   - Run tests (all green)

5. **Test 5 (Red)**: fibonacci(4) → 3
   - Prompt: "Add a failing test asserting fibonacci(4) == 3."
   - Green: Should pass with existing logic
   - Run tests (all green)

#### Phase C — Larger Numbers (n=5, 6, 7)

6. **Test 6 (Red)**: fibonacci(5) → 5
   - Green: Should pass with existing algorithm
   - Run tests (all green)

7. **Test 7 (Red)**: fibonacci(6) → 8
   - Green: Triangulation confirms algorithm
   - Run tests (all green)

8. **Test 8 (Red)**: fibonacci(7) → 13
   - Green: Further validation
   - Run tests (all green)

#### Phase D — Edge Cases and Optimization

9. **Test 9 (Red)**: fibonacci(10) → 55
   - Green: Test larger position
   - Run tests (all green)

10. **(Optional)** Consider refactoring:
    - If using recursion, is it too slow? Add memoization
    - If using iteration, is the code clear? Refactor for readability
    - Add helper functions if needed

### Minimal Acceptance Tests (Recap)

**Base cases:**
- `fibonacci(0) == 0`
- `fibonacci(1) == 1`

**Simple sequence:**
- `fibonacci(2) == 1`
- `fibonacci(3) == 2`
- `fibonacci(4) == 3`

**Larger numbers:**
- `fibonacci(5) == 5`
- `fibonacci(6) == 8`
- `fibonacci(7) == 13`

**Edge case:**
- `fibonacci(10) == 55`

### Refactor Checklist (Use After Green)

- No magic numbers - use named constants if helpful
- Clear variable names (a, b for iterative is common practice)
- Consider extracting helper functions if complexity grows
- If using recursion, consider memoization for performance
- Keep tests fast and isolated

### Example Unit Tests (Python / pytest)

```python
# Phase A: Base cases
def test_fibonacci_0_is_0():
    assert fibonacci(0) == 0

def test_fibonacci_1_is_1():
    assert fibonacci(1) == 1

# Phase B: Simple sequence
def test_fibonacci_2_is_1():
    assert fibonacci(2) == 1

def test_fibonacci_3_is_2():
    assert fibonacci(3) == 2

def test_fibonacci_4_is_3():
    assert fibonacci(4) == 3

# Phase C: Larger numbers
def test_fibonacci_5_is_5():
    assert fibonacci(5) == 5

def test_fibonacci_6_is_8():
    assert fibonacci(6) == 8

def test_fibonacci_7_is_13():
    assert fibonacci(7) == 13

# Phase D: Edge cases
def test_fibonacci_10_is_55():
    assert fibonacci(10) == 55
```

### Implementation Approaches

Through TDD, one of these implementations will emerge:

**Iterative (recommended for TDD):**
```python
def fibonacci(n):
    if n <= 1:
        return n
    a, b = 0, 1
    for _ in range(2, n + 1):
        a, b = b, a + b
    return b
```

**Recursive (simple but inefficient):**
```python
def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n-1) + fibonacci(n-2)
```

**With memoization:**
```python
def fibonacci(n, memo={}):
    if n in memo:
        return memo[n]
    if n <= 1:
        return n
    memo[n] = fibonacci(n-1, memo) + fibonacci(n-2, memo)
    return memo[n]
```

### Common Test Commands

- Python: `pytest` or `ptw` (watch mode)

### Running the Solution

- Python: `python -c "from src.fibonacci import fibonacci; print(fibonacci(10))"`

### Stretch Goals (Pick 1–2 if Time Remains)

- Add input validation (reject negative numbers or non-integers)
- Create `fibonacci_sequence(n)` that returns list of first n numbers
- Implement using matrix exponentiation (O(log n))
- Create a generator function for infinite Fibonacci sequence
- Add performance benchmarking tests

### Common Pitfalls

- Writing multiple failing tests at once
- Implementing full algorithm before simplest test demands it
- Not starting with base cases (0 and 1)
- Over-optimizing too early (premature optimization)
- Using recursion without memoization for large n

### "Script" to Drive AI Pairing (Step by Step)

1. "Create a test file. Add one failing test: fibonacci(0) == 0. Don't touch production code."
2. "Make it green with the simplest change (return 0). No refactor yet."
3. "Add one failing test: fibonacci(1) == 1. Make it green with smallest change."
4. "Add one failing test: fibonacci(2) == 1. Make it green - this forces the algorithm."
5. "Add one failing test: fibonacci(3) == 2. Confirm no code change needed (triangulation)."
6. "Add one failing test: fibonacci(4) == 3. Confirm green."
7. "Add tests for fibonacci(5), fibonacci(6), fibonacci(7). Triangulate the algorithm."
8. "Add test for fibonacci(10). Consider refactoring if needed."

### Collaboration Contract with AI Pair

Give concise prompts and ask for small diffs. Examples:
- "Write one test: fibonacci(0) returns 0. Do not change production code."
- "Make it green with the simplest change. No refactors yet."
- "Refactor to remove duplication. No behavior changes."
- "Propose the next test that forces new behavior. Explain why."

### Understanding the Algorithm (For Learning)

The Fibonacci sequence has a simple pattern:
- F(0) = 0
- F(1) = 1
- F(n) = F(n-1) + F(n-2) for n > 1

The iterative approach keeps track of the last two numbers and builds up to n.

The recursive approach directly follows the mathematical definition but is inefficient without memoization.

Through TDD, you'll discover which approach emerges naturally from your tests!
