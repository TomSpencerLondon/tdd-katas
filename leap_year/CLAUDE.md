# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Leap Year Kata (AI Pairing Edition)

### Goal

Write a function that determines if a year is a leap year according to the Gregorian Calendar rules. This kata should be performed using Test Driven Development (TDD).

### Historical Context

Prior to 1582, the Julian Calendar defined leap years as every year divisible by 4. The Gregorian Calendar was introduced to more accurately align the calendar year with the solar year by thinning out leap years.

### APIs to Build

- `is_leap_year(year) -> bool`

### Acceptance Criteria (Gregorian Calendar Rules)

1. All years divisible by 400 ARE leap years (e.g., 2000)
2. All years divisible by 100 but not by 400 are NOT leap years (e.g., 1700, 1800, 1900, 2100)
3. All years divisible by 4 but not by 100 ARE leap years (e.g., 2008, 2012, 2016)
4. All years not divisible by 4 are NOT leap years (e.g., 2017, 2018, 2019)

### Ground Rules (TDD Flow)

1. **Red**: add one failing test (only one new assertion at a time)
2. **Green**: write the simplest code to pass all tests
3. **Refactor**: improve the code without changing behavior. Keep tests green
4. **Commit**: commit regularly with descriptive messages documenting the phase and functionality
5. **Repeat**

Keep each change small enough that an AI pair can follow and reason about it.

### Commit Strategy

**Commit frequently** after completing each phase or logical grouping of tests. This creates a reviewable history that makes it easy to:
- Understand the TDD progression
- See how the solution evolved
- Learn from the step-by-step approach
- Rollback if needed

**Commit message format:**
```
Phase X: Brief description of what was added

TDD Red-Green-Refactor cycle:
- Test description: What changed in production code
- Test description: What changed in production code

Tests: X passing
Coverage: X%
```

### Step-by-Step Plan

#### Phase A — Non-leap years (not divisible by 4)

1. **Test 1 (Red)**: 2017 → False
   - Prompt: "Add a failing test asserting is_leap_year(2017) == False."
   - Green (Fake it): `return False` for input 2017
   - Run tests (all green)

2. **Test 2 (Red)**: 2018 → False
   - Prompt: "Add a failing test asserting is_leap_year(2018) == False."
   - Green: The hardcoded `False` still works
   - Run tests (all green)

3. **Test 3 (Red)**: 2019 → False
   - Prompt: "Add a failing test asserting is_leap_year(2019) == False."
   - Green: Triangulation confirms - still `return False`
   - Run tests (all green)

Rationale: We start with the simplest case first (non-leap years) before introducing complexity.

#### Phase B — Basic leap years (divisible by 4 but not by 100)

4. **Test 4 (Red)**: 2016 → True
   - Prompt: "Add a failing test asserting is_leap_year(2016) == True."
   - Green: `if year % 4 == 0: return True` / `else: return False`
   - Run tests (all green)

5. **Test 5 (Red)**: 2012 → True (triangulate)
   - Prompt: "Add a failing test asserting is_leap_year(2012) == True."
   - Green: The `% 4` rule already covers this. No code change needed
   - Run tests (all green)

6. **Test 6 (Red)**: 2008 → True (triangulate)
   - Prompt: "Add a failing test asserting is_leap_year(2008) == True."
   - Green: Covered by existing logic
   - Run tests (all green)

#### Phase C — Century years NOT divisible by 400 are NOT leap years

7. **Test 7 (Red)**: 1900 → False
   - Prompt: "Add a failing test asserting is_leap_year(1900) == False."
   - Green: This will FAIL with current logic (1900 % 4 == 0 returns True)
   - Fix: Add check for century years:
     ```python
     if year % 100 == 0 and year % 400 != 0:
         return False
     if year % 4 == 0:
         return True
     return False
     ```
   - Run tests (all green)

8. **Test 8 (Red)**: 1800 → False (triangulate)
   - Prompt: "Add a failing test asserting is_leap_year(1800) == False."
   - Green: Covered by `% 100` check. No change needed
   - Run tests (all green)

9. **Test 9 (Red)**: 1700 → False (triangulate)
   - Prompt: "Add a failing test asserting is_leap_year(1700) == False."
   - Green: Covered by existing logic
   - Run tests (all green)

10. **Test 10 (Red)**: 2100 → False (future century year)
    - Prompt: "Add a failing test asserting is_leap_year(2100) == False."
    - Green: Covered by existing logic
    - Run tests (all green)

#### Phase D — Century years divisible by 400 ARE leap years

11. **Test 11 (Red)**: 2000 → True
    - Prompt: "Add a failing test asserting is_leap_year(2000) == True."
    - Green: Modify the century check:
      ```python
      if year % 400 == 0:
          return True
      if year % 100 == 0:
          return False
      if year % 4 == 0:
          return True
      return False
      ```
    - Run tests (all green)

12. **Test 12 (Red)**: 2400 → True (triangulate)
    - Prompt: "Add a failing test asserting is_leap_year(2400) == True."
    - Green: Covered by `% 400` check. No change needed
    - Run tests (all green)

13. **Test 13 (Red)**: 1600 → True (triangulate)
    - Prompt: "Add a failing test asserting is_leap_year(1600) == True."
    - Green: Covered by existing logic
    - Run tests (all green)

### Minimal Acceptance Tests (Recap)

**Non-leap years (not divisible by 4):**
- `is_leap_year(2017) == False`
- `is_leap_year(2018) == False`
- `is_leap_year(2019) == False`

**Basic leap years (divisible by 4, not by 100):**
- `is_leap_year(2016) == True`
- `is_leap_year(2012) == True`
- `is_leap_year(2008) == True`

**Century years NOT divisible by 400:**
- `is_leap_year(1900) == False`
- `is_leap_year(1800) == False`
- `is_leap_year(1700) == False`
- `is_leap_year(2100) == False`

**Century years divisible by 400:**
- `is_leap_year(2000) == True`
- `is_leap_year(2400) == True`
- `is_leap_year(1600) == True`

### Refactor Checklist (Use After Green)

- Consider extracting helper functions for clarity:
  ```python
  def is_divisible_by(year, divisor):
      return year % divisor == 0
  ```
- Simplify boolean logic where possible
- Alternative implementation using combined conditions:
  ```python
  return (year % 4 == 0 and year % 100 != 0) or (year % 400 == 0)
  ```
- Keep tests fast and isolated
- Consider edge cases (negative years, year 0) if needed

### Example Unit Tests (Python / pytest)

```python
# Phase A: Non-leap years
def test_2017_is_not_leap_year():
    assert is_leap_year(2017) == False

def test_2018_is_not_leap_year():
    assert is_leap_year(2018) == False

def test_2019_is_not_leap_year():
    assert is_leap_year(2019) == False

# Phase B: Basic leap years
def test_2016_is_leap_year():
    assert is_leap_year(2016) == True

def test_2012_is_leap_year():
    assert is_leap_year(2012) == True

def test_2008_is_leap_year():
    assert is_leap_year(2008) == True

# Phase C: Century years NOT divisible by 400
def test_1900_is_not_leap_year():
    assert is_leap_year(1900) == False

def test_1800_is_not_leap_year():
    assert is_leap_year(1800) == False

def test_1700_is_not_leap_year():
    assert is_leap_year(1700) == False

def test_2100_is_not_leap_year():
    assert is_leap_year(2100) == False

# Phase D: Century years divisible by 400
def test_2000_is_leap_year():
    assert is_leap_year(2000) == True

def test_2400_is_leap_year():
    assert is_leap_year(2400) == True

def test_1600_is_leap_year():
    assert is_leap_year(1600) == True
```

### Common Test Commands by Language

- Python: `pytest` or `python -m pytest` (if pytest is used), `python -m unittest` (if unittest is used)
- JavaScript/Node: `npm test` or `jest` (if configured)
- TypeScript: `npm test` or `ts-node` for running directly
- Java: `mvn test` or `gradle test` (depending on build tool)
- Ruby: `rspec` or `ruby <test_file>.rb`

### Running the Solution

- Python: `python leap_year.py` or `python3 leap_year.py`
- JavaScript: `node leap_year.js`
- TypeScript: `ts-node leap_year.ts` or `npm start`
- Java: `javac LeapYear.java && java LeapYear`
- Ruby: `ruby leap_year.rb`

### Stretch Goals (Pick 1-2 if Time Remains)

- Add the 4000-year rule: Years divisible by 4000 are NOT leap years (even more accurate)
- Add input validation (reject negative years or non-integer inputs)
- Create a `leap_years_between(start_year, end_year) -> list` function
- Create a `next_leap_year(year) -> int` function
- Add property tests (e.g., all leap years must be divisible by 4)
- Calculate days in February: `days_in_february(year) -> int` returns 29 or 28

### Common Pitfalls

- Writing multiple failing tests at once (creates ambiguity)
- Not handling century years correctly (1900 should not be a leap year)
- Checking conditions in the wrong order (check 400 before 100, or use combined boolean logic)
- Over-engineering too early. Favor simple code that today's tests demand

### "Script" to Drive AI Pairing (Step by Step)

1. "Create a test file. Add one failing test: is_leap_year(2017) == False. Don't touch production code."
2. "Make it green with the simplest change (fake it). No refactor yet."
3. "Add one failing test: is_leap_year(2018) == False. Confirm it's still green."
4. "Add one failing test: is_leap_year(2019) == False. Triangulate—still green with False."
5. "Add one failing test: is_leap_year(2016) == True. Make it green with modulo 4 check."
6. "Add one failing test: is_leap_year(2012) == True. Confirm no code change needed."
7. "Add one failing test: is_leap_year(2008) == True. Confirm green."
8. "Add one failing test: is_leap_year(1900) == False. This will fail! Make it green by handling century years."
9. "Add one failing test: is_leap_year(1800) == False. Confirm green."
10. "Add one failing test: is_leap_year(1700) == False. Confirm green."
11. "Add one failing test: is_leap_year(2100) == False. Confirm green."
12. "Add one failing test: is_leap_year(2000) == True. Make it green by checking divisibility by 400 first."
13. "Add one failing test: is_leap_year(2400) == True. Confirm green."
14. "Add one failing test: is_leap_year(1600) == True. Confirm green."
15. "(Optional) Refactor to use combined boolean expression for clarity."

### Collaboration Contract with AI Pair

Give concise prompts and ask for small diffs. Examples:
- "Write one test: when I check 2017, I get False. Do not change production code."
- "Make it green with the simplest change. No refactors yet."
- "Refactor to use a cleaner boolean expression. No behavior changes."
- "Propose the next test that forces new behavior (not just more examples of the same). Explain why."

### Understanding the Logic (For Learning)

The leap year rules can be expressed as a single boolean expression:

```python
def is_leap_year(year):
    return (year % 4 == 0 and year % 100 != 0) or (year % 400 == 0)
```

This reads as: "A year is a leap year if it's divisible by 4 AND NOT a century year, OR if it's divisible by 400."

However, through TDD, you'll discover this logic incrementally, building confidence that each rule works correctly.
