# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## FizzBuzz (AI Pairing Edition)

### Goal

Write a function that turns numbers into strings with these rules:
- Multiples of 3 → "Fizz"
- Multiples of 5 → "Buzz"
- Multiples of both 3 and 5 → "FizzBuzz"
- Everything else → the number as a string

You'll first build a single-number API, then a sequence API for 1..100.

### APIs to Build

- `fizzbuzz_of(number) -> string`
- `fizzbuzz_1_to(n=100) -> list[string]` (return a list of strings; let the caller join if needed)

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

Example: After completing Phase A & B, commit with details about which tests were added and what production code changes were made at each step.

### Step-by-Step Plan

#### Phase A — Numbers that are not multiples of 3 or 5

1. **Test 1 (Red)**: 1 → "1"
   - Prompt: "Add a failing test asserting fizzbuzz_of(1) == "1"."
   - Green (Fake it): `return "1"` for input 1
   - Run tests (all green)

2. **Test 2 (Red)**: 2 → "2"
   - Prompt: "Add a failing test asserting fizzbuzz_of(2) == "2"."
   - Green: `if number == 1: return "1"` / `else: return "2"`
   - Run tests (all green)

3. **Test 3 (Red)**: 4 → "4"
   - Prompt: "Add a failing test asserting fizzbuzz_of(4) == "4"."
   - Green: Extend the obvious implementation
   - Refactor: Remove duplication → `return str(number)`
   - Run tests (all green)

Rationale: We didn't pick 3 yet because that introduces new behavior (Fizz). We finish the current behavior first.

#### Phase B — Multiples of 3 produce "Fizz"

4. **Test 4 (Red)**: 3 → "Fizz"
   - Prompt: "Add a failing test asserting fizzbuzz_of(3) == "Fizz"."
   - Green: `if number % 3 == 0: return "Fizz"` / `else: return str(number)`
   - Run tests (all green)

5. **Test 5 (Red)**: 6 → "Fizz" (triangulate)
   - Prompt: "Add a failing test asserting fizzbuzz_of(6) == "Fizz". Keep changes minimal."
   - Green: The `% 3` rule already covers this. No code change needed
   - Run tests (all green)

#### Phase C — Multiples of 5 produce "Buzz"

6. **Test 6 (Red)**: 5 → "Buzz"
   - Green: Add `if number % 5 == 0: return "Buzz"`
   - Run tests (all green)

7. **Test 7 (Red)**: 10 → "Buzz"
   - Green: Covered by `% 5`. No change (confirm with tests)

#### Phase D — Multiples of 3 and 5 produce "FizzBuzz"

8. **Test 8 (Red)**: 15 → "FizzBuzz"
   - Green: Ensure the combined case is handled first:
     ```
     if number % 15 == 0: return "FizzBuzz"
     if number % 3 == 0:  return "Fizz"
     if number % 5 == 0:  return "Buzz"
     return str(number)
     ```
   - Run tests (all green)

#### Phase E — Sequence API

9. **Test 9 (Red)**: `fizzbuzz_1_to(5)` returns `["1","2","Fizz","4","Buzz"]`
   - Prompt: "Add a failing test for fizzbuzz_1_to(5) that asserts exact list equality."
   - Green: `return [fizzbuzz_of(i) for i in range(1, n+1)]`
   - Run tests (all green)

10. **(Optional) Test 10 (Red)**: default to 100
    - Prompt: "Add a failing test: calling fizzbuzz_1_to() returns a list of length 100."
    - Green: default parameter `n=100`

### Minimal Acceptance Tests (Recap)

- `fizzbuzz_of(1) == "1"`
- `fizzbuzz_of(2) == "2"`
- `fizzbuzz_of(4) == "4"`
- `fizzbuzz_of(3) == "Fizz"`
- `fizzbuzz_of(6) == "Fizz"`
- `fizzbuzz_of(5) == "Buzz"`
- `fizzbuzz_of(10) == "Buzz"`
- `fizzbuzz_of(15) == "FizzBuzz"`
- `fizzbuzz_1_to(5) == ["1","2","Fizz","4","Buzz"]`
- (Optional) `len(fizzbuzz_1_to()) == 100`

### Refactor Checklist (Use After Green)

- No magic numbers (15) scattered—consider named helpers: `is_multiple(of, n) = (n % of == 0)`
- Readability: clear early returns; no nested if ladders
- Alternative approach (compute parts then combine):
  ```
  out = ""
  if n % 3 == 0: out += "Fizz"
  if n % 5 == 0: out += "Buzz"
  return out or str(n)
  ```
- Keep tests fast and isolated

### Example Unit Tests (Python / pytest)

```python
def test_1_is_string():
    assert fizzbuzz_of(1) == "1"

def test_2_is_string():
    assert fizzbuzz_of(2) == "2"

def test_4_is_string():
    assert fizzbuzz_of(4) == "4"

def test_3_is_fizz():
    assert fizzbuzz_of(3) == "Fizz"

def test_6_is_fizz():
    assert fizzbuzz_of(6) == "Fizz"

def test_5_is_buzz():
    assert fizzbuzz_of(5) == "Buzz"

def test_10_is_buzz():
    assert fizzbuzz_of(10) == "Buzz"

def test_15_is_fizzbuzz():
    assert fizzbuzz_of(15) == "FizzBuzz"

def test_sequence_to_5():
    assert fizzbuzz_1_to(5) == ["1","2","Fizz","4","Buzz"]
```

### Common Test Commands by Language

- Python: `pytest` or `python -m pytest` (if pytest is used), `python -m unittest` (if unittest is used)
- JavaScript/Node: `npm test` or `jest` (if configured)
- TypeScript: `npm test` or `ts-node` for running directly
- Java: `mvn test` or `gradle test` (depending on build tool)
- Ruby: `rspec` or `ruby <test_file>.rb`

### Running the Solution

- Python: `python fizzbuzz.py` or `python3 fizzbuzz.py`
- JavaScript: `node fizzbuzz.js`
- TypeScript: `ts-node fizzbuzz.ts` or `npm start`
- Java: `javac FizzBuzz.java && java FizzBuzz`
- Ruby: `ruby fizzbuzz.rb`

### Stretch Goals (Pick 1–2 if Time Remains)

- Make the words and divisors configurable, e.g. `rules=[(3,"Fizz"),(5,"Buzz")]`
- Add input validation (reject non-positive ints)
- Output as a single string joined with spaces or newlines (document the format)
- Property tests (e.g., all multiples of 3 map to strings containing "Fizz")

### Common Pitfalls

- Writing multiple failing tests at once (creates ambiguity)
- Handling 3 && 5 after single rules (wrong order). Always check the combined case first or build the string and fall back
- Over-engineering too early. Favor simple code that today's tests demand

### "Script" to Drive AI Pairing (Step by Step)

1. "Create a test file. Add one failing test: fizzbuzz_of(1) == "1". Don't touch production code."
2. "Make it green with the simplest change (fake it). No refactor yet."
3. "Add one failing test: fizzbuzz_of(2) == "2". Make it green with the smallest change."
4. "Add one failing test: fizzbuzz_of(4) == "4". Make it green, then propose a safe refactor to remove duplication. Apply it."
5. "Add one failing test: fizzbuzz_of(3) == "Fizz". Make it green."
6. "Add one failing test: fizzbuzz_of(6) == "Fizz". Confirm no code change needed."
7. "Add one failing test: fizzbuzz_of(5) == "Buzz". Make it green."
8. "Add one failing test: fizzbuzz_of(10) == "Buzz". Confirm green."
9. "Add one failing test: fizzbuzz_of(15) == "FizzBuzz". Make it green (ensure combined case works)."
10. "Add one failing test: fizzbuzz_1_to(5) equals ['1','2','Fizz','4','Buzz']. Make it green with a loop/map over fizzbuzz_of."
11. "(Optional) Add a failing test that default fizzbuzz_1_to() returns 100 items."

### Collaboration Contract with AI Pair

Give concise prompts and ask for small diffs. Examples:
- "Write one test: when I fizzbuzz 1, I get '1'. Do not change production code."
- "Make it green with the simplest change. No refactors yet."
- "Refactor to remove duplication. No behavior changes."
- "Propose the next test that forces new behavior (not just more examples of the same). Explain why."
