# Anagrams Kata (Python)

A TDD implementation of an anagram generator kata using Python and pytest.

## Problem Statement

Write a program to generate all potential anagrams of an input string.

**Function signature**: `generate_anagrams(word) -> list[str]`

An anagram is a rearrangement of all the letters in a word. For example, the potential anagrams of "biro" are all possible permutations of its letters.

### Examples

**Example 1: "biro"**
```
Anagrams (24 total):
biro bior brio broi boir bori
ibro ibor irbo irob iobr iorb
rbio rboi ribo riob roib robi
obir obri oibr oirb orbi orib
```

**Example 2: "ab"**
```
Anagrams (2 total):
ab ba
```

**Example 3: "abc"**
```
Anagrams (6 total):
abc acb bac bca cab cba
```

**Example 4: "a"**
```
Anagrams (1 total):
a
```

**Example 5: "aa" (duplicates)**
```
Anagrams (1 total - duplicates removed):
aa
```

### Edge Cases
- **Empty string**: `generate_anagrams("")` → `[""]`
- **Single character**: `generate_anagrams("a")` → `["a"]`
- **Duplicate letters**: Should return unique anagrams only
- **All same letters**: `generate_anagrams("aaa")` → `["aaa"]`

### Mathematical Note

The number of permutations of n distinct items is **n! (factorial)**:
- 1 character: 1! = 1
- 2 characters: 2! = 2
- 3 characters: 3! = 6
- 4 characters: 4! = 24

With duplicate letters, the formula is: **n! / (n₁! × n₂! × ... × nₖ!)**
where n₁, n₂, ..., nₖ are counts of each duplicate letter.

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

### Anagrams Kata Phases

#### Phase A: Base Cases (empty, single character)
- Test empty string: Returns [""]
- Test single character "a": Returns ["a"]
- **Learning**: Start with simplest cases

#### Phase B: Two Characters
- Test "ab": Returns ["ab", "ba"] (2 permutations)
- Test "aa": Returns ["aa"] (handle duplicates)
- **Learning**: Discover permutation algorithm

#### Phase C: Three Characters
- Test "abc": Returns 6 permutations
- Test "aab": Returns unique permutations only
- **Learning**: Algorithm scales, duplicates handled

#### Phase D: Four Characters (The Example)
- Test "biro": Returns 24 permutations
- **Learning**: Validate against problem statement

#### Phase E: Edge Cases
- Test "aaa": All same characters
- Test longer strings with duplicates
- **Learning**: Robustness and uniqueness

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
pytest src/tests/test_anagrams.py::test_empty_string
```

## Expected Test Results

After completing all phases:
- **~9 tests passing**
- **100% code coverage**
- **Clean git history** showing TDD progression

## Key Learning Goals

1. **TDD Rhythm**: Red-Green-Refactor cycle
2. **Fake it till you make it**: Start simple, generalize later
3. **Triangulation**: Multiple tests drive general solutions
4. **Permutation algorithms**: Recursive or iterative approach
5. **Handling duplicates**: Using sets or other techniques
6. **Algorithm discovery**: Let tests guide implementation

## Project Structure

```
anagrams/
├── src/
│   ├── __init__.py
│   ├── anagrams.py              # Implementation
│   └── tests/
│       ├── __init__.py
│       └── test_anagrams.py     # Test suite
├── pyproject.toml               # Project configuration
├── .flake8                      # Linting rules
├── .pre-commit-config.yaml      # Git hooks
├── .gitignore                   # Git ignore rules
├── README.md                    # This file
└── CLAUDE.md                    # TDD guide for Claude
```

## Technologies Used

- **Language**: Python 3.10+
- **Testing**: pytest, pytest-cov, pytest-watch
- **Linting**: flake8
- **Git Hooks**: pre-commit

## Algorithm Approaches

### Approach 1: Recursive (Most common for TDD)
Generate permutations by:
1. Pick each character as first
2. Recursively generate permutations of remaining characters
3. Combine first character with each permutation

### Approach 2: itertools.permutations (Pythonic)
Use Python's built-in library:
```python
from itertools import permutations
return [''.join(p) for p in permutations(word)]
```

### Approach 3: Iterative
Build permutations iteratively using backtracking or other techniques.

TDD will reveal which approach emerges naturally!

## Author

**Tom Spencer**
- Email: tomspencerlondon@gmail.com
- GitHub: [@TomSpencerLondon](https://github.com/TomSpencerLondon)

---

**Generated with**: [Claude Code](https://claude.com/claude-code)  
**Last Updated**: November 2025
