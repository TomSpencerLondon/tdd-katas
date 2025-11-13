# Anagrams Kata - TDD Journey Summary

## Overview

**Kata**: Anagrams Generator  
**Language**: Python 3.10+  
**Testing Framework**: pytest  
**Final Results**: 9 tests passing, 100% code coverage  
**Implementation**: 8 lines total (5 executable statements)

## Problem Statement

Write a program to generate all potential anagrams (permutations) of an input string.

**Function signature**: `generate_anagrams(word) -> list[str]`

**Example**: `generate_anagrams("biro")` → 24 unique permutations

## TDD Journey - Five Phases

### Phase A: Base Cases (Empty, Single Character)

#### Test 1: Empty String
**RED**: Test expects [""]
**GREEN**: Hardcoded return `[""]`
**Commit**: "Phase A: Test empty string - hardcoded return"

#### Test 2: Single Character "a"
**RED**: Test expects ["a"]
**GREEN**: Added conditional - if empty return [""], else return [word]
**Commit**: "Phase A: Test single character 'a'"

**Phase A Key Learning**: Start with simplest cases. Fake it till you make it!

---

### Phase B: Two Characters - THE BREAKTHROUGH

#### Test 3: Two Different Characters "ab"
**RED**: Test expects ["ab", "ba"]
**GREEN**: Implemented real permutation logic!
```python
from itertools import permutations
return [''.join(p) for p in permutations(word)]
```
**Commit**: "Phase B: Test two characters 'ab' - permutation algorithm emerges!"

**This is the key moment** - the algorithm emerged from the test!

#### Test 4: Two Same Characters "aa"
**RED**: Test expects ["aa"] (single result)
**Actual**: Got ["aa", "aa"] (duplicates)
**GREEN**: Remove duplicates using set
```python
return list(set(''.join(p) for p in permutations(word)))
```
**Commit**: "Phase B: Test 'aa' - handle duplicates"

**Phase B Key Learning**: itertools.permutations + set solves the problem elegantly!

---

### Phase C: Three Characters - Validation

#### Test 5 & 6: Three Characters (with and without duplicates)
**GREEN → GREEN**: Both passed WITHOUT code changes!
- "abc" → 6 permutations (3! = 6)
- "aab" → 3 unique permutations

**Commit**: "Phase C: Three character tests - validation"

**Phase C Key Learning**: General solution scales correctly!

---

### Phase D: Four Characters - Problem Statement Match

#### Test 7: "biro" Example
**GREEN → GREEN**: Passed WITHOUT code changes!
- Returns exactly 24 permutations (4! = 24)
- All unique
- Matches problem statement perfectly

**Commit**: "Phase D: Test 'biro' - matches problem statement!"

**Phase D Key Learning**: Solution validated against original requirement!

---

### Phase E: Edge Cases - Final Robustness

#### Test 8 & 9: Edge Cases
**GREEN → GREEN**: Both passed WITHOUT code changes!
- "aaa" → ["aaa"] (1 permutation)
- "aabb" → 6 unique permutations

**Commit**: "Phase E: Edge case tests - final validation"

**Phase E Key Learning**: Edge cases confirm robustness!

---

## Final Implementation

```python
from itertools import permutations


def generate_anagrams(word):
    """Generate all unique anagrams (permutations) of a word."""
    # Handle empty string
    if not word:
        return [""]
    
    # Generate all permutations using itertools
    # Convert each tuple of characters to a string
    # Use list(set(...)) to remove duplicates
    return list(set(''.join(p) for p in permutations(word)))
```

**Lines of code**: 8 total (5 executable statements)  
**Complexity**: O(n! × n) time, O(n! × n) space  
**Readability**: Extremely clear and Pythonic

## Git Commit History

1. **Initial setup**: Project structure, README, CLAUDE.md
2. **Phase A - Test 1**: Empty string (hardcoded return)
3. **Phase A - Test 2**: Single character (conditional logic)
4. **Phase B - Test 3**: Two characters (permutation algorithm emerges!)
5. **Phase B - Test 4**: Two same (handle duplicates with set)
6. **Phase C - Tests 5-6**: Three characters (validation)
7. **Phase D - Test 7**: "biro" example (matches problem)
8. **Phase E - Tests 8-9**: Edge cases (final validation)
9. **Presentation**: KATA_SUMMARY.md, PowerPoint, and PDF

## Test Results

```
============================= test session starts ==============================
collected 9 items

src/tests/test_anagrams.py::test_empty_string PASSED                     [ 11%]
src/tests/test_anagrams.py::test_single_character PASSED                 [ 22%]
src/tests/test_anagrams.py::test_two_different_characters PASSED         [ 33%]
src/tests/test_anagrams.py::test_two_same_characters PASSED              [ 44%]
src/tests/test_anagrams.py::test_three_different_characters PASSED       [ 55%]
src/tests/test_anagrams.py::test_three_with_duplicates PASSED            [ 66%]
src/tests/test_anagrams.py::test_four_characters_biro PASSED             [ 77%]
src/tests/test_anagrams.py::test_all_same_characters PASSED              [ 88%]
src/tests/test_anagrams.py::test_longer_with_duplicates PASSED           [100%]

================================ tests coverage ================================
Name              Stmts   Miss    Cover   Missing
-------------------------------------------------
src/anagrams.py       5      0  100.00%
-------------------------------------------------
TOTAL                 5      0  100.00%
============================== 9 passed in 0.04s ===============================
```

## Key TDD Principles Demonstrated

1. **Red-Green-Refactor**: Followed for each test
2. **Fake It Till You Make It**: Started with hardcoded `[""]`
3. **Triangulation**: Multiple tests confirmed general solution
4. **Let Algorithm Emerge**: Didn't design upfront - test 3 forced it
5. **Small Steps**: One test at a time
6. **Python stdlib**: itertools.permutations is perfect for this

## Key Learnings

### Technical
1. **itertools.permutations**: Python's built-in handles complexity
2. **set for deduplication**: Simple and effective
3. **List comprehension**: Concise and readable
4. **Generator expression**: Efficient for large inputs

### TDD Process
1. **Hardcoding is OK early**: Gets TDD rhythm going
2. **Breakthrough test**: Test 3 forced real solution
3. **Triangulation validates**: Tests 4-9 confirmed correctness
4. **Edge cases as validation**: All passing confirms good design

### Design
1. **Chose right abstractions**: itertools over manual recursion
2. **Handle empty early**: Simplifies rest of logic
3. **Pythonic solution**: Leverages stdlib effectively
4. **Simple is best**: 5 statements vs complex recursive logic

## Comparison to Other Approaches

### Our Approach: itertools + set
```python
return list(set(''.join(p) for p in permutations(word)))
```
**Pros**: Simple, readable, leverages stdlib  
**Cons**: Generates duplicates then removes them  
**Verdict**: Perfect for this kata!

### Alternative: Manual Recursion
```python
def permute(remaining, current=""):
    if not remaining:
        return [current]
    result = []
    for i, char in enumerate(remaining):
        result.extend(permute(remaining[:i] + remaining[i+1:], current + char))
    return list(set(result))
```
**Pros**: Educational, no imports  
**Cons**: More complex, harder to understand  
**Verdict**: Good for learning recursion

## Mathematical Note

Permutations of n distinct items: **n!**
- 1 char: 1! = 1
- 2 chars: 2! = 2
- 3 chars: 3! = 6
- 4 chars: 4! = 24
- 5 chars: 5! = 120

With duplicates: **n! / (n₁! × n₂! × ...)**
- "aa": 2! / 2! = 1
- "aab": 3! / 2! = 3
- "aabb": 4! / (2! × 2!) = 6

## Usage Examples

```python
# Basic
generate_anagrams("ab")  # ['ab', 'ba']

# Problem example
generate_anagrams("biro")  # 24 permutations

# With duplicates
generate_anagrams("aab")  # ['aab', 'aba', 'baa']

# All same
generate_anagrams("aaa")  # ['aaa']

# Empty
generate_anagrams("")  # ['']
```

## Possible Extensions

1. **Sorted output**: Return sorted list for consistency
2. **Count only**: Option to return count instead of list
3. **Iterator**: Yield permutations one at a time for memory efficiency
4. **Partial permutations**: Generate permutations of length k
5. **Performance**: For very long strings, warn about factorial growth

## Conclusion

The Anagrams kata beautifully demonstrates how TDD leads to simple, elegant solutions. By letting the tests drive the implementation, we discovered that Python's `itertools.permutations` combined with `set` provides everything we need. The result is 8 lines of highly readable, fully tested code that handles all edge cases.

**Most Important Lesson**: Leverage your language's standard library - it often has exactly what you need!

---

**Generated with**: [Claude Code](https://claude.com/claude-code)  
**Last Updated**: November 2025  
**Author**: Tom Spencer
