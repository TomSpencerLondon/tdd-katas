# Anagrams Kata - TDD Guide for Claude

This guide provides step-by-step instructions for implementing the Anagrams kata using Test-Driven Development (TDD).

## Problem Statement

Write a function that generates all anagrams (permutations) of an input string.

**Function signature**: `generate_anagrams(word) -> list[str]`

**Example**: `generate_anagrams("biro")` should return all 24 permutations.

## Ground Rules for TDD Flow

1. **RED**: Write ONE failing test
2. **GREEN**: Write the SIMPLEST code to make it pass (even hardcoding!)
3. **REFACTOR**: Improve code while keeping tests green
4. **COMMIT**: Make a descriptive commit after each green or refactor
5. **REPEAT**: Move to next test

### Important TDD Principles

- **Start simple**: Hardcode return values initially
- **Fake it till you make it**: Don't solve the general problem immediately
- **Triangulation**: Add more tests to force generalization
- **Small steps**: One test at a time
- **Let the algorithm emerge**: Don't design upfront

## Step-by-Step TDD Plan

### Phase A: Base Cases (empty, single character)

**Test 1: Empty string**
```python
def test_empty_string():
    """Test that empty string returns list with empty string."""
    result = generate_anagrams("")
    assert result == [""]
```
- **Make it GREEN**: Return hardcoded `[""]`
- **Commit**: "Phase A: Test empty string - hardcoded return"

**Test 2: Single character**
```python
def test_single_character():
    """Test that single character 'a' returns ['a']."""
    result = generate_anagrams("a")
    assert result == ["a"]
```
- **Make it GREEN**: Check if length is 1, return `[word]`
- **Commit**: "Phase A: Test single character 'a'"

**Phase A Key Learning**: Start with simplest cases!

---

### Phase B: Two Characters

**Test 3: Two different characters**
```python
def test_two_different_characters():
    """Test that 'ab' returns ['ab', 'ba']."""
    result = generate_anagrams("ab")
    assert sorted(result) == sorted(["ab", "ba"])
```
- **Make it GREEN**: This forces you to implement permutation logic
- **Options**:
  - Recursive approach: Pick first char, permute rest
  - Use itertools.permutations
  - Manual swapping
- **Commit**: "Phase B: Test two characters 'ab' - permutation logic emerges"

**Test 4: Two same characters**
```python
def test_two_same_characters():
    """Test that 'aa' returns ['aa'] (handle duplicates)."""
    result = generate_anagrams("aa")
    assert result == ["aa"]
```
- **Make it GREEN**: Use `set` to remove duplicates or check during generation
- **Commit**: "Phase B: Test 'aa' - handle duplicate results"

**Phase B Key Learning**: The permutation algorithm must emerge from tests!

---

### Phase C: Three Characters

**Test 5: Three different characters**
```python
def test_three_different_characters():
    """Test that 'abc' returns 6 permutations."""
    result = generate_anagrams("abc")
    expected = ["abc", "acb", "bac", "bca", "cab", "cba"]
    assert sorted(result) == sorted(expected)
    assert len(result) == 6
```
- **Make it GREEN**: Should pass with existing implementation
- **Commit**: "Phase C: Test 'abc' - validate 6 permutations"

**Test 6: Three characters with duplicates**
```python
def test_three_with_duplicates():
    """Test that 'aab' returns 3 unique permutations."""
    result = generate_anagrams("aab")
    expected = ["aab", "aba", "baa"]
    assert sorted(result) == sorted(expected)
    assert len(result) == 3
```
- **Make it GREEN**: Should pass if duplicates already handled
- **Commit**: "Phase C: Test 'aab' - verify duplicate handling"

**Phase C Key Learning**: Algorithm scales correctly!

---

### Phase D: Four Characters (The Example)

**Test 7: Four characters "biro"**
```python
def test_four_characters_biro():
    """Test that 'biro' returns 24 permutations."""
    result = generate_anagrams("biro")
    assert len(result) == 24
    assert len(set(result)) == 24  # All unique
    # Verify a few specific ones
    assert "biro" in result
    assert "bior" in result
    assert "obri" in result
```
- **Make it GREEN**: Should pass with existing implementation
- **Commit**: "Phase D: Test 'biro' - 24 permutations"

**Phase D Key Learning**: Matches problem statement exactly!

---

### Phase E: Edge Cases

**Test 8: All same characters**
```python
def test_all_same_characters():
    """Test that 'aaa' returns ['aaa']."""
    result = generate_anagrams("aaa")
    assert result == ["aaa"]
    assert len(result) == 1
```
- **Make it GREEN**: Should pass with duplicate handling
- **Commit**: "Phase E: Test 'aaa' - all same characters"

**Test 9: Longer string with duplicates (optional)**
```python
def test_longer_with_duplicates():
    """Test that 'aabb' returns correct unique permutations."""
    result = generate_anagrams("aabb")
    # Should have 6 unique permutations: aabb, abab, abba, baab, baba, bbaa
    assert len(result) == 6
    assert len(set(result)) == 6
```
- **Make it GREEN**: Should pass with existing implementation
- **Commit**: "Phase E: Test 'aabb' - longer string validation"

**Phase E Key Learning**: Edge cases confirm robustness!

---

## Possible Final Implementations

### Approach 1: Using itertools.permutations (Simplest)

```python
from itertools import permutations

def generate_anagrams(word):
    """Generate all unique anagrams of a word."""
    if not word:
        return [""]
    
    # Generate all permutations and convert to strings
    perms = [''.join(p) for p in permutations(word)]
    
    # Remove duplicates by converting to set, then back to list
    return list(set(perms))
```

### Approach 2: Recursive (Educational)

```python
def generate_anagrams(word):
    """Generate all unique anagrams of a word."""
    if not word:
        return [""]
    
    if len(word) == 1:
        return [word]
    
    result = []
    for i, char in enumerate(word):
        # Get remaining characters
        remaining = word[:i] + word[i+1:]
        # Recursively generate permutations of remaining
        for perm in generate_anagrams(remaining):
            result.append(char + perm)
    
    # Remove duplicates
    return list(set(result))
```

### Approach 3: Using set from the start

```python
from itertools import permutations

def generate_anagrams(word):
    """Generate all unique anagrams of a word."""
    if not word:
        return [""]
    
    # Use set comprehension to avoid duplicates
    return sorted({''.join(p) for p in permutations(word)})
```

## Expected Results

- **~9 tests**: All passing
- **100% coverage**: Complete test coverage
- **~10 lines of code**: Depending on approach
- **Clear commits**: Each phase well-documented

## Common Pitfalls to Avoid

1. **Don't implement full solution first**: Let tests drive it
2. **Don't skip hardcoding**: Important for TDD rhythm
3. **Don't forget duplicates**: Test "aa" early
4. **Don't write multiple tests at once**: One at a time
5. **Don't over-engineer**: itertools.permutations is fine!

## Key Learnings

1. **TDD reveals simplicity**: itertools might emerge as simplest
2. **Duplicates matter**: Testing "aa" forces duplicate handling
3. **Sorting helps testing**: Compare sorted lists for order independence
4. **Permutations grow fast**: 4! = 24, 5! = 120, 6! = 720
5. **Python's stdlib is powerful**: Built-in tools solve common problems

## Algorithm Complexity

- **Time**: O(n! × n) - generating n! permutations, each of length n
- **Space**: O(n! × n) - storing all permutations
- **Note**: Factorial growth means this becomes impractical for long strings

---

**Ready to start?** Begin with Phase A, Test 1!
