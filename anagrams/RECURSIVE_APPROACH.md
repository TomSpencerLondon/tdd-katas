# Anagrams Kata - Recursive Implementation

This document explains how to implement the anagrams kata using recursion instead of `itertools.permutations`.

## Why Learn the Recursive Approach?

While our kata uses `itertools.permutations` (which is the Pythonic choice), understanding the recursive implementation is valuable because:

1. **Interview questions**: Implementing permutations is a common coding interview problem
2. **Algorithm understanding**: Teaches recursion and divide-and-conquer
3. **Language agnostic**: Works in languages without built-in permutation functions
4. **Educational value**: Deeper understanding of the problem

## The Recursive Algorithm

### Core Insight

**To generate all permutations of a string, pick each character as "first", then recursively generate all permutations of the remaining characters.**

### Visual Example: "abc"

```
generate_anagrams("abc"):
  
  Pick 'a' first → remaining = "bc"
    ├─ generate_anagrams("bc")
    │   ├─ Pick 'b' → remaining = "c" → ["bc"]
    │   └─ Pick 'c' → remaining = "b" → ["cb"]
    └─ Results: "a" + "bc" = "abc"
               "a" + "cb" = "acb"
  
  Pick 'b' first → remaining = "ac"
    ├─ generate_anagrams("ac")
    │   ├─ Pick 'a' → remaining = "c" → ["ac"]
    │   └─ Pick 'c' → remaining = "a" → ["ca"]
    └─ Results: "b" + "ac" = "bac"
               "b" + "ca" = "bca"
  
  Pick 'c' first → remaining = "ab"
    ├─ generate_anagrams("ab")
    │   ├─ Pick 'a' → remaining = "b" → ["ab"]
    │   └─ Pick 'b' → remaining = "a" → ["ba"]
    └─ Results: "c" + "ab" = "cab"
               "c" + "ba" = "cba"

Final result: ["abc", "acb", "bac", "bca", "cab", "cba"]
```

## Implementation

### Recursive Version

```python
def generate_anagrams(word):
    """Generate all unique anagrams using recursion.
    
    Args:
        word: String to generate anagrams for
        
    Returns:
        List of strings containing all unique permutations
    """
    # Base case: empty string
    if not word:
        return [""]
    
    # Base case: single character
    if len(word) == 1:
        return [word]
    
    result = []
    
    # For each character in the word
    for i in range(len(word)):
        # Pick character at position i as "first"
        first_char = word[i]
        
        # Get remaining characters (everything except position i)
        remaining = word[:i] + word[i+1:]
        
        # Recursively generate permutations of remaining characters
        for perm in generate_anagrams(remaining):
            result.append(first_char + perm)
    
    # Remove duplicates (for strings with repeated characters)
    return list(set(result))
```

### Step-by-Step Execution for "ab"

```python
generate_anagrams("ab")

# Iteration 1: i=0
first_char = 'a'
remaining = 'b'
generate_anagrams('b') returns ['b']
  → append 'a' + 'b' = 'ab' to result

# Iteration 2: i=1
first_char = 'b'
remaining = 'a'
generate_anagrams('a') returns ['a']
  → append 'b' + 'a' = 'ba' to result

# result = ['ab', 'ba']
# set removes no duplicates (all unique)
# return ['ab', 'ba']
```

### Step-by-Step Execution for "aa"

```python
generate_anagrams("aa")

# Iteration 1: i=0
first_char = 'a'
remaining = 'a'
generate_anagrams('a') returns ['a']
  → append 'a' + 'a' = 'aa' to result

# Iteration 2: i=1
first_char = 'a'
remaining = 'a'
generate_anagrams('a') returns ['a']
  → append 'a' + 'a' = 'aa' to result

# result = ['aa', 'aa']
# set removes duplicate: {'aa'}
# return ['aa']
```

## Complexity Analysis

### Time Complexity: O(n! × n)

- **n! permutations**: For n characters, there are n! permutations
- **n to build each**: Each permutation requires n character concatenations
- **Examples**:
  - "ab": 2! × 2 = 4 operations
  - "abc": 3! × 3 = 18 operations
  - "biro": 4! × 4 = 96 operations

### Space Complexity: O(n! × n)

- **n! strings**: Store all permutations
- **n characters each**: Each string has length n
- **Recursion stack**: O(n) depth
- **Examples**:
  - "ab": 2 strings × 2 chars = 4 space
  - "biro": 24 strings × 4 chars = 96 space

## Comparison with itertools

### Code Comparison

**itertools approach (our kata)**:
```python
from itertools import permutations

def generate_anagrams(word):
    if not word:
        return [""]
    return list(set(''.join(p) for p in permutations(word)))
```
- **Lines**: 2 (excluding imports)
- **Readability**: Very high (clear intent)
- **Performance**: Fastest (C implementation)

**Recursive approach**:
```python
def generate_anagrams(word):
    if not word:
        return [""]
    if len(word) == 1:
        return [word]
    
    result = []
    for i in range(len(word)):
        first_char = word[i]
        remaining = word[:i] + word[i+1:]
        for perm in generate_anagrams(remaining):
            result.append(first_char + perm)
    
    return list(set(result))
```
- **Lines**: 12
- **Readability**: Medium (requires understanding recursion)
- **Performance**: Slower (Python recursion overhead)

### When to Use Each

| Approach | Use When |
|----------|----------|
| **itertools** | Production code, Pythonic solutions, Performance matters |
| **Recursive** | Interviews, Teaching recursion, No stdlib available |

## Testing the Recursive Implementation

All our existing tests would pass with the recursive implementation! Here's how you could verify:

```python
# Save the original implementation
def generate_anagrams_itertools(word):
    from itertools import permutations
    if not word:
        return [""]
    return list(set(''.join(p) for p in permutations(word)))

# Implement recursive version
def generate_anagrams_recursive(word):
    if not word:
        return [""]
    if len(word) == 1:
        return [word]
    
    result = []
    for i in range(len(word)):
        first_char = word[i]
        remaining = word[:i] + word[i+1:]
        for perm in generate_anagrams_recursive(remaining):
            result.append(first_char + perm)
    
    return list(set(result))

# Test they produce same results
test_words = ["", "a", "ab", "aa", "abc", "aab", "biro", "aaa"]
for word in test_words:
    iter_result = sorted(generate_anagrams_itertools(word))
    rec_result = sorted(generate_anagrams_recursive(word))
    assert iter_result == rec_result, f"Mismatch for '{word}'"
    print(f"✓ '{word}': {len(iter_result)} anagrams match")
```

## Alternative: Backtracking Approach

Another recursive approach uses backtracking to build permutations character by character:

```python
def generate_anagrams(word):
    """Generate anagrams using backtracking."""
    if not word:
        return [""]
    
    result = []
    
    def backtrack(current, remaining):
        # Base case: no more characters to add
        if not remaining:
            result.append(current)
            return
        
        # Try each remaining character as the next one
        for i in range(len(remaining)):
            # Add character at position i to current
            backtrack(
                current + remaining[i],
                remaining[:i] + remaining[i+1:]
            )
    
    backtrack("", word)
    return list(set(result))
```

This is more "interview-style" code but essentially the same algorithm.

## Common Interview Questions

### Q1: "Implement permutations without using libraries"
**Answer**: Use the recursive approach shown above.

### Q2: "How would you optimize for strings with many duplicate characters?"
**Answer**: 
- Track character counts instead of positions
- Only recurse when count > 0
- Avoids generating duplicates in the first place

```python
from collections import Counter

def generate_anagrams_optimized(word):
    if not word:
        return [""]
    
    result = []
    
    def backtrack(current, counts):
        if len(current) == len(word):
            result.append(current)
            return
        
        for char in counts:
            if counts[char] > 0:
                counts[char] -= 1
                backtrack(current + char, counts)
                counts[char] += 1
    
    backtrack("", Counter(word))
    return result
```

### Q3: "What's the time complexity?"
**Answer**: O(n! × n) because we generate n! permutations and each takes O(n) to build.

### Q4: "Can you do better?"
**Answer**: No, because we must generate all n! permutations, and each requires O(n) work. The problem is inherently O(n! × n).

## Key Takeaways

1. **Recursion pattern**: Pick first, recurse on rest, combine
2. **Base cases**: Empty string and single character
3. **Duplicate handling**: Use set at the end
4. **Performance**: itertools is faster but recursive is more educational
5. **Interviews**: Know how to implement without libraries

## Why We Chose itertools

For the TDD kata, we chose `itertools.permutations` because:

1. **Pythonic**: Using stdlib is idiomatic Python
2. **Simple**: 1 line vs 12 lines
3. **Clear intent**: Immediately obvious what we're doing
4. **Production-ready**: Battle-tested, optimized implementation
5. **Focus on TDD**: The kata is about TDD, not recursion

However, understanding the recursive implementation gives you:
- Deeper algorithm knowledge
- Interview preparation
- Language-agnostic skills
- Appreciation for Python's stdlib!

---

**Generated with**: [Claude Code](https://claude.com/claude-code)  
**Last Updated**: November 2025  
**Author**: Tom Spencer
