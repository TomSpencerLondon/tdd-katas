# Stats Calculator Kata - TDD Journey Summary

## Overview

**Kata**: Stats Calculator  
**Language**: Python 3.10+  
**Testing Framework**: pytest  
**Final Results**: 10 tests passing, 100% code coverage  
**Implementation**: 11 lines total (4 statements of actual logic)

## Problem Statement

Create a function that calculates statistics (min, max, count, average) for a sequence of integers.

**Function signature**: `calculate_stats(numbers) -> dict`

**Return format**:
```python
{
    'min': <smallest value or None>,
    'max': <largest value or None>,
    'count': <number of elements>,
    'average': <mean value or None>
}
```

## TDD Journey - Four Phases

### Phase A: Empty/Single Element (Base Cases)

#### Test 1: Empty Sequence
**RED**: Test expects `{'min': None, 'max': None, 'count': 0, 'average': None}`
```python
def test_empty_sequence():
    result = calculate_stats([])
    assert result == {'min': None, 'max': None, 'count': 0, 'average': None}
```

**GREEN**: Hardcoded return value
```python
def calculate_stats(numbers):
    return {'min': None, 'max': None, 'count': 0, 'average': None}
```

**Commit**: Phase A - Test 1 complete

#### Test 2: Single Element [5]
**RED**: Test expects all stats to equal 5
```python
def test_single_element():
    result = calculate_stats([5])
    assert result == {'min': 5, 'max': 5, 'count': 1, 'average': 5.0}
```

**GREEN**: Added conditional logic
```python
def calculate_stats(numbers):
    if not numbers:
        return {'min': None, 'max': None, 'count': 0, 'average': None}
    
    first = numbers[0]
    return {'min': first, 'max': first, 'count': 1, 'average': float(first)}
```

**Commit**: Phase A - Test 2 complete

**Phase A Key Learning**: Start with simplest cases. Fake it till you make it!

---

### Phase B: Simple Sequences (2-3 elements)

#### Test 3: Two Elements [1, 2] - THE KEY MOMENT
**RED**: Test expects correct min/max/count/average
```python
def test_two_elements():
    result = calculate_stats([1, 2])
    assert result == {'min': 1, 'max': 2, 'count': 2, 'average': 1.5}
```

**GREEN**: Implemented REAL calculation logic
```python
def calculate_stats(numbers):
    if not numbers:
        return {'min': None, 'max': None, 'count': 0, 'average': None}
    
    return {
        'min': min(numbers),
        'max': max(numbers),
        'count': len(numbers),
        'average': sum(numbers) / len(numbers)
    }
```

**This is the breakthrough moment!** The algorithm emerged from the tests. We used Python's built-in functions which are:
- Simple and readable
- Handle all edge cases automatically
- Efficient (min/max are O(n), average is O(n))

**Commit**: Phase B - Test 3 complete (algorithm emerges!)

#### Test 4 & 5: Three Elements (Triangulation)
**GREEN → GREEN**: Both pass without code changes!

```python
def test_three_elements_ordered():
    result = calculate_stats([1, 2, 3])
    assert result == {'min': 1, 'max': 3, 'count': 3, 'average': 2.0}

def test_three_elements_unordered():
    result = calculate_stats([3, 1, 2])
    assert result == {'min': 1, 'max': 3, 'count': 3, 'average': 2.0}
```

**Commit**: Phase B - Triangulation complete

**Phase B Key Learning**: Let tests force the general solution. Triangulation validates it works.

---

### Phase C: Edge Cases (Negatives and Duplicates)

All four tests added, all pass **without any code changes**!

#### Test 6: Negative Numbers
```python
def test_negative_numbers():
    result = calculate_stats([-5, -1, -3])
    assert result == {'min': -5, 'max': -1, 'count': 3, 'average': -3.0}
```

#### Test 7: Mixed Positive/Negative
```python
def test_mixed_positive_negative():
    result = calculate_stats([-2, 0, 2])
    assert result == {'min': -2, 'max': 2, 'count': 3, 'average': 0.0}
```

#### Test 8: All Duplicates
```python
def test_all_duplicates():
    result = calculate_stats([2, 2, 2])
    assert result == {'min': 2, 'max': 2, 'count': 3, 'average': 2.0}
```

#### Test 9: Duplicates at Extremes
```python
def test_duplicates_at_extremes():
    result = calculate_stats([1, 2, 1])
    expected_average = 4 / 3
    assert result['min'] == 1
    assert result['max'] == 2
    assert result['count'] == 3
    assert abs(result['average'] - expected_average) < 0.01
```

**Commit**: Phase C - All edge cases pass

**Phase C Key Learning**: Python's built-in functions handle edge cases automatically! This validates our design choice.

---

### Phase D: Larger Sequences

#### Test 10: Larger Sequence [1..10]
**GREEN → GREEN**: Passes without code changes!

```python
def test_larger_sequence():
    result = calculate_stats([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
    assert result == {'min': 1, 'max': 10, 'count': 10, 'average': 5.5}
```

**Commit**: Phase D - Final test passes. Kata complete!

**Phase D Key Learning**: Algorithm scales correctly without modification.

---

## Final Implementation

```python
def calculate_stats(numbers):
    """Calculate statistics for a sequence of numbers.
    
    Args:
        numbers: List of integers
        
    Returns:
        Dict with min, max, count, and average
    """
    # Handle empty sequence
    if not numbers:
        return {'min': None, 'max': None, 'count': 0, 'average': None}
    
    # For non-empty sequences, calculate actual statistics
    return {
        'min': min(numbers),
        'max': max(numbers),
        'count': len(numbers),
        'average': sum(numbers) / len(numbers)
    }
```

**Lines of code**: 11 total (4 executable statements)  
**Complexity**: O(n) time, O(1) space  
**Readability**: Extremely clear and maintainable

## Git Commit History

1. **Initial setup**: Project structure, README, CLAUDE.md
2. **Phase A - Test 1**: Empty sequence (hardcoded return)
3. **Phase A - Test 2**: Single element (conditional logic)
4. **Phase B - Test 3**: Two elements (real implementation emerges!)
5. **Phase B - Tests 4-5**: Triangulation (validates general solution)
6. **Phase C - Tests 6-9**: Edge cases (all pass, no code changes)
7. **Phase D - Test 10**: Larger sequence (validates scalability)
8. **Presentation**: KATA_SUMMARY.md and PowerPoint

## Test Results

```
============================= test session starts ==============================
collected 10 items

src/tests/test_stats_calculator.py::test_empty_sequence PASSED           [ 10%]
src/tests/test_stats_calculator.py::test_single_element PASSED           [ 20%]
src/tests/test_stats_calculator.py::test_two_elements PASSED             [ 30%]
src/tests/test_stats_calculator.py::test_three_elements_ordered PASSED   [ 40%]
src/tests/test_stats_calculator.py::test_three_elements_unordered PASSED [ 50%]
src/tests/test_stats_calculator.py::test_negative_numbers PASSED         [ 60%]
src/tests/test_stats_calculator.py::test_mixed_positive_negative PASSED  [ 70%]
src/tests/test_stats_calculator.py::test_all_duplicates PASSED           [ 80%]
src/tests/test_stats_calculator.py::test_duplicates_at_extremes PASSED   [ 90%]
src/tests/test_stats_calculator.py::test_larger_sequence PASSED          [100%]

================================ tests coverage ================================
Name                      Stmts   Miss    Cover   Missing
---------------------------------------------------------
src/stats_calculator.py       4      0  100.00%
---------------------------------------------------------
TOTAL                         4      0  100.00%
============================== 10 passed in 0.03s ==============================
```

## Key TDD Principles Demonstrated

### 1. Red-Green-Refactor Cycle
- **Red**: Write failing test first
- **Green**: Make it pass with simplest code
- **Refactor**: Improve without changing behavior
- We followed this religiously for each test

### 2. Fake It Till You Make It
- Started with hardcoded return for empty sequence
- Only generalized when tests forced us to

### 3. Triangulation
- Multiple similar tests (3 ordered, 3 unordered, edge cases)
- Validates that general solution works for all cases

### 4. Let Algorithm Emerge
- Didn't design the solution upfront
- Test 3 (two elements) forced the real implementation
- Simple solution using built-in functions emerged naturally

### 5. Small Steps
- One test at a time
- Frequent commits documenting progress
- Never jumped ahead

### 6. Edge Cases Validate Design
- All edge cases passed without code changes
- Confirms we chose the right abstraction (built-in functions)

## Comparison to Other Katas

| Kata | Tests | Coverage | LOC | Key Learning |
|------|-------|----------|-----|--------------|
| FizzBuzz | 10 | 100% | ~20 | Order of conditions matters |
| Leap Year | 13 | 100% | 8 | Specific → general rules |
| Fibonacci | 9 | 100% | 7 | Algorithm discovery |
| **Stats Calc** | **10** | **100%** | **11** | **Built-ins handle complexity** |

## What Makes This Kata Special

1. **Simplicity through built-ins**: Python's min(), max(), sum(), len() do all the work
2. **Edge cases for free**: Built-in functions handle negatives, duplicates, etc.
3. **Dictionary return**: Clean, extensible API design
4. **Real-world relevance**: Statistics calculation is a common task

## Alternative Approaches (Not Taken)

### Approach 1: Single Pass with Manual Tracking
```python
def calculate_stats(numbers):
    if not numbers:
        return {'min': None, 'max': None, 'count': 0, 'average': None}
    
    min_val = max_val = numbers[0]
    total = 0
    
    for num in numbers:
        if num < min_val:
            min_val = num
        if num > max_val:
            max_val = num
        total += num
    
    return {
        'min': min_val,
        'max': max_val,
        'count': len(numbers),
        'average': total / len(numbers)
    }
```
**Pros**: Single pass through data  
**Cons**: More complex, harder to read, same O(n) complexity  
**Verdict**: Built-in approach is better for readability

### Approach 2: Class-Based
```python
class Stats:
    def __init__(self, numbers):
        self.numbers = numbers
    
    @property
    def min(self):
        return min(self.numbers) if self.numbers else None
    
    # ... etc
```
**Pros**: More object-oriented  
**Cons**: Overkill for simple calculation, more code  
**Verdict**: Function is sufficient for this use case

## Key Learnings

### Technical
1. **Python built-ins are powerful**: min(), max(), sum(), len() handle edge cases
2. **Simple is better**: 4 statements vs manual tracking
3. **Dictionary returns**: Clean API for multiple values
4. **Division in Python 3**: `/` always returns float (good for average)

### TDD Process
1. **Hardcoding is OK early**: Gets the TDD rhythm going
2. **Breakthrough test**: Test 3 forced the real solution
3. **Triangulation validates**: Tests 4-9 confirmed correctness
4. **Edge cases as validation**: All passing without changes confirms good design

### Design
1. **Chose right abstractions**: Built-in functions over manual loops
2. **Handle empty early**: Simplifies rest of logic
3. **Type hints would help**: Could add for production code
4. **Extensible**: Easy to add median, mode, etc.

## Usage Examples

```python
# Basic usage
stats = calculate_stats([1, 2, 3, 4, 5])
# {'min': 1, 'max': 5, 'count': 5, 'average': 3.0}

# Empty list
stats = calculate_stats([])
# {'min': None, 'max': None, 'count': 0, 'average': None}

# Negative numbers
stats = calculate_stats([-10, -5, 0, 5, 10])
# {'min': -10, 'max': 10, 'count': 5, 'average': 0.0}

# All same
stats = calculate_stats([7, 7, 7])
# {'min': 7, 'max': 7, 'count': 3, 'average': 7.0}
```

## Possible Extensions

1. **Add median calculation**: Requires sorting
2. **Add mode calculation**: Most frequent value
3. **Add standard deviation**: Measure of spread
4. **Add variance**: Square of standard deviation
5. **Support for floating point**: Currently assumes integers
6. **Type hints**: Add for better IDE support
7. **Error handling**: Validate input is list of numbers
8. **Performance optimization**: For very large sequences, consider numpy

## Conclusion

The Stats Calculator kata beautifully demonstrates how TDD leads to simple, correct solutions. By letting the tests drive the implementation, we discovered that Python's built-in functions provide everything we need. The result is 11 lines of highly readable, fully tested code that handles all edge cases.

**Most Important Lesson**: Don't over-engineer. The simplest solution is often the best.

---

**Generated with**: [Claude Code](https://claude.com/claude-code)  
**Last Updated**: November 2025  
**Author**: Tom Spencer
