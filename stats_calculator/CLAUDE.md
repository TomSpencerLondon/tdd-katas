# Stats Calculator Kata - TDD Guide for Claude

This guide provides step-by-step instructions for implementing the Stats Calculator kata using Test-Driven Development (TDD).

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

### Phase A: Empty/Single Element (Base Cases)

**Test 1: Empty sequence**
```python
def test_empty_sequence():
    """Test that empty sequence returns None for all stats."""
    result = calculate_stats([])
    assert result == {'min': None, 'max': None, 'count': 0, 'average': None}
```
- **Make it GREEN**: Return hardcoded dict
- **Commit**: "Phase A: Test empty sequence - hardcoded return"

**Test 2: Single element**
```python
def test_single_element():
    """Test that single element [5] returns all stats as 5."""
    result = calculate_stats([5])
    assert result == {'min': 5, 'max': 5, 'count': 1, 'average': 5.0}
```
- **Make it GREEN**: Check if empty, else use first element
- **Commit**: "Phase A: Test single element [5]"

---

### Phase B: Simple Sequences (2-3 elements)

**Test 3: Two elements**
```python
def test_two_elements():
    """Test that [1, 2] calculates stats correctly."""
    result = calculate_stats([1, 2])
    assert result == {'min': 1, 'max': 2, 'count': 2, 'average': 1.5}
```
- **Make it GREEN**: This forces you to implement actual min(), max(), len(), sum() logic
- **Commit**: "Phase B: Test two elements [1, 2] - implement real calculations"

**Test 4: Three elements ordered**
```python
def test_three_elements_ordered():
    """Test that [1, 2, 3] calculates stats correctly (triangulation)."""
    result = calculate_stats([1, 2, 3])
    assert result == {'min': 1, 'max': 3, 'count': 3, 'average': 2.0}
```
- **Make it GREEN**: Should pass with existing code
- **Commit**: "Phase B: Test three elements [1, 2, 3] - triangulation"

**Test 5: Three elements unordered**
```python
def test_three_elements_unordered():
    """Test that [3, 1, 2] calculates stats correctly (order independence)."""
    result = calculate_stats([3, 1, 2])
    assert result == {'min': 1, 'max': 3, 'count': 3, 'average': 2.0}
```
- **Make it GREEN**: Should pass with existing code
- **Commit**: "Phase B: Test unordered [3, 1, 2] - verify order independence"

---

### Phase C: Edge Cases (Negatives and Duplicates)

**Test 6: Negative numbers**
```python
def test_negative_numbers():
    """Test that negative numbers [-5, -1, -3] work correctly."""
    result = calculate_stats([-5, -1, -3])
    assert result == {'min': -5, 'max': -1, 'count': 3, 'average': -3.0}
```
- **Make it GREEN**: Should pass with existing code (min/max handle negatives)
- **Commit**: "Phase C: Test negative numbers [-5, -1, -3]"

**Test 7: Mixed positive and negative**
```python
def test_mixed_positive_negative():
    """Test that mixed [-2, 0, 2] works correctly."""
    result = calculate_stats([-2, 0, 2])
    assert result == {'min': -2, 'max': 2, 'count': 3, 'average': 0.0}
```
- **Make it GREEN**: Should pass with existing code
- **Commit**: "Phase C: Test mixed positive/negative [-2, 0, 2]"

**Test 8: All duplicates**
```python
def test_all_duplicates():
    """Test that duplicates [2, 2, 2] work correctly."""
    result = calculate_stats([2, 2, 2])
    assert result == {'min': 2, 'max': 2, 'count': 3, 'average': 2.0}
```
- **Make it GREEN**: Should pass with existing code
- **Commit**: "Phase C: Test duplicates [2, 2, 2]"

**Test 9: Duplicates at extremes**
```python
def test_duplicates_at_extremes():
    """Test that [1, 2, 1] handles duplicates at min correctly."""
    result = calculate_stats([1, 2, 1])
    expected_average = 4 / 3  # ~1.333...
    assert result['min'] == 1
    assert result['max'] == 2
    assert result['count'] == 3
    assert abs(result['average'] - expected_average) < 0.01
```
- **Make it GREEN**: Should pass with existing code
- **Commit**: "Phase C: Test duplicates at extremes [1, 2, 1]"

---

### Phase D: Larger Sequences

**Test 10: Larger sequence**
```python
def test_larger_sequence():
    """Test that larger sequence [1..10] works correctly."""
    result = calculate_stats([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
    assert result == {'min': 1, 'max': 10, 'count': 10, 'average': 5.5}
```
- **Make it GREEN**: Should pass with existing code
- **Commit**: "Phase D: Test larger sequence [1..10]"

---

## Final Implementation Should Look Like:

```python
def calculate_stats(numbers):
    """Calculate statistics for a sequence of numbers.
    
    Args:
        numbers: List of integers
        
    Returns:
        Dict with min, max, count, and average
    """
    if not numbers:
        return {'min': None, 'max': None, 'count': 0, 'average': None}
    
    return {
        'min': min(numbers),
        'max': max(numbers),
        'count': len(numbers),
        'average': sum(numbers) / len(numbers)
    }
```

## Expected Results

- **10 tests**: All passing
- **100% coverage**: Complete test coverage
- **~10 lines of code**: Simple, elegant solution
- **Clear commits**: Each phase well-documented

## Common Pitfalls to Avoid

1. **Don't design the whole solution upfront**: Let tests drive the implementation
2. **Don't skip the hardcoding step**: It's important for TDD rhythm
3. **Don't write multiple tests at once**: One test at a time
4. **Don't forget to commit**: Regular commits document the journey
5. **Don't over-engineer**: Use Python's built-in functions

## Key Learnings

1. **TDD drives simple solutions**: Built-in functions (min, max, sum, len) are sufficient
2. **Edge cases first**: Handling empty sequence early simplifies later tests
3. **Triangulation validates**: Multiple similar tests confirm correctness
4. **Return structure matters**: Dictionary is clear and extensible
5. **Python's strengths**: Built-in functions handle edge cases automatically

---

**Ready to start?** Begin with Phase A, Test 1!
