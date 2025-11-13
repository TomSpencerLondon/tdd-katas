# Statistics Explained: Mean, Median, Mode

## Overview

When analyzing data, there are three main measures of **central tendency** (the "middle" or "typical" value):
- **Mean** (arithmetic average)
- **Median** (middle value when sorted)
- **Mode** (most frequent value)

Our Stats Calculator kata implements **mean** (average). This document explains all three measures and why we chose mean.

---

## Mean (Arithmetic Average)

### Definition
The **mean** is the sum of all values divided by the count of values.

### Formula
```
mean = sum(values) / count(values)
```

### Our Implementation
```python
'average': sum(numbers) / len(numbers)
```

### Examples

**Example 1: Simple sequence [1, 2, 3, 4, 5]**
```
mean = (1 + 2 + 3 + 4 + 5) / 5
mean = 15 / 5
mean = 3.0
```

**Example 2: With duplicates [2, 4, 4, 4, 5, 5, 7, 9]**
```
mean = (2 + 4 + 4 + 4 + 5 + 5 + 7 + 9) / 8
mean = 40 / 8
mean = 5.0
```

**Example 3: With negatives [-10, -5, 0, 5, 10]**
```
mean = (-10 + -5 + 0 + 5 + 10) / 5
mean = 0 / 5
mean = 0.0
```

### Characteristics
- ✅ **Uses all values**: Every number contributes to the result
- ✅ **Easy to calculate**: Simple formula
- ✅ **Algebraic properties**: Useful for further calculations
- ⚠️ **Sensitive to outliers**: Extreme values can skew the mean
- ⚠️ **May not exist in dataset**: Mean can be a value that's not in the list

### When to Use Mean
- When you want to know the "typical" value
- When all values should contribute equally
- When data is relatively evenly distributed
- For continuous data (measurements, temperatures, etc.)

---

## Median (Middle Value)

### Definition
The **median** is the middle value when all values are sorted. If there's an even count, it's the average of the two middle values.

### Algorithm
```
1. Sort the values
2. If odd count: return middle value
3. If even count: return average of two middle values
```

### Implementation (Not in our kata)
```python
def calculate_median(numbers):
    if not numbers:
        return None
    
    sorted_nums = sorted(numbers)
    n = len(sorted_nums)
    mid = n // 2
    
    if n % 2 == 0:
        # Even count: average of two middle values
        return (sorted_nums[mid - 1] + sorted_nums[mid]) / 2
    else:
        # Odd count: middle value
        return sorted_nums[mid]
```

### Examples

**Example 1: Odd count [1, 2, 3, 4, 5]**
```
Sorted: [1, 2, 3, 4, 5]
Middle position: 2 (3rd element)
Median = 3
```

**Example 2: Even count [1, 2, 3, 4]**
```
Sorted: [1, 2, 3, 4]
Middle positions: 1 and 2 (2nd and 3rd elements)
Median = (2 + 3) / 2 = 2.5
```

**Example 3: With outlier [1, 2, 3, 4, 100]**
```
Sorted: [1, 2, 3, 4, 100]
Middle position: 2 (3rd element)
Median = 3
(Note: Outlier 100 doesn't affect median!)
```

**Example 4: With duplicates [2, 4, 4, 4, 5, 5, 7, 9]**
```
Already sorted: [2, 4, 4, 4, 5, 5, 7, 9]
Middle positions: 3 and 4 (4th and 5th elements)
Median = (4 + 5) / 2 = 4.5
```

### Characteristics
- ✅ **Robust to outliers**: Extreme values don't affect it much
- ✅ **Represents "middle"**: Half values above, half below
- ✅ **Works well for skewed data**: Better than mean for income, housing prices
- ⚠️ **Requires sorting**: More computationally expensive (O(n log n))
- ⚠️ **Doesn't use all information**: Only looks at middle value(s)

### When to Use Median
- When data has outliers (e.g., salaries, house prices)
- When data is skewed (not evenly distributed)
- When you want the "middle" value, not the average
- For ordinal data (rankings, ratings)

---

## Mode (Most Frequent Value)

### Definition
The **mode** is the value that appears most frequently. A dataset can have:
- **No mode**: All values appear with same frequency
- **One mode** (unimodal): One value appears most often
- **Multiple modes** (multimodal): Multiple values tied for highest frequency

### Implementation (Not in our kata)
```python
from collections import Counter

def calculate_mode(numbers):
    if not numbers:
        return None
    
    counts = Counter(numbers)
    max_count = max(counts.values())
    
    # Find all values with max count
    modes = [num for num, count in counts.items() if count == max_count]
    
    # If all values appear once, no mode
    if max_count == 1:
        return None
    
    # Return single mode or list of modes
    return modes[0] if len(modes) == 1 else modes
```

### Examples

**Example 1: Single mode [1, 2, 2, 2, 3, 4, 5]**
```
Counts: {1: 1, 2: 3, 3: 1, 4: 1, 5: 1}
Most frequent: 2 (appears 3 times)
Mode = 2
```

**Example 2: Multiple modes [1, 1, 2, 2, 3]**
```
Counts: {1: 2, 2: 2, 3: 1}
Most frequent: 1 and 2 (both appear 2 times)
Mode = [1, 2] (bimodal)
```

**Example 3: No mode [1, 2, 3, 4, 5]**
```
Counts: {1: 1, 2: 1, 3: 1, 4: 1, 5: 1}
All appear once
Mode = None (or all values are modes)
```

**Example 4: Our duplicate test [2, 2, 2]**
```
Counts: {2: 3}
Most frequent: 2 (appears 3 times)
Mode = 2
```

### Characteristics
- ✅ **Shows popularity**: What's most common
- ✅ **Works for non-numeric data**: Colors, categories, etc.
- ✅ **Easy to understand**: "Most common value"
- ⚠️ **Can be ambiguous**: Multiple modes or no mode
- ⚠️ **Doesn't use all information**: Only looks at frequency
- ⚠️ **Sensitive to binning**: How you group data affects mode

### When to Use Mode
- For categorical data (favorite colors, product preferences)
- When you want the "most common" value
- For discrete data with repeated values
- In manufacturing (most common defect)

---

## Comparison Table

| Measure | Formula/Algorithm | Complexity | Outlier Sensitive | Best For |
|---------|------------------|------------|-------------------|----------|
| **Mean** | sum / count | O(n) | ✅ Yes | Continuous data, even distribution |
| **Median** | Middle when sorted | O(n log n) | ❌ No | Skewed data, ordinal data |
| **Mode** | Most frequent | O(n) | ❌ No | Categorical data, discrete values |

---

## Real-World Example: Test Scores

Consider test scores: [55, 60, 65, 70, 75, 80, 85, 90, 95, 100]

**Mean (Average)**:
```
mean = (55 + 60 + 65 + 70 + 75 + 80 + 85 + 90 + 95 + 100) / 10
mean = 775 / 10 = 77.5
```
Interpretation: The average score is 77.5

**Median (Middle)**:
```
Sorted: [55, 60, 65, 70, 75, 80, 85, 90, 95, 100]
Middle positions: 5th and 6th (75 and 80)
median = (75 + 80) / 2 = 77.5
```
Interpretation: Half scored below 77.5, half above

**Mode (Most Common)**:
```
All scores appear once
Mode = None (no mode)
```
Interpretation: No score was more common than others

---

## Real-World Example: Salaries (With Outlier)

Consider salaries: [$30k, $35k, $40k, $45k, $50k, $55k, $60k, $65k, $70k, $500k]

**Mean (Average)**:
```
mean = (30 + 35 + 40 + 45 + 50 + 55 + 60 + 65 + 70 + 500) / 10
mean = 950 / 10 = $95k
```
⚠️ **Problem**: The $500k outlier (CEO) skews the mean!  
Most employees earn much less than $95k.

**Median (Middle)**:
```
Sorted: [30, 35, 40, 45, 50, 55, 60, 65, 70, 500]
Middle positions: 5th and 6th (50 and 55)
median = (50 + 55) / 2 = $52.5k
```
✅ **Better**: $52.5k represents the "typical" employee better.  
The outlier doesn't affect it!

**Mode (Most Common)**:
```
All salaries appear once
Mode = None
```

**Conclusion**: For salary data, median is better than mean because it's not affected by the CEO's outlier salary.

---

## Why We Chose Mean for Our Kata

### Decision Rationale

1. **Problem specification**: The kata asks for "average value" which typically means mean
2. **Simplicity**: Mean is the simplest to calculate (O(n) vs O(n log n) for median)
3. **Common usage**: "Average" in everyday language usually means mean
4. **TDD friendly**: Simple formula makes testing straightforward
5. **Python built-ins**: `sum()` and `len()` are perfect for this

### Code Comparison

**Mean (What we implemented)**:
```python
'average': sum(numbers) / len(numbers)  # One line!
```

**Median (More complex)**:
```python
def get_median(numbers):
    sorted_nums = sorted(numbers)
    n = len(sorted_nums)
    mid = n // 2
    if n % 2 == 0:
        return (sorted_nums[mid - 1] + sorted_nums[mid]) / 2
    return sorted_nums[mid]

'median': get_median(numbers)  # Multiple lines, sorting required
```

**Mode (Even more complex)**:
```python
from collections import Counter

def get_mode(numbers):
    counts = Counter(numbers)
    max_count = max(counts.values())
    modes = [num for num, count in counts.items() if count == max_count]
    return modes[0] if len(modes) == 1 else modes

'mode': get_mode(numbers)  # Requires Counter, handling multiple modes
```

---

## Extending the Kata

If you wanted to add median and mode to the kata, here's how:

### Extended Implementation
```python
from collections import Counter

def calculate_stats(numbers):
    """Calculate statistics including mean, median, and mode."""
    if not numbers:
        return {
            'min': None,
            'max': None,
            'count': 0,
            'mean': None,
            'median': None,
            'mode': None
        }
    
    # Basic stats (what we have)
    result = {
        'min': min(numbers),
        'max': max(numbers),
        'count': len(numbers),
        'mean': sum(numbers) / len(numbers)
    }
    
    # Median
    sorted_nums = sorted(numbers)
    n = len(sorted_nums)
    mid = n // 2
    if n % 2 == 0:
        result['median'] = (sorted_nums[mid - 1] + sorted_nums[mid]) / 2
    else:
        result['median'] = sorted_nums[mid]
    
    # Mode
    counts = Counter(numbers)
    max_count = max(counts.values())
    if max_count == 1:
        result['mode'] = None  # No mode if all appear once
    else:
        modes = [num for num, count in counts.items() if count == max_count]
        result['mode'] = modes[0] if len(modes) == 1 else modes
    
    return result
```

### Example Output
```python
>>> calculate_stats([1, 2, 2, 3, 4, 5, 100])
{
    'min': 1,
    'max': 100,
    'count': 7,
    'mean': 16.714285714285715,  # Affected by outlier 100
    'median': 3,                   # Not affected by outlier
    'mode': 2                      # Most frequent value
}
```

---

## Key Takeaways

1. **Mean** = sum ÷ count
   - Use for: Even distributions, continuous data
   - Warning: Sensitive to outliers

2. **Median** = Middle value when sorted
   - Use for: Skewed data, ordinal data, data with outliers
   - Warning: Requires sorting (slower)

3. **Mode** = Most frequent value
   - Use for: Categorical data, finding "most common"
   - Warning: Can have no mode or multiple modes

4. **Our kata uses mean** because:
   - It's what "average" typically means
   - Simple to calculate and test
   - Works well with Python built-ins
   - Appropriate for the problem domain

5. **No single measure is always best**:
   - Consider your data and what question you're answering
   - Sometimes report multiple measures
   - Understand the trade-offs

---

## Further Reading

- **Statistics Basics**: Khan Academy, Statistics Course
- **Python Statistics**: `statistics` module in Python standard library
- **NumPy**: For advanced statistical operations
- **Pandas**: For data analysis with built-in mean, median, mode

---

**Generated with**: [Claude Code](https://claude.com/claude-code)  
**Last Updated**: November 2025  
**Author**: Tom Spencer
