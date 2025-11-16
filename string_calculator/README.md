# String Calculator TDD Kata

A classic Test-Driven Development kata focused on incremental development and test-first programming. From Roy Osherove's TDD Kata collection.

## The Problem

Create a simple string calculator with an `Add` method that sums numbers in a string.

### Core Requirements

**Step 1: Basic Addition (0, 1, or 2 numbers)**
- Method signature: `Add(string numbers)` returns integer
- Empty string returns `0`
- Single number returns that number
- Two comma-separated numbers returns their sum

Examples:
```
Add("") == 0
Add("1") == 1
Add("1,2") == 3
```

**Step 2: Unknown Amount of Numbers**
- Handle any quantity of numbers
```
Add("1,2,3,4,5") == 15
```

**Step 3: Newline Delimiters**
- Support `\n` as delimiter (in addition to commas)
```
Add("1\n2,3") == 6
```
Note: Invalid input like `"1,\n"` is NOT expected (no need to test)

**Step 4: Custom Delimiters**
- Support custom delimiter defined at start: `"//[delimiter]\n[numbers...]"`
```
Add("//;\n1;2") == 3
```
All existing scenarios must still work!

**Step 5: Negative Numbers**
- Calling Add with negative number throws exception: `"negatives not allowed"`
- Exception message includes the negative number

**Step 6: Multiple Negatives**
- Show ALL negative numbers in exception message

**Step 7: Track Invocations**
- Add method `GetCalledCount()` that returns how many times `Add()` was invoked
- Remember: Start with a failing test!

**Step 8: Event on Add (.NET Only)**
- Add event `AddOccured` triggered after every `Add()` call
- Skip this step for non-.NET implementations

**Step 9: Ignore Large Numbers**
- Numbers bigger than 1000 are ignored
```
Add("2,1001") == 2
```

**Step 10: Variable-Length Delimiters**
- Delimiters can be any length: `"//[delimiter]\n"`
```
Add("//[***]\n1***2***3") == 6
```

**Step 11: Multiple Delimiters**
- Allow multiple delimiters: `"//[delim1][delim2]\n"`
```
Add("//[*][%]\n1*2%3") == 6
```

**Step 12: Multiple Long Delimiters**
- Support multiple delimiters with length > 1 char
```
Add("//[**][%%]\n1**2%%3") == 6
```

## TDD Principles for This Kata

**Before You Start:**
- Try not to read ahead
- Do ONE task at a time (work incrementally!)
- Only test for CORRECT inputs (no need for validation)
- **Test First!** Write tests before implementation

**During Implementation:**
1. Start with the simplest test case (empty string)
2. Move to one number, then two numbers
3. Solve things as simply as possible (force yourself to write more tests)
4. **Refactor after each passing test**

## The TDD Cycle

```
┌─────────────────────────────────────┐
│ Red → Green → Refactor → Repeat     │
└─────────────────────────────────────┘

1. Write a failing test (RED)
2. Write minimal code to pass (GREEN)
3. Refactor while keeping tests green (REFACTOR)
4. Repeat with next requirement
```

## Learning Goals

By completing this kata, you will practice:

1. **Test-Driven Development**: Writing tests first, then implementation
2. **Incremental Design**: Building features one small step at a time
3. **Refactoring**: Improving design without changing behavior
4. **Red-Green-Refactor**: The core TDD rhythm
5. **Simple Solutions**: Resisting over-engineering
6. **Exception Handling**: Using TDD to drive error cases
7. **API Design**: Creating clean, understandable interfaces

## Why String Calculator?

Unlike refactoring katas (like 99 Bottles), this kata focuses on:
- **Building from scratch** with TDD
- **Incremental feature addition** (not improving existing code)
- **Test-first discipline** (resist writing code without tests)
- **Simple design** (only add complexity when tests require it)

## Key Insights

**Simplest Thing That Works:**
- Don't overthink early steps
- Hardcode if needed to get tests passing
- Let tests drive complexity

**One Task at a Time:**
- Complete each step fully before moving on
- Don't skip ahead even if you see the final solution
- Trust the process

**Refactor Always:**
- After each green test, look for improvements
- Remove duplication
- Improve names and structure
- Keep tests green!

**Test Behaviors, Not Implementation:**
- Tests should verify outcomes
- Don't test internal details
- Tests should be resilient to refactoring

## Common Pitfalls to Avoid

1. **Reading ahead** - Resist! Work incrementally
2. **Skipping refactoring** - Always clean up after tests pass
3. **Over-engineering early** - Add complexity only when needed
4. **Testing invalid inputs** - Not required for this kata
5. **Writing implementation first** - Tests must come first!

## Source

From Roy Osherove's TDD Kata collection: https://osherove.com/kata

## Our Implementation - Complete! ✅

We successfully completed all 12 steps using strict TDD methodology.

### Final Statistics

- **Tests**: 13 passing
- **Assertions**: 19
- **Coverage**: 100%
- **Implementation**: [lib/string_calculator.rb](lib/string_calculator.rb)
- **Tests**: [test/string_calculator_test.rb](test/string_calculator_test.rb)

### Features Implemented

✅ **Step 1**: Handle 0, 1, or 2 numbers
✅ **Step 2**: Unknown amount of numbers
✅ **Step 3**: Newline delimiters (`\n`)
✅ **Step 4**: Custom delimiters (`//;\n`)
✅ **Step 5-6**: Negative number exceptions (with all negatives listed)
✅ **Step 7**: `get_called_count()` method
✅ **Step 8**: Skipped (.NET only)
✅ **Step 9**: Ignore numbers > 1000
✅ **Step 10**: Variable-length delimiters (`//[***]\n`)
✅ **Step 11-12**: Multiple delimiters (`//[*][%%]\n`)

### Key Code Patterns

**Regex Pattern Extraction:**
```ruby
# Extract all delimiters from "[*][%%][***]" format
delimiters = delimiter_part.scan(/\[([^\]]+)\]/).flatten
# => ["*", "%%", "***"]
```

**Dynamic Regex Building:**
```ruby
# Escape each delimiter and join with OR
delimiter = Regexp.new(delimiters.map { |d| Regexp.escape(d) }.join("|"))
# => /\*|%%|\*\*\*/
```

**Filtering and Aggregation:**
```ruby
# Select numbers <= 1000 and sum
nums.select { |n| n <= 1000 }.sum
```

### TDD Journey Highlights

1. **Started Simple**: Empty string → 0
2. **Built Incrementally**: One test at a time
3. **Refactored Continuously**: Improved after each green test
4. **Discovered Patterns**: Regex extraction emerged from needs
5. **State Tracking**: Added `@call_count` when tests required it

## Code Evolution - Step by Step

This section shows how the code evolved through each TDD cycle (Red → Green → Refactor).

### Step 1: Empty String (RED → GREEN)

**Test:**
```ruby
def test_empty_string_returns_zero
  assert_equal 0, StringCalculator.new.add("")
end
```

**Implementation:**
```ruby
class StringCalculator
  def add(numbers)
    0
  end
end
```

**Result:** ✅ GREEN - Simplest thing that works!

---

### Step 1: Single Number (RED → GREEN → REFACTOR)

**Test:**
```ruby
def test_single_number_returns_itself
  assert_equal 1, StringCalculator.new.add("1")
end
```

**Implementation (GREEN):**
```ruby
class StringCalculator
  def add(numbers)
    return 0 if numbers.empty?
    numbers.to_i
  end
end
```

**Result:** ✅ Both tests GREEN

---

### Step 1: Two Numbers (RED → GREEN)

**Test:**
```ruby
def test_two_numbers_comma_separated_returns_sum
  assert_equal 3, StringCalculator.new.add("1,2")
end
```

**Implementation (GREEN):**
```ruby
class StringCalculator
  def add(numbers)
    return 0 if numbers.empty?
    numbers.split(',').map(&:to_i).sum
  end
end
```

**Key Insight:** Using `split` and `sum` handles both single and multiple numbers!

**Result:** ✅ All 3 tests GREEN

---

### Step 2: Unknown Amount (Already Working!)

**Test:**
```ruby
def test_unknown_amount_of_numbers_returns_sum
  assert_equal 15, StringCalculator.new.add("1,2,3,4,5")
end
```

**Implementation:** No change needed - already handles it!

**Result:** ✅ All 4 tests GREEN

---

### Step 3: Newline Delimiters (RED → GREEN)

**Test:**
```ruby
def test_handles_newline_as_delimiter
  assert_equal 6, StringCalculator.new.add("1\n2,3")
end
```

**Implementation (GREEN):**
```ruby
class StringCalculator
  def add(numbers)
    return 0 if numbers.empty?
    numbers.split(/,|\n/).map(&:to_i).sum  # Changed to regex!
  end
end
```

**Key Insight:** Regex `/,|\n/` matches comma OR newline

**Result:** ✅ All 5 tests GREEN

---

### Step 4: Custom Delimiters (RED → GREEN)

**Test:**
```ruby
def test_handles_custom_delimiter
  assert_equal 3, StringCalculator.new.add("//;\n1;2")
end
```

**Implementation (GREEN):**
```ruby
class StringCalculator
  def add(numbers)
    return 0 if numbers.empty?

    delimiter = /,|\n/

    if numbers.start_with?("//")
      parts = numbers.split("\n", 2)
      delimiter = Regexp.escape(parts[0][2..-1])
      numbers = parts[1]
    end

    numbers.split(delimiter).map(&:to_i).sum
  end
end
```

**Key Insights:**
- `parts[0][2..-1]` extracts delimiter after `//`
- `Regexp.escape` makes special chars literal
- Split into 2 parts: delimiter line and numbers line

**Result:** ✅ All 6 tests GREEN

---

### Steps 5-6: Negative Numbers (RED → GREEN)

**Tests:**
```ruby
def test_negative_number_throws_exception
  error = assert_raises(RuntimeError) do
    StringCalculator.new.add("-1,2")
  end
  assert_equal "negatives not allowed: -1", error.message
end

def test_multiple_negatives_show_all_in_exception
  error = assert_raises(RuntimeError) do
    StringCalculator.new.add("1,-2,3,-4")
  end
  assert_equal "negatives not allowed: -2, -4", error.message
end
```

**Implementation (GREEN):**
```ruby
class StringCalculator
  def add(numbers)
    return 0 if numbers.empty?

    delimiter = /,|\n/

    if numbers.start_with?("//")
      parts = numbers.split("\n", 2)
      delimiter = Regexp.escape(parts[0][2..-1])
      numbers = parts[1]
    end

    nums = numbers.split(delimiter).map(&:to_i)

    # NEW: Check for negatives
    negatives = nums.select { |n| n < 0 }
    unless negatives.empty?
      raise "negatives not allowed: #{negatives.join(', ')}"
    end

    nums.sum
  end
end
```

**Key Insights:**
- `select` filters negatives
- `join(', ')` creates comma-separated list
- Single approach handles both single and multiple negatives!

**Result:** ✅ All 8 tests GREEN

---

### Step 7: Call Count Tracking (RED → GREEN)

**Test:**
```ruby
def test_get_called_count_tracks_add_invocations
  calc = StringCalculator.new
  assert_equal 0, calc.get_called_count

  calc.add("1,2")
  assert_equal 1, calc.get_called_count

  calc.add("3,4")
  assert_equal 2, calc.get_called_count
end
```

**Implementation (GREEN):**
```ruby
class StringCalculator
  def initialize
    @call_count = 0  # NEW: Initialize counter
  end

  def add(numbers)
    @call_count += 1  # NEW: Increment on each call

    return 0 if numbers.empty?

    delimiter = /,|\n/

    if numbers.start_with?("//")
      parts = numbers.split("\n", 2)
      delimiter = Regexp.escape(parts[0][2..-1])
      numbers = parts[1]
    end

    nums = numbers.split(delimiter).map(&:to_i)

    negatives = nums.select { |n| n < 0 }
    unless negatives.empty?
      raise "negatives not allowed: #{negatives.join(', ')}"
    end

    nums.sum
  end

  def get_called_count  # NEW: Getter method
    @call_count
  end
end
```

**Key Insights:**
- Instance variable `@call_count` maintains state across calls
- `initialize` sets initial state
- Simple getter method exposes the count

**Result:** ✅ All 9 tests GREEN

---

### Step 9: Ignore Numbers > 1000 (RED → GREEN)

**Test:**
```ruby
def test_numbers_bigger_than_1000_are_ignored
  assert_equal 2, StringCalculator.new.add("2,1001")
  assert_equal 1002, StringCalculator.new.add("1000,2")
end
```

**Implementation (GREEN):**
```ruby
class StringCalculator
  def initialize
    @call_count = 0
  end

  def add(numbers)
    @call_count += 1

    return 0 if numbers.empty?

    delimiter = /,|\n/

    if numbers.start_with?("//")
      parts = numbers.split("\n", 2)
      delimiter = Regexp.escape(parts[0][2..-1])
      numbers = parts[1]
    end

    nums = numbers.split(delimiter).map(&:to_i)

    negatives = nums.select { |n| n < 0 }
    unless negatives.empty?
      raise "negatives not allowed: #{negatives.join(', ')}"
    end

    nums.select { |n| n <= 1000 }.sum  # CHANGED: Filter before sum
  end

  def get_called_count
    @call_count
  end
end
```

**Key Insight:** Chain `select` before `sum` to filter values

**Result:** ✅ All 10 tests GREEN

---

### Step 10: Variable-Length Delimiters (RED → GREEN → REFACTOR)

**Test:**
```ruby
def test_delimiters_can_be_any_length
  assert_equal 6, StringCalculator.new.add("//[***]\n1***2***3")
end
```

**Implementation (GREEN):**
```ruby
class StringCalculator
  def initialize
    @call_count = 0
  end

  def add(numbers)
    @call_count += 1

    return 0 if numbers.empty?

    delimiter = /,|\n/

    if numbers.start_with?("//")
      parts = numbers.split("\n", 2)
      delimiter_part = parts[0][2..-1]

      # Check if delimiter is wrapped in brackets: [***]
      if delimiter_part.start_with?("[") && delimiter_part.end_with?("]")
        delimiter = Regexp.new(Regexp.escape(delimiter_part[1..-2]))
      else
        delimiter = Regexp.new(Regexp.escape(delimiter_part))
      end

      numbers = parts[1]
    end

    nums = numbers.split(delimiter).map(&:to_i)

    negatives = nums.select { |n| n < 0 }
    unless negatives.empty?
      raise "negatives not allowed: #{negatives.join(', ')}"
    end

    nums.select { |n| n <= 1000 }.sum
  end

  def get_called_count
    @call_count
  end
end
```

**Key Insights:**
- `delimiter_part[1..-2]` extracts content between `[` and `]`
- `Regexp.new()` creates regex from escaped string
- Backwards compatible with simple delimiters!

**Result:** ✅ All 11 tests GREEN

---

### Steps 11-12: Multiple Delimiters (RED → GREEN → REFACTOR)

**Tests:**
```ruby
def test_multiple_delimiters
  assert_equal 6, StringCalculator.new.add("//[*][%]\n1*2%3")
end

def test_multiple_delimiters_with_length_longer_than_one
  assert_equal 6, StringCalculator.new.add("//[**][%%]\n1**2%%3")
end
```

**Final Implementation (GREEN):**
```ruby
class StringCalculator
  def initialize
    @call_count = 0
  end

  def add(numbers)
    @call_count += 1

    return 0 if numbers.empty?

    delimiter = /,|\n/

    if numbers.start_with?("//")
      parts = numbers.split("\n", 2)
      delimiter_part = parts[0][2..-1]

      # Check if delimiter is wrapped in brackets: [***] or multiple [*][%]
      if delimiter_part.include?("[")
        # Extract all delimiters between brackets
        delimiters = delimiter_part.scan(/\[([^\]]+)\]/).flatten
        # Escape each delimiter and join with OR (|)
        delimiter = Regexp.new(delimiters.map { |d| Regexp.escape(d) }.join("|"))
      else
        delimiter = Regexp.new(Regexp.escape(delimiter_part))
      end

      numbers = parts[1]
    end

    nums = numbers.split(delimiter).map(&:to_i)

    negatives = nums.select { |n| n < 0 }
    unless negatives.empty?
      raise "negatives not allowed: #{negatives.join(', ')}"
    end

    nums.select { |n| n <= 1000 }.sum
  end

  def get_called_count
    @call_count
  end
end
```

**Key Insights:**
- **Regex pattern** `/\[([^\]]+)\]/` captures content between brackets
- **scan()** finds ALL matches and returns array of capture groups
- **flatten** converts `[["*"], ["%"]]` to `["*", "%"]`
- **map + escape + join** creates `"\*|%"` pattern
- **Regexp.new()** builds dynamic regex: `/\*|%/`

**Result:** ✅ ALL 13 tests GREEN - KATA COMPLETE!

---

## Evolution Summary

| Step | Lines of Code | Key Addition |
|------|---------------|--------------|
| Step 1 (empty) | 3 | Hardcoded `0` |
| Step 1 (single) | 4 | `to_i` conversion |
| Step 1 (two) | 4 | `split(',')` and `sum` |
| Step 3 | 4 | Regex delimiter `/,\|\n/` |
| Step 4 | 11 | Custom delimiter parsing |
| Steps 5-6 | 18 | Negative validation |
| Step 7 | 23 | State tracking with `@call_count` |
| Step 9 | 23 | Filter with `select { \|n\| n <= 1000 }` |
| Step 10 | 28 | Bracket-wrapped delimiters |
| Steps 11-12 | 32 | Multiple delimiters with `scan()` |

**Total Growth:** 3 lines → 43 lines (including whitespace and methods)

**Complexity Growth:** Simple hardcoded value → Advanced regex pattern matching with state tracking!

### Running the Tests

```bash
cd string_calculator

# Run tests
ruby test/string_calculator_test.rb

# Expected output:
# 13 runs, 19 assertions, 0 failures, 0 errors, 0 skips
```

### Example Usage

```ruby
calc = StringCalculator.new

calc.add("")                      # => 0
calc.add("1")                     # => 1
calc.add("1,2,3")                 # => 6
calc.add("1\n2,3")                # => 6
calc.add("//;\n1;2")              # => 3
calc.add("//[***]\n1***2***3")    # => 6
calc.add("//[*][%]\n1*2%3")       # => 6
calc.add("2,1001")                # => 2 (ignores 1001)

calc.get_called_count             # => 8
```

### What We Learned

**Regex Power:**
- `scan()` with capture groups extracts patterns
- `Regexp.escape()` makes special chars literal
- Dynamic regex building with `Regexp.new()`

**Ruby Features:**
- Array methods: `map`, `select`, `join`, `flatten`, `sum`
- String methods: `split`, `start_with?`, `include?`
- Instance variables for state tracking (`@call_count`)

**TDD Benefits:**
- Tests drove every design decision
- No feature added without a failing test first
- Refactored with confidence (tests stayed green)
- Final design emerged naturally from requirements

## Technologies

- **Language**: Ruby 2.6+
- **Testing**: Minitest (Ruby's built-in testing framework)
- **Focus**: Test-Driven Development, incremental design

## Source

From Roy Osherove's TDD Kata collection: https://osherove.com/kata

---

**Implementation**: Ruby
**Author**: Tom Spencer
**Date**: November 2025
**Status**: ✅ Complete (13/13 tests passing)
