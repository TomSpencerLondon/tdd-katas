# Money Kata - Kent Beck's TDD by Example

This kata follows Kent Beck's seminal book "Test-Driven Development: By Example" - specifically Part I: The Money Example (Chapters 1-17).

## The Problem

Build a multi-currency money system that can:
- Multiply amounts by numbers (e.g., 5 USD * 2 = 10 USD)
- Add amounts in different currencies with exchange rates
- Convert between currencies

### Example Scenario

```
$5 + 10 CHF = $10 (if exchange rate is 2:1)
$5 * 2 = $10
```

## Kent Beck's Approach

This implementation follows Beck's TDD methodology exactly as presented in his book:
1. **Write a test** - Imagine the perfect API
2. **Make it compile** - Add stubs, do the minimum
3. **Make it run** - Get to green quickly (even with "sins")
4. **Make it right** - Refactor to remove duplication
5. **Repeat** - Small steps, one test at a time

## Chapter-by-Chapter Journey

Each chapter demonstrates specific TDD techniques and design insights:

### **Chapter 1: Multi-Currency Money**
**What we did**: Wrote first test for `$5 * 2 = $10`
**TDD Steps**:
- Started with hardcoded `amount = 10` (the "sin"!)
- Made duplication visible: `amount = 5 * 2`
- Generalized step-by-step to use constructor parameter and multiplier
**Key Insight**: "Fake it till you make it" - hardcode first, then generalize

### **Chapter 2: Degenerate Objects**
**What we did**: Eliminated side effects in `times()`
**Problem**: `five.times(2)` was changing `five` to 10!
**Solution**: Return new `Dollar` object instead of mutating
**Test Change**: `product = five.times(2)` instead of checking `five.amount`
**Key Insight**: Value objects should be immutable

### **Chapter 3: Equality for All**
**What we did**: Implemented `__eq__` for Dollar
**TDD Technique**: **Triangulation** - wrote two tests ($5 == $5, $5 != $6) to force generalization
**Why**: Value objects need equality to compare results
**Key Insight**: Tests drive out the operations we need

### **Chapter 4: Privacy**
**What we did**: Used equality in tests instead of accessing `.amount`
**Before**: `assert product.amount == 10`
**After**: `assert Dollar(10) == product`
**Key Insight**: Using the just-developed feature (equality) improves tests and enables encapsulation

### **Chapter 5: Franc-ly Speaking**
**What we did**: Copy-pasted Dollar to create Franc
**Beck's Permission**: "Shameless duplication" to get green fast!
**Code Smell**: Intentional - we'll clean it up soon
**Key Insight**: Speed to green trumps design... temporarily. The cycle isn't complete without refactoring!

### **Chapter 6: Equality for All, Redux**
**What we did**: Extracted Money superclass
**Steps**:
1. Created empty `Money` class
2. Made `Dollar` extend `Money`
3. Pushed up `amount` field
4. Pushed up `__eq__` method
5. Did same for `Franc`
**Key Insight**: Small, safe steps. Run tests after each tiny change.

### **Chapter 7: Apples and Oranges**
**What we did**: Made Francs not equal Dollars
**Test Added**: `assert not (Franc(5) == Dollar(5))`
**Fix**: Changed `__eq__` to compare `__class__` not just amount
**Beck's Note**: Comparing classes is "smelly" but we don't have currency concept yet
**Key Insight**: Turned discomfort into a test

### **Chapter 8: Makin' Objects**
**What we did**: Introduced factory methods `Money.dollar()` and `Money.franc()`
**Why**: Decouple tests from concrete subclasses
**Test Change**: `Dollar(5)` → `Money.dollar(5)`
**Goal**: Prepare to eliminate subclasses entirely
**Key Insight**: Factory methods give us flexibility to change implementation without breaking tests

### **Chapter 9: Times We're Livin' In**
**What we did**: Added `currency()` method returning "USD" or "CHF"
**Steps**:
1. Added `_currency` instance variable
2. Set it in constructor: `Money(amount, currency)`
3. Made factory methods pass currency: `Dollar(amount, "USD")`
4. Pushed up identical constructors to Money
**Key Insight**: Move variation to the caller to enable consolidation

### **Chapter 10: Interesting Times**
**What we did**: Made Money concrete and pushed up `times()`
**Critical Change**: `__eq__` now compares `_currency` instead of `__class__`
**Why This Works**:
- `Dollar(10, "USD")` and `Money(10, "USD")` are now equal!
- We can return `Money` from `times()` instead of subclass
**Added**: `__str__` for better test output
**Key Insight**: Tests tell us if our refactoring works - ask the computer, don't just reason!

### **Chapter 11: The Root of All Evil** ⬅️ WE ARE HERE
**What we're doing**: Delete Dollar and Franc subclasses entirely!
**Why Possible**: Subclasses only have constructors now
**Change**: Factory methods return `Money` directly
**Tests to Delete**: Redundant tests that no longer add value
**Key Insight**: As design evolves, some tests become redundant. Delete them!

## Setup

### 1. Create Virtual Environment

```bash
cd money
python3 -m venv venv
source venv/bin/activate
```

### 2. Install Dependencies

```bash
pip install -e ".[dev]"
```

### 3. Install Pre-commit Hooks

```bash
pre-commit install
```

## TDD Workflow

Follow Kent Beck's Red-Green-Refactor cycle:

1. **Red**: Write a failing test
2. **Green**: Make it pass (commit sins if needed!)
3. **Refactor**: Remove duplication
4. **Commit**: Document the journey
5. **Repeat**: Next test from the to-do list

## Running Tests

### Basic Test Run

```bash
# Run all tests
pytest

# Run with verbose output
pytest -v

# Run with coverage report
pytest --cov=src --cov-report=term-missing

# Run specific test file
pytest src/tests/test_money.py -v
```

### Watch Mode (Recommended for TDD)

```bash
# Automatically re-run tests when files change
ptw
```

### Using Python directly (without pytest installed)

```bash
python -m pytest src/tests/test_money.py -v
```

Follow [CLAUDE.md](CLAUDE.md) for the step-by-step TDD guide through all 17 chapters.

## Expected Results

- **Tests**: ~30+ passing (one or more per chapter)
- **Coverage**: 100%
- **Commits**: ~20+ (documenting the TDD journey)
- **Final Design**: Clean multi-currency money system with exchange rates

## Key Learnings

- **Small steps**: TDD enables tiny, safe increments
- **Duplication drives design**: Eliminating duplication reveals abstractions
- **Tests as documentation**: Each test tells a story
- **Refactoring with confidence**: Green bar enables bold refactoring
- **Design emerges**: Don't plan upfront, let tests guide you

## Technologies

- Python 3.10+
- pytest (testing framework)
- pytest-cov (coverage)
- pytest-watch (watch mode)

---

**Based on**: "Test Driven Development: By Example" by Kent Beck
**Generated with**: [Claude Code](https://claude.com/claude-code)
**Last Updated**: November 2025
**Author**: Tom Spencer
