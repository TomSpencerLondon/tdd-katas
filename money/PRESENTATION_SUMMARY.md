# Money Kata - TDD by Example
## Complete Journey Through Kent Beck's Classic

---

## Overview

**Goal**: Build a multi-currency money system using Test-Driven Development

**Problem**:
- Multiply amounts: `$5 × 2 = $10`
- Add different currencies: `$5 + 10 CHF = $10 USD` (with exchange rate 2:1)

**Process**: 17 chapters following Kent Beck's "Test-Driven Development: By Example"

**Result**: Clean design with 4 classes, 12 tests, 93% coverage

---

## The TDD Cycle

### Red-Green-Refactor

1. **🔴 Red**: Write a failing test
2. **🟢 Green**: Make it pass (commit sins if needed!)
3. **🔵 Refactor**: Remove duplication, improve design
4. **🔄 Repeat**: Small steps, one test at a time

### Three Approaches to Green

1. **Fake It**: Hardcode the answer, then generalize
2. **Triangulation**: Use multiple tests to force generalization
3. **Obvious Implementation**: Type in the solution when clear

---

## Part 1: Foundation (Chapters 1-4)

### Chapter 1: Multi-Currency Money
**What**: First test for `$5 × 2 = $10`
**How**:
```python
# Start with hardcoded
amount = 10

# Make duplication visible
amount = 5 * 2

# Generalize
amount = self.amount * multiplier
```
**Insight**: "Fake it till you make it" - hardcode first!

---

### Chapter 2: Degenerate Objects
**What**: Eliminate side effects
**Problem**: `five.times(2)` was changing `five`!
**Solution**: Return new `Dollar` object
**Insight**: Value objects should be immutable

---

### Chapter 3: Equality for All
**What**: Implement `__eq__` for comparison
**Technique**: **Triangulation** - two tests force generalization
```python
assert Dollar(5) == Dollar(5)   # Force comparison
assert not (Dollar(5) == Dollar(6))  # Force actual logic
```
**Insight**: Tests drive out needed operations

---

### Chapter 4: Privacy
**What**: Use equality instead of accessing `.amount`
**Before**: `assert product.amount == 10`
**After**: `assert Dollar(10) == product`
**Insight**: Use just-developed features to improve tests

---

## Part 2: Duplication (Chapters 5-11)

### The Duplication Journey

| Chapter | Action | Classes |
|---------|--------|---------|
| 5 | Copy-paste Dollar → Franc | Dollar, Franc |
| 6 | Extract Money superclass | Money, Dollar, Franc |
| 7 | Compare classes not amounts | ↓ |
| 8 | Add factory methods | ↓ |
| 9 | Add currency() method | ↓ |
| 10 | Push up times(), compare currency | Money |
| 11 | **Delete subclasses!** | **Money only!** |

---

### Chapter 5: Franc-ly Speaking
**What**: Shameless duplication of Dollar as Franc
**Permission**: Beck says it's OK to copy-paste!
**Why**: Get to green fast
**Insight**: Cycle isn't complete without refactoring

---

### Chapter 8: Makin' Objects
**What**: Factory methods `Money.dollar(5)` and `Money.franc(5)`
**Why**: Decouple tests from concrete classes
**Goal**: Enable deleting subclasses later
**Insight**: Factories give flexibility to change implementation

---

### Chapter 11: The Root of All Evil
**What**: Delete Dollar and Franc entirely!
**How**: They only had constructors left
**Also**: Delete redundant tests
**Insight**: As design evolves, tests can become redundant

---

## Part 3: Addition (Chapters 12-15)

### The Expression Pattern Emerges

```
Expression (interface)
├── Money (leaf - simple)
│   └── reduce() → convert currency
└── Sum (composite - complex)
    └── reduce() → reduce parts, then add
```

---

### Chapter 12: Addition, Finally
**What**: Start with `$5 + $5 = $10`
**Design Decision**:
```python
# Why this?
bank.reduce(expression, "USD")

# Not this?
expression.reduce("USD", bank)
```
**Answer**: Keep Expression simple, Bank knows rates
**Insight**: "Imposter" objects - looks like Money but isn't!

---

### Chapter 13: Make It
**What**: Polymorphic reduction
**Created**: `Sum` class for unevaluated addition

**Refactoring Journey**:
```python
# Started ugly with isinstance checks
if isinstance(source, Money):
    return source

# Ended elegant with polymorphism
return source.reduce(self, to)
```
**Insight**: "Make it work, make it right"

---

### Chapter 14: Change
**What**: Currency conversion!
**Test**: `2 CHF = $1 USD` (with rate 2:1)

**Implementation**:
```python
bank.add_rate("CHF", "USD", 2)

# Money.reduce()
rate = bank.rate(self._currency, to)
return Money(self.amount / rate, to)
```

**Rate Storage**: Dictionary with `(from, to)` tuple keys
**Identity Rate**: Same currency always = 1

---

### Chapter 15: Mixed Currencies
**THE BIG TEST**: `$5 + 10 CHF = $10 USD`

**Problem Found**:
```python
# WRONG: Added before converting
amount = augend.amount + addend.amount  # 5 + 10 = 15!
```

**The Fix**:
```python
# RIGHT: Reduce THEN add
amount = (augend.reduce(bank, to).amount +
          addend.reduce(bank, to).amount)
```

**Flow**:
1. `bank.reduce(sum, "USD")`
2. `sum.reduce(bank, "USD")` reduces parts
3. `augend.reduce()` → `$5`
4. `addend.reduce()` → `$5` (10 CHF ÷ 2)
5. Add: `5 + 5 = 10`

**Insight**: Recursive reduction enables nested expressions!

---

## Part 4: Completion (Chapters 16-17)

### Chapter 16: Abstraction, Finally
**What**: Complete Expression interface

**Added to Sum**:
```python
# Sum.plus() - same as Money.plus()!
def plus(self, addend):
    return Sum(self, addend)

# Sum.times() - distribute multiplication
def times(self, multiplier):
    return Sum(augend.times(multiplier),
               addend.times(multiplier))
```

**Abstraction**: Added to Expression interface
**Enables**: `($5 + 10 CHF) + $5 = $15` and `($5 + 10 CHF) × 2 = $20`

**Duplication Noticed**: `Sum.plus()` = `Money.plus()`
**Decision**: Leave it - "Perfect is enemy of good enough"

---

### Chapter 17: Money Retrospective

#### Is It Finished?
**No!** And that's okay.
- Duplication remains
- Could refactor more
- Beck: "I don't believe in 'finished'"
- Hot paths solid, periphery can be messier

#### The Power of Metaphor 🤯
**Previous attempts**: MoneyBag, MoneySum, Wallet
→ Implied flat collections, had to merge duplicates

**Expression metaphor**: Complete game-changer
→ Nested structures, no merging needed
→ Cleaner than Beck's 20+ previous attempts!

**Lesson**: Metaphor shapes design profoundly

---

#### Code Metrics

| Metric | Production | Test |
|--------|-----------|------|
| Classes | 4 | 1 |
| Lines | ~60 | ~60 |
| Cyclomatic Complexity | 1.0 | 1.0 |

**Key**: Equal test and production code is typical!

---

#### Test-Driven Metrics

From Beck's coding session:
- Ran tests **125 times**
- ~**1 test run per minute** when coding
- Only **1 surprise** (rushed refactoring)
- **93-100% statement coverage**
- **Cyclomatic complexity near 1.0** (polymorphism!)

---

#### Three Big TDD Surprises

When teaching TDD, these three always surprise:

1. **Three approaches** to green
   - Fake it, triangulation, obvious implementation

2. **Removing duplication** drives design
   - Between test and code
   - Design emerges, not planned

3. **Control the gap** between tests
   - Small steps when uncertain
   - Big leaps when confident
   - "Traction control for software!"

---

## Final Design

### The Four Classes

```python
class Expression(ABC):
    """Interface for money-like values"""
    def reduce(self, bank, to): pass
    def plus(self, addend): pass
    def times(self, multiplier): pass

class Money(Expression):
    """Concrete currency amount (leaf)"""
    # Factory methods
    Money.dollar(5)
    Money.franc(10)

class Sum(Expression):
    """Unevaluated addition (composite)"""
    # Contains two Expressions

class Bank:
    """Manages exchange rates"""
    def reduce(self, expr, to): ...
    def add_rate(self, from, to, rate): ...
```

---

### Design Patterns Used

1. **Composite Pattern**: Money (leaf) + Sum (composite)
2. **Strategy Pattern**: Bank encapsulates rate algorithms
3. **Factory Methods**: Hide Money construction
4. **Value Objects**: Immutable with value equality
5. **Polymorphic Dispatch**: No type checking!

---

## Key TDD Techniques

### 1. Fake It Till You Make It
Start with hardcoded values, generalize step by step
```python
# First: return Money.dollar(10)
# Then: return Money.dollar(5 + 5)
# Finally: return Money(augend + addend, to)
```

### 2. Triangulation
Multiple tests force proper generalization
```python
assert Dollar(5) == Dollar(5)
assert not (Dollar(5) == Dollar(6))  # Forces real logic!
```

### 3. Remove Duplication
Between test and code drives design
```python
# Test: $5 + $5 = $10
# Code: return Money.dollar(10)  # Duplication!
# Refactor: Make code actually add
```

---

### 4. Test List
Maintain to-do list, cross off as you go

**Example from Chapter 15**:
- ✅ $5 + 10 CHF = $10 if rate is 2:1
- ✅ $5 + $5 = $10
- ✅ Return Money from $5 + $5
- ✅ Bank.reduce(Money)
- ✅ Reduce Money with conversion

---

### 5. Small Steps
Keep gap small when uncertain, widen when confident

**Uncertain** (Chapter 6):
1. Create empty Money class
2. Make Dollar extend Money
3. Push up amount field
4. Push up equals method
5. Run tests after EACH step

**Confident** (Chapter 10):
- Deleted entire classes in one step
- Knew tests would catch problems

---

### 6. Refactoring with Confidence
Tests enable bold changes

**Example**: Chapter 10-11
- Pushed up methods
- Compared currency not class
- Deleted two entire classes
- Changed 15+ test assertions
- **All tests still passed!**

---

## Lessons Learned

### 🎯 Design
- **Metaphor matters** more than you think
- **Design emerges** from tests, not planned upfront
- **Duplication reveals** the right abstractions
- **Polymorphism beats** conditionals

### 🧪 Testing
- **Equal test/prod code** is normal and good
- **Tests are documentation** for future maintainers
- **Coverage improves** by simplifying code, not just more tests
- **Low complexity** comes from refactoring

---

### 🚀 Process
- **Small steps** compound into big changes
- **Three approaches** give options at any moment
- **Gap control** lets you adjust pace
- **Red-Green-Refactor** rhythm becomes natural

### 💡 Philosophy
- **"Finished" is a myth** - code evolves
- **Perfect is the enemy** of good enough
- **Experiments fail** - discard them gracefully
- **TDD enables courage** to refactor

---

## The Complete Test Suite

```python
✅ test_multiplication            # Ch 1-4: $5 × 2 = $10
✅ test_equality                  # Ch 3: $5 == $5
✅ test_currency                  # Ch 9: currency()
✅ test_simple_addition           # Ch 12: $5 + $5 = $10
✅ test_plus_returns_sum          # Ch 13: Returns Sum
✅ test_reduce_sum                # Ch 13: Bank reduces Sum
✅ test_reduce_money              # Ch 13: Bank reduces Money
✅ test_reduce_money_different_currency  # Ch 14: 2 CHF = $1
✅ test_identity_rate             # Ch 14: USD→USD = 1
✅ test_mixed_addition            # Ch 15: $5 + 10 CHF = $10
✅ test_sum_plus_money            # Ch 16: Sum + Money
✅ test_sum_times                 # Ch 16: Sum × 2
```

**12 tests, 93% coverage, all green! 🟢**

---

## Why TDD?

### Economics
Write 2× lines/day OR half the lines for same functionality

### Confidence
Tests catch regressions immediately

### Design
Duplication drives good abstractions

### Documentation
Tests show how to use the code

### Courage
Can refactor boldly with safety net

---

## Try It Yourself!

### The Challenge
Implement from scratch following Beck's book

### The Process
1. Write the test first
2. Make it pass (fake it!)
3. Refactor
4. Commit
5. Next chapter

### The Reward
- Deep understanding of TDD
- See design emerge from tests
- Experience the rhythm
- Build confidence

---

## Resources

**Book**: "Test Driven Development: By Example" by Kent Beck

**Code**: `/Users/tomspencer/Desktop/katas/money/`
- `src/money.py` - Implementation
- `src/tests/test_money.py` - Tests
- `README.md` - Chapter-by-chapter guide

**Chapters**: 17 chapters, ~2 hours to complete

**Difficulty**: Beginner to Intermediate

---

## Summary

### What We Built
Multi-currency money system with:
- Money values in any currency
- Addition across currencies
- Multiplication
- Exchange rate conversion

### How We Built It
- 17 chapters of TDD
- 12 tests driving design
- Red-Green-Refactor rhythm
- Emerged design, not planned

### What We Learned
- TDD techniques (fake, triangulate, obvious)
- Design patterns (Composite, Strategy, Value Object)
- Refactoring with confidence
- The power of metaphor

---

## Final Thoughts

> "I'm not a great programmer; I'm just a good programmer with great habits."
>
> — Kent Beck

**TDD is the habit.**

The small steps compound.
The tests give courage.
The design emerges.
The rhythm becomes natural.

**Now go practice!** 🚀

---

## Next Steps

1. **Review** the chapter-by-chapter README
2. **Study** the commit history to see evolution
3. **Try it yourself** - delete code and rebuild
4. **Experiment** - add new features with TDD
5. **Teach others** - best way to solidify learning

---

**Generated with** [Claude Code](https://claude.com/claude-code)
**Based on** "Test Driven Development: By Example" by Kent Beck
**Author**: Tom Spencer
**Date**: November 2025
