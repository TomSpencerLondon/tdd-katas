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

### **Chapter 11: The Root of All Evil**
**What we did**: Deleted Dollar and Franc subclasses entirely!
**Why Possible**: Subclasses only have constructors now
**Change**: Factory methods return `Money` directly
**Tests to Delete**: Redundant tests that no longer add value
**Key Insight**: As design evolves, some tests become redundant. Delete them!

### **Chapter 12: Addition, Finally**
**What we did**: Started implementing addition with Expression pattern
**Test**: `$5 + $5 = $10` (simpler than `$5 + 10 CHF`)
**New Concepts**:
- **Expression** - Interface for anything that can be reduced to Money
- **Bank** - Applies exchange rates to reduce Expressions
- `five.plus(five)` returns an Expression (not Money!)
- `bank.reduce(sum, "USD")` converts Expression to Money

**Design Decision**: Why `bank.reduce(expression, "USD")` instead of `expression.reduce("USD", bank)`?
- Keep Expression objects simple and ignorant of the world
- Bank is the natural place for exchange rate knowledge
- Separates concerns: Money is value, Bank is converter

**Key Insight**: Introduced "imposter" pattern - object that looks like Money but isn't!

### **Chapter 13: Make It**
**What we did**: Implemented polymorphic reduction
**New Class**: `Sum` - represents unevaluated addition of two Expressions
**Test**: `five.plus(five)` should return a `Sum` with `augend` and `addend`

**The Composite Pattern Emerges**:
```
Expression (interface)
├── Money (leaf - simple expression)
└── Sum (composite - contains two Expressions)
```

**Why Both Inherit from Expression?**
- **Money** = Simple expression (just an amount in a currency)
  - `reduce()` → convert to target currency
- **Sum** = Complex expression (combination of two expressions)
  - `reduce()` → reduce both parts, then add

**Polymorphism in Action**:
```python
# Bank doesn't care what kind of Expression it is!
def reduce(self, source, to):
    return source.reduce(self, to)  # Polymorphic call
```

Both Money and Sum implement `reduce()` differently:
- **Money.reduce()**: Convert using exchange rate
- **Sum.reduce()**: Reduce parts, then sum amounts

**Refactoring Journey**:
1. Started with ugly `isinstance` checks in Bank
2. Moved reduction logic to Sum and Money
3. Added `reduce()` to Expression interface
4. Eliminated all type checking - pure polymorphism!

**Key Insight**: "Make it work, make it right" - started ugly, refactored to elegant

### **Chapter 14: Change**
**What we did**: Implemented currency conversion!
**Test**: `bank.reduce(Money.franc(2), "USD") == Money.dollar(1)` (with rate 2:1)

**Exchange Rate System**:
```python
bank.add_rate("CHF", "USD", 2)  # 2 CHF = 1 USD
```

**Implementation Details**:
- **Rate Storage**: Dictionary with `(from_currency, to_currency)` tuple keys
- **Pair Class**: Created hashable key object for rate lookup
- **Identity Rate**: Same currency always has rate of 1 (e.g., USD→USD)

**Money.reduce() Evolution**:
```python
# Before: Just returned self
def reduce(self, bank, to):
    return self

# After: Actually converts!
def reduce(self, bank, to):
    rate = bank.rate(self._currency, to)
    return Money(self.amount / rate, to)
```

**Key Insight**: Passed Bank as parameter to reduce() - we "knew" we'd need it!

### **Chapter 15: Mixed Currencies**
**What we did**: THE BIG TEST - `$5 + 10 CHF = $10 USD`
**Problem Found**: Sum was adding raw amounts before converting!
```python
# WRONG: 5 + 10 = 15 USD (oops!)
amount = augend.amount + addend.amount
```

**The Fix**: Reduce THEN add!
```python
# RIGHT: reduce($5) + reduce(10 CHF) = $5 + $5 = $10
amount = (self.augend.reduce(bank, to).amount +
          self.addend.reduce(bank, to).amount)
```

**Recursive Reduction**:
- Sum reduces its parts (which might be Sums themselves!)
- Each part converts to target currency
- Then the amounts are added

**Generalization Refactoring**:
- Changed types from `Money` to `Expression` where possible
- Started from the edges (Sum fields) worked back to test
- Let compiler guide us through rippling changes
- Added `plus()` to Expression interface

**The Full Picture**:
```python
# $5 + 10 CHF with rate 2:1
five_bucks = Money.dollar(5)           # Money
ten_francs = Money.franc(10)           # Money
sum_expr = five_bucks.plus(ten_francs) # Sum(Money, Money)
result = bank.reduce(sum_expr, "USD")  # Money(10, "USD")

# What happens inside:
# 1. bank.reduce(sum_expr, "USD") calls sum_expr.reduce(bank, "USD")
# 2. Sum.reduce() calls:
#    - augend.reduce(bank, "USD") → Money(5, "USD")
#    - addend.reduce(bank, "USD") → Money(5, "USD")  [10 CHF / 2]
# 3. Sum adds: 5 + 5 = 10
# 4. Returns Money(10, "USD")
```

**Key Insight**: Composite pattern + polymorphism = expressions that can nest and evaluate themselves!

### **Chapter 16: Abstraction, Finally**
**What we did**: Completed the Expression interface with `plus()` and `times()`
**New Tests**:
- `Sum.plus()` - Can add to a Sum: `($5 + 10 CHF) + $5 = $15`
- `Sum.times()` - Can multiply a Sum: `($5 + 10 CHF) * 2 = $20`

**Implementation**:
```python
# Sum.plus() - Same as Money.plus()!
def plus(self, addend):
    return Sum(self, addend)

# Sum.times() - Distribute multiplication over addition
def times(self, multiplier):
    return Sum(self.augend.times(multiplier),
               self.addend.times(multiplier))
```

**Abstraction Complete**: Added `plus()` and `times()` to Expression interface
- Both Money and Sum now implement the full Expression protocol
- Expressions can be combined and transformed uniformly
- Distributive law: `(a + b) * n = a*n + b*n`

**Code Duplication Noticed**: `Sum.plus()` and `Money.plus()` are identical!
- Beck suggests making Expression a class instead of interface
- But we'll leave it for now - "I don't believe in 'finished'"
- Perfect is the enemy of good enough

**Experiment Attempted**: Return Money when adding same currencies (`$1 + $1`)
- Would require checking if addend is Money with same currency
- No clean way to do this without `isinstance` checks
- Deleted the test - optimization not worth the complexity

**Test/Code Ratio**: Test code is often longer than implementation!
- 12 tests, ~60 lines of test code
- 5 classes, ~50 lines of implementation
- Roughly equal lines of test and production code
- TDD economic sense: write 2x lines/day OR half the lines for same functionality

**Key Insights**:
- Not all optimization ideas pan out - be willing to discard experiments
- Test code is a "Rosetta stone for future generations" - write clearly!
- TDD produces low cyclomatic complexity (polymorphism instead of conditionals)
- Equal test/production code is typical in TDD

### **Chapter 17: Money Retrospective** ⬅️ COMPLETE!
**What we did**: Reflected on the entire TDD journey

**Is the Code Finished?** No! And that's okay.
- Still have duplication between `Sum.plus()` and `Money.plus()`
- Could make Expression a class instead of interface
- Beck: "I don't believe in 'finished'"
- Hot paths should be rock solid; periphery can be messier
- Use code critics (like flake8) to catch what you forget

**The Power of Metaphor**: Biggest surprise!
- Previous implementations used MoneyBag, MoneySum, Wallet metaphors
- All implied flat collections: "2 USD + 5 CHF + 3 USD" → "5 USD + 5 CHF"
- **Expression metaphor** freed us from merging duplicates
- Design went in completely different direction
- Cleaner code than Beck had ever written before
- "What if I could rewrite everything 20 times?"

**Test-Driven Metrics** (from Beck's coding session):
- Ran tests 125 times during implementation
- Ran tests about once per minute when actively coding
- Only surprised by test result once (during rushed refactoring)
- **100% statement coverage** (except debug toString)
- **Defect insertion** (Jester): Only fake hashCode could change without breaking tests

**Code Metrics**:
| Metric | Production | Test |
|--------|-----------|------|
| Classes | 5 | 1 |
| Functions | 22 | 15 |
| Lines | 91 | 89 |
| Cyclomatic Complexity | 1.04 | 1.0 |
| Lines/Function | 4.1 | 5.9 |

**Process**: Red-Green-Refactor in practice
- Most changes: 1-3 steps to compile and run
- Refactoring: Can be many steps (leptokurtotic/"fat tail" distribution)
- Small steps keep you in control
- Can adjust gap between tests: slow down when slippery, speed up when clear

**Test Quality**:
- TDD tests aren't sufficient for all testing needs
- Still need: performance testing, stress testing, usability testing
- But if defect density is low enough, testers become "communication amplifiers"
- Coverage improves two ways:
  1. Write more tests (test-driven developer: 6 tests, professional tester: 65 tests!)
  2. **Simplify the code** - refactoring often removes conditionals entirely

**Three Big Surprises** (that come up when teaching TDD):
1. **Three approaches**: Fake it, triangulation, obvious implementation
2. **Removing duplication** between test and code drives design
3. **Control the gap** between tests - traction control for software!

**Final Stats (Python Implementation)**:
- 12 tests, all passing
- 93% coverage
- 4 classes (Expression, Money, Sum, Bank)
- ~60 lines production code, ~60 lines test code
- Cyclomatic complexity near 1.0 (no branches, just polymorphism!)

**Key Learnings**:
- Metaphor shapes design more than you think
- TDD enables courage to refactor
- Small steps compound into big changes
- Tests are documentation for future maintainers
- "Finished" is a myth - code evolves

---

## Summary: The Complete TDD Journey

We followed Kent Beck's TDD process through 17 chapters, implementing a multi-currency money system from scratch. Here's what we built:

**Final Design**:
- `Expression` interface: Common protocol for all money-like values
- `Money` class: Concrete currency amounts (replaced Dollar and Franc)
- `Sum` class: Unevaluated addition (Composite pattern)
- `Bank` class: Manages exchange rates and reduces Expressions

**Key Design Patterns**:
- **Composite Pattern**: Money (leaf) and Sum (composite) both implement Expression
- **Strategy Pattern**: Bank encapsulates exchange rate algorithms
- **Factory Methods**: `Money.dollar()` and `Money.franc()` hide construction
- **Value Objects**: Immutable Money objects with value-based equality
- **Polymorphic Dispatch**: No type checking, just polymorphic `reduce()`

**TDD Techniques Learned**:
1. **Fake It Till You Make It**: Start with hardcoded values, generalize incrementally
2. **Triangulation**: Use multiple tests to force proper generalization
3. **Obvious Implementation**: Type in the solution when it's clear
4. **Remove Duplication**: Between test and code to drive design
5. **Test List**: Maintain to-do list, cross off as you go
6. **Small Steps**: Keep the gap between tests small when uncertain
7. **Refactoring**: Bold changes enabled by comprehensive tests

**The Red-Green-Refactor Rhythm**:
1. **Red**: Write a failing test
2. **Green**: Make it pass quickly (commit sins!)
3. **Refactor**: Remove duplication and improve design
4. **Repeat**: Small, safe steps accumulate

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
