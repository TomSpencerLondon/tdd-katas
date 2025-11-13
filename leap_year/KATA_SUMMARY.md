# Leap Year Kata - Completion Summary

## Final Results

- **Tests**: 13 passing
- **Coverage**: 100%
- **Commits**: 4 phases (A, B, C, D)
- **Duration**: Complete TDD journey from empty stubs to working solution

## TDD Journey

### Phase A: Non-leap years (not divisible by 4)
**Commits**: fe5f47c

Started with the simplest case - years that are NOT leap years:
- Test 2017 → False: Hardcoded `return False`
- Test 2018 → False: Still works
- Test 2019 → False: Triangulation confirms pattern

**Learning**: "Fake it till you make it" - start simple, let tests drive complexity.

### Phase B: Basic leap years (divisible by 4)
**Commits**: 23759ba

Added the first real logic:
- Test 2016 → True: FAILED with hardcoded False
- Production: Added `if year % 4 == 0: return True`
- Test 2012 → True: Triangulation (no code change)
- Test 2008 → True: Further triangulation

**Learning**: Tests force us to implement general logic, not hardcoded values.

### Phase C: Century years NOT divisible by 400
**Commits**: 2969d65

Discovered the century exception:
- Test 1900 → False: FAILED (returned True because 1900 % 4 == 0)
- Production: Added century check BEFORE the modulo 4 check
  ```python
  if year % 100 == 0 and year % 400 != 0:
      return False
  ```
- Test 1800, 1700, 2100 → False: All pass with existing logic

**Learning**: Order matters! Check exceptions before general rules.

### Phase D: Century years divisible by 400 ARE leap years
**Commits**: bb08149

Made the 400-year rule explicit:
- Test 2000 → True: Actually PASSED (logic accidentally worked)
- Refactored to make it explicit:
  ```python
  if year % 400 == 0:
      return True
  if year % 100 == 0:
      return False
  if year % 4 == 0:
      return True
  return False
  ```
- Test 2400, 1600 → True: Triangulation confirms logic

**Learning**: Make implicit logic explicit. Check most specific rules first.

## Final Implementation

```python
def is_leap_year(year):
    """Determine if a year is a leap year according to Gregorian Calendar rules."""
    # Years divisible by 400 ARE leap years
    if year % 400 == 0:
        return True
    # Century years (divisible by 100) are NOT leap years
    if year % 100 == 0:
        return False
    # Years divisible by 4 are leap years
    if year % 4 == 0:
        return True
    return False
```

**Alternative (condensed boolean expression)**:
```python
def is_leap_year(year):
    return (year % 4 == 0 and year % 100 != 0) or (year % 400 == 0)
```

## Test Coverage

### Phase A: Non-leap years (3 tests)
- 2017, 2018, 2019 → False

### Phase B: Basic leap years (3 tests)
- 2016, 2012, 2008 → True

### Phase C: Century years NOT divisible by 400 (4 tests)
- 1900, 1800, 1700, 2100 → False

### Phase D: Century years divisible by 400 (3 tests)
- 2000, 2400, 1600 → True

## Key TDD Principles Demonstrated

1. **Red-Green-Refactor**: Every change followed this cycle
2. **Fake it till you make it**: Started with hardcoded values
3. **Triangulation**: Multiple tests confirm general solutions
4. **Small steps**: One test at a time, minimal code changes
5. **Incremental complexity**: Built from simple to complex rules
6. **Order matters**: Most specific rules checked first
7. **Frequent commits**: Clear history showing progression

## Git History

```
bb08149 Phase D: Century years divisible by 400 ARE leap years
2969d65 Phase C: Century years NOT divisible by 400
23759ba Phase B: Basic leap years (divisible by 4)
fe5f47c Phase A: Non-leap years (not divisible by 4)
```

Each commit includes:
- Clear phase description
- Red-Green-Refactor details
- Test counts and coverage
- Learning points

## What We Learned

1. **TDD drives design**: Tests forced us to discover the rules incrementally
2. **Simplicity first**: Start simple, add complexity only when tests demand it
3. **Rule order matters**: Check most specific conditions first (400 → 100 → 4)
4. **Explicit is better**: Make implicit logic explicit for clarity
5. **Triangulation validates**: Multiple examples confirm logic is general, not hardcoded
6. **Small commits**: Create a reviewable history that tells the story

## Next Steps (Optional Extensions)

1. **4000-year rule**: Years divisible by 4000 are NOT leap years
2. **Helper functions**: Extract divisibility checks
3. **Alternative implementations**: Try the condensed boolean version
4. **Property-based testing**: All leap years must be divisible by 4
5. **Days in February**: Build on this to calculate days in February

## Commands Used

```bash
# Setup
python3 -m venv venv
source venv/bin/activate
pip install -e ".[dev]"
pre-commit install

# Development
pytest -v                                    # Run tests
pytest --cov=src --cov-report=term-missing  # With coverage
ptw                                          # Watch mode

# Git
git log --oneline                            # View history
git show <commit>                            # View specific commit
```

## Conclusion

This kata demonstrates the power of TDD:
- Started with no implementation
- Let tests drive the design
- Arrived at a correct, well-tested solution
- Created clear documentation through commits
- Achieved 100% test coverage

The journey is as valuable as the destination! 🎉
