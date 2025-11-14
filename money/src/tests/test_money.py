"""Tests for Money kata - Following Kent Beck's TDD by Example.

Chapter by chapter implementation following the book exactly.
"""

from src.money import Dollar, Franc


# Chapter 1-4: Multi-Currency Money & Value Objects
def test_multiplication():
    """Test $5 * 2 = $10, and $5 * 3 = $15 (using equality, not amount)."""
    five = Dollar(5)
    assert Dollar(10) == five.times(2)
    assert Dollar(15) == five.times(3)


# Chapter 3, 6, 7: Equality for All
def test_equality():
    """Test that $5 equals $5, and $5 does not equal $6."""
    assert Dollar(5) == Dollar(5)
    assert not (Dollar(5) == Dollar(6))
    # Chapter 6: Add Franc equality tests
    assert Franc(5) == Franc(5)
    assert not (Franc(5) == Franc(6))
    # Chapter 7: Francs should not equal Dollars
    assert not (Franc(5) == Dollar(5))


# Chapter 5: Franc-ly Speaking
def test_franc_multiplication():
    """Test 5 CHF * 2 = 10 CHF, and 5 CHF * 3 = 15 CHF."""
    five = Franc(5)
    assert Franc(10) == five.times(2)
    assert Franc(15) == five.times(3)
