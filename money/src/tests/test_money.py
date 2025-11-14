"""Tests for Money kata - Following Kent Beck's TDD by Example.

Chapter by chapter implementation following the book exactly.
"""

from src.money import Money


# Chapter 1-4, 8: Multi-Currency Money & Value Objects
def test_multiplication():
    """Test $5 * 2 = $10, and $5 * 3 = $15 (using factory methods)."""
    five = Money.dollar(5)
    assert Money.dollar(10) == five.times(2)
    assert Money.dollar(15) == five.times(3)


# Chapter 3, 6, 7, 8: Equality for All
def test_equality():
    """Test that $5 equals $5, and $5 does not equal $6."""
    assert Money.dollar(5) == Money.dollar(5)
    assert not (Money.dollar(5) == Money.dollar(6))
    # Chapter 6: Add Franc equality tests
    assert Money.franc(5) == Money.franc(5)
    assert not (Money.franc(5) == Money.franc(6))
    # Chapter 7: Francs should not equal Dollars
    assert not (Money.franc(5) == Money.dollar(5))


# Chapter 5, 8: Franc-ly Speaking
def test_franc_multiplication():
    """Test 5 CHF * 2 = 10 CHF, and 5 CHF * 3 = 15 CHF."""
    five = Money.franc(5)
    assert Money.franc(10) == five.times(2)
    assert Money.franc(15) == five.times(3)


# Chapter 9: Times We're Livin' In
def test_currency():
    """Test that currency() returns correct currency codes."""
    assert "USD" == Money.dollar(1).currency()
    assert "CHF" == Money.franc(1).currency()
