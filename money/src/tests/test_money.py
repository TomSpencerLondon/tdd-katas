"""Tests for Money kata - Following Kent Beck's TDD by Example.

Chapter by chapter implementation following the book exactly.
"""

from src.money import Money, Bank


# Chapter 1-4, 8: Multi-Currency Money & Value Objects
def test_multiplication():
    """Test $5 * 2 = $10, and $5 * 3 = $15 (using factory methods)."""
    five = Money.dollar(5)
    assert Money.dollar(10) == five.times(2)
    assert Money.dollar(15) == five.times(3)


# Chapter 3, 6, 7, 8, 11: Equality for All
def test_equality():
    """Test that money objects compare correctly."""
    assert Money.dollar(5) == Money.dollar(5)
    assert not (Money.dollar(5) == Money.dollar(6))
    # Chapter 7: Different currencies are not equal
    assert not (Money.franc(5) == Money.dollar(5))
    # Chapter 11: Deleted redundant Franc(5)==Franc(5) tests
    # (same logic as Dollar tests above, just different currency)


# Chapter 9: Times We're Livin' In
def test_currency():
    """Test that currency() returns correct currency codes."""
    assert "USD" == Money.dollar(1).currency()
    assert "CHF" == Money.franc(1).currency()


# Chapter 11: test_franc_multiplication DELETED!
# It tested the exact same times() logic as test_multiplication,
# just with CHF instead of USD. Now that there's only one Money class,
# this test is redundant and adds no value.


# Chapter 12: Addition, Finally
def test_simple_addition():
    """Test $5 + $5 = $10 using Expression and Bank."""
    five = Money.dollar(5)
    sum_expr = five.plus(five)  # Returns an Expression
    bank = Bank()
    reduced = bank.reduce(sum_expr, "USD")  # Bank reduces Expression to Money
    assert Money.dollar(10) == reduced
