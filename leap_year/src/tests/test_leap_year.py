"""Tests for Leap Year kata.

Following TDD Red-Green-Refactor cycle.
See CLAUDE.md for the step-by-step guide.

Add tests one at a time, following the phases:
- Phase A: Non-leap years (not divisible by 4)
- Phase B: Basic leap years (divisible by 4 but not by 100)
- Phase C: Century years NOT divisible by 400
- Phase D: Century years divisible by 400
"""

from src.leap_year import is_leap_year


# Phase A: Non-leap years (not divisible by 4)
def test_2017_is_not_leap_year():
    """Test that 2017 is not a leap year."""
    assert is_leap_year(2017) is False


def test_2018_is_not_leap_year():
    """Test that 2018 is not a leap year."""
    assert is_leap_year(2018) is False


def test_2019_is_not_leap_year():
    """Test that 2019 is not a leap year."""
    assert is_leap_year(2019) is False


# Phase B: Basic leap years (divisible by 4 but not by 100)
def test_2016_is_leap_year():
    """Test that 2016 is a leap year."""
    assert is_leap_year(2016) is True


def test_2012_is_leap_year():
    """Test that 2012 is a leap year (triangulation)."""
    assert is_leap_year(2012) is True


def test_2008_is_leap_year():
    """Test that 2008 is a leap year (triangulation)."""
    assert is_leap_year(2008) is True


# Phase C: Century years NOT divisible by 400
def test_1900_is_not_leap_year():
    """Test that 1900 is not a leap year (century exception)."""
    assert is_leap_year(1900) is False


def test_1800_is_not_leap_year():
    """Test that 1800 is not a leap year (century exception)."""
    assert is_leap_year(1800) is False


def test_1700_is_not_leap_year():
    """Test that 1700 is not a leap year (century exception)."""
    assert is_leap_year(1700) is False


def test_2100_is_not_leap_year():
    """Test that 2100 is not a leap year (future century exception)."""
    assert is_leap_year(2100) is False


# Phase D: Century years divisible by 400
def test_2000_is_leap_year():
    """Test that 2000 is a leap year (divisible by 400)."""
    assert is_leap_year(2000) is True


def test_2400_is_leap_year():
    """Test that 2400 is a leap year (divisible by 400)."""
    assert is_leap_year(2400) is True


def test_1600_is_leap_year():
    """Test that 1600 is a leap year (divisible by 400)."""
    assert is_leap_year(1600) is True
