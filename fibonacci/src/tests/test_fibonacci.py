"""Tests for Fibonacci kata.

Following TDD Red-Green-Refactor cycle.
See CLAUDE.md for the step-by-step guide.

Add tests one at a time, following the phases:
- Phase A: Base cases (n=0, n=1)
- Phase B: Simple sequence (n=2, 3, 4)
- Phase C: Larger numbers (n=5, 6, 7)
- Phase D: Edge cases (n=10+)
"""

from src.fibonacci import fibonacci


# Phase A: Base cases
def test_fibonacci_0_is_0():
    """Test that fibonacci(0) returns 0."""
    assert fibonacci(0) == 0


def test_fibonacci_1_is_1():
    """Test that fibonacci(1) returns 1."""
    assert fibonacci(1) == 1


# Phase B: Simple sequence
def test_fibonacci_2_is_1():
    """Test that fibonacci(2) returns 1."""
    assert fibonacci(2) == 1


def test_fibonacci_3_is_2():
    """Test that fibonacci(3) returns 2 (triangulation)."""
    assert fibonacci(3) == 2


def test_fibonacci_4_is_3():
    """Test that fibonacci(4) returns 3 (triangulation)."""
    assert fibonacci(4) == 3


# Phase C: Larger numbers
def test_fibonacci_5_is_5():
    """Test that fibonacci(5) returns 5."""
    assert fibonacci(5) == 5


def test_fibonacci_6_is_8():
    """Test that fibonacci(6) returns 8 (triangulation)."""
    assert fibonacci(6) == 8


def test_fibonacci_7_is_13():
    """Test that fibonacci(7) returns 13 (triangulation)."""
    assert fibonacci(7) == 13


# Phase D: Edge cases
def test_fibonacci_10_is_55():
    """Test that fibonacci(10) returns 55 (larger position)."""
    assert fibonacci(10) == 55
