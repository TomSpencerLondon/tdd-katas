"""Tests for Stats Calculator kata.

Following TDD Red-Green-Refactor cycle.
See CLAUDE.md for the step-by-step guide.

Add tests one at a time, following the phases:
- Phase A: Empty/single element (base cases)
- Phase B: Simple sequences (2-3 elements)
- Phase C: Edge cases (negatives, duplicates)
- Phase D: Larger sequences
"""

from src.stats_calculator import calculate_stats


# Phase A: Empty/Single Element (Base Cases)
def test_empty_sequence():
    """Test that empty sequence returns None for all stats."""
    result = calculate_stats([])
    assert result == {'min': None, 'max': None, 'count': 0, 'average': None}


def test_single_element():
    """Test that single element [5] returns all stats as 5."""
    result = calculate_stats([5])
    assert result == {'min': 5, 'max': 5, 'count': 1, 'average': 5.0}

# Phase B: Simple Sequences (2-3 elements)
def test_two_elements():
    """Test that [1, 2] calculates stats correctly."""
    result = calculate_stats([1, 2])
    assert result == {'min': 1, 'max': 2, 'count': 2, 'average': 1.5}


def test_three_elements_ordered():
    """Test that [1, 2, 3] calculates stats correctly (triangulation)."""
    result = calculate_stats([1, 2, 3])
    assert result == {'min': 1, 'max': 3, 'count': 3, 'average': 2.0}


def test_three_elements_unordered():
    """Test that [3, 1, 2] calculates stats correctly (order independence)."""
    result = calculate_stats([3, 1, 2])
    assert result == {'min': 1, 'max': 3, 'count': 3, 'average': 2.0}

# Phase C: Edge Cases (Negatives and Duplicates)
def test_negative_numbers():
    """Test that negative numbers [-5, -1, -3] work correctly."""
    result = calculate_stats([-5, -1, -3])
    assert result == {'min': -5, 'max': -1, 'count': 3, 'average': -3.0}


def test_mixed_positive_negative():
    """Test that mixed [-2, 0, 2] works correctly."""
    result = calculate_stats([-2, 0, 2])
    assert result == {'min': -2, 'max': 2, 'count': 3, 'average': 0.0}


def test_all_duplicates():
    """Test that duplicates [2, 2, 2] work correctly."""
    result = calculate_stats([2, 2, 2])
    assert result == {'min': 2, 'max': 2, 'count': 3, 'average': 2.0}


def test_duplicates_at_extremes():
    """Test that [1, 2, 1] handles duplicates at min correctly."""
    result = calculate_stats([1, 2, 1])
    expected_average = 4 / 3  # ~1.333...
    assert result['min'] == 1
    assert result['max'] == 2
    assert result['count'] == 3
    assert abs(result['average'] - expected_average) < 0.01

# Phase D: Larger Sequences
def test_larger_sequence():
    """Test that larger sequence [1..10] works correctly."""
    result = calculate_stats([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
    assert result == {'min': 1, 'max': 10, 'count': 10, 'average': 5.5}
