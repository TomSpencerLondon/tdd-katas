"""Stats Calculator kata implementation.

Implement the calculate_stats function following the TDD guide.
See CLAUDE.md for the step-by-step TDD guide.
"""


def calculate_stats(numbers):
    """Calculate statistics for a sequence of numbers.

    Args:
        numbers: List of integers

    Returns:
        Dict containing:
        - min: Minimum value (or None if empty)
        - max: Maximum value (or None if empty)
        - count: Number of elements
        - average: Mean value (or None if empty)

    Examples:
        calculate_stats([]) → {'min': None, 'max': None, 'count': 0, 'average': None}
        calculate_stats([5]) → {'min': 5, 'max': 5, 'count': 1, 'average': 5.0}
        calculate_stats([1, 2, 3]) → {'min': 1, 'max': 3, 'count': 3, 'average': 2.0}
    """
    # Handle empty sequence
    if not numbers:
        return {'min': None, 'max': None, 'count': 0, 'average': None}

    # For non-empty sequences, calculate actual statistics
    return {
        'min': min(numbers),
        'max': max(numbers),
        'count': len(numbers),
        'average': sum(numbers) / len(numbers)
    }
