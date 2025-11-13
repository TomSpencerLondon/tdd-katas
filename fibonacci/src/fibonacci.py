"""Fibonacci kata implementation.

Implement the fibonacci function following the TDD guide.
See CLAUDE.md for the step-by-step TDD guide.
"""


def fibonacci(n):
    """Calculate the nth Fibonacci number.

    Args:
        n: The position in the Fibonacci sequence (0-indexed)

    Returns:
        The Fibonacci number at position n

    Examples:
        fibonacci(0) → 0
        fibonacci(1) → 1
        fibonacci(2) → 1
        fibonacci(5) → 5
        fibonacci(10) → 55
    """
    # Base cases: fibonacci(0) = 0, fibonacci(1) = 1
    if n <= 1:
        return n
    # For n >= 2, calculate using iteration
    a, b = 0, 1
    for _ in range(2, n + 1):
        a, b = b, a + b
    return b
