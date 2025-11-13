"""Leap Year kata implementation.

Implement the is_leap_year function following the Gregorian Calendar rules.
See CLAUDE.md for the step-by-step TDD guide.
"""


def is_leap_year(year):
    """Determine if a year is a leap year according to Gregorian Calendar rules.

    Args:
        year: An integer representing the year

    Returns:
        True if the year is a leap year, False otherwise

    Rules:
        1. Years divisible by 400 ARE leap years (e.g., 2000)
        2. Years divisible by 100 but not by 400 are NOT leap years (e.g., 1900)
        3. Years divisible by 4 but not by 100 ARE leap years (e.g., 2016)
        4. Years not divisible by 4 are NOT leap years (e.g., 2017)
    """
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
