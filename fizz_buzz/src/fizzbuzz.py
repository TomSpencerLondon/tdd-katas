"""FizzBuzz kata implementation."""


def fizzbuzz_of(number):
    """Convert a number to FizzBuzz string.

    Args:
        number: Integer to convert

    Returns:
        String representation according to FizzBuzz rules
    """
    if number % 15 == 0:
        return "FizzBuzz"
    if number % 3 == 0:
        return "Fizz"
    if number % 5 == 0:
        return "Buzz"
    return str(number)


def fizzbuzz_1_to(n=100):
    """Generate FizzBuzz sequence from 1 to n.

    Args:
        n: Upper limit of sequence (default 100)

    Returns:
        List of FizzBuzz strings from 1 to n
    """
    return [fizzbuzz_of(i) for i in range(1, n + 1)]
