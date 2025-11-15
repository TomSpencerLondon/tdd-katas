"""Money kata implementation - Following Kent Beck's TDD by Example."""

from abc import ABC, abstractmethod


# ============ Chapter 12: Expression Pattern ============
# Expression = anything that represents money (Money, Sum, etc.)
# Think: (2 + 3) is an expression, so is ($5 + 10 CHF)

class Expression(ABC):
    """Interface for any money-like value that can be reduced to a currency."""

    @abstractmethod
    def reduce(self, bank, to):
        """Reduce this expression to Money in the given currency."""
        pass


# ============ Chapter 13: Sum Expression ============
# Sum represents the addition of two Money amounts (not yet reduced)

class Sum(Expression):
    """Represents the sum of two Money amounts - an unevaluated expression."""

    def __init__(self, augend, addend):
        """Create a sum. Augend is the first value, addend is the second."""
        self.augend = augend  # The first money being added
        self.addend = addend  # The second money being added

    def reduce(self, bank, to):
        """Reduce this sum to Money by adding the amounts."""
        amount = self.augend.amount + self.addend.amount
        return Money(amount, to)


class Money(Expression):
    """Money in different currencies - ONE class to rule them all!"""

    def __init__(self, amount, currency):
        self.amount = amount
        self._currency = currency

    def times(self, multiplier):
        return Money(self.amount * multiplier, self._currency)

    def plus(self, addend):
        """Add two money amounts - returns a Sum Expression (not yet reduced)."""
        return Sum(self, addend)  # Ch 13: Return Sum, not Money!

    def reduce(self, bank, to):
        """Money reduces to target currency using Bank's exchange rate."""
        # Ch 14: Ask the bank for the exchange rate!
        rate = bank.rate(self._currency, to)
        # Divide amount by rate to convert
        # Example: 2 CHF with rate 2 → 2/2 = 1 USD
        return Money(self.amount / rate, to)

    def currency(self):
        return self._currency

    @staticmethod
    def dollar(amount):
        return Money(amount, "USD")

    @staticmethod
    def franc(amount):
        return Money(amount, "CHF")

    def __eq__(self, other):
        return self.amount == other.amount and self._currency == other._currency

    def __str__(self):
        return f"{self.amount} {self._currency}"


# ============ Chapter 12, 14: Bank ============
# Bank knows exchange rates and can "reduce" Expressions to Money

class Bank:
    """Bank applies exchange rates to reduce Expressions to Money."""

    def __init__(self):
        """Initialize the bank with an empty rate table."""
        self._rates = {}  # Dictionary to store exchange rates

    def reduce(self, source, to):
        """Reduce an Expression to Money in the target currency."""
        # Ch 13: Use polymorphism! Let the Expression reduce itself.
        return source.reduce(self, to)

    def add_rate(self, from_currency, to_currency, rate):
        """Add an exchange rate. Example: CHF to USD at rate 2 means 2 CHF = 1 USD."""
        # Ch 14: Store rates in a dictionary using (from, to) as key
        key = (from_currency, to_currency)  # Python tuple as dict key
        self._rates[key] = rate

    def rate(self, from_currency, to_currency):
        """Get the exchange rate from one currency to another."""
        # Ch 14: Special case - converting to same currency always has rate 1
        if from_currency == to_currency:
            return 1

        # Look up the rate in our table
        key = (from_currency, to_currency)
        return self._rates.get(key, 1)


# Dollar and Franc subclasses DELETED! No longer needed.
# The factory methods return Money directly with different currencies.
