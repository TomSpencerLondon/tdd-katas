"""Money kata implementation - Following Kent Beck's TDD by Example."""


class Money:
    """Money in different currencies - ONE class to rule them all!"""

    def __init__(self, amount, currency):
        self.amount = amount
        self._currency = currency

    def times(self, multiplier):
        return Money(self.amount * multiplier, self._currency)

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


# Dollar and Franc subclasses DELETED! No longer needed.
# The factory methods return Money directly with different currencies.
