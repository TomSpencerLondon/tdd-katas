"""Money kata implementation - Following Kent Beck's TDD by Example."""


class Money:
    """Money in different currencies - no longer abstract!"""

    def __init__(self, amount, currency):
        self.amount = amount
        self._currency = currency

    def times(self, multiplier):
        return Money(self.amount * multiplier, self._currency)

    def currency(self):
        return self._currency

    @staticmethod
    def dollar(amount):
        return Dollar(amount, "USD")

    @staticmethod
    def franc(amount):
        return Franc(amount, "CHF")

    def __eq__(self, other):
        return self.amount == other.amount and self._currency == other._currency

    def __str__(self):
        return f"{self.amount} {self._currency}"


class Dollar(Money):
    """Represents an amount in dollars."""

    def __init__(self, amount, currency):
        super().__init__(amount, currency)


class Franc(Money):
    """Represents an amount in francs."""

    def __init__(self, amount, currency):
        super().__init__(amount, currency)
