"""Money kata implementation - Following Kent Beck's TDD by Example."""


from abc import ABC, abstractmethod


class Money(ABC):
    """Base class for money in different currencies."""

    def __init__(self, amount, currency):
        self.amount = amount
        self._currency = currency

    @abstractmethod
    def times(self, multiplier):
        pass

    def currency(self):
        return self._currency

    @staticmethod
    def dollar(amount):
        return Dollar(amount, "USD")

    @staticmethod
    def franc(amount):
        return Franc(amount, "CHF")

    def __eq__(self, other):
        return self.amount == other.amount and self.__class__ == other.__class__


class Dollar(Money):
    """Represents an amount in dollars."""

    def __init__(self, amount, currency):
        super().__init__(amount, currency)

    def times(self, multiplier):
        return Money.dollar(self.amount * multiplier)


class Franc(Money):
    """Represents an amount in francs."""

    def __init__(self, amount, currency):
        super().__init__(amount, currency)

    def times(self, multiplier):
        return Money.franc(self.amount * multiplier)
