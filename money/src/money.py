"""Money kata implementation - Following Kent Beck's TDD by Example."""


from abc import ABC, abstractmethod


class Money(ABC):
    """Base class for money in different currencies."""

    def __init__(self, amount):
        self.amount = amount

    @abstractmethod
    def times(self, multiplier):
        pass

    @staticmethod
    def dollar(amount):
        return Dollar(amount)

    @staticmethod
    def franc(amount):
        return Franc(amount)

    def __eq__(self, other):
        return self.amount == other.amount and self.__class__ == other.__class__


class Dollar(Money):
    """Represents an amount in dollars."""

    def times(self, multiplier):
        return Dollar(self.amount * multiplier)


class Franc(Money):
    """Represents an amount in francs."""

    def times(self, multiplier):
        return Franc(self.amount * multiplier)
