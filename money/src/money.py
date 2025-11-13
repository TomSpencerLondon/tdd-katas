"""Money kata implementation - Following Kent Beck's TDD by Example."""


class Dollar:
    """Represents an amount in dollars."""

    def __init__(self, amount):
        self.amount = amount

    def times(self, multiplier):
        return Dollar(self.amount * multiplier)

    def __eq__(self, other):
        return self.amount == other.amount
