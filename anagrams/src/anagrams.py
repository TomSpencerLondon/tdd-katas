"""Anagrams kata implementation.

Implement the generate_anagrams function following the TDD guide.
See CLAUDE.md for the step-by-step TDD guide.
"""

from itertools import permutations


def generate_anagrams(word):
    """Generate all unique anagrams (permutations) of a word.

    Args:
        word: String to generate anagrams for

    Returns:
        List of strings containing all unique permutations

    Examples:
        generate_anagrams("") → [""]
        generate_anagrams("a") → ["a"]
        generate_anagrams("ab") → ["ab", "ba"]
        generate_anagrams("abc") → ["abc", "acb", "bac", "bca", "cab", "cba"]
        generate_anagrams("biro") → [... 24 permutations ...]
    """
    # Handle empty string
    if not word:
        return [""]

    # Generate all permutations using itertools
    # Convert each tuple of characters to a string
    # Use list(set(...)) to remove duplicates
    return list(set(''.join(p) for p in permutations(word)))
