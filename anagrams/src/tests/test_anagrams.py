"""Tests for Anagrams kata.

Following TDD Red-Green-Refactor cycle.
See CLAUDE.md for the step-by-step guide.

Add tests one at a time, following the phases:
- Phase A: Base cases (empty, single character)
- Phase B: Two characters
- Phase C: Three characters with duplicates
- Phase D: Four characters (biro example)
- Phase E: Edge cases
"""

from src.anagrams import generate_anagrams


# Phase A: Base Cases
def test_empty_string():
    """Test that empty string returns list with empty string."""
    result = generate_anagrams("")
    assert result == [""]


def test_single_character():
    """Test that single character 'a' returns ['a']."""
    result = generate_anagrams("a")
    assert result == ["a"]

# Phase B: Two Characters
def test_two_different_characters():
    """Test that 'ab' returns ['ab', 'ba']."""
    result = generate_anagrams("ab")
    assert sorted(result) == sorted(["ab", "ba"])
    assert len(result) == 2


def test_two_same_characters():
    """Test that 'aa' returns ['aa'] (handle duplicates)."""
    result = generate_anagrams("aa")
    assert result == ["aa"]
    assert len(result) == 1

# Phase C: Three Characters
def test_three_different_characters():
    """Test that 'abc' returns 6 permutations."""
    result = generate_anagrams("abc")
    expected = ["abc", "acb", "bac", "bca", "cab", "cba"]
    assert sorted(result) == sorted(expected)
    assert len(result) == 6


def test_three_with_duplicates():
    """Test that 'aab' returns 3 unique permutations."""
    result = generate_anagrams("aab")
    expected = ["aab", "aba", "baa"]
    assert sorted(result) == sorted(expected)
    assert len(result) == 3

# Phase D: Four Characters (The Example)
def test_four_characters_biro():
    """Test that 'biro' returns 24 permutations (matches problem statement)."""
    result = generate_anagrams("biro")
    assert len(result) == 24
    assert len(set(result)) == 24  # All unique
    # Verify a few specific ones from the problem statement
    assert "biro" in result
    assert "bior" in result
    assert "obri" in result
    assert "orib" in result

# Phase E: Edge Cases
def test_all_same_characters():
    """Test that 'aaa' returns ['aaa'] (all same)."""
    result = generate_anagrams("aaa")
    assert result == ["aaa"]
    assert len(result) == 1


def test_longer_with_duplicates():
    """Test that 'aabb' returns correct unique permutations."""
    result = generate_anagrams("aabb")
    # Should have 6 unique permutations: aabb, abab, abba, baab, baba, bbaa
    assert len(result) == 6
    assert len(set(result)) == 6
    # Verify a few specific ones
    assert "aabb" in result
    assert "abab" in result
    assert "bbaa" in result
