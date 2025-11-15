# 99 Bottles of OOP

A refactoring kata based on Sandi Metz's book "99 Bottles of OOP" - focusing on code smells, refactoring techniques, and discovering good object-oriented design.

## The Problem

Write a program that generates the lyrics to the classic counting song about beverages on a wall.

### Song Structure

The song counts down from 99 to 0, with each verse following a pattern:

**Typical verse (e.g., verse 99):**
```
99 bottles of beer on the wall, 99 bottles of beer.
Take one down and pass it around, 98 bottles of beer on the wall.
```

**Special cases:**
- **Verse 2**: Uses "1 bottle" (singular) in the last line
- **Verse 1**: Says "Take it down" instead of "Take one down", ends with "no more bottles"
- **Verse 0**: Starts with "No more bottles", says "Go to the store and buy some more"

### The Challenge

This kata is **NOT** about writing the song from scratch. It's about:

1. **Starting with working code** (even if it's messy)
2. **Identifying code smells** (duplication, complexity, poor naming)
3. **Refactoring systematically** to reveal better design
4. **Discovering abstractions** that emerge from the code itself

## What Makes This Kata Special

Unlike the Money kata (which builds features with TDD), 99 Bottles teaches:

- **Code Smells**: Recognize when code "smells bad"
- **Refactoring Recipes**: Specific techniques to improve code safely
- **Horizontal Refactoring**: Make many small changes in one dimension before changing dimension
- **Discovering Abstractions**: Let patterns emerge rather than planning upfront
- **Flocking Rules**: Conway's Rules for finding the right abstractions
- **Open/Closed Principle**: Code open for extension, closed for modification

## The Journey (9 Chapters)

### Part I: Foundations
- **Chapter 1**: Rediscovering Simplicity - What makes code simple?
- **Chapter 2**: Test Driving Shameless Green - Write the "obvious" solution first
- **Chapter 3**: Unearthing Concepts - Finding hidden abstractions

### Part II: Refactoring
- **Chapter 4**: Practicing Horizontal Refactoring - Flocking to DRY
- **Chapter 5**: Separating Responsibilities - Extract classes
- **Chapter 6**: Achieving Openness - Open/Closed Principle
- **Chapter 7**: Manufacturing Intelligence - Factory patterns

### Part III: Design
- **Chapter 8**: Developing a Programming Aesthetic - Code quality metrics
- **Chapter 9**: Reaping the Benefits of Design - Ease of adding features

## Key Concepts

### Code Smells
- **Duplication**: Same logic in multiple places
- **Inconsistency**: Different ways of doing the same thing
- **Poor Naming**: Unclear or missing names for concepts
- **Complexity**: Nested conditionals, ternaries within ternaries

### Refactoring Techniques
- **Extract Method**: Pull logic into named methods
- **Replace Conditional with Polymorphism**: Objects instead of if/else
- **Introduce Parameter Object**: Group related parameters
- **Remove Dead Code**: Delete unused paths

### Flocking Rules (Conway's Rules)
1. Select things that are most alike
2. Find the smallest difference between them
3. Make the smallest change to remove that difference

Repeat until you see patterns emerge, then extract abstractions.

## Ruby vs Python

This kata was originally written in Ruby, but we'll implement it in Ruby to:
- Learn Ruby's object-oriented features
- Experience Ruby's "everything is an object" philosophy
- See how Ruby enables elegant DSLs and refactoring

### Ruby Basics for Python Programmers

```ruby
# Ruby                          # Python equivalent
class Bottles                   class Bottles:
  def song                        def song(self):
    verses(99, 0)                   return self.verses(99, 0)
  end
end

# Calling methods
bottles = Bottles.new           bottles = Bottles()
bottles.song                    bottles.song()

# Blocks (Ruby's lambda)
[1,2,3].map { |n| n * 2 }      [n * 2 for n in [1,2,3]]

# String interpolation
"#{n} bottles"                  f"{n} bottles"

# Everything is an object!
5.times { puts "Hi" }           # No Python equivalent
"hello".upcase                  "hello".upper()
```

## Running Tests

```bash
# Run tests
ruby test/bottles_test.rb

# Or with minitest directly
ruby -I lib:test test/bottles_test.rb
```

## Learning Goals

By the end of this kata, you will:

1. **Recognize code smells** intuitively
2. **Apply refactoring recipes** systematically
3. **Discover good abstractions** from messy code
4. **Understand Open/Closed Principle** through practice
5. **Experience Ruby's OO philosophy** hands-on
6. **Learn when to stop refactoring** (good enough beats perfect)

## The "Shameless Green" Philosophy

Sandi Metz teaches: **First, make it work. Then, make it right.**

The first solution should be:
- **Simple** enough to understand
- **Complete** (all tests pass)
- **Shameless** about duplication

Only THEN do you refactor, guided by:
- Tests (your safety net)
- Code smells (your nose)
- Flocking rules (your process)

## Chapter-by-Chapter Progress

### **Chapter 2: Test Driving Shameless Green** ✅ COMPLETE!

**Step 1: First Test (Verse 99)**
- ✅ Created `test/bottles_test.rb` with test for verse 99
- ✅ Created `lib/bottles.rb` with Bottles class
- ✅ Followed TDD: Red (no file) → Red (no class) → Red (no method) → Red (wrong return) → Green!
- ✅ Hardcoded the entire verse 99 lyrics - **Shameless!**

**Step 2: Generalization (Verse 3)**
- ✅ Added test for verse 3
- ✅ Used Ruby string interpolation `#{number}` and `#{number-1}`
- ✅ Both tests passing

**Step 3: Special Cases**
- ✅ Verse 2: Added ternary for singular "bottle" when count is 1
- ✅ Verse 1: Special case with "Take it down" and "no more bottles"
- ✅ Verse 0: Completely different verse - "No more" and "Go to the store"

**Final Shameless Green Implementation:**
```ruby
class Bottles
  def verse(number)
    if number == 0
      "No more bottles of beer on the wall, " +
      "no more bottles of beer.\n" +
      "Go to the store and buy some more, " +
      "99 bottles of beer on the wall.\n"
    elsif number == 1
      "1 bottle of beer on the wall, " +
      "1 bottle of beer.\n" +
      "Take it down and pass it around, " +
      "no more bottles of beer on the wall.\n"
    else
      "#{number} bottles of beer on the wall, " +
      "#{number} bottles of beer.\n" +
      "Take one down and pass it around, " +
      "#{number-1} #{(number-1) == 1 ? 'bottle' : 'bottles'} of beer on the wall.\n"
    end
  end
end
```

**Code Smells Introduced (Intentionally!):**
- **Duplication**: "bottles of beer on the wall" repeated many times
- **Conditionals**: if/elsif/else creates complexity
- **String duplication**: Similar patterns across all branches
- **Mixed abstraction levels**: Ternary within string interpolation

**Status**: 5 tests passing (verses 99, 3, 2, 1, 0)
**Key Learning**: Make it work first! We'll refactor in later chapters.

---

### **Chapter 3: Unearthing Concepts** ⬅️ WE ARE HERE

**The New Requirement**: Output "1 six-pack" where it currently says "6 bottles"

**Key Concepts Introduced:**
- **Open/Closed Principle**: Code should be open for extension, closed for modification
- **Code Smells**: Identified Duplicated Code and Switch Statements
- **Flocking Rules**: Systematic approach to refactoring
  1. Select the things that are most alike
  2. Find the smallest difference between them
  3. Make the simplest change to remove that difference

**Refactoring Journey:**

**Step 1: Add song() and verses() methods**
- ✅ `song()`: Returns full song (verses 99 to 0)
- ✅ `verses(upper, lower)`: Returns range of verses joined with newlines
- Preparing for later refactoring

**Step 2: Extract container() - Remove verse 2 special case**
- ✅ Applied Flocking Rules: Compared else branch with verse 2
- ✅ Found smallest difference: "bottle" vs "bottles"
- ✅ Extracted `container(number)` method
- ✅ Result: else branch now handles verse 2 correctly!
- Reduced from 4 branches to 3 branches

**Step 3: Extract quantity() and action() - Remove verse 1 special case**
- ✅ Compared else branch with verse 1
- ✅ Found differences: "Take it down" vs "Take one down", "no more" vs number
- ✅ Extracted `quantity(number)`: handles "no more" vs number.to_s
- ✅ Extracted `action(number)`: handles "Take it down" vs "Take one down"
- ✅ Generalized else branch to use all helper methods
- ✅ Result: Reduced from 3 branches to 2 branches!

**Current Implementation:**
```ruby
class Bottles
  def song
    verses(99, 0)
  end

  def verses(upper, lower)
    upper.downto(lower).collect { |i| verse(i) }.join("\n")
  end

  def verse(number)
    if number == 0
      "No more bottles of beer on the wall, " +
      "no more bottles of beer.\n" +
      "Go to the store and buy some more, " +
      "99 bottles of beer on the wall.\n"
    else
      "#{quantity(number)} #{container(number)} of beer on the wall, " +
      "#{quantity(number)} #{container(number)} of beer.\n" +
      "#{action(number)}, " +
      "#{quantity(number-1)} #{container(number-1)} of beer on the wall.\n"
    end
  end

  def container(number)
    number == 1 ? 'bottle' : 'bottles'
  end

  def quantity(number)
    number == 0 ? 'no more' : number.to_s
  end

  def action(number)
    number == 1 ? 'Take it down and pass it around' : 'Take one down and pass it around'
  end
end
```

**What We Achieved:**
- **Before**: 4 conditional branches (verse 0, 1, 2, else) with lots of duplication
- **After**: 2 conditional branches (verse 0, else) with extracted helper methods
- **Verses 1-99** all handled by single else branch using polymorphic helper methods
- **Verse 0** still special (completely different structure)

**Key Learnings:**
- **Flocking Rules work!** We didn't need to know the final design upfront
- **Abstractions emerged** from systematic application of simple rules
- **Small steps are safe** - tests passed after every change
- **Faith in the process** - nibbling away at code smells reveals the path forward

**Step 4: Extract pronoun() - Further DRY**
- ✅ Created `pronoun(number)` combining `quantity()` and `container()`
- ✅ Eliminates repeated calls to both methods
- ✅ Applied to verse 0 and else branch
- Code now very DRY!
- Note: "pronoun" may not be the perfect domain name - could be "quantity_phrase" or similar

**Step 5: Implement Six-Pack Requirement - OPEN/CLOSED ACHIEVED!** ✅
- ✅ Added special case in `pronoun()` for number == 6
- ✅ Returns "1 six-pack" instead of "6 bottles"
- ✅ Added tests for verse 6 and verse 7
- **KEY**: Only modified `pronoun()` method - did NOT touch `verse()` method!
- **This is the Open/Closed Principle in action!**

**Final Implementation (Chapter 3):**
```ruby
def pronoun(number)
  if number == 6
    "1 six-pack"
  else
    "#{quantity(number)} #{container(number)}"
  end
end
```

**What Changed to Add Six-Pack:**
- **Before refactoring**: Would need to add 2 more branches to conditional (verse 6, verse 7)
- **After refactoring**: Only 1 line change in 1 method (`pronoun()`)
- **Code was OPEN** to the new requirement!

**Status**: 8 tests passing (including verse 6 and verse 7)
**Chapter 3 Complete!** Successfully demonstrated:
- Flocking Rules remove duplication systematically
- Abstractions emerge from process, not upfront design
- Open/Closed Principle makes code extensible
**Next**: Chapter 4 - Practicing Horizontal Refactoring (or continue polishing Chapter 3)

---

## Technologies

- **Language**: Ruby 2.6+
- **Testing**: Minitest (built-in)
- **Book**: "99 Bottles of OOP" by Sandi Metz, Katrina Owen, and TJ Stankus

---

**Based on**: "99 Bottles of OOP" by Sandi Metz
**Implementation**: Ruby (original book uses Ruby)
**Generated with**: [Claude Code](https://claude.com/claude-code)
**Author**: Tom Spencer
**Date**: November 2025
