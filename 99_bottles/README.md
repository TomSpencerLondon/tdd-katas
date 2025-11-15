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
