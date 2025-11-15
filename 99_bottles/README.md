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

---

### **Chapter 4: Practicing Horizontal Refactoring** ✅ COMPLETE!

**The Power of Systematic Refactoring**

Continuing with Flocking Rules to eliminate ALL conditionals from `verse()` method.

**Step 1: Introduce successor() - The "What Comes Next" Abstraction**
- ✅ Discovered deep abstraction: What verse number comes next?
- ✅ Created `successor(number)` method returning `number - 1` normally, `99` for verse 0
- ✅ Replaced all `number-1` references with `successor(number)`
- **Key Insight**: This abstraction wasn't obvious upfront - emerged from systematic refactoring!

**Step 2: Extend action() to Handle Verse 0**
- ✅ Added verse 0 case to `action()`: "Go to the store and buy some more"
- ✅ Updated verse 0 to use `action(number)` instead of hardcoded string
- ✅ Result: Verse 0 and else branch now STRUCTURALLY IDENTICAL!

**Step 3: Final Unification - DELETE THE CONDITIONAL! 🎉**
- ✅ Noticed only difference: verse 0 capitalizes first pronoun
- ✅ Made else branch also capitalize first pronoun
- ✅ **Deleted the entire if/else conditional**
- ✅ All 8 tests still passing!

**Final verse() Implementation (Chapter 4):**
```ruby
def verse(number)
  "#{pronoun(number).capitalize} of beer on the wall, " +
  "#{pronoun(number)} of beer.\n" +
  "#{action(number)}, " +
  "#{pronoun(successor(number))} of beer on the wall.\n"
end
```

**The Journey:**
- **Chapter 2 Start**: 4 conditional branches (verse 0, 1, 2, else) with massive duplication
- **Chapter 3 Progress**: 2 conditional branches (verse 0, else) with extracted helpers
- **Chapter 4 Achievement**: 0 conditional branches - SINGLE implementation handles ALL cases!

**Helper Methods Summary:**
- `container(number)`: "bottle" vs "bottles"
- `quantity(number)`: "no more" vs number.to_s
- `action(number)`: Handles verses 0, 1, and rest differently
- `pronoun(number)`: Combines quantity+container, handles six-pack
- `successor(number)`: What verse comes next (0→99, else→number-1)

**What We Learned:**
- **Horizontal Refactoring**: Make many small changes in one dimension before changing dimension
- **Flocking Rules Work**: Systematic application reveals hidden abstractions
- **Faith in Process**: Don't need to know the end design - it emerges from small steps
- **Polymorphism via Methods**: Methods handle variation, not conditionals
- **Deep Abstractions**: `successor()` wasn't obvious - emerged from removing duplication

**Current Code State (After Chapter 4):**
```ruby
class Bottles
  def song
    verses(99, 0)
  end

  def verses(upper, lower)
    upper.downto(lower).collect { |i| verse(i) }.join("\n")
  end

  def verse(number)
    "#{pronoun(number).capitalize} of beer on the wall, " +
    "#{pronoun(number)} of beer.\n" +
    "#{action(number)}, " +
    "#{pronoun(successor(number))} of beer on the wall.\n"
  end

  def container(number)
    number == 1 ? 'bottle' : 'bottles'
  end

  def quantity(number)
    number == 0 ? 'no more' : number.to_s
  end

  def action(number)
    if number == 0
      'Go to the store and buy some more'
    elsif number == 1
      'Take it down and pass it around'
    else
      'Take one down and pass it around'
    end
  end

  def pronoun(number)
    if number == 6
      "1 six-pack"
    else
      "#{quantity(number)} #{container(number)}"
    end
  end

  def successor(number)
    if number == 0
      99
    else
      number - 1
    end
  end
end
```

**Comparison**:
- **Lines of code**: ~50 lines (from ~40 in Chapter 2)
- **verse() conditionals**: 0 (from 4 in Chapter 2!)
- **Helper methods**: 6 (container, quantity, action, pronoun, successor, verses)
- **Cyclomatic complexity of verse()**: 1 (no branches!)
- **Tests passing**: 8/8
- **Open/Closed**: Can add new requirements by modifying helper methods only

**Status**: 8 tests passing, verse() method has ZERO conditionals!
**Chapter 4 Complete!** Successfully eliminated all conditionals through systematic horizontal refactoring.

---

### **Chapter 5: Separating Responsibilities** ✅ COMPLETE!

**Curing Primitive Obsession with Extract Class**

Identified the dominant code smell and extracted a new class to represent bottle numbers.

**Code Smell Identified: Primitive Obsession**
- The "flocked five" methods (`container`, `quantity`, `action`, `pronoun`, `successor`) all:
  - Take `number` as an argument (representing a **bottle number**, not a verse number)
  - Have the same shape (conditional testing `number` for equality)
  - Depend more on the `number` argument than on the class as a whole
  - Could be considered "private"
- This is **Primitive Obsession** - using a primitive (Integer) to represent a domain concept (bottle number)

**The Cure: Extract Class**
- Recipe: Extract the methods that obsess on the primitive into a new class
- New class name: `BottleNumber` (named after what it **is**, not what it **does**)
- BottleNumber represents a **number**, not a bottle (it's an idea/abstraction, not a thing)

**Step 1: Create Empty BottleNumber Class**
- ✅ Created empty `BottleNumber` class
- ✅ Tests still passing (parsing new code)

**Step 2: Copy Methods to BottleNumber**
- ✅ Copied all five methods (`container`, `quantity`, `action`, `pronoun`, `successor`) to BottleNumber
- ✅ Original methods still in Bottles (duplication is temporary)
- ✅ Tests still passing (parsing duplicated code)

**Step 3: Add attr_reader and initialize**
- ✅ Added `attr_reader :number` to create number() method
- ✅ Added `initialize(number)` to set `@number` instance variable
- ✅ BottleNumber now holds onto the number value

**Step 4: Wire Bottles to Forward to BottleNumber**
- ✅ Changed each Bottles method to forward to BottleNumber:
  - `BottleNumber.new(number).quantity(number)` (parse and execute)
  - Then moved to last line (parse, execute, use result)
  - Then deleted old implementation
- ✅ Repeated for all five methods
- ✅ Tests passing - BottleNumber fully wired in!

**Step 5: Remove Redundant number Arguments**
- BottleNumber instances **know** their number, so methods don't need arguments
- **Recipe for removing arguments**:
  1. Rename parameter to `delete_me=nil` (makes it optional)
  2. Remove argument from all senders
  3. Delete the parameter entirely
- ✅ Applied to `quantity`, `container`, `action`, `pronoun`, `successor`
- **Caught Issue**: `pronoun` calls `quantity(number)` and `container(number)` internally!
  - Fixed by updating BottleNumber.pronoun to call `quantity` and `container` without arguments
  - This is like the book's example with `action` calling `pronoun`

**Step 6: Cache BottleNumber Instances in verse()**
- ✅ Created `bottle_number = BottleNumber.new(number)` in verse()
- ✅ Created `next_bottle_number = BottleNumber.new(bottle_number.successor)`
- ✅ Updated template to send messages to cached objects:
  - `bottle_number.pronoun.capitalize`
  - `bottle_number.pronoun`
  - `bottle_number.action`
  - `next_bottle_number.pronoun`

**Step 7: Delete Forwarding Methods**
- ✅ Deleted all five forwarding methods from Bottles
- ✅ Bottles.verse() now talks directly to BottleNumber objects
- ✅ Tests still passing!

**Key Insights from Chapter 5**:

**On Object-Oriented Design**:
- **Conditionals indicate missing objects**: The flocked five took an argument and examined it - deeply non-OO!
- **Objects should be message senders, not examiners**: Want to send messages to smart objects, not supply behavior for dumb ones
- **Modeling abstractions**: BottleNumber is an **idea** (a number with bottle-ish behavior), not a **thing** (an actual bottle)
- **Naming**: Classes are named after what they **are** (BottleNumber), methods after what they **mean**

**On Immutability**:
- **Immutable objects are easier to reason about**: They never change, so what you see at creation is what you get
- **Easier to test**: No need to set up complex state changes
- **Thread safe**: Can't break shared state if it doesn't change
- **Performance concerns often premature**: Create new objects freely, optimize later if needed

**On Caching**:
- **Caching is easy, cache invalidation is hard**: Knowing when to update a cache adds complexity
- **Premature optimization raises costs**: Humans are bad at predicting performance problems
- **Write simple code first**: Measure performance, then optimize slowest parts
- **verse() creates 2 BottleNumber instances** (down from 9 if we didn't cache!)

**On Liskov Violations**:
- **successor still returns a number, not a BottleNumber**: This violates the principle!
- **Promises broken**: Method named `successor` should return an object with same API as receiver
- **Temporary shameless code**: Line 12 shows the violation: `BottleNumber.new(bottle_number.successor)`
- We'll fix this in the next chapter

**Final Code State (After Chapter 5):**
```ruby
class Bottles
  def song
    verses(99, 0)
  end

  def verses(upper, lower)
    upper.downto(lower).collect { |i| verse(i) }.join("\n")
  end

  def verse(number)
    bottle_number = BottleNumber.new(number)
    next_bottle_number = BottleNumber.new(bottle_number.successor)

    "#{bottle_number.pronoun.capitalize} of beer on the wall, " +
    "#{bottle_number.pronoun} of beer.\n" +
    "#{bottle_number.action}, " +
    "#{next_bottle_number.pronoun} of beer on the wall.\n"
  end
end

class BottleNumber
  attr_reader :number

  def initialize(number)
    @number = number
  end

  def container
    number == 1 ? 'bottle' : 'bottles'
  end

  def quantity
    number == 0 ? 'no more' : number.to_s
  end

  def action
    if number == 0
      'Go to the store and buy some more'
    elsif number == 1
      'Take it down and pass it around'
    else
      'Take one down and pass it around'
    end
  end

  def pronoun
    if number == 6
      "1 six-pack"
    else
      "#{quantity} #{container}"
    end
  end

  def successor
    if number == 0
      99
    else
      number - 1
    end
  end
end
```

**Comparison**:
- **Classes**: 2 (was 1)
- **Bottles methods**: 3 (song, verses, verse)
- **BottleNumber methods**: 6 (container, quantity, action, pronoun, successor, initialize)
- **Conditionals in verse()**: 0 (still!)
- **Conditionals total**: 5 (in BottleNumber helper methods)
- **Responsibilities**: Separated! Bottles knows about verses, BottleNumber knows about bottle numbers
- **Tests passing**: 8/8
- **Known issues**: Liskov violation in successor (returns Integer, not BottleNumber)

**What We Achieved**:
- ✅ Extracted BottleNumber class to cure Primitive Obsession
- ✅ Bottles is now free of conditionals
- ✅ Clear separation of responsibilities
- ✅ verse() method is a clean template sending messages to objects
- ✅ Immutable BottleNumber objects
- ✅ Simple caching reduces object creation

**What Still Needs Work**:
- ❌ successor() returns Integer instead of BottleNumber (Liskov violation)
- ❌ Still have conditionals (moved to BottleNumber)
- ❌ No unit tests for BottleNumber (relies on Bottles tests)
- ❌ Code not yet open to six-pack requirement

**Status**: 8 tests passing, 2 classes with separated responsibilities!
**Chapter 5 Complete!** Successfully extracted BottleNumber class and cured Primitive Obsession.
**Next**: Chapter 6 - Achieving Openness (fix Liskov violation, make code open to six-pack)

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
