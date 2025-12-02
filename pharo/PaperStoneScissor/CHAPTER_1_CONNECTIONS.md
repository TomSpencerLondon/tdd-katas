# Connecting Rock-Paper-Scissors to OOSC2 Chapter 1

This document connects your Pharo game design to the concepts we studied in the Ruby exercises for Chapter 1.

---

## Your Game vs. Chapter 1 Ruby Exercises

### Exercise 1: Correctness & Robustness → Your Game

**In the Ruby Exercise**, we showed:
- **Bad**: No validation, crashes on bad input
- **Good**: Clear contracts, graceful error handling

**In Your Pharo Game**:

**Current State** (Missing Contracts):
```smalltalk
Weapon >> play: aWeapon withResultHandler: aResultHandler
  "What if aWeapon is nil? What if handler is wrong type?"
  ^ aWeapon playAgainst: self withResultHandler: aResultHandler
```

**Proposed Improvement** (Add Correctness):
```smalltalk
Weapon >> play: aWeapon withResultHandler: aResultHandler
  "CORRECTNESS: Define clear contract"
  self assert: aWeapon isNotNil description: 'Weapon required'.
  self assert: (aWeapon isKindOf: Weapon) description: 'Must be a Weapon'.

  "ROBUSTNESS: Handle abnormal cases"
  (aResultHandler respondsTo: #handleDraw)
    ifFalse: [ self error: 'Invalid result handler protocol' ].

  ^ aWeapon playAgainst: self withResultHandler: aResultHandler
```

**Meyer's Point**: "Correctness addresses the behavior of a system in cases covered by its specification; robustness characterizes what happens outside of that specification."

---

### Exercise 2: Extendibility → Your Game

**In the Ruby Exercise**, we showed:
- **Bad**: Centralized `case` statement - must modify existing code to add shapes
- **Good**: Decentralized classes - add new shapes without touching existing code

**In Your Pharo Game**:

You're doing GREAT with decentralization! But there's a subtle coupling:

**Current State** (Good, but could be better):
```smalltalk
"Adding a new weapon (Fire) requires modifying EVERY existing weapon:"
Stone >> playAgainstFire: aFire withResultHandler: handler
  "Must add this method to Stone, Paper, Scissors, Lizard, Spock"
  handler fireExtinguishesStone

"And Fire must know about all existing weapons:"
Fire >> playAgainstStone: aStone withResultHandler: handler
  handler fireExtinguishesStone

Fire >> playAgainstPaper: aPaper withResultHandler: handler
  handler fireBurnsPaper

"... 5 methods total"
```

**Problem**: Adding 1 weapon requires changing 6 classes!

**Proposed Improvement** (Table-Driven for Maximum Extendibility):
```smalltalk
"Define rules in ONE place:"
WeaponRules >> initialize
  super initialize.
  rules := Dictionary new.
  self
    weapon: #Stone defeats: #Scissors reason: #stoneCrushesScissors;
    weapon: #Stone defeats: #Lizard reason: #stoneCrushesLizard;
    "... all other rules ..."
    weapon: #Fire defeats: #Paper reason: #fireBurnsPaper;
    weapon: #Fire defeats: #Scissors reason: #fireMeltsScissors

"Now adding Fire only requires:"
"1. Create Fire class (inherits play: from Weapon)"
"2. Add 2 lines per rule to WeaponRules"
"3. No changes to any existing weapon!"

Weapon subclass: #Fire
  "That's it! The rules table handles everything."
```

**Meyer's Point**: "The object-oriented method helps designers produce systems whose structure remains both simple (even for large systems) and decentralized."

Your current design is decentralized (good!), but we can make it even simpler when it comes to adding weapons.

---

### Exercise 3: Reusability → Your Game

**In the Ruby Exercise**, we showed:
- **Bad**: Same statistics code duplicated across GradeAnalyzer, SalesAnalyzer, etc.
- **Good**: Generic `CollectionStatistics` module used everywhere

**In Your Pharo Game**:

**Current State** (Some duplication in tests):
```smalltalk
StoneTest >> testStoneBeatsScissors
  | stone scissors handler |
  stone := Stone new.
  scissors := Scissors new.
  handler := TestResultHandler new.
  stone play: scissors withResultHandler: handler.
  self assert: handler lastOutcome equals: #stoneCrushesScissors

StoneTest >> testStoneBeatsLizard
  | stone lizard handler |
  stone := Stone new.
  lizard := Lizard new.
  handler := TestResultHandler new.
  stone play: lizard withResultHandler: handler.
  self assert: handler lastOutcome equals: #stoneCrushesLizard

"Same pattern repeated 50+ times!"
```

**Proposed Improvement** (Reusable Test Helper):
```smalltalk
"Create once:"
WeaponTestCase >> assertWeapon: winner beats: loser outcome: expectedSymbol
  | handler |
  handler := TestResultHandler new.
  winner play: loser withResultHandler: handler.
  self assert: handler lastOutcome equals: expectedSymbol

"Use many times:"
StoneTest >> testStoneBeatsScissors
  self assertWeapon: Stone new beats: Scissors new outcome: #stoneCrushesScissors

StoneTest >> testStoneBeatsLizard
  self assertWeapon: Stone new beats: Lizard new outcome: #stoneCrushesLizard

"Much cleaner!"
```

**Meyer's Point**: "By capturing a pattern, a reusable software element will be applicable to many different developments."

You've captured the game pattern in `Weapon` superclass (reusable), but you can also capture the testing pattern!

---

### Exercise 4: Compatibility → Your Game

**In the Ruby Exercise**, we showed:
- **Bad**: Each component uses different data format (hash, array, custom object)
- **Good**: All components use standardized `DataRecord` interface

**In Your Pharo Game**:

**Current State** (11 methods in ResultHandler):
```smalltalk
ResultHandler >> handleDraw
ResultHandler >> paperCoversStone
ResultHandler >> stoneCrushesScissors
ResultHandler >> scissorsCutsPaper
ResultHandler >> lizardEatsPaper
ResultHandler >> spockSmashesScissors
ResultHandler >> paperDisprovesSpock
ResultHandler >> lizardPoisonsSpock
ResultHandler >> stoneCrushesLizard
ResultHandler >> scissorsDecapitatesLizard
ResultHandler >> spockVaporizesStone

"Adding Fire adds 10 more methods!"
```

**Problem**: Interface grows with O(n²) where n = number of weapons!

**Proposed Improvement** (Standardized GameOutcome):
```smalltalk
"Create a uniform data format:"
Object subclass: #GameOutcome
  instanceVariableNames: 'winner loser description outcomeType'

GameOutcome class >> winner: w loser: l description: d type: t
  ^ self new
    winner: w; loser: l; description: d; outcomeType: t; yourself

"Simplify ResultHandler to ONE method:"
ResultHandler >> handleOutcome: aGameOutcome
  self subclassResponsibility

TranscriptResultHandler >> handleOutcome: aGameOutcome
  Transcript show: aGameOutcome description; cr

ScoreKeepingHandler >> handleOutcome: aGameOutcome
  aGameOutcome isDraw
    ifTrue: [ draws := draws + 1 ]
    ifFalse: [ self recordWin: aGameOutcome winner ]

"Now weapons use uniform protocol:"
Stone >> playAgainstScissors: scissors withResultHandler: handler
  handler handleOutcome:
    (GameOutcome
      winner: self
      loser: scissors
      description: 'Stone crushes Scissors!'
      type: #stoneCrushesScissors)
```

**Benefits**:
- **Compatibility**: All handlers use same protocol
- **Extendibility**: Adding weapons doesn't add handler methods
- **Simplicity**: One method instead of 11 (or 21, or 31...)

**Meyer's Point**: "The key to compatibility lies in homogeneity of design, and in agreeing on standardized conventions for inter-program communication."

Your `ResultHandler` is like the Ruby `DataRecord` - it's a communication protocol. Making it uniform makes components more compatible!

---

## Side-by-Side Comparison

### Ruby Chapter 1 Exercise 2 (Shapes) vs. Your Pharo Game (Weapons)

**Both use the same pattern!**

| Aspect | Ruby Shapes | Pharo Weapons |
|--------|-------------|---------------|
| **Base Class** | `Shape` | `Weapon` |
| **Concrete Classes** | `Circle`, `Rectangle`, `Pentagon` | `Stone`, `Paper`, `Scissors` |
| **Polymorphism** | `area()`, `perimeter()` | `playAgainst...` methods |
| **Extensibility** | Add new shape = new class only | Add new weapon = new class + update others |
| **Challenge** | None (shapes don't interact) | Weapons interact (O(n²) problem) |

**Key Insight**: Your game is MORE complex than the shapes example because weapons interact with each other! This is why the table-driven approach becomes attractive.

---

## The O(n²) Problem in Your Design

This is the core challenge with your current design:

**With 5 weapons:**
- Each weapon needs 5 methods: `playAgainstStone:`, `playAgainstPaper:`, etc.
- Total: 5 weapons × 5 methods = **25 methods**
- ResultHandler needs **11 outcome methods** (10 wins + 1 draw)

**With 10 weapons:**
- Each weapon needs 10 methods
- Total: 10 weapons × 10 methods = **100 methods**
- ResultHandler needs **46 outcome methods** (45 wins + 1 draw)

**With 20 weapons:**
- Each weapon needs 20 methods
- Total: 20 weapons × 20 methods = **400 methods**
- ResultHandler needs **191 outcome methods**

**Solution**: Table-driven rules + GameOutcome reduces this to:
- Each weapon: 1 class definition only
- Rules table: n*(n-1)/2 entries (but all in one place)
- ResultHandler: **1 method** regardless of weapon count!

---

## What Meyer Would Say About Your Design

### Strengths (Already Good!)

✅ **"Classes as modules"** - Each weapon is a proper module
✅ **"Feature-based computation"** - Using message passing
✅ **"Polymorphism"** - Double dispatch is elegant polymorphism
✅ **"Separation of concerns"** - Game logic separate from display

### Areas for Improvement

⚠️ **Correctness**: Add contracts (assertions) to specify requirements
⚠️ **Robustness**: Validate inputs, handle edge cases
⚠️ **Extendibility**: Consider table-driven to reduce O(n²) coupling
⚠️ **Compatibility**: Standardize handler protocol with GameOutcome

### What He'd Love

Meyer would LOVE your `ResultHandler` strategy pattern! It's exactly the kind of "design for change" he advocates:

> "Support for change is a basic goal of object technology and a running theme through this book."

Your `TranscriptResultHandler` can be swapped for `ScoreKeepingHandler`, `HTMLHandler`, etc. without touching weapon code. Perfect!

---

## Recommended Reading Path

1. **Re-read Chapter 1 Section 1.2** ("A Review of External Factors") with your game in mind
2. **Jump ahead to Chapter 11** ("Design by Contract") to see how assertions work in Eiffel
3. **Check Chapter 23.1** ("Side Effects in Functions") - relevant to your `play:` method
4. **Read Chapter 24** ("Using Inheritance Well") when you get there - it discusses when inheritance is appropriate

---

## Action Items

Pick ONE to try this week:

### Option 1: Add Contracts (Correctness/Robustness)
- Add preconditions to `play:withResultHandler:`
- Add validation for weapon types
- Test with `nil` and invalid inputs
- **Estimated time**: 1 hour
- **Learning**: How contracts improve correctness

### Option 2: Introduce GameOutcome (Compatibility)
- Create `GameOutcome` class
- Refactor one weapon to use it
- Simplify `ResultHandler` to one method
- **Estimated time**: 2 hours
- **Learning**: How standardization improves compatibility

### Option 3: Extract Rules Table (Extendibility)
- Create `WeaponRules` class
- Define all rules declaratively
- Measure how easy it is to add "Fire" weapon
- **Estimated time**: 3 hours
- **Learning**: Trade-offs between explicitness and scalability

### Option 4: Compare Designs (Learning)
- Implement ALL THREE approaches in parallel
- Benchmark them
- Write up pros/cons of each
- **Estimated time**: 6 hours
- **Learning**: Deep understanding of design trade-offs

---

## Questions for Reflection

1. **Correctness**: How do you currently know your game rules are correct? Could assertions help?

2. **Robustness**: What happens if someone passes `nil` to `play:`? Should it crash or handle it?

3. **Extendibility**: If you had to add 10 new weapons, would you rather:
   - Add 10 methods to each existing weapon (double dispatch)
   - Add 45 rules to a table (table-driven)

4. **Reusability**: Are there patterns in your test code that could be extracted?

5. **Compatibility**: Would it be easier to add new `ResultHandler` types with 11 methods or 1 method?

---

## Connection to Future Chapters

Your game will be perfect for demonstrating:

- **Chapter 11**: Design by Contract - Add pre/postconditions
- **Chapter 14**: Inheritance - Is `Weapon` hierarchy appropriate?
- **Chapter 15**: Multiple Inheritance - Could weapons have multiple parents?
- **Chapter 23**: Class Design Principles - Should `play:` have side effects?
- **Chapter 24**: Using Inheritance Well - When to inherit vs. compose?

Keep experimenting! Your Pharo game is a perfect laboratory for testing these concepts.

---

*Want me to implement any of these refactorings in Smalltalk/Pharo code?*
