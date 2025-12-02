# Rock-Paper-Scissors-Lizard-Spock: Refactoring Summary

## What We Accomplished

We refactored a Rock-Paper-Scissors-Lizard-Spock game to demonstrate key Object-Oriented principles from Bertrand Meyer's OOSC2 Chapter 1, using Stéphane Ducasse's "Tell, Don't Ask" approach.

---

## Before: Original Design

```smalltalk
"Simple double dispatch returning symbols"
Stone new play: Scissors new.  "→ #stone"
Paper new play: Stone new.     "→ #paper"
```

**Characteristics:**
- Each weapon had a `play:` method using double dispatch
- Methods returned symbols (`#stone`, `#paper`, `#draw`, etc.)
- Client code had to interpret the returned symbol with if statements
- Game logic and result handling were coupled

---

## After: Ducasse's ResultHandler Pattern

```smalltalk
"Tell, Don't Ask - weapons notify handlers directly"
handler := TestResultHandler new.
Stone new vs: Scissors new withResultHandler: handler.
"Handler receives #stoneWon message - NO if statements needed!"
```

**Characteristics:**
- Weapons TELL handlers what happened (don't return data)
- Handlers decide what to do (print, score, animate, etc.)
- Game logic separated from result handling
- Easy to add new handlers without modifying weapons

---

## The Key Insight: Pure "Tell, Don't Ask"

Ducasse's brilliant simplicity:

```smalltalk
Paper >> playAgainstStone: aStone withResultHandler: aResultHandler
    ^ aResultHandler paperWon
```

**No if statements. No symbol checking. Just tell the handler what happened!**

---

## Design Patterns Applied

### 1. Double Dispatch (Resolves game without conditionals)
```smalltalk
"Step 1: Stone asks Scissors to play against it"
Stone >> vs: aWeapon withResultHandler: aResultHandler
    ^ aWeapon playAgainst: self withResultHandler: aResultHandler

"Step 2: Scissors knows it's playing Stone, dispatches to specific method"
Scissors >> playAgainst: aWeapon withResultHandler: aResultHandler
    ^ aWeapon playAgainstScissors: self withResultHandler: aResultHandler

"Step 3: Stone knows it's against Scissors, tells handler the result"
Stone >> playAgainstScissors: aScissors withResultHandler: aResultHandler
    ^ aResultHandler stoneWon
```

### 2. Strategy Pattern (ResultHandler)
```smalltalk
"Different handlers for different purposes:"
TranscriptResultHandler  "→ prints to Transcript"
TestResultHandler        "→ captures result for testing"
ScoreKeepingHandler      "→ tracks wins/losses"
```

### 3. Template Method (Abstract Superclass)
```smalltalk
Weapon >> vs: aWeapon withResultHandler: aResultHandler
    "Shared algorithm in superclass"
    ^ aWeapon playAgainst: self withResultHandler: aResultHandler

Weapon >> playAgainstStone: aStone withResultHandler: aResultHandler
    "Subclasses must implement"
    self subclassResponsibility
```

---

## Final Class Structure

```
Weapon (abstract)
├── Stone
├── Paper
├── Scissors
├── Lizard
└── Spock

ResultHandler (abstract)
├── TranscriptResultHandler (prints results)
└── TestResultHandler (captures for testing)
```

---

## Methods Each Weapon Implements

### Entry Point (inherited from Weapon)
```smalltalk
vs: aWeapon withResultHandler: aResultHandler
    ^ aWeapon playAgainst: self withResultHandler: aResultHandler
```

### Dispatcher (1 per weapon)
```smalltalk
Stone >> playAgainst: aWeapon withResultHandler: aResultHandler
    ^ aWeapon playAgainstStone: self withResultHandler: aResultHandler
```

### Specific Handlers (5 per weapon)
```smalltalk
Stone >> playAgainstStone: aStone withResultHandler: aResultHandler
    ^ aResultHandler draw

Stone >> playAgainstPaper: aPaper withResultHandler: aResultHandler
    ^ aResultHandler paperWon

Stone >> playAgainstScissors: aScissors withResultHandler: aResultHandler
    ^ aResultHandler stoneWon

Stone >> playAgainstLizard: aLizard withResultHandler: aResultHandler
    ^ aResultHandler stoneWon

Stone >> playAgainstSpock: aSpock withResultHandler: aResultHandler
    ^ aResultHandler spockWon
```

---

## ResultHandler Interface

```smalltalk
ResultHandler >> stoneWon
    self subclassResponsibility

ResultHandler >> paperWon
    self subclassResponsibility

ResultHandler >> scissorsWon
    self subclassResponsibility

ResultHandler >> lizardWon
    self subclassResponsibility

ResultHandler >> spockWon
    self subclassResponsibility

ResultHandler >> draw
    self subclassResponsibility
```

---

## TestResultHandler (Test Double)

```smalltalk
"Captures game outcomes for test assertions"
TestResultHandler >> stoneWon
    wasDraw := false.
    lastWinner := #stone

TestResultHandler >> draw
    wasDraw := true.
    lastWinner := nil

TestResultHandler >> lastWinner
    ^ lastWinner

TestResultHandler >> wasDraw
    ^ wasDraw
```

---

## TDD Approach Used

1. **Write test first** (RED)
```smalltalk
testStoneVsScissors
    | handler |
    handler := TestResultHandler new.
    Stone new vs: Scissors new withResultHandler: handler.
    self assert: handler lastWinner equals: #stone
```

2. **Run test** - it fails

3. **Implement minimum code** (GREEN)
```smalltalk
Stone >> playAgainstScissors: aScissors withResultHandler: aResultHandler
    ^ aResultHandler stoneWon
```

4. **Run test** - it passes

5. **Refactor** if needed

6. **Repeat** for next feature

---

## OOSC2 Chapter 1 Principles Demonstrated

| Principle | How We Applied It |
|-----------|-------------------|
| **Correctness** | Clear contracts with `subclassResponsibility` |
| **Extendibility** | Easy to add new handlers without modifying weapons |
| **Reusability** | `vs:withResultHandler:` defined once in Weapon superclass |
| **Separation of Concerns** | Game logic (weapons) separate from presentation (handlers) |
| **Compatibility** | Standardized ResultHandler protocol |

---

## What We Learned

### Smalltalk Syntax
- Method names include colons: `playAgainstStone:withResultHandler:`
- `self subclassResponsibility` marks abstract methods
- `compile:` dynamically adds methods
- Debugger-driven development in Pharo

### OO Design
- Double dispatch eliminates conditionals
- Strategy pattern enables behavior swapping
- "Tell, Don't Ask" improves encapsulation
- Abstract classes define contracts

### TDD
- Write failing test first
- Implement just enough to pass
- Use test doubles (TestResultHandler) for isolation

---

## Connection to OOSC2

This exercise demonstrates Meyer's core thesis from Chapter 1:

> "The object-oriented method is, before anything else, a system architecture method which helps designers produce systems whose structure remains both simple (even for large systems) and decentralized."

Our refactoring achieved:
- **Simplicity**: Each method does one thing
- **Decentralization**: Weapons and handlers are independent
- **Extendibility**: Add new handlers without touching weapons
- **Correctness**: Clear contracts, testable behavior

---

## Remaining Work

- [ ] Complete all weapons with full handler methods
- [ ] Create abstract `ResultHandler` superclass
- [ ] Create `TranscriptResultHandler` for output
- [ ] Remove old `play:` methods
- [ ] Update or remove old `RockPaperScissorsTest`

---

## Original State (Before Refactoring)

```smalltalk
"5 weapon classes, each with:"
- play: aWeapon (double dispatch entry point)
- playAgainstStone (returns #stone, #draw, etc.)
- playAgainstPaper (returns #paper, #draw, etc.)
- playAgainstScissors (returns #scissors, #draw, etc.)
- playAgainstLizard (returns #lizard, #draw, etc.)
- playAgainstSpock (returns #spock, #draw, etc.)

"Example usage:"
| stone scissors result |
stone := Stone new.
scissors := Scissors new.
result := stone play: scissors.  "returns #stone"

result = #stone ifTrue: [ Transcript show: 'Stone wins!' ]
result = #scissors ifTrue: [ Transcript show: 'Scissors wins!' ]
result = #draw ifTrue: [ Transcript show: 'Draw!' ]
```

**What you have**: Clean double dispatch returning symbols ✅

**Total code**:
- 5 classes
- 30 methods (5 weapons × 6 methods each)

---

## Refactoring Phase 1: Create Weapon Superclass

**Goal**: Eliminate duplication and establish common interface

**Chapter 1 Connection**: This improves **extendibility** through better structure

### Step 1.1: Create the abstract Weapon class

Open a **Playground** and execute:

```smalltalk
"1. Create the abstract superclass"
Object subclass: #Weapon
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'RockPaperScissors'.

"2. Add a comment explaining its role"
Weapon class comment: 'I am the abstract superclass for all weapons in Rock-Paper-Scissors-Lizard-Spock. My subclasses implement the game rules using double dispatch.'.
```

**What this does**: Creates a new class to be the parent of all weapons

**Verify**: Open System Browser, you should see `Weapon` in the `RockPaperScissors` package

---

### Step 1.2: Move the common play: method to Weapon

```smalltalk
"3. Add the play: method to Weapon (it's identical in all subclasses)"
Weapon compile: 'play: aWeapon
    "Use double dispatch to determine the winner.
    Each weapon will call back with playAgainst..."
    ^ aWeapon playAgainst: self'.
```

**What this does**: Defines `play:` once instead of 5 times

**Verify**: In System Browser, click on `Weapon` class, you should see the `play:` method

---

### Step 1.3: Define the abstract interface

```smalltalk
"4. Define abstract methods that subclasses must implement"
Weapon compile: 'playAgainstStone
    "Subclasses must implement this"
    self subclassResponsibility'.

Weapon compile: 'playAgainstPaper
    self subclassResponsibility'.

Weapon compile: 'playAgainstScissors
    self subclassResponsibility'.

Weapon compile: 'playAgainstLizard
    self subclassResponsibility'.

Weapon compile: 'playAgainstSpock
    self subclassResponsibility'.
```

**What this does**: Documents what methods subclasses MUST implement

**Verify**: `Weapon` now has 6 methods (1 real + 5 abstract)

---

### Step 1.4: Make Stone, Paper, etc. inherit from Weapon

```smalltalk
"5. Change the superclass of Stone"
Weapon subclass: #Stone
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'RockPaperScissors'.

"6. Do the same for the other weapons"
Weapon subclass: #Paper
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'RockPaperScissors'.

Weapon subclass: #Scissors
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'RockPaperScissors'.

Weapon subclass: #Lizard
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'RockPaperScissors'.

Weapon subclass: #Spock
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'RockPaperScissors'.
```

**What this does**: Changes class hierarchy from:
```
Object
  ├── Stone
  ├── Paper
  ├── Scissors
  ├── Lizard
  └── Spock
```

To:
```
Object
  └── Weapon
      ├── Stone
      ├── Paper
      ├── Scissors
      ├── Lizard
      └── Spock
```

**Verify**: In System Browser, click on `Stone` → should show `Weapon` as superclass

---

### Step 1.5: Remove duplicate play: methods from subclasses

```smalltalk
"7. Remove the play: method from Stone (now inherited from Weapon)"
Stone removeSelector: #play:.

"8. Do the same for all other weapons"
Paper removeSelector: #play:.
Scissors removeSelector: #play:.
Lizard removeSelector: #play:.
Spock removeSelector: #play:.
```

**What this does**: Eliminates duplication - now `play:` defined once in `Weapon`

**Verify**:
- Click on `Stone` → should see 6 methods (no more `play:`)
- Click on `Weapon` → should see the `play:` method

---

### Step 1.6: Test that everything still works

```smalltalk
"9. Test in Playground"
| stone scissors result |
stone := Stone new.
scissors := Scissors new.
result := stone play: scissors.

Transcript show: 'Result: ', result asString; cr.
self assert: result = #stone.

"Test a draw"
| stone1 stone2 result |
stone1 := Stone new.
stone2 := Stone new.
result := stone1 play: stone2.

self assert: result = #draw.

Transcript show: 'Phase 1 complete! All tests pass.'; cr.
```

**Expected output**:
```
Result: #stone
Phase 1 complete! All tests pass.
```

---

## ✅ Checkpoint: What We've Achieved

**Before**:
- 5 weapon classes, 30 methods, code duplicated

**After**:
- 1 abstract `Weapon` class + 5 concrete subclasses, 30 methods (1 shared + 29 specific)
- Clear inheritance hierarchy
- Better structure for future changes

**Chapter 1 Quality Factors Improved**:
- ✅ **Extendibility**: Clearer structure
- ✅ **Reusability**: Common code in one place
- ✅ **Maintainability**: Fix `play:` bug once, not 5 times

**Line Count**: Same (but better organized!)

---

## Refactoring Phase 2: Add ResultHandler

**Goal**: Separate "what happened" from "what to do about it"

**Chapter 1 Connection**: This improves **extendibility** (easy to add new behaviors) and **compatibility** (standardized protocol)

### Step 2.1: Create ResultHandler abstract class

```smalltalk
"1. Create the abstract handler"
Object subclass: #ResultHandler
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'RockPaperScissors'.

ResultHandler class comment: 'I am responsible for handling the outcome of a Rock-Paper-Scissors game. My subclasses decide what to do when a game is won, lost, or drawn.'.
```

**What this does**: Creates a new abstraction for "handling results"

---

### Step 2.2: Define the handler interface

Currently, your weapons return 6 possible symbols:
- `#stone`, `#paper`, `#scissors`, `#lizard`, `#spock`
- `#draw`

We need to design a good interface. Let's use a simple one that mirrors your symbols:

```smalltalk
"2. Define abstract methods for each outcome"
ResultHandler compile: 'handleDraw
    "Called when the game is a draw"
    self subclassResponsibility'.

ResultHandler compile: 'handleWinner: winnerSymbol
    "Called when there is a winner. winnerSymbol is #stone, #paper, etc."
    self subclassResponsibility'.
```

**What this does**: Defines what handlers must be able to do

**Alternative design** (more explicit, used in your original description):

```smalltalk
"Alternative: Have a method for each specific outcome"
ResultHandler compile: 'stoneCrushesScissors
    self subclassResponsibility'.

ResultHandler compile: 'stoneCrushesLizard
    self subclassResponsibility'.

ResultHandler compile: 'paperCoversStone
    self subclassResponsibility'.

"... and 7 more methods for all outcomes"
```

**Decision Point**: Which interface do you prefer?

**Option A (Simple)**: 2 methods (`handleDraw`, `handleWinner:`)
- ✅ Simple
- ✅ Easy to implement handlers
- ➖ Less descriptive messages

**Option B (Explicit)**: 11 methods (one per outcome)
- ✅ Very descriptive
- ✅ Handler can customize each outcome
- ➖ More methods to implement

**Recommendation**: Start with Option A (simple), we can always refine later.

---

### Step 2.3: Create TranscriptResultHandler

```smalltalk
"3. Create concrete handler"
ResultHandler subclass: #TranscriptResultHandler
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'RockPaperScissors'.

TranscriptResultHandler class comment: 'I print game results to the Transcript window.'.

"4. Implement the interface"
TranscriptResultHandler compile: 'handleDraw
    Transcript show: ''It is a draw!''; cr'.

TranscriptResultHandler compile: 'handleWinner: winnerSymbol
    Transcript show: winnerSymbol asString capitalized, '' wins!''; cr'.
```

**What this does**: Creates a handler that prints results

**Verify**: Create instance and test:
```smalltalk
| handler |
handler := TranscriptResultHandler new.
handler handleWinner: #stone.   "Should print: Stone wins!"
handler handleDraw.              "Should print: It is a draw!"
```

---

### Step 2.4: Refactor Weapon to use ResultHandler

Now we need to change weapons to NOTIFY a handler instead of RETURNING a symbol.

**Current**:
```smalltalk
Stone >> play: aWeapon
    ^ aWeapon playAgainstStone

Stone >> playAgainstScissors
    ^ #stone
```

**New** (Option 1 - with handler parameter):
```smalltalk
Weapon >> play: aWeapon withHandler: aHandler
    "Play against another weapon and notify handler of result"
    | resultSymbol |
    resultSymbol := aWeapon playAgainst: self.

    resultSymbol = #draw
        ifTrue: [ aHandler handleDraw ]
        ifFalse: [ aHandler handleWinner: resultSymbol ]

"Keep old play: method for backward compatibility"
Weapon >> play: aWeapon
    "Convenience method that returns symbol (backward compatible)"
    ^ aWeapon playAgainst: self
```

**What this does**:
- Keeps old `play:` working (returns symbol)
- Adds new `play:withHandler:` that notifies handler

**Implement**:
```smalltalk
"5. Add play:withHandler: to Weapon superclass"
Weapon compile: 'play: aWeapon withHandler: aHandler
    "Play and notify handler of outcome"
    | resultSymbol |
    resultSymbol := aWeapon playAgainst: self.

    resultSymbol = #draw
        ifTrue: [ aHandler handleDraw ]
        ifFalse: [ aHandler handleWinner: resultSymbol ]'.
```

---

### Step 2.5: Test the new interface

```smalltalk
"6. Test in Playground"
| stone scissors handler |
stone := Stone new.
scissors := Scissors new.
handler := TranscriptResultHandler new.

"New style with handler"
stone play: scissors withHandler: handler.
"Should print: Stone wins!"

"Old style still works"
| result |
result := stone play: scissors.
Transcript show: 'Old style result: ', result asString; cr.
"Should print: Old style result: #stone"

Transcript show: 'Phase 2 complete! Handler pattern working.'; cr.
```

---

### Step 2.6: Create additional handlers (optional)

Let's create a **ScoreKeepingHandler** to demonstrate the power of this pattern:

```smalltalk
"7. Create a score keeper"
ResultHandler subclass: #ScoreKeepingHandler
    instanceVariableNames: 'scores'
    classVariableNames: ''
    package: 'RockPaperScissors'.

"8. Initialize scores dictionary"
ScoreKeepingHandler compile: 'initialize
    super initialize.
    scores := Dictionary new'.

"9. Implement handler methods"
ScoreKeepingHandler compile: 'handleDraw
    "Do nothing for draws"'.

ScoreKeepingHandler compile: 'handleWinner: winnerSymbol
    | currentScore |
    currentScore := scores at: winnerSymbol ifAbsent: [ 0 ].
    scores at: winnerSymbol put: currentScore + 1'.

"10. Add accessor for querying scores"
ScoreKeepingHandler compile: 'scoreFor: weaponSymbol
    ^ scores at: weaponSymbol ifAbsent: [ 0 ]'.

ScoreKeepingHandler compile: 'scores
    ^ scores copy'.
```

---

### Step 2.7: Test multiple handlers

```smalltalk
"11. Run a tournament"
| weapons handler |
weapons := { Stone new. Paper new. Scissors new. Lizard new. Spock new }.
handler := ScoreKeepingHandler new.

"Play every weapon against every other"
weapons do: [ :weapon1 |
    weapons do: [ :weapon2 |
        weapon1 play: weapon2 withHandler: handler ] ].

"Display results"
#(stone paper scissors lizard spock) do: [ :name |
    Transcript show: name asString capitalized, ': ',
        (handler scoreFor: name) asString, ' wins'; cr ].

Transcript show: 'Phase 2 complete! Tournament results above.'; cr.
```

**Expected output** (approximately):
```
Stone: 4 wins
Paper: 4 wins
Scissors: 4 wins
Lizard: 4 wins
Spock: 4 wins
Phase 2 complete! Tournament results above.
```

(Each weapon wins against 2 others, plays 4 other opponents = 8 games, wins 4)

---

## ✅ Final Checkpoint: What We've Achieved

**Before Phase 1**:
- 5 duplicate weapon classes
- No abstraction
- Hard to modify common behavior

**After Phase 1**:
- Clean inheritance hierarchy with `Weapon` superclass
- Common behavior in one place
- Clear interface definition

**After Phase 2**:
- Separation of concerns (game logic vs. result handling)
- Easy to add new result handlers without touching weapons
- Can switch between `TranscriptResultHandler`, `ScoreKeepingHandler`, etc.
- Backward compatible (old `play:` still works)

**Chapter 1 Quality Factors Achieved**:

| Factor | How We Achieved It |
|--------|-------------------|
| **Correctness** | Clear interface with `subclassResponsibility` |
| **Extendibility** | Easy to add new weapons (inherit from `Weapon`) |
| **Extendibility** | Easy to add new handlers (inherit from `ResultHandler`) |
| **Reusability** | `play:` logic shared across all weapons |
| **Compatibility** | Standardized `ResultHandler` protocol |
| **Separation of Concerns** | Weapons don't know about Transcript or scoring |

---

## Meyer's Analysis of Your Design

### What Meyer Would Love ❤️

1. **"Classes as modules"** - Each weapon is a clean module ✅
2. **"Polymorphism"** - Double dispatch is elegant ✅
3. **"Design for change"** - Handlers allow behavior changes without modifying weapons ✅
4. **"Separation of concerns"** - Game rules separate from presentation ✅

### What Meyer Would Suggest 💡

1. **Add contracts** (Chapter 11):
   ```smalltalk
   Weapon >> play: aWeapon withHandler: aHandler
       self assert: aWeapon isNotNil description: 'Weapon required'.
       self assert: (aWeapon isKindOf: Weapon) description: 'Must be a Weapon'.
       "... rest of method"
   ```

2. **Consider table-driven rules** for better scalability (discussed in his methodology chapters)

3. **Add tests** to verify correctness systematically

---

## Next Experiments to Try

### Experiment A: Add Validation (Correctness)
Add preconditions to catch bugs early

### Experiment B: Add More Handlers (Reusability)
- `HTMLResultHandler` - generates HTML
- `AnimationResultHandler` - triggers animations
- `StatisticsHandler` - tracks detailed stats

### Experiment C: Add a New Weapon (Extendibility Test)
Add "Fire" weapon and see how easy/hard it is:
- How many classes to modify?
- How many methods to add?
- How long does it take?

### Experiment D: Compare with Table-Driven (Alternative Design)
Implement rules in a dictionary and compare

---

## Files in Your Repository

```
katas/pharo/PaperStoneScissor/
├── STEP_BY_STEP_REFACTORING.md      # This file!
├── REFACTORING_PLAN.md               # Strategic overview
├── CHAPTER_1_CONNECTIONS.md          # Links to OOSC2 concepts
└── RockPaperScissors.st             # Your current code
```

---

## Summary: Two Phases Done!

**Phase 1: Weapon Superclass**
- Created `Weapon` abstract class
- Moved common `play:` method up
- Defined abstract interface
- Made all weapons inherit from `Weapon`

**Phase 2: ResultHandler Pattern**
- Created `ResultHandler` abstraction
- Implemented `TranscriptResultHandler`
- Added `play:withHandler:` method
- Kept backward compatibility
- Demonstrated with `ScoreKeepingHandler`

**Total Time**: ~30-45 minutes
**Total New Code**: ~50 lines
**Total Benefit**: Much better architecture for future changes!

---

Want me to help with the next step, or would you like to try these refactorings yourself first?
