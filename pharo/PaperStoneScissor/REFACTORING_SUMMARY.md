# Rock-Paper-Scissors-Lizard-Spock: Refactoring Summary

## What We Did

We refactored a Rock-Paper-Scissors-Lizard-Spock game to demonstrate key Object-Oriented principles from Bertrand Meyer's OOSC2 Chapter 1.

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
- Client code had to interpret the returned symbol
- Game logic and result handling were coupled

---

## After: ResultHandler Pattern

```smalltalk
"Tell, Don't Ask - weapons notify handlers"
handler := TranscriptResultHandler new.
Stone new vs: Scissors new withResultHandler: handler.
"Handler receives #stoneWon message and prints: Stone wins!"
```

**Characteristics:**
- Weapons TELL handlers what happened (don't return data)
- Handlers decide what to do (print, score, animate, etc.)
- Game logic separated from result handling
- Easy to add new handlers without modifying weapons

---

## Key Design Patterns Applied

### 1. Double Dispatch
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

### 3. Tell, Don't Ask
```smalltalk
"OLD (Ask): Return data, client interprets it"
result := stone play: scissors.
result = #stone ifTrue: [ Transcript show: 'Stone wins!' ]

"NEW (Tell): Weapon tells handler what happened"
stone vs: scissors withResultHandler: handler.
"Handler already knows what to do!"
```

### 4. Template Method (Abstract Superclass)
```smalltalk
Weapon >> vs: aWeapon withResultHandler: aResultHandler
    "Shared algorithm in superclass"
    ^ aWeapon playAgainst: self withResultHandler: aResultHandler

Weapon >> playAgainstStone: aStone withResultHandler: aResultHandler
    "Subclasses must implement"
    self subclassResponsibility
```

---

## Class Structure

```
Weapon (abstract)
├── Stone
├── Paper
├── Scissors
├── Lizard
└── Spock

ResultHandler (abstract - to be created)
├── TranscriptResultHandler (prints results)
└── TestResultHandler (captures for testing)
```

---

## Methods Each Weapon Needs

### 1. Dispatcher Method (1 per weapon)
```smalltalk
Stone >> playAgainst: aWeapon withResultHandler: aResultHandler
    "Tell other weapon to play against Stone"
    ^ aWeapon playAgainstStone: self withResultHandler: aResultHandler
```

### 2. Specific Handler Methods (5 per weapon)
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

**Purpose:** Captures game outcomes for test assertions without I/O side effects.

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

## TDD Approach Used

1. **Write test first** (RED)
```smalltalk
testStoneVsScissors
    | stone scissors handler |
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

## What We Learned

### Smalltalk Syntax
- Method names include colons: `playAgainstStone:withResultHandler:`
- `self subclassResponsibility` marks abstract methods
- `compile:` dynamically adds methods
- `removeSelector:` removes methods

### OO Design
- Double dispatch eliminates conditionals
- Strategy pattern enables behavior swapping
- "Tell, Don't Ask" improves encapsulation
- Abstract classes define contracts

### TDD
- Write failing test first
- Implement just enough to pass
- Use test doubles (TestResultHandler) for isolation
- Debugger-driven development in Pharo

---

## Remaining Work

- [ ] Complete all weapons with full handler methods
- [ ] Create abstract `ResultHandler` superclass
- [ ] Create `TranscriptResultHandler` for output
- [ ] Remove old `play:` methods
- [ ] Update or remove old `RockPaperScissorsTest`

---

## Key Insight: Stéphane Ducasse's Approach

The cleanest implementation follows Ducasse's simple pattern:

```smalltalk
Paper >> playAgainstStone: aStone withResultHandler: aResultHandler
    ^ aResultHandler paperWon
```

**No if statements. No symbol checking. Just tell the handler what happened!**

This is pure "Tell, Don't Ask" - the essence of object-oriented design.

---

## Connection to OOSC2

This exercise demonstrates Meyer's core thesis from Chapter 1:

> "The object-oriented method is, before anything else, a system architecture method which helps designers produce systems whose structure remains both simple (even for large systems) and decentralized."

Our refactoring achieved:
- **Simplicity**: Each method does one thing
- **Decentralization**: Weapons and handlers are independent
- **Extendibility**: Add new handlers without touching weapons
- **Correctness**: Clear contracts, testable behavior
