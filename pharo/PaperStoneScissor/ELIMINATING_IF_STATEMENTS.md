# Eliminating If Statements: Making ResultHandler More OO

## The Problem: Asking Instead of Telling

**Current code** (with ResultHandler but still procedural):

```smalltalk
Weapon >> play: aWeapon withHandler: aHandler
    | resultSymbol |
    resultSymbol := aWeapon playAgainst: self.  "Returns #stone or #draw etc."

    "BAD: We're ASKING and then deciding!"
    resultSymbol = #draw
        ifTrue: [ aHandler handleDraw ]
        ifFalse: [ aHandler handleWinner: resultSymbol ]
```

**Why is this bad?**
- We're using `if` statements to inspect data
- We're making decisions based on the result's value
- Not polymorphic - the logic is in `Weapon`, not in the result itself
- Violates "Tell, Don't Ask" principle

**Meyer's perspective** (Chapter 23.1 in OOSC2):
> "Ask not what an object is, but what it can do"

---

## Solution 1: GameResult Object (Recommended!)

**Key insight**: Instead of returning a symbol, return an **object that knows how to notify handlers**!

### Step 1.1: Create GameResult hierarchy

```smalltalk
"1. Create abstract GameResult"
Object subclass: #GameResult
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'RockPaperScissors'.

GameResult class comment: 'I represent the outcome of a game. My subclasses know how to notify result handlers.'.

"2. Define abstract interface"
GameResult compile: 'notifyHandler: aHandler
    "Tell the handler about this result"
    self subclassResponsibility'.
```

### Step 1.2: Create concrete result classes

```smalltalk
"3. Create DrawResult"
GameResult subclass: #DrawResult
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'RockPaperScissors'.

DrawResult compile: 'notifyHandler: aHandler
    "I know I am a draw, so I tell the handler"
    aHandler handleDraw'.

"4. Create WinResult"
GameResult subclass: #WinResult
    instanceVariableNames: 'winnerSymbol'
    classVariableNames: ''
    package: 'RockPaperScissors'.

WinResult compile: 'winnerSymbol: aSymbol
    winnerSymbol := aSymbol'.

WinResult compile: 'notifyHandler: aHandler
    "I know who won, so I tell the handler"
    aHandler handleWinner: winnerSymbol'.

"5. Add factory methods for easy creation"
GameResult class compile: 'winner: aSymbol
    ^ WinResult new winnerSymbol: aSymbol; yourself'.

GameResult class compile: 'draw
    ^ DrawResult new'.
```

### Step 1.3: Refactor weapons to return GameResult

```smalltalk
"6. Change Stone's methods (example)"
Stone compile: 'playAgainstScissors
    "Return a result object, not a symbol"
    ^ GameResult winner: #stone'.

Stone compile: 'playAgainstPaper
    ^ GameResult winner: #paper'.

Stone compile: 'playAgainstStone
    ^ GameResult draw'.

"Do the same for all other playAgainst... methods in all weapons"
```

### Step 1.4: Simplify Weapon>>play:withHandler: (NO IF STATEMENTS!)

```smalltalk
"7. The beautiful part - NO IF STATEMENTS!"
Weapon compile: 'play: aWeapon withHandler: aHandler
    | result |
    result := aWeapon playAgainst: self.  "Returns a GameResult object"

    "TELL the result to notify the handler - pure OO!"
    result notifyHandler: aHandler'.

"8. For backward compatibility, add a method to get the symbol"
GameResult compile: 'asSymbol
    self subclassResponsibility'.

DrawResult compile: 'asSymbol
    ^ #draw'.

WinResult compile: 'asSymbol
    ^ winnerSymbol'.

"9. Keep old play: working"
Weapon compile: 'play: aWeapon
    | result |
    result := aWeapon playAgainst: self.
    ^ result asSymbol'.
```

### Step 1.5: Test it!

```smalltalk
"Test in Playground"
| stone scissors handler result |
stone := Stone new.
scissors := Scissors new.
handler := TranscriptResultHandler new.

"New way - pure OO, no if statements!"
result := scissors playAgainst: stone.  "Returns GameResult"
result notifyHandler: handler.  "Result tells handler what to do!"

"Or use the convenience method"
stone play: scissors withHandler: handler.

"Old way still works"
| oldStyleResult |
oldStyleResult := stone play: scissors.
Transcript show: 'Old style: ', oldStyleResult asString; cr.
```

---

## Why This Is Better (OO Principles)

### Before (Procedural):
```
Weapon asks result "are you a draw?"
  └─> if yes: tell handler it's a draw
  └─> if no: tell handler who won
```
- **Logic lives in Weapon** (wrong place!)
- **Uses if statements** (not polymorphic)
- **Violates Tell, Don't Ask**

### After (Object-Oriented):
```
Weapon tells result "notify the handler"
  └─> DrawResult tells handler "handleDraw"
  └─> WinResult tells handler "handleWinner: #stone"
```
- **Logic lives in Result objects** (right place!)
- **Uses polymorphism** (each result knows what to do)
- **Follows Tell, Don't Ask**

---

## Solution 2: More Specific ResultHandler Methods (Alternative)

Instead of eliminating if statements in `Weapon`, we could push them into the `ResultHandler` itself using **more specific methods**:

```smalltalk
"Change ResultHandler interface to be more specific"
ResultHandler compile: 'stoneCrushesScissors
    self subclassResponsibility'.

ResultHandler compile: 'paperCoversStone
    self subclassResponsibility'.

"... 9 more specific methods ..."

ResultHandler compile: 'handleDraw
    self subclassResponsibility'.

"Then weapons call specific methods"
Stone compile: 'playAgainstScissors: scissors withHandler: handler
    handler stoneCrushesScissors'.

Stone compile: 'playAgainstPaper: paper withHandler: handler
    handler paperCoversStone'.
```

**Pros:**
- Very explicit about what happened
- Handler can customize each outcome
- No if statements anywhere!

**Cons:**
- ResultHandler interface has 11 methods (1 draw + 10 wins)
- More work to implement each handler
- Grows with O(n²) as weapons increase

**When to use this:**
- When you want VERY detailed control over each outcome
- When different outcomes need different handling
- When you have few weapons (< 10)

---

## Solution 3: Strategy Pattern with Polymorphic Results

**Most advanced**: Combine GameResult with richer behavior

```smalltalk
"GameResult can carry more information"
GameResult subclass: #WinResult
    instanceVariableNames: 'winner loser description'

WinResult class >> winner: w loser: l description: desc
    ^ self new
        winner: w;
        loser: l;
        description: desc;
        yourself

"Weapons create rich results"
Stone compile: 'playAgainstScissors: scissors withHandler: handler
    | result |
    result := WinResult
        winner: self
        loser: scissors
        description: ''Stone crushes Scissors!''.
    result notifyHandler: handler'.

"Handler gets full context"
TranscriptResultHandler compile: 'handleWin: aWinResult
    Transcript show: aWinResult description; cr'.
```

---

## Comparison: Three Approaches

| Approach | If Statements? | OO-ness | Complexity | Extensibility |
|----------|---------------|---------|------------|---------------|
| **Current (symbols)** | ✗ Yes (in Weapon) | ⭐⭐ | ⭐ Simple | ⭐⭐ OK |
| **Solution 1 (GameResult)** | ✓ No! | ⭐⭐⭐⭐ | ⭐⭐ Medium | ⭐⭐⭐ Good |
| **Solution 2 (Specific methods)** | ✓ No | ⭐⭐⭐ | ⭐⭐⭐ Complex | ⭐⭐ OK |
| **Solution 3 (Rich results)** | ✓ No | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ Very complex | ⭐⭐⭐⭐ Excellent |

---

## Meyer's Take: "Replace Conditionals with Polymorphism"

From OOSC2, Meyer emphasizes:

> "The power of object-oriented programming lies in its ability to replace explicit case analysis (if-then-else, case statements) with polymorphism and dynamic binding."

**Your if statement is a code smell!** It's checking "what type of result is this?" - exactly what polymorphism is designed to eliminate.

### The OO Mindset Shift:

**Procedural thinking:**
```
Get the data → inspect it → decide what to do
```

**OO thinking:**
```
Get the object → tell it what you need → let it decide how
```

---

## Recommendation: Use Solution 1 (GameResult)

**Why?**
1. ✅ **Eliminates if statements** completely
2. ✅ **Clear separation of concerns**: Results know how to notify handlers
3. ✅ **Easy to extend**: Add new result types without touching weapons or handlers
4. ✅ **Testable**: Can test results independently
5. ✅ **Backward compatible**: Old `play:` still works

**Implementation time**: ~20 minutes

**Payoff**: Much more OO design that will scale better

---

## Step-by-Step Implementation of Solution 1

### Complete Code Sequence:

```smalltalk
"=== Part 1: Create GameResult hierarchy ==="

"1. Abstract superclass"
Object subclass: #GameResult
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'RockPaperScissors'.

"2. Abstract interface"
GameResult compile: 'notifyHandler: aHandler
    self subclassResponsibility'.

GameResult compile: 'asSymbol
    "For backward compatibility"
    self subclassResponsibility'.

"3. DrawResult"
GameResult subclass: #DrawResult
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'RockPaperScissors'.

DrawResult compile: 'notifyHandler: aHandler
    aHandler handleDraw'.

DrawResult compile: 'asSymbol
    ^ #draw'.

"4. WinResult"
GameResult subclass: #WinResult
    instanceVariableNames: 'winnerSymbol'
    classVariableNames: ''
    package: 'RockPaperScissors'.

WinResult compile: 'winnerSymbol: aSymbol
    winnerSymbol := aSymbol'.

WinResult compile: 'notifyHandler: aHandler
    aHandler handleWinner: winnerSymbol'.

WinResult compile: 'asSymbol
    ^ winnerSymbol'.

"5. Factory methods"
GameResult class compile: 'winner: aSymbol
    ^ WinResult new winnerSymbol: aSymbol; yourself'.

GameResult class compile: 'draw
    ^ DrawResult new'.

"=== Part 2: Update ONE weapon as example ==="

"6. Refactor Stone (then do others the same way)"
Stone compile: 'playAgainstScissors
    ^ GameResult winner: #stone'.

Stone compile: 'playAgainstLizard
    ^ GameResult winner: #stone'.

Stone compile: 'playAgainstPaper
    ^ GameResult winner: #paper'.

Stone compile: 'playAgainstSpock
    ^ GameResult winner: #spock'.

Stone compile: 'playAgainstStone
    ^ GameResult draw'.

"=== Part 3: Update Weapon superclass ==="

"7. New play:withHandler: - NO IF STATEMENTS!"
Weapon compile: 'play: aWeapon withHandler: aHandler
    | result |
    result := aWeapon playAgainst: self.
    result notifyHandler: aHandler'.

"8. Keep backward compatibility"
Weapon compile: 'play: aWeapon
    | result |
    result := aWeapon playAgainst: self.
    ^ result asSymbol'.

"=== Part 4: Test it! ==="

"9. Test"
| stone scissors handler |
stone := Stone new.
scissors := Scissors new.
handler := TranscriptResultHandler new.

"Test with handler"
stone play: scissors withHandler: handler.

"Test backward compatibility"
| result |
result := stone play: scissors.
Transcript show: 'Old style result: ', result asString; cr.
```

---

## After You Implement This...

### Notice How Clean It Is:

```smalltalk
"Before (procedural):"
play: aWeapon withHandler: aHandler
    | symbol |
    symbol := aWeapon playAgainst: self.
    symbol = #draw
        ifTrue: [ aHandler handleDraw ]
        ifFalse: [ aHandler handleWinner: symbol ]

"After (object-oriented):"
play: aWeapon withHandler: aHandler
    | result |
    result := aWeapon playAgainst: self.
    result notifyHandler: aHandler
```

**From 6 lines with if statements → 3 lines of pure OO!**

---

## Chapter 1 Connection: Correctness & Extendibility

**Correctness** improves because:
- Each result object has ONE clear responsibility
- No complex conditional logic to get wrong
- Type system ensures all results can notify handlers

**Extendibility** improves because:
- Want to add more result info? Add instance variables to WinResult
- Want a new result type (TieBreaker)? Create new GameResult subclass
- Want results to do something else? Add method to GameResult

**Meyer's quote:**
> "Polymorphism and dynamic binding are the technical keys to the flexibility and extendibility that characterize object-oriented software"

You're replacing a conditional with polymorphism - **this is the essence of OO design!**

---

Want me to help you implement this step by step?
