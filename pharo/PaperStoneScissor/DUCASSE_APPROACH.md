# Stéphane Ducasse's Approach: Simple "Tell, Don't Ask"

## The Key Insight

Ducasse's design is **much simpler** than the complex ResultHandler we discussed. Here's why it's brilliant:

**His approach:**
```smalltalk
Paper new vs: Paper new withResultHandler: ResultHandler new

Paper >> playAgainstStone: aStone withResultHandler: aResultHandler
    ^ aResultHandler paperWon
```

**Beautiful simplicity!**
- Weapon TELLS handler what happened
- Handler decides what to do
- No if statements
- No GameResult objects needed
- Pure "Tell, Don't Ask"

---

## Why This Is Better Than My Earlier Suggestions

### My Overcomplicated Version:
```smalltalk
"I suggested returning symbols, then checking them:"
result := stone play: scissors.
result = #draw ifTrue: [ ... ] ifFalse: [ ... ]

"OR creating GameResult objects with polymorphism"
```

### Ducasse's Simple Version:
```smalltalk
"Just tell the handler directly:"
Paper >> playAgainstStone: aStone withResultHandler: aResultHandler
    ^ aResultHandler paperWon
```

**No intermediary! No symbols! No GameResult objects!**

---

## Implementation: Ducasse's Way

### Step 1: Change the Weapon API

**Add `vs:withResultHandler:` method to Weapon:**

```smalltalk
"1. Add the main entry point to Weapon superclass"
Weapon compile: 'vs: aWeapon withResultHandler: aResultHandler
    "Play against another weapon using double dispatch, telling the handler what happens"
    ^ aWeapon playAgainst: self withResultHandler: aResultHandler'.
```

**What this does:**
- Replaces `play:` with clearer `vs:` naming
- Takes handler as parameter
- Delegates to double dispatch

---

### Step 2: Update playAgainst... Methods

Now each `playAgainst...` method directly tells the handler what happened:

```smalltalk
"2. Update Stone's methods"
Stone compile: 'playAgainstStone: aStone withResultHandler: aResultHandler
    ^ aResultHandler draw'.

Stone compile: 'playAgainstPaper: aPaper withResultHandler: aResultHandler
    ^ aResultHandler paperWon'.

Stone compile: 'playAgainstScissors: aScissors withResultHandler: aResultHandler
    ^ aResultHandler stoneWon'.

Stone compile: 'playAgainstLizard: aLizard withResultHandler: aResultHandler
    ^ aResultHandler stoneWon'.

Stone compile: 'playAgainstSpock: aSpock withResultHandler: aResultHandler
    ^ aResultHandler spockWon'.
```

**Notice:**
- Direct calls to handler methods
- No if statements!
- No checking results!
- Pure "Tell, Don't Ask"

---

### Step 3: Define ResultHandler Interface

```smalltalk
"3. Create abstract ResultHandler"
Object subclass: #ResultHandler
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'RockPaperScissors'.

ResultHandler class comment: 'I handle the outcome of Rock-Paper-Scissors games. My subclasses decide what to do when a weapon wins or the game is a draw.'.

"4. Define the interface"
ResultHandler compile: 'stoneWon
    self subclassResponsibility'.

ResultHandler compile: 'paperWon
    self subclassResponsibility'.

ResultHandler compile: 'scissorsWon
    self subclassResponsibility'.

ResultHandler compile: 'lizardWon
    self subclassResponsibility'.

ResultHandler compile: 'spockWon
    self subclassResponsibility'.

ResultHandler compile: 'draw
    self subclassResponsibility'.
```

**Key difference from my earlier suggestion:**
- Handler has 6 simple methods (one per weapon + draw)
- NOT 11 methods (stoneCrushesScissors, paperCoversStone, etc.)
- Simpler interface!

---

### Step 4: Create TranscriptResultHandler

```smalltalk
"5. Create concrete handler"
ResultHandler subclass: #TranscriptResultHandler
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'RockPaperScissors'.

"6. Implement the interface"
TranscriptResultHandler compile: 'stoneWon
    Transcript show: ''Stone wins!''; cr'.

TranscriptResultHandler compile: 'paperWon
    Transcript show: ''Paper wins!''; cr'.

TranscriptResultHandler compile: 'scissorsWon
    Transcript show: ''Scissors wins!''; cr'.

TranscriptResultHandler compile: 'lizardWon
    Transcript show: ''Lizard wins!''; cr'.

TranscriptResultHandler compile: 'spockWon
    Transcript show: ''Spock wins!''; cr'.

TranscriptResultHandler compile: 'draw
    Transcript show: ''It''''s a draw!''; cr'.
```

---

### Step 5: Test It!

```smalltalk
"7. Test in Playground"
| paper stone handler |
paper := Paper new.
stone := Stone new.
handler := TranscriptResultHandler new.

"Ducasse's clean API:"
paper vs: stone withResultHandler: handler.
"Prints: Paper wins!"

stone vs: stone withResultHandler: handler.
"Prints: It's a draw!"
```

---

## Complete Implementation for ALL Weapons

Here's the full code for all 5 weapons:

### Paper

```smalltalk
Paper compile: 'playAgainstPaper: aPaper withResultHandler: aResultHandler
    ^ aResultHandler draw'.

Paper compile: 'playAgainstStone: aStone withResultHandler: aResultHandler
    ^ aResultHandler paperWon'.

Paper compile: 'playAgainstScissors: aScissors withResultHandler: aResultHandler
    ^ aResultHandler scissorsWon'.

Paper compile: 'playAgainstLizard: aLizard withResultHandler: aResultHandler
    ^ aResultHandler lizardWon'.

Paper compile: 'playAgainstSpock: aSpock withResultHandler: aResultHandler
    ^ aResultHandler paperWon'.
```

### Scissors

```smalltalk
Scissors compile: 'playAgainstScissors: aScissors withResultHandler: aResultHandler
    ^ aResultHandler draw'.

Scissors compile: 'playAgainstStone: aStone withResultHandler: aResultHandler
    ^ aResultHandler stoneWon'.

Scissors compile: 'playAgainstPaper: aPaper withResultHandler: aResultHandler
    ^ aResultHandler scissorsWon'.

Scissors compile: 'playAgainstLizard: aLizard withResultHandler: aResultHandler
    ^ aResultHandler scissorsWon'.

Scissors compile: 'playAgainstSpock: aSpock withResultHandler: aResultHandler
    ^ aResultHandler spockWon'.
```

### Lizard

```smalltalk
Lizard compile: 'playAgainstLizard: aLizard withResultHandler: aResultHandler
    ^ aResultHandler draw'.

Lizard compile: 'playAgainstStone: aStone withResultHandler: aResultHandler
    ^ aResultHandler stoneWon'.

Lizard compile: 'playAgainstPaper: aPaper withResultHandler: aResultHandler
    ^ aResultHandler lizardWon'.

Lizard compile: 'playAgainstScissors: aScissors withResultHandler: aResultHandler
    ^ aResultHandler scissorsWon'.

Lizard compile: 'playAgainstSpock: aSpock withResultHandler: aResultHandler
    ^ aResultHandler lizardWon'.
```

### Spock

```smalltalk
Spock compile: 'playAgainstSpock: aSpock withResultHandler: aResultHandler
    ^ aResultHandler draw'.

Spock compile: 'playAgainstStone: aStone withResultHandler: aResultHandler
    ^ aResultHandler spockWon'.

Spock compile: 'playAgainstPaper: aPaper withResultHandler: aResultHandler
    ^ aResultHandler paperWon'.

Spock compile: 'playAgainstScissors: aScissors withResultHandler: aResultHandler
    ^ aResultHandler spockWon'.

Spock compile: 'playAgainstLizard: aLizard withResultHandler: aResultHandler
    ^ aResultHandler lizardWon'.
```

---

## Why Ducasse's Approach Is Brilliant

### 1. Simpler Than My Suggestions
**My overcomplicated approach:**
- Symbols → checking if statements → creating GameResult objects → polymorphism

**Ducasse's approach:**
- Directly tell the handler what happened!

### 2. Pure "Tell, Don't Ask"
```smalltalk
"NOT asking:"
if result = #paper then handler.handlePaperWin

"TELLING:"
aResultHandler paperWon
```

### 3. No Intermediate Objects Needed
- No symbols to return
- No GameResult objects
- No if statements anywhere!
- Direct method calls

### 4. Easy to Understand
```smalltalk
Paper >> playAgainstStone: aStone withResultHandler: aResultHandler
    ^ aResultHandler paperWon
```

**Reading this:** "When Paper plays against Stone, tell the result handler that paper won"

Crystal clear!

---

## Experimenting with Different Handlers

Now you can easily create different handlers:

### ScoreKeepingHandler

```smalltalk
ResultHandler subclass: #ScoreKeepingHandler
    instanceVariableNames: 'scores'
    classVariableNames: ''
    package: 'RockPaperScissors'.

ScoreKeepingHandler compile: 'initialize
    super initialize.
    scores := Dictionary new'.

ScoreKeepingHandler compile: 'stoneWon
    self incrementScore: #stone'.

ScoreKeepingHandler compile: 'paperWon
    self incrementScore: #paper'.

ScoreKeepingHandler compile: 'scissorsWon
    self incrementScore: #scissors'.

ScoreKeepingHandler compile: 'lizardWon
    self incrementScore: #lizard'.

ScoreKeepingHandler compile: 'spockWon
    self incrementScore: #spock'.

ScoreKeepingHandler compile: 'draw
    "Do nothing for draws"'.

ScoreKeepingHandler compile: 'incrementScore: weaponSymbol
    | current |
    current := scores at: weaponSymbol ifAbsent: [ 0 ].
    scores at: weaponSymbol put: current + 1'.

ScoreKeepingHandler compile: 'scoreFor: weaponSymbol
    ^ scores at: weaponSymbol ifAbsent: [ 0 ]'.
```

### CompositeHandler (Run Multiple Handlers)

```smalltalk
ResultHandler subclass: #CompositeHandler
    instanceVariableNames: 'handlers'
    classVariableNames: ''
    package: 'RockPaperScissors'.

CompositeHandler class compile: 'with: aCollectionOfHandlers
    ^ self new handlers: aCollectionOfHandlers; yourself'.

CompositeHandler compile: 'handlers: aCollection
    handlers := aCollection'.

"Delegate to all handlers"
CompositeHandler compile: 'stoneWon
    handlers do: [ :each | each stoneWon ]'.

CompositeHandler compile: 'paperWon
    handlers do: [ :each | each paperWon ]'.

"... etc for other methods"
```

**Usage:**
```smalltalk
| transcript scorer composite |
transcript := TranscriptResultHandler new.
scorer := ScoreKeepingHandler new.
composite := CompositeHandler with: { transcript. scorer }.

Paper new vs: Stone new withResultHandler: composite.
"Both prints AND tracks score!"
```

---

## Comparison: My Approach vs. Ducasse's

| Aspect | My GameResult Approach | Ducasse's Approach |
|--------|------------------------|-------------------|
| **Complexity** | ⭐⭐⭐ Complex | ⭐ Simple |
| **Objects Created** | 3 new classes | 1 new class |
| **If Statements** | ✓ None | ✓ None |
| **Tell, Don't Ask** | ✓ Yes | ✓ Yes |
| **Understanding** | Takes thought | Immediately clear |
| **Extensibility** | ⭐⭐⭐ Good | ⭐⭐⭐ Good |
| **Lines of Code** | More | Fewer |
| **Best For** | Academic study | Practical use |

**Verdict**: Ducasse's approach is **simpler and just as good**! 🎯

---

## Why I Overcomplicated It

I was thinking too academically about:
- "Replace conditionals with polymorphism" → led me to GameResult
- "Avoid symbol checking" → led me to intermediate objects

**But Ducasse saw the simpler solution:**
- Just tell the handler directly!
- No intermediaries needed!

**Lesson**: Sometimes the simple solution IS the object-oriented solution!

---

## Connection to OOSC2 Chapter 1

This demonstrates:

### ✅ Extendibility
Easy to add new handlers without touching weapons:
```smalltalk
ResultHandler subclass: #AnimationHandler
ResultHandler subclass: #SoundEffectHandler
ResultHandler subclass: #NetworkHandler
```

### ✅ Reusability
Handlers can be reused across different games:
```smalltalk
"Same ScoreKeepingHandler works for:"
- Rock-Paper-Scissors
- Rock-Paper-Scissors-Lizard-Spock
- Any turn-based game!
```

### ✅ Separation of Concerns
Game logic (weapons) completely separate from presentation (handlers)

### ✅ Simplicity
Meyer emphasizes: "Design simplicity: a simple architecture will always be easier to adapt"

**Ducasse's approach achieves ALL the benefits with LESS complexity!**

---

## What To Do Now

### Option 1: Implement Ducasse's Approach (Recommended)

Follow the steps above to:
1. Add `vs:withResultHandler:` to Weapon
2. Update all `playAgainst...` methods
3. Create ResultHandler and TranscriptResultHandler
4. Test it!
5. Experiment with ScoreKeepingHandler

**Time**: 45 minutes
**Learning**: Maximum value, minimum complexity

### Option 2: Keep Current Design

Your current design (returning symbols) is fine for learning double dispatch.

Add handlers later when you want to experiment with different behaviors.

### Option 3: Compare Both

Implement both and measure:
- Lines of code
- Ease of adding new weapon
- Ease of adding new behavior
- Which is easier to understand?

---

## Step-by-Step Implementation Checklist

Use this to guide your implementation:

```
Phase 1: Set up infrastructure
[ ] Create Weapon superclass (if you haven't already)
[ ] Add vs:withResultHandler: method to Weapon
[ ] Create ResultHandler abstract class
[ ] Define 6 abstract methods (stoneWon, paperWon, etc.)
[ ] Create TranscriptResultHandler
[ ] Implement 6 methods in TranscriptResultHandler

Phase 2: Update Stone (as example)
[ ] Add 5 playAgainst...withResultHandler: methods to Stone
[ ] Each method calls appropriate handler method
[ ] Test Stone vs other weapons

Phase 3: Update remaining weapons
[ ] Update Paper's 5 methods
[ ] Update Scissors's 5 methods
[ ] Update Lizard's 5 methods
[ ] Update Spock's 5 methods

Phase 4: Test thoroughly
[ ] Test all weapon combinations
[ ] Verify Transcript output
[ ] Test with old play: method (if kept for compatibility)

Phase 5: Experiment
[ ] Create ScoreKeepingHandler
[ ] Create CompositeHandler
[ ] Try creating your own handler
```

---

## Final Thoughts

**Ducasse's approach teaches:**
- Simple solutions are often the best
- "Tell, Don't Ask" doesn't require complex objects
- Direct method calls are clear and powerful
- Less is more

**Meyer would approve:**
> "Design simplicity: a simple architecture will always be easier to adapt to changes than a complex one."

This is **exactly** what Ducasse demonstrates! ✨

---

Want me to help you implement this step-by-step?
