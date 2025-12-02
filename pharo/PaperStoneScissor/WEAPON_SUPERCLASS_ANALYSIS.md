# Should You Create a Weapon Superclass? Analysis

## The Three Proposed Steps

1. Create abstract `Weapon` class
2. Move common `play:` method to superclass
3. Define abstract interface with `subclassResponsibility`

## Quick Answer: YES, Definitely Worth It! ✅

**Why?** Because you have **duplicated code** right now.

---

## Current State: Code Duplication

**You currently have this in 5 different classes:**

```smalltalk
"In Stone class:"
Stone >> play: aWeapon
    ^ aWeapon playAgainstStone

"In Paper class:"
Paper >> play: aWeapon
    ^ aWeapon playAgainstPaper

"In Scissors class:"
Scissors >> play: aWeapon
    ^ aWeapon playAgainstScissors

"In Lizard class:"
Lizard >> play: aWeapon
    ^ aWeapon playAgainstLizard

"In Spock class:"
Spock >> play: aWeapon
    ^ aWeapon playAgainstSpock
```

**5 nearly identical methods!** Only difference is the method name being called.

---

## What Ducasse Would Say

### 1. "Remove Duplication" 👍

Ducasse is a strong advocate for eliminating duplication. This is a code smell.

**From Pharo by Example:**
> "Don't repeat yourself. If you find yourself writing the same code twice, extract it into a method or class."

**Verdict**: He would **definitely** recommend the Weapon superclass.

---

### 2. "Use Inheritance for Common Behavior" 👍

The `play:` method (or `vs:withResultHandler:`) is **common behavior** shared by all weapons.

**Ducasse's principle:**
> "Inheritance should capture common behavior and interface"

**Verdict**: Perfect use case for a superclass.

---

### 3. "Make Interfaces Explicit" 👍

The `subclassResponsibility` pattern makes the contract explicit:

```smalltalk
Weapon >> playAgainstStone: aStone withResultHandler: aResultHandler
    self subclassResponsibility
```

**This documents:**
- What methods subclasses MUST implement
- What the protocol/interface is
- Fails fast if you forget to implement

**Ducasse's approach in Pharo:**
> "Use subclassResponsibility to make abstract interfaces clear"

**Verdict**: He would **strongly recommend** this.

---

## Detailed Analysis of Each Step

### Step 1: Create Abstract Weapon Class

**Benefits:**
- ✅ Documents that Stone, Paper, etc. are all the same "kind of thing"
- ✅ Provides a place for shared behavior
- ✅ Enables polymorphism (can treat all weapons uniformly)
- ✅ Makes the design intent clear

**Costs:**
- ➖ One more class to understand (minimal)
- ➖ Extra indirection (minimal)

**Ducasse's verdict**: **DO IT** - This is good OO design.

---

### Step 2: Move play: Method to Superclass

**Current duplication:**
```smalltalk
"Stone:"
play: aWeapon → aWeapon playAgainstStone

"Paper:"
play: aWeapon → aWeapon playAgainstPaper

"Scissors:"
play: aWeapon → aWeapon playAgainstScissors
```

**Wait!** These are NOT identical - they call different methods!

**Can we eliminate this duplication?**

#### Option A: Use #perform: (Dynamic dispatch)

```smalltalk
Weapon >> play: aWeapon
    | selector |
    selector := ('playAgainst', self class name) asSymbol.
    ^ aWeapon perform: selector with: self
```

**Pros:**
- ✅ No duplication
- ✅ One method instead of 5

**Cons:**
- ➖ Uses string manipulation
- ➖ Loses type safety
- ➖ Harder to understand
- ➖ Error-prone

**Ducasse's verdict**: **NO** - Too clever, loses clarity.

#### Option B: Keep vs:withResultHandler: in Subclasses

```smalltalk
"Each weapon still has its own method:"
Stone >> vs: aWeapon withResultHandler: aHandler
    ^ aWeapon playAgainst: self withResultHandler: aHandler

Paper >> vs: aWeapon withResultHandler: aHandler
    ^ aWeapon playAgainst: self withResultHandler: aHandler
```

**Wait, these ARE identical now!** With the handler approach:

```smalltalk
"This is the same for all weapons:"
Weapon >> vs: aWeapon withResultHandler: aHandler
    ^ aWeapon playAgainst: self withResultHandler: aHandler
```

**Because `playAgainst:` uses double dispatch, `self` is the right weapon!**

**Ducasse's verdict**: **YES** - This duplication should be removed!

---

### Step 3: Define Abstract Interface

**Make the contract explicit:**

```smalltalk
"In Weapon superclass:"
Weapon >> playAgainstStone: aStone withResultHandler: aHandler
    self subclassResponsibility

Weapon >> playAgainstPaper: aPaper withResultHandler: aHandler
    self subclassResponsibility

Weapon >> playAgainstScissors: aScissors withResultHandler: aHandler
    self subclassResponsibility

Weapon >> playAgainstLizard: aLizard withResultHandler: aHandler
    self subclassResponsibility

Weapon >> playAgainstSpock: aSpock withResultHandler: aHandler
    self subclassResponsibility
```

**Benefits:**
- ✅ **Documents the interface** - anyone reading Weapon knows what to implement
- ✅ **Fails fast** - if you forget a method, you get a clear error
- ✅ **IDE support** - tools can show you what's required
- ✅ **Testable** - can verify all subclasses implement all methods

**Example of failing fast:**
```smalltalk
"If you forget to implement playAgainstSpock: in Stone:"
Stone new vs: Spock new withResultHandler: TranscriptResultHandler new.

"You get a clear error:"
"MessageNotUnderstood: Stone>>playAgainstSpock:withResultHandler:
Should have been implemented by subclass!"
```

**Ducasse's verdict**: **ABSOLUTELY DO THIS** - Best practice in Pharo!

---

## What Would the Final Design Look Like?

### With Weapon Superclass (Recommended)

```smalltalk
"Abstract superclass"
Object subclass: #Weapon

"One shared method"
Weapon >> vs: aWeapon withResultHandler: aHandler
    ^ aWeapon playAgainst: self withResultHandler: aHandler

"5 abstract methods (interface definition)"
Weapon >> playAgainstStone: aStone withResultHandler: aHandler
    self subclassResponsibility

Weapon >> playAgainstPaper: aPaper withResultHandler: aHandler
    self subclassResponsibility
"... 3 more"

"Concrete subclasses"
Weapon subclass: #Stone
Weapon subclass: #Paper
Weapon subclass: #Scissors
Weapon subclass: #Lizard
Weapon subclass: #Spock

"Each implements 5 methods"
Stone >> playAgainstScissors: aScissors withResultHandler: aHandler
    ^ aHandler stoneWon

Stone >> playAgainstPaper: aPaper withResultHandler: aHandler
    ^ aHandler paperWon
"... 3 more"
```

**Total code:**
- 1 abstract class: 6 methods (1 real + 5 abstract)
- 5 concrete classes: 5 methods each (25 total)
- **Net result**: Clear structure, no duplication!

---

## Comparison: With vs Without Superclass

### Without Weapon Superclass (Current)

**Structure:**
```
Object
  ├── Stone (6 methods)
  ├── Paper (6 methods)
  ├── Scissors (6 methods)
  ├── Lizard (6 methods)
  └── Spock (6 methods)
```

**Problems:**
- ❌ `vs:withResultHandler:` duplicated 5 times
- ❌ No clear documentation of required interface
- ❌ If you forget to implement a method, silent failure or confusing error
- ❌ No way to treat all weapons polymorphically (can't do `Weapon allSubclasses`)

### With Weapon Superclass (Recommended)

**Structure:**
```
Object
  └── Weapon (1 real method + 5 abstract)
      ├── Stone (5 methods)
      ├── Paper (5 methods)
      ├── Scissors (5 methods)
      ├── Lizard (5 methods)
      └── Spock (5 methods)
```

**Benefits:**
- ✅ `vs:withResultHandler:` defined once
- ✅ Interface clearly documented
- ✅ Fails fast with clear error messages
- ✅ Can treat all weapons polymorphically
- ✅ Can query: `Weapon allSubclasses` → `{Stone. Paper. Scissors. Lizard. Spock}`
- ✅ Can create: `weapons := Weapon allSubclasses collect: [:each | each new]`

---

## Practical Example: Why Superclass Matters

### Adding a New Weapon (Fire)

**Without Weapon superclass:**
```smalltalk
"Create Fire class"
Object subclass: #Fire

"Implement 7 methods:"
Fire >> vs: aWeapon withResultHandler: aHandler
    ^ aWeapon playAgainst: self withResultHandler: aHandler  "Copy-paste from Stone!"

Fire >> playAgainstStone: aStone withResultHandler: aHandler
    ^ aHandler stoneWon

"... 5 more playAgainst methods"

"OOPS! Forgot to implement playAgainstLizard:"
"Will fail at runtime with confusing error"
```

**With Weapon superclass:**
```smalltalk
"Create Fire class"
Weapon subclass: #Fire

"Implement 5 methods:"
Fire >> playAgainstStone: aStone withResultHandler: aHandler
    ^ aHandler fireWon

"... 4 more"

"vs:withResultHandler: is inherited - no duplication!"

"If you forget playAgainstLizard:"
"Clear error: 'Should have been implemented by subclass Fire'"
```

---

## Testing Benefits

### Without Superclass

```smalltalk
"Hard to verify all weapons implement all methods:"
"Must manually check each weapon class"
```

### With Superclass

```smalltalk
"Can write a test that verifies completeness:"
WeaponTest >> testAllWeaponsImplementFullInterface
    | requiredSelectors |
    requiredSelectors := #(
        playAgainstStone:withResultHandler:
        playAgainstPaper:withResultHandler:
        playAgainstScissors:withResultHandler:
        playAgainstLizard:withResultHandler:
        playAgainstSpock:withResultHandler:
    ).

    Weapon allSubclasses do: [ :weaponClass |
        requiredSelectors do: [ :selector |
            self assert: (weaponClass includesSelector: selector)
                description: weaponClass name, ' missing ', selector ] ]

"Automatically catches missing implementations!"
```

---

## Meyer's Perspective (OOSC2)

### Chapter 1: Correctness

**Superclass with `subclassResponsibility`** improves correctness:
- Clear contract (specification)
- Fails fast on violations
- Documents requirements

### Chapter 14: Inheritance

Meyer would say:
> "Use inheritance when you have common behavior (vs:withResultHandler:) and a clear interface that all subclasses must implement"

**This is a textbook example!**

---

## Ducasse's Recommendation (My Prediction)

Based on Ducasse's writing and teaching style, he would say:

### ✅ Step 1: Create Weapon Superclass
**"Yes! Clearly shows all weapons share a common interface"**

### ✅ Step 2: Move vs:withResultHandler: to Superclass
**"Absolutely! Don't repeat yourself. This method is identical for all weapons."**

### ✅ Step 3: Use subclassResponsibility
**"Essential! Makes the contract explicit and catches errors early."**

**Overall verdict**: **All three steps are worth doing!** 👍

---

## Implementation Order

### Phase 1: Create Superclass (10 minutes)
```smalltalk
"1. Create abstract class"
Object subclass: #Weapon
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'RockPaperScissors'.

"2. Add the shared method"
Weapon compile: 'vs: aWeapon withResultHandler: aHandler
    "Play against another weapon, notifying handler of result"
    ^ aWeapon playAgainst: self withResultHandler: aHandler'.
```

### Phase 2: Define Interface (10 minutes)
```smalltalk
"3. Define abstract interface"
Weapon compile: 'playAgainstStone: aStone withResultHandler: aHandler
    self subclassResponsibility'.

"Repeat for Paper, Scissors, Lizard, Spock"
```

### Phase 3: Reparent Existing Classes (5 minutes)
```smalltalk
"4. Change superclass"
Weapon subclass: #Stone
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'RockPaperScissors'.

"Repeat for all 5 weapons"
```

### Phase 4: Remove Duplication (5 minutes)
```smalltalk
"5. Remove vs:withResultHandler: from each weapon"
Stone removeSelector: #vs:withResultHandler:.
Paper removeSelector: #vs:withResultHandler:.
"... etc"

"Now inherited from Weapon!"
```

**Total time**: ~30 minutes
**Total benefit**: Much clearer structure!

---

## Final Recommendation

### Do All Three Steps! ✅

**Benefits far outweigh costs:**

| Benefit | Value |
|---------|-------|
| Removes duplication | ⭐⭐⭐ High |
| Documents interface | ⭐⭐⭐ High |
| Fails fast on errors | ⭐⭐⭐ High |
| Enables polymorphism | ⭐⭐ Medium |
| Better testability | ⭐⭐⭐ High |
| **Cost** | ⭐ Low (30 minutes) |

### Order of Implementation

**Combine with Ducasse's ResultHandler approach:**

1. ✅ Create Weapon superclass
2. ✅ Define abstract interface with `subclassResponsibility`
3. ✅ Move `vs:withResultHandler:` to superclass
4. ✅ Update all weapons to use `playAgainst:withResultHandler:`
5. ✅ Create ResultHandler hierarchy
6. ✅ Test everything!

**This gives you the BEST design:**
- No duplication
- Clear contracts
- "Tell, Don't Ask"
- Extensible
- Simple

---

## Quick Checklist

Use this to implement:

```
Creating Weapon Superclass:
[ ] Create Weapon class
[ ] Add vs:withResultHandler: method
[ ] Add 5 subclassResponsibility methods
[ ] Change Stone to inherit from Weapon
[ ] Change Paper to inherit from Weapon
[ ] Change Scissors to inherit from Weapon
[ ] Change Lizard to inherit from Weapon
[ ] Change Spock to inherit from Weapon
[ ] Remove vs:withResultHandler: from all 5 weapons
[ ] Test that everything still works!

Benefits you get:
[ ] No duplicated vs:withResultHandler: code
[ ] Clear interface documentation
[ ] Fast failure on missing methods
[ ] Can use Weapon allSubclasses
[ ] Better testability
```

---

## Answer to Your Question

**"Is it worth doing?"**

**YES! Absolutely worth it!** 🎯

**Why?**
1. Removes duplication (DRY principle)
2. Makes interface explicit (clarity)
3. Fails fast (robustness)
4. Takes only ~30 minutes
5. Makes future changes easier

**Ducasse would say:**
> "Don't repeat yourself. Make interfaces explicit with subclassResponsibility. This is how we write good Smalltalk."

**Meyer would say (Chapter 1):**
> "Extendibility requires design simplicity and decentralization. A clear superclass with an explicit interface achieves both."

---

**TL;DR**: Do all three steps. They're Smalltalk/Pharo best practices, take minimal time, and significantly improve your design! 🚀
