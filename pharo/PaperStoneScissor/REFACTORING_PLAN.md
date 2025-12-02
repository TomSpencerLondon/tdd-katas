# Rock-Paper-Scissors-Lizard-Spock: Refactoring Plan

## Current State (What You Have)

### Architecture Overview
```
Weapon (abstract)
  - play: aWeapon withResultHandler: aResultHandler
  - playAgainstPaper: ... (subclassResponsibility)
  - playAgainstStone: ... (subclassResponsibility)
  - etc.

Stone, Paper, Scissors, Lizard, Spock (concrete weapons)
  - Each implements all playAgainst... methods
  - Uses double dispatch pattern

ResultHandler (abstract)
  - handleDraw (subclassResponsibility)
  - paperCoversStone (subclassResponsibility)
  - etc.

TranscriptResultHandler (concrete handler)
  - Prints results to Transcript
```

### What's Already Good

✅ **Extendibility**: Adding new weapons requires creating a new class
✅ **Separation of Concerns**: Game logic separate from result handling
✅ **Polymorphism**: Double dispatch elegantly resolves game outcomes
✅ **Tell, Don't Ask**: ResultHandler receives commands, doesn't query state

---

## Applying Chapter 1 Quality Factors

Let's systematically improve the design using Meyer's principles:

### 1. CORRECTNESS: Add Contracts (Design by Contract)

**Current State**: No explicit specifications or validation

**Problem**:
- What happens if you pass `nil` to `play:`?
- Are all weapon combinations handled?
- How do we know the rules are correct?

**Refactoring Goal**: Add assertions to specify contracts

```smalltalk
"Add preconditions and postconditions"
Weapon >> play: aWeapon withResultHandler: aResultHandler
  "Precondition: Arguments must not be nil and aResultHandler must respond to protocol"
  self assert: aWeapon isNotNil description: 'Weapon cannot be nil'.
  self assert: aResultHandler isNotNil description: 'ResultHandler cannot be nil'.
  self assert: (aResultHandler respondsTo: #handleDraw) description: 'Invalid handler'.

  "Delegate using double dispatch"
  aWeapon playAgainst: self withResultHandler: aResultHandler.

  "Postcondition: Handler was notified (could track this)"
```

**Benefits (from Chapter 1)**:
- **Correctness**: Clear specification of what inputs are valid
- **Robustness**: Graceful handling of invalid cases
- **Documentation**: The code documents its own requirements

---

### 2. ROBUSTNESS: Handle Edge Cases

**Current State**: Assumes all inputs are valid weapon objects

**Problems**:
- What if someone passes a String instead of a Weapon?
- What if a new Weapon subclass forgets to implement a method?
- What if ResultHandler is incomplete?

**Refactoring Goal**: Add defensive validation

```smalltalk
"In Weapon superclass"
Weapon >> play: aWeapon withResultHandler: aResultHandler
  "Validate input types"
  (aWeapon isKindOf: Weapon)
    ifFalse: [ self error: 'Can only play against another Weapon' ].

  (aResultHandler isKindOf: ResultHandler)
    ifFalse: [ self error: 'Invalid result handler' ].

  "Check handler completeness"
  self validateResultHandler: aResultHandler.

  ^ aWeapon playAgainst: self withResultHandler: aResultHandler

Weapon >> validateResultHandler: aResultHandler
  "Ensure handler implements required protocol"
  #(handleDraw paperCoversStone stoneCrushesScissors
    scissorsCutsPaper lizardEatsPaper spockSmashesScissors
    paperDisprovesSpock lizardPoisonsSpock stoneCrushesLizard
    scissorsDecapitatesLizard spockVaporizesStone)
  do: [ :selector |
    (aResultHandler respondsTo: selector)
      ifFalse: [ self error: 'Handler missing required method: ', selector ] ]
```

**Benefits**:
- **Robustness**: System doesn't crash on invalid input
- **Debugging**: Clear error messages
- **Safety**: Prevents silent failures

---

### 3. EXTENDIBILITY: Make Adding Weapons Easier

**Current Challenge**: Adding a new weapon (e.g., "Fire") requires:
1. Create new class
2. Implement 5 playAgainst... methods (one for each existing weapon)
3. Add 1 new method to each existing weapon (playAgainstFire:)
4. Add new result methods to ResultHandler

**Refactoring Goal**: Reduce coupling, make extension easier

**Option A: Table-Driven Rules**

```smalltalk
"Create a WeaponRules class"
Object subclass: #WeaponRules
  instanceVariableNames: 'rules'
  classVariableNames: ''
  package: 'RockPaperScissors'

WeaponRules >> initialize
  super initialize.
  rules := Dictionary new.
  self defineRules

WeaponRules >> defineRules
  "Define all winning combinations"
  self
    weapon: Stone defeats: Scissors reason: #stoneCrushesScissors;
    weapon: Stone defeats: Lizard reason: #stoneCrushesLizard;
    weapon: Paper defeats: Stone reason: #paperCoversStone;
    weapon: Paper defeats: Spock reason: #paperDisprovesSpock;
    weapon: Scissors defeats: Paper reason: #scissorsCutsPaper;
    weapon: Scissors defeats: Lizard reason: #scissorsDecapitatesLizard;
    weapon: Lizard defeats: Paper reason: #lizardEatsPaper;
    weapon: Lizard defeats: Spock reason: #lizardPoisonsSpock;
    weapon: Spock defeats: Scissors reason: #spockSmashesScissors;
    weapon: Spock defeats: Stone reason: #spockVaporizesStone

WeaponRules >> weapon: winnerClass defeats: loserClass reason: reasonSymbol
  | key |
  key := winnerClass name, '->', loserClass name.
  rules at: key put: reasonSymbol

WeaponRules >> result: weapon1 versus: weapon2
  | key |
  key := weapon1 class name, '->', weapon2 class name.
  ^ rules at: key ifAbsent: [ #handleDraw ]
```

Then simplify Weapon:

```smalltalk
Weapon class >> rules
  ^ WeaponRules singleton

Weapon >> play: aWeapon withResultHandler: aResultHandler
  | outcomeSymbol |
  outcomeSymbol := self class rules result: self versus: aWeapon.
  aResultHandler perform: outcomeSymbol
```

**Benefits**:
- **Extendibility**: Add new weapons by updating rules table only
- **Maintainability**: All rules in one place
- **Testability**: Easy to verify completeness of rules

**Trade-off**: Less explicit than double dispatch, but more scalable

---

### 4. REUSABILITY: Extract Common Patterns

**Current Duplication**: Each weapon has 5 very similar methods

**Refactoring Goal**: Create reusable testing utilities

```smalltalk
"Create a test helper"
Object subclass: #WeaponTestHelper
  instanceVariableNames: ''
  classVariableNames: ''
  package: 'RockPaperScissors-Tests'

WeaponTestHelper class >> assertWeapon: winner beats: loser expectedMessage: symbol
  | handler outcome |
  handler := TestResultHandler new.
  winner play: loser withResultHandler: handler.
  self assert: handler lastOutcome equals: symbol

WeaponTestHelper class >> assertDraw: weapon1 with: weapon2
  | handler |
  handler := TestResultHandler new.
  weapon1 play: weapon2 withResultHandler: handler.
  self assert: handler lastOutcome equals: #handleDraw

"Now tests become very concise"
StoneTest >> testStoneBeatsScissors
  WeaponTestHelper
    assertWeapon: Stone new
    beats: Scissors new
    expectedMessage: #stoneCrushesScissors
```

**Benefits (Chapter 1)**:
- **Reusability**: Test pattern captured once, used everywhere
- **Correctness**: Consistent test structure reduces errors
- **Maintainability**: Change test approach in one place

---

### 5. COMPATIBILITY: Standardize Result Protocol

**Current State**: ResultHandler has 11 methods (one per outcome)

**Potential Issue**: As weapons grow, methods proliferate

**Refactoring Goal**: Use a more uniform protocol

```smalltalk
"Create a GameOutcome value object"
Object subclass: #GameOutcome
  instanceVariableNames: 'winner loser outcomeType description'
  classVariableNames: ''
  package: 'RockPaperScissors'

GameOutcome class >> winner: aWinner loser: aLoser type: typeSymbol description: aString
  ^ self new
    winner: aWinner;
    loser: aLoser;
    outcomeType: typeSymbol;
    description: aString;
    yourself

GameOutcome class >> draw
  ^ self new
    outcomeType: #draw;
    description: 'It is a draw!';
    yourself

"Simplify ResultHandler to single method"
ResultHandler >> handleOutcome: aGameOutcome
  self subclassResponsibility

TranscriptResultHandler >> handleOutcome: aGameOutcome
  Transcript show: aGameOutcome description; cr

"Weapons now use uniform protocol"
Stone >> playAgainstScissors: scissors withResultHandler: handler
  handler handleOutcome:
    (GameOutcome
      winner: self
      loser: scissors
      type: #stoneCrushesScissors
      description: 'Stone crushes Scissors! Stone wins.')
```

**Benefits**:
- **Compatibility**: All handlers use same protocol
- **Extendibility**: Easy to add new handler types
- **Simplicity**: Handler interface reduced to one method

---

### 6. EFFICIENCY: Consider Performance

**Current State**: Double dispatch is efficient for small games

**Potential Issue**: With many weapons, message sends add up

**Refactoring Goal**: Profile first, optimize if needed

```smalltalk
"Add benchmarking"
WeaponBenchmark >> run
  | stone scissors handler |
  stone := Stone new.
  scissors := Scissors new.
  handler := NullResultHandler new. "Does nothing"

  ^ [1000000 timesRepeat: [
      stone play: scissors withResultHandler: handler
    ]] timeToRun

"Create NullResultHandler for testing/benchmarking"
ResultHandler subclass: #NullResultHandler
  "Implements all methods as no-ops for performance testing"
```

**Benefits**:
- **Efficiency**: Know actual performance characteristics
- **Correctness**: Don't optimize prematurely (Meyer's warning!)

---

## Implementation Phases

### Phase 1: Add Contracts (Correctness + Robustness)
- [ ] Add preconditions to `play:withResultHandler:`
- [ ] Add validation for weapon types
- [ ] Add validation for result handler completeness
- [ ] Add tests for error cases

**Estimated Time**: 1-2 hours
**Risk**: Low
**Benefit**: High (catches bugs early)

### Phase 2: Introduce GameOutcome (Compatibility + Simplicity)
- [ ] Create GameOutcome class
- [ ] Refactor one weapon (e.g., Stone) to use it
- [ ] Refactor ResultHandler to use single method
- [ ] Migrate all weapons gradually
- [ ] Update tests

**Estimated Time**: 2-3 hours
**Risk**: Medium (significant change)
**Benefit**: High (cleaner design)

### Phase 3: Extract Rules Table (Extendibility)
- [ ] Create WeaponRules class
- [ ] Define all rules declaratively
- [ ] Refactor weapons to use rules
- [ ] Add completeness validation
- [ ] Test thoroughly

**Estimated Time**: 3-4 hours
**Risk**: High (architectural change)
**Benefit**: Very High (easy to add new weapons)

### Phase 4: Extract Test Helpers (Reusability)
- [ ] Create WeaponTestHelper
- [ ] Refactor existing tests
- [ ] Add tests for test helper itself

**Estimated Time**: 1-2 hours
**Risk**: Low
**Benefit**: Medium (cleaner tests)

---

## Decision Points

### Should You Use Table-Driven Rules?

**Pros**:
- Much easier to add new weapons
- Rules visible in one place
- Can validate completeness programmatically
- Can generate documentation automatically

**Cons**:
- Less explicit than double dispatch
- Slightly less "object-oriented" (more data-driven)
- Loses compile-time method checking

**Meyer's Perspective**: He would likely prefer keeping double dispatch for its clarity and type safety, BUT he also values extendibility highly. Consider:
- Keep double dispatch for small, stable weapon sets
- Use table-driven for large, evolving weapon sets

### Should You Use GameOutcome Object?

**Pros**:
- Single, uniform protocol for handlers
- Easier to add new handler types
- Can carry additional metadata (timestamps, player info, etc.)
- Aligns with Meyer's emphasis on contracts

**Cons**:
- Extra object creation (minor)
- One more class to understand

**Recommendation**: YES - This aligns perfectly with Chapter 1's emphasis on clear interfaces and compatibility.

---

## Experiments to Try

### Experiment 1: Add a New Weapon

Try adding "Fire" that:
- Burns Paper
- Melts Scissors
- Evaporates Lizard
- Loses to Stone (extinguished)
- Loses to Spock (force field)

**Questions**:
- How many classes do you need to modify?
- How long does it take?
- What breaks?

### Experiment 2: Add a New ResultHandler

Try creating:
- `ScoreKeepingResultHandler` - Tracks wins/losses
- `HTMLResultHandler` - Generates HTML output
- `AnimationResultHandler` - Triggers visual effects

**Questions**:
- How easy is it to add new behavior?
- Do the handlers work with all weapons?

### Experiment 3: Add Contracts

Add assertions to enforce contracts:
- Preconditions on inputs
- Postconditions on results
- Invariants on weapon state

**Questions**:
- Do they catch any bugs?
- Do they help document the code?
- What's the performance impact?

---

## Connection to OOSC2 Chapter 1

Your current design already demonstrates several quality factors:

| Factor | Current State | How to Improve |
|--------|---------------|----------------|
| **Correctness** | 😐 Implicit | ✅ Add contracts/assertions |
| **Robustness** | 😐 No validation | ✅ Add input validation |
| **Extendibility** | 😐 Medium | ✅ Table-driven rules OR keep double dispatch |
| **Reusability** | ✅ Good | ✅ Extract test helpers |
| **Compatibility** | 😐 Handler interface works | ✅ Standardize with GameOutcome |
| **Efficiency** | ✅ Fast enough | ✅ Profile if needed |

---

## Next Steps

1. **Read this plan** and decide which phases interest you most
2. **Pick one experiment** to try first (I recommend GameOutcome)
3. **Implement incrementally** - don't change everything at once
4. **Compare before/after** - Keep old version to contrast
5. **Document learnings** - What worked? What didn't?

Want me to help you implement any of these refactorings in Pharo code?
