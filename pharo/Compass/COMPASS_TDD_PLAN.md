# Compass Kata: TDD in Smalltalk

## The Problem

Build a Compass that knows the cardinal and intercardinal directions (N, NE, E, SE, S, SW, W, NW) and can:
1. Turn clockwise or counter-clockwise
2. Report the current direction
3. Calculate degrees
4. Find the opposite direction

This kata teaches **double dispatch** and **"Tell, Don't Ask"** - the same patterns you learned in Rock-Paper-Scissors.

---

## Why This Kata?

| Concept | How Compass Teaches It |
|---------|------------------------|
| **Polymorphism** | Each direction is an object with behavior |
| **Double Dispatch** | Directions know their relationships |
| **State Pattern** | Compass holds current direction object |
| **Tell, Don't Ask** | Ask compass to turn, don't calculate externally |

---

## TDD Approach: The Rules

1. **RED**: Write a failing test first
2. **GREEN**: Write minimum code to pass
3. **REFACTOR**: Clean up while tests stay green
4. **Repeat**: One small step at a time

**Key Mindset**: Let the tests drive the design. Don't think ahead!

---

## Phase 1: Start with the Simplest Thing

### Test 1: A Direction Exists

**What we're testing**: Can we create a North direction?

```smalltalk
CompassTest >> testNorthExists
    | north |
    north := North new.
    self assert: north isNotNil.
```

**To make it pass**:
```smalltalk
Object subclass: #North
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Compass'.
```

**What we learned**: Start with existence, not behavior.

---

### Test 2: Direction Knows Its Name

**What we're testing**: Can North identify itself?

```smalltalk
CompassTest >> testNorthName
    | north |
    north := North new.
    self assert: north name equals: 'N'.
```

**To make it pass**:
```smalltalk
North >> name
    ^ 'N'
```

**Refactor opportunity**: Should we add all directions now? NO! Only add what tests require.

---

### Test 3: North Knows What's Clockwise

**What we're testing**: If I'm facing North and turn clockwise, what do I face?

```smalltalk
CompassTest >> testNorthClockwiseIsNorthEast
    | north next |
    north := North new.
    next := north clockwise.
    self assert: next name equals: 'NE'.
```

**To make it pass**:
```smalltalk
"First, create NorthEast"
Object subclass: #NorthEast
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Compass'.

NorthEast >> name
    ^ 'NE'

"Then, implement clockwise in North"
North >> clockwise
    ^ NorthEast new
```

**Discussion**: Notice we're creating objects on demand. TDD reveals what we need.

---

## Phase 2: Build the Ring of Directions

### Test 4: Continue Around the Compass

```smalltalk
CompassTest >> testNorthEastClockwiseIsEast
    self assert: NorthEast new clockwise name equals: 'E'.

CompassTest >> testEastClockwiseIsSouthEast
    self assert: East new clockwise name equals: 'SE'.

"... continue for all 8 directions"

CompassTest >> testNorthWestClockwiseIsNorth
    "The ring completes!"
    self assert: NorthWest new clockwise name equals: 'N'.
```

**Pattern emerging**: Each direction knows its clockwise neighbor.

**After all tests pass, you'll have**:
```
North → NorthEast → East → SouthEast → South → SouthWest → West → NorthWest → North
```

---

### Test 5: Counter-Clockwise (The Opposite Direction of Travel)

```smalltalk
CompassTest >> testNorthCounterClockwiseIsNorthWest
    self assert: North new counterClockwise name equals: 'NW'.

CompassTest >> testEastCounterClockwiseIsNorthEast
    self assert: East new counterClockwise name equals: 'NE'.
```

**Implementation options**:

**Option A**: Hardcode each relationship
```smalltalk
North >> counterClockwise
    ^ NorthWest new
```

**Option B**: Derive from clockwise (smarter, but wait for refactor phase!)
```smalltalk
"After all directions exist:"
Direction >> counterClockwise
    "Go clockwise 7 times = go counter-clockwise once"
    ^ self clockwise clockwise clockwise clockwise
           clockwise clockwise clockwise
```

---

## Phase 3: Introduce the Compass Object

### Test 6: Compass Starts Facing North

```smalltalk
CompassTest >> testCompassStartsFacingNorth
    | compass |
    compass := Compass new.
    self assert: compass currentDirection name equals: 'N'.
```

**Implementation**:
```smalltalk
Object subclass: #Compass
    instanceVariableNames: 'currentDirection'
    classVariableNames: ''
    package: 'Compass'.

Compass >> initialize
    currentDirection := North new.

Compass >> currentDirection
    ^ currentDirection
```

---

### Test 7: Compass Can Turn Clockwise

```smalltalk
CompassTest >> testCompassTurnClockwise
    | compass |
    compass := Compass new.
    compass turnClockwise.
    self assert: compass currentDirection name equals: 'NE'.
```

**Implementation**:
```smalltalk
Compass >> turnClockwise
    currentDirection := currentDirection clockwise
```

**Notice**: The Compass TELLS the direction to give its clockwise neighbor. It doesn't calculate anything itself. This is "Tell, Don't Ask"!

---

### Test 8: Multiple Turns

```smalltalk
CompassTest >> testCompassTurnClockwiseTwice
    | compass |
    compass := Compass new.
    compass turnClockwise; turnClockwise.
    self assert: compass currentDirection name equals: 'E'.

CompassTest >> testCompassFullRotation
    | compass |
    compass := Compass new.
    8 timesRepeat: [ compass turnClockwise ].
    self assert: compass currentDirection name equals: 'N'.
```

---

## Phase 4: Refactor - Introduce Direction Superclass

**When tests are green, we can refactor!**

### Observation: All Directions Share Structure

```smalltalk
"Current state - lots of similar classes:"
North >> name
    ^ 'N'

NorthEast >> name
    ^ 'NE'

"... 6 more identical patterns"
```

### Refactoring Step 1: Create Abstract Superclass

```smalltalk
Object subclass: #Direction
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Compass'.

Direction >> name
    self subclassResponsibility.

Direction >> clockwise
    self subclassResponsibility.

Direction >> counterClockwise
    self subclassResponsibility.
```

### Refactoring Step 2: Reparent All Directions

```smalltalk
Direction subclass: #North
Direction subclass: #NorthEast
Direction subclass: #East
"... etc"
```

**Run tests**: They should all still pass!

---

## Phase 5: Advanced Features

### Test 9: Degrees

```smalltalk
CompassTest >> testNorthIs0Degrees
    self assert: North new degrees equals: 0.

CompassTest >> testEastIs90Degrees
    self assert: East new degrees equals: 90.

CompassTest >> testSouthIs180Degrees
    self assert: South new degrees equals: 180.
```

**Implementation**:
```smalltalk
North >> degrees
    ^ 0

NorthEast >> degrees
    ^ 45

East >> degrees
    ^ 90

"... etc, each 45 degrees apart"
```

---

### Test 10: Opposite Direction

```smalltalk
CompassTest >> testNorthOppositeIsSouth
    self assert: North new opposite name equals: 'S'.

CompassTest >> testEastOppositeIsWest
    self assert: East new opposite name equals: 'W'.
```

**Implementation Options**:

**Option A**: Hardcode
```smalltalk
North >> opposite
    ^ South new
```

**Option B**: Calculate (turn 4 times)
```smalltalk
Direction >> opposite
    ^ self clockwise clockwise clockwise clockwise
```

---

### Test 11: Distance Between Directions

```smalltalk
CompassTest >> testDistanceNorthToEast
    self assert: (North new distanceTo: East new) equals: 2.

CompassTest >> testDistanceNorthToSouth
    self assert: (North new distanceTo: South new) equals: 4.

CompassTest >> testDistanceNorthToNorthWest
    self assert: (North new distanceTo: NorthWest new) equals: 1.
    "Or 7 going the long way - shortest is 1"
```

**This is where double dispatch shines!**

---

## Phase 6: Double Dispatch (Advanced)

### The Problem: How Does North Know Its Distance to East?

**Bad approach (if statements)**:
```smalltalk
North >> distanceTo: aDirection
    aDirection name = 'N' ifTrue: [ ^ 0 ].
    aDirection name = 'NE' ifTrue: [ ^ 1 ].
    aDirection name = 'E' ifTrue: [ ^ 2 ].
    "... ugly!"
```

**Good approach (double dispatch)**:
```smalltalk
North >> distanceTo: aDirection
    ^ aDirection distanceFromNorth

East >> distanceFromNorth
    ^ 2  "N → NE → E"

South >> distanceFromNorth
    ^ 4  "N → NE → E → SE → S"
```

**Each direction knows its distance FROM every other direction!**

---

## Complete Test List

Use this checklist to track progress:

```
Phase 1: Basic Direction
[ ] testNorthExists
[ ] testNorthName

Phase 2: Clockwise Ring
[ ] testNorthClockwiseIsNorthEast
[ ] testNorthEastClockwiseIsEast
[ ] testEastClockwiseIsSouthEast
[ ] testSouthEastClockwiseIsSouth
[ ] testSouthClockwiseIsSouthWest
[ ] testSouthWestClockwiseIsWest
[ ] testWestClockwiseIsNorthWest
[ ] testNorthWestClockwiseIsNorth (ring completes!)

Phase 3: Counter-Clockwise
[ ] testNorthCounterClockwiseIsNorthWest
[ ] testEastCounterClockwiseIsNorthEast

Phase 4: Compass Object
[ ] testCompassStartsFacingNorth
[ ] testCompassTurnClockwise
[ ] testCompassTurnCounterClockwise
[ ] testCompassFullRotation

Phase 5: Properties
[ ] testNorthIs0Degrees
[ ] testEastIs90Degrees
[ ] testSouthIs180Degrees
[ ] testWestIs270Degrees

Phase 6: Relationships
[ ] testNorthOppositeIsSouth
[ ] testEastOppositeIsWest
[ ] testDistanceNorthToEast
[ ] testDistanceNorthToSouth
```

---

## Key Lessons This Kata Teaches

### 1. Objects Have Behavior, Not Just Data

**Wrong thinking**: "A direction is just a string like 'N'"
**Right thinking**: "A direction is an object that knows its neighbors"

### 2. Let Tests Drive Design

You didn't plan 8 direction classes upfront. Each test revealed what was needed next.

### 3. Polymorphism Eliminates Conditionals

Instead of:
```smalltalk
if direction = 'N' then next := 'NE'
if direction = 'NE' then next := 'E'
```

You have:
```smalltalk
next := direction clockwise
```

### 4. Double Dispatch for Relationships

When two objects need to interact (distanceTo:), let them collaborate through message passing, not conditionals.

### 5. Refactor When Green

Only restructure code (add superclass, move methods) when all tests pass. Tests protect you during refactoring.

---

## Connection to OOSC2 Chapter 1

| Principle | How Compass Demonstrates It |
|-----------|---------------------------|
| **Correctness** | Tests verify each direction behaves correctly |
| **Robustness** | Ring structure means no edge cases - always a valid next direction |
| **Extendibility** | Adding 16-point compass (NNE, ENE, etc.) just means more classes |
| **Reusability** | Direction objects can be used anywhere (maps, games, navigation) |

---

## How to Start in Pharo

1. **Create a new package**: `Compass`

2. **Create test class**:
```smalltalk
TestCase subclass: #CompassTest
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Compass'.
```

3. **Write your first test** (copy from Phase 1)

4. **Run it** - watch it fail (RED)

5. **Implement minimum code** to pass (GREEN)

6. **Repeat!**

---

## Tips for Effective TDD

1. **Smallest possible test**: Test one thing at a time
2. **Watch it fail first**: Confirms the test is actually testing something
3. **Minimum code to pass**: Don't anticipate future needs
4. **Refactor only when green**: Never refactor while tests are failing
5. **Commit after each green**: Small, safe steps

---

## Next Steps

1. Open Pharo
2. Create `Compass` package
3. Create `CompassTest` class
4. Write `testNorthExists`
5. Watch it fail
6. Create `North` class
7. Watch it pass
8. Continue with the plan!

Good luck! Let the tests guide you.
