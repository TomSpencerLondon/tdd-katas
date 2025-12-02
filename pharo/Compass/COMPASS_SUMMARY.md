# Compass Kata: TDD in Pharo Smalltalk

## Overview

Built a Compass application in Pharo 13 using strict Test-Driven Development (RED → GREEN → REFACTOR).

**Goal:** Learn TDD and OO design by modeling compass directions as objects.

---

## What We Built

### The Clockwise Ring

```
N → NE → E → SE → S → SW → W → NW → N (loops back)
```

- 8 Direction classes (North, NorthEast, East, etc.)
- 1 Compass class that holds current direction

### Each Direction Has

| Method | Returns |
|--------|---------|
| `name` | 'N', 'NE', 'E', etc. |
| `clockwise` | Next direction object |

### Compass Has

| Method | Purpose |
|--------|---------|
| `initialize` | Starts facing North |
| `currentDirection` | Returns current direction object |

---

## The Code

### Direction Class (North as example)

```smalltalk
Object << #North
    slots: { };
    package: 'Compass'.

North >> name
    ^ 'N'

North >> clockwise
    ^ NorthEast new
```

### Compass Class

```smalltalk
Object << #Compass
    slots: { #currentDirection };
    package: 'Compass'.

Compass >> initialize
    super initialize.
    currentDirection := North new.

Compass >> currentDirection
    ^ currentDirection
```

---

## The Tests

```smalltalk
"Basic existence and naming"
testNorthExists
    self assert: North new isNotNil.

testNorthName
    self assert: North new name equals: 'N'.

"Clockwise ring"
testNorthClockwiseIsNorthEast
    self assert: North new clockwise name equals: 'NE'.

testNorthEastClockwiseIsEast
    self assert: NorthEast new clockwise name equals: 'E'.

testEastClockwiseIsSouthEast
    self assert: East new clockwise name equals: 'SE'.

testSouthEastClockwiseIsSouth
    self assert: SouthEast new clockwise name equals: 'S'.

testSouthClockwiseIsSouthWest
    self assert: South new clockwise name equals: 'SW'.

testSouthWestClockwiseIsWest
    self assert: SouthWest new clockwise name equals: 'W'.

testWestClockwiseIsNorthWest
    self assert: West new clockwise name equals: 'NW'.

testNorthWestClockwiseIsNorth
    self assert: NorthWest new clockwise name equals: 'N'.

"Compass state"
testCompassStartsFacingNorth
    | compass |
    compass := Compass new.
    self assert: compass currentDirection name equals: 'N'.
```

---

## TDD Process

### The Cycle

1. **RED** - Write a failing test
2. **GREEN** - Write minimum code to pass
3. **REFACTOR** - Clean up (tests stay green)
4. **REPEAT**

### Example

**Step 1: RED**
```smalltalk
testNorthClockwiseIsNorthEast
    self assert: North new clockwise name equals: 'NE'.
```
Run → Fails: `North does not understand #clockwise`

**Step 2: GREEN**
```smalltalk
"Create NorthEast"
NorthEast >> name
    ^ 'NE'

"Add clockwise to North"
North >> clockwise
    ^ NorthEast new
```
Run → Passes!

**Step 3: REFACTOR**
Nothing to clean up yet.

**Step 4: REPEAT**
Write next test: `testNorthEastClockwiseIsEast`

---

## Key Pharo 13 Lessons

### 1. Class Definition Syntax

```smalltalk
"Pharo 13 uses slots: instead of instanceVariableNames:"
Object << #Compass
    slots: { #currentDirection };
    package: 'Compass'.
```

### 2. Always Call super initialize

```smalltalk
initialize
    super initialize.    "Must be first line!"
    currentDirection := North new.
```

### 3. Instance Side vs Class Side

- `initialize` is an **instance side** method
- Called automatically by `Compass new`
- Select "Inst. side" in System Browser (not "Class side")

---

## OO Design Insights

### Objects Know Their Neighbors

Instead of:
```smalltalk
"BAD: Lookup table or conditionals"
nextDirection := directionMap at: currentDirection.
```

We have:
```smalltalk
"GOOD: Each object knows its neighbor"
North >> clockwise
    ^ NorthEast new
```

### TDD Reveals Design

We didn't plan 8 classes upfront. Each failing test told us what to build next:

1. Test needs `North` → Create `North`
2. Test needs `name` → Add `name` method
3. Test needs `clockwise` → Add `clockwise`, create `NorthEast`
4. Continue around the ring...

### Polymorphism

All directions respond to same messages (`name`, `clockwise`), but each returns different values. No type checking needed.

---

## Class Hierarchy

```
Object
├── North       (name → 'N',  clockwise → NorthEast)
├── NorthEast   (name → 'NE', clockwise → East)
├── East        (name → 'E',  clockwise → SouthEast)
├── SouthEast   (name → 'SE', clockwise → South)
├── South       (name → 'S',  clockwise → SouthWest)
├── SouthWest   (name → 'SW', clockwise → West)
├── West        (name → 'W',  clockwise → NorthWest)
├── NorthWest   (name → 'NW', clockwise → North)
└── Compass
    └── currentDirection (slot)

TestCase
└── CompassTest (11 tests)
```

---

## Next Steps

### Continue the Kata

| Feature | Test First |
|---------|------------|
| `turnClockwise` | `compass turnClockwise. self assert: compass currentDirection name equals: 'NE'` |
| `counterClockwise` | `self assert: North new counterClockwise name equals: 'NW'` |
| `turnCounterClockwise` | `compass turnCounterClockwise. self assert: ...` |
| `degrees` | `self assert: North new degrees equals: 0` (N=0, E=90, S=180, W=270) |
| `opposite` | `self assert: North new opposite name equals: 'S'` |

### Refactor: Create Direction Superclass

Once all directions exist, extract common structure:

```smalltalk
Object << #Direction
    slots: { };
    package: 'Compass'.

Direction >> name
    self subclassResponsibility.

Direction >> clockwise
    self subclassResponsibility.
```

Then reparent: `Direction << #North`

---

## Connection to OOSC2

| Principle | How Applied |
|-----------|-------------|
| **Polymorphism** | All directions have same interface |
| **Encapsulation** | Compass hides internal direction |
| **Single Responsibility** | Each class does one thing |
| **Tell, Don't Ask** | `compass turnClockwise` not `compass.setDirection(compass.getDirection().getNext())` |
| **Design by Contract** | Tests define expected behavior |

---

## Summary

- **TDD works:** Tests drove the entire design
- **Objects model reality:** Directions are objects, not strings
- **Polymorphism eliminates conditionals:** No `if direction = 'N' then...`
- **Small steps:** One test at a time, always passing before moving on
