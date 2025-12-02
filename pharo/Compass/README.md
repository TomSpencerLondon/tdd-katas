# Compass Kata

A TDD exercise in Pharo Smalltalk demonstrating object-oriented design principles.

## Overview

This kata models compass directions as objects, teaching:
- **Test-Driven Development** (RED → GREEN → REFACTOR)
- **Polymorphism** - all directions respond to the same messages
- **Objects know their neighbors** - no lookup tables or conditionals
- **Tell, Don't Ask** - compass mutates state, doesn't just return values

## The Design

### Clockwise Ring

```
N → NE → E → SE → S → SW → W → NW → N (loops back)
```

Each direction is its own class that knows:
- Its `name` ('N', 'NE', 'E', etc.)
- Its `clockwise` neighbor

### Classes

```
Object
├── North       → clockwise → NorthEast
├── NorthEast   → clockwise → East
├── East        → clockwise → SouthEast
├── SouthEast   → clockwise → South
├── South       → clockwise → SouthWest
├── SouthWest   → clockwise → West
├── West        → clockwise → NorthWest
├── NorthWest   → clockwise → North
└── Compass (holds currentDirection)
```

## Usage

```smalltalk
"Create a compass - starts facing North"
compass := Compass new.
compass currentDirection name.  "→ 'N'"

"Turn clockwise"
compass turnClockwise.
compass currentDirection name.  "→ 'NE'"

"Ask a direction directly"
North new clockwise name.  "→ 'NE'"
East new clockwise name.   "→ 'SE'"
```

## Installation

Requires **Pharo 13**.

1. Clone this repository
2. Open Pharo
3. Use Iceberg to load the project:
   - Open Iceberg (World menu → Iceberg)
   - Add → Import from existing clone
   - Select the cloned directory
   - Right-click → Load

## Running Tests

```smalltalk
CompassTest runAll
```

Or use the Test Runner (World menu → Test Runner → select Compass-Test).

## Key Lessons Learned

### Assignment vs Return

```smalltalk
"WRONG - returns value but doesn't change state"
turnClockwise
    ^ currentDirection clockwise

"RIGHT - mutates the compass"
turnClockwise
    currentDirection := currentDirection clockwise
```

### Objects Know Their Neighbors

```smalltalk
"Instead of a lookup table..."
North >> clockwise
    ^ NorthEast new
```

No conditionals. No case statements. Pure polymorphism.

### Pharo 13 Syntax

```smalltalk
"Class definition uses slots:"
Object << #Compass
    slots: { #currentDirection };
    package: 'Compass'.

"Always call super initialize"
initialize
    super initialize.
    currentDirection := North new.
```

## Next Steps

Potential extensions to continue the kata:

| Feature | Test First |
|---------|------------|
| `counterClockwise` | `North new counterClockwise name = 'NW'` |
| `turnCounterClockwise` | Compass turns the other way |
| `degrees` | `North new degrees = 0`, `East new degrees = 90` |
| `opposite` | `North new opposite name = 'S'` |
| Direction superclass | Extract common interface |

## Connection to OOSC2

This kata demonstrates principles from Bertrand Meyer's *Object-Oriented Software Construction*:

| Principle | Application |
|-----------|-------------|
| Polymorphism | All directions share the same interface |
| Encapsulation | Compass hides its internal direction |
| Single Responsibility | Each class does one thing |
| Tell, Don't Ask | `compass turnClockwise` not `compass.setDirection(...)` |

## License

MIT
