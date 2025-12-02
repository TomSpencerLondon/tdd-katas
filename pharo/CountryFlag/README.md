# CountryFlag - Pharo TDD Kata

A Country Flag Browser built in Pharo using Test-Driven Development. Displays country shapes from SVG and fetches flags from the web.

## Screenshot

When complete, the app shows:
- Dropdown list of 256 countries
- Country code display
- Country shape (blue Roassal visualization)
- Flag image fetched from flagcdn.com

## What You'll Learn

- **TDD** (Test-Driven Development) in Pharo
- **Roassal** visualization (SVG paths)
- **XML parsing** (loading country data)
- **HTTP client** (fetching flags from the web)
- **Spec UI** (building a browser application)

Based on the [CountryFlag tutorial](https://github.com/SquareBracketAssociates/booklet-CountryTutorial/) by Stéphane Ducasse.

---

## Prerequisites

- Pharo 13 image
- `world.svg` file (download from tutorial repo)

## Setup

### 1. Load XMLParser

```smalltalk
Metacello new
    baseline: 'XMLParser';
    repository: 'github://pharo-contributions/XML-XMLParser/src';
    load.
```

### 2. Download world.svg

Get `world.svg` from the [tutorial repo](https://github.com/SquareBracketAssociates/booklet-CountryTutorial/) and save it to this directory.

---

## Classes

### EarthMapCountry

Represents a single country.

**Slots:**
- `name` — Country name (e.g., 'France')
- `code` — ISO code (e.g., 'FR')
- `svgPath` — SVG path data for country shape

**Methods:**
- `name:`, `code:`, `svgPath:` — Accessors
- `fromXML:` — Create from XML element
- `asRSShape` — Convert to Roassal shape
- `fetchFlag` — Fetch flag PNG from flagcdn.com
- `printOn:` — Display name in inspector

### EarthMap

Container for all countries.

**Slots:**
- `countries` — Collection of EarthMapCountry objects

**Methods:**
- `initialize` — Create empty countries collection
- `countries` — Accessor
- `addCountry:` — Add a country
- `importCountryFromXMLNode:` — Import single country from XML
- `importCountriesFrom:` — Load all countries from world.svg
- `xmlTreeFromFile:` — Parse XML file
- `populatedCanvas` — Create Roassal canvas with all country shapes

### EarthCountryBrowser

Spec UI presenter for browsing countries.

**Slots:**
- `countryList` — Dropdown of countries
- `countryCode` — Text field showing ISO code
- `countryShape` — Roassal presenter for country outline
- `countryFlag` — Image presenter for flag

**Methods:**
- `initializePresenters` — Set up UI widgets
- `defaultLayout` — Arrange widgets (shape and flag side by side)
- `connectPresenters` — Wire up selection events
- `onCountrySelected:` — Handle country selection (updates shape, flag, code)
- `initializeWindow:` — Set window title and size
- `setModelBeforeInitialization:` — Receive model before init

---

## Usage

```smalltalk
| map |
map := EarthMap new.
map importCountriesFrom: '/path/to/world.svg'.
(EarthCountryBrowser on: map) open.
```

### View the world map

```smalltalk
| map |
map := EarthMap new.
map importCountriesFrom: '/path/to/world.svg'.
map populatedCanvas open.
```

---

## TDD Steps

| Step | Feature | Tests |
|------|---------|-------|
| 1 | Basic properties (name, code, svgPath) | 3 |
| 2 | printOn: method | 1 |
| 3 | asRSShape (Roassal) | 1 |
| 4 | fromXML: parsing | 1 |
| 5 | EarthMap container | 2 |
| 6 | Import from XML node | 1 |
| 7 | populatedCanvas | 1 |
| 8 | Import from file | 1 |
| 9 | Flag fetching | 1 |
| 10 | Spec UI | Manual |

**Total: 12 automated tests**

---

## Running Tests

In Pharo:
1. Open Test Runner (Cmd+O, U)
2. Select `CountryFlag-Tests` package
3. Run all tests

---

## Files

```
CountryFlag/
├── README.md           # This file
├── TDD_PLAN.md         # Step-by-step TDD guide
├── CountryFlag.pdf     # Original tutorial PDF
└── world.svg           # Country shape data
```

---

## Key Lessons

### TDD Cycle
1. **RED** — Write failing test
2. **GREEN** — Minimum code to pass
3. **REFACTOR** — Clean up

### Pharo 13 Class Syntax
```smalltalk
Object << #MyClass
    slots: { #mySlot };
    package: 'MyPackage'
```

### Spec UI Pattern
```smalltalk
SpPresenter << #MyBrowser
    slots: { #myWidget . #model };
    package: 'MyPackage'
```

Key methods:
- `initializePresenters` — Create widgets
- `defaultLayout` — Arrange widgets
- `connectPresenters` — Wire events
- `setModelBeforeInitialization:` — Receive model

### Updating Roassal in Spec UI
Use `script:` to rebuild the canvas (not `canvas:`):
```smalltalk
roassalPresenter script: [ :canvas |
    | shape |
    shape := myObject asRSShape.
    canvas add: shape.
    canvas zoomToFit ].
```

### Double Dispatch for Flags
The `fetchFlag` method uses ZnClient to fetch from flagcdn.com:
```smalltalk
fetchFlag
    | request pngArray |
    request := ZnClient new.
    request get: 'https://flagcdn.com/w320/', code asLowercase, '.png'.
    request isSuccess ifTrue: [
        pngArray := request response contents.
        ^ ImageReadWriter formFromStream: (ReadStream on: pngArray) ].
    ^ nil
```

---

## What We Learned

### 1. TDD in Pharo
We followed the RED → GREEN → REFACTOR cycle throughout:
- Write a failing test first
- Implement minimum code to pass
- Clean up while keeping tests green

This gave us confidence that each piece worked before building the next.

### 2. Domain Modeling
We separated concerns into clear classes:
- **EarthMapCountry** — Knows about one country (data + behavior)
- **EarthMap** — Manages collection of countries (import, query)
- **EarthCountryBrowser** — UI only (no domain logic)

Each class has a single responsibility.

### 3. XML Parsing
We learned to:
- Load XML with `XMLDOMParser`
- Navigate the tree with `nodes`, `first`, `attributeAt:`
- Convert XML elements to domain objects with `fromXML:`

### 4. Roassal Visualization
- `RSSVGPath` renders SVG path data
- `RSCanvas` is the drawing surface
- `zoomToFit` scales shapes to visible size
- `@ RSCanvasController` adds zoom/pan interaction

### 5. HTTP in Pharo
`ZnClient` makes HTTP requests simple:
```smalltalk
request := ZnClient new.
request get: 'https://example.com/image.png'.
request isSuccess ifTrue: [ ... ].
```

### 6. Spec UI Framework
Key patterns we discovered:
- **Factory methods**: `newDropList`, `newImage`, `newRoassal`
- **Layout**: `SpBoxLayout` with `newTopToBottom`, `newLeftToRight`
- **Model timing**: Use `setModelBeforeInitialization:` to receive model before `initializePresenters`
- **Roassal updates**: Use `script:` block (not `canvas:`) to rebuild dynamically

### 7. Debugging in Pharo
- Use Playground to test code snippets
- Use `inspect` to examine objects
- Use Transcript for logging
- Isolate problems by testing components separately

### 8. Image-Based Development
Everything lives in the Pharo image:
- Classes are created interactively
- Code is saved by saving the image
- No separate compile step needed

---

## Resources

- [Original Tutorial PDF](CountryFlag.pdf)
- [Tutorial GitHub Repo](https://github.com/SquareBracketAssociates/booklet-CountryTutorial/)
- [Roassal Documentation](https://github.com/pharo-graphics/Roassal)
- [Spec2 Book](https://github.com/SquareBracketAssociates/BuildingApplicationWithSpec2/)
- [Pharo by Example](https://books.pharo.org/)
