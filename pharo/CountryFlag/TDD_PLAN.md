# CountryFlag Tutorial - TDD Plan

## Overview
Build a Country Flag Browser in Pharo using Test-Driven Development. The app displays country shapes (from SVG) and fetches flags from the web.

## Prerequisites
- Fresh Pharo 13 image
- Download `world.svg` from: https://github.com/SquareBracketAssociates/booklet-CountryTutorial/

## Step 0: Load Dependencies

```smalltalk
"Load XMLParser"
Metacello new
    baseline: 'XMLParser';
    repository: 'github://pharo-contributions/XML-XMLParser/src';
    load.
```

Roassal should already be included in Pharo 13.

---

## Step 1: EarthMapCountry - Basic Properties

### RED: Write failing tests first

Create test class `EarthMapCountryTest` in package `CountryFlag-Tests`:

```smalltalk
TestCase << #EarthMapCountryTest
    slots: { };
    package: 'CountryFlag-Tests'.
```

#### Test 1.1: Country has a name
```smalltalk
testCountryHasName
    | country |
    country := EarthMapCountry new.
    country name: 'France'.
    self assert: country name equals: 'France'.
```

#### Test 1.2: Country has a code
```smalltalk
testCountryHasCode
    | country |
    country := EarthMapCountry new.
    country code: 'FR'.
    self assert: country code equals: 'FR'.
```

#### Test 1.3: Country has an SVG path
```smalltalk
testCountryHasSvgPath
    | country |
    country := EarthMapCountry new.
    country svgPath: 'M 0 0 L 10 10'.
    self assert: country svgPath equals: 'M 0 0 L 10 10'.
```

### GREEN: Implement to pass

```smalltalk
Object << #EarthMapCountry
    slots: { #name . #code . #svgPath };
    package: 'CountryFlag'.

EarthMapCountry >> name
    ^ name

EarthMapCountry >> name: aString
    name := aString

EarthMapCountry >> code
    ^ code

EarthMapCountry >> code: aString
    code := aString

EarthMapCountry >> svgPath
    ^ svgPath

EarthMapCountry >> svgPath: aString
    svgPath := aString
```

---

## Step 2: EarthMapCountry - Behavior

### RED: Test printOn

#### Test 2.1: Country prints its name
```smalltalk
testCountryPrintOn
    | country |
    country := EarthMapCountry new.
    country name: 'France'.
    self assert: (country printString includesSubstring: 'France').
```

### GREEN: Implement printOn:

```smalltalk
EarthMapCountry >> printOn: aStream
    super printOn: aStream.
    aStream nextPutAll: ' '.
    aStream nextPutAll: (name ifNil: [ 'unnamed' ])
```

---

## Step 3: EarthMapCountry - Roassal Shape

### RED: Test asRSShape

#### Test 3.1: Country converts to Roassal shape
```smalltalk
testCountryAsRSShape
    | country shape |
    country := EarthMapCountry new.
    country svgPath: 'M 0 0 L 10 10'.
    shape := country asRSShape.
    self assert: (shape isKindOf: RSSVGPath).
```

### GREEN: Implement asRSShape

```smalltalk
EarthMapCountry >> asRSShape
    ^ RSSVGPath new svgPath: svgPath
```

---

## Step 4: EarthMapCountry - From XML

### RED: Test fromXML:

#### Test 4.1: Country can be created from XML element
```smalltalk
testCountryFromXML
    | xml country element |
    xml := '<path id="FR" title="France" d="M 0 0 L 10 10"/>'.
    element := (XMLDOMParser parse: xml) root.
    country := EarthMapCountry new fromXML: element.
    self assert: country name equals: 'France'.
    self assert: country code equals: 'FR'.
    self assert: country svgPath equals: 'M 0 0 L 10 10'.
```

### GREEN: Implement fromXML:

```smalltalk
EarthMapCountry >> fromXML: aXMLElement
    name := aXMLElement attributeAt: 'title'.
    code := aXMLElement attributeAt: 'id'.
    svgPath := aXMLElement attributeAt: 'd'.
```

---

## Step 5: EarthMap - Container Class

### RED: Write tests for EarthMap

Create test class `EarthMapTest`:

```smalltalk
TestCase << #EarthMapTest
    slots: { };
    package: 'CountryFlag-Tests'.
```

#### Test 5.1: EarthMap initializes with empty countries
```smalltalk
testEarthMapInitializesEmpty
    | map |
    map := EarthMap new.
    self assert: map countries isEmpty.
```

#### Test 5.2: EarthMap can add a country
```smalltalk
testEarthMapCanAddCountry
    | map country |
    map := EarthMap new.
    country := EarthMapCountry new name: 'France'; code: 'FR'.
    map addCountry: country.
    self assert: map countries size equals: 1.
```

### GREEN: Implement EarthMap

```smalltalk
Object << #EarthMap
    slots: { #countries };
    package: 'CountryFlag'.

EarthMap >> initialize
    super initialize.
    countries := OrderedCollection new.

EarthMap >> countries
    ^ countries

EarthMap >> addCountry: aCountry
    countries add: aCountry
```

---

## Step 6: EarthMap - Import from XML

### RED: Test importing from XML node

#### Test 6.1: Import country from XML node
```smalltalk
testEarthMapImportCountryFromXMLNode
    | map xml element |
    map := EarthMap new.
    xml := '<path id="FR" title="France" d="M 0 0"/>'.
    element := (XMLDOMParser parse: xml) root.
    map importCountryFromXMLNode: element.
    self assert: map countries size equals: 1.
    self assert: map countries first name equals: 'France'.
```

### GREEN: Implement importCountryFromXMLNode:

```smalltalk
EarthMap >> importCountryFromXMLNode: aXMLElement
    countries add: (EarthMapCountry new fromXML: aXMLElement)
```

---

## Step 7: EarthMap - Populated Canvas

### RED: Test populatedCanvas

#### Test 7.1: EarthMap creates a canvas with shapes
```smalltalk
testEarthMapPopulatedCanvas
    | map country canvas |
    map := EarthMap new.
    country := EarthMapCountry new svgPath: 'M 0 0 L 10 10'.
    map addCountry: country.
    canvas := map populatedCanvas.
    self assert: (canvas isKindOf: RSCanvas).
    self assert: canvas shapes size equals: 1.
```

### GREEN: Implement populatedCanvas

```smalltalk
EarthMap >> populatedCanvas
    ^ RSCanvas new
        addAll: (countries collect: [ :country | country asRSShape ]);
        @ RSCanvasController;
        yourself
```

---

## Step 8: EarthMap - Import from File

### RED: Test importing from file

#### Test 8.1: Import countries from SVG file
```smalltalk
testEarthMapImportFromFile
    | map |
    map := EarthMap new.
    map importCountriesFrom: '/path/to/world.svg'.
    self deny: map countries isEmpty.
```

### GREEN: Implement importCountriesFrom:

```smalltalk
EarthMap >> importCountriesFrom: aFilePath
    | document |
    document := self xmlTreeFromFile: aFilePath.
    document nodes first nodes do: [ :node |
        (node class = XMLElement)
            ifTrue: [ self importCountryFromXMLNode: node ] ]

EarthMap >> xmlTreeFromFile: aFileName
    ^ aFileName asFileReference readStreamDo: [ :stream |
        (XMLDOMParser parse: stream) document ]
```

---

## Step 9: Flag Fetching (Integration Test)

### RED: Test flag fetching

```smalltalk
testFetchFlagReturnsForm
    | flag |
    flag := EarthMapCountry new fetchFlagForCode: 'fr'.
    self assert: (flag isKindOf: Form).
```

### GREEN: Implement fetchFlagForCode:

```smalltalk
EarthMapCountry >> fetchFlagForCode: aCode
    | request pngArray |
    request := ZnClient new.
    request get: 'https://flagcdn.com/w320/', aCode asLowercase, '.png'.
    request isSuccess ifTrue: [
        pngArray := request response contents.
        ^ ImageReadWriter formFromStream: (ReadStream on: pngArray) ].
    ^ nil
```

---

## Step 10: Spec UI (Final Integration)

Once all domain classes are tested, build the UI:

1. Create `EarthCountryBrowser` class (inherits `SpPresenterWithModel`)
2. Add presenters: `countryList`, `countryCode`, `countryFlag`
3. Implement `initializePresenters`, `defaultLayout`, `connectPresenters`

The UI is harder to unit test, so focus on manual testing here.

---

## Test Summary

| Step | Test Class | Tests |
|------|------------|-------|
| 1-4 | EarthMapCountryTest | 6 tests |
| 5-8 | EarthMapTest | 5 tests |
| 9 | Integration | 1 test |
| 10 | Manual | UI testing |

**Total: ~12 automated tests**

---

## TDD Cycle Reminder

1. **RED**: Write a failing test
2. **GREEN**: Write minimum code to pass
3. **REFACTOR**: Clean up while tests stay green
4. **REPEAT**

---

## Ready to Start?

Begin with Step 1: Create `EarthMapCountryTest` and write the first failing test!
