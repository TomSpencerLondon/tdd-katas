# Leap Year Kata - Presentation Script

## Slide 1: Title Slide
**[SLIDE: Leap Year Kata - A Test-Driven Development Journey]**

"Good morning/afternoon everyone. Today I'm going to walk you through the Leap Year kata, demonstrating Test-Driven Development in action. This is a perfect kata for learning TDD because it has clear rules, increasing complexity, and teaches us important lessons about how to approach problem-solving incrementally."

---

## Slide 2: What is a Leap Year?
**[SLIDE: What is a Leap Year?]**

"Let's start with some context. A leap year is a year with an extra day - February 29th - which helps align our calendar year with the solar year.

Before 1582, the Julian Calendar simply made every year divisible by 4 a leap year. But this wasn't quite accurate enough, so the Gregorian Calendar was introduced with more refined rules.

Our goal today is to implement a function called `is_leap_year` that takes a year and returns true or false based on the Gregorian Calendar rules."

---

## Slide 3: The Rules
**[SLIDE: Gregorian Calendar Rules]**

"Here are the four rules we need to implement:

First, years divisible by 400 ARE leap years. For example, 2000 was a leap year.

Second, years divisible by 100 but NOT by 400 are NOT leap years. So 1900, 1800, and 1700 were not leap years, and 2100 won't be either.

Third, years divisible by 4 but not by 100 ARE leap years. This is the most common case - 2016, 2012, 2008, and so on.

And fourth, years not divisible by 4 are NOT leap years - like 2017, 2018, and 2019.

Now, these rules might seem complex, but here's the key insight of TDD: we're NOT going to implement all of this at once. We're going to discover these rules incrementally through our tests."

---

## Slide 4: TDD Approach
**[SLIDE: Our TDD Approach]**

"Let's talk about the TDD cycle we'll follow.

RED: First, we write a failing test. This test describes behavior we want but haven't implemented yet.

GREEN: Then we write the simplest possible code to make that test pass. And I mean SIMPLEST - even hardcoding values is fine at this stage.

REFACTOR: Once the test passes, we look for opportunities to improve the code without changing its behavior.

COMMIT: We commit our changes with a clear message documenting what we did and what we learned.

And then we REPEAT: We add another test and go through the cycle again.

This might feel slow at first, but it builds confidence and creates a clear history of how the solution evolved."

---

## Slide 5: The Four Phases
**[SLIDE: The Four Phases]**

"We're going to tackle this kata in four phases, each building on the last:

Phase A focuses on non-leap years. We'll start with the simplest case and just return false.

Phase B adds basic leap years - those divisible by 4. This forces us to add real logic.

Phase C introduces century exceptions - years like 1900 that are divisible by 4 but NOT leap years. This will break our simple logic and force us to handle the special case.

And Phase D handles the 400-year rule - making sure years like 2000 ARE leap years despite being divisible by 100.

Let's work through each phase now."

---

## Slide 6: Phase A - Test 2017
**[SLIDE: Phase A - Test 2017 → False]**

"Phase A: Non-leap years. We start with the absolute simplest test.

RED: We write a test that says 2017 should not be a leap year. When we run it, it fails because our function returns None.

GREEN: Now here's where TDD gets interesting. We make it pass with the SIMPLEST possible code - just `return False`. No logic, no conditionals, just a hardcoded value.

This might feel silly, but there's an important principle here called 'Fake it till you make it.' We start simple and let the tests force us to add complexity.

LEARNING: This teaches us to resist the urge to over-engineer. Write the simplest code that passes the current tests."

---

## Slide 7: Phase A Complete
**[SLIDE: Phase A Complete - 3 Tests]**

"We add two more tests for 2018 and 2019. They both pass immediately with our hardcoded `return False`.

This is called triangulation - using multiple examples to confirm our solution works. All three tests pass, and we have 100% coverage.

At this point, we commit with a clear message documenting Phase A."

---

## Slide 8: Phase B - Test 2016
**[SLIDE: Phase B - Test 2016 → True]**

"Phase B: Basic leap years. Now things get interesting.

RED: We write a test that 2016 should be a leap year. This test FAILS because our hardcoded `return False` doesn't work anymore.

GREEN: Now we have to add real logic. We check if the year is divisible by 4 - if so, return True, otherwise return False.

LEARNING: The tests forced us to implement actual logic. We couldn't keep using a hardcoded value. This is TDD at work - the tests drive the design."

---

## Slide 9: Phase B Complete
**[SLIDE: Phase B Complete - 6 Tests]**

"We add tests for 2012 and 2008. These both pass without any code changes - our modulo 4 logic already handles them.

This triangulation gives us confidence that our solution is general, not just hardcoded for 2016.

We're at 6 tests passing, still 100% coverage. Phase B complete - commit!"

---

## Slide 10: Phase C - Test 1900
**[SLIDE: Phase C - Test 1900 → False]**

"Phase C: Century exceptions. This is where it gets really interesting.

RED: We write a test that 1900 should NOT be a leap year. But wait - our test FAILS! Our current code returns True because 1900 is divisible by 4.

This is a key moment in TDD. The test reveals a flaw in our logic. We thought divisible by 4 was the whole story, but there's an exception we didn't account for.

GREEN: We add a check for century years BEFORE our modulo 4 check. If the year is divisible by 100 but not by 400, we return False.

LEARNING: Order matters! We have to check the exception BEFORE the general rule. This is a crucial insight that we might have missed if we tried to implement everything at once."

---

## Slide 11: Phase C Complete
**[SLIDE: Phase C Complete - 10 Tests]**

"We add tests for 1800, 1700, and 2100. All pass with our century logic.

Notice how we're building confidence through triangulation - multiple tests that confirm our logic is correct.

10 tests passing, 100% coverage. Phase C complete!"

---

## Slide 12: Phase D - Test 2000
**[SLIDE: Phase D - Test 2000 → True]**

"Phase D: The 400-year rule.

RED: We write a test that 2000 should be a leap year. Interestingly, this test actually PASSES!

Why? Because 2000 % 400 equals 0, so our century check evaluates to False, and we fall through to the modulo 4 check which returns True.

Our logic accidentally works, but it's implicit and hard to understand.

REFACTOR: We make the 400-year rule explicit by checking it FIRST. Now the code reads top to bottom: check 400, then 100, then 4.

LEARNING: Make implicit logic explicit. Even though the tests pass, we refactor for clarity. The rule order is now obvious: most specific first (400), then less specific (100), then general (4)."

---

## Slide 13: Phase D Complete
**[SLIDE: Phase D Complete - 13 Tests]**

"We add tests for 2400 and 1600. Both pass with our refactored logic.

13 tests passing, 100% code coverage. We're done!"

---

## Slide 14: Final Implementation
**[SLIDE: Final Implementation]**

"Here's our final implementation. It's clean, easy to read, and clearly shows the rule priority:

First, check if divisible by 400 - if yes, it's a leap year.
Then check if divisible by 100 - if yes, it's NOT a leap year.
Then check if divisible by 4 - if yes, it's a leap year.
Otherwise, it's not a leap year.

Every line of this code was driven by a test. We didn't write any code that wasn't demanded by a failing test."

---

## Slide 15: Alternative Implementation
**[SLIDE: Alternative - Boolean Expression]**

"There's also a condensed version using a boolean expression: divisible by 4 AND NOT a century year, OR divisible by 400.

This is more elegant but less explicit. Both implementations are correct - it's a matter of style.

The important thing is that we discovered BOTH of these through TDD. The tests gave us the confidence to refactor."

---

## Slide 16: Test Coverage Breakdown
**[SLIDE: Test Coverage Breakdown]**

"Let's look at our test coverage:

Phase A covered non-leap years - 3 tests.
Phase B covered basic leap years - 3 tests.
Phase C covered century exceptions - 4 tests.
Phase D covered the 400-year rule - 3 tests.

Total: 13 tests with 100% code coverage. Every branch of our code is tested, and every rule is validated by multiple examples."

---

## Slide 17: Key TDD Principles
**[SLIDE: Key TDD Principles]**

"Let's reflect on what we learned:

One: Red-Green-Refactor. We followed this cycle religiously for every change.

Two: Fake it till you make it. We started with hardcoded values and let tests force complexity.

Three: Triangulation. Multiple tests confirmed our solutions were general, not hardcoded.

Four: Small steps. One test at a time, minimal code changes. This kept us focused and reduced bugs.

Five: Order matters. We learned that checking rules in the right order is crucial - most specific first.

Six: Frequent commits. Each phase got its own commit, creating a reviewable history."

---

## Slide 18: Git Commit History
**[SLIDE: Git Commit History]**

"Our git history tells the story of our journey:

Four commits, one for each phase. Each commit message includes:
- Which phase we completed
- Red-Green-Refactor details for each test
- Test counts and coverage percentages
- Key learning points

This creates excellent documentation. Anyone can read through the commits and understand exactly how the solution evolved."

---

## Slide 19: What We Learned
**[SLIDE: What We Learned]**

"So what did we learn from this kata?

TDD drives design incrementally. We didn't need to understand all the rules upfront - we discovered them through testing.

Start simple, let tests demand complexity. We resisted over-engineering and only added what was needed.

Rule order matters. We learned through a failing test that we need to check 400 before 100 before 4.

Explicit is better than implicit. Even when tests passed, we refactored for clarity.

Triangulation validates solutions. Multiple examples gave us confidence our logic was general.

Small commits tell a story. Our git history is a tutorial on how to solve this problem."

---

## Slide 20: Final Slide
**[SLIDE: Complete! 🎉]**

"And we're done! 13 tests passing, 100% coverage, 4 clear commits documenting our journey.

This kata demonstrates the power of TDD. We started with an empty function, let tests drive the design, and arrived at a correct, well-tested solution.

The key insight is this: TDD isn't just about testing - it's about design. The tests guided us to discover the right order of checks, the right structure for the code, and even edge cases we might have missed.

The journey is as valuable as the destination. By taking small steps and following the Red-Green-Refactor cycle, we built both a working solution and deep understanding of the problem.

Thank you! Questions?"

---

## Q&A Preparation

### Common Questions and Answers

**Q: "Why start with hardcoded values? Isn't that a waste of time?"**

A: "Great question! It feels counterintuitive, but starting with hardcoded values serves two purposes. First, it forces us to resist over-engineering - we only add complexity when tests demand it. Second, it helps us focus on making the test pass first, then making the code good. Trying to write perfect code on the first attempt often leads to bugs and over-complication."

**Q: "How do you know when to refactor?"**

A: "Refactor when you see duplication, unclear code, or when you notice a simpler way to express the same logic. The key rule: only refactor when all tests are green. In our kata, we refactored in Phase D to make the 400-year rule explicit, even though the tests already passed. Clarity matters!"

**Q: "What if you write the wrong test?"**

A: "That's part of the process! If a test doesn't add value or tests the wrong thing, delete it or change it. Tests aren't sacred - they're tools to drive design. The commit history shows us discovering the century exception in Phase C - we didn't know about that rule until the test forced us to handle it."

**Q: "Isn't this too slow for real development?"**

A: "It feels slow at first, but TDD actually speeds up development in the long run. You spend less time debugging because you catch issues immediately. You refactor with confidence because tests catch regressions. And you write better code because tests force good design. Plus, this kata took about 15 minutes - not slow at all!"

**Q: "How do you decide what test to write next?"**

A: "Write the simplest test that forces new behavior. In our kata, we went from non-leap years (simplest) to basic leap years (simple logic) to century exceptions (edge case) to the 400-year rule (exception to the exception). Each test added one new piece of complexity."

**Q: "What about performance? Does TDD consider that?"**

A: "TDD focuses on correctness first, optimization second. Make it work, make it right, make it fast - in that order. Our leap year function is already fast (just a few modulo operations), but if we needed to optimize, we'd write a test that verified performance, then refactor to meet that test."

**Q: "Can you do TDD with legacy code that has no tests?"**

A: "Yes, but it's harder. The strategy is: write tests around the area you need to change (characterization tests), then make your changes using TDD, then gradually expand test coverage. It's called 'putting it under test' before refactoring."

**Q: "Do you always write tests first in real projects?"**

A: "Good teams do! The discipline of writing tests first catches bugs early, produces better designs, and creates living documentation. Some situations call for prototyping first, but even then, you should add tests before considering the code 'done'. The test suite becomes your safety net."

---

## Timing Guide

**Total presentation time: 25-30 minutes**

- Intro and rules (Slides 1-5): 5 minutes
- Phase A walkthrough (Slides 6-7): 3 minutes
- Phase B walkthrough (Slides 8-9): 3 minutes  
- Phase C walkthrough (Slides 10-11): 4 minutes
- Phase D walkthrough (Slides 12-13): 4 minutes
- Final implementation and learnings (Slides 14-19): 6 minutes
- Conclusion (Slide 20): 2 minutes
- Q&A: 10-15 minutes

**Tips for presenting:**
- Show the actual tests running if possible (demo)
- Emphasize the Red-Green-Refactor rhythm
- Pause after each phase to check for questions
- Show the git log to demonstrate the clear history
- If you have time, live code one phase to show the flow

---

## Demo Script (If Doing Live Coding)

If you want to demonstrate TDD live, here's a condensed script:

**Setup (1 minute):**
```bash
source venv/bin/activate
pytest -v  # Show all tests passing
```

**Demo Phase A (3 minutes):**
"Let me show you Phase A in action. I'll delete the implementation..."

```python
# In leap_year.py - delete the implementation
def is_leap_year(year):
    pass
```

"Now run the tests..."
```bash
pytest -v  # All fail
```

"Let's start with the first test. Make it pass with the simplest code..."
```python
def is_leap_year(year):
    return False
```

"Run tests again..."
```bash
pytest -v  # Phase A tests pass!
```

"That's the TDD cycle: Red (failing test), Green (simplest code), Refactor (improve). Let's look at the full solution we built..."

```bash
git log --oneline  # Show the journey
```

---

## Slide Transition Phrases

Use these to smoothly move between slides:

- "Now let's move on to..."
- "Building on that foundation..."
- "This is where it gets interesting..."
- "Notice what happens when..."
- "Let's see how the tests drive this decision..."
- "Here's the key insight..."
- "This taught us an important lesson..."
- "Now we're ready to tackle..."

---

## Key Messages to Emphasize

1. **TDD is about design, not just testing**
2. **Small steps reduce bugs and build confidence**
3. **Tests should drive implementation, not document it afterward**
4. **Refactoring is safe when you have tests**
5. **Triangulation confirms solutions are general**
6. **Commit history tells the story of discovery**
7. **Simple code first, optimize later if needed**
8. **The journey teaches as much as the destination**

---

## Closing Thoughts

"This kata is more than just an exercise - it's a microcosm of how TDD works on larger projects. The same principles apply whether you're writing a 10-line function or a 10,000-line application:

- Let tests drive design
- Take small steps
- Refactor with confidence
- Document your journey through commits
- Build incrementally from simple to complex

Thank you for your attention. Now go practice TDD on your own projects!"

---

## Additional Resources to Mention

- Kent Beck's "Test Driven Development: By Example"
- Martin Fowler's "Refactoring"
- The Leap Year kata on GitHub for practice
- cyber-dojo.org for more TDD katas
- exercism.io for language-specific TDD practice

---

**End of Script**
