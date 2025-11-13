# FizzBuzz TDD Journey - Presentation Script

## Slide 1: Title Slide
Hello! Welcome to this presentation on the FizzBuzz Kata with Test-Driven Development. Today, I'm going to walk you through a complete step-by-step journey of building the classic FizzBuzz problem using TDD principles. We'll cover everything from project setup to the final implementation, and you'll see how proper testing and automation can transform the way you write code.

---

## Slide 2: What is Test-Driven Development?
Let's start with the fundamentals. Test-Driven Development, or TDD, is built around the Red-Green-Refactor cycle.

First, you write a failing test - that's the Red phase. The test fails because the functionality doesn't exist yet.

Then, you write the minimal code needed to make that test pass - that's the Green phase. You're not trying to write perfect code yet, just enough to pass the test.

Finally, you refactor to improve the code quality without changing its behavior - that's the Refactor phase. Your tests act as a safety net during this process.

And after each complete phase, we commit our changes with descriptive messages.

The benefits are substantial: you ensure correctness from the start, create living documentation through your tests, make refactoring much safer, and naturally encourage simple, focused design.

---

## Slide 3: FizzBuzz Problem
So what is FizzBuzz? It's a classic programming problem with simple rules.

Convert numbers to strings following these rules: Multiples of 3 become "Fizz". Multiples of 5 become "Buzz". Numbers that are multiples of both 3 and 5 become "FizzBuzz". Everything else just becomes the number as a string.

For this kata, we'll build two APIs. First, fizzbuzz_of, which takes a single number and returns the appropriate string. Second, fizzbuzz_1_to, which generates a list of FizzBuzz strings from 1 to n, with a default of 100.

It might sound simple, but there's a lot to learn from this problem when approached with TDD discipline.

---

## Slide 4: Project Setup - Python Environment
Let's talk about the project setup. I used pyenv for Python version management, which allows easy switching between different Python versions. For this project, I selected Python 3.10.13.

The project follows a modern structure. All production code goes in the src directory. The tests live in src/tests - I specifically chose to put tests inside src rather than at the root level, which helps keep the structure clean.

We're using pyproject.toml for modern Python packaging. This is the current standard and replaces the older setup.py approach. It's cleaner and more declarative.

---

## Slide 5: pyproject.toml Configuration
Here's what the pyproject.toml looks like. We declare the project name, version, and minimum Python version.

Under optional dependencies, we specify our development tools: pytest for testing, pytest-cov for coverage reporting, pytest-watch for continuous testing, flake8 for code quality checks, and pre-commit for git hooks.

The pytest configuration section tells pytest where to find tests and what options to use. We specify testpaths as "src/tests", and add options for verbose output and coverage reporting.

This one file handles all our project configuration in a clean, standardized way.

---

## Slide 6: Quality Gates - Pre-commit Hooks
Quality gates are crucial for maintaining code standards. I set up pre-commit hooks that run automated checks before every commit.

These hooks run flake8 for code style and quality checks. They run pytest to ensure all tests pass. They remove trailing whitespace, validate YAML and TOML files, and check for large files.

The benefits are immediate. You can't commit bad code - the hooks prevent it. Code style stays consistent across all commits. And you catch errors early, before they reach your repository or CI/CD pipeline.

Think of this as your first line of defense against bugs and style issues.

---

## Slide 7: Continuous Integration - GitHub Actions
The second line of defense is our CI/CD pipeline using GitHub Actions. This provides automated testing on every push to the repository.

The pipeline tests our code on Python 3.10, 3.11, and 3.12 using a matrix strategy. It runs flake8 for code quality, then runs pytest with coverage reporting.

This ensures our code works across multiple Python versions. If we accidentally use a feature only available in Python 3.11, the 3.10 build will catch it. This gives us confidence when collaborating with others or deploying to different environments.

---

## Slide 8: Git Setup - Local vs Global Config
One important setup detail: managing multiple git identities. I have work projects on GitLab and personal projects on GitHub, each requiring different credentials.

The solution is using global config for work and local config for personal projects. For this personal project, I used git config --local to set my personal name and email.

I also used GitHub CLI - the gh command - for repository management. With gh auth login and gh repo create, I could set up the entire GitHub integration from the command line. This is much faster than using the web interface.

---

## Slide 9: Phase A Overview
Now let's dive into the TDD implementation, starting with Phase A. The goal of Phase A is to handle numbers that aren't multiples of 3 or 5 - the simple case where we just return the number as a string.

Test 1: We test that 1 returns "1". Red - we write the failing test. Green - we return the hardcoded string "1". This is called "fake it till you make it."

Test 2: We test that 2 returns "2". Red - add the test. Green - use an if/else to handle both 1 and 2.

Test 3: We test that 4 returns "4". Red - add the test. Green - extend the logic. But now we see a pattern! Refactor - we replace all the conditional logic with simply return str(number).

Notice we didn't test 3 yet. That's intentional, and we'll see why shortly.

---

## Slide 10: Phase A Code
Here's what the final Phase A code looks like. The implementation is beautifully simple: just return str(number).

The three tests verify this behavior for 1, 2, and 4. Each test is clear and focused. They document the expected behavior.

This might seem overly simple, but that's the point of TDD. We let the tests guide us to the simplest solution that could possibly work. We didn't try to predict what we'd need later. We solved the problem at hand.

---

## Slide 11: TDD Principle - One Behavior at a Time
So why did we skip 3 in Phase A? This illustrates a key TDD principle: test one behavior at a time.

The number 3 introduces NEW behavior - it should return "Fizz", not "3". Phase A focused exclusively on the "number to string" behavior. We wanted to finish that behavior completely before adding new ones.

This keeps each phase focused and simple. We're not juggling multiple concerns simultaneously.

The tests for 1, 2, and 4 demonstrate triangulation. By testing multiple examples of the same behavior, we forced ourselves to find a general solution - str(number) - rather than hard-coding specific cases.

This disciplined approach might feel slow at first, but it leads to better design and fewer bugs.

---

## Slide 12: Phase B Overview
Phase B introduces the "Fizz" behavior for multiples of 3.

Test 4: We test that 3 returns "Fizz". Red - write the failing test. Green - add a condition: if number mod 3 equals 0, return "Fizz".

Test 5: We test that 6 also returns "Fizz". This is triangulation. Red - add the test. Green - and here's something interesting: no code change is needed! The mod 3 rule already handles 6.

When triangulation doesn't require code changes, that's validation that our solution is general enough. It's a great feeling when that happens.

---

## Slide 13: Phase B Code
Here's the Phase B code evolution. Now our function checks if the number is divisible by 3, and if so, returns "Fizz". Otherwise, it returns the string representation of the number.

The two new tests verify this behavior for 3 and 6. Notice how the test for 6 is marked with "triangulate" in the comment. This documents our intent - we're not just testing another number, we're verifying our solution is general.

The code is still simple and readable. We haven't over-engineered anything.

---

## Slide 14: Phase C Overview
Phase C follows the same pattern, adding "Buzz" for multiples of 5.

Test 6: We test that 5 returns "Buzz". Red - write the failing test. Green - add another condition: if number mod 5 equals 0, return "Buzz".

Test 7: We test that 10 also returns "Buzz" to triangulate. Red - add the test. Green - again, no code change needed!

A pattern is emerging in our TDD practice: test, implement, triangulate. This rhythm becomes natural with practice.

---

## Slide 15: Phase C Code
The Phase C code now has both the Fizz and Buzz logic. If the number is divisible by 3, return "Fizz". If divisible by 5, return "Buzz". Otherwise, return the string representation.

The tests for 5 and 10 verify the Buzz behavior. Each test is independent and clearly named.

At this point, you might be thinking: "What about numbers divisible by both 3 and 5?" That's exactly what Phase D addresses.

---

## Slide 16: Phase D Challenge
Phase D presents the combined case challenge. We need to test that 15 returns "FizzBuzz".

Here's the problem: 15 is a multiple of both 3 and 5. With our current code, when we check if 15 is divisible by 3, we'd return "Fizz" and stop. But we need "FizzBuzz" instead.

The solution? Order matters! We need to check the combined case - divisibility by 15 - first, before checking the individual cases.

This is a key TDD insight: the tests drive the design. The test for 15 forces us to restructure our conditionals. Without this test, we might have missed this edge case entirely.

---

## Slide 17: Phase D Code
Here's the final Phase D implementation. The key change is at the top: we check if number mod 15 equals 0 first, and return "FizzBuzz" if so.

Then we check the individual cases: mod 3 for "Fizz", mod 5 for "Buzz". Finally, we return the string representation of the number.

The order of these conditionals is crucial. This is why we have the test - it documents and enforces this requirement.

The new test verifies that 15 returns "FizzBuzz". Now all our tests pass, and we have complete coverage of the FizzBuzz rules.

---

## Slide 18: Alternative - Build String Approach
There's actually an alternative approach worth mentioning. Instead of checking for 15 explicitly, we can build the result string progressively.

Start with an empty string. If the number is divisible by 3, add "Fizz". If divisible by 5, add "Buzz". Then return the result if it's not empty, or the string representation of the number if it is.

This approach has some elegance: we check both conditions independently, build the string progressively, and don't need to check mod 15 explicitly. The combined case emerges naturally.

Same behavior, different structure. Both are valid. The tests don't care about the implementation - they just verify the behavior. That's the beauty of TDD.

---

## Slide 19: Phase E Overview
Phase E adds the sequence API. The goal is to generate a FizzBuzz sequence from 1 to n.

Test 9: We test that fizzbuzz_1_to(5) returns the list ["1", "2", "Fizz", "4", "Buzz"]. Red - write the failing test. Green - implement using a list comprehension that calls fizzbuzz_of for each number. This demonstrates code reuse - we're building on what we already have.

Test 10: We test that calling fizzbuzz_1_to with no arguments returns 100 items by default. Red - write the test. Green - add a default parameter n equals 100.

This phase shows how TDD naturally leads to composable functions. We didn't duplicate the FizzBuzz logic; we reused it.

---

## Slide 20: Phase E Code
Here's the Phase E implementation. The fizzbuzz_1_to function takes n with a default of 100. It returns a list comprehension: fizzbuzz_of(i) for i in range(1, n plus 1).

This is beautiful code. It's concise, readable, and reuses our existing function. Someone reading this immediately understands what it does.

The first test verifies the sequence for 1 to 5. The second test verifies the default parameter works correctly, checking both the length and some boundary values.

At this point, we have both APIs complete, and all 10 tests passing.

---

## Slide 21: Power of Code Reuse
Let's pause to appreciate the power of code reuse. The fizzbuzz_1_to function reuses fizzbuzz_of.

The benefits are substantial. We have a single source of truth for the FizzBuzz logic. If the rules change - say we need to handle multiples of 7 as well - we only modify one place. This is composition over duplication.

The code is easy to test and maintain. Each function has one responsibility. This follows the DRY principle: Don't Repeat Yourself.

Good code reuse isn't just about saving typing. It's about creating a clear, maintainable architecture where changes are localized and safe.

---

## Slide 22: Final Test Results
Let's look at our final test results. We have 10 tests, all passing. The green checkmark is what we're always aiming for.

Coverage is 100%. Every line of production code is executed by at least one test. This gives us high confidence in correctness.

These tests run in multiple places. Locally, the pre-commit hooks run them. On GitHub, the Actions pipeline runs them. And they run on Python 3.10, 3.11, and 3.12.

The total CI run time is about 30 seconds. That's fast feedback. Within half a minute of pushing code, we know if we've broken anything.

This comprehensive testing infrastructure wasn't expensive to set up, and it pays dividends every single day.

---

## Slide 23: Git History - The Story
Looking at our git history, we can see the entire story of this project. Each commit represents a milestone.

We started with the initial commit adding the CLAUDE.md guidelines. Then we added the Python project setup with pytest and TDD structure. Next came the pre-commit hooks with flake8 and pytest.

We tested the pre-commit hooks worked, cleaned up test files, and added the GitHub Actions pipeline.

Then came the TDD phases: Phase A and B together, Phase C for Buzz, Phase D for the combined case, and finally Phase E for the sequence API.

We also added documentation: the commit strategy to CLAUDE.md and the phase descriptions to the README.

This history isn't just a log - it's a learning resource. Anyone can clone this repository and step through the commits to see how the code evolved.

---

## Slide 24: Commit Strategy for Learning
The commit strategy was deliberate and designed for learning. We commit after each phase completion.

Each commit message includes the phase identifier - A, B, C, D, or E. It lists what tests were added. It describes what production code changed. And it includes the test count and coverage.

The benefits for revision are enormous. It's easy to replay the progression by walking through commits. You can see how tests drive design. You understand each decision point.

This is particularly valuable when learning. You can use git log and git show to review each step. You can even use git revert to go back and try different approaches.

Good commit messages are documentation. They explain not just what changed, but why.

---

## Slide 25: Key TDD Learnings
Let me summarize the key TDD learnings from this kata.

First: Write ONE failing test at a time. Focus prevents overwhelm. When you're only thinking about one test, the problem becomes manageable.

Second: Write minimal code to pass. "Fake it till you make it" is not only OK, it's encouraged. Let triangulation force you to generalize.

Third: Triangulation forces general solutions. Multiple examples reveal patterns you might miss with a single test.

Fourth: Refactor only when tests are green. The safety net prevents breaking changes. If you refactor while tests are red, you don't know what caused new failures.

Fifth: Test one behavior at a time. This keeps phases focused and manageable. Don't try to test everything at once.

These aren't just rules - they're a discipline that leads to better code.

---

## Slide 26: Quality Automation Stack
Our quality automation has two layers, and they work together beautifully.

Layer 1 is pre-commit hooks running locally. They run before each commit, providing fast feedback in less than 5 seconds. They prevent bad commits from ever entering the repository.

Layer 2 is GitHub Actions running remotely. They run on every push to GitHub. They test multiple Python versions and catch environment-specific issues we might miss locally.

The result is high confidence with low manual effort. I never manually run tests before committing - the hooks do it. I never wonder if my code works on Python 3.12 - the pipeline tells me.

Automation isn't about replacing human judgment. It's about freeing humans from repetitive tasks so we can focus on design and problem-solving.

---

## Slide 27: Final Project Structure
Here's the final project structure. At the root, we have .github/workflows containing the CI pipeline configuration.

The src directory contains __init__.py, our fizzbuzz.py production code, and a tests subdirectory with the test files.

We have .pre-commit-config.yaml for the hooks configuration, .flake8 for linting rules, and pyproject.toml for project configuration.

CLAUDE.md serves as an AI pairing guide with the step-by-step instructions we followed. And README.md provides documentation.

This structure is clean, organized, and follows Python community standards. Someone familiar with modern Python projects will immediately understand the layout.

---

## Slide 28: Potential Refactorings - Stretch Goals
If we wanted to extend this kata, there are several potential refactorings.

We could make the divisors configurable. Instead of hard-coding 3 and 5, we could pass in a rules list: [(3, 'Fizz'), (5, 'Buzz')]. This makes the code more flexible for variations.

We could add input validation. Reject negative numbers, reject non-integers, provide clear error messages.

We could enhance output formatting. Join the sequence with newlines or spaces, print directly to console, format as JSON.

The key point? All of these would be test-driven! We'd write tests for negative number validation before implementing it. We'd test the configurable rules before coding them.

Once you've learned TDD, you apply it everywhere.

---

## Slide 29: Applying These Lessons
These lessons apply to any kata or project, not just FizzBuzz.

First, set up quality gates before writing code. Pre-commit hooks and CI/CD pipeline come first. This prevents technical debt from accumulating.

Second, break the problem into phases. One behavior per phase, progressing from simple to complex. Don't try to solve everything at once.

Third, follow Red-Green-Refactor strictly. Don't skip the red phase. Don't refactor while tests are failing.

Fourth, commit regularly with good messages. Your git history should tell a story.

Fifth, use git history as a learning tool. Future you will thank present you for clear commits.

These principles work for katas, work projects, open source contributions - anywhere you write code.

---

## Slide 30: Summary - FizzBuzz TDD Journey
Let's summarize our FizzBuzz TDD journey.

We set up a modern Python project with proper tooling. We implemented automated quality gates with hooks and CI/CD. We completed 5 phases, each building on the last. We wrote 10 tests achieving 100% coverage. We created clean, maintainable code. And our git history tells the complete story.

But here's the crucial insight: TDD isn't just about testing. It's a design methodology that leads to better architecture, higher confidence, living documentation, and easier refactoring.

The tests aren't just validating correctness - they're guiding the design. The small steps aren't just safety - they're focus. The commits aren't just history - they're learning resources.

Thank you for following along on this journey. I hope this has shown you the power and discipline of Test-Driven Development. Now go forth and write some tests first!
