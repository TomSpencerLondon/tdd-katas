#!/usr/bin/env python3
"""Generate PDF presentation from FizzBuzz kata git history."""

from reportlab.lib.pagesizes import letter, landscape
from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.lib.units import inch
from reportlab.lib.enums import TA_CENTER
from reportlab.platypus import SimpleDocTemplate, Paragraph, Spacer, PageBreak, Preformatted
from reportlab.lib.colors import HexColor, black
import subprocess


def run_git_command(cmd):
    """Run a git command and return output."""
    result = subprocess.run(cmd, shell=True, capture_output=True, text=True)
    return result.stdout.strip()


def create_pdf_presentation():
    """Create a PDF presentation."""
    pdf_file = "FizzBuzz_TDD_Journey.pdf"
    doc = SimpleDocTemplate(
        pdf_file,
        pagesize=landscape(letter),
        rightMargin=0.5*inch,
        leftMargin=0.5*inch,
        topMargin=0.5*inch,
        bottomMargin=0.5*inch
    )

    # Styles
    styles = getSampleStyleSheet()

    title_style = ParagraphStyle(
        'CustomTitle',
        parent=styles['Heading1'],
        fontSize=32,
        textColor=HexColor('#1a1a1a'),
        spaceAfter=12,
        alignment=TA_CENTER,
        fontName='Helvetica-Bold'
    )

    subtitle_style = ParagraphStyle(
        'CustomSubtitle',
        parent=styles['Heading2'],
        fontSize=18,
        textColor=HexColor('#555555'),
        spaceAfter=20,
        alignment=TA_CENTER,
        fontName='Helvetica'
    )

    heading_style = ParagraphStyle(
        'CustomHeading',
        parent=styles['Heading1'],
        fontSize=24,
        textColor=HexColor('#1a1a1a'),
        spaceAfter=16,
        fontName='Helvetica-Bold'
    )

    bullet_style = ParagraphStyle(
        'CustomBullet',
        parent=styles['Normal'],
        fontSize=13,
        textColor=black,
        leftIndent=20,
        spaceAfter=6,
        leading=16,
        fontName='Helvetica'
    )

    bullet2_style = ParagraphStyle(
        'CustomBullet2',
        parent=styles['Normal'],
        fontSize=12,
        textColor=HexColor('#333333'),
        leftIndent=40,
        spaceAfter=4,
        leading=15,
        fontName='Helvetica'
    )

    code_style = ParagraphStyle(
        'Code',
        parent=styles['Code'],
        fontSize=10,
        fontName='Courier',
        leftIndent=20,
        spaceAfter=4,
        leading=12
    )

    story = []

    # Helper functions
    def add_title_slide(title, subtitle=""):
        story.append(Spacer(1, 2*inch))
        story.append(Paragraph(title, title_style))
        if subtitle:
            story.append(Spacer(1, 0.3*inch))
            story.append(Paragraph(subtitle, subtitle_style))
        story.append(PageBreak())

    def add_content_slide(title, bullets):
        story.append(Paragraph(title, heading_style))
        story.append(Spacer(1, 0.2*inch))
        for bullet in bullets:
            if isinstance(bullet, tuple):
                text, level = bullet
                if level == 0:
                    story.append(Paragraph(f"• {text}", bullet_style))
                else:
                    story.append(Paragraph(f"◦ {text}", bullet2_style))
            elif bullet == "":
                story.append(Spacer(1, 0.1*inch))
            else:
                story.append(Paragraph(f"• {bullet}", bullet_style))
        story.append(PageBreak())

    def add_code_slide(title, code):
        story.append(Paragraph(title, heading_style))
        story.append(Spacer(1, 0.2*inch))
        # Split code into lines and add each as Preformatted
        for line in code.split('\n'):
            story.append(Preformatted(line, code_style))
        story.append(PageBreak())

    # Slide 1: Title
    add_title_slide("FizzBuzz Kata with TDD",
                    "A Step-by-Step Journey Through Test-Driven Development")

    # Slide 2: What is TDD?
    add_content_slide("What is Test-Driven Development?", [
        "Red-Green-Refactor Cycle",
        ("Write a failing test first (Red)", 1),
        ("Write minimal code to pass the test (Green)", 1),
        ("Refactor to improve code quality (Refactor)", 1),
        ("Commit after each complete phase", 1),
        "",
        "Benefits of TDD",
        ("Ensures code correctness from the start", 1),
        ("Creates living documentation through tests", 1),
        ("Makes refactoring safer", 1),
        ("Encourages simple, focused design", 1)
    ])

    # Slide 3: FizzBuzz Problem
    add_content_slide("FizzBuzz Problem", [
        "Convert numbers to strings with these rules:",
        ("Multiples of 3 → 'Fizz'", 1),
        ("Multiples of 5 → 'Buzz'", 1),
        ("Multiples of both 3 and 5 → 'FizzBuzz'", 1),
        ("Everything else → the number as a string", 1),
        "",
        "Two APIs to build:",
        ("fizzbuzz_of(number) → string", 1),
        ("fizzbuzz_1_to(n=100) → list[string]", 1)
    ])

    # Slide 4: Project Setup - Python
    add_content_slide("Project Setup: Python Environment", [
        "Used pyenv for Python version management",
        ("Python 3.10.13 selected", 1),
        ("Allows easy switching between versions", 1),
        "",
        "Modern project structure:",
        ("src/ - production code", 1),
        ("src/tests/ - test code (tests inside src)", 1),
        ("pyproject.toml - modern Python packaging", 1)
    ])

    # Slide 5: pyproject.toml
    add_code_slide("pyproject.toml Configuration", """[project]
name = "fizz-buzz-kata"
version = "0.1.0"
requires-python = ">=3.10"

[project.optional-dependencies]
dev = [
    "pytest>=7.4.0",
    "pytest-cov>=4.1.0",
    "pytest-watch>=4.2.0",
    "flake8>=6.0.0",
    "pre-commit>=3.0.0",
]

[tool.pytest.ini_options]
testpaths = ["src/tests"]
addopts = ["-v", "--cov=src"]""")

    # Slide 6: Pre-commit Hooks
    add_content_slide("Quality Gates: Pre-commit Hooks", [
        "Automated checks before every commit:",
        ("flake8 - Code style and quality checks", 1),
        ("pytest - All tests must pass", 1),
        ("Trailing whitespace removal", 1),
        ("YAML/TOML validation", 1),
        "",
        "Benefits:",
        ("Prevents bad code from being committed", 1),
        ("Enforces consistent code style", 1),
        ("Catches errors early", 1)
    ])

    # Slide 7: GitHub Actions
    add_content_slide("Continuous Integration: GitHub Actions", [
        "Automated testing on every push:",
        ("Tests on Python 3.10, 3.11, 3.12", 1),
        ("Runs flake8 for code quality", 1),
        ("Runs pytest with coverage", 1),
        ("Matrix strategy for multiple versions", 1),
        "",
        "Ensures code works across Python versions",
        "Provides confidence for collaboration"
    ])

    # Slide 8: Git Configuration
    add_content_slide("Git Setup: Local vs Global Config", [
        "Work vs Personal Projects:",
        ("Global config for work (GitLab)", 1),
        ("Local config for personal (GitHub)", 1),
        "",
        "Commands used:",
        ("git config --local user.name 'TomSpencerLondon'", 1),
        ("git config --local user.email 'tomspencerlondon@gmail.com'", 1),
        "",
        "GitHub CLI (gh) for repository management",
        ("gh auth login", 1),
        ("gh repo create", 1)
    ])

    # Slide 9: Phase A Overview
    add_content_slide("Phase A: Numbers (not multiples of 3 or 5)", [
        "Goal: Return number as string for non-special numbers",
        "",
        "Test 1: 1 → '1'",
        ("Red: Write failing test", 1),
        ("Green: return '1' (fake it)", 1),
        "",
        "Test 2: 2 → '2'",
        ("Red: Add test for 2", 1),
        ("Green: if/else for 1 and 2", 1),
        "",
        "Test 3: 4 → '4'",
        ("Red: Add test for 4", 1),
        ("Green: Extend logic", 1),
        ("Refactor: Replace with return str(number)", 1)
    ])

    # Slide 10: Phase A Code
    add_code_slide("Phase A: Final Code", """def fizzbuzz_of(number):
    '''Convert a number to FizzBuzz string.'''
    return str(number)


# Tests
def test_1_is_string():
    assert fizzbuzz_of(1) == "1"

def test_2_is_string():
    assert fizzbuzz_of(2) == "2"

def test_4_is_string():
    assert fizzbuzz_of(4) == "4"
""")

    # Slide 11: Why Not Test 3?
    add_content_slide("TDD Principle: One Behavior at a Time", [
        "Why skip 3 in Phase A?",
        ("3 introduces NEW behavior (Fizz)", 1),
        ("Phase A focuses on 'number → string' behavior", 1),
        ("Finish current behavior before adding new ones", 1),
        "",
        "This keeps each phase focused and simple",
        "",
        "Triangulation with 1, 2, 4:",
        ("Multiple examples of same behavior", 1),
        ("Forces general solution: str(number)", 1)
    ])

    # Slide 12: Phase B Overview
    add_content_slide("Phase B: Multiples of 3 → 'Fizz'", [
        "Test 4: 3 → 'Fizz'",
        ("Red: Write failing test", 1),
        ("Green: if number % 3 == 0: return 'Fizz'", 1),
        "",
        "Test 5: 6 → 'Fizz' (triangulate)",
        ("Red: Add test for 6", 1),
        ("Green: No code change needed!", 1),
        ("The % 3 rule already handles it", 1),
        "",
        "Triangulation confirms our solution is general"
    ])

    # Slide 13: Phase B Code
    add_code_slide("Phase B: Code Evolution", """def fizzbuzz_of(number):
    '''Convert a number to FizzBuzz string.'''
    if number % 3 == 0:
        return "Fizz"
    return str(number)


# New tests
def test_3_is_fizz():
    assert fizzbuzz_of(3) == "Fizz"

def test_6_is_fizz():
    assert fizzbuzz_of(6) == "Fizz"
""")

    # Slide 14: Phase C Overview
    add_content_slide("Phase C: Multiples of 5 → 'Buzz'", [
        "Test 6: 5 → 'Buzz'",
        ("Red: Write failing test", 1),
        ("Green: if number % 5 == 0: return 'Buzz'", 1),
        "",
        "Test 7: 10 → 'Buzz' (triangulate)",
        ("Red: Add test for 10", 1),
        ("Green: No code change needed!", 1),
        "",
        "Pattern emerging: Test, implement, triangulate"
    ])

    # Slide 15: Phase C Code
    add_code_slide("Phase C: Adding Buzz Logic", """def fizzbuzz_of(number):
    '''Convert a number to FizzBuzz string.'''
    if number % 3 == 0:
        return "Fizz"
    if number % 5 == 0:
        return "Buzz"
    return str(number)


# New tests
def test_5_is_buzz():
    assert fizzbuzz_of(5) == "Buzz"

def test_10_is_buzz():
    assert fizzbuzz_of(10) == "Buzz"
""")

    # Slide 16: Phase D Challenge
    add_content_slide("Phase D: The Combined Case Challenge", [
        "Test 8: 15 → 'FizzBuzz'",
        "",
        "Problem: 15 is a multiple of BOTH 3 and 5",
        ("Current code would return 'Fizz' and stop", 1),
        ("We need 'FizzBuzz' instead", 1),
        "",
        "Solution: Order matters!",
        ("Check combined case (% 15) first", 1),
        ("Then check individual cases", 1),
        "",
        "Key TDD insight: Tests drive the design"
    ])

    # Slide 17: Phase D Code
    add_code_slide("Phase D: Final Implementation", """def fizzbuzz_of(number):
    '''Convert a number to FizzBuzz string.'''
    if number % 15 == 0:
        return "FizzBuzz"
    if number % 3 == 0:
        return "Fizz"
    if number % 5 == 0:
        return "Buzz"
    return str(number)


# New test
def test_15_is_fizzbuzz():
    assert fizzbuzz_of(15) == "FizzBuzz"
""")

    # Slide 18: Alternative Approach
    add_code_slide("Alternative: Build String Approach", """def fizzbuzz_of(number):
    '''Alternative implementation.'''
    result = ""
    if number % 3 == 0:
        result += "Fizz"
    if number % 5 == 0:
        result += "Buzz"
    return result or str(number)

# This approach:
# - Checks both conditions independently
# - Builds the string progressively
# - No need to check % 15 explicitly
# - Same behavior, different structure""")

    # Slide 19: Phase E Overview
    add_content_slide("Phase E: Sequence API", [
        "Goal: Generate sequence from 1 to n",
        "",
        "Test 9: fizzbuzz_1_to(5)",
        ("Red: Test expects ['1','2','Fizz','4','Buzz']", 1),
        ("Green: Use list comprehension", 1),
        ("Reuse fizzbuzz_of() for each number", 1),
        "",
        "Test 10: Default to 100",
        ("Red: Test expects 100 items by default", 1),
        ("Green: Add default parameter n=100", 1)
    ])

    # Slide 20: Phase E Code
    add_code_slide("Phase E: Sequence Implementation", """def fizzbuzz_1_to(n=100):
    '''Generate FizzBuzz sequence from 1 to n.

    Args:
        n: Upper limit (default 100)

    Returns:
        List of FizzBuzz strings from 1 to n
    '''
    return [fizzbuzz_of(i) for i in range(1, n + 1)]


# Tests
def test_sequence_to_5():
    assert fizzbuzz_1_to(5) == ["1", "2", "Fizz", "4", "Buzz"]

def test_sequence_default_is_100():
    result = fizzbuzz_1_to()
    assert len(result) == 100
    assert result[99] == "Buzz"
""")

    # Slide 21: Code Reuse
    add_content_slide("Power of Code Reuse", [
        "fizzbuzz_1_to reuses fizzbuzz_of",
        "",
        "Benefits:",
        ("Single source of truth for FizzBuzz logic", 1),
        ("Changes to rules only need one place", 1),
        ("Composition over duplication", 1),
        ("Easy to test and maintain", 1),
        "",
        "This is the 'DRY' principle:",
        ("Don't Repeat Yourself", 1)
    ])

    # Slide 22: Final Results
    add_content_slide("Final Test Results", [
        "10 tests, all passing ✓",
        "",
        "Coverage: 100%",
        ("Every line of production code is tested", 1),
        ("High confidence in correctness", 1),
        "",
        "Tests run in multiple places:",
        ("Pre-commit hooks (local)", 1),
        ("GitHub Actions (CI/CD)", 1),
        ("Python 3.10, 3.11, 3.12", 1),
        "",
        "Total time: ~30 seconds per full CI run"
    ])

    # Slide 23: Git History
    commits = run_git_command("git log --oneline --reverse").split('\n')
    commit_bullets = []
    for commit in commits[:12]:  # Limit to prevent overflow
        if commit.strip():
            parts = commit.split(' ', 1)
            if len(parts) == 2:
                commit_bullets.append(f"{parts[1]}")

    add_content_slide("Git History: The Story", commit_bullets)

    # Slide 24: Commit Strategy
    add_content_slide("Commit Strategy for Learning", [
        "Commit after each phase completion",
        "",
        "Each commit message includes:",
        ("Phase identifier (A, B, C, D, E)", 1),
        ("What tests were added", 1),
        ("What production code changed", 1),
        ("Test count and coverage", 1),
        "",
        "Benefits for revision:",
        ("Easy to replay the progression", 1),
        ("See how tests drive design", 1),
        ("Understand each decision point", 1)
    ])

    # Slide 25: Key Learnings
    add_content_slide("Key TDD Learnings", [
        "1. Write ONE failing test at a time",
        ("Focus prevents overwhelm", 1),
        "",
        "2. Write minimal code to pass",
        ("'Fake it till you make it' is OK", 1),
        "",
        "3. Triangulation forces general solutions",
        ("Multiple examples reveal patterns", 1),
        "",
        "4. Refactor only when tests are green",
        ("Safety net prevents breaking changes", 1),
        "",
        "5. Test one behavior at a time",
        ("Keeps phases focused and manageable", 1)
    ])

    # Slide 26: Quality Stack
    add_content_slide("Quality Automation Stack", [
        "Layer 1: Pre-commit Hooks (Local)",
        ("Runs before each commit", 1),
        ("Fast feedback (< 5 seconds)", 1),
        ("Prevents bad commits", 1),
        "",
        "Layer 2: GitHub Actions (Remote)",
        ("Runs on push to GitHub", 1),
        ("Tests multiple Python versions", 1),
        ("Catches environment-specific issues", 1),
        "",
        "Result: High confidence, low manual effort"
    ])

    # Slide 27: Project Structure
    add_code_slide("Final Project Structure", """fizz_buzz/
├── .github/
│   └── workflows/
│       └── ci.yml          # GitHub Actions
├── src/
│   ├── __init__.py
│   ├── fizzbuzz.py         # Production code
│   └── tests/
│       ├── __init__.py
│       └── test_fizzbuzz.py # Tests
├── .pre-commit-config.yaml  # Hooks config
├── .flake8                  # Linting rules
├── pyproject.toml           # Project config
├── CLAUDE.md                # AI pairing guide
└── README.md                # Documentation""")

    # Slide 28: Refactoring Opportunities
    add_content_slide("Potential Refactorings (Stretch Goals)", [
        "Make divisors configurable:",
        ("rules = [(3, 'Fizz'), (5, 'Buzz')]", 1),
        ("More flexible for variations", 1),
        "",
        "Add input validation:",
        ("Reject negative numbers", 1),
        ("Reject non-integers", 1),
        "",
        "Output formatting:",
        ("Join with newlines or spaces", 1),
        ("Print directly to console", 1),
        "",
        "All these would be test-driven!"
    ])

    # Slide 29: Future Katas
    add_content_slide("Applying These Lessons", [
        "This approach works for any kata:",
        "",
        "1. Set up quality gates first",
        ("Pre-commit hooks", 1),
        ("CI/CD pipeline", 1),
        "",
        "2. Break problem into phases",
        ("One behavior per phase", 1),
        ("Simple to complex", 1),
        "",
        "3. Follow Red-Green-Refactor strictly",
        "",
        "4. Commit regularly with good messages",
        "",
        "5. Use git history as learning tool"
    ])

    # Slide 30: Summary
    add_content_slide("Summary: FizzBuzz TDD Journey", [
        "✓ Modern Python project setup",
        "✓ Automated quality gates (hooks + CI/CD)",
        "✓ 5 phases, each building on the last",
        "✓ 10 tests, 100% coverage",
        "✓ Clean, maintainable code",
        "✓ Git history tells the story",
        "",
        "TDD isn't just about testing—",
        "it's a design methodology that leads to:",
        ("Better architecture", 1),
        ("Higher confidence", 1),
        ("Living documentation", 1),
        ("Easier refactoring", 1)
    ])

    # Build PDF
    doc.build(story)
    return pdf_file


if __name__ == "__main__":
    print("Generating FizzBuzz TDD PDF presentation...")
    pdf_file = create_pdf_presentation()
    print(f"✓ PDF saved as {pdf_file}")
