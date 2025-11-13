# GitHub Setup Guide for Personal Katas

This guide helps you set up GitHub for your personal kata projects while keeping your work GitLab configuration separate.

## Current Configuration

**Work (GitLab - Global)**:
- Email: `thomas.spencer@homeoffice.gov.uk`
- Name: `Thomas Spencer`
- Scope: Configured globally (applies to all repositories by default)

**Personal (GitHub - Local)**:
- Email: `tomspencerlondon@gmail.com`
- Name: `Tom Spencer`
- GitHub Account: `TomSpencerLondon`
- Scope: Configured locally in this kata repository only

## How Git Configuration Works

Git uses a configuration hierarchy where **local settings override global settings**:

1. **Local** config (`.git/config` in this repository) - **highest priority**
2. **Global** config (`~/.gitconfig` for your user) - used as fallback
3. **System** config (all users) - lowest priority

### Check Your Configuration

```bash
# View global config (work settings)
git config --global --list

# View local config (this repository only)
git config --local --list

# View effective config (what will actually be used)
git config user.email
git config user.name
```

For this kata directory, the local config overrides your global work settings, so your commits will use your personal GitHub email.

## Step-by-Step Setup

### 1. Install GitHub CLI

GitHub CLI (`gh`) provides seamless integration with GitHub and works great with Claude Code.

```bash
# Install GitHub CLI using Homebrew
brew install gh

# Verify installation
gh --version
```

### 2. Authenticate with GitHub

```bash
# Start authentication flow
gh auth login
```

**Follow the prompts**:
- What account do you want to log into? → **GitHub.com**
- What is your preferred protocol? → **HTTPS** (recommended)
- Authenticate Git with your GitHub credentials? → **Yes**
- How would you like to authenticate? → **Login with a web browser** (easiest)

Copy the one-time code shown, press Enter, and your browser will open. Paste the code and authorize.

### 3. Verify Authentication

```bash
# Check authentication status
gh auth status

# Should show you're logged in as TomSpencerLondon
```

### 4. Create a GitHub Repository for This Kata

Choose one of these options:

**Option A: Quick setup (recommended)**
```bash
# Create repo and push in one command
gh repo create fizz-buzz-kata --public --source=. --remote=origin --push
```

**Option B: Manual setup**
```bash
# Create repo on GitHub
gh repo create fizz-buzz-kata --public

# Add remote and push
git remote add origin https://github.com/TomSpencerLondon/fizz-buzz-kata.git
git branch -M main
git push -u origin main
```

The repository will be created under your personal account `TomSpencerLondon`.

## Common GitHub CLI Commands

Claude Code integrates very well with the GitHub CLI. Here are useful commands:

### Repository Management
```bash
# Create a new repository
gh repo create <repo-name> --public

# View repository in browser
gh repo view --web

# Clone a repository
gh repo clone TomSpencerLondon/<repo-name>
```

### Pull Requests (Claude Code Integration)
```bash
# Create a pull request (Claude Code can do this automatically!)
gh pr create --title "Add new feature" --body "Description"

# List pull requests
gh pr list

# View a PR in browser
gh pr view <number> --web

# Check PR status and CI/CD
gh pr status

# Merge a PR
gh pr merge <number>
```

### Issues
```bash
# Create an issue
gh issue create --title "Bug: something broken" --body "Details"

# List issues
gh issue list

# View issue in browser
gh issue view <number> --web

# Close an issue
gh issue close <number>
```

### Viewing Information
```bash
# Check current repository status
gh repo view

# See recent workflow runs (CI/CD)
gh run list

# View run details
gh run view <run-id>
```

## Setting Up New Kata Repositories

For each new kata repository you create, follow these steps:

### 1. Create Directory and Initialize Git
```bash
mkdir my-new-kata
cd my-new-kata
git init
```

### 2. Configure Local Git for Personal GitHub
```bash
git config user.name "Tom Spencer"
git config user.email "tomspencerlondon@gmail.com"
```

### 3. Verify Configuration
```bash
# Check which email will be used
git config user.email
# Should show: tomspencerlondon@gmail.com
```

### 4. Create Initial Commit
```bash
# Add your files
git add .

# Commit
git commit -m "Initial commit"
```

### 5. Create GitHub Repo and Push
```bash
gh repo create my-new-kata --public --source=. --remote=origin --push
```

## Quick Reference: Configuration Commands

```bash
# Check effective config (what will be used)
git config user.email
git config user.name

# Check global config only
git config --global user.email
git config --global user.name

# Check local config only
git config --local user.email
git config --local user.name

# Set local config for current repository
git config user.email "tomspencerlondon@gmail.com"
git config user.name "Tom Spencer"

# Set global config (affects all repos)
git config --global user.email "your-email@example.com"
git config --global user.name "Your Name"
```

## Verify Which Account You're Using

Before making commits, you can verify everything is configured correctly:

```bash
# Check git configuration
echo "Git config that will be used:"
git config user.email
git config user.name

# Check GitHub authentication
echo -e "\nGitHub authentication:"
gh auth status

# Check current remote
echo -e "\nRemote repository:"
git remote -v
```

## Troubleshooting

### Wrong Email on Commits

If you accidentally commit with your work email:

```bash
# Fix the last commit
git commit --amend --author="Tom Spencer <tomspencerlondon@gmail.com>"

# Or reset author of last commit
git commit --amend --reset-author --no-edit
```

### Check Which Config File is Being Used

```bash
# Show where each setting comes from
git config --list --show-origin | grep user
```

### Multiple GitHub Accounts

If you need to switch between GitHub accounts:

```bash
# Log out of current account
gh auth logout

# Log in with different account
gh auth login
```

### Verify Remote Repository

```bash
# See which remote repository you're connected to
git remote -v

# Should show:
# origin  https://github.com/TomSpencerLondon/repo-name.git (fetch)
# origin  https://github.com/TomSpencerLondon/repo-name.git (push)
```

## Benefits for Claude Code

With GitHub CLI authenticated, Claude Code can:

- ✅ Create pull requests directly
- ✅ Check PR status and comments
- ✅ Interact with issues
- ✅ View CI/CD status
- ✅ Merge pull requests
- ✅ Create repositories
- ✅ Seamless integration without manual GitHub API token setup

This makes Claude Code much more powerful for your development workflow!

## Next Steps

Run these commands in order:

```bash
# 1. Install GitHub CLI
brew install gh

# 2. Authenticate with GitHub
gh auth login

# 3. Verify authentication
gh auth status

# 4. Create repository and push (choose one)
# Quick:
gh repo create fizz-buzz-kata --public --source=. --remote=origin --push

# Or manual:
gh repo create fizz-buzz-kata --public
git remote add origin https://github.com/TomSpencerLondon/fizz-buzz-kata.git
git push -u origin main
```

## Summary

- ✅ Git repository initialized
- ✅ Local git config set to use personal GitHub email
- ✅ Global git config still uses work email (for work projects)
- ⏳ Install GitHub CLI: `brew install gh`
- ⏳ Authenticate: `gh auth login`
- ⏳ Create repo: `gh repo create fizz-buzz-kata --public --source=. --remote=origin --push`

Happy coding with Claude Code! 🚀
