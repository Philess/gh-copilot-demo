---
name: create-github-issue
description: 'Create a GitHub issue for this repository. Use when: filing a bug report, requesting a feature, tracking a task, opening an issue, creating a ticket, reporting a problem, suggesting an improvement.'
argument-hint: 'Describe the issue to create (e.g. "bug: login fails on mobile")'
---

# Create GitHub Issue

## When to Use
- Filing a bug report or regression
- Requesting a new feature or enhancement
- Tracking a task or work item
- Reporting a problem found in code review or testing

## Procedure

### 1. Gather Information
If not provided by the user, ask for:
- **Title**: Short, descriptive summary (under 72 characters)
- **Type**: bug / feature / task / question
- **Description**: What is the problem or request?
- **Steps to reproduce** (bugs only): Numbered list
- **Expected vs actual behavior** (bugs only)
- **Labels**: Any relevant labels (bug, enhancement, documentation, etc.)

### 2. Draft the Issue Body
Use the appropriate template below.

#### Bug Report Template
```markdown
## Description
<clear description of the bug>

## Steps to Reproduce
1.
2.
3.

## Expected Behavior
<what should happen>

## Actual Behavior
<what actually happens>

## Environment
- OS:
- Browser / Runtime:
- Version:
```

#### Feature Request Template
```markdown
## Summary
<one-paragraph description of the feature>

## Motivation
<why is this needed? what problem does it solve?>

## Proposed Solution
<description of the desired behavior>

## Alternatives Considered
<other approaches that were considered>
```

#### Task Template
```markdown
## Objective
<what needs to be done>

## Acceptance Criteria
- [ ]
- [ ]

## Notes
<any additional context>
```

### 3. Create the Issue
Use the `github-pull-request_doSearch` or GitHub tools to create the issue, or run:

```bash
gh issue create \
  --title "<title>" \
  --body "<body>" \
  --label "<label>"
```

### 4. Confirm
Report back the issue URL and number to the user.

## Tips
- Keep titles concise and action-oriented (e.g. "Fix: album cover not loading on slow connections")
- Add relevant labels to improve discoverability
- Reference related PRs or issues with `#<number>` in the body
- Assign the issue if ownership is known
